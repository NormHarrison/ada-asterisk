--with Ada.Text_IO;       use Ada.Text_IO;
with Ada.IO_Exceptions;

with GNAT.Byte_Swapping;


package body Asterisk.AudioSocket is

   ------------------
   -- Code_To_Kind --
   ------------------

   function Code_To_Kind (Code : in Byte) return Message_Kind
   is (case Code is
          when Hang_Up_Message_Code => Kind_Hang_Up,
          when UUID_Message_Code    => Kind_UUID,
          when Silence_Message_Code => Kind_Silence,
          when Audio_Message_Code   => Kind_Audio,
          when Error_Message_Code   => Kind_Error,
          when others               => Kind_Unknown);

   ----------
   -- Kind --
   ----------

   function Kind (Message : in Message_Type) return Message_Kind
   is (Message.Kind);

   -------------
   -- Payload --
   -------------

   function Payload (Message : in Message_Type) return Byte_Array
   is (Message.Payload);

   ---------------------
   -- Receive_Message --
   ---------------------

   function Receive_Message
     (Connection : in out Connection_Type) return Message_Type
   is
      Channel : GNAT.Sockets.Stream_Access :=
        GNAT.Sockets.Stream (Connection.Socket);

      Message_Code : Byte;
      Message_Size : Short_Integer;

   begin
      if Connection.Hung_Up then
         return No_Message;
      end if;

      Byte'Read (Channel, Message_Code);
      Short_Integer'Read (Channel, Message_Size);
      --  Read the 3 byte header of the message:
      --    - 1 byte for the type (`Message_Code`).
      --    - 2 bytes for the size (`Message_Size`).

      GNAT.Byte_Swapping.Swap2 (Message_Size'Address);
      --  The size is sent in big endian byte order and must
      --  be converted before being used to read the payload.

      declare
         Message : Message_Type (Size => Integer (Message_Size));

      begin
         Message.Kind := Code_To_Kind (Message_Code);

         --  We only act upon the presense of an error in this function's body.
         --  The original plan was to act upon hang up messages too, but Asterisk
         --  never actually sends such messages to us.

         if Message.Kind = Kind_Error then
            Connection.Hung_Up := True;

            case Payload (Message) (1) is
               when Hang_Up_Error_Code       => raise Hang_Up_Error;
               when Frame_Forward_Error_Code => raise Frame_Forward_Error;
               when Memory_Error_Code        => raise Memory_Error;
               when others                   => raise Unknown_Error;
            end case;
            --  ! Does the socket need to be closed here?
         end if;

         Byte_Array'Read (Channel, Message.Payload);
         GNAT.Sockets.Free (Channel);
         return Message;
      end;

   exception
      when Ada.IO_Exceptions.End_Error =>
         GNAT.Sockets.Free (Channel);
         Connection.Hung_Up := True;
         return No_Message;

   end Receive_Message;

   ----------------
   -- Send_Audio --
   ----------------

   procedure Send_Audio
     (Connection : in out Connection_Type;
      Audio      : in     Byte_Array)
   is
      Payload_Size : Short_Integer;
      Channel      : GNAT.Sockets.Stream_Access;

   begin
      if Audio'Length > Short_Integer'Last then
         raise Constraint_Error with "Size of audio data passed to "
           & "`Send_Audio`" & Integer'Image (Audio'Length)
           & ", is larger than the maximum allowed message size of "
           & "65,535 bytes. The audio must be chunked into this size "
           & "or smaller before being sent.";

      elsif Connection.Hung_Up then
         return;
      end if;

      Channel := GNAT.Sockets.Stream (Connection.Socket);

      Byte'Write (Channel, Audio_Message_Code);
      --  Message kind is always audio.

      Payload_Size := Audio'Length;
      GNAT.Byte_Swapping.Swap2 (Payload_Size'Address);
      Short_Integer'Write (Channel, Payload_Size);
      --  Similar to the `Receive_Message` function, the endianess
      --  of the payload size must be swapped to big endian.

      Byte_Array'Write (Channel, Audio);

      GNAT.Sockets.Free (Channel);

   exception
      when Ada.IO_Exceptions.End_Error =>
         GNAT.Sockets.Free (Channel);
         Connection.Hung_Up := True;

   end Send_Audio;

   --------------
   -- Get_UUID --
   --------------

   function Get_UUID (Connection : in Connection_Type) return Byte_Array
   is (Connection.UUID);

   -------------------
   -- Get_Peer_Name --
   -------------------

   function Get_Peer_Name
     (Connection : in Connection_Type) return GNAT.Sockets.Sock_Addr_Type
   is (Connection.Peer_Name);

   -----------------
   -- Has_Hung_Up --
   -----------------

   function Has_Hung_Up (Connection : in Connection_Type) return Boolean
   is (Connection.Hung_Up);

   -------------
   -- Hang_Up --
   -------------

   procedure Hang_Up (Connection : in out Connection_Type) is
   begin
      if not Connection.Hung_Up then
         Connection.Hung_Up := True;
         GNAT.Sockets.Close_Socket (Connection.Socket);
      end if;
   end Hang_Up;

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket
     (Connection : in Connection_Type) return GNAT.Sockets.Socket_Type
   is (Connection.Socket);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server     : in out Server_Type;
      Address    : in     GNAT.Sockets.Sock_Addr_Type;
      Queue_Size : in     Natural := 15)
   is
      use GNAT.Sockets;

   begin
      Create_Socket
        (Socket => Server.Socket,
         Family => Address.Family,
         Mode   => Socket_Stream);

      Bind_Socket (Server.Socket, Address);
      Listen_Socket (Server.Socket, Queue_Size);
   end Create_Server;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (Server     : in     Server_Type;
      Connection : in out Connection_Type)
   is
      UUID_Message : Message_Type (Size => 16);
      --  The UUID message is always the first to arrive, and is always
      --  the same size (16 bytes), so we can declare it ahead of time.

   begin
      GNAT.Sockets.Accept_Socket
        (Server  => Server.Socket,
         Socket  => Connection.Socket,
         Address => Connection.Peer_Name);

      Connection.Hung_Up := False;
      --  Incase `Connection_Type` instances get re-used, reset their
      --  `Hung_Up` component to false after a successfull call to `Accept`.

      UUID_Message := Receive_Message (Connection);
      Connection.UUID := Payload (UUID_Message);

   exception
      when GNAT.Sockets.Socket_Error =>
         Connection.Hung_Up := True;

   end Accept_Connection;

   ------------------
   -- Close_Server --
   ------------------

   procedure Close_Server (Server : in Server_Type) is
   begin
      GNAT.Sockets.Close_Socket (Server.Socket);
   end Close_Server;

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket
     (Server : in Server_Type) return GNAT.Sockets.Socket_Type
   is (Server.Socket);

end Asterisk.AudioSocket;
