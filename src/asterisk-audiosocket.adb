with Ada.Text_IO;       use Ada.Text_IO;
with Ada.IO_Exceptions;

with GNAT.Byte_Swapping;


package body Asterisk.AudioSocket is

   Code_To_Kind : constant array (Byte) of Message_Kind :=
     (Hang_Up_Message_Code => Kind_Hang_Up,
      UUID_Message_Code    => Kind_UUID,
      Silence_Message_Code => Kind_Silence,
      Audio_Message_Code   => Kind_Audio,
      Error_Message_Code   => Kind_Error,
      others               => Kind_Unknown);
   --  ! What assembly does this generate? Should we avoid using an array the
   --  size of `Byte`? Will this help efficiency?


--------------------------------------
-- Begginning of public subprograms --
--------------------------------------

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
      Message_Header : Byte_Array (1 .. 3);

   begin
      if Connection.Hung_Up then
         return No_Message;
      end if;

      Byte_Array'Read (Connection.Stream, Message_Header);
      --  Read the 3 byte header of the message:
      --    - 1 byte for the message type.
      --    - 2 bytes for the message length.

      GNAT.Byte_Swapping.Swap2 (Message_Header (2)'Address);
      --  The size is sent in big endian byte order and must
      --  be converted before being used to read the payload.

      declare
         Length : Unsigned_16;
         for Length'Address use Message_Header (2)'Address;
         Message : Message_Type (Size => Length);

      begin
         Message.Kind := Code_To_Kind (Message_Header (1));

         --  We only act upon the presense of an error in this function's body.
         --  The original plan was to act upon hang up messages too, but Asterisk
         --  never actually sends such messages to us.

         if Message.Kind = Kind_Error then

            Connection.Hung_Up := True;
            GNAT.Sockets.Close_Socket (Connection.Socket);
            GNAT.Sockets.Free (Connection.Stream);

            case Message.Payload (1) is
               when Hang_Up_Error_Code       => raise Hang_Up_Error;
               when Frame_Forward_Error_Code => raise Frame_Forward_Error;
               when Memory_Error_Code        => raise Memory_Error;
               when others                   => raise Unknown_Error;
            end case;
         end if;

         Byte_Array'Read (Connection.Stream, Message.Payload);
         return Message;
      end;

   exception
      when Ada.IO_Exceptions.End_Error =>
         --  ! Allow to propagate, or handle GNAT.Sockets.Socket_Error too?
         --  This is usually the code that ends up setting `Hung_Up` to true.
         GNAT.Sockets.Free (Connection.Stream);
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
      Audio_Length : constant Unsigned_16 := Audio'Length;

      Message_Length : Byte_Array (1 .. 2);
      for Message_Length'Address use Audio_Length'Address;

      Message_Header : Byte_Array (1 .. 3);

   begin
      if Connection.Hung_Up then
         return;
      end if;

      Message_Header (1) := Audio_Message_Code;
      --  Message kind is always audio.

      Message_Header (2 .. 3) := Message_Length;

      GNAT.Byte_Swapping.Swap2 (Message_Header (2)'Address);
      --  Similar to the `Receive_Message` function, the endianess
      --  of the payload size must be swapped to big endian.

      Byte_Array'Write (Connection.Stream, Message_Header & Audio);

   exception
      when Ada.IO_Exceptions.End_Error =>
         Connection.Hung_Up := True;
         GNAT.Sockets.Close_Socket (Connection.Socket);
         GNAT.Sockets.Free (Connection.Stream);

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
         GNAT.Sockets.Free (Connection.Stream);
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
     (Server           : in out Server_Type;
      Address          : in     GNAT.Sockets.Sock_Addr_Type;
      Queue_Size       : in     Natural := 15;
      Do_Reuse_Address : in     Boolean := False)
   is
      use GNAT.Sockets;

   begin
      if Server.Listening then
         Close_Server (Server);
      end if;

     if Do_Reuse_Address then
        Set_Socket_Option
          (Socket => Server.Socket,
           Level  => Socket_Level,
           Option => (Name    => Reuse_Address,
                      Enabled => True));
     end if;

      Create_Socket
        (Socket => Server.Socket,
         Family => Address.Family,
         Mode   => Socket_Stream);

      Bind_Socket (Server.Socket, Address);
      Server.Address := Address;

      Listen_Socket (Server.Socket, Queue_Size);
      Server.Listening := True;
   end Create_Server;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (Server     : in     Server_Type;
      Connection : in out Connection_Type)
   is
      use type GNAT.Sockets.Stream_Access;

      UUID_Message : Message_Type (Size => 16);
      --  The UUID message is always the first to arrive, and is always
      --  the same size (16 bytes), so we can declare it ahead of time.

   begin
      GNAT.Sockets.Accept_Socket
        (Server  => Server.Socket,
         Socket  => Connection.Socket,
         Address => Connection.Peer_Name);

      if Connection.Stream /= null then
         GNAT.Sockets.Free (Connection.Stream);
      end if;

      Connection.Stream := GNAT.Sockets.Stream (Connection.Socket);
      Connection.Hung_Up := False;
      --  Incase `Connection_Type` instances get re-used, reset their `Hung_Up`
      --  component to false and free any previously allocated stream after
      --  a successfull call to `Accept`.

      --  ! If we don't let exceptions propagate, should we reset
      --  `Connection.UUID` to `No_UUID` too in case reading it below fails?

      UUID_Message := Receive_Message (Connection);
      Connection.UUID := Payload (UUID_Message);

   exception
      when GNAT.Sockets.Socket_Error =>
      --  ! Let this propagate?
         Connection.Hung_Up := True;
         GNAT.Sockets.Free (Connection.Stream);

   end Accept_Connection;

   ------------------
   -- Close_Server --
   ------------------

   procedure Close_Server (Server : in out Server_Type) is
      use type GNAT.Sockets.Family_Type;

   begin
      if Server.Listening then
         GNAT.Sockets.Close_Socket (Server.Socket);
         Server.Socket    := GNAT.Sockets.No_Socket;
         Server.Listening := False;

         Server.Address :=
           (if Server.Address_Family = GNAT.Sockets.Family_Inet then
              (Family => GNAT.Sockets.Family_Inet,
               Port   => GNAT.Sockets.No_Port,
               Addr   => GNAT.Sockets.Any_Inet_Addr)
            else
              (Family => GNAT.Sockets.Family_Inet6,
               Port   => GNAT.Sockets.No_Port,
               Addr   => GNAT.Sockets.Any_Inet6_Addr));
      end if;
   end Close_Server;

   ------------------
   -- Is_Listening --
   ------------------

   function Is_Listening (Server : in Server_Type) return Boolean
   is (Server.Listening);

   ----------------------
   -- Get_Bind_Address --
   ----------------------

   function Get_Bind_Address (Server : in Server_Type)
     return GNAT.Sockets.Sock_Addr_Type
   is (Server.Address);

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket
     (Server : in Server_Type) return GNAT.Sockets.Socket_Type
   is (Server.Socket);

end Asterisk.AudioSocket;
