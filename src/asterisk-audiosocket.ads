with GNAT.Sockets;


package Asterisk.AudioSocket is

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   type Byte_Array is array (Positive range <>) of Byte;


   type Message_Kind is
     (Kind_Hang_Up,
      Kind_UUID,
      Kind_Silence,
      Kind_Audio,
      Kind_Error,
      Kind_Unknown);

   Hang_Up_Error       : exception;
   Frame_Forward_Error : exception;
   Memory_Error        : exception;
   Unknown_Error       : exception;
   --  ! Strongly consider replacing with enumeration types, which would then
   --    warrent the creation of an `Error/Error_Code` function.

   type Message_Type (Size : Natural) is private;

   No_Message : constant Message_Type;

   function Kind (Message : in Message_Type) return Message_Kind;

   function Payload (Message : in Message_Type) return Byte_Array;


   type Connection_Type (Address_Family : GNAT.Sockets.Family_Type) is
     limited private;
   --  ! Consider renaming to `AudioSocket_Type`,
   --    `Connection_Type` or `Channel_Type`.

   function Receive_Message
     (Connection : in out Connection_Type) return Message_Type;

   procedure Send_Audio
     (Connection : in out Connection_Type;
      Audio      : in     Byte_Array);

   No_UUID : constant Byte_Array := (1 .. 16 => 0);

   function Get_UUID (Connection : in Connection_Type) return Byte_Array;

   function Get_Peer_Name
     (Connection : in Connection_Type) return GNAT.Sockets.Sock_Addr_Type;

   function Has_Hung_Up (Connection : in Connection_Type) return Boolean;

   procedure Hang_Up (Connection : in out Connection_Type);

   function Get_Socket
     (Connection : in Connection_Type) return GNAT.Sockets.Socket_Type;


   type Server_Type is limited private;

   procedure Create_Server
     (Server     : in out Server_Type;
      Address    : in     GNAT.Sockets.Sock_Addr_Type;
      Queue_Size : in     Natural := 15);

   procedure Accept_Connection
     (Server     : in     Server_Type;
      Connection : in out Connection_Type);

   procedure Close_Server (Server : in Server_Type);

   function Get_Socket (Server : in Server_Type) return GNAT.Sockets.Socket_Type;

private

   type Message_Type (Size : Natural) is record
      Kind    : Message_Kind;
      Payload : Byte_Array (1 .. Size) := (1 .. Size => 0);
   end record;

   No_Message : constant Message_Type :=
     (Size    => 0,
      Kind    => Kind_Unknown,
      Payload => <>);

   type Connection_Type (Address_Family : GNAT.Sockets.Family_Type) is record
      Socket    : GNAT.Sockets.Socket_Type;
      Peer_Name : GNAT.Sockets.Sock_Addr_Type (Address_Family);
      Hung_Up   : Boolean              := False;
      UUID      : Byte_Array (1 .. 16) := No_UUID;
   end record;

   type Server_Type is record
      Socket : GNAT.Sockets.Socket_Type;
   end record;

   --  Underlying message codes.
   Hang_Up_Message_Code : constant Byte := 16#00#;
   UUID_Message_Code    : constant Byte := 16#01#;
   Silence_Message_Code : constant Byte := 16#02#;
   Audio_Message_Code   : constant Byte := 16#10#;
   Error_Message_Code   : constant Byte := 16#FF#;

   --  Underlying error codes.
   None_Error_Code          : constant Byte := 16#00#;
   Hang_Up_Error_Code       : constant Byte := 16#01#;
   Frame_Forward_Error_Code : constant Byte := 16#02#;
   Memory_Error_Code        : constant Byte := 16#04#;


end Asterisk.AudioSocket;

