with GNAT.Sockets;


package Asterisk.AudioSocket is

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   type Byte_Array is array (Unsigned_16 range <>) of Byte;
   --  All audio data included in AudioSocket messages is represented by
   --  a simple array of bytes of a variable size. When initiating an
   --  AudioSocket connection via the `AudioSocket()` dialplan application,
   --  audio will always be sent to the server in 16-bit signed linear
   --  8KHz little-endian PCM. However, when initated via a call to `Dial()`
   --  using the `AudioSocket` channel driver, audio is sent encoded in
   --  whatever codec was agreed upon between the two channels before the
   --  call began.


   type Message_Kind is
     (Kind_Hang_Up,
      --  Indicate to Asterisk that the channel should be hungup.
      --  Flow: Us -> Asterisk.

      Kind_UUID,
      --  The message payload contains the UUID provided when initiated the
      --  AudioSocket connection from within Asterisk.
      --  Flow: Us <- Asterisk.

      Kind_Silence,
      --  The message payload contains silence (! All 0s? No data? Is this sent
      --  to us, or do we send this to Asterisk?).
      --  Flow: ???

      Kind_Audio,
      --  The message payload contains audio data from the connected channel,
      --  the format of this audio data depends on how the AudioSocket
      --  connection was initiated in Asterisk (see note under `Byte_Array`).
      --  Flow: Us <-> Asterisk.

      Kind_Error,
      --  The message payload contains an error code from Asterisk.
      --  Flow: Us <- Asterisk.

      Kind_Unknown);  --  ! Consider renaming to `Kind_None`.
      --  no message could be read from the AudioSocket, usually because the
      --  connection was closed/hung up. Rarely, this can also indicate that
      --  Asterisk sent an uknown message type.
      --  Flow: Us <- Asterisk.

   Hang_Up_Error       : exception;
   Frame_Forward_Error : exception;
   Memory_Error        : exception;
   Unknown_Error       : exception;
   --  Exceptions that represent the different error codes that Asterisk
   --  can send.
   --  ! Strongly consider replacing with enumeration types, which would then
   --    warrent the creation of an `Error/Error_Code` function.

   type Message_Type (Size : Unsigned_16) is private;
   --  Represents an AudioSocket message.
   --  ! Use `Unsigned_16` instead of `Natural`?

   No_Message : constant Message_Type;
   --  ! Consider getting rid of this, we only return it when a non
   --  AudioSocket-related exception occurrs.
   --  Indicates an AudioSocket message with no content, either because an
   --  non-AudioSocket related exception occurred while trying to read a
   --  message, or because the connection has been closed.

   function Kind (Message : in Message_Type) return Message_Kind with Inline;
   --  Returns the kind of the message.

   function Payload (Message : in Message_Type) return Byte_Array with Inline;
   --  Returns the payload of the message, always audio data or an error code.


   type Connection_Type (Address_Family : GNAT.Sockets.Family_Inet_4_6) is
     limited private;
   --  Represents an AudioSocket connection between the server (us) and
   --  Asterisk (the client).
   --  ! Consider renaming to `AudioSocket_Type`,
   --    `Connection_Type` or `Channel_Type`.

   function Receive_Message
     (Connection : in out Connection_Type) return Message_Type;
   --  Receives 

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


   type Server_Type
     (Address_Family : GNAT.Sockets.Family_Inet_4_6)
   is limited private;

   procedure Create_Server
     (Server           : in out Server_Type;
      Address          : in     GNAT.Sockets.Sock_Addr_Type;
      Queue_Size       : in     Natural := 15;
      Do_Reuse_Address : in     Boolean := False);

   procedure Accept_Connection
     (Server     : in     Server_Type;
      Connection : in out Connection_Type);

   procedure Close_Server (Server : in out Server_Type);

   function Is_Listening (Server : in Server_Type) return Boolean;

   function Get_Bind_Address (Server : in Server_Type)
     return GNAT.Sockets.Sock_Addr_Type;

   function Get_Socket (Server : in Server_Type) return GNAT.Sockets.Socket_Type;

private

   type Message_Type (Size : Unsigned_16) is record
      Kind    : Message_Kind;
      Payload : Byte_Array (1 .. Size) := (1 .. Size => 0);
   end record;

   No_Message : constant Message_Type :=
     (Size    => 0,
      Kind    => Kind_Unknown,
      Payload => <>);

   type Connection_Type
     (Address_Family : GNAT.Sockets.Family_Inet_4_6)
   is record
      Socket    : GNAT.Sockets.Socket_Type;
      Stream    : GNAT.Sockets.Stream_Access;
      Peer_Name : GNAT.Sockets.Sock_Addr_Type (Address_Family);
      Hung_Up   : Boolean              := False;
      UUID      : Byte_Array (1 .. 16) := No_UUID;
   end record;

   type Server_Type  (Address_Family : GNAT.Sockets.Family_Inet_4_6) is record
      Socket    : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
      Address   : GNAT.Sockets.Sock_Addr_Type (Address_Family);
      Listening : Boolean := False;
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

