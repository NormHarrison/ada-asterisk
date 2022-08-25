with Ada.Streams;

with GNAT.Sockets;

private with Ada.Unchecked_Deallocation;

private with Buffered_Streams.Unique_Buffer;
private with Buffered_Streams.Socket_Streamer;


package Asterisk.AMI is

--  ! Consider implementing `Message_Type` with a hashed map instead
--  of arrays of unbounded strings.

--  ! Consider conveying timeouts of `Await_Message` via a Boolean out
--  parameter or return value instead of via exceptions.

--  ! Consider creating a procedure specifically meant for raising exception
--  that automatically includes the address of the server that the client was
--  in contact with. This would more easily allow users to identify which
--  AMI client/server is the one having trouble when multiple are use in a
--  single application.

--  ! Strongly consider keeping the contents of the `Address` parameter
--  of the last call to `Login` inside the `Server_Address` component of
--  the `Client_Type` instance, specifically to make reconnections easier.

--  ! Strongly consider adding a special operator that provides
--  "syntactic sugar" for adding fields/values to an `Action_Type`.
--   Though this didn't work out as well as we thought, we would have
--  to make separate index components inside the `Action_Type` record
--  for fields and values. Additionally, since operators can only accept
--  two parameters, we would need two separate ones for adding fields and
--  values, each of which would have to use a different symbol since their
--  signatures would be the same. We could pass around the field and value
--  at the same time via an array, but this array would need to hold string
--  accesses or unbounded strings, which to create literal values of
--  succinctly would require another function (pass in string, an array of
--  string accesses or unbounded strings is returned).

--  ! Consider renaming/declaring `Ada.Streams.Stream_Element_Offset`
--  directly in this package specification.

   AMI_Server_Unreachable_Error  : exception;
   --  Indicates that the AMI server at the provided address can't be reached.

   AMI_Timeout_Error             : exception;
   --  Indicates that the AMI server at the provided address could be reached,
   --  but didn't respond to the packets sent before the specified timeout.

   AMI_Invalid_Credentials_Error : exception;
   --  Indicates that incorrect credentials (username/secret) were provided to
   --  the AMI server at the specified address. In rare cases, this exception
   --  may also be raised when the response sent back was unintelligible.

   AMI_Deliberate_Disconnect     : exception;
   --  Indicates that the AMI client was disconnected from the server due to
   --  a deliberate invocation of the `Logoff` procedure (i.e. not due to a
   --  disconnection by the server itself or some other network error).

   --AMI_Recursion_Limit_Error : exception renames
     --Buffered_Streams.Unique_Buffer.BS_Recursion_Limit_Error;

   --  ! What is the best way to handle giving the user access to this
   --  exception? Redeclaring a new exception to represent the one in
   --  the underlying `Buffered_Streams.Unique_Buffer` package would
   --  probably be the best idea, if we decide to represent it in this
   --  package at all.

   type Message_Type (Header_Count : Natural) is private;
   --  Represents data received from Asterisk, used for AMI events and
   --  action responses. While Instances of this type are never manually
   --  initialized by the user, it is an indefinite type, meaning that when
   --  receiving action responses initialization must performed during
   --  declaration (primarily with the return value of the function
   --  `Send_Action`).

   No_Response : constant Message_Type;
   --  Returned by the function `Send_Action` when no response from Asterisk
   --  was received before the timeout.

   function Get_Header
     (Message  : in Message_Type;
      Name     : in String;
      Instance : in Positive := 1) return String;
   --  Retrieves the value corresponding to the provided field name instance,
   --  used for inspecting received action responses and events. Returns an
   --  empty string if the provided header is not present in the message.
   --  Since some AMI messages contain duplicate header names, the optional
   --  `Instance` parameter can be used to specify which one is desired, by
   --  default the first instance is retreived.

   function Get_Field
     (Message : in Message_Type;
      Index   : in Positive) return String with Inline;
   --  Returns the field name at the specified index from a message.
   --  The lower bound is always 1, and the upper bound can be obtained via
   --  the `Header_Count` discriminant of `Message_Type`. Mainly for use when
   --  iterating over a message. The exception `Constraint_Error' is raised
   --  when an out-of-bounds index is accessed.

   function Get_Value
     (Message : in Message_Type;
      Index   : in Positive) return String with Inline;
   --  Same as the above function, but returns the value present at the
   --  specified index instead of the field name.

   type Action_Type (Header_Count : Natural) is limited private;
   --  Represent AMI actions sent to Asterisk. The number of headers
   --  the action will contain must be specified upon declaration via the
   --  `Header_Count` discriminant. This can be made more convenient via Ada's
   --  inline declaration regions (the `declare` statement). If more
   --  flexibility is desired, consider allocating instances of this type on
   --  the heap via the `new` keyword.

   procedure Set_Header
     (Action : in out Action_Type;
      Field  : in     String;
      Value  : in     String);
   --  Sets the field-value pair as a header on the provided `Action_Type`
   --  instance. Raises `Constraint_Error` if more headers are added than
   --  the amount that the action instance has room for (the `Header_Count`
   --  discriminant of `Action_Type` determines this).

   --  ! Could Pre/Post conditions for these two procedures benefit us?

   procedure Set_Header_At
     (Action : in out Action_Type;
      Index  : in     Positive;
      Field  : in     String := "";
      Value  : in     String := "");
   --  Overwrites an existing field-value pair on the provided `Action_Type`
   --  instance, useful for replacing/updating previously set values. Passing
   --  an empty string for the `Field` or `Value` parameters will leave their
   --  content as-is in the underlying `Action_Type` instance (as they should
   --  never actually be empty. This is also useful for when only the field or
   --  only the value should be updated, not both). The `Index` parameter must
   --  not be greater than the value of the `Action_Type` instance's
   --  `Header_Count` discriminant. If it is larger, the exception
   --  `Constraint_Error` is raised.

   type Client_Type
     (Address_Family   : GNAT.Sockets.Family_Inet_4_6;
      Read_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Recursion_Limit  : Natural)
   is tagged limited private;

   type Client_Access is access all Client_Type'Class;

   --  Represents a connection to an Asterisk server's AMI interface,
   --  pemitting the sending of actions, receipt of their responses and of
   --  standalone AMI events. If standalone AMI events need to be received,
   --  the type must dervived from and the `On_Event` primitive overridden.
   --  This type is instantiated via a successfull login to an AMI server.
   --  Instances of this type can safely be shared between multiple tasks
   --  (threads).

   function Get_Socket (Self : in Client_Type) return GNAT.Sockets.Socket_Type
     with Inline;
   --  Returns the underlying socket of the client, for use with
   --  subprograms from the GNAT.Sockets package (for example, setting
   --  or retreiving socket options).

   Default_AMI_Port : constant GNAT.Sockets.Port_Type := 5039;

   procedure Login
     (Self     : in out Client_Type;
      Address  : in     GNAT.Sockets.Sock_Addr_Type;
      Username : in     String;
      Secret   : in     String;
      Timeout  : in     GNAT.Sockets.Timeval_Duration := 5.0);
   --  Connects a `Client_Type` instance to the specified AMI server using
   --  the supplied credentials. Upon successfull login, actions can be sent
   --  to the server, and `Await_Message` can be invoked to receive action
   --  responses and/or receive standalone AMI events (if the `On_Event`
   --  primitive is overridden). In the scenario that the client is disconnected
   --  from the AMI server, this procedure may be invoked again to re-connect
   --  the client. If invoked while the client is already connected to an AMI
   --  server, the client is first disconnected from the server it's currently
   --  logged into.

   --  `Login` can raise the following exceptions:

   --     `AMI_Server_Unreachable_Error` when the host specified in `Address`
   --     was unreachable.

   --     `AMI_Timeout_Error` when the server at `Address` doesn't respond
   --     before the specified timeout.

   --     `AMI_Credentials_Invalid_Error` when the login credentials
   --     (username/password) were incorrect, or some other the response
   --     sent back by Asterisk was unintelligible.

   --     `GNAT.Sockets.Socket_Error` when an unexpected problem occurrs
   --     with the underlying TCP socket.

   --     Any exception inside the packge `Ada.IO_Exceptions` when an
   --     unexpected problem occurrs with the underlying stream created
   --     for the TCP socket.

   procedure On_Event
     (Self  : in out Client_Type;
      Name  : in     String;
      Event : in     Message_Type) is null;
   --  To be overridden by the user if needed. This procedure is invoked
   --  after a call to `Await_Message` once an AMI event has arrived from
   --  Asterisk. If not overridden, events are silently ignored.

   procedure Await_Message
     (Self    : in out Client_Type'Class;
      Timeout : in     GNAT.Sockets.Timeval_Duration := GNAT.Sockets.Forever);
   --  The "message pump" for the entire client, this procedure must be
   --  continously invoked in a loop to receive action responses and
   --  standalone AMI events (usually in a separate task).

   --  Hint on handling exceptions: A good pattern for exception handling
   --  with this procedure, is explicitly handling the
   --  `AMI_Deliberate_Disconnect` exception, and then creating a catch-all
   --  handler for all other exceptions (inside which the client would could
   --  be reconnected to the server).

   --  `Await_Message` can raise the following exceptions:

   --    `AMI_Deliberate_Disconnect` when `Await_Message` is invoked after
   --    the client has be deliberately disconnected from the server it was
   --    previously connected to via a call to `Logoff`.

   --    `Buffered_Streams.Unique_Buffer.Recursion_Limit_Error` when the
   --    internal stream buffer is not able to find the current message's
   --    delimiter before encountering the `Recursion_Limit` of the client
   --    instance.

   --    `GNAT.Sockets.Socket_Error` when an unexpected problem occurrs
   --    with the underlying TCP socket.

   --    Any exception inside the packge `Ada.IO_Exceptions` when an
   --    unexpected problem occurrs with the underlying stream created
   --    for the TCP socket.


   procedure Send_Action
     (Self   : in out Client_Type;
      Action : in out Action_Type) with Inline;
   --  Sends an `Action_Type` instance to the Asterisk server that the client
   --  is currently connected to, ignoring any response the action might send
   --  back. This primitive may be used inside the event loop task (invoked
   --  inside the `On_Event` callback) without causing a deadlock. If the
   --  client is not connected to any server, this procedure currently just
   --  returns silently without any indication.

   function Send_Action
     (Self    : in out Client_Type;
      Action  : in out Action_Type;
      Timeout : in     Duration := Duration'Last)
   return Message_Type;
   --  Similar to the above procedure, but waits `Timeout` seconds for a
   --  response to `Action`. If a response is received within the timeout,
   --  then it is the function's return value, otherwise the constant
   --  `No_Response` is returned. Receipt of an action response cannot be
   --  performed inside the `On_Event` primitive, or any subprogram that is
   --  invoked inside it, unless it's execution is performed inside a
   --  separate task. If the client is not connected to any server, this
   --  procedure currently just returns silently without any indication.

   procedure Logoff (Self : in out Client_Type);
   --  Disconnects a `Client_Type` instance from the Asterisk server it's
   --  currently connected to. After a call to this primitive, the client
   --  will no longer be able to send actions, receive actions responses or
   --  standalone AMI events until it is reconnected to an AMI server via
   --  `Login`.

   function Is_Connected (Self : in Client_Type) return Boolean with Inline;
   --  Returns `True` if `Client` is still connected to the server that was
   --  specified in the call to `Login`, `False` if not. The only way for a
   --  user to disconnect `Client` is via a call to `Logoff`, all other
   --  disconnects that occur are due to network related errors, or a
   --  forcefull logoff triggered by the server itself.

   function Get_Server_Address
     (Self : in Client_Type) return GNAT.Sockets.Sock_Addr_Type;
   --  Returns the socket address of the AMI server that the client is
   --  currently connected to. If the client is not currently connected to
   --  any server, returns `GNAT.Sockets.Any_Inet_Addr` or
   --  `GNAT.Sockets.Any_Inet6_Addr` in combination with
   --  `GNAT.Sockets.No_Port`.

   function Get_AMI_Version (Self : in Client_Type) return String with Inline;
   --  Returns the version number that was provided by the currently connected
   --  Asterisk AMI server during initial login. Returns an empty string if
   --  the client is not currently connected to any server.


---------------------------
-- Start of Private part --
---------------------------

private

   type Message_Type (Header_Count : Natural) is record
      Fields : Unbounded_String_Array (1 .. Header_Count);
      Values : Unbounded_String_Array (1 .. Header_Count);
   end record;
   --  ! Consider switching this type to and indefinite hashed map.

   type Message_Access is access Message_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Name   => Message_Access,
      Object => Message_Type);

   No_Response : constant Message_Type :=
     (Header_Count =>  0,
      others       => <>);

   type Action_ID_Range is range 1 .. 256;
   --  ! Consider including 0 in the above range to use as an
   --  "invalid" value, or create a subtype compatible with `Natural`.
   --  Though this would increase the size of the type (not that this matters).
   --for Action_ID_Range'Size use 8;

   type Action_ID_Array is array (Action_ID_Range) of Action_ID_Range;
   --  ! Make element type `Natural`?

   type Action_Type (Header_Count : Natural) is limited record
      Fields : Unbounded_String_Array (1 .. Header_Count);
      Values : Unbounded_String_Array (1 .. Header_Count);

      Header_Index   : Positive        := 1;
      Wants_Response : Boolean         := False;
      ID             : Action_ID_Range := 1;
   end record;

   protected type Action_ID_Manager is
      procedure Initialize;
      function Index_Position return Natural;
      entry Acquire_ID (ID : out Action_ID_Range);
      procedure Release_ID (ID : in Action_ID_Range);
   private
      Next_Free_ID_Index : Natural := 0;
      Action_IDs         : Action_ID_Array;
   end Action_ID_Manager;

   protected type Response_Event_Type is
      entry Await_Response (Response : out Message_Access);
      procedure Set_Response (Response : in Message_Access);
      procedure Reset;
   private
       Stored_Response  : Message_Access;
       Response_Arrived : Boolean := False;
   end Response_Event_Type;

   type Response_Event_Array is array (Action_ID_Range) of Response_Event_Type;

   use GNAT.Sockets;

   package Buffered_Strings is new Buffered_Streams.Unique_Buffer
     (Index_Type   => Positive,
      Element_Type => Character,
      Array_Type   => String,
      "+"          => "+");

   type Client_Type
     (Address_Family   : GNAT.Sockets.Family_Inet_4_6;
      Read_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Recursion_Limit  : Natural)
   is tagged limited record

      Stream          : Buffered_Streams.Socket_Streamer.TCP_Stream_Type;
      Buffered_Socket : Buffered_Strings.Unique_Buffer_Type
        (Read_Buffer_Size  => Read_Buffer_Size,
         Write_Buffer_Size => 0,
         Recursion_Limit   => Recursion_Limit);
         --  ! We may want to utilize the write buffer to avoid having to
         --  manually concatenate messages on the stack.

      Last_Await_Timeout : GNAT.Sockets.Timeval_Duration :=
        GNAT.Sockets.Forever;

      Socket_Write_Mutex    : Critical_Section;
      --  ! Replace with atomic spin lock?

      Connected             : Boolean        := False;
      Deliberate_Disconnect : Boolean        := False;
      Server_Address        : Sock_Addr_Type :=
        (case Address_Family is
            when Family_Inet =>
               (Family => Family_Inet,
                Port   => No_Port,
                Addr   => Any_Inet_Addr),

            when Family_Inet6 =>
               (Family => Family_Inet6,
                Port   => No_Port,
                Addr   => Any_Inet6_Addr));

      AMI_Version : Ada.Strings.Unbounded.Unbounded_String;

      Action_Responses : Response_Event_Array;
      Action_IDs       : Action_ID_Manager;
   end record;

end Asterisk.AMI;

