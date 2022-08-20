with Ada.Streams;

with GNAT.Sockets;

private with Ada.Unchecked_Deallocation;

private with Buffered_Streams.Unique_Buffer;
private with Buffered_Streams.Socket_Streamer;


package Asterisk.AMI is

--  ! Test ability to re-login to a server after a disconnection,
--  make sure that the way in which it is done is realistic/not
--  convoluted and isn't tied to a very specific pattern.

--  ! Strongly consider using the socket option-based timeout option instead
--  of the `select` based one, which seems to produce an un-handleable
--  exception when it interrupts the system call.

--  ! Strongly consider adding a special operator that provides
--  "syntactic sugar" for adding fields/values to an `Action_Type`.

--  ! Consider renaming/declaring `Ada.Streams.Stream_Element_Offset`
--  directly in this package specification.

   AMI_Error : exception;
   --  The exception raised when any known error from this package's
   --  subprograms is encountered (except for client disconnection).
   --  Subprograms that rause this exception, and for what reasons they
   --  do so, have it indicated in their descriptions below.
   --  ! Opt for more specific variations, or use Boolean out parameters?

   --AMI_Recursion_Limit_Error : exception renames
     --Buffered_Streams.Unique_Buffer.BS_Recursion_Limit_Error;
   --  ! What is the best way to handle giving the user access to this
   --  exception? Generics complicate things a bit...

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
   --  standalone AMI events. This type is meant to be dervived from in order
   --  to receive events via a user provided callback, see `Event_Callback`
   --  near the end of the specification's public part. This type is
   --  instantiated via a successfull login to an AMI server. Instances of
   --  this type can safely be shared between multiple tasks (threads).

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
      Timeout  : in     Duration := 5.0);
   --  Connects a `Client_Type` instance to the specified AMI server using
   --  the supplied credentials. Upon successfull login, the event loop is
   --  started, allowing the client to send actions, receive action responses
   --  and general AMI events. In the scenario that the client is disconnected
   --  from the AMI server, this procedure may be invoked again to re-connect
   --  the client. If invoked while the client is already connected to an AMI
   --  server, the client is first disconnected from the server it's currently
   --  logged into.

   --  ! Consider creating invidiual exceptions for each of the below cases.

   --  Raises 'AMI_ERROR' under the following circumstances:

   --     1. The host specified in `Address` was unreachable.

   --     2. The client wasn't able to read the server's version infomation.

   --     3. No response was received for the login action before the timeout.

   --     4. The client's event loop has terminated due to a handled
   --        exception in the event callback.

   --     5. The login credentials were incorrect, or some other
   --        Asterisk-related error occurred.

   procedure On_Event
     (Self  : in out Client_Type;
      Name  : in     String;
      Event : in     Message_Type) is null;
   --  To be overridden by the user if needed. This procedure is invoked
   --  after a call to `Await_Message` once an AMI event has arrived from
   --  Asterisk. If not overridden, events are silently ignored.

   --procedure On_Disconnect (Self : in out Client_Type) is null;
   --  To be overridden by the user if needed. This procedure is invoked if
   --  the client gets disconnected from the AMI server during a call to
   --  `Await_Message`. A disconnect can occur because of a deliberate logoff,
   --  a network issue, or socket closure from the server's end.
   --  ! Consider forcing user's to override, along with discerning between
   --    deliberate and accidental disconnects.
   --  ! Pass buffered streams exception back via an out parameter?
   --  ! Consider getting rid of completely and just determining disconnects
   --  via exceptions raised or returned from `Await_Message`?


   procedure Await_Message
     (Self    : in out Client_Type'Class;
      Timeout : in     Duration := Duration'Last);
   --  ! Raise buffered streams exception from this subprogram?

   procedure Send_Action
     (Self   : in out Client_Type;
      Action : in out Action_Type) with Inline;
   --  Sends an `Action_Type` instance to the Asterisk server that the client
   --  is currently connected to, ignoring any response the action might send
   --  back. This primitive may be used inside the event loop task
   --  (invoked inside the event callback) without causing a deadlock.

   function Send_Action
     (Self    : in out Client_Type;
      Action  : in out Action_Type;
      Timeout : in     Duration := 5.0) return Message_Type;
   --  Similar to the above procedure, but waits `Timeout` seconds for a
   --  response to `Action`. If a response is received within the timeout,
   --  then it is the function's return value, otherwise the constant
   --  `No_Response` is returned. Receipt of an action response cannot be performed
   --  inside the `Event_Callback` primitive, or any subprogram that is invoked
   --  inside it. Attempting to do this will result in the exception `AMI_Error`
   --  being raised.

   procedure Logoff (Self : in out Client_Type);
   --  Disconnects a `Client_Type` instance from the Asterisk server it's
   --  currently connected to. After a call to this primitive, the client
   --  will no longer be able to send actions, receive actions responses or
   --  general AMI events until it is reconnected to an AMI server via `Login`.

   function Is_Connected (Self : in Client_Type) return Boolean with Inline;
   --  Returns `True` if `Client` is still connected to the server that was
   --  specified in the call to `Login`, `False` if not. The only way for a
   --  user to disconnect `Client` is via a call to `Logoff`, all other
   --  disconnects that occur are due to network related errors, or a
   --  forcefull logoff triggered by the server itself.

   function Get_Server_Address
     (Self : in Client_Type) return GNAT.Sockets.Sock_Addr_Type;
   --  Returns the socket address of the AMI server that the client was last
   --  connected to.

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

      Socket_Write_Mutex : Critical_Section;
      --  ! Replace with atomic spin lock?
      Connected          : Boolean        := False;
      Server_Address     : Sock_Addr_Type :=
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

