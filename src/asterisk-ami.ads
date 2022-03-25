with Ada.Calendar;
with Ada.Task_Termination;

with GNAT.Sockets;

with Agnostic_IO;

private with Ada.Unchecked_Deallocation;
private with Ada.Exceptions;

private with Socket_AIO;


package Asterisk.AMI is

   AMI_Error : exception;
   --  The exception raised when any known error from this package's
   --  subprograms is encountered (except for client disconnection).
   --  Subprograms that rause this exception, and for what reasons they
   --  do so, have it indicated in their descriptions below.
   --  ! Opt for more specific variations, or use Boolean out parameters?

   AMI_Event_Loop_Terminated_Error : exception;
   --  ! Consider renaming to `AMI_Client_Disconnected_Error`.

   type Message_Type (Header_Count : Natural) is private;
   --  Represents data received from Asterisk, used when for AMI events and
   --  action responses. While Instances of this type are never manually
   --  initialized by the user, it is an indefinite type, meaning that when
   --  receiving action responces, a declaration of a variable using this
   --  type will need to be initialized by the return value of the
   --  'Send_Action' function.

   No_Response : constant Message_Type;
   --  Used exclusively as a return value for the function `Send_Action`,
   --  indicates that no response from Asterisk was received before the
   --  timeout.

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
   --  Used to represent AMI actions sent to Asterisk. The number of headers
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
     (Address_Family : GNAT.Sockets.Family_Type)
   is abstract tagged limited private;

   type Client_Access is access all Client_Type'Class;

   --  Represents a connection to an Asterisk server's AMI interface,
   --  pemitting the sending of actions, receipt of their responses and of
   --  standalone AMI events. This type is meant to be dervived from in order
   --  to receive events via a user provided callback, see `Event_Callback`
   --  near the end of this specification's public part. This type is
   --  instantiated via a successfull login to an AMI server. Instances of
   --  this type can safely be shared between multiple tasks (threads).

   --------------------------------------------
   --  NOTE: Handling Exceptions in Subtasks --
   --------------------------------------------

   --  The `Client_Type` utilizes an additional task (thread) to implement the
   --  event loop. It is strongly encouraged that you setup a task termination
   --  handler via the functionality provided in the language defined package
   --  `Ada.Task_Termination`. This is recommended because, if an exception is
   --  raised inside the `Event_Callback` primitive, the program will keep
   --  running without any indication of failure until the environment task
   --  terminates too. Task termination handlers provide a means of getting
   --  notified when a task terminates and for what reason it did so.

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

   procedure Logoff (Self : in out Client_Type);
   --  Disconnects a `Client_Type` instance from the Asterisk server it's
   --  currently connected to. After a call to this primitive, the client
   --  will no longer be able to send actions, receive actions responses or
   --  general AMI events until it is reconnected to an AMI server via `Login`.

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

   procedure Event_Callback
     (Self  : in out Client_Type;
      Name  : in     String;
      Event : in     Message_Type) is null;
   --  To be overridden by the user if needed. This procedure is invoked in
   --  a separate task (the client's event loop) whenever an AMI event arrives
   --  from Asterisk. If not overridden, events are silently ignored.

   type Cause_Of_Disconnection is (Abnormal_Disconnection, Deliberate_Logoff);

   subtype Read_Error_Kind is Agnostic_IO.Read_Error_Kind;

   procedure Client_Disconnection_Callback
     (Self  : in out Client_Type;
      Cause : in     Cause_Of_Disconnection;
      Error : in     Read_Error_Kind) is abstract;
   --  Must be overridden when deriving from `Client_Type`. This primitive
   --  is invoked whenever the client instance loses its connection to the
   --  AMI server it was previously logged into. The cause of the disconnection
   --  is provided in `Cause`, if it was due to a socket error, it is provided
   --  in `Error`.

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

   Never_Disconnected : constant Ada.Calendar.Time;
   --  Used to indicate that a client has not been/was never disconnected
   --  from it's AMI server.

   function Get_Disconnection_Time
     (Self : in Client_Type) return Ada.Calendar.Time with Inline;
   --  Returns an instance of `Ada.Calendar.Time` containing the time that
   --  the client was disconnected from it's AMI server at. This disconnection
   --  time can be either from a deliberate call to `Logoff`, or due to an
   --  unexpected loss of communication with the server. If a disconnection
   --  hasn't occurred since the last call to `Login`, the value
   --  `Never_Disconnected` is returned.


---------------------------
-- Start of Private part --
---------------------------

private

   type Message_Type (Header_Count : Natural) is record
      Fields : Unbounded_String_Array (1 .. Header_Count);
      Values : Unbounded_String_Array (1 .. Header_Count);
   end record;

   type Message_Access is access Message_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Name   => Message_Access,
      Object => Message_Type);

   No_Response : constant Message_Type :=
     (Header_Count =>  0,
      others       => <>);

   subtype Action_ID_Range is Natural range 1 .. 256;

   type Action_ID_Array is array (Action_ID_Range) of Action_ID_Range;

   type Action_Type (Header_Count : Natural) is limited record
      Fields : Unbounded_String_Array (1 .. Header_Count);
      Values : Unbounded_String_Array (1 .. Header_Count);

      Header_Index   : Positive := 1;
      Wants_Response : Boolean  := False;
      ID             : Natural  := 0;
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
   private
       Stored_Response  : Message_Access;
       Response_Arrived : Boolean := False;
   end Response_Event_Type;

   type Response_Event_Array is
     array (Action_ID_Range) of Response_Event_Type;

   Never_Disconnected : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
     (Year  => 1970,
      Month => 1,
      Day   => 1);

   task type Event_Loop_Type is
      entry Start (Client : in out Client_Type);
      entry Await_Cleanup;
   end Event_Loop_Type;

   type Event_Loop_Access is access Event_Loop_Type;

   use GNAT.Sockets;

   type Client_Type
     (Address_Family : GNAT.Sockets.Family_Type)
   is abstract tagged limited record
      Channel : Socket_AIO.Socket_Channel_Type
        (Buffer_Start_Size => 500,
         Line_Ending       => Socket_AIO.Carriage_Return_Line_Feed,
         Recursion_Limit   => 4);

      Server_Address     : Sock_Addr_Type (Address_Family);
      Socket_Write_Mutex : Critical_Section;

      AMI_Version : Ada.Strings.Unbounded.Unbounded_String;

      Deliberate_Logoff    : Boolean           := False;
      Disconnect_Timestamp : Ada.Calendar.Time := Never_Disconnected;

      Event_Loop           : Event_Loop_Type;
      Termination_Handler  : Ada.Task_Termination.Termination_Handler;
      Event_Loop_Exception : Ada.Exceptions.Exception_Occurrence;

      Action_Responses : Response_Event_Array;
      Action_IDs       : Action_ID_Manager;
   end record;

end Asterisk.AMI;

