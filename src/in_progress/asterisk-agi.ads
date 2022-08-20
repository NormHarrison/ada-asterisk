with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Agnostic_IO;


package Asterisk.AGI is

   AGI_Invalid_Command_Error : exception;  --  510
   --  The AGI command doesn't exist.
   AGI_Command_Usage_Error   : exception;  --  520
   --  The arguments passed to the AGI command were incorrect.
   AGI_Channel_Hangup_Error  : exception;  --  511
   --  The AGI command cannot be used on a channel that's been hungup
   --  (a dead channel).
   AGI_Unknown_Error         : exception;  --  ?
   --  AGI execution status was set to an unknown value or no
   --  response was sent back.

   --  These four exceptions represent errors that can occur before execution
   --  of the AGI command itself (i.e. not the result of the command after it
   --  finished). When no exception is raised, AGI was able to execute the
   --  command; however, this does NOT mean that the command itself succeeded
   --  at whatever it was meant to do. The out parameter `Result_Code` of the
   --  `Command` subprograms indicate this.

   type Environment_Variable_Name is
     (AGI_Network,     AGI_Network_Script,
      AGI_Request,     AGI_Channel,        AGI_Language,
      AGI_Type,        AGI_UniqueID,       AGI_Version,
      AGI_CallerID,    AGI_CallerIDName,   AGI_CallingPres,
      AGI_CallingANI2, AGI_CallingTON,     AGI_CallingTNS,
      AGI_DNID,        AGI_RDNIS,          AGI_Context,
      AGI_Extension,   AGI_Priority,       AGI_Enhanced,
      AGI_AccountCode, AGI_ThreadID);
   --  The initial AGI environment variables provided by Asterisk upon session
   --  initialization. Used to retrieve a variable's corresponding value via the
   --  "Variable" subprograms.

   type Base_Session_Type
     (Argument_Count             : Natural;
      Supress_Hang_Up_Exceptions : Boolean)
   is abstract tagged limited private;
   --  Represents an AGI session with Asterisk. The discriminant
   --  `Argument_Count` should match the number of arguments in the call to
   --  `AGI()` in the Asterisk dialplan. This value may also be used to specify
   --  the maximum number of arguments to accept, and any unfilled argument
   --  positions will contain null strings. `Supress_Hang_Up_Exceptions`
   --  determines whether or not executing a command that can't operate on
   --  a hung up (dead) channel will raise an exception. This only applies
   --  to FastAGI sessions by default, as Asterisk terminates process-based
   --  AGI programs as soon as the channel hangs up. However, this can be
   --  changed by setting the `AGISIGHUP` dialplan variable to `no`.
   --  This type is not task (thread) safe.

   function Variable
     (Self : in Base_Session_Type;
      Name : in Environment_Variable_Name) return String with Inline;
   --  Returns an AGI environment variable (a value provided by Asterisk in
   --  the initial state message).

   function Variable
     (Self : in Base_Session_Type;
      Name : in Environment_Variable_Name) return Unbounded_String with Inline;
   --  Same as above, but returns the value as an
   --  `Ada.Strings.Unbounded.Unbounded_String`.

   function Argument
     (Self   : in Base_Session_Type;
      Number : in Positive) return String with Inline;
   --  Returns a session argument (a value manually added to the initial
   --  message via the dialplan `AGI()` call). Raises `Constraint_Error`
   --  if the index is outside the range of arguments specified during package
   --  instantiation. The number of arguments can be checked via the
   --  `Argument_Count` discriminant of any session instance.

   function Argument
     (Self   : in Base_Session_Type;
      Number : in Positive) return Unbounded_String with Inline;
   --  Same as above, but returns the value as an
   --  `Ada.Strings.Unbounded.Unbounded_String`.

   function Command
     (Self : in out Base_Session_Type;
      Data : in     String) return String;
   --  Sends the command specified in `Data` to the connected Asterisk channel.
   --  Returns the whatever the command does, otherwise a null string is
   --  returned. The status code of the command's execution can be checked via
   --  the `Last_Status_Code` primitive.

   --  This subprogram can raise the following exceptions:

   --  - `AGI_Invalid_Command_Error`

   --  - `AGI_Channel_Hungup_Error`

   --  - `AGI_Command_Usage_Error`

   --  - `AGI_Unknown_Error`

   function Command
     (Self : in out Base_Session_Type;
      Data : in     String) return Unbounded_String with Inline;
   --  Same as above, but returns the command result as an `Unbounded_String`.

   procedure Command
     (Self          : in out Base_Session_Type;
      Data          : in     String;
      Skip_Response : in     Boolean := False) with Inline;
   --  Same as the first `Command` function, but is meant to be used with AGI
   --  commands that return no value. When `Skip_Response` is `True`, the
   --  command's response is not awaited, preventing long running AGI commands
   --  from blocking execution in Ada. Care must be taken when doing this:
   --  1. You will not know whether the command executed successfully.
   --  2. With FastAGI, the socket's receive buffer may fill up and cause a
   --  disconnect to occur (which can be mitigated by increasing the receive
   --  buffer size via the socket option `Receive_Buffer`).

   procedure Hang_Up (Self : in out Base_Session_Type) with Inline;
   --  Hangs up the channel represented by this session. After this
   --  procedure is invoked, only AGI commands that can "run dead"
   --  (execute on hung up channels) will successfully execute.

   function Has_Hung_Up (Self : in Base_Session_Type) return Boolean
     with Inline;
   --  Indicates whether or not the channel represented by the session instance
   --  has been hung up. Once this function returns `True`, only AGI commands
   --  that can "run dead" (execute on hung up channels) will successfully
   --  execute. Hang ups occur either by executing the above `Hang_Up`
   --  procedure, or from Asterisk sending a "HANGUP" message (FastAGI) or the
   --  signal "SIGHUP" (Process-based AGI).

   function Last_Result_Code (Self : in Base_Session_Type) return Integer
     with Inline;
   --  Returns the result code produced by the most recently executed command.
   --  ! This has the possiblity of becoming inaccurate when used in a multi-
   --    threaded environment. Consider providing access to this value via an
   --    `out` parameter on the `Command` subprograms instead.

   ---------------------------------
   --  NOTE: Result code meanings --
   ---------------------------------

   --  Result codes between AGI commands are not very consistent. -1 seems to
   --  always indicate failure, 1 usually indicates success and 0's meaning
   --  alternates. Advice: only take the command result code seriously if
   --  the command you're executing publically documents its result codes.
   --  If it doesn't mention what result codes it returns, it's probably best
   --  to ignore it. You can check this information on the Asterisk wiki
   --  (https://wiki.asterisk.org/wiki/display/AST/Asterisk+18+AGI+Commands)
   --  or by running `agi show commands topic <command>` in the Asterisk CLI.

   ---------------------------
   -- Start of Private part --
   ---------------------------

private

   --  The format of AGI command responses are:
   --  `<agi_code> result=<command_code> (<response_data_from_command>)`

   Last_Variable_Index : constant Positive :=
     Environment_Variable_Name'Pos (Environment_Variable_Name'Last);
   --  Used in operations where values of the 'Environment_Variable_Name'
   --  enumeration type must be compared with regular numerical types.
   --  Keep in mind that enumeration values are zero-based.

   --type Stream_Direction is (Input, Output);

   --type Stream_Access_Array is array (Stream_Direction) of Stream_Access;

   type Environment_Variable_Array is
     array (Environment_Variable_Name) of Unbounded_String;

   type Base_Session_Type
     (Argument_Count             : Natural;
      Supress_Hang_Up_Exceptions : Boolean)
   is abstract tagged limited record
      Agnostic_Channel  : Agnostic_IO.Root_Channel_Access;
      Skipped_Responses : Natural := 0;

      Variables   : Environment_Variable_Array;
      Arguments   : Unbounded_String_Array (1 .. Argument_Count);
      Result_Code : Integer := -1;
      Hung_Up     : Boolean := False;
   end record;

   procedure Parse_Initial_Message
     (Self                : in out Base_Session_Type;
      Starting_Variable   : in     Environment_Variable_Name;
      Last_Variable_Index : in     Natural);

end Asterisk.AGI;
