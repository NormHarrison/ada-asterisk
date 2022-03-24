--with Ada.Text_IO;       use Ada.Text_IO;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Task_Identification;
with Ada.Strings;
with Ada.Streams;
with Ada.Calendar.Formatting;


package body Asterisk.AMI is

   use Ada.Strings.Unbounded;

   -----------------
   -- Count_Lines --
   -----------------

   function Count_Lines (Source : in String) return Natural is
      Count : Natural := 0;

   begin
      for Index in Source'Range loop
         if Source (Index) = Line_Feed then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Lines;

   ----------------------
   -- Form_AMI_Message --
   ----------------------

   procedure Form_AMI_Message
     (Data        : in     String;
      Message_Acc :    out Message_Access)
   is
      CRLF_Set : constant Ada.Strings.Maps.Character_Set :=
        Ada.Strings.Maps.To_Set (CRLF);
      --  ! Consider moving to the package body's/specification's
      --    declaration region.

      Line_Count : constant Natural := Count_Lines (Data);
      Line_Start :          Natural := 0;
      Line_End   :          Natural;

   begin
      Message_Acc := new Message_Type (Header_Count => Line_Count);

      for Line_Number in 1 .. Line_Count loop

         Ada.Strings.Fixed.Find_Token
           (Source => Data,
            Set    => CRLF_Set,
            From   => Line_Start + 1,
            Test   => Ada.Strings.Outside,
            First  => Line_Start,
            Last   => Line_End);

         declare
            Single_Line : constant String  := Data (Line_Start .. Line_End);
            Space_Index : constant Natural := Ada.Strings.Fixed.Index
              (Source  => Single_Line,
               Pattern => " ");

         begin
            Message_Acc.Fields (Line_Number) := To_Unbounded_String
              (Single_Line (Line_Start .. Space_Index - 2));

            Message_Acc.Values (Line_Number) := To_Unbounded_String
              (Single_Line (Space_Index + 1 .. Line_End));
         end;

         Line_Start := Line_End;
      end loop;

   end Form_AMI_Message;

   ---------------------
   -- Clean_Up_Client --
   ---------------------

   procedure Clean_Up_Client (Client_Acc : in Client_Access) is
   begin
      Client_Acc.Disconnect_Timestamp := Ada.Calendar.Clock;
      if Client_Acc.Channel.Is_Connected then
         Client_Acc.Channel.Close;
      end if;
   end Clean_Up_Client;

   ---------------------
   -- Event_Loop_Type --
   ---------------------

   task body Event_Loop_Type is

      Double_CRLF_Delimiter : constant Ada.Streams.Stream_Element_Array :=
        Socket_AIO.To_Stream_Element_Array (CRLF & CRLF);
      --  ! Consider moving to the package body's/specification's
      --    declaration region.

      Parent_Client : Client_Access;
      Message_Acc   : Message_Access;

   begin
      <<Reset_Event_Loop>>

      select
         accept Start (Client : in out Client_Type) do
            Parent_Client := Client'Unrestricted_Access;
            --  This is safe to do, because the event loop task instance
            --  is declared inside of `Client`, and `Client` only goes out
            --  of scope once the event loop task has terminated, preventing
            --  the possiblity of a dangling pointer.
         end Start;
      or
         terminate;
      end select;

      Indefinite : loop

         declare
            use Agnostic_IO;

            Read_Error : Read_Error_Kind;

            Socket_Data : constant String := Parent_Client.Channel.Read
              (Delimiter => Double_CRLF_Delimiter,
               Error     => Read_Error) & CRLF;

            Action_ID : Action_ID_Range;

         begin
            if Read_Error /= Agnostic_IO.No_Error then
               Clean_Up_Client (Parent_Client);

               Parent_Client.Client_Disconnection_Callback
                 (Cause => (if Parent_Client.Deliberate_Logoff then
                               Deliberate_Logoff
                            else
                               Abnormal_Disconnection),

                  Error => (if Parent_Client.Deliberate_Logoff then
                               Agnostic_IO.No_Error
                            else
                               Read_Error));

               accept Await_Cleanup;
               goto Reset_Event_Loop;
            end if;

            Form_AMI_Message (Socket_Data, Message_Acc);

            if Get_Header (Message_Acc.all, "Event") /= "" then

               Parent_Client.Event_Callback
                 (Name  => Get_Header (Message_Acc.all, "Event"),
                  Event => Message_Acc.all);

               Free (Message_Acc);
               --  Heap allocated events are automatically deallocated
               --  after the user's callback is finished executing.

            elsif Get_Header (Message_Acc.all, "ActionID") /= "" and then
                  Get_Header (Message_Acc.all, "Response") /= ""     then

               Action_ID := Action_ID_Range'Value
                 (Get_Header (Message_Acc.all, "ActionID"));

               Parent_Client.Action_Responses
                 (Action_ID).Set_Response (Message_Acc);
               --  Heap allocated responses are copied to the stack of the
               --  task that the user invokes the function `Send_Action`
               --  from. After they are successfully received, the copy on
               --  the heap is then deallocated. Responses that don't arrive
               --  before the timeout set by the user, but still arrive
               --  later on, are deallocated the next time that the
               --  `Response_Event_Type` instance is used to receive an
               --  action's response.
            end if;
         end;

         --  Some AMI actions/commands send back events containing
         --  `Response` and `ActionID` fields too, instead of only using
         --  a `Response` message (Originate is an example). This is
         --  currently being ignored because the below conditional statement
         --  is combined via `elsif`, only ever executing one branch, even if
         --  a particular message contains `Event`, `ActionID` and `Response`
         --  fields. If this ever poses a problem with functionality, then
         --  they can be separated.

      end loop Indefinite;

   exception
      when Error : others =>
         Free (Message_Acc);
         Clean_Up_Client (Parent_Client);

         Ada.Exceptions.Save_Occurrence
           (Target => Parent_Client.Event_Loop_Exception,
            Source => Error);

         raise;
         --  Re-raise exception after clean up.

   end Event_Loop_Type;

   -----------------------
   -- Action_ID_Manager --
   -----------------------

   protected body Action_ID_Manager is

      ----------------
      -- Acquire_ID --
      ----------------

      entry Acquire_ID (ID : out Action_ID_Range)
        when Next_Free_ID_Index > Action_ID_Range'First
      is
      begin
         ID := Action_IDs (Next_Free_ID_Index);
         Next_Free_ID_Index := Next_Free_ID_Index - 1;
      end Acquire_ID;

      ----------------
      -- Release_ID --
      ----------------

      procedure Release_ID (ID : in Action_ID_Range) is
      begin
         Next_Free_ID_Index := Next_Free_ID_Index + 1;
         Action_IDs (Next_Free_ID_Index) := ID;
      end Release_ID;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         for ID in Action_IDs'Range loop
            Release_ID (ID);
         end loop;
      end Initialize;

      --------------------
      -- Index_Position --
      --------------------

      function Index_Position return Natural
      is (Next_Free_ID_Index);

   end Action_ID_Manager;

   -------------------------
   -- Response_Event_Type --
   -------------------------

   protected body Response_Event_Type is

      --------------------
      -- Await_Response --
      --------------------

      entry Await_Response
        (Response : out Message_Access) when Response_Arrived
      is
      begin
         Response         := Stored_Response;
         Stored_Response  := null;
         Response_Arrived := False;
      end Await_Response;

      ------------------
      -- Set_Response --
      ------------------

      procedure Set_Response (Response : in Message_Access) is
      begin
         if Stored_Response /= null then
            --  A response will still be allocated while invoking this
            --  procedure when the action's response that last used this
            --  `Response_Event_Type` instance timed out.
            Free (Stored_Response);
         end if;

         Stored_Response  := Response;
         Response_Arrived := True;
      end Set_Response;

   end Response_Event_Type;

   -------------------------
   -- Concatenate_Headers --
   -------------------------

   --  ! Possibly try to make this usable for regular `Message_Type`
   --    instances too.

   function Concatenate_Headers
     (Action : in Action_Type) return String with Inline
   is
      CRLF : constant String := Character'Val (13) & Character'Val (10);

      Action_ID_Header : constant String :=
        (if Action.Wants_Response then
            "ActionID: " & Action_ID_Range'Image (Action.ID) & CRLF
         else
            "");

      Index : Positive := 1;

      -----------------------
      -- Concatenate_Lines --
      -----------------------

      function Concatenate_Lines (Previous_Lines : in String) return String is
         Current_Line : constant String :=
           To_String (Action.Fields (Index)) & ": " &
           To_String (Action.Values (Index)) & CRLF;

      begin
         if Index = Action.Header_Count then
            return Previous_Lines & Current_Line;
         else
            Index := Index + 1;
            return Concatenate_Lines (Previous_Lines & Current_Line);
         end if;
      end Concatenate_Lines;

   --  Start of `Concatenate_Headers`.

   begin
      return Concatenate_Lines (Action_ID_Header);
   end Concatenate_Headers;

   ----------------
   -- Get_Header --
   ----------------

   function Get_Header
     (Message  : in Message_Type;
      Name     : in String;
      Instance : in Positive := 1) return String
   is
      Instance_Count : Positive := 1;

   begin
      for Index in 1 .. Message.Header_Count loop
         if To_String (Message.Fields (Index)) = Name then

            if Instance = Instance_Count then
               return To_String (Message.Values (Index));
            else
               Instance_Count := Instance_Count + 1;
            end if;

         end if;
      end loop;

      return "";

   end Get_Header;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Message : in Message_Type;
      Index   : in Positive) return String
   is (To_String (Message.Fields (Index)));

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Message : in Message_Type;
      Index   : in Positive) return String
   is (To_String (Message.Values (Index)));

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header
     (Action: in out Action_Type;
      Field : in     String;
      Value : in     String)
   is
   begin
      if Action.Header_Index > Action.Header_Count then
         raise Constraint_Error with "Tried to add header number"
           & Positive'Image (Action.Header_Index)
           & " (" & Field & ":" & Value
           & ") to an action initialized with room for only"
           & Positive'Image (Action.Header_Count) & " header(s).";
      end if;

      Action.Fields (Action.Header_Index) := To_Unbounded_String (Field);
      Action.Values (Action.Header_Index) := To_Unbounded_String (Value);
      Action.Header_Index := Action.Header_Index + 1;
   end Set_Header;

   -------------------
   -- Set_Header_At --
   -------------------

   procedure Set_Header_At
     (Action : in out Action_Type;
      Index  : in     Positive;
      Field  : in     String := "";
      Value  : in     String := "")
   is
   begin
      if Index > Action.Header_Count then
         raise CONSTRAINT_ERROR;
      else
         if Field /= "" then
            Action.Fields (Index) := To_Unbounded_String (Field);
         end if;

         if Value /= "" then
            Action.Values (Index) := To_Unbounded_String (Value);
         end if;
      end if;
   end Set_Header_At;

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket (Self : in Client_Type) return GNAT.Sockets.Socket_Type
   is (Self.Channel.To_Socket);

   -----------
   -- Login --
   -----------

   procedure Login
     (Self     : in out Client_Type;
      Address  : in     GNAT.Sockets.Sock_Addr_Type;
      Username : in     String;
      Secret   : in     String;
      Timeout  : in     Duration := 5.0)
   is
      use Ada.Exceptions;
      use type Agnostic_IO.Read_Error_Kind;

      Socket         : GNAT.Sockets.Socket_Type;
      Connect_Result : Selector_Status;
      Read_Error     : Agnostic_IO.Read_Error_Kind;
      Action_Login   : Action_Type (Header_Count => 3);

   begin
      if Self.Channel.Is_Connected then
         Self.Logoff;
      end if;

      GNAT.Sockets.Create_Socket
        (Socket => Socket,
         Family => Self.Address_Family,
         Mode   => GNAT.Sockets.Socket_Stream);

      begin
         GNAT.Sockets.Connect_Socket
           (Socket  => Socket,
            Server  => Self.Server_Address,
            Timeout => Timeout,
            Status  => Connect_Result);

      exception
         when GNAT.Sockets.Socket_Error =>
            Connect_Result := GNAT.Sockets.Aborted;
      end;

      if Connect_Result /= GNAT.Sockets.Completed then
         raise AMI_Error with "No response from host '"
           & Image (Address) & "' during login attempt.";
      end if;

      Self.Channel.Set_Socket (Socket);

      Self.AMI_Version := To_Unbounded_String
        (Self.Channel.Read_Line (Read_Error));

      if Read_Error /= Agnostic_IO.No_Error then
         raise AMI_Error with "Error while reading version information: "
           & Agnostic_IO.Read_Error_Kind'Image (Read_Error);
      end if;

      if not Self.Channel.Is_Connected then
         raise AMI_Error with "Host at '"
           & Image (Self.Server_Address) & "' closed socket "
           & "unexpectedly during login attempt.";
      end if;

      if Self.Event_Loop'Terminated then
         raise AMI_Event_Loop_Terminated_Error with "Client can't be used to "
           & "login to the server '"
           & Image (Self.Server_Address)
           & "' because it's event loop terminated at "
           & Ada.Calendar.Formatting.Image (Self.Disconnect_Timestamp) & CRLF
           & "The cause of termination was the exception "
           & Exception_Name    (Self.Event_Loop_Exception) & " : "
           & Exception_Message (Self.Event_Loop_Exception);
      else
         Self.Event_Loop.Start (Client => Self);
         --  See note in `Event_Loop_Type` `accept Start` body.
      end if;

      if Self.Action_IDs.Index_Position = 0 then
         --  This is the first time this client is logging in, so intialize
         --  the stack of available action IDs.

         --  ! Although very unlikely, this would falsely get invoked if
         --    a package user managed to have all action IDs in use, log out
         --    and then log back in. A solution to this (possibly the best
         --    one), and to a few other initialization issues we've encountered
         --    since allowing re-logins to occur, would be to try and derive
         --    the AMI client type from `Ada.Finalization.Controlled`.
         Self.Action_IDs.Initialize;
      end if;

      Set_Header (Action_Login, "Action",   "Login");
      Set_Header (Action_Login, "Username", Username);
      Set_Header (Action_Login, "Secret",   Secret);

      declare
         Login_Response : constant Message_Type :=
           Self.Send_Action (Action_Login);

      begin
         if Login_Response = No_Response then

            raise AMI_Error with "Couldn't login to AMI server at '"
              & Image (Self.Server_Address) & "' because no "
              & "response was received before the timeout.";

         elsif Get_Header (Login_Response, "Response") /= "Success" then

            raise AMI_Error with "Couldn't login to AMI server at '"
              & Image (Self.Server_Address)
              & "', the AMI server responded with: '"
              & Get_Header (Login_Response, "Message") & "'";

         end if;
      end;

      Self.Disconnect_Timestamp := Never_Disconnected;

   end Login;

   ------------
   -- Logoff --
   ------------

   procedure Logoff (Self : in out Client_Type) is
      Action_Logoff : Action_Type (Header_Count => 1);

   begin
      --  ! Raise exception if event loop has already terminated?

      Self.Deliberate_Logoff := True;

      Set_Header       (Action_Logoff, "Action", "Logoff");
      Self.Send_Action (Action_Logoff);

      Self.Event_Loop.Await_Cleanup;
      --  The event loop handles closure of the socket from our end.
      --  ! Consider moving the call to `Self.Channel.Close` back here.
   end Logoff;

   -----------------
   -- Send_Action --
   -----------------

   procedure Send_Action
     (Self   : in out Client_Type;
      Action : in out Action_Type)
   is
      Action_String : constant String := Concatenate_Headers (Action);

   begin
      if Self.Channel.Is_Connected then
         Self.Socket_Write_Mutex.Acquire;
         Self.Channel.Write_Line (Action_String);
         Self.Socket_Write_Mutex.Release;
         --  `Channel.Write_Line` shouldn't ever raise an exception, so an
         --  exception handler to ensure the mutex is released isn't
         --  required here.
      end if;
   end Send_Action;

   -----------------

   function Send_Action
     (Self    : in out Client_Type;
      Action  : in out Action_Type;
      Timeout : in     Duration := 5.0) return Message_Type
   is
      use Ada.Task_Identification;

      Response_Acc : Message_Access;

   begin
      if Ada.Task_Identification.Current_Task = Self.Event_Loop'Identity then

         raise AMI_Error with "Receipt of action responses cannot be performed "
           & "inside the event callback (a deadlock would occur).";

      elsif Self.Channel.Is_Connected = False then

         return No_Response;
      end if;

      Self.Action_IDs.Acquire_ID (Action.ID);
      Action.Wants_Response := True;

      Self.Send_Action (Action);

      select
         delay Timeout;
         Response_Acc := null;

      then abort
         Self.Action_Responses (Action.ID).Await_Response (Response_Acc);
         --  ! Check how abortion is allowed to stop the subprogram calls
         --    exactly (we assume it allows entry calls to complete only
         --    once their guard is lifted).

      end select;

         Self.Action_IDs.Release_ID (Action.ID);
      --  Release the action ID used regardless of whether or not
      --  we received a response from Asterisk. If a timeout occurred, but
      --  the response does arrive eventually, this action ID's corresponding
      --  `Response_Event_Type` will still hold an un-freed access to a
      --  `Message_Type` instance. To prevent a complete memory leak, the next
      --  time this `Response_Event_Type` instance is used, the `Message_Type`
      --  instance inside will be freed before being overwritten with the new
      --  response.

      --  The older, alternate idea for solving the above problem:
      --  "Maybe we can also fix this by making 'Set_Response'
      --  an entry with a timeout as well, which would always free
      --  the unreceived response after it's own timeout. The issue
      --  with this is, that it would block the event loop for whatever
      --  amount of time we wait for it to arrive, creating a rather
      --  large delay."

      Action.Wants_Response := False;
      --  Reset to `False` in case the user re-uses this action in the
      --  regular procedure variant of `Send_Action`.

      if Response_Acc = null then
         return No_Response;
      else
         declare
            Response : constant Message_Type := Response_Acc.all;
            --  Copy the heap allocated `Message_Type` instance that holds
            --  the action response to the stack of the task (thread)
            --  that the user invoked this function from.
         begin
            Free (Response_Acc);
            return Response;
         end;
      end if;

   end Send_Action;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected (Self : in Client_Type) return Boolean
   is (Self.Channel.Is_Connected);

   ------------------------
   -- Get_Server_Address --
   ------------------------

   function Get_Server_Address
     (Self : in Client_Type) return GNAT.Sockets.Sock_Addr_Type
   is (Self.Server_Address);

   ---------------------
   -- Get_AMI_Version --
   ---------------------

   function Get_AMI_Version (Self : in Client_Type) return String
   is (if Self.Channel.Is_Connected then
          To_String (Self.AMI_Version)
       else
          "");

   ----------------------------
   -- Get_Disconnection_Time --
   ----------------------------

   function Get_Disconnection_Time
     (Self : in Client_Type) return Ada.Calendar.Time
   is (Self.Disconnect_Timestamp);

end Asterisk.AMI;
