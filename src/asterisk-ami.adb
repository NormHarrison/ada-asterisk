--with Ada.Text_IO;       use Ada.Text_IO;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings;
with Ada.Exceptions;


package body Asterisk.AMI is

   use Ada.Strings.Unbounded;

   CRLF_Delimiter : constant Ada.Streams.Stream_Element_Array :=
     Buffered_Strings.To_SEA (CRLF);

   Double_CRLF_Delimiter : constant Ada.Streams.Stream_Element_Array :=
     Buffered_Strings.To_SEA (CRLF & CRLF);

   ----------------------
   -- Is_Timeout_Error --
   ----------------------

   function Is_Timeout_Error
     (Occurrence : in Ada.Exceptions.Exception_Occurrence) return Boolean
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (Occurrence) = Socket_Error'Identity and then
         Exception_Message  (Occurrence) (2 .. 3) = "35"
      then
         return True;
      else
         return False;
      end if;
   end Is_Timeout_Error;

   ------------------------
   -- Cleanup_Connection --
   ------------------------

   procedure Cleanup_Connection (Self : in out Client_Type'Class) is
   begin
      Self.Connected      := False;
      Self.Server_Address :=
        (case Self.Address_Family is
            when Family_Inet =>
               (Family => Family_Inet,
                Port   => No_Port,
                Addr   => Any_Inet_Addr),

            when Family_Inet6 =>
               (Family => Family_Inet6,
                Port   => No_Port,
                Addr   => Any_Inet6_Addr));

      GNAT.Sockets.Close_Socket (Self.Stream.To_Socket);
   end Cleanup_Connection;

   ----------------------
   -- Read_AMI_Message --
   ----------------------

   function Read_AMI_Message
     (AMI_Client      : in out Client_Type'Class;
      Delimiter       : in     Ada.Streams.Stream_Element_Array;
      New_Timeout     : in     GNAT.Sockets.Timeval_Duration;
      Error           :    out Ada.Exceptions.Exception_Occurrence)
   return String is
   begin

      if New_Timeout /= AMI_Client.Last_Await_Timeout then

         GNAT.Sockets.Set_Socket_Option
           (Socket => AMI_Client.Stream.To_Socket,
            Level  => GNAT.Sockets.Socket_Level,
            Option => (Name    => GNAT.Sockets.Receive_Timeout,
                       Timeout => New_Timeout));

         AMI_Client.Last_Await_Timeout := New_Timeout;

      end if;

      declare
         Socket_Data : constant String :=
           AMI_Client.Buffered_Socket.Read_Until (Delimiter, Error);

      begin
         return Socket_Data;
      end;

   end Read_AMI_Message;

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

   -----------------------
   -- Parse_AMI_Message --
   -----------------------

   CRLF_Set : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (CRLF);
   --  ! Consider moving to private part of specification.

   procedure Parse_AMI_Message
     (Data        : in     String;
      Message_Acc :    out Message_Access)
   is
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

   end Parse_AMI_Message;

   -------------------
   -- Await_Message --
   -------------------

   procedure Await_Message
     (Self    : in out Client_Type'Class;
      Timeout : in     GNAT.Sockets.Timeval_Duration := GNAT.Sockets.Forever)
   is
      use type Ada.Exceptions.Exception_ID;

      Read_Error : Ada.Exceptions.Exception_Occurrence;

      Socket_Data : constant String := Read_AMI_Message
        (AMI_Client  => Self,
         Delimiter   => Double_CRLF_Delimiter,
         New_Timeout => Timeout,
         Error       => Read_Error);

      Message_Acc : Message_Access;
      Action_ID   : Action_ID_Range;

   begin
      if Ada.Exceptions.Exception_Identity (Read_Error) /=
         Ada.Exceptions.Null_Id
      then

         if Self.Connected then

            if Is_Timeout_Error (Read_Error) then
               raise AMI_Timeout_Error with "No message received after "
                 & Duration'Image (Timeout) & " seconds.";
            else
               Cleanup_Connection (Self);
               Ada.Exceptions.Reraise_Occurrence (Read_Error);
               --  ! This behavior may not be optimal when the recursion limit
               --  is reached. We could treat the recursion limit exception
               --  the same as when a timeout occurrs (i.e. not calling
               --  `Cleanup_Connection`).
            end if;

         else
            raise AMI_Deliberate_Disconnect;
         end if;

      end if;

      Parse_AMI_Message (Socket_Data, Message_Acc);

      if Get_Header (Message_Acc.all, "Event") /= "" then

         Self.On_Event
           (Name  => Get_Header (Message_Acc.all, "Event"),
            Event => Message_Acc.all);

         Free (Message_Acc);
         --  Heap allocated events are automatically deallocated
         --  after the user's callback is finished executing.

      elsif Get_Header (Message_Acc.all, "ActionID") /= "" and then
            Get_Header (Message_Acc.all, "Response") /= ""     then

         Action_ID := Action_ID_Range'Value
           (Get_Header (Message_Acc.all, "ActionID"));

         Self.Action_Responses (Action_ID).Set_Response (Message_Acc);
         --  Heap allocated responses are copied to the stack of the
         --  task that the user invokes the function `Send_Action`
         --  from. After they are successfully received, the copy on
         --  the heap is then deallocated. Responses that don't arrive
         --  before the timeout set by the user, but still arrive
         --  later on, are deallocated the next time that the
         --  `Response_Event_Type` instance is used to receive an
         --  action's response.

      else
         null;
         --  ! Create an exception for corrupted/unknown messages?
         --  This may also be a place where `Self.Connected` is set
         --  to False and the socket is closed.
      end if;

      --  Some AMI actions/commands send back events containing
      --  `Response` and `ActionID` fields too, instead of only using
      --  a `Response` message (Originate is an example). This is
      --  currently being ignored because the below conditional statement
      --  is combined via `elsif`, only ever executing one branch, even if
      --  a particular message contains `Event`, `ActionID` and `Response`
      --  fields. If this ever poses a problem with functionality, then
      --  they can be separated.

   end Await_Message;

   -----------------------
   -- Action_ID_Manager --
   -----------------------

   protected body Action_ID_Manager is

      ----------------
      -- Acquire_ID --
      ----------------

      entry Acquire_ID (ID : out Action_ID_Range)
        when Next_Free_ID_Index >= Natural (Action_IDs'First)
      is
      begin
         ID := Action_IDs (Action_ID_Range (Next_Free_ID_Index));
         --  Cast of Natural value `Next_Free_ID_Index` to type
         --  `Action_ID_Range` is safe, as the entry's guard will
         --  prevent this entry from executing until `Next_Free_ID_Index`
         --  is greater than 0.
         Next_Free_ID_Index := Next_Free_ID_Index - 1;
      end Acquire_ID;

      ----------------
      -- Release_ID --
      ----------------

      procedure Release_ID (ID : in Action_ID_Range) is
      begin
         Next_Free_ID_Index := Next_Free_ID_Index + 1;
         Action_IDs (Action_ID_Range (Next_Free_ID_Index)) := ID;
         --  ! `Next_Free_ID_Index` can technically hold a value
         --  greater than what values of type `Action_ID_Range` can
         --  hold. A guard on this procedure could prevent this, but
         --  since users can never interact with this type directly,
         --  and since it also task (thread) safe, this shouldn't be
         --  an issue.
      end Release_ID;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Next_Free_ID_Index := 0;
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

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Free (Stored_Response);
         Response_Arrived := False;
      end Reset;

   end Response_Event_Type;

   -------------------------
   -- Concatenate_Headers --
   -------------------------

   --  ! Possibly try to make this usable for regular `Message_Type`
   --    instances too.

   function Concatenate_Headers
     (Action : in Action_Type) return String with Inline
   is
      Action_ID_Header : constant String :=
        (if Action.Wants_Response then
            "ActionID:" & Action_ID_Range'Image (Action.ID) & CRLF
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
         raise Constraint_Error;
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
   is (Self.Stream.To_Socket);

   -----------
   -- Login --
   -----------

   procedure Login
     (Self     : in out Client_Type;
      Address  : in     GNAT.Sockets.Sock_Addr_Type;
      Username : in     String;
      Secret   : in     String;
      Timeout  : in     GNAT.Sockets.Timeval_Duration := 5.0)
   is
      use type Ada.Exceptions.Exception_Id;

      Socket         : GNAT.Sockets.Socket_Type;
      Connect_Result : Selector_Status;
      Read_Error     : Ada.Exceptions.Exception_Occurrence;

      Action_Login : Action_Type (Header_Count => 3);

   begin
      if Self.Connected then
         Self.Logoff;
      end if;

      GNAT.Sockets.Create_Socket
        (Socket => Socket,
         Family => Self.Address_Family,
         Mode   => GNAT.Sockets.Socket_Stream);

      begin
         GNAT.Sockets.Connect_Socket
           (Socket  => Socket,
            Server  => Address,
            Timeout => Timeout,
            Status  => Connect_Result);

      exception
         when GNAT.Sockets.Socket_Error =>
            Connect_Result := GNAT.Sockets.Aborted;
      end;

      if Connect_Result /= GNAT.Sockets.Completed then
         raise AMI_Server_Unreachable_Error with "No response from host '"
           & Image (Address) & "' during login attempt.";
      end if;

      Self.Server_Address := Address;
      Self.Connected      := True;

      Self.Stream.Initialize (Socket);
      Self.Buffered_Socket.Set_Stream (Self.Stream.To_Access);

      Self.AMI_Version := To_Unbounded_String
        (Read_AMI_Message (AMI_Client  => Self,
                           Delimiter   => CRLF_Delimiter,
                           New_Timeout => Timeout,
                           Error       => Read_Error));

      if Ada.Exceptions.Exception_Identity (Read_Error) /=
         Ada.Exceptions.Null_Id
      then
         Self.Cleanup_Connection;

         if Is_Timeout_Error (Read_Error) then
            raise AMI_Timeout_Error with "Couldn't login to AMI server at """
              & Image (Self.Server_Address) & """ because no version info "
              & "was received before the timeout.";
         end if;

         Ada.Exceptions.Reraise_Occurrence (Read_Error);
      end if;

      Self.Action_IDs.Initialize;
      for Index in Self.Action_Responses'Range loop
         Self.Action_Responses (Index).Reset;
      end loop;

      Set_Header (Action_Login, "Action", "Login");
      Set_Header (Action_Login, "Username", Username);
      Set_Header (Action_Login, "Secret", Secret);

      Self.Send_Action (Action_Login);

      declare
         Socket_Data : constant String := Read_AMI_Message
           (AMI_Client   => Self,
            Delimiter    => CRLF_Delimiter,
            New_Timeout  => Timeout,
            Error        => Read_Error);

      begin
         if Ada.Exceptions.Exception_Identity (Read_Error) /=
            Ada.Exceptions.Null_Id
         then

            Self.Cleanup_Connection;

            if Is_Timeout_Error (Read_Error) then
               raise AMI_Timeout_Error with "Couldn't login to AMI server at """
                 & Image (Self.Server_Address) & """ because no login "
                 & "response was received before the timeout.";
            end if;

            Ada.Exceptions.Reraise_Occurrence (Read_Error);

         elsif not Find_Pattern ("Success", In_String => Socket_Data) then

            Self.Cleanup_Connection;
            raise AMI_Invalid_Credentials_Error with "Couldn't login to AMI "
              & "server at """ & Image (Self.Server_Address)
              & """, the AMI server responded with:" & CRLF
              & """" & Socket_Data & """";

         end if;
      end;

      if Timeout /= GNAT.Sockets.Forever then

         GNAT.Sockets.Set_Socket_Option
           (Socket => Self.Stream.To_Socket,
            Level  => GNAT.Sockets.Socket_Level,
            Option => (Name    => GNAT.Sockets.Receive_Timeout,
                       Timeout => GNAT.Sockets.Forever));
      end if;

   end Login;

   ------------
   -- Logoff --
   ------------

   procedure Logoff (Self : in out Client_Type) is
      Action_Logoff : Action_Type (Header_Count => 1);

   begin
      if not Self.Connected then
         return;
      end if;

      --Self.Deliberate_Disconnect := True;
      Self.Connected := False;

      Set_Header       (Action_Logoff, "Action", "Logoff");
      Self.Send_Action (Action_Logoff);

      Self.Cleanup_Connection;
      Self.AMI_Version := Null_Unbounded_String;
   end Logoff;

   -----------------
   -- Send_Action --
   -----------------

   procedure Send_Action
     (Self   : in out Client_Type;
      Action : in out Action_Type)
   is
      Action_String : constant String := Concatenate_Headers (Action) & CRLF;

   begin
      if Self.Connected then
         Self.Socket_Write_Mutex.Acquire;
         String'Write (Self.Stream.To_Access, Action_String);
         Self.Socket_Write_Mutex.Release;
      end if;

   exception
      when others =>
         Self.Socket_Write_Mutex.Release;
         --  ! Re-raise exception?

   end Send_Action;

   -----------------

   function Send_Action
     (Self    : in out Client_Type;
      Action  : in out Action_Type;
      Timeout : in     Duration := Duration'Last)
   return Message_Type
   is
      Response_Acc : Message_Access;

   begin
      if not Self.Connected then
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
            --  that the user invoked this function from, then free the
            --  heap allocated instance.
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
   is (Self.Connected);

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
   is (if Self.Connected then To_String (Self.AMI_Version) else "");

end Asterisk.AMI;
