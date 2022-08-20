--with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Exceptions;


package body Asterisk.AGI is

   --------------
   -- Variable --
   --------------

   function Variable
     (Self : in Base_Session_Type;
      Name : in Environment_Variable_Name) return String
   is (To_String (Self.Variables (Name)));

   function Variable
     (Self : in Base_Session_Type;
      Name : in Environment_Variable_Name) return Unbounded_String
   is (Self.Variables (Name));

   --------------
   -- Argument --
   --------------

   function Argument
     (Self   : in Base_Session_Type;
      Number : in Positive) return String
   is (To_String (Self.Arguments (Number)));

   function Argument
     (Self   : in Base_Session_Type;
      Number : in Positive) return Unbounded_String
   is (Self.Arguments (Number));

   -------------
   -- Command --
   -------------

   function Command
     (Self : in out Base_Session_Type;
      Data : in     String) return String
   is

      ---------------------
      -- Raise_AGI_Error --
      ---------------------

      procedure Raise_AGI_Error (Error : in Ada.Exceptions.Exception_ID) is
      begin
         Ada.Exceptions.Raise_Exception
           (E       => Error,
            Message => "Occurred while executing: """ & Data & """");
      end Raise_AGI_Error;

   begin
      Self.Agnostic_Channel.Write_Line (Data);

      <<Read_Again>>
      --  This label is only jumped to in the scenario where Asterisk sent
      --  us a `HANGUP` message, and another read must occur to obtain the
      --  response to the user's original command.

      declare
         use Agnostic_IO;

         Delimiter_Index :          Natural;
         Status_Code     :          Integer;
         Read_Error      :          Agnostic_IO.Read_Error_Kind;
         Response        : constant String :=
           Self.Agnostic_Channel.Read_Line (Error => Read_Error);

      begin
         case Read_Error is
            when Source_Read_Error |
                 Remote_Socket_Closure_Error =>
               Self.Hung_Up := True;
               return "";

            when Recursion_Limit_Error       =>
               --  ! Socket stays open if this occurrs, so we may want
               --    to try and handle this a different way...
               raise AGI_Unknown_Error with "The data received from Asterisk "
                 & "was malformed (missing delimiting line feed character).";

            when No_Error                    =>
               if Response = "" then
                  raise AGI_Unknown_Error with "Asterisk provided no response"
                  & " after sending the command: """ & Data & """";
               end if;
         end case;

         if Response = "HANGUP" then
            Self.Hung_Up := True;
            goto Read_Again;
         end if;

         if Self.Skipped_Responses > 0 then
            --  Keep reading response lines until we reach the one meant for
            --  most recently executed command. The amount of times we perform
            --  a read may be more than the value of `Skipped_Responses`, since
            --  receipt of a "HANGUP" message will also trigger one (as the
            --  hang up message is an unanticipated additional line to read).
            Self.Skipped_Responses := Self.Skipped_Responses - 1;
            goto Read_Again;
         end if;

         Status_Code := Integer'Value
           (Response (Response'First .. Response'First + 2));
         --  ! Possibly add exception handling to this statement,
         --  turning the `Constraint_Error` exception it can raise into
         --  a `AGI_Unknown_Error` exception. Although checking whether
         --  the string is empty or not above should prevent this statement
         --  from needing its own exception handling.

         case Status_Code is
            when 510    =>
               Raise_AGI_Error (AGI_Invalid_Command_Error'Identity);

            when 520    =>
               Raise_AGI_Error (AGI_Command_Usage_Error'Identity);

            when 511    =>
               if Self.Supress_Hang_Up_Exceptions then
                  Self.Hung_Up := True;
               else
                  Raise_AGI_Error (AGI_Channel_Hangup_Error'Identity);
               end if;

            when 200    =>
               null;

            when others =>
               Raise_AGI_Error (AGI_Unknown_Error'Identity);
         end case;

         Delimiter_Index := Ada.Strings.Fixed.Index
           (Source  => Response,
            Pattern => "(",
            From    => 11);

         if Delimiter_Index = 0 then
            --  AGI Command returned no response data, or status code
            --  511 occurred (its message is missing parantheses).

            if Status_Code /= 511 then
               Self.Result_Code := Integer'Value
                 (Response (Response'First + 11 .. Response'Last));
            end if;

            return "";

         else
            --  AGI Command returned response data.
            Self.Result_Code := Integer'Value
              (Response (Response'First + 11 .. Delimiter_Index - 2));

            return Response (Delimiter_Index + 1 .. Response'Last - 1);

         end if;
      end;
   end Command;

   ----------------

   function Command
     (Self : in out Base_Session_Type;
      Data : in     String) return Unbounded_String
   is (To_Unbounded_String (Self.Command (Data)));

   -----------------

   procedure Command
     (Self          : in out Base_Session_Type;
      Data          : in     String;
      Skip_Response : in     Boolean := False)
   is
   begin
      if Skip_Response then
         Self.Skipped_Responses := Self.Skipped_Responses + 1;
         Self.Agnostic_Channel.Write_Line (Data);
      else
         declare
            Unused : constant String := Self.Command (Data);
         begin
            null;
         end;
      end if;
   end Command;

   -------------
   -- Hang_Up --
   -------------

   procedure Hang_Up (Self : in out Base_Session_Type) is
   begin
      Self.Hung_Up := True;
      Self.Command ("HANGUP");
   end Hang_Up;

   ------------------
   --  Has_Hung_Up --
   ------------------

   function Has_Hung_Up (Self : in Base_Session_Type) return Boolean
   is (Self.Hung_Up);

   ----------------------
   -- Last_Result_Code --
   ----------------------

   function Last_Result_Code (Self : in Base_Session_Type) return Integer
   is (Self.Result_Code);

   ---------------------------
   -- Parse_Initial_Message --
   ---------------------------

   procedure Parse_Initial_Message
     (Self                : in out Base_Session_Type;
      Starting_Variable   : in     Environment_Variable_Name;
      Last_Variable_Index : in     Natural)
   is
      Line_Number : Natural := 0;

      Variable_Index : Environment_Variable_Name := Starting_Variable;
      Argument_Index : Positive                  := 1;

   begin
      Parse : loop

         Extract_Value : declare
            use Agnostic_IO;

            Read_Error  : Agnostic_IO.Read_Error_Kind;

            Single_Line : constant String  := Self.Agnostic_Channel.Read_Line
              (Error => Read_Error);
            --  ! Eventually read entire message at once instead of per-line.

            Space_Index : constant Natural := Ada.Strings.Fixed.Index
              (Single_Line, " ");

            Value_Only : Unbounded_String;

         begin
            case Read_Error is
               when Source_Read_Error |
                    Remote_Socket_Closure_Error =>
                  Self.Hung_Up := True;
                  exit;

               when Recursion_Limit_Error       =>
                  --  ! Socket stays open if this occurrs, so we may want
                  --    to try and handle this a different way...
                  raise AGI_Unknown_Error with "The data received from "
                    & "Asterisk was malformed (missing delimiting line "
                    & " feed character).";

               when No_Error                    =>
                  exit when Single_Line = "";
            end case;

            Value_Only := To_Unbounded_String
              (Single_Line (Space_Index + 1 .. Single_Line'Last));

            if Starting_Variable = Environment_Variable_Name'First and then
               Line_Number       = 1
            then
               declare
                  Field_Only : constant String := Single_Line
                    (Single_Line'First .. Space_Index - 1);
               begin
                  if Field_Only /= "agi_network_script:" then
                     Line_Number    := Line_Number + 1;
                     Variable_Index := Environment_Variable_Name'Succ
                       (Variable_Index);
                  end if;
               end;
            end if;

            if Line_Number <= Last_Variable_Index then
               --  AGI environment variables are being read.

               Self.Variables (Variable_Index) := Value_Only;

               if Variable_Index /= Environment_Variable_Name'Last then
                  Variable_Index := Environment_Variable_Name'Succ
                    (Variable_Index);
               end if;

            elsif Self.Argument_Count >= Argument_Index then
               --  AGI arguments are being read.

               Self.Arguments (Argument_Index) := Value_Only;
               Argument_Index := Argument_Index + 1;

            end if;
         end Extract_Value;

         Line_Number := Line_Number + 1;

      end loop Parse;

      --  ! Consider checking the value of `Line_Number` here, and if
      --    it less than the length of the `Self.Variables` array, set
      --    `Self.Hung_Up` to `True`, or possibly raise an exception.
   end Parse_Initial_Message;

end Asterisk.AGI;
