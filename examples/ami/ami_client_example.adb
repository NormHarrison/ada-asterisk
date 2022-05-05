with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Task_Termination;
with Ada.Task_Identification;

with GNAT.Sockets;

with Asterisk.AMI;
with My_AMI_Client;
with Exception_Reporter;


procedure AMI_Client_Example is

   AMI_Server_Address : constant GNAT.Sockets.Sock_Addr_Type :=
     (Family => GNAT.Sockets.Family_Inet,
      Port   => Asterisk.AMI.Default_AMI_Port,
      Addr   => (Family => GNAT.Sockets.Family_Inet,
                 Sin_V4 => (127, 0, 0, 1)));

   AMI_Client : My_AMI_Client.Client_Type
     (Address_Family => GNAT.Sockets.Family_Inet);

   ------------------
   -- Message_Loop --
   ------------------

   task Message_Loop is
      entry Start;
      entry Stop;
   end Message_Loop;

   task body Message_Loop is
      Exit_Poll_Loop : Boolean;

   begin
      Forever : loop

         Exit_Poll_Loop := False;
         accept Start;

         Poll : loop

            select
               accept Stop do
                  Exit_Poll_Loop := True;
               end Stop;
            else
               null;
            end select;

            exit when Exit_Poll_Loop or else not AMI_Client.Await_Message;

         end loop Poll;

      end loop Forever;
   end Message_Loop;

   ---------
   -- Put --
   ---------

   procedure Put (Response : in Asterisk.AMI.Message_Type) is
   begin
      for Index in 1 .. Response.Header_Count loop
          Put_Line (Asterisk.AMI.Get_Field (Response, Index)
            & ": " & Asterisk.AMI.Get_Value (Response, Index));
      end loop;
      New_Line;
   end Put;

   Action_CoreStatus : Asterisk.AMI.Action_Type (Header_Count => 1);

begin
   Ada.Task_Termination.Set_Specific_Handler
     (T       => Ada.Task_Identification.Environment_Task,
      Handler => Exception_Reporter.Terminating_Task.Handler'Access);
   --  Informs of errors that occurr within the environment task.

   Ada.Task_Termination.Set_Dependents_Fallback_Handler
     (Handler => Exception_Reporter.Terminating_Task.Handler'Access);
   --  Informs of errors that occurr within any subtask
   --  (in our case, the message loop task).

   Asterisk.AMI.Set_Header (Action_CoreStatus, "Action", "CoreStatus");

   Forever : loop

      Put_Line ("Logging in...");

      AMI_Client.Login
        (Username => "<username>",
         Secret   => "<secret>",
         Address  => AMI_Server_Address);

      Put_Line ("Connected to: " & AMI_Client.Get_AMI_Version);

      Message_Loop.Start;

      for Index in 1 .. 10 loop

         declare
            use type Asterisk.AMI.Message_Type;

            CoreStatus_Response : constant Asterisk.AMI.Message_Type :=
              AMI_Client.Send_Action (Action_CoreStatus);

         begin
            if CoreStatus_Response /= Asterisk.AMI.No_Response then
               Put (CoreStatus_Response);
            end if;
         end;

         Put_Line ("Sent action number " & Index'Image);
         delay 1.0;

      end loop;

      Put_Line ("Logging off in 10 seconds...");
      delay 10.0;
      --  Print out all events received for 10 seconds, then logoff.

      AMI_Client.Logoff;
      Message_Loop.Stop;
      Put_Line ("Logged off.");

   end loop Forever;

end AMI_Client_Example;
