with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Task_Termination;
with Ada.Task_Identification;

with GNAT.Sockets;

with Asterisk.AMI;
with My_AMI_Client;
with Exception_Reporter;


procedure AMI_Client_Example is

   AMI_Client        : My_AMI_Client.Client_Type;
   Action_CoreStatus : Asterisk.AMI.Action_Type (Header_Count => 1);

begin

   Ada.Task_Termination.Set_Specific_Handler
     (T       => Ada.Task_Identification.Environment_Task,
      Handler => Exception_Reporter.Terminating_Task.Handler'Access);
   --  Informs of errors that occurr within the environment task.

   AMI_Client.Set_Event_Loop_Termination_Handler
     (Handler => Exception_Reporter.Terminating_Task.Handler'Access);

   Asterisk.AMI.Set_Header (Action_CoreStatus, "Action", "CoreStatus");

   <<Do_It_Again>>

   AMI_Client.Login
     (Username => "<ami_username>",
      Secret   => "<ami_secret>",
      Port     => 5039,
      Address  => (Family => GNAT.Sockets.Family_Inet,
                   Sin_V4 => (127, 0, 0, 1)));

   Put_Line ("Connected to: " & AMI_Client.Get_AMI_Version);

   for Index in 1 .. 2 loop

      declare
         use type Asterisk.AMI.Message_Type;

         CoreStatus_Response : constant Asterisk.AMI.Message_Type :=
           AMI_Client.Send_Action (Action_CoreStatus);
      begin

         if CoreStatus_Response /= Asterisk.AMI.No_Response then

            for Index in 1 .. CoreStatus_Response.Header_Count loop
               Put_Line (Asterisk.AMI.Get_Field (CoreStatus_Response, Index)
                 & ": " & Asterisk.AMI.Get_Value (CoreStatus_Response, Index));
            end loop;

            New_Line;

         end if;
      end;

      Put_Line ("Sent action number " & Index'Image);
      delay 1.0;

   end loop;

   delay 2.0;
   --  Print out all events received for 30 seconds, then logoff.

   AMI_Client.Logoff;
   Put_Line ("Logged off.");
   delay 1.0;

   Put_Line ("Loggin in again...");
   goto Do_It_Again;

end AMI_Client_Example;
