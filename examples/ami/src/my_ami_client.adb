with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Calendar.Formatting;


package body My_AMI_Client is

   ---------------------
   --  Event_Callback --
   ---------------------

   overriding procedure Event_Callback
     (Unused_Client : in out Client_Type;
      Name          : in     String;
      Event         : in     Asterisk.AMI.Message_Type)
   is
   begin
      --  The user defined callback for AMI events, achieved by
      --  overriding the package supplied null procedure. This
      --  procedure is invoked from within the event loop's task.

      Put_Line ("Received event: " & Name);

      for Index in 1 .. Event.Header_Count loop
         Put_Line (Asterisk.AMI.Get_Field (Event, Index) & ": "
                 & Asterisk.AMI.Get_Value (Event, Index));
      end loop;
      New_Line;

   end Event_Callback;

   -----------------------------------
   -- Client_Disconnection_Callback --
   -----------------------------------

   overriding procedure Client_Disconnection_Callback
     (Client : in out Client_Type;
      Cause  : in     Asterisk.AMI.Cause_Of_Disconnection;
      Error  : in     Agnostic_IO.Read_Error_Kind)
   is
   begin
      case Cause is
         when Asterisk.AMI.Deliberate_Logoff      =>
            Put_Line ("Logged off from server at "
              & Ada.Calendar.Formatting.Image (Client.Get_Disconnection_Time));

         when Asterisk.AMI.Abnormal_Disconnection =>
            Put_Line ("Abnormal client disconnection occurred due to "
              & Agnostic_IO.Read_Error_Kind'Image (Error));
      end case;
   end Client_Disconnection_Callback;

end My_AMI_Client;
