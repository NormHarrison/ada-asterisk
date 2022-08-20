with Ada.Text_IO; use Ada.Text_IO;


package body My_AMI_Client is

   ---------------
   --  On_Event --
   ---------------

   overriding procedure On_Event
     (Unused_Client : in out Client_Type;
      Name          : in     String;
      Event         : in     Asterisk.AMI.Message_Type)
   is
   begin
      --  The user defined callback for AMI events, achieved by
      --  overriding the package supplied null procedure. This
      --  procedure is invoked if an event arrives during a call
      --  to `Await_Message`.

      Put_Line ("Received event: " & Name);

      for Index in 1 .. Event.Header_Count loop
         Put_Line (Asterisk.AMI.Get_Field (Event, Index) & ": "
                 & Asterisk.AMI.Get_Value (Event, Index));
      end loop;
      New_Line;

   end On_Event;

end My_AMI_Client;
