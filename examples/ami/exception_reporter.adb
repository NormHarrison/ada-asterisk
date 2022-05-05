with Ada.Text_IO; use Ada.Text_IO;


package body Exception_Reporter is

   protected body Terminating_Task is

      procedure Handler
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Task_Termination;

      begin
         Put_Line ("The task with ID " & Ada.Task_Identification.Image (T)
           & " terminated with cause: " & Cause_Of_Termination'Image (Cause));

         if Cause = Ada.Task_Termination.Unhandled_Exception then
              Put_Line ("Exception: "
                & Ada.Exceptions.Exception_Information (X));
         end if;

      end Handler;

   end Terminating_Task;

end Exception_Reporter;
