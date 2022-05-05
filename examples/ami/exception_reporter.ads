with Ada.Task_Termination;
with Ada.Task_Identification;
with Ada.Exceptions;

package Exception_Reporter is

   protected Terminating_Task is

      procedure Handler
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);

   end Terminating_Task;

end Exception_Reporter;
