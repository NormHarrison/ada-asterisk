with Asterisk.AGI.ProcessAGI;


procedure ProcessAGI_Example is

   AGI : Asterisk.AGI.ProcessAGI.Session_Type (Argument_Count => 1);

begin
   AGI.Initialize;
   AGI.Command ("VERBOSE ""AGI session information:"" 0");
   AGI.Command ("VERBOSE """ & AGI.Variable
     (Asterisk.AGI.AGI_UniqueID) & """ 0");
   AGI.Command ("VERBOSE """ & AGI.Argument (1) & """ 0");
   AGI.Command ("VERBOSE """ & AGI.Command
     ("GET FULL VARIABLE ${CHANNEL}") & """ 0");
end ProcessAGI_Example;
