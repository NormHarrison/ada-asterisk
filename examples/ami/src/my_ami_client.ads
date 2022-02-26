with Asterisk.AMI;
with Agnostic_IO;


package My_AMI_Client is

   type Client_Type is new Asterisk.AMI.Client_Type with null record;

   overriding procedure Event_Callback
     (Unused_Client : in out Client_Type;
      Name          : in     String;
      Event         : in     Asterisk.AMI.Message_Type);

   overriding procedure Client_Disconnection_Callback
     (Client : in out Client_Type;
      Cause  : in     Asterisk.AMI.Cause_Of_Disconnection;
      Error  : in     Agnostic_IO.Read_Error_Kind);

end My_AMI_Client;
