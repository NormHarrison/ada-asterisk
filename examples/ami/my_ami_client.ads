with GNAT.Sockets;

with Asterisk.AMI;
--with Agnostic_IO;


package My_AMI_Client is

   type Client_Type
     (Address_Family : GNAT.Sockets.Family_Type)
   is new Asterisk.AMI.Client_Type (Address_Family) with null record;

   overriding procedure On_Event
     (Unused_Client : in out Client_Type;
      Name          : in     String;
      Event         : in     Asterisk.AMI.Message_Type);

   overriding procedure On_Disconnect (Unused_Client : in out Client_Type);

end My_AMI_Client;
