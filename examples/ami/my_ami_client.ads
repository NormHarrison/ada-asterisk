with Ada.Streams;

with GNAT.Sockets;

with Asterisk.AMI;


package My_AMI_Client is

   type Client_Type
     (Address_Family   : GNAT.Sockets.Family_Inet_4_6;
      Read_Buffer_Size : Ada.Streams.Stream_Element_Offset;
      Recursion_Limit  : Natural)
   is new Asterisk.AMI.Client_Type
     (Address_Family, Read_Buffer_Size, Recursion_Limit) with null record;

   overriding procedure On_Event
     (Unused_Client : in out Client_Type;
      Name          : in     String;
      Event         : in     Asterisk.AMI.Message_Type);

end My_AMI_Client;
