with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Sockets;

with Asterisk.AGI.FastAGI;


procedure Example_Server is

   procedure Handle_Connection
     (AGI : in out Asterisk.AGI.FastAGI.Session_Type)
   is
      use Asterisk.AGI.FastAGI;

   begin
      --  Print the requested FastAGI script/service, this is
      --  the string after the '/' in the AGI() dialplan call.
      Put_Line (AGI.Variable (Asterisk.AGI.AGI_Network_Script));

      --  Retreive the first session argument, this is the string
      --  after the first ',' in the AGI() dialplan call
      Put_Line (AGI.Argument (1));

      --  Set a variable on the connected channel, this is using
      --  the procedure variant of the 'Command' subprogram. For when
      --  you dont need the return value of the AGI command.
      AGI.Command ("SET VARIABLE FROM_ADA TEST123");

      --  Obtain the channel's unique ID variable, this demonstrates the
      --  function 'Command' primitive, for AGI commands that return data.
      Put_Line (AGI.Command ("GET FULL VARIABLE ${UNIQUEID}"));

      --  Close the underlying socket connected to the remote Asterisk channel.
      AGI.Disconnect;

   end Handle_Connection;

-----------------------------
-- Start of main procedure --
-----------------------------

   AGI_Server : Asterisk.AGI.FastAGI.Server_Type
     (Address_Family => GNAT.Sockets.Family_Inet);

   AGI_Session : Asterisk.AGI.FastAGI.Session_Type
     (Argument_Count             => 1,
      Address_Family             => GNAT.Sockets.Family_Inet,
      Supress_Hang_Up_Exceptions => True);

begin
   --  Create the base server, binding it to an interface
   --  and then listening for connections.
   Asterisk.AGI.FastAGI.Create_Server
     (Server  => AGI_Server,
      Port    => 1234,
      Address => (Family => GNAT.Sockets.Family_Inet,
                  Sin_V4 => (127, 0, 0, 1)));

   Put_Line ("Listening for connections on: " & GNAT.Sockets.Image
     (Asterisk.AGI.FastAGI.Get_Bind_Address (AGI_Server)));

   --  Accept connections forever.
   Indefinite : loop

      Asterisk.AGI.FastAGI.Accept_Connection
        (Server  => AGI_Server,
         Session => AGI_Session);

      Handle_Connection (AGI_Session);

   end loop Indefinite;

end Example_Server;
