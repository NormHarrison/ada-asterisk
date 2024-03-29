with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Sockets;

with Asterisk.AudioSocket; use Asterisk.AudioSocket;


procedure AudioSocket_Example is

   procedure Handle_Connection
     (Connection : in out Asterisk.AudioSocket.Connection_Type)
   is
      Connection_UUID : constant Byte_Array := Get_UUID (Connection);

   begin
      Put_Line ("Received connection from peer "
        & GNAT.Sockets.Image (Get_Peer_Name (Connection))
        & " with a UUID of ");

        for Index in Connection_UUID'Range loop
           Put (Byte'Image (Connection_UUID (Index)));
        end loop;
        New_Line;

      while not Has_Hung_Up (Connection) loop
         declare
            Message : constant Asterisk.AudioSocket.Message_Type :=
              Receive_Message (Connection);

         begin
            Put_Line ("Received message was of kind " & Kind (Message)'Image);
         end;

      end loop;

      Put ("Communication with connection with UUID");
      for Index in Connection_UUID'Range loop
        Put (Byte'Image (Connection_UUID (Index)));
      end loop;
      Put_Line (" is now over.");

   end Handle_Connection;

   Bind_Address : constant GNAT.Sockets.Sock_Addr_Type :=
     (Family => GNAT.Sockets.Family_Inet,
      Port   => 35551,
      Addr   => GNAT.Sockets.Any_Inet_Addr);

   AS_Server     : Asterisk.AudioSocket.Server_Type
     (Address_Family => GNAT.Sockets.Family_Inet);

   AS_Connection : Asterisk.AudioSocket.Connection_Type
     (Address_Family => GNAT.Sockets.Family_Inet);

begin
   Create_Server
     (Server  => AS_Server,
      Address => Bind_Address);

   Put_Line ("Listening at " & GNAT.Sockets.Image (Bind_Address));

   Indefinite : loop
      Put_Line ("Ready for the next connection...");
      Accept_Connection (AS_Server, AS_Connection);
      Handle_Connection (AS_Connection);
   end loop Indefinite;

end AudioSocket_Example;
