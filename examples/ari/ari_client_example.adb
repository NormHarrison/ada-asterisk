with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON;

with AWS.Response;
--with AWS.Messages;

with Asterisk.ARI;


procedure ARI_Client_Example is

   type ARI_Client is new Asterisk.ARI.Client_Type with null record;

   ARI : ARI_Client;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Unused_Socket : in out ARI_Client;
      Message       : in     String)
   is
      use Asterisk.ARI;
      use GNATCOLL.JSON;

      JSON         : constant JSON_Value := Read (Message);
      Message_Type : constant String     := Get (JSON, "type");
      Channel_ID   : constant String     := Get (Get (JSON, "channel"), "id");

      HTTP_Response : AWS.Response.Data;

   begin
      Put_Line ("Message type " & Message_Type);
      if Message_Type = "StasisStart" then
         ARI.Query
           (Method   => Asterisk.ARI.HTTP_POST,
            Path     => "channels/" & Channel_ID
                          & "/play?media=sound:tt-monkeys",
            Response => HTTP_Response);

         Put_Line ("Response status: "
           & AWS.Response.Status_Code (HTTP_Response)'Image);
      end if;
   end On_Message;

   --Message_Body : Asterisk.ARI.Parameters_Type
     --(Kind  => Asterisk.ARI.PK_Query,
     -- Count => 5);

begin
   ARI.Create_Client
     (Host             => "10.0.0.14:8088",
      Username         => "BOBBY",
      Secret           => "Wasabi123",
      HTTP_Secure      => False,
      HTTP_Persistence => True);

   ARI.Connect_Client
     (Application => "first_test",
      WS_Secure   => False);

   Indefinite : loop
      if not ARI.Poll (Timeout => Duration'Last) then
         Put_Line ("WS poll timed out.");
      end if;
   end loop Indefinite;

end ARI_Client_Example;
