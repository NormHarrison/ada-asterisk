package body Asterisk.AGI.FastAGI is

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket
     (Self : in Session_Type) return GNAT.Sockets.Socket_Type
   is (Self.Channel.To_Socket);

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected (Self: in Session_Type) return Boolean
   is (Self.Channel.Is_Connected);

   -------------------
   -- Get_Peer_Name --
   -------------------

   function Get_Peer_Name
     (Self : in Session_Type) return GNAT.Sockets.Sock_Addr_Type
   is (Self.Address);

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Self : in out Session_Type) is
   begin
      Self.Channel.Close;
   end Disconnect;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Server           : in out Server_Type;
      Address          : in     GNAT.Sockets.Sock_Addr_Type;
      Queue_Length     : in     Natural := 15;
      Do_Reuse_Address : in     Boolean := False)
   is
      use GNAT.Sockets;

   begin
      if Server.Listening then
         Close_Server (Server);
      end if;

      Create_Socket
        (Socket => Server.Socket,
         Family => Server.Address_Family,
         Mode   => Socket_Stream);

      if Do_Reuse_Address then
         Set_Socket_Option
           (Socket => Server.Socket,
            Level  => Socket_Level,
            Option => (Name    => Reuse_Address,
                       Enabled => True));
      end if;

      Bind_Socket (Server.Socket, Address);
      Server.Port := Get_Socket_Name (Server.Socket).Port;

      Listen_Socket (Server.Socket, Queue_Length);
      Server.Listening := True;

   end Create_Server;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (Server  : in out Server_Type;
      Session :    out Session_Type;
      Timeout : in     Duration := GNAT.Sockets.Forever)
   is
      use type GNAT.Sockets.Selector_Status;

      Connection    : GNAT.Sockets.Socket_Type;
      Accept_Status : GNAT.Sockets.Selector_Status;

   begin
      GNAT.Sockets.Accept_Socket
        (Server  => Server.Socket,
         Socket  => Connection,
         Address => Session.Address,
         Timeout => Timeout,
         Status  => Accept_Status);

      if Accept_Status /= GNAT.Sockets.Completed then
         raise FastAGI_Accept_Timeout_Error;
         --  ! Consider indicating timeouts via by setting the
         --    `Self.Connected` component to `False` upon return.
      end if;

      Session.Channel.Set_Socket (Connection);

      Session.Agnostic_Channel := Session.Channel'Unrestricted_Access;
      --  This is safe because `Self.Channel` has the same scope
      --  as `Self.Agnostic_Channel`, therefore their life times are
      --  identical and `Self.Agnostic_Channel` can never dangle.

      Session.Parse_Initial_Message
        (Starting_Variable   => Environment_Variable_Name'First,
         Last_Variable_Index => Last_Variable_Index);

   end Accept_Connection;

   ------------------
   -- Close_Server --
   ------------------

   procedure Close_Server (Server : in out Server_Type) is
   begin
      if Server.Listening then
         GNAT.Sockets.Close_Socket (Server.Socket);
         Server.Socket    := GNAT.Sockets.No_Socket;
         Server.Port      := GNAT.Sockets.No_Port;
         Server.Listening := False;
      end if;
   end Close_Server;

   ------------------
   -- Is_Listening --
   ------------------

   function Is_Listening (Server : in Server_Type) return Boolean
   is (Server.Listening);

   --------------------
   -- Get_Bound_Port --
   --------------------

   function Get_Bound_Port
     (Server : in Server_Type) return GNAT.Sockets.Port_Type
   is (Server.Port);

end Asterisk.AGI.FastAGI;
