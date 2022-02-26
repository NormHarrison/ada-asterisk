package body Asterisk.AGI.FastAGI is

   ---------------
   -- To_Socket --
   ---------------

   function To_Socket
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
      Address          : in     GNAT.Sockets.Inet_Addr_Type;
      Port             : in     GNAT.Sockets.Port_Type := 4573;
      Queue_Length     : in     Natural                := 15;
      Do_Reuse_Address : in     Boolean                := True)
   is
      use GNAT.Sockets;

   begin
      Server.Address :=
        (Family => <>,
         Addr   => Address,
         Port   => Port);

      Create_Socket
        (Socket => Server.Socket,
         Family => Server.Address.Family,
         Mode   => Socket_Stream);

      Set_Socket_Option
        (Socket => Server.Socket,
         Level  => Socket_Level,
         Option => (Name    => Reuse_Address,
                    Enabled => Do_Reuse_Address));

      Bind_Socket   (Server.Socket, Server.Address);
      Listen_Socket (Server.Socket, Queue_Length);

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
      GNAT.Sockets.Close_Socket (Server.Socket);
      --  ! Reset `Server.Address` to `No_Socket_Addr`?
   end Close_Server;

   ----------------------
   -- Get_Bind_Address --
   ----------------------

   function Get_Bind_Address
     (Server : in Server_Type) return GNAT.Sockets.Sock_Addr_Type
   is (Server.Address);

end Asterisk.AGI.FastAGI;
