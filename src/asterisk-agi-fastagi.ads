with GNAT.Sockets;

private with Socket_AIO;

--  NOTES:
--  - Default FastAGI port is 4573.
---------

package Asterisk.AGI.FastAGI is

   FastAGI_Accept_Timeout_Error : exception;

   type Session_Type
     (Argument_Count             : Natural;
      Supress_Hang_Up_Exceptions : Boolean;
      Address_Family             : GNAT.Sockets.Family_Type)
   is new Base_Session_Type with private;

   -----------------------------
   -- Session_Type primitives --
   -----------------------------

   function Get_Socket (Self : in Session_Type) return GNAT.Sockets.Socket_Type
     with Inline;
   --  Returns the underlying socket of the session, for use with
   --  subprograms from the GNAT.Sockets package (for example, setting
   --  or retreiving socket options).

   function Is_Connected (Self: in Session_Type) return Boolean with Inline;
   --  Returns True if the `Session_Type` instance is still connected to the
   --  remote Asterisk channel, otherwise False. The session is deemed as
   --  connected as long as `Disconnect` has not been called and the
   --  `Command` subprograms have not encountered IO exceptions while trying
   --  to send or receive data (meaning the Asterisk channel most likely closed
   --  the socket itself, or some type of network-level failure occurred).

   function Get_Peer_Name
     (Self : in Session_Type) return GNAT.Sockets.Sock_Addr_Type with Inline;
   --  Returns the full socket address of the remote peer
   --  that initiated the connection.

   procedure Disconnect (Self : in out Session_Type) with Inline;
   --  Closes the connection between the server and the Asterisk channel.
   --  Note that this does not hung up the channel, it only gives control
   --  of the channel back to the Asterisk dialplan. Conversely, hanging up
   --  the channel does not disconnect it (commands that can "run dead" will
   --  still work). This procedure does nothing if the session has already
   --  been disconnected.

   -----------------------------

   type Server_Type
     (Address_Family : GNAT.Sockets.Family_Type := GNAT.Sockets.Family_Inet) is
   limited private;
   --  The base server that accepts incoming connections from Asterisk. The
   --  instance should be passed into the `Create_Server` subprogram first.
   --  Afterwards, `Accept_Connection` is used to wait for and accept an
   --  inbound connection. Once a connection has been received, a `Session_Type`
   --  instance will be returned where all future interaction with the connected
   --  channel will occur. The discriminant `Address_Family` should be set to
   --  the family of the address which the server will be bound to during the
   --  call to `Create_Server`.

   -----------------------------
   -- Server_Type subprograms --
   -----------------------------

   Default_FastAGI_Port : constant := 4573;

   procedure Create_Server
     (Server           : in out Server_Type;
      Address          : in     GNAT.Sockets.Sock_Addr_Type;
      Queue_Length     : in     Natural := 15;
      Do_Reuse_Address : in     Boolean := False);
   --  Initializes a new FastAGI `Server_Type` instance, binding it to the
   --  specified `Address` and `Port` and then listening for connections.
   --  `Queue_Length` sets the maximum number of connections that may pend on
   --  the socket before any additional ones get declined. `Do_Reuse_Address`
   --  allows a previously used socket address to be re-binded to (helps
   --  prevent socket error "[98] Address already in use" during quick
   --  restarts of the server).

   procedure Accept_Connection
     (Server  : in out Server_Type;
      Session :    out Session_Type;
      Timeout : in     Duration := GNAT.Sockets.Forever);
   --  Accepts an awaiting FastAGI connection, filling in the `Session` out
   --  parameter with an instantiated `Session_Type` instance. A session
   --  represents a connection to a remote Asterisk channel. This subprogram
   --  can be called in a loop to repeatedly accept future connections, just
   --  like a regular socket. `Timeout` sets a limit (in seconds) on how long a
   --  call to this subprogram may block execution before raising the exception
   --  `FastAGI_Accept_Timeout_Error`.

   procedure Close_Server (Server : in out Server_Type) with Inline;
   --  Closes an active `Server_Type` instance, preventing new inbound
   --  connections. Does nothing if the server hasn't been created yet.

   function Is_Listening (Server : in Server_Type) return Boolean;

   function Get_Bound_Port
     (Server : in Server_Type) return GNAT.Sockets.Port_Type with Inline;
   --  Returns the port to which the server's socket was bound
   --  to upon invoking `Create_Server`. Useful for when the port was
   --  set dynamically. Returns `GNAT.Sockets.No_Port` when
   --  the server isn't currently listening for connections.

   ---------------------------
   -- Start of Private part --
   ---------------------------

private

   type Session_Type
     (Argument_Count             : Natural;
      Supress_Hang_Up_Exceptions : Boolean;
      Address_Family             : GNAT.Sockets.Family_Type)
   is new Base_Session_Type (Argument_Count, Supress_Hang_Up_Exceptions)
   with record
      Address   : GNAT.Sockets.Sock_Addr_Type (Address_Family);
      Channel   : aliased Socket_AIO.Socket_Channel_Type
        (Buffer_Start_Size => 100,
         Line_Ending       => Socket_AIO.Line_Feed,
         Recursion_Limit   => 10);
   end record;

   type Server_Type
     (Address_Family : GNAT.Sockets.Family_Type := GNAT.Sockets.Family_Inet)
   is record
      Socket    : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
      Port      : GNAT.Sockets.Port_Type   := GNAT.Sockets.No_Port;
      Listening : Boolean                  := False;
   end record;

end Asterisk.AGI.FastAGI;
