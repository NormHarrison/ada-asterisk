with AWS.Net.Websocket;
with AWS.Response;

private with Ada.Strings.Unbounded;

private with AWS.Client;


package Asterisk.ARI is

   User_Agent : constant String := "Ada Asterisk ARI Client";

   type Parameters_Kind is (PK_Body, PK_Query);

   type Parameters_Type
     (Kind  : Parameters_Kind;
      Count : Natural)
   is private;

   No_Parameters : constant Parameters_Type;

   --  ! Only exposed while testing;
   function Concatenate_Parameters
     (Parameters : in Parameters_Type) return String;

   procedure Add
     (Parameters : in out Parameters_Type;
      Key        : in     String;
      Value      : in     String;
      Is_String  : in     Boolean := True);
   --  ! Use preconditions to prevent adding more parameters
   --    than the type was instantiated for?

   procedure Update
     (Parameters : in out Parameters_Type;
      Key        : in     String;
      Value      : in     String;
      Is_String  : in     Boolean := True);



   type Client_Type is new AWS.Net.WebSocket.Object with private;

   procedure Create_Client
     (Self             : in out Client_Type;
      Host             : in     String;
      Username         : in     String;
      Secret           : in     String;
      HTTP_Secure      : in     Boolean;
      HTTP_Persistence : in     Boolean := True);

   procedure Connect_Client
     (Self        : in out Client_Type;
      Application : in     String;
      WS_Secure   : in     Boolean);

   --  Primitive operations inherited from `AWS.Net.WebSocket.Object`,
   --  to be overridden again by the end user.

   overriding procedure On_Message
     (Socket  : in out Client_Type;
      Message : in     String) is null;

   overriding procedure On_Open
     (Socket  : in out Client_Type;
      Message : in     String) is null;

   overriding procedure On_Close
     (Socket  : in out Client_Type;
      Message : in     String) is null;

   overriding procedure On_Error
     (Socket  : in out Client_Type;
      Message : in     String) is null;

   type HTTP_Method_Kind is (HTTP_POST, HTTP_PUT, HTTP_GET, HTTP_DELETE);

   procedure Query
     (Self             : in out Client_Type;
      Method           : in     HTTP_Method_Kind;
      Path             : in     String;
      Query_Parameters : in     Parameters_Type := No_Parameters;
      Body_Parameters  : in     Parameters_Type := No_Parameters;
      Response         :    out AWS.Response.Data);
   --  ! Rename to `Send`, `Do`, `Send/Do_Query` or something similiar?

   procedure Close_Client (Self : in out Client_Type);


private

   type Parameters_Symbol_Kind is
     (PSK_Begin,
      PSK_Key_Value_Container,
      PSK_Key_Value_Split,
      PSK_Parameter_Split,
      PSK_End);

   type Parameters_Symbol_Array is
     array (Parameters_Symbol_Kind) of Ada.Strings.Unbounded.String_Access;
   --  ! Look into the possible `Implicit_Deference` aspect?

   type Parameters_Syntax_Array is
     array (Parameters_Kind) of Parameters_Symbol_Array;

   Parameters_Syntax : constant Parameters_Syntax_Array :=
       (PK_Body  =>
          (PSK_Begin               => new String'("{"),
           PSK_Key_Value_Container => new String'(""""),
           PSK_Key_Value_Split     => new String'(":"),
           PSK_Parameter_Split     => new String'(","),
           PSK_End                 => new String'("}")),

        PK_Query =>
          (PSK_Begin               => new String'("?"),
           PSK_Key_Value_Container => new String'(""),
           PSK_Key_Value_Split     => new String'("="),
           PSK_Parameter_Split     => new String'("?"),
           PSK_End                 => new String'("")));

   type Parameters_Type
     (Kind  : Parameters_Kind;
      Count : Natural)
   is record
      Keys   : Unbounded_String_Array (1 .. Count) :=
        (others => Ada.Strings.Unbounded.Null_Unbounded_String);

      Values : Unbounded_String_Array (1 .. Count) :=
        (others => Ada.Strings.Unbounded.Null_Unbounded_String);

      Active_Index : Natural := 0;
   end record;

   No_Parameters : constant Parameters_Type :=
     (Count        => 0,
      Kind         => PK_Body,
      Keys         => <>,
      Values       => <>,
      Active_Index => <>);

   type ARI_WebSocket_Type is new AWS.Net.WebSocket.Object with null record;
   --  ! Move `WS_Client_Connected` component to this type?

   type Client_Type is new AWS.Net.WebSocket.Object with record
      HTTP_Client           : AWS.Client.HTTP_Connection_Access;
      HTTP_Persistence      : Boolean := False;
      HTTP_Secure           : Boolean := False;
      HTTP_Client_Connected : Boolean := False;

      WS_Client_Connected : Boolean := False;
      --  ! Probably isn't needed since the user can override `On_Close`.

      Username : Ada.Strings.Unbounded.Unbounded_String;
      Secret   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Asterisk.ARI;
