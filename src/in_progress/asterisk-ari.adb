with Ada.Text_IO;

with Ada.Unchecked_Deallocation;


package body Asterisk.ARI is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => AWS.Client.HTTP_Connection,
      Name   => AWS.Client.HTTP_Connection_Access);

   ----------------------------
   -- Concatenate_Parameters --
   ----------------------------

   --  ! Make public?
   function Concatenate_Parameters
     (Parameters : in Parameters_Type) return String
   is
      Parameter_Index : Natural := 0;

      --------------------
      -- Recursive_Part --
      --------------------

      function Recursive_Part (Data_So_Far : in String) return String is
      begin
         if Parameter_Index = Parameters.Count then
            return Data_So_Far;

         else
            Parameter_Index := Parameter_Index + 1;

            return Recursive_Part
              (Data_So_Far & To_String (Parameters.Keys   (Parameter_Index))
                           & To_String (Parameters.Values (Parameter_Index)));
         end if;
      end Recursive_Part;

   begin
      if Parameters.Count = 0 then
         return "";
      else
         return Recursive_Part ("");
      end if;
   end Concatenate_Parameters;

   --------------------------
   -- Parameter_Syntax_Set --
   --------------------------

   function Parameter_Syntax_Set
     (For_Kind : in Parameters_Kind) return Parameters_Symbol_Array
   is (PSK_Begin               =>
         Parameters_Syntax (For_Kind) (PSK_Begin),

       PSK_Key_Value_Container =>
         Parameters_Syntax (For_Kind) (PSK_Key_Value_Container),

       PSK_Key_Value_Split     =>
         Parameters_Syntax (For_Kind) (PSK_Key_Value_Split),

       PSK_Parameter_Split     =>
         Parameters_Syntax (For_Kind) (PSK_Parameter_Split),

       PSK_End                 =>
         Parameters_Syntax (For_Kind) (PSK_End));

   ---------------
   -- Build_Key --
   ---------------

   function Build_Key
     (Parameters : in Parameters_Type;
      Key        : in String) return String
   is
      PS : constant Parameters_Symbol_Array := Parameter_Syntax_Set
        (For_Kind => Parameters.Kind);

   begin
      return (if Parameters.Active_Index = 1 then PS (PSK_Begin).all else "")
        & PS (PSK_Key_Value_Container).all
        & Key
        & PS (PSK_Key_Value_Container).all
        & PS (PSK_Key_Value_Split).all;
   end Build_Key;

   -----------------
   -- Build_Value --
   -----------------

   function Build_Value
     (Parameters : in Parameters_Type;
      Value      : in String;
      Is_String  : in Boolean) return String
   is
      PS : constant Parameters_Symbol_Array := Parameter_Syntax_Set
        (For_Kind => Parameters.Kind);

   begin
      return (if Is_String then PS (PSK_Key_Value_Container).all else "")
        & Value
        & (if Is_String then PS (PSK_Key_Value_Container).all else "")
        & (if Parameters.Active_Index = Parameters.Count then
              PS (PSK_End).all
           else
              PS (PSK_Parameter_Split).all);
   end Build_Value;

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameters : in out Parameters_Type;
      Key        : in     String;
      Value      : in     String;
      Is_String  : in     Boolean := True)
   is
   begin
      Parameters.Active_Index := Parameters.Active_Index + 1;

      Parameters.Keys (Parameters.Active_Index) :=
        To_Unbounded_String (Build_Key (Parameters, Key));

      Parameters.Values (Parameters.Active_Index) :=
        To_Unbounded_String (Build_Value (Parameters, Value, Is_String));
   end Add;

   ------------
   -- Update --
   ------------

   procedure Update
     (Parameters : in out Parameters_Type;
      Key        : in     String;
      Value      : in     String;
      Is_String  : in     Boolean := True)
   is
      Original_Active_Index : constant Natural := Parameters.Active_Index;

   begin
      for Index in 1 .. Parameters.Count loop
         if Find_Pattern
              (Pattern   => Key,
               In_String => To_String (Parameters.Keys (Index)))
         then
            Parameters.Active_Index := Index;
            Parameters.Values (Index) := To_Unbounded_String
              (Build_Value (Parameters, Value, Is_String));
            Parameters.Active_Index := Original_Active_Index;
            return;
         end if;
      end loop;

      Parameters.Active_Index := Original_Active_Index;
   end Update;

   -------------------
   -- Create_Client --
   -------------------

   procedure Create_Client
     (Self             : in out Client_Type;
      Host             : in     String;
      Username         : in     String;
      Secret           : in     String;
      HTTP_Secure      : in     Boolean;
      HTTP_Persistence : in     Boolean := True)
   is
   begin
      if Self.HTTP_Client_Connected or else
         Self.WS_Client_Connected
      then
         Self.Close_Client;
      end if;

      Self.HTTP_Persistence := HTTP_Persistence;
      Self.HTTP_Secure := HTTP_Secure;

      Self.Username := To_Unbounded_String (Username);
      Self.Secret := To_Unbounded_String (Secret);

      Self.HTTP_Client := new AWS.Client.HTTP_Connection;

      AWS.Client.Create
        (Connection => Self.HTTP_Client.all,
         Host       => (if HTTP_Secure then "https://" else "http://") & Host,
         User       => Username,
         Pwd        => Secret,
         Persistent => HTTP_Persistence,
         User_Agent => User_Agent);

      Self.HTTP_Client_Connected := True;
      --  ! Is this the right location to do this in?
   end Create_Client;

   --------------------
   -- Connect_Client --
   --------------------

   procedure Connect_Client
     (Self        : in out Client_Type;
      Application : in     String;
      WS_Secure   : in     Boolean)
   is
      URL  : constant String := AWS.Client.Host (Self.HTTP_Client.all);
      Host : constant String :=
        (if Self.HTTP_Secure then
            URL (9 .. URL'Last)
         else
            URL (8 .. URL'Last));

      WS_URI : constant String := (if WS_Secure then "wss://" else "ws://")
        & Host
        & "/ari/events?api_key="
        & To_String (Self.Username)
        & ':'
        & To_String (Self.Secret)
        & "&app="
        & Application;

   begin
      if Self.WS_Client_Connected then
         Self.Close (Message => "");
         --  Closure of WebSocket client.
         Self.WS_Client_Connected := False;
      end if;

      Self.Connect (WS_URI);
      --  Connection of WebSocket client.

      Self.WS_Client_Connected := True;
      --  ! Is this the right location to do this in?

      --  ! Exception handler for `AWS.Client.Connection_Error` where
      --    `Self.HTTP_Client_Connected` is set to false and then re-raised?
   end Connect_Client;

   -----------
   -- Query --
   -----------

   procedure Query
     (Self             : in out Client_Type;
      Method           : in     HTTP_Method_Kind;
      Path             : in     String;
      Query_Parameters : in     Parameters_Type := No_Parameters;
      Body_Parameters  : in     Parameters_Type := No_Parameters;
      Response         :    out AWS.Response.Data)
   is
      Actual_Path : constant String := AWS.Client.Host (Self.HTTP_Client.all)
        & "/ari/" & Path & Concatenate_Parameters (Query_Parameters);

   begin
      if not Self.HTTP_Client_Connected then
         Put_Line ("HTTP CLIENT ISN'T CONNECTED INSIDE `QUERY`");
      end if;

      case Method is

         when HTTP_POST   =>
            AWS.Client.Post
              (Connection   => Self.HTTP_Client.all,
               Result       => Response,
               Data         => Concatenate_Parameters (Body_Parameters),
               URI          => Actual_Path,
               Content_Type => "application/json");

         when HTTP_PUT    =>
            AWS.Client.Put
              (Connection => Self.HTTP_Client.all,
               Result     => Response,
               Data       => Concatenate_Parameters (Body_Parameters),
               URI        => Actual_Path);

         when HTTP_GET    =>
            AWS.Client.Get
              (Connection => Self.HTTP_Client.all,
               Result     => Response,
               URI        => Actual_Path);

         when HTTP_DELETE =>
            AWS.Client.Delete
              (Connection => Self.HTTP_Client.all,
               Result     => Response,
               Data       => Concatenate_Parameters (Body_Parameters),
               URI        => Actual_Path);

      end case;

      --  ! Exception handler for `AWS.Client.Connection_Error` where
      --    `Self.HTTP_Client_Connected` is set to false and then re-raised?
   end Query;

   ------------------
   -- Close_Client --
   ------------------

   procedure Close_Client (Self : in out Client_Type) is
   begin
      Self.HTTP_Persistence := False;
      Self.HTTP_Secure := False;
      Self.HTTP_Client_Connected := False;
      Self.WS_Client_Connected := False;

      Self.Username := Null_Unbounded_String;
      Self.Secret := Null_Unbounded_String;

      Self.Close (Message => "");
      --  Closure of WebSocket client.

      AWS.Client.Close (Self.HTTP_Client.all);
      Free (Self.HTTP_Client);
   end Close_Client;

end Asterisk.ARI;
