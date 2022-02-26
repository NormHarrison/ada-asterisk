with Ada.Text_IO;

with GNAT.OS_Lib;


package body Asterisk.AGI.ProcessAGI is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Session_Type) is
   begin
      Interrupt_Handler.Set_Session (Self'Unrestricted_Access);

      Self.Channel.Set_Pipes
        (Input  => Ada.Text_IO.Standard_Input,
         Output => Ada.Text_IO.Standard_Output);

      Self.Agnostic_Channel := Self.Channel'Unrestricted_Access;
      --  This is safe because `Self.Channel` has the same scope
      --  as `Self.Agnostic_Channel`, therefore their life times are
      --  identical and `Self.Agnostic_Channel` can never dangle.

      Self.Variables (AGI_Network)        := To_Unbounded_String ("no");
      Self.Variables (AGI_Network_Script) := Null_Unbounded_String;

      Self.Parse_Initial_Message
        (Starting_Variable   => AGI_Request,
         Last_Variable_Index => Last_Variable_Index - 2);

   end Initialize;

   -----------------------------
   -- Set_Interrupt_Callbacks --
   -----------------------------

   procedure Set_Interrupt_Callbacks
     (SIGHUP_Callback  : in Interrupt_Callback_Access;
      SIGPIPE_Callback : in Interrupt_Callback_Access)
   is
   begin
      Interrupt_Handler.Set_Callbacks (SIGHUP_Callback, SIGPIPE_Callback);
   end Set_Interrupt_Callbacks;

   ----------------
   -- Read_Audio --
   ----------------

   function Read_Audio (Buffer : out Byte_Array) return Integer is
      EAGI_FD : constant GNAT.OS_Lib.File_Descriptor := 3;

   begin
      return GNAT.OS_Lib.Read
        (FD => EAGI_FD,
         A  => Buffer'Address,
         N  => Buffer'Length);
   end Read_Audio;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   protected body Interrupt_Handler is

      procedure Set_Session (Session_Acc : Session_Access) is
      begin
         The_Session := Session_Acc;
      end Set_Session;

      procedure Set_Callbacks
        (SIGHUP_Callback  : in Interrupt_Callback_Access;
         SIGPIPE_Callback : in Interrupt_Callback_Access)
      is
      begin
         On_SIGHUP  := SIGHUP_Callback;
         On_SIGPIPE := SIGPIPE_Callback;
      end Set_Callbacks;

      procedure SIGHUP_Handler is
      begin
         if The_Session /= null and then
              On_SIGHUP /= null
         then
            The_Session.Hung_Up := True;
            On_SIGHUP (The_Session.all);
         end if;
      end SIGHUP_Handler;

      procedure SIGPIPE_Handler is
      begin
         if The_Session /= null and then
             On_SIGPIPE /= null
         then
            The_Session.Hung_Up := True;
            --  ! Technically, we don't know if the channel was hung up,
            --    we just can't communicate with it anymore.
            On_SIGPIPE (The_Session.all);
         end if;
      end SIGPIPE_Handler;

   end Interrupt_Handler;

end Asterisk.AGI.ProcessAGI;
