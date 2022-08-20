private with Ada.Interrupts.Names;

private with Text_Pipe_AIO;


package Asterisk.AGI.ProcessAGI is
--  ! Consider renaming: "TraditionalAGI", "RegularAGI", "PipeAGI".

   type Session_Type
     (Argument_Count             : Natural;
      Supress_Hang_Up_Exceptions : Boolean)
   is new Base_Session_Type with private;

   procedure Initialize (Self : in out Session_Type);
   --  Populates the internal structures of the session type with the
   --  initialization data sent by Asterisk. This primitive should be called
   --  first before using the session instance with any other operation.

   type Interrupt_Callback_Access is
     access procedure (Self : in out Session_Type);
   --  ! Possibly take away parameter, which would require the user
   --    to declare their AMI session instance before their callbacks.

   procedure Set_Interrupt_Callbacks
     (SIGHUP_Callback  : in Interrupt_Callback_Access;
      SIGPIPE_Callback : in Interrupt_Callback_Access);

   --  ! Either split into separate procedures, or find a different
   --    way to offer (or encourage) such functionality.

   --  Sets the callback procedures that are invoked when the respective
   --  signals occurr. `SIGHUP_Callback` is invoked when the connected
   --  Asterisk channel hangs up (sending the program the signal SIGHUP (1)).
   --  `SIGPIPE_Callback` is invoked when the signal SIGPIPE (13) occurrs,
   --  indicating communication between the program and Asterisk was destroyed
   --  is some way. Passing `null` for either parameter removes any previously
   --  set callback.

   type Byte is range 0 .. 255;

   type Byte_Array is array (Positive range <>) of Byte;

   function Read_Audio (Buffer : out Byte_Array) return Integer with Inline;
   --  If this program was launched via the `EAGI()` dialplan application,
   --  the connected channel's inbound audio stream can be obtained using
   --  this function. Audio is returned as an array of bytes via the out
   --  parameter `Buffer`. The length of the provided byte array object
   --  determines how many bytes will be read, the actual number of bytes
   --  read is returned as the function's result. If the number of bytes
   --  actually read is less than the size of the array, the end of the audio
   --  stream has been reached and no more invocations of this function should
   --  occurr. The format of the audio data provided by Asterisk can be changed
   --  via the `EAGI_AUDIO_FORMAT` Asterisk dialplan variable.

   ---------------------------
   -- Start of Private part --
   ---------------------------

private

   type Session_Type
     (Argument_Count             : Natural;
      Supress_Hang_Up_Exceptions : Boolean)
   is new Base_Session_Type (Argument_Count, Supress_Hang_Up_Exceptions)
   with record
      Channel : aliased Text_Pipe_AIO.Text_Channel_Type;
   end record;

   type Session_Access is access all Session_Type;

   protected Interrupt_Handler is
      procedure Set_Session (Session_Acc : in Session_Access);

      procedure Set_Callbacks
        (SIGHUP_Callback  : in Interrupt_Callback_Access;
         SIGPIPE_Callback : in Interrupt_Callback_Access);

      procedure SIGHUP_Handler with
        Interrupt_Handler => True,
        Attach_Handler    => Ada.Interrupts.Names.SIGHUP;

      procedure SIGPIPE_Handler with
        Interrupt_Handler => True,
        Attach_Handler    => Ada.Interrupts.Names.SIGPIPE;

   private
      The_Session : access Session_Type;

      On_SIGHUP  : Interrupt_Callback_Access;
      On_SIGPIPE : Interrupt_Callback_Access;
   end Interrupt_Handler;

end Asterisk.AGI.ProcessAGI;
