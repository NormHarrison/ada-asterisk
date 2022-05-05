private with Ada.Strings.Unbounded;


package Asterisk is

--  ! Can we make use of the "Pure" pragma here?

   --  ! This parent package holds subprograms and types that are made use
   --  of by multiple child packages... (will we have entities to place in
   --  the visible part?)

private

   Line_Feed : constant Character := Character'Val (10);
   CRLF      : constant String := Character'Val (13) & Line_Feed;

   type Unbounded_String_Array is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   type Message_Extraction_Target is (Key, Value);

   function Find_Pattern
     (Pattern   : in String;
      In_String : in String) return Boolean;

   protected type Critical_Section is
      --  ! Specialize for IO so declaring two instances of this type
      --    ins't needed for protecting against read and writes separately.
      entry Acquire;
      procedure Release;
      function Is_Locked return Boolean;
      --  ! Possibly change name of all subprograms.
   private
      Locked : Boolean := False;
   end Critical_Section;

end Asterisk;
