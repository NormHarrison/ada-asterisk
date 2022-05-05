package body Asterisk is

   ------------------
   -- Find_Pattern --
   ------------------

   function Find_Pattern
     (Pattern   : in String;
      In_String : in String) return Boolean
   is
   begin
      if Pattern'Length > In_String'Length then
         return False;
      end if;

      declare
         First : Positive := 1;
         Last  : Positive := Pattern'Last;

      begin
         loop
            if Pattern = In_String (First .. Last) then
               return True;
            end if;

            if Last = In_String'Length then
               return False;
            end if;

            First := First + 1;
            Last := Last + 1;
         end loop;
      end;

   end Find_Pattern;

   ----------------------
   -- Critical_Section --
   ----------------------

   protected body Critical_Section is

      entry Acquire when not Locked is
      begin
         Locked := True;
      end Acquire;

      procedure Release is
      begin
         Locked := False;
      end Release;

      function Is_Locked return Boolean
      is (Locked);

   end Critical_Section;

end Asterisk;
