package body Asterisk is

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
