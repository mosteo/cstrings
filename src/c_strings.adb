package body C_Strings is

   -----------
   -- Value --
   -----------

   function Value (Str : CS.Chars_Ptr; Free : Boolean := False) return String is
   begin
      if Str = CS.Null_Ptr then
         return "";
      end if;

      return Ada_Str : constant String := CS.Value (Str) do
         if Free then
            declare
               Freeable : CS.Chars_Ptr := Str;
            begin
               CS.Free (Freeable);
            end;
         end if;
      end return;
   end Value;

end C_Strings;
