with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

package C_Strings with Preelaborate is

   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;
   
   Null_Ptr : CS.Chars_Ptr renames CS.Null_Ptr;
   
   type C_String (<>) is tagged limited private;
   
   function To_C (S : String) return C_String;
   
   function To_Ptr (Str                   : aliased C_String;
                    Null_Instead_Of_Empty :         Boolean := True) 
                    return CS.Chars_Ptr;
   
   function Value (Str : CS.Chars_Ptr) return String renames CS.Value;
   
-- function To_Ptr (S : String) return CS.Chars_Ptr;
-- This was a bad idea, because the internal instance goes out of scope
--   and the pointer is suddenly dangling (which is somewhat concerning, 
--   because I'd thought that shouldn't compile).
   
   
private
   
   use all type C.Size_T;
   
   type C_String (Len : C.size_t) is tagged limited record
      Cstr : aliased C.Char_Array (1 .. Len);
   end record;
   --  Convenience type for the many conversions       
   
   ----------
   -- To_C --
   ----------

   function To_C (S : String) return C_String is
     (Len => S'Length + 1, 
      Cstr => C.To_C (S));   
   
   type Char_Access is access constant C.Char;
   
   ------------------------------
   -- Char_Access_To_Chars_Ptr --
   ------------------------------

   function Char_Access_To_Chars_Ptr is new 
     Ada.Unchecked_Conversion (Char_Access, CS.Chars_Ptr);
   
   ------------
   -- To_Ptr --
   ------------

   function To_Ptr (Str                   : aliased C_String;
                    Null_Instead_Of_Empty :         Boolean := True) 
                    return CS.Chars_Ptr is
     (if Null_Instead_Of_Empty and then Str.Len = 0 
      then Null_Ptr
      else Char_Access_To_Chars_Ptr (Str.Cstr (Str.Cstr'First)'Access));
   --  This obviously presumes the pointer won't be kept elsewhere.
   --  We shall see if this blows up in our face or what.

end C_Strings;
