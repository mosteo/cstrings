with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

package C_Strings with Preelaborate is

   package CS renames Interfaces.C.Strings;
   
   Null_Ptr : CS.Chars_Ptr renames CS.Null_Ptr;
   
   type C_String (<>) is tagged limited private;
   
   function To_C (S : String) return C_String;
   
   function To_Ptr (Str                   : aliased C_String;
                    Null_Instead_Of_Empty :         Boolean := True) 
                    return CS.Chars_Ptr;
   --  Null_Instead_Of_Empty: if Str is an empty string,
   --    a null will be returned instead of a pointer to null.
   
   function Value (Str : CS.Chars_Ptr) return String renames CS.Value;
   
-- function To_Ptr (S : String) return CS.Chars_Ptr;
-- This was a bad idea, because the internal instance goes out of scope
--   and the pointer is suddenly dangling (which is somewhat concerning, 
--   because I'd thought that shouldn't compile! No Unchecked_Access is used).
   
   
private
   
   package C  renames Interfaces.C;
   
   use all type C.Size_T;
   
   --  Holds a C string
   type C_String (Len : C.size_t) is tagged limited record
      Cstr : aliased C.Char_Array (1 .. Len);
   end record;     
   
   ----------
   -- To_C --
   ----------

   function To_C (S : String) return C_String is
     (Len  => S'Length + 1, -- For the null terminator
      Cstr => C.To_C (S));   
   
   type Char_Access is access constant C.Char;
   
   ------------------------------
   -- Char_Access_To_Chars_Ptr --
   ------------------------------

   function Char_Access_To_Chars_Ptr is new 
     Ada.Unchecked_Conversion (Char_Access, CS.Chars_Ptr);
   --  I have not found a way to obtain a Chars_Ptr from a member Char_Array
   --    because the explicit bounds above change the subtype somehow, and
   --    CS.To_Chars_Ptr no longer works.
   
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
