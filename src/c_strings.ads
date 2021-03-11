with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

package C_Strings with Preelaborate is

   -- Intended use: in calls to C subprograms that expect a pointer to char,
   --   which -fada-dump-spec translates as Chars_Ptr, do:
   --   Call_To_C (C_Strings.To_C ("whatever").To_Ptr);

   package CS renames Interfaces.C.Strings;

   Null_Ptr : CS.Chars_Ptr renames CS.Null_Ptr;

   type C_String (<>) is tagged limited private;

   function To_Ada (C : C_String) return String;

   function To_C (S : String) return C_String;

   function Unchecked_To_Ptr (Str : access Character) return CS.Chars_Ptr;
   --  Direct cast. No copy, no nul appended, no nothing! You must know what
   --  you're doing

   function To_Ptr (Str                   : aliased C_String;
                    Null_Instead_Of_Empty :         Boolean := True)
                    return CS.Chars_Ptr;
   --  Null_Instead_Of_Empty: if Str is an empty string,
   --    a null will be returned instead of a pointer to null.

   function Value (Str : CS.Chars_Ptr) return String renames CS.Value;

private

   package C  renames Interfaces.C;

   use all type C.Size_T;

   --  Holds a C string
   type C_String (Len : C.size_t) is tagged limited record
      Cstr : aliased C.Char_Array (1 .. Len);
   end record;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (C : C_String) return String is
      (Interfaces.C.To_Ada (C.Cstr));

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
      else Char_Access_To_Chars_Ptr
             (Str.Cstr (Str.Cstr'First)'Unchecked_Access));
   --  This obviously presumes the pointer won't be kept elsewhere.
   --  We shall see if this blows up in our face or what.

   ----------------------
   -- Unchecked_To_Ptr --
   ----------------------

   type Ada_Char_Access is access all Character;

   function Ada_Char_To_Chars_Ptr is new
     Ada.Unchecked_Conversion (Ada_Char_Access, CS.Chars_Ptr);

   function Unchecked_To_Ptr (Str : access Character) return CS.Chars_Ptr
   is (Ada_Char_To_Chars_Ptr (Str));

end C_Strings;
