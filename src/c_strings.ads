private with Ada.Finalization;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

package C_Strings with Preelaborate is

   -- Intended use: in calls to C subprograms that expect a pointer to char,
   --   which -fada-dump-spec translates as Chars_Ptr, do:
   --   Call_To_C (C_Strings.To_C ("whatever").To_Ptr);
   -- The Ada string memory is reused; no allocations are taking place.

   -- There are a few extra subprograms to work in the C -> Ada directions too,
   --   but those are not the main purpose of the library.

   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;

   subtype Chars_Ptr is CS.Chars_Ptr;

   Null_Ptr : CS.Chars_Ptr renames CS.Null_Ptr;

   type C_String (<>) is tagged limited private;

   function Size   (This : C_String) return Natural;
   function C_Size (This : C_String) return C.Size_T;
   --  Total reserved space, not the logical length with or without any final \0

   function To_Ada (C : C_String) return String;

   function To_C (S : String) return C_String;

   function Buffer (Length : Positive) return C_String;
   --  Allocate an uninitialized buffer to e.g. retrieve strings from C side

   function Unchecked_To_Ptr (Str : access Character) return CS.Chars_Ptr;
   --  Direct cast. No copy, no nul appended, no nothing! You must know what
   --  you're doing

   function To_Ptr (Str                   : aliased C_String;
                    Null_Instead_Of_Empty :         Boolean := True)
                    return CS.Chars_Ptr;
   --  Null_Instead_Of_Empty: if Str is an empty string,
   --    a null will be returned instead of a pointer to null.

   function Value (Str  : CS.Chars_Ptr;
                   Free : Boolean := False)
                   return String;
   --  Will return "" for either an empty string or a NULL ptr (no string at
   --  all). If Free, the original Str is freed after being copied

private

   use Ada.Finalization;

   use all type C.Size_T;
   use all type CS.Chars_Ptr;

   --  Holds a C string
   type C_String (Len : C.size_t) is new Limited_Controlled with record
      Owned : Boolean;
      Cstr  : aliased C.Char_Array (1 .. Len);
   end record;

   ----------
   -- Size --
   ----------

   function Size   (This : C_String) return Natural is (Natural (This.Len));

   ------------
   -- C_Size --
   ------------

   function C_Size (This : C_String) return C.Size_T is (This.Len);

   ------------
   -- To_Ada --
   ------------

   function To_Ada (C : C_String) return String is
      (Interfaces.C.To_Ada (C.Cstr));

   ----------
   -- To_C --
   ----------

   function To_C (S : String) return C_String is
     (Limited_Controlled with
      Len   => S'Length + 1, -- For the null terminator
      Cstr  => C.To_C (S),
      Owned => False);

   ------------
   -- Buffer --
   ------------

   function Buffer (Length : Positive) return C_String is
     (Limited_Controlled with
      Len   => C.Size_T (Length),
      Cstr  => <>,
      Owned => False);

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
   is (Ada_Char_To_Chars_Ptr (Ada_Char_Access (Str)));

end C_Strings;
