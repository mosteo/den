with C_Strings; use C_Strings;

private package Den.OS with Preelaborate is

   --  Imports for the common C functions in all supported OSes

   function C_Create_Link (Target,
                           Name   : Chars_Ptr;
                           Is_Dir : C.C_bool)
                           return C.int
        with Import, Convention => C;

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean);
   --  Ada variant

end Den.OS;