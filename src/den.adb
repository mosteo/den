--  with Ada.Directories;

with Ada.IO_Exceptions;

with C_Strings;

with System;

package body Den is

   package C renames C_Strings.C;
   --  package Dirs renames Ada.Directories;
   package IOex renames Ada.IO_Exceptions;

   ---------------
   -- Operators --
   ---------------

   package body Operators is

      ---------
      -- "/" --
      ---------

      function "/" (L : Path; R : Relative_Path) return Path
      is (if L (L'Last) = Dir_Separator
          then L & R
          else L & Dir_Separator & R);

   end Operators;

   ---------
   -- "/" --
   ---------

   function "/" (L : Path; R : Relative_Path) return Path
                 renames Operators."/";

   ---------------
   -- Ancestors --
   ---------------

   function Ancestors (This : Normal_Path) return Sorted_Paths is
      P : constant Path_Parts := Parts (This);
   begin
      return Result : Sorted_Paths do
         for Part of P loop
            if Result.Is_Empty then
               Result.Insert (Part);
            else
               Result.Insert (Result.Last_Element / Part);
            end if;
         end loop;

         Result.Delete_Last;
      end return;
   end Ancestors;

   --------------------
   -- String_Is_Root --
   --------------------

   function String_Is_Root (This : String) return Boolean is
      subtype Drive_Letter is Character with Dynamic_Predicate =>
        Drive_Letter in 'a' .. 'z' | 'A' .. 'Z';
   begin
      return
        This = "/"
        or else
          (Dir_Separator = '\' -- on Windows
           and then
             (This = "\"
              or else
                 This = "\\" -- UNC format
              or else
                 This = "\\?\"
              or else
                 This = "\\.\"
              or else
                (This'Length = 3
                 and then This (This'Last - 1) = ':'
                 and then This (This'Last) = '\'
                 and then This (This'First) in Drive_Letter)
              or else
                (This'Length = 2
                 and then This (This'Last) = ':'
                 and then (This (This'First) in Drive_Letter))));
   end String_Is_Root;

   function Is_Absolute (This : Path) return Boolean
   is (GNAT.OS_Lib.Is_Absolute_Path (This)
       or else
         --  Special case of Network/UNC Windows paths
         (Dir_Separator = '\' and then
          This'Length >= 2 and then
          This (This'First .. This'First + 1) = "\\"));

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Path) return Boolean
   is (String_Is_Root (This));

   -----------------
   -- Is_Softlink --
   -----------------

   function Is_Softlink (This : Path) return Boolean
   is
      function C_Is_Softlink (Link : C_Strings.Chars_Ptr)
                              return C_Strings.C.int
        with Import, Convention => C;
   begin
      return C_Is_Softlink (C_Strings.To_C (This).To_Ptr) not in 0;
   end Is_Softlink;

   ---------------
   -- Can_Scrub --
   ---------------

   function Can_Scrub (This : String) return Boolean is
   begin
      return Scrub (This) /= "";
   exception
      when Bad_Path =>
         return False;
   end Can_Scrub;

   ---------------
   -- Canonical --
   ---------------

   function Canonical (This : Existing_Path) return Canonical_Path
   is (if OS_Canonical (This) = "" or else
          OS_Canonical (This) not in Canonical_Path
       then raise Bad_Path with
         "Cannot canonicalize: " & This
         & " [os_canonical: " & OS_Canonical (This) & "]"
       else OS_Canonical (This));

   -----------------
   -- Canonizable --
   -----------------

   function Canonizable (This : String) return Boolean is
   begin
      return Canonical (Scrub (This)) /= "";
   exception
      when others =>
         return False;
   end Canonizable;

   --------------
   -- Eat_Dots --
   --------------

   function Eat_Dots (This   :        Path;
                      Parted : in out Path_Parts;
                      I      : in out Integer)
                      return Boolean
   is
   begin
      if Parted (I) = "." then
         Parted.Delete (I);
         return True;
      elsif Parted (I) = ".." then
         Parted.Delete (I);
         if I = Parted.First_Index then
            raise Bad_Path with
              "Too many parents in path while normalizing: " & This;
            --  At root, drop as if it where "."
         else
            Parted.Delete (I - 1);
            I := I - 1;
         end if;
         return True;
      else
         return False;
      end if;
   end Eat_Dots;

   ----------
   -- Kind --
   ----------

   function Kind (This : Path; Resolve_Links : Boolean := False) return Kinds
   is
      -----------------
      -- File_Exists --
      -----------------
      --  Original in a-direct.adb. We use this to avoid use of
      --  non-preelaborable subprograms.
      function File_Exists (Name : String) return Boolean is
         function C_File_Exists (A : System.Address) return Integer;
         pragma Import (C, C_File_Exists, "__gnat_file_exists");

         C_Name : String (1 .. Name'Length + 1);

      begin
         C_Name (1 .. Name'Length) := Name;
         C_Name (C_Name'Last) := ASCII.NUL;
         return C_File_Exists (C_Name'Address) = 1;
      end File_Exists;

      package OS renames GNAT.OS_Lib;

   begin
      return
        (if Resolve_Links then
           (if not Is_Resolvable (This) then
                 Softlink
            else
               Kind (Canonical (This), Resolve_Links => False))
         elsif Is_Softlink (This) then
            Softlink
         elsif not File_Exists (This) then
            Nothing
         elsif OS.Is_Directory (This) then
            Directory
         elsif OS.Is_Regular_File (This) then
            File
         else
            Special);
   end Kind;

   ----------
   -- Name --
   ----------

   function Name (This : Path) return Part
   is (Parts (This).Last_Element);

   ------------
   -- Parent --
   ------------

   function Parent (This : Path) return Path
   is (if Is_Root (This) then
          This
       elsif This in Part then
          raise IOex.Use_Error with "Path has no parent: " & This
       else
          Parts (This).Up_To (Integer (Parts (This).Length) - 1).To_Path);

   -----------
   -- Parts --
   -----------

   function Parts (This : Path) return Path_Parts is
   begin
      if Is_Root (This) then
         return To_Vector (This);
      end if;

      return Result : Path_Parts do
         --  Special cases for Windows
         if Dir_Separator = '\' then
            if AAA.Strings.Has_Prefix (This, "\\?\") then
               Result :=
                 To_Vector ("\\?\")
                 & Split (This (This'First + 4 .. This'Last), Dir_Separator);
            elsif AAA.Strings.Has_Prefix (This, "\\.\") then
               Result :=
                 To_Vector ("\\.\")
                 & Split (This (This'First + 4 .. This'Last), Dir_Separator);
            elsif AAA.Strings.Has_Prefix (This, "\\") then
               Result :=
                 To_Vector ("\\")
                 & Split (This (This'First + 2 .. This'Last), Dir_Separator);
            else
               Result := Split (This, Dir_Separator);

               --  Adjust for drive letter
               if Is_Absolute (This) then
                  Result.Replace_Element
                    (Result.First_Index,
                     Result.First_Element & Dir_Separator);
               end if;
            end if;
         else
            --  Sane UNIX paths
            if Is_Absolute (This) then
               Result :=
                 To_Vector ("" & Dir_Separator)
                 & Split (This (This'First + 1 .. This'Last), Dir_Separator);
            else
               Result := Split (This, Dir_Separator);
            end if;
         end if;
      end return;
   end Parts;

   -------------
   -- Resolve --
   -------------

   function Resolve (This : Path) return Path is
   begin
      if Is_Softlink (This) then
         declare
            Link_Target : constant Path := Scrub (Target (This));
         begin
            if Is_Absolute (Link_Target) then
               return Link_Target;
            elsif Has_Parent (This) then
               return Parent (This) / Link_Target;
            else
               return Link_Target;
            end if;
         end;
      else
         return This;
      end if;
   end Resolve;

   ----------
   -- Root --
   ----------

   function Root (This : Absolute_Path) return Root_Path is
   begin
      for I in This'Range loop
         if This (I) = Dir_Separator then
            return This (This'First .. I);
         end if;
      end loop;

      raise Program_Error with "Cannot find root in abs path: " & This;
   end Root;

   ------------
   -- Normal --
   ------------

   function Normal (This : Path) return Normal_Path is
      Parted : Path_Parts := Parts (This);
      I : Integer := Parted.First_Index;
   begin
      while I <= Parted.Last_Index loop
         if Eat_Dots (This, Parted, I) then
            null; -- Eat_Dots does what needs to be done
         else
            I := I + 1;
         end if;
      end loop;

      return Parted.To_Path;
   end Normal;

   -------------------
   -- Canonical_Raw --
   -------------------
   --  Use OS-specific ways of obtaining a canonical path, resolving softlinks
   --  if possible. In case of error (broken, recursive links?), it will return
   --  "".
   function OS_Canonical (This : Path) return String is
      function C_Canonical (Target, Buffer : C_Strings.Chars_Ptr;
                            Bufsiz         : C.size_t)
                            return C.int
        with Import, Convention => C;

      use type C_Strings.C.int;
      Bufsize : Integer := 32768; -- Theoretically, this is Windows max
   begin
      loop
         declare
            Cbuf : C_Strings.C_String := C_Strings.Buffer (Bufsize);
         begin
            case C_Canonical (C_Strings.To_C (This).To_Ptr,
                              Cbuf.To_Ptr,
                              Cbuf.C_Size)
            is
               when 0 =>
                  return Cbuf.To_Ada;
               when -1 =>
                  if Integer'Last / 2 < Bufsize then
                     return "";
                  else
                     Bufsize := Bufsize * 2;
                  end if;
                  --  And try again
               when 1 .. C.int'Last =>
                  return "";
               when others =>
                  raise Program_Error with "should be unreachable";
            end case;
         end;
      end loop;
   end OS_Canonical;

   ------------------
   -- Is_Recursive --
   ------------------

   function Is_Recursive (This : Path) return Boolean
   is (if not Is_Softlink (This)
       then False
       else Exists (Resolve (This)) and then
         (OS_Canonical (This) = "" or else Is_Softlink (OS_Canonical (This))));

   -------------------
   -- Target_Length --
   -------------------

   function Target_Length (This : Path) return Positive is
      function C_Link_Len (Link : C_Strings.Chars_Ptr)
                           return C_Strings.C.size_t
        with Import, Convention => C;
   begin
      if not Is_Softlink (This) then
         raise Constraint_Error with "Not a softlink: " & This;
      end if;
      return Positive (C_Link_Len (C_Strings.To_C (This).To_Ptr));
   end Target_Length;

   ------------
   -- Target --
   ------------

   function Target (This : Path) return String
   is
      use C_Strings;
      use type C.int;

      function C_Link_Target (This, Buffer : Chars_Ptr;
                              Buffer_Length : C.size_t)
                              return C.int
        with Import, Convention => C;
   begin
      if Is_Softlink (This) then
         loop
            --  There's a race condition in which a changing target of
            --  increased length cannot be retrieved; we retry until satisfied.
            declare
               Cbuf : C_String := C_Strings.Buffer (Target_Length (This) + 1);
               Code : constant C.int
                 := C_Link_Target (To_C (This).To_Ptr,
                                   Cbuf.To_Ptr,
                                   Cbuf.C_Size);
            begin
               case Code is
                  when 0 =>
                     return Cbuf.To_Ada;
                  when -1 =>
                     null; -- Retry with new buffer size
                  when others =>
                     raise Program_Error
                       with "cannot retrieve link target, error: "
                            & Code'Image;
               end case;
            end;
         end loop;
      else
         return This;
      end if;
   end Target;

   -------------
   -- Explain --
   -------------

   function Explain (This : Path) return String
   is (case Kind (This) is
          when Special   => " (special)",
          when Softlink  =>
             " --> " & Target (This) &
               (if Is_Broken (This)    then " (broken)"    else "") &
               (if Is_Recursive (This) then " (recursive)" else ""),
          when Nothing   => " (not found)",
          when Directory => "" & Dir_Separator,
          when File      => "");

   -----------
   -- Scrub --
   -----------

   function Scrub (This : String) return Path is
      subtype LC_Drive is Character range 'a' .. 'z';
      subtype UC_Drive is Character range 'A' .. 'Z';

      Bad_Sep : constant Character :=
                  (case Dir_Separator is
                      when '/' => '\',
                      when '\' => '/',
                      when others =>
                        raise Program_Error with "Unsupported platform");
   begin
      --  Empty string
      if This = "" then
         raise Bad_Path with "Bad path: (empty)";
      end if;

      --  Mixed separators
      if (for some Char of This => Char = Bad_Sep) then
         return AAA.Strings.Replace (This, "" & Bad_Sep, "" & Dir_Separator);
      end if;

      --  Duplicated separators
      for I in This'Range loop
         if I < This'Last then
            if This (I) = Dir_Separator
              and then This (I + 1) = Dir_Separator
              and then
                (I /= This'First or else Dir_Separator = '/')
                --  On Windows, an initial '\\' is UNC marker that must be kept
            then
               return
                 Scrub (This (This'First .. I) & This (I + 2 .. This'Last));
            end if;
         end if;
      end loop;

      --  Consistent drive letter
      if This in Root_Path then
         if This (This'First) in LC_Drive then
            return Scrub (UC_Drive'Val (LC_Drive'Pos (This (This'First)))
                          & This (This'First + 1 .. This'Last));
         else
            return This;
         end if;
      end if;

      --  Closing separator for non-root paths
      if This (This'Last) = Dir_Separator then
         return Scrub (This (This'First .. This'Last - 1));
      end if;

      if This in Path then
         return This;
      elsif This = "" then
         raise Bad_Path with "Bad path: (empty)";
      else
         raise Bad_Path with "Bad path: " & This;
      end if;
   end Scrub;

   -------------
   -- To_Path --
   -------------

   function To_Path (This : Path_Parts) return String
   is (if This.Is_Empty
       then ""
       elsif Is_Root (This.First_Element) -- Already includes dir separator
       then This.First_Element
            & This.Slice (This.First_Index + 1, This.Last_Index)
                  .Flatten (Dir_Separator)
       else This.Flatten (Dir_Separator));

end Den;
