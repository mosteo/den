--  with Ada.Directories;

with Ada.Environment_Variables;
with Ada.IO_Exceptions;

with Den.OS;

with GNAT.IO;

package body Den is

   --  package Dirs renames Ada.Directories;
   package IOex renames Ada.IO_Exceptions;

   -----------
   -- Debug --
   -----------
   Cached_Debug       : Boolean;
   Cached_Debug_Valid : Boolean := False;
   function Debug return Boolean is
   begin
      if Cached_Debug_Valid then
         return Cached_Debug;
      else
         Cached_Debug :=
           False
           or else Ada.Environment_Variables.Exists ("DEN_DEBUG");
         Cached_Debug_Valid := True;
         return Cached_Debug;
      end if;
   end Debug;

   ---------
   -- Log --
   ---------

   procedure Log (Message  : String;
                  Location : String := GNAT.Source_Info.Source_Location)
   is
   begin
      if Debug then
         GNAT.IO.Put_Line ("[DEN] " & Location & ": " & Message);
      end if;
   end Log;

   -----------
   -- Error --
   -----------

   function Error (Info     : String;
                   Location : String := GNAT.Source_Info.Source_Location)
                   return String
   is (Location & ": " & Info);

   ---------------
   -- Operators --
   ---------------

   package body Operators is

      ---------
      -- "/" --
      ---------

      function "/" (L : Path; R : Relative_Path) return Path
      is (if Is_Dir_Separator (L (L'Last))
            or else
              (L'Length = 2 and then Prefix_Is_Drive (L, On_Windows))
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

   -------------------
   -- Prefix_Length --
   -------------------

   function Prefix_Length (This : String; Windows : Boolean) return Natural is
      First : constant Integer := This'First;

      function Is_Sep (Pos : Integer) return Boolean
      is (Pos <= This'Last and then This (Pos) in '\' | '/');

      function Has_Drive (Pos : Integer) return Boolean
      is (Pos < This'Last
          and then This (Pos + 1) = ':'
          and then This (Pos) in 'a' .. 'z' | 'A' .. 'Z');

      function Starts_With (Pos : Integer; Text : String) return Boolean
      is (Pos + Text'Length - 1 <= This'Last
          and then AAA.Strings.To_Lower_Case
                     (This (Pos .. Pos + Text'Length - 1)) = Text);

      -----------
      -- After --
      -----------

      --  The position after the name starting at Pos, and its closing separator

      function After (Pos : Integer) return Integer is
         I : Integer := Pos;
      begin
         while I <= This'Last and then not Is_Sep (I) loop
            I := I + 1;
         end loop;

         return (if I <= This'Last then I + 1 else I);
      end After;

   begin
      if not Windows then
         return (if This'Length > 0 and then This (First) = '/' then 1 else 0);
      end if;

      --  \\?\ and \\.\, which only the former can be followed by a root

      if This'Length >= 4
        and then Is_Sep (First) and then Is_Sep (First + 1)
        and then This (First + 2) in '?' | '.'
        and then Is_Sep (First + 3)
      then
         declare
            Rest : constant Integer := First + 4;
         begin
            if This (First + 2) = '.' then
               return 4;
            elsif Has_Drive (Rest) then
               return (if Is_Sep (Rest + 2) then 7 else 6);
            elsif Starts_With (Rest, "unc\") then
               return After (After (Rest + 4)) - First;
            elsif Starts_With (Rest, "volume{") then
               return After (Rest) - First;
            else
               return 4;
            end if;
         end;
      end if;

      --  \\server\share, or just \\ when incomplete

      if Is_Sep (First) and then Is_Sep (First + 1) then
         declare
            Share : constant Integer := After (First + 2);
         begin
            return (if Share <= This'Last then After (Share) - First else 2);
         end;
      end if;

      if Is_Sep (First) then
         return 1;
      elsif Has_Drive (First) then
         return (if Is_Sep (First + 2) then 3 else 2);
      else
         return 0;
      end if;
   end Prefix_Length;

   ---------------------
   -- Prefix_Is_Drive --
   ---------------------

   function Prefix_Is_Drive (This : String; Windows : Boolean) return Boolean
   is (Prefix_Length (This, Windows) = 2 and then This (This'First + 1) = ':');

   ------------------
   -- Absolute_For --
   ------------------

   --  On Windows, the root \ and the drive C: depend on the current state

   function Absolute_For (This : String; Windows : Boolean) return Boolean
   is (if Windows
       then Prefix_Length (This, Windows) > 1
            and then not Prefix_Is_Drive (This, Windows)
       else Prefix_Length (This, Windows) = 1);

   ----------------
   -- Split_Path --
   ----------------

   function Split_Path (This : String; Windows : Boolean)
                        return AAA.Strings.Vector
   is
      Prefix   : constant Natural := Prefix_Length (This, Windows);
      Verbatim : constant Boolean :=
                   Windows and then Prefix >= 4
                   and then This (This'First + 2) = '?';
      Start    : Integer := This'First + Prefix;

      function Is_Sep (Char : Character) return Boolean
      is (if Windows
          then Char = '\' or else (Char = '/' and then not Verbatim)
          else Char = '/');

      ------------
      -- Native --
      ------------

      function Native (Root : String) return String
      is (if Windows
          then AAA.Strings.Replace (Root, "/", "\")
               & (if Absolute_For (Root, Windows)
                    and then Root (Root'Last) not in '\' | '/'
                  then "\"
                  else "")
          else Root);

   begin
      return Result : AAA.Strings.Vector do
         if Prefix > 0 then
            Result.Append (Native (This (This'First .. Start - 1)));
         end if;

         for I in Start .. This'Last + 1 loop
            if I > This'Last or else Is_Sep (This (I)) then
               if I > Start then
                  Result.Append (This (Start .. I - 1));
               end if;
               Start := I + 1;
            end if;
         end loop;
      end return;
   end Split_Path;

   --------------------
   -- String_Is_Root --
   --------------------

   function String_Is_Root (This : String) return Boolean
   is (This'Length > 0
       and then Prefix_Length (This, On_Windows) = This'Length
       and then not Prefix_Is_Drive (This, On_Windows));

   -----------------
   -- Is_Absolute --
   -----------------

   function Is_Absolute (This : Path) return Boolean
   is (Absolute_For (This, On_Windows));

   -----------------
   -- Is_Relative --
   -----------------

   function Is_Relative (This : String) return Boolean
   is (Prefix_Length (This, On_Windows) = 0);

   --------------
   -- Has_Root --
   --------------

   function Has_Root (This : Path) return Boolean
   is (not Is_Relative (This)
       and then not Prefix_Is_Drive (This, On_Windows));

   ----------------------
   -- Is_Dir_Separator --
   ----------------------

   function Is_Dir_Separator (Char : Character) return Boolean
   is (Char = Dir_Separator or else (On_Windows and then Char = '/'));

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Path) return Boolean
   is (String_Is_Root (This));

   -----------------
   -- Is_Softlink --
   -----------------

   function Is_Softlink (This : Path) return Boolean
   is (Kind (This) = Softlink);

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

   function Canonical (This : Existing_Path) return Canonical_Path is
      Result : constant String := OS_Canonical (This);
   begin
      if Result = "" or else Result not in Canonical_Path then
         raise Bad_Path with
           "Cannot canonicalize: " & This & " [os_canonical: " & Result & "]";
      end if;

      return Result;
   end Canonical;

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

   function Eat_Dots (This        :        Path;
                      Parted      : in out Path_Parts;
                      I           : in out Integer;
                      Drop_Excess :        Boolean := False)
                      return Boolean
   is
   begin
      if Parted (I) = "." then
         Parted.Delete (I);
         return True;
      elsif Parted (I) = ".." then
         Parted.Delete (I);

         --  There's nothing to go up from at the start of a relative path, or
         --  after a root or drive

         if I = Parted.First_Index
           or else Prefix_Length (Parted (I - 1), On_Windows) > 0
         then
            if not Drop_Excess then
               raise Bad_Path with
                 "Too many parents in path while normalizing: " & This;
            end if;
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
      Own_Kind : constant Kinds := OS.Kind_Of (This);
   begin
      if Resolve_Links and then Own_Kind = Softlink then
         declare
            Resolved : constant String := OS_Canonical (This);
         begin
            return (if Resolved = "" then Nothing else OS.Kind_Of (Resolved));
         end;
      else
         return Own_Kind;
      end if;
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
       elsif not Has_Parent (This) then
          raise IOex.Use_Error with "Path has no parent: " & This
       else
          Parts (This).Up_To (Integer (Parts (This).Length) - 1).To_Path);

   -----------
   -- Parts --
   -----------

   function Parts (This : Path) return Path_Parts
   is (Split_Path (This, On_Windows) with null record);

   -------------
   -- Resolve --
   -------------

   function Resolve (This : Path) return Path is
   begin
      if Is_Softlink (This) then
         declare
            Link_Target : constant Path := Scrub (Target (This));
         begin
            if Is_Relative (Link_Target) then
               return (if Has_Parent (This)
                       then Parent (This) / Link_Target
                       else Link_Target);
            elsif Is_Absolute (Link_Target) or else not Has_Root (Link_Target)
            then
               return Link_Target;
            else
               --  A rooted target is in the same drive as the link
               return Root (OS.Full_Path (This))
                 & Link_Target (Link_Target'First + 1 .. Link_Target'Last);
            end if;
         end;
      else
         return This;
      end if;
   end Resolve;

   ----------
   -- Root --
   ----------

   function Root (This : Absolute_Path) return Root_Path
   is (Parts (This).First_Element);

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

      if Parted.Is_Empty then
         raise Bad_Path with "Nothing left after normalizing: " & This;
      end if;

      return Parted.To_Path;
   end Normal;

   -------------------
   -- Canonical_Raw --
   -------------------
   --  Use OS-specific ways of obtaining a canonical path, resolving softlinks
   --  if possible. In case of error (broken, recursive links?), it will return
   --  "".
   function OS_Canonical (This : Path) return String is
   begin
      begin
         return OS.Canonical (This);
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Log ("Cannot canonicalize path: " & This);
            return "";
      end;
   end OS_Canonical;

   ------------------
   -- Is_Recursive --
   ------------------

   --  function Is_Recursive (This : Path) return Boolean
   --  is (if not Is_Softlink (This)
   --      then False
   --      else Exists (Resolve (This)) and then
   --    (OS_Canonical (This) = "" or else Is_Softlink (OS_Canonical (This))));

   -------------------
   -- Target_Length --
   -------------------

   function Target_Length (This : Path) return Natural is
   begin
      if not Is_Softlink (This) then
         raise Constraint_Error with "Not a softlink: " & This;
      end if;
      return OS.Link_Target (This)'Length;
   end Target_Length;

   ------------
   -- Target --
   ------------

   function Target (This : Path) return String
   is
   begin
      if Is_Softlink (This) then
         return OS.Link_Target (This);
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
               (if Is_Broken (This)    then " (broken)"    else ""), -- &
               --  (if Is_Recursive (This) then " (recursive)" else ""),
          when Nothing   => " (not found)",
          when Directory => "" & Dir_Separator,
          when File      => "");

   -----------
   -- Scrub --
   -----------

   function Scrub (This : String) return Path is
      subtype LC_Drive is Character range 'a' .. 'z';
   begin
      --  Empty string
      if This = "" then
         raise Bad_Path with "Bad path: (empty)";
      end if;

      --  Consistent drive letter
      if On_Windows
        and then This'Length >= 2
        and then This (This'First + 1) = ':'
        and then This (This'First) in LC_Drive
      then
         return Scrub (Character'Val (Character'Pos (This (This'First))
                                      - Character'Pos ('a')
                                      + Character'Pos ('A'))
                       & This (This'First + 1 .. This'Last));
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

      if This in Root_Path then
         return This;
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
       elsif Integer (This.Length) = 1
       then This.First_Element
       else This.First_Element
            / This.Slice (This.First_Index + 1, This.Last_Index)
                  .Flatten (Dir_Separator));

end Den;
