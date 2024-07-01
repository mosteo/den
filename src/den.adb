with Ada.Directories;

with C_Strings;

with Den.Iterators;

--  with GNAT.IO; use GNAT.IO;

package body Den is

   package Dirs renames Ada.Directories;
   package OS renames GNAT.OS_Lib;

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

   ---------------
   -- Ancestors --
   ---------------

   function Ancestors (This : Normal_Path) return Sorted_Paths is
      P : constant Path_Parts := Parts (This);
      use Operators;
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
                (This'Length = 3
                 and then This (This'Last - 1) = ':'
                 and then This (This'Last) = '\'
                 and then This (This'First) in Drive_Letter)
              or else
                (This'Length = 2
                 and then This (This'Last) = ':'
                 and then (This (This'First) in Drive_Letter))));
   end String_Is_Root;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Path) return Boolean
   is (String_Is_Root (This));

   -----------------
   -- Is_Softlink --
   -----------------

   function Is_Softlink (This : Path) return Boolean
   is (OS.Is_Symbolic_Link (This));

   ---------------
   -- Canonical --
   ---------------

   function Canonical (This : Path) return Canonical_Path
   is (if Is_Recursive (This)
       then raise Recursive_Softlink with "Cannot canonicalize: " & This
       else OS.Normalize_Pathname (This, Resolve_Links => True));

   -----------------------
   -- Canonical_Or_Same --
   -----------------------

   function Canonical_Or_Same (This : Path) return Path is
   begin
      return Canonical (This);
   exception
      when Recursive_Softlink =>
         return This;
   end Canonical_Or_Same;

   ----------
   -- Kind --
   ----------

   function Kind (This : Path; Resolve_Links : Boolean := False) return Kinds
   is (if Resolve_Links then
         (if Is_Recursive (This) then
               Softlink
          else
             Kind (Canonical (This), Resolve_Links => False))
       elsif Is_Softlink (This) then
          Softlink
       elsif not Dirs.Exists (This) then
          Nothing
       else
         (case Dirs.Kind (This) is
             when Dirs.Directory => Directory,
             when Dirs.Ordinary_File => File,
             when Dirs.Special_File  => Special));

   ----------
   -- Name --
   ----------

   function Name (This : Path) return Part
   is (Dirs.Simple_Name (This));

   ------------
   -- Parent --
   ------------

   function Parent (This : Path) return Path
   is (Dirs.Containing_Directory (This));

   -----------
   -- Parts --
   -----------

   function Parts (This : Path) return Path_Parts is
   begin
      if Is_Root (This) then
         return AAA.Strings.To_Vector (This);
      end if;

      return Result : Path_Parts := AAA.Strings.Split (This, Dir_Separator) do
         --  Adjust the root if necessary
         if Is_Absolute (This) then
            declare
               Replacement : constant Part :=
                               Result (Result.First_Index) & Dir_Separator;
            begin
               Result.Delete_First;
               Result.Prepend (Replacement);
            end;
         end if;
      end return;
   end Parts;

   -------------
   -- Resolve --
   -------------

   function Resolve (This : Path) return Path is
      use Operators;
   begin
      if Is_Softlink (This) then
         declare
            Link_Target : constant Path := Target (This);
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

   ------------------
   -- Is_Recursive --
   ------------------

   function Is_Recursive (This : Path) return Boolean
   is (if not Is_Softlink (This)
       then False
       else OS.Normalize_Pathname (This, Resolve_Links => True) = "");

   -------------------
   -- Target_Length --
   -------------------

   function Target_Length (This : Path) return Positive is
      function Link_Len (Link : C_Strings.Chars_Ptr) return C_Strings.C.size_t
        with Import, Convention => C;
   begin
      return Positive (Link_Len (C_Strings.To_C (This).To_Ptr));
   end Target_Length;

   ------------
   -- Target --
   ------------

   function Target (This : Path) return String
   is
      use C_Strings;
      use type C.int;

      function Link_Target (This, Buffer : Chars_Ptr;
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
                 := Link_Target (To_C (This).To_Ptr,
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

   --------
   -- Ls --
   --------

   function Ls (This    : Path;
                Options : Ls_Options := (others => <>))
                return Sorted_Paths is

      ------------
      -- Insert --
      ------------

      procedure Insert (This : Path; Into : in out Sorted_Paths) is
      begin
         if Options.Canonicalize then
            Into.Include -- resolved links may point to the same file
              (OS.Normalize_Pathname
                 (This,
                  Resolve_Links => True));
         else
            Into.Insert (This);
         end if;
      end Insert;

   begin
      return Result : Sorted_Paths do
         case Kind (This) is
            when Nothing =>
               return;

            when Childless_Kinds =>
               Insert (This, Into => Result);

            when Directory =>
               for Item of Iterators.Iterate (This) loop
                  Insert (Item, Into => Result);
               end loop;
         end case;
      end return;
   end Ls;

   ----------
   -- Find --
   ----------

   procedure Find
     (This    : Path;
      Action  : access procedure (This  : Item;
                                  Enter : in out Boolean;
                                  Stop  : in out Boolean);
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
   is

      Visited : Sorted_Paths; -- For deduping

      Enter : Boolean := True;
      Stop  : Boolean := False;

      --------------
      -- New_Item --
      --------------

      function New_Item (Here : Path; Depth : Natural) return Item
      is
      begin
         return (Length => Here'Length,
                 Path   => Here,
                 Depth  => Depth);
      end New_Item;

      subtype Dir_Path is String with Dynamic_Predicate =>
        Dir_Path (Dir_Path'Last) = Dir_Separator;

      ----------
      -- Find --
      ----------

      procedure Find (Parent : Dir_Path; Depth : Positive) is
         Base : constant Dir_Path :=
                  (if Options.Canonicalize /= None
                   then Canonical_Or_Same
                     (Parent (Parent'First .. Parent'Last - 1))
                    & OS.Directory_Separator
                   else Parent);
      begin
         for Item of Ls (Parent (Parent'First .. Parent'Last - 1),
                         (Canonicalize => False))
         loop
            if Stop then
               return;
            end if;

            if not Filter.Match (Item) then
               goto Continue;
            end if;

            declare
               Child_Plain : constant Path := Base & Item;
               Child : constant Path :=
                         (case Options.Canonicalize is
                             when None | Den.Base =>
                               Base & Item,
                             when All_With_Dupes | All_Deduped =>
                               Canonical_Or_Same (Base & Item));
            begin
               Enter := True;
               Stop  := False;

               if Options.Canonicalize /= All_Deduped or else
                 not Visited.Contains (Child)
               then
                  Action (This  => New_Item (Child, Depth),
                          Enter => Enter,
                          Stop  => Stop);

                  if Stop then
                     return;
                  end if;

                  if Options.Canonicalize = All_Deduped then
                     Visited.Insert (Child);
                  end if;
               end if;

               if Enter then
                  if (Options.Enter_Regular_Dirs
                      and then Kind (Child_Plain) = Directory)
                    or else
                      (Options.Enter_Softlinked_Dirs
                       and then Target_Kind (Child_Plain) = Directory)
                  then
                     Find (Child_Plain & OS.Directory_Separator, Depth + 1);
                  end if;
               end if;
            end;

            <<Continue>>
         end loop;
      end Find;

   begin
      if Target_Kind (This) /= Directory then
         Action (New_Item (This, 0),
                 Enter => Enter,
                 Stop  => Stop);
      else
         Find (This & OS.Directory_Separator, 1);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (This    : Path;
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
      return Items
   is
   begin
      return Result : Items do
         declare

            ----------
            -- Find --
            ----------

            procedure Find (This         : Item;
                            Unused_Enter : in out Boolean;
                            Unused_Stop  : in out Boolean)
            is
            begin
               Result.Insert (This);
            end Find;

         begin
            Find (This, Find'Access, Options, Filter);
         end;
      end return;
   end Find;

   -------------
   -- Current --
   -------------

   function Current return Path
   is (Dirs.Current_Directory);

   -----------
   -- Scrub --
   -----------

   function Scrub (This : String) return Path is
   begin
      if This = "" then
         raise Bad_Path with "Bad path: (empty)";
      end if;

      if This in Root_Path then
         return This;
      end if;

      if This (This'Last) = Dir_Separator then
         return Scrub (This (This'First .. This'Last - 1));
      end if;

      for I in This'Range loop
         if I < This'Last then
            if This (I) = Dir_Separator and then This (I + 1) = Dir_Separator
            then
               return
                 Scrub (This (This'First .. I) & This (I + 2 .. This'Last));
            end if;
         end if;
      end loop;

      if This in Path then
         return This;
      elsif This = "" then
         raise Bad_Path with "Bad path: (empty)";
      else
         raise Bad_Path with "Bad path: " & This;
      end if;
   end Scrub;

end Den;
