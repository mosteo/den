with Ada.Directories;

with C_Strings;

with Den.Iterators;

--  with GNAT.IO; use GNAT.IO;

package body Den is

   package Dirs renames Ada.Directories;
   package OS renames GNAT.OS_Lib;

   use all type Dirs.File_Kind;

   ---------------
   -- Operators --
   ---------------

   package body Operators is

      ---------
      -- "/" --
      ---------

      function "/" (L, R : Path) return Path
      is (if L (L'Last) = Dir_Separator
          then L & R
          else L & Dir_Separator & R);

   end Operators;

   ------------
   -- Exists --
   ------------

   function Exists (This : Path) return Boolean
   is (Dirs.Exists (This));

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (This : Path) return Boolean
   is (Dirs.Exists (This) and then Dirs.Kind (This) = Directory);

   -------------
   -- Is_File --
   -------------

   function Is_File (This : Path) return Boolean
   is (Dirs.Exists (This) and then Dirs.Kind (This) = Ordinary_File);

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Path) return Boolean is
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
   end Is_Root;

   ----------------
   -- Is_Special --
   ----------------

   function Is_Special (This : Path) return Boolean
   is (Dirs.Exists (This) and then Dirs.Kind (This) = Special_File);

   -----------------
   -- Is_Softlink --
   -----------------

   function Is_Softlink (This : Path) return Boolean
   is (OS.Is_Symbolic_Link (This));

   ---------------
   -- Full_Path --
   ---------------

   function Full_Path (This          : Path;
                       Resolve_Links : Boolean := True)
                       return Absolute_Path
   is
      use Operators;
   begin
      if Is_Softlink (This) and then not Resolve_Links then
         if Has_Parent (This) then
            return
              Parent (OS.Normalize_Pathname (This, Resolve_Links => False))
                / Name (This);
         else
            return Current / This; -- ??? REVIEW
         end if;
      else
         return OS.Normalize_Pathname (This, Resolve_Links => Resolve_Links);
      end if;
   end Full_Path;

   ----------
   -- Name --
   ----------

   function Name (This : Path) return Path
   is (Dirs.Simple_Name (This));

   ------------
   -- Parent --
   ------------

   function Parent (This : Path) return Path
   is (Dirs.Containing_Directory (This));

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

   function Target (This : Path) return Path
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
                return Paths is

      ------------
      -- Insert --
      ------------

      procedure Insert (This : Path; Into : in out Paths) is
      begin
         if Options.Normalize_Paths then
            Into.Include -- resolved links may point to the same file
              (OS.Normalize_Pathname
                 (This,
                  Resolve_Links => Options.Resolve_Links));
         else
            Into.Insert (This);
         end if;
      end Insert;

   begin
      return Result : Paths do
         if not Exists (This) then
            return;
         end if;

         if Is_Softlink (This) or else not Is_Directory (This) then
            Insert (This, Into => Result);
            return;
         end if;

         for Item of Iterators.Iterate (This) loop
            Insert (Item, Into => Result);
         end loop;
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

      ----------
      -- Find --
      ----------

      procedure Find (Parent : Path; Depth : Positive) is
         Base : constant Path :=
                  (if Options.Normalize_Paths
                   then OS.Normalize_Pathname
                     (Parent,
                      Resolve_Links => Options.Resolve_Links)
                    & OS.Directory_Separator
                   else Parent);
      begin
         for Item of Ls (Parent, (Normalize_Paths => False, others => <>)) loop
            if Stop then
               return;
            end if;

            if not Filter.Match (Item) then
               goto Continue;
            end if;

            declare
               Child : constant Path := Base & Item;
            begin
               Action (This  => New_Item (Child, Depth),
                       Enter => Enter,
                       Stop  => Stop);

               if Stop then
                  return;
               end if;

               if Enter and then Is_Directory (Child) then
                  if (Is_Softlink (Child)
                      and then Options.Enter_Softlinked_Dirs)
                    or else
                      (not Is_Softlink (Child)
                       and then Options.Enter_Regular_Dirs)
                  then
                     Find (Child & OS.Directory_Separator, Depth + 1);
                  end if;
               end if;
            end;

            <<Continue>>
         end loop;
      end Find;

   begin
      if not Exists (This) or else Is_Softlink (This) then
         Action (New_Item (This, 0),
                 Enter => Enter,
                 Stop  => Stop);
      elsif Is_Directory (This)
        and then This (This'Last) /= OS.Directory_Separator
      then
         Find (This & OS.Directory_Separator, 1);
      else
         Find (This, 1);
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

end Den;
