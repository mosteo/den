with Ada.Directories;

with Den.Iterators;

package body Den is

   package Dirs renames Ada.Directories;
   package OS renames GNAT.OS_Lib;

   use all type Dirs.File_Kind;

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

   ------------
   -- Target --
   ------------

   function Target (This : Path) return Path
   is (if Exists (This) then
          Dirs.Full_Name (This)
       elsif Is_Softlink (This) then -- Must be broken
          raise Program_Error with "unimplemented"
       else This);

   --------
   -- Ls --
   --------

   function Ls (This : Path) return Paths is
   begin
      return Result : Paths do
         if not Exists (This) then
            return;
         end if;

         if Is_Softlink (This) or else not Is_Directory (This) then
            Result.Insert
              (OS.Normalize_Pathname (This, Resolve_Links => False));
            return;
         end if;

         for Item of Iterators.Iterate (This) loop
            Result.Insert (Item);
         end loop;
      end return;
   end Ls;

   ----------
   -- Find --
   ----------

   procedure Find
     (This    : Path;
      Action  : Actions;
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
   is null;

   ----------
   -- Find --
   ----------

   function Find
     (This    : Path;
      Action  : Actions;
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
      return Items
   is (raise Program_Error);

   -------------
   -- Current --
   -------------

   function Current return Path renames Dirs.Current_Directory;

end Den;
