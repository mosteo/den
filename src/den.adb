with Ada.Directories;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

package body Den is

   package Dirs renames Ada.Directories;
   package Ops renames GNAT.Directory_Operations;
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

         declare
            Dir  : Ops.Dir_Type;
            Name : String (1 .. Max_Length);
         begin
            --  Use a dynamically growing limit with retry for max length here.
         end;
      end return;
   end Ls;

   function Dir (This : Path) return Paths renames Ls;

   type Filters is interface;

   function Match (This : Filters; Item : Path) return Boolean is abstract;

   type No_Filter is new Filters with null record;

   overriding function Match (This : No_Filter;
                              Item : Path)
                              return Boolean is (True) with Inline;

   subtype Depths is Natural;

   type Item (Length : Natural) is record
      Path  : Absolute_Path (1 .. Length);
      Depth : Depths;
      --  0 depth is for the top-level file only, <>/file_0_depth
      --  1 depth is for files inside top-level dir, <>/dir/files_1_depth
   end record;

   function "<" (L, R : Item) return Boolean is (L.Path < R.Path);

   type Actions is access procedure (This  : Item;
                                     Enter : in out Boolean;
                                     Stop  : in out Boolean);

   type Find_Options is record
      Enter_Regular_Dirs    : Boolean := True;
      Enter_Softlinked_Dirs : Boolean := False;
      Visit_Softlinks       : Boolean := True;
   end record;

   procedure Find
     (This    : Path;
      Action  : Actions;
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record));
   --  Will visit all children of This, or only This if not a directory, if it
   --  exists. If given a Filter, Action will be only called for those matching
   --  it. The order of visiting is alphabetical. If Enter_Softlinked_Dirs,
   --  beware that loops can occur. "." and ".." are never visited.
   --  Complexity is O(n log n) due to the sorting of entries in a directory.

   package Item_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Item);

   subtype Items is Item_Sets.Set;

   function Find
     (This    : Path;
      Action  : Actions;
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
      return Items;
   --  As the procedure version, but returns the path that would be visited.
   --  May take a long time without feedback...

end Den;
