with AAA.Strings;

with Ada.Containers.Indefinite_Ordered_Sets;

with GNAT.OS_Lib;

package Den is

   subtype Path is String;
   --  A raw, system-encoded path denoting a file or folder

   subtype Absolute_Path is Path
     with Dynamic_Predicate =>
       GNAT.OS_Lib.Is_Absolute_Path (Absolute_Path);

   subtype Relative_Path is Path
     with Dynamic_Predicate =>
       not GNAT.OS_Lib.Is_Absolute_Path (Relative_Path);
   --  Just to make intentions clear

   subtype Paths is AAA.Strings.Set;
   --  A bunch of paths, sorted alphabetically

   function Exists (This : Path) return Boolean;
   --  True if This designates some existing filesystem entity; False for
   --  broken links.

   function Is_Directory (This : Path) return Boolean
     with Post => (if not Exists (This) then not Is_Directory'Result);
   --  True for softlinks pointing to a directory

   function Is_File (This : Path) return Boolean
     with Post => (if not Exists (This) then not Is_File'Result);

   function Is_Special (This : Path) return Boolean
     with Post => (if not Exists (This) then not Is_Special'Result);

   function Is_Softlink (This : Path) return Boolean;
   --  Always false in platforms without softlink support. True even for broken
   --  links.

   function Is_Broken (This : Path) return Boolean
   is (Is_Softlink (This) and then not Exists (This));
   --  Note that this is false for a path that points to nothing

   function Full_Path (This : Path) return Absolute_Path;

   function Target (This : Path) return Path
     with Post =>
       (if Exists (This)
          then Target'Result /= "" and then Target'Result in Absolute_Path
        elsif Is_Softlink (This)
          then Target'Result /= ""
        else
          Target'Result = "");
   --  The canonical path for a softlink, even if broken, or the original path
   --  otherwise, if it exists, or "" if not.

   function Ls (This      : Path;
                Normalize : Boolean := False)
                return Paths
     with Post =>
       (case Exists (This) is
          when False => Ls'Result.Is_Empty,
          when True  =>
              (if Is_Softlink (This) or else not Is_Directory (This)
               then Ls'Result.Length in 1
               else True));
   --  Return immediate children of a directory, unless This is not one and
   --  then the result is itself, if it exists. Won't include "." or "..".
   --  Paths are absolute.

   function Dir (This      : Path;
                 Normalize : Boolean := False)
                 return Paths
                 renames Ls;

   type Filters is interface;

   function Match (This : Filters; Item : Path) return Boolean is abstract;

   type No_Filter is new Filters with null record;

   overriding function Match (This : No_Filter;
                              Item : Path)
                              return Boolean is (True) with Inline;

   subtype Depths is Natural;

   type Item (Length : Natural) is record
      Path  : Den.Path (1 .. Length);
      Depth : Depths;
      --  0 depth is for the top-level file only, <>/file_0_depth
      --  1 depth is for files inside top-level dir, <>/dir/files_1_depth
   end record;

   function "<" (L, R : Item) return Boolean is (L.Path < R.Path);

   type Find_Options is record
      Enter_Regular_Dirs    : Boolean := True;
      Enter_Softlinked_Dirs : Boolean := False;
      Visit_Softlinks       : Boolean := True;
      Normalize_Paths       : Boolean := False;
   end record;

   procedure Find
     (This    : Path;
      Action  : access procedure (This  : Item;
                                  Enter : in out Boolean;
                                  Stop  : in out Boolean);
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
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
      return Items;
   --  As the procedure version, but returns the paths that would be visited.
   --  May take a long time without feedback...

   function Current return Path;
   function CWD return Path renames Current;

end Den;
