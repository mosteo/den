with AAA.Strings;

with Ada.Containers.Indefinite_Ordered_Sets;

with GNAT.OS_Lib;

package Den is

   Dir_Separator : constant Character;

   subtype Path is String
     with Static_Predicate => Path /= "";
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

   function Is_Absolute (This : Path) return Boolean
                         renames GNAT.OS_Lib.Is_Absolute_Path;

   function Is_Directory (This : Path) return Boolean
     with Post => (if not Exists (This) then not Is_Directory'Result);
   --  True for softlinks pointing to a directory

   function Is_File (This : Path) return Boolean
     with Post => (if not Exists (This) then not Is_File'Result);

   function Is_Root (This : Path) return Boolean;
   --  True if This denotes explicitly a root name ("/", "C:\")

   function Is_Special (This : Path) return Boolean
     with Post => (if not Exists (This) then not Is_Special'Result);

   function Is_Softlink (This : Path) return Boolean;
   --  Always false in platforms without softlink support. True even for broken
   --  links.

   function Is_Broken (This : Path) return Boolean
   is (Is_Softlink (This) and then not Exists (This));
   --  Note that this is false for a path that points to nothing

   function Full_Path (This          : Path;
                       Resolve_Links : Boolean := True)
                       return Absolute_Path;
   --  This will take care in case of broken

   function Name (This : Path) return Path
     with Post => (for all Char of Name'Result => Char /= Dir_Separator);
   --  Just the last component in the path

   function Has_Parent (This : Path) return Boolean
   is (not Is_Root (This)
       and then (for some Char of This => Char = Dir_Separator));
   --  Say if This, as-is, has a parent. It may have one logically if it's a
   --  simple name and the current directory is not the root, but not if given
   --  as a simple name.

   function Parent (This : Path) return Path
     with Pre => not Is_Root (This) and then Has_Parent (This);
   --  Will not try to obtain absolute paths

   function Resolve (This : Path) return Path;
   --  Identity for non-links, else change This for its target without
   --  expanding the path. Note that the result might be a new soft link.
   --  To obtain the canonical absolute path, use Full_Path.

   function Target_Length (This : Path) return Positive
     with Pre => Is_Softlink (This);
   --  The length of a softlink target name. Not generally useful to clients
   --  but who knows...

   function Target (This : Path) return Path
     with Post =>
       (if Is_Softlink (This)
          then Target'Result /= ""
        else
          Target'Result = This);
   --  The target of a softlink, even if broken, or the original path given.
   --  This returns the information proper stored in the softlink, not the
   --  original path with the target replacing the softlink, use Resolve for
   --  that. The returned result is not normalized or resolved, use Full_Path
   --  for that.

   type Ls_Options is record
      Normalize_Paths       : Boolean := False;
      Resolve_Links         : Boolean := False;
   end record
     with Dynamic_Predicate =>
       (if Ls_Options.Resolve_Links then Ls_Options.Normalize_Paths);

   function Ls (This    : Path;
                Options : Ls_Options := (others => <>))
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

   function Dir (This    : Path;
                 Options : Ls_Options := (others => <>))
                 return Paths
                 renames Ls;

   type Filters is interface;

   function Match (This : Filters; Item : Path) return Boolean is abstract;
   --  Paths matched will be visited

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
      Resolve_Links         : Boolean := False;
   end record
     with Dynamic_Predicate =>
       (if Find_Options.Resolve_Links then Find_Options.Normalize_Paths);

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

   package Operators is

      function "/" (L, R : Path) return Path
        with Pre => R not in Absolute_Path;

   end Operators;

private

   dir_separator : constant Character := GNAT.OS_Lib.Directory_Separator;

end Den;
