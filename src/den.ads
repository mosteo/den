with AAA.Strings;

with Ada.Containers.Indefinite_Ordered_Sets;

with GNAT.OS_Lib;

package Den is

   Recursive_Softlink : exception;
   --  Raised for softlinks that point to themselves, directly or through
   --  several hops, and that hence cannot be resolved as a canonical path.

   Dir_Separator : constant Character;

   Parent_Dir    : constant String := "..";
   Current_Dir   : constant String := ".";

   --  A raw, system-encoded path denoting a file or folder, but well-formed
   subtype Path is String
     with Dynamic_Predicate => Path /= ""
     --  not empty
     and then (Path (Path'Last) /= Dir_Separator or else String_Is_Root (Path))
     --  not ending in '/' unless a filesystem root
     and then (for all I in Path'Range =>
                 (if I < Path'Last then
                      Path (I) /= Dir_Separator
                      or else Path (I + 1) /= Dir_Separator));
   --  not containing consecutive separators

   subtype Absolute_Path is Path
     with Dynamic_Predicate =>
       GNAT.OS_Lib.Is_Absolute_Path (Absolute_Path);

   subtype Relative_Path is Path
     with Dynamic_Predicate =>
       not GNAT.OS_Lib.Is_Absolute_Path (Relative_Path);

   subtype Normal_Path is Path with Dynamic_Predicate =>
     (for all P of Split (Normal_Path) => P not in Relative_Parts);

   subtype Hard_Path is Path with Dynamic_Predicate =>
     (for all A of Ancestors (Hard_Path) => Kind (A) /= Softlink);

   subtype Canonical_Path is Path with Dynamic_Predicate =>
     Is_Absolute (Canonical_Path)
     and then Canonical_Path in Hard_Path
     and then Canonical_Path in Normal_Path;
   --  The unique path if there are no hard links involved.

   type Paths is new AAA.Strings.Set with null record
     with Dynamic_Predicate => (for all P of Paths => P in Path);
   --  A bunch of paths, sorted alphabetically

   type Parts is new AAA.Strings.Vector with null record;
   --  All parts in a path (everything in between dir separators)

   subtype Part is String with
     Dynamic_Predicate => (for all Char of Part => Char /= Dir_Separator);

   subtype Relative_Parts is String with Static_Predicate =>
     Relative_Parts = Parent_Dir or else Relative_Parts = Current_Dir;

   function Normalize (This : Path) return Path
     with Post => Normalize'Result in Normal_Path;

   function Ancestors (This : Path) return Paths;
   --  Returns all ancestors of this path, without canonicalizing it first.
   --  E.g.; /a/b/c/d => { /a, /a/b, /a/b/c }, b/c => { b }, z => {}

   function Split (This : Path) return Parts
     with Post =>
       not Split'Result.Is_Empty
       and then Is_Root (Split'Result.First_Element) = Is_Absolute (This);

   type Kinds is
     (Nothing,   -- A path pointing nowhere valid
      Directory,
      File,
      Softlink,
      Special);

   subtype Existing_Kinds is Kinds range Kinds'Succ (Nothing) .. Kinds'Last;

   subtype Final_Kinds is Kinds
     with Static_Predicate => Final_Kinds /= Softlink;

   function Kind (This : Path; Resolve_Links : Boolean := False) return Kinds
     with Post => (if Resolve_Links then Kind'Result in Final_Kinds);

   function Target_Kind (This : Path) return Final_Kinds
   is (Kind (This, Resolve_Links => True));

   function Exists (This : Path; Resolve_Links : Boolean := False)
                    return Boolean
   is (Kind (This, Resolve_Links) in Existing_Kinds);

   function Target_Exists (This : Path) return Boolean
   is (Exists (This, Resolve_Links => True));

   function Is_Absolute (This : Path) return Boolean
                         renames GNAT.OS_Lib.Is_Absolute_Path;

   function String_Is_Root (This : String) return Boolean;
   --  Used to break recursion among predicates

   function Is_Root (This : Path) return Boolean;
   --  True if This denotes explicitly a root name ("/", "C:\")

   function Is_Broken (This : Path) return Boolean
   is (Kind (This) = Softlink and then not Target_Exists (This));
   --  Note that this is false for a path that points to nothing

   function Is_Recursive (This : Path) return Boolean
     with Post => Kind (This) = Softlink or else Is_Recursive'Result = False;
   --  Can only be True if this designates a softlink

   function Canonical (This : Path) return Canonical_Path;
   --  May raise Recursive_Softlink

   function Full (This : Path) return Canonical_Path renames Canonical;

   function Name (This : Path) return Part;
   --  Just the last component in the path

   function Has_Parent (This : Path) return Boolean
   is (not Is_Root (This)
       and then (for some Char of This => Char = Dir_Separator));
   --  Say if This, as-is, has a parent. It may have one logically if it's a
   --  simple name and the current directory is not the root, but not if given
   --  as a simple name.

   function Parent (This : Path) return Path
     with Pre => not Is_Root (This) and then Has_Parent (This);
   --  Will not try to obtain absolute paths, nor canonalize nor do any
   --  other processing. See Canonical_Parent for that.

   function Canonical_Parent (This : Path) return Canonical_Path
   is (Parent (Canonical (This)))
     with Pre => not Is_Root (This);

   function Resolve (This : Path) return Path;
   --  Identity for non-links, else change This for its target without further
   --  processing. Note that the result might be a new soft link. Intermediate
   --  softlinks are not resolved either. Use with care, prefer Canonical.

   function Target_Length (This : Path) return Positive
     with Pre => Kind (This) = Softlink;
   --  The length of a softlink target name. Not generally useful to clients
   --  but who knows...

   function Target (This : Path) return Path
     with Post =>
       (if Kind (This) = Softlink
          then Target'Result /= ""
        else
          Target'Result = This);
   --  The target of a softlink, even if broken, or the original path given.
   --  This returns the information proper stored in the softlink, not the
   --  original path with the target replacing the softlink, use Resolve for
   --  that. The returned result is not normalized or resolved, use Full_Path
   --  for that.

   VOY POR AQUÍ

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

   Dir_Separator : constant Character := GNAT.OS_Lib.Directory_Separator;

   function Is_Softlink (This : Path) return Boolean;
   --  Always false in platforms without softlink support. True even for broken
   --  links. False if This doesn't designate anything in the filesystem.

end Den;
