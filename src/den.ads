with AAA.Strings;

with Ada.Containers.Indefinite_Ordered_Multisets;

with GNAT.OS_Lib;

package Den is

   Bad_Path : exception;
   --  Raised when attempting to obtain a path from a plain string in some
   --  conversions.

   Unresolvable_Softlink : exception;
   --  Raised by functions that require a valid target at some point

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

   Driveless_Root : constant Absolute_Path;
   --  The root of the filesystem in Unix-like (/), the current drive root (\)
   --  on Windows without the actual drive (\).

   subtype Relative_Path is Path
     with Dynamic_Predicate =>
       not GNAT.OS_Lib.Is_Absolute_Path (Relative_Path);

   subtype Normal_Path is Path with
     Dynamic_Predicate => Is_Normal (Normal_Path);

   subtype Hard_Path is Path with Dynamic_Predicate => Is_Hard (Hard_Path);

   subtype Canonical_Path is Normal_Path with Dynamic_Predicate =>
     Is_Absolute (Canonical_Path)
     and then Canonical_Path in Hard_Path;
   --  The unique path if there are no hard links involved.

   subtype Root_Path is Path with Dynamic_Predicate =>
     Root_Path (Root_Path'Last) = Dir_Separator;

   subtype Sorted_Paths is AAA.Strings.Set
     with Dynamic_Predicate => Contains_Paths (Sorted_Paths);
   --  A bunch of paths, sorted alphabetically

   function Contains_Paths (This : AAA.Strings.Set) return Boolean
   is (for all P of This => P in Path);

   function Scrub (This : String) return Path;
   --  Fix obvious problems like trailing '/' or duplicated "//" parts. May
   --  raise Bad_Path if no good path remains after scrubbing.

   subtype Path_Parts is AAA.Strings.Vector
     with Dynamic_Predicate => Contains_Parts (Path_Parts);
   --  All parts in a path (everything in between dir separators)

   subtype Part is String with
     Dynamic_Predicate =>
       (for all Char of Part => Char /= Dir_Separator)
        or else Is_Root (Part);

   function Contains_Parts (This : AAA.Strings.Vector) return Boolean
   is (for all P of This => P in Part);

   subtype Relative_Parts is Part with Dynamic_Predicate =>
     Relative_Parts = Parent_Dir or else Relative_Parts = Current_Dir;

   function Absolute (This : Path) return Absolute_Path with
     Post => (if Is_Absolute (This) then Absolute'Result = This);

   function Ancestors (This : Normal_Path) return Sorted_Paths;
   --  Returns all ancestors of this path, without canonicalizing it first.
   --  E.g.: /a/b/c => { /, /a, /a/b }, a/b/c => { a, a/b }, z => {}

   function Parts (This : Path) return Path_Parts
     with Post =>
       not Parts'Result.Is_Empty
       and then Is_Root (Parts'Result.First_Element) = Is_Absolute (This);

   function Is_Normal (This : Path) return Boolean
   is (for all P of Parts (This) => P not in Relative_Parts);

   function Normal (This : Path) return Normal_Path;
   --  Remove ".", ".." from path, but without first making it absolute, so it
   --  may raise even for valid relative paths. Use Normal (Absolute (This)) in
   --  such cases.

   type Kinds is
     (Nothing,   -- A path pointing nowhere valid
      Directory,
      File,
      Softlink,
      Special);

   subtype Existing_Kinds is Kinds range Kinds'Succ (Nothing) .. Kinds'Last;

   subtype Final_Kinds is Kinds
     with Static_Predicate => Final_Kinds /= Softlink;

   subtype Childless_Kinds is Kinds range File .. Special;

   function Kind (This : Path; Resolve_Links : Boolean := False) return Kinds;
   --  May return Softlink for a self-referential link!

   function Target_Kind (This : Path) return Final_Kinds
   is (Kind (This, Resolve_Links => True));

   function Is_Hard (This : Path) return Boolean
   is ((for all A of Ancestors (This) => Kind (A) /= Softlink)
       and then Kind (This) /= Softlink);

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

   function Root (This : Absolute_Path) return Root_Path;

   function Resolve (This : Path) return Path;
   --  Identity for non-links, else change This for its target without further
   --  processing. Note that the result might be a new soft link. Intermediate
   --  softlinks are not resolved either. Use with care, prefer Canonical.
   --  Note also that Scrub will be performed on Target (This), so use Target
   --  for the raw contents of a softlink. May raise if the result is not a
   --  well-formed Path even after scrubbing.

   function Is_Broken (This : Path) return Boolean
   is (Kind (This) = Softlink and then not Exists (Resolve (This)));
   --  Note that this is false for a path that points to nothing

   function Is_Recursive (This : Path) return Boolean
     with Post => Kind (This) = Softlink or else Is_Recursive'Result = False;
   --  Can only be True if this designates a softlink

   function Is_Resolvable (This : Path) return Boolean
   is (Kind (This) /= Nothing and then
      (Kind (This) /= Softlink or else
         not (Is_Broken (This) or else Is_Recursive (This))));

   function Canonical (This : Path) return Canonical_Path;
   --  May raise Unresolvable_Softlink, as the resulting path must not contain
   --  soft links. Note though, that for Kind (This) = Nothing, this can
   --  be made canonical but will point to a non-existent item. Check out
   --  Semicanonical.

   function Semicanonical (This : Path) return String;
   --  For broken links, the path will be canonical up to that point, with the
   --  link target appended. For recursive links, the path will be canonical
   --  and the simple name will remain the same. Should never raise.

   function Full (This : Path) return Canonical_Path renames Canonical;

   function Name (This : Path) return Part
     with Post => (if Is_Root (This) then Name'Result = This);
   --  Just the last component in the path; the name of a root is itself.

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

   function Target_Length (This : Path) return Positive
     with Pre => Kind (This) = Softlink;
   --  The length of a softlink target name. Not generally useful to clients
   --  but who knows...

   function Target (This : Path) return String
     with Post =>
       (if Kind (This) = Softlink
          then Target'Result /= ""
        else
          Target'Result = This);
   --  The target of a softlink, even if broken, or the original path given.
   --  This returns the information proper stored in the softlink, not the
   --  original path with the target replacing the softlink, use Resolve for
   --  that. The returned result is not normalized or resolved, use Full_Path
   --  for that. Note that the result may be not a proper path, e.g. something
   --  like "mal//formed". Use Scrub to clean such things.

   type Ls_Options is record
      Canonicalize : Boolean := False;
   end record;

   function Ls (This    : Path;
                Options : Ls_Options := (others => <>))
                return Sorted_Paths
     with Post =>
       (case Kind (This) is
          when Nothing                   => Ls'Result.Is_Empty,
          when Softlink | File | Special => Ls'Result.Length in 1,
          when others                    => True);
   --  Return immediate children of a directory. Won't include "." or ".."

   function Dir (This    : Path;
                 Options : Ls_Options := (others => <>))
                 return Sorted_Paths
                 renames Ls;

   type Filters is interface;

   function Match (This : Filters; Item : Path) return Boolean is abstract;
   --  Paths matched will be visited

   type No_Filter is new Filters with null record;

   overriding function Match (Unused_This : No_Filter;
                              Unused_Item : Path)
                              return Boolean is (True) with Inline;

   type Kind_Is_Filter (Kind : Kinds) is new Filters with null record;

   overriding function Match (This : Kind_Is_Filter;
                              Item : Path)
                              return Boolean is (Kind (Item) = This.Kind)
     with Inline;

   function Kind_Is (Kind : Kinds) return Kind_Is_Filter
   is (Kind_Is_Filter'(Kind => Kind));

   subtype Depths is Natural;

   type Item (Length : Natural) is record
      Path  : Den.Path (1 .. Length);
      Depth : Depths;
      --  0 depth is for the top-level file only, <>/file_0_depth
      --  1 depth is for files inside top-level dir, <>/dir/files_1_depth
   end record;

   function "<" (L, R : Item) return Boolean is (L.Path < R.Path);

   type Canonical_Parts is
     (None,    -- Do not canonicalize anything
      Base,    -- Canonicalize the base directory of a path (all but the name)
      All_With_Dupes, -- Canonicalize the full path, don't dedupe
      All_Deduped     -- Canonicalize the full path, dedupe targets
     );

   type Find_Options is record
      Enter_Regular_Dirs    : Boolean := True;
      Enter_Softlinked_Dirs : Boolean := False;
      --  Beware that loops may occur and the user should break them
      Visit_Softlinks       : Boolean := True;
      --  Whether Action will be called on softlinks
      Canonicalize          : Canonical_Parts := None;
      --  Note that Complete_Deduped requires storing all found canonical paths
      --  to avoid revisiting them through several converging softlinks, so it
      --  may take O(n) memory on the number of paths.
   end record;

   procedure Find
     (This    : Path;
      Action  : access procedure (This  : Item;
                                  Enter : in out Boolean;
                                  Stop  : in out Boolean);
      --  Use action to prevent entering a particular dir, or to stop early.
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record));
   --  Will visit all children of This, or only This if not a directory, if it
   --  exists. If given a Filter, Action will be only called for those matching
   --  it. The order of visiting is alphabetical. "." and ".." are never
   --  visited. Complexity is O(n log n) due to the sorting of entries in
   --  a directory.

   package Item_Sets is new Ada.Containers.Indefinite_Ordered_Multisets (Item);
   --  Multiset as to preserve the semantics of All_With_Dupes

   subtype Items is Item_Sets.Set;

   function Find
     (This    : Path;
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
      return Items;
   --  As the procedure version, but returns the paths that would be visited.
   --  May take a long time without feedback, and softlink dir loops will cause
   --  infinite recursion.

   function Current return Path;
   function CWD return Path renames Current;

   package Operators is

      function "/" (L : Path; R : Relative_Path) return Path;

   end Operators;

private

   Dir_Separator : constant Character := GNAT.OS_Lib.Directory_Separator;

   Driveless_Root : constant Absolute_Path := "" & Dir_Separator;

   function Is_Softlink (This : Path) return Boolean;
   --  Always false in platforms without softlink support. True even for broken
   --  links. False if This doesn't designate anything in the filesystem.

end Den;
