with AAA.Strings;

with GNAT.OS_Lib;

package Den with Preelaborate is

   Bad_Operation : exception;
   --  Raised when some transformation between paths can be completed

   Bad_Path : exception;
   --  Raised when attempting to obtain a path from a plain string in some
   --  conversions, or for any problematic path in general (e.g., C:\..\) if
   --  it has to be resolved.

   function Dir_Separator return Character with Inline;

   Dots_Parent_Dir  : constant String := "..";
   Dots_Current_Dir : constant String := ".";

   --  A raw, system-encoded path denoting a file or folder, but well-formed
   subtype Path is String
     with Dynamic_Predicate => Path /= ""
     --  not empty
     and then (Path (Path'Last) /= Dir_Separator or else String_Is_Root (Path))
     --  not ending in '/' unless a filesystem root
     and then (for all I in Path'Range =>
                 (if I < Path'Last then
                      Path (I) /= Dir_Separator
                      or else Path (I + 1) /= Dir_Separator
                      or else (Dir_Separator = '\' and then I = Path'First)));
   --  not containing consecutive separators, except on Windows for net paths

   subtype Absolute_Path is Path
     with Dynamic_Predicate =>
       GNAT.OS_Lib.Is_Absolute_Path (Absolute_Path);

   function Driveless_Root return Absolute_Path with Inline;
   --  The root of the filesystem in Unix-like (/), the current drive root (\)
   --  on Windows without the actual drive (\).

   subtype Relative_Path is Path
     with Dynamic_Predicate =>
       not GNAT.OS_Lib.Is_Absolute_Path (Relative_Path);

   subtype Normal_Path is Path with
     Dynamic_Predicate => Is_Normal (Normal_Path);

   subtype Absnormal_Path is Normal_Path with
     Dynamic_Predicate => Is_Absolute (Absnormal_Path);

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
   --  Fix obvious problems like trailing '/', duplicated "//", '\' instead of
   --  '/' (and viceversa). May raise Bad_Path if no good path remains after
   --  scrubbing. Will respect initial '\\' on Windows.

   function Can_Scrub (This : String) return Boolean;
   --  Says if Scrub would not raise Bad_Path;

   type Path_Parts is new AAA.Strings.Vector with null record with
     Dynamic_Predicate => Contains_Parts (AAA.Strings.Vector (Path_Parts));
   --  All parts in a path (everything in between dir separators)

   function To_Path (This : Path_Parts) return String with
     Post => (if This.Is_Empty then To_Path'Result = "");

   subtype Part is String with
     Dynamic_Predicate =>
       (for all Char of Part => Char /= Dir_Separator)
        or else Is_Root (Part);

   function Contains_Parts (This : AAA.Strings.Vector) return Boolean
   is (for all P of This => P in Part);

   subtype Relative_Parts is Part with Dynamic_Predicate =>
     Relative_Parts = Dots_Parent_Dir or else
     Relative_Parts = Dots_Current_Dir;

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
   --  such cases. Too many ".." (going "up" of root) will also raise.

   type Kinds is
     (Nothing,   -- A path pointing nowhere valid
      Directory,
      File,
      Softlink,
      Special);

   subtype Existing_Kinds is Kinds range Kinds'Succ (Nothing) .. Kinds'Last;

   subtype Existing_Path is Path
     with Dynamic_Predicate => Kind (Existing_Path) in Existing_Kinds;

   subtype Final_Kinds is Kinds
     with Static_Predicate => Final_Kinds /= Softlink;

   subtype Childless_Kinds is Kinds range File .. Special;

   subtype Canonical_Kinds is Kinds with
     Dynamic_Predicate => Canonical_Kinds not in Nothing | Softlink;

   function Explain (This : Path) return String;
   --  A string that may be useful while debugging, says the kind of This

   function Kind (This : Path; Resolve_Links : Boolean := False) return Kinds;
   --  May return Softlink for a self-referential link even with Resolve_Links!

   function Target_Kind (This : Path) return Kinds
   is (Kind (This, Resolve_Links => True));
   --  May still return softlink if the softlink is unresolvable

   function Is_Hard (This : Path) return Boolean
   is ((for all A of Ancestors (This) => Kind (A) = Directory)
       and then Kind (This) in Canonical_Kinds);
   --  Note that a non-existing path maybe hard

   function Exists (This : Path; Resolve_Links : Boolean := False)
                    return Boolean
   is (Kind (This, Resolve_Links) in Existing_Kinds);

   function Target_Exists (This : Path) return Boolean
   is (Exists (This, Resolve_Links => True));

   function Is_Absolute (This : Path) return Boolean;

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

   function Canonical (This : Existing_Path) return Canonical_Path;
   --  Returns the absolute hard path to This, which would be unique if
   --  no hard links are involved. May raise Bad_Path for broken/recursive
   --  links, as the resulting path must not contain soft links. Check out
   --  FS.Pseudocanonical for when a real, existing path is not mandatory.

   function Canonizable (This : String) return Boolean;
   --  Says if Canonical (This) will succeed.

   function Name (This : Path) return Part
     with Post => (if Is_Root (This) then Name'Result = This);
   --  Just the last component in the path; the name of a root is itself.

   function Simple_Name (This : Path) return Part renames Name;
   --  For Ada.Directories compatibility

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

   package Operators is

      function "/" (L : Path; R : Relative_Path) return Path;

   end Operators;

private

   Debug : constant Boolean := False;

   -------
   -- P --
   -------

   function P (S : String) return String is (" (" & S & ") ");

   function Dir_Separator return Character
   is (GNAT.OS_Lib.Directory_Separator);

   function Driveless_Root return Absolute_Path is (1 => Dir_Separator);

   function Eat_Dots (This   :        Path;
                      Parted : in out Path_Parts;
                      I      : in out Integer)
                      return Boolean;
   --  Removes only "." and ".." at pos I. Return True if something removed,
   --  False otherwise. May raise Bad_Path

   function Is_Softlink (This : Path) return Boolean;
   --  Always false in platforms without softlink support. True even for broken
   --  links. False if This doesn't designate anything in the filesystem.

   function OS_Canonical (This : Path) return String;
   --  The OS own canonicalization function. Returns a canonical path if
   --  This exists, or "" otherwise. This is not a "clever" function like
   --  GNAT.OS_Lib.Normalize_Pathname or std::filesystem::weak_canonical.
   --  To preserve cross-platform behavior, we do that in Pseudocanonical.

end Den;
