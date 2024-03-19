with AAA.Strings;

package Den is

   subtype Path is String;
   --  A raw, system-encoded path denoting a file or folder

   subtype Paths is AAA.Strings.Set;
   --  A bunch of, sorted alphabetically

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

   function Is_Softlink (This : Path) return Boolean
     with Post => (if not Exists (This) then not Is_Softlink'Result);
   --  Always false in platforms without softlink support

   function Is_Broken (This : Path) return Boolean
   is (Is_Softlink (This) and then not Exists (This));
   --  Note that this is false for a path that points to nothing

   function Target (This : Path) return Path
     with Post =>
       (if Exists (This)
          then Target'Result /= ""
        elsif Is_Softlink (This)
          then Target'Result /= ""
        else
          Target'Result /= "");
   --  The canonical path for a softlink, even if broken, or the original path
   --  otherwise, if it exists, or "" if not.

   function Ls (This : Path) return Paths
     with Post =>
       (case Exists (This) is
          when False => Ls'Result.Is_Empty,
          when True  =>
              (if not Is_Directory (This)
               then Ls'Result.Length in 1
               else True));
   --  Return immediate children of a directory, unless This is not one and
   --  then the result is itself, if it exists. Won't include "." or "..".

   type Filters is interface;

   function Match (This : Filters; Item : Path) return Boolean is abstract;

   type No_Filter is new Filters with null record;

   overriding function Match (This : No_Filter;
                              Item : Path)
                              return Boolean is (True) with Inline;

   type Actions is access procedure (This  : Path;
                                     Enter : in out Boolean;
                                     Stop  : in out Boolean);

   procedure Find
     (This                  : Path;
      Action                : Actions;
      Enter_Regular_Dirs    : Boolean := True;
      Enter_Softlinked_Dirs : Boolean := False;
      Visit_Softlinks       : Boolean := True;
      Filter                : Filters'Class := No_Filter'(null record));
   --  Will visit all children of This, or only This if not a directory, if it
   --  exists. If given a Filter, Action will be only called for those matching
   --  it. The order of visiting is alphabetical. If Enter_Softlinked_Dirs,
   --  beware that loops can occur. "." and ".." are never visited.

   function Find
     (This                  : Path;
      Action                : Actions;
      Enter_Regular_Dirs    : Boolean := True;
      Enter_Softlinked_Dirs : Boolean := False;
      Visit_Softlinks       : Boolean := True;
      Filter                : Filters'Class := No_Filter'(null record))
      return Paths;
   --  As the procedure version, but returns the path that would be visited.
   --  May take a long time without feedback...

end Den;
