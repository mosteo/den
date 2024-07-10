with Ada.Containers.Indefinite_Ordered_Multisets;

package Den.Walk is

   --  Walk directories with these subprograms

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

end Den.Walk;
