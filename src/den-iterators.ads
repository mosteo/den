private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Den.Iterators is

   --  RAII type over GNAT.Dir_Operations. It iterates over all directory
   --  entries, including broken links and links to directories, so be wary to
   --  skip as needed. "." and ".." are not iterated over. Order of iteration
   --  is undefined. There is no limit on max name length.

   type Dir_Iterator (<>) is limited private with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Element);

   function Iterate (This : Path) return Dir_Iterator;

   type Cursor (<>) is private;

   function First (This : Dir_Iterator) return Cursor;

   function Next (This : Dir_Iterator; C : Cursor) return Cursor
     with Pre => Has_Element (This, C);

   function Has_Element (This : Dir_Iterator; C : Cursor) return Boolean;

   function Element (This : Dir_Iterator; C : Cursor) return Path;

private

   package Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Path);

   type Dir_Iterator is limited record
      Items : Lists.List;
   end record;

   type Cursor is record
      Pos : Lists.Cursor;
   end record;

end Den.Iterators;
