private with Ada.Finalization;

package Den.Iterators is

   --  RAII type over GNAT.Dir_Operations. It iterates over all directory
   --  entries, including broken links and links to directories, so be wary to
   --  skip as needed. "." and ".." are not iterated over. Order of iteration
   --  is undefined. There is no limit on max name lenght.

   type Dir_Iterator (<>) is limited private with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Element);

   function Iterate (This : Path) return Dir_Iterator;

   type Cursor is limited null record;

   function First (This : Dir_Iterator) return Cursor;

   function Next (This : Dir_Iterator; C : Cursor) return Cursor;

   function Has_Element (This : Dir_Iterator; C : Cursor) return Boolean;

   function Element (This : Dir_Iterator; C : Cursor) return Path;

private

   type Dir_Iterator is new Ada.Finalization.Limited_Controlled with record
      I : Integer;
   end record;

end Den.Iterators;
