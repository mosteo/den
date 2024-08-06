with Ada.Containers.Indefinite_Ordered_Multisets;
with Ada.Directories;

generic
   --  Since ordering is stored in a package-global function pointer, by making
   --  the library generic we avoid any concurrency troubles
package Den.Du is

   subtype Sizes is Ada.Directories.File_Size;

   type Base_Item is abstract tagged null record;

   function "<" (L, R : Base_Item'Class) return Boolean;

   package Item_Sets is
     new Ada.Containers.Indefinite_Ordered_Multisets (Base_Item'Class);
   --  In practice, all elements are Items (see below)

   subtype Tree is Item_Sets.Set;

   type Item (Path_Length : Natural) is new Base_Item with record
      Kind          : Kinds;
      Path          : Den.Path (1 .. Path_Length); -- Full absolute path
      Size          : Sizes;
      --  For directories and special files this is impl defined
      Children_Size : Sizes; -- Cummulative size of children
      Children      : Tree;
   end record;

   function Element (Base : Base_Item'Class) return Item is (Item (Base))
     with Inline;

   function Tree_Size (Base : Base_Item'Class) return Sizes;
   --  The full size of all children plus the item itself

   type Sorting is access function (This, Than : Item) return Boolean;

   function Is_Larger (This, Than : Item) return Boolean;

   function List (Path     : Den.Path;
                  Sort     : Sorting := Is_Larger'Access;
                  Progress : access procedure (Exploring : String) := null)
                  return Tree;
   --  Default size is by decreasing size, alphabetical if same size

   procedure Print (This : Tree);
   --  Basic dump for debugging purposes mostly

private

   use type Sizes;

   ---------------
   -- Tree_Size --
   ---------------

   function Tree_Size (Base : Base_Item'Class) return Sizes
   is (Base.Element.Size + Base.Element.Children_Size);

end Den.Du;
