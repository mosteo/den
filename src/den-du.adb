with Den.Iterators;

with GNAT.IO;

package body Den.Du is

   package Adirs renames Ada.Directories;

   Current_Sorting : Sorting := Is_Larger'Access;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Base_Item'Class) return Boolean
   is (Current_Sorting (Item (L), Item (R)));

   ---------------
   -- Is_Larger --
   ---------------

   function Is_Larger (This, Than : Item) return Boolean
   is (This.Tree_Size > Than.Tree_Size
       or else
         (This.Tree_Size = Than.Tree_Size
          and then This.Path < Than.Path));

   ----------
   -- List --
   ----------

   function List (Path     : Den.Path;
                  Sort     : Sorting := Is_Larger'Access;
                  Progress : access procedure (Exploring : String) := null)
                  return Tree
   is

      -------------------
      -- List_Internal --
      -------------------

      function List_Internal (Path : Den.Path) return Tree is
      begin
         if Progress /= null then
            Progress (Path);
         end if;

         return Result : Tree do
            for Name of Den.Iterators.Iterate (Path) loop
               declare
                  use Den.Operators;
                  Child         : constant Den.Path := Path / Name;
                  Children      : Tree;
                  Children_Size : Sizes := 0;
               begin
                  if Kind (Child) = Directory then
                     Children := List_Internal (Child);
                     for Child of Children loop
                        Children_Size := Children_Size + Child.Tree_Size;
                     end loop;
                  end if;

                  Result.Insert
                    (Du.Item'
                       (Path_Length   => Child'Length,
                        Kind          => Den.Kind (Child),
                        Path          => Child,
                        Size          =>
                          (case Den.Kind (Child) is
                              when File      => Adirs.Size (Child),
                              when Softlink  =>
                                Sizes (Den.Target_Length (Child)),
                              when others    => 0),
                        Children_Size => Children_Size,
                        Children      => Children));
               end;
            end loop;
         end return;
      end List_Internal;

      Clean_Path : constant Den.Path := Den.Scrub (Path);
   begin
      Current_Sorting := Sort;

      return Root : Tree do
         if Den.Kind (Clean_Path) not in Directory then
            return;
         end if;

         declare
            Children : constant Tree := List_Internal (Clean_Path);
            Size     : Sizes := 0;
         begin
            for Child of Children loop
               Size := Size + Child.Tree_Size;
            end loop;

            Root.Insert
              (Item'
                 (Path_Length   => Clean_Path'Length,
                  Kind          => Den.Kind (Clean_Path),
                  Path          => Clean_Path,
                  Size          =>
                    (case Den.Kind (Clean_Path) is
                        when File      => Adirs.Size (Clean_Path),
                        when Softlink  =>
                          Sizes (Den.Target_Length (Clean_Path)),
                        when others    => 0),
                  Children_Size => Size,
                  Children      => Children));
         end;
      end return;
   end List;

   -----------
   -- Print --
   -----------

   procedure Print (This : Tree) is

      -----------
      -- Print --
      -----------

      procedure Print (Prefix : String; This : Tree) is
         use GNAT.IO;
      begin
         for Item of This loop
            Put_Line (Prefix
                      & Item.Element.Path
                      & ":"
                      & Sizes'(Item.Element.Children_Size
                             + Item.Element.Size)'Image
                      & " tree bytes,"
                      & Item.Element.Size'Image & " self bytes");
            if not Item.Element.Children.Is_Empty then
               Print (Prefix & "   ", Item.Element.Children);
            end if;
         end loop;
      end Print;

   begin
      Print ("", This);
   end Print;

end Den.Du;
