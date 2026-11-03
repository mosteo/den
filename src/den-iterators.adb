with Den.OS;

package body Den.Iterators is

   -------------
   -- Iterate --
   -------------

   function Iterate (This : Path) return Dir_Iterator is
   begin
      return Result : Dir_Iterator do
         declare
            Items : Lists.List renames Result.Items;
            --  This convoluted procedure-inside-return is both to work around
            --  a bug in GNAT 13.2 from Ubuntu 22.04 and to avoid a copy of
            --  these Items.

            procedure Add (Name : String) is
            begin
               if Name /= "." and then Name /= ".." then
                  Items.Append (Name);
               end if;
            end Add;
         begin
            OS.List (This, Add'Access);
         end;
      end return;
   end Iterate;

   -----------
   -- First --
   -----------

   function First (This : Dir_Iterator) return Cursor
   is (Pos => This.Items.First);

   ----------
   -- Next --
   ----------

   function Next (This : Dir_Iterator; C : Cursor) return Cursor
   is (Pos => Lists.Next (C.Pos));

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Dir_Iterator; C : Cursor) return Boolean
   is (Lists.Has_Element (C.Pos));

   -------------
   -- Element --
   -------------

   function Element (This : Dir_Iterator; C : Cursor) return Path
   is (Lists.Element (C.Pos));

end Den.Iterators;
