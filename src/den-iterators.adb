with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations;
--  with GNAT.IO; use GNAT.IO;

package body Den.Iterators is

   package Ops renames GNAT.Directory_Operations;

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

            procedure Iterate is
               Dir : Ops.Dir_Type;
               type Path_Access is access String;
               -- Predicate on Path gives trouble
               procedure Free is
                 new Ada.Unchecked_Deallocation (String, Path_Access);

               Last  : Natural;
               Max   : Positive := 1024;
            begin
               --  Try to read all dir elements; if insufficient buffer, try
               --  again. Recursivity could be removed, but with exponential
               --  grow wo cares..
               loop
                  declare
                     Item : Path_Access := new Path (1 .. Max);
                     function Read return Path is (Item (1 .. Last))
                       with Inline;
                  begin
                     Ops.Open (Dir, This);
                     loop
                        Ops.Read (Dir, Item.all, Last);

                        if Last = 0 then
                           Ops.Close (Dir);
                           Free (Item);
                           return;
                        elsif Last = Item'Last then
                           --  Too small, restart
                           Items.Clear;
                           Max := Max * 2;
                           Ops.Close (Dir);
                           Free (Item);
                           exit;
                        elsif Read /= "." and then Read /= ".." then
                           Items.Append (Item (Item'First .. Last));
                        end if;
                     end loop;
                  end;
               end loop;
            end Iterate;
         begin
            Iterate;
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
