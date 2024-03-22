with Ada.Directories;

with Den.Iterators;

with GNAT.IO; use GNAT.IO;

package body Den is

   package Dirs renames Ada.Directories;
   package OS renames GNAT.OS_Lib;

   use all type Dirs.File_Kind;

   ------------
   -- Exists --
   ------------

   function Exists (This : Path) return Boolean
   is (Dirs.Exists (This));

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (This : Path) return Boolean
   is (Dirs.Exists (This) and then Dirs.Kind (This) = Directory);

   -------------
   -- Is_File --
   -------------

   function Is_File (This : Path) return Boolean
   is (Dirs.Exists (This) and then Dirs.Kind (This) = Ordinary_File);

   ----------------
   -- Is_Special --
   ----------------

   function Is_Special (This : Path) return Boolean
   is (Dirs.Exists (This) and then Dirs.Kind (This) = Special_File);

   -----------------
   -- Is_Softlink --
   -----------------

   function Is_Softlink (This : Path) return Boolean
   is (OS.Is_Symbolic_Link (This));

   ---------------
   -- Full_Path --
   ---------------

   function Full_Path (This : Path) return Absolute_Path
   is (OS.Normalize_Pathname

   ------------
   -- Target --
   ------------

   function Target (This : Path) return Path
   is (if Exists (This) then
          Dirs.Full_Name (This)
       elsif Is_Softlink (This) then -- Must be broken
          raise Program_Error with "unimplemented"
       else This);

   --------
   -- Ls --
   --------

   function Ls (This      : Path;
                Normalize : Boolean := False)
                return Paths is
   begin
      return Result : Paths do
         if not Exists (This) then
            return;
         end if;

         if Is_Softlink (This) or else not Is_Directory (This) then
            if Normalize then
               Result.Insert
                 (OS.Normalize_Pathname (This, Resolve_Links => False));
            else
               Result.Insert (This);
            end if;
            return;
         end if;

         for Item of Iterators.Iterate (This) loop
            if Normalize then
               Result.Insert
                 (OS.Normalize_Pathname (Item, Resolve_Links => False));
            else
               Result.Insert (Item);
            end if;
         end loop;
      end return;
   end Ls;

   ----------
   -- Find --
   ----------

   procedure Find
     (This    : Path;
      Action  : access procedure (This  : Item;
                                  Enter : in out Boolean;
                                  Stop  : in out Boolean);
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
   is

      Enter : Boolean := True;
      Stop  : Boolean := False;

      --------------
      -- New_Item --
      --------------

      function New_Item (Here : Path; Depth : Natural) return Item
      is
      begin
         return (Length => Here'Length,
                 Path   => Here,
                 Depth  => Depth);
      end New_Item;

      ----------
      -- Find --
      ----------

      procedure Find (Parent : Path; Depth : Positive) is
         Base : constant Path :=
                  (if Options.Normalize_Paths
                   then OS.Normalize_Pathname (Parent, Resolve_Links => False)
                      & OS.Directory_Separator
                   else Parent);
      begin
         for Item of Ls (Parent, Normalize => False) loop
            if Stop then
               return;
            end if;

            declare
               Child : constant Path := Base & Item;
            begin
               Action (This  => New_Item (Child, Depth),
                       Enter => Enter,
                       Stop  => Stop);

               if Stop then
                  return;
               end if;

               if Enter and then Is_Directory (Child) then
                  if (Is_Softlink (Child)
                      and then Options.Enter_Softlinked_Dirs)
                    or else
                      (not Is_Softlink (Child)
                       and then Options.Enter_Regular_Dirs)
                  then
                     Find (Child & OS.Directory_Separator, Depth + 1);
                  end if;
               end if;
            end;
         end loop;
      end Find;

   begin
      if not Exists (This) or else Is_Softlink (This) then
         Action (New_Item (This, 0),
                 Enter => Enter,
                 Stop  => Stop);
      elsif Is_Directory (This)
        and then This (This'Last) /= OS.Directory_Separator
      then
         Find (This & OS.Directory_Separator, 1);
      else
         Find (This, 1);
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (This    : Path;
      Options : Find_Options  := (others => <>);
      Filter  : Filters'Class := No_Filter'(null record))
      return Items
   is
   begin
      return Result : Items do
         declare

            ----------
            -- Find --
            ----------

            procedure Find (This  : Item;
                            Enter : in out Boolean;
                            Stop  : in out Boolean)
            is
            begin
               Result.Insert (This);
            end Find;

         begin
            Find (This, Find'Access, Options, Filter);
         end;
      end return;
   end Find;

   -------------
   -- Current --
   -------------

   function Current return Path renames Dirs.Current_Directory;

end Den;
