with Den.Iterators;

package body Den.Walk is

   package OS renames GNAT.OS_Lib;

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

      Visited : Sorted_Paths; -- For deduping

      Enter   : Boolean := True;
      Stop    : Boolean := False;

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

      subtype Dir_Path is String with Dynamic_Predicate =>
        Dir_Path (Dir_Path'Last) = Dir_Separator;

      ----------
      -- Find --
      ----------

      procedure Find (Parent : Dir_Path; Depth : Positive) is
         Base : constant Dir_Path :=
                  (if Options.Canonicalize /= None
                   then Pseudocanonical
                     (Parent (Parent'First .. Parent'Last - 1))
                   & OS.Directory_Separator
                   else Parent);
      begin
         for Item of Ls (Parent (Parent'First .. Parent'Last - 1),
                         (Canonicalize => False))
         loop
            if Stop then
               return;
            end if;

            declare
               Child_Plain : constant Path := Base & Item;
               Child       : constant Path :=
                               (case Options.Canonicalize is
                                   when None | Den.Walk.Base         =>
                                     Base & Item,
                                   when All_With_Dupes | All_Deduped =>
                                     Pseudocanonical (Base & Item));
            begin
               Enter := True;
               Stop  := False;

               if Options.Canonicalize /= All_Deduped or else
                 not Visited.Contains (Child)
               then
                  --  Match the fully resolved path
                  if not Filter.Match (Child) then
                     goto Continue;
                  end if;

                  Action (This  => New_Item (Child, Depth),
                          Enter => Enter,
                          Stop  => Stop);

                  if Stop then
                     return;
                  end if;

                  if Options.Canonicalize = All_Deduped then
                     Visited.Insert (Child);
                  end if;
               end if;

               if Enter then
                  if (Options.Enter_Regular_Dirs
                      and then Kind (Child_Plain) = Directory)
                    or else
                      (Options.Enter_Softlinked_Dirs
                       and then Target_Kind (Child_Plain) = Directory)
                  then
                     Find (Child_Plain & OS.Directory_Separator, Depth + 1);
                  end if;
               end if;
            end;

            <<Continue>>
         end loop;
      end Find;

   begin
      if Target_Kind (This) /= Directory and then Filter.Match (This) then
         Action (New_Item (This, 0),
                 Enter => Enter,
                 Stop  => Stop);
      else
         Find (This & OS.Directory_Separator, 1);
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

            procedure Find (This         : Item;
                            Unused_Enter : in out Boolean;
                            Unused_Stop  : in out Boolean)
            is
            begin
               Result.Insert (This);
            end Find;

         begin
            Find (This, Find'Access, Options, Filter);
         end;
      end return;
   end Find;

   --------
   -- Ls --
   --------

   function Ls (This    : Path;
                Options : Ls_Options := (others => <>))
                return Sorted_Paths is

      ------------
      -- Insert --
      ------------

      procedure Insert (This : Path; Into : in out Sorted_Paths) is
      begin
         if Options.Canonicalize then
            Into.Include -- resolved links may point to the same file
              (OS_Canonical (This));
         else
            Into.Insert (This);
         end if;
      end Insert;

   begin
      return Result : Sorted_Paths do
         case Kind (This) is
            when Nothing =>
               return;

            when Childless_Kinds =>
               Insert (This, Into => Result);

            when Directory =>
               for Item of Iterators.Iterate (This) loop
                  Insert (Item, Into => Result);
               end loop;
         end case;
      end return;
   end Ls;

end Den.Walk;
