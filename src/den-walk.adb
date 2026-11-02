with Ada.IO_Exceptions;

with Den.Filesystem; use Den.Filesystem;
with Den.Iterators;
with Den.OS;

--  with GNAT.IO; use GNAT.IO;

package body Den.Walk is

   use Operators;

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

      --  The real directories being entered, which is only needed when links
      --  are followed, as they can lead back to one of them, or for deduping,
      --  in which case they stay here after being done with.
      Track_Entered : constant Boolean :=
                        Options.Enter_Softlinked_Dirs
                        or else Options.Canonicalize = All_Deduped;
      Entered       : Sorted_Paths;

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

      ------------
      -- Visits --
      ------------

      --  Softlinks may be excluded from being reported, not from being entered

      function Visits (Here : Path) return Boolean
      is (Options.Visit_Softlinks or else Kind (Here) /= Softlink);

      procedure List (Dir : Path; Depth : Positive);

      ---------------
      -- Enter_Dir --
      ---------------

      procedure Enter_Dir (Dir : Path; Depth : Positive) is
         Real : constant String :=
                  (if Track_Entered then OS_Canonical (Dir) else "");
      begin
         if not Track_Entered then
            List (Dir, Depth);

         elsif Real /= "" and then not Entered.Contains (Real) then
            Entered.Insert (Real);
            List (Dir, Depth);

            if Options.Canonicalize /= All_Deduped then
               Entered.Delete (Real);
            end if;
         end if;
      end Enter_Dir;

      ----------
      -- List --
      ----------

      procedure List (Dir : Path; Depth : Positive) is
         Base : constant Path :=
                  (if Options.Canonicalize /= None
                   then Pseudocanonical (Dir)
                   else Dir);
         Listed : constant Path :=
                    (if Kind (Dir) = Softlink then Canonical (Dir) else Dir);
         --  Ls of a softlink gives the link itself, so list its target when
         --  entering a softlinked dir (the start of the walk, or any one with
         --  Enter_Softlinked_Dirs). Children are still reported below Base.
      begin
         for Item of Ls (Listed,
                         (Canonicalize    => False,
                          Skip_Unreadable => Options.Skip_Unreadable))
         loop
            if Stop then
               return;
            end if;

            declare
               Child_Plain : constant Path := Base / Item;
               Child       : constant Path :=
                               (case Options.Canonicalize is
                                   when None | Den.Walk.Base         =>
                                     Child_Plain,
                                   when All_With_Dupes | All_Deduped =>
                                     Pseudocanonical (Child_Plain));
            begin
               if Kind (Child_Plain) = Nothing then
                  OS.Check_Not_Too_Long (Child_Plain);
               end if;

               Enter := True;

               if Options.Canonicalize /= All_Deduped
                 or else not Visited.Contains (Child)
               then
                  if Options.Canonicalize = All_Deduped then
                     Visited.Insert (Child);
                  end if;

                  --  Match the fully resolved path
                  if Filter.Match (Child) and then Visits (Child_Plain) then
                     Action (This  => New_Item (Child, Depth),
                             Enter => Enter,
                             Stop  => Stop);

                     if Stop then
                        return;
                     end if;
                  end if;
               end if;

               if Enter
                 and then
                   ((Options.Enter_Regular_Dirs
                     and then Kind (Child_Plain) = Directory)
                    or else
                      (Options.Enter_Softlinked_Dirs
                       and then Kind (Child_Plain) = Softlink
                       and then Target_Kind (Child_Plain) = Directory))
               then
                  Enter_Dir (Child_Plain, Depth + 1);
               end if;
            end;
         end loop;
      end List;

   begin
      if Kind (This) = Nothing then
         OS.Check_Not_Too_Long (This);
      end if;

      if Target_Kind (This) = Directory then
         Enter_Dir (This, 1);
      elsif Exists (This) and then Filter.Match (This) and then Visits (This)
      then
         Action (New_Item (This, 0),
                 Enter => Enter,
                 Stop  => Stop);
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

      --  Name is what is reported, unless canonicalizing the Full path to it

      procedure Insert (Name, Full : Path; Into : in out Sorted_Paths) is
      begin
         if Options.Canonicalize then
            declare
               Canonical : constant String := OS_Canonical (Full);
            begin
               if Canonical /= "" then
                  --  Resolved links may point to the same file
                  Into.Include (Canonical);
               else
                  raise Bad_Path with
                    "Cannot find canonical path for: " & Full;
               end if;
            end;
         else
            Into.Insert (Name);
         end if;
      end Insert;

   begin
      return Result : Sorted_Paths do
         case Kind (This) is
            when Nothing =>
               OS.Check_Not_Too_Long (This);
               return;

            when Childless_Kinds =>
               Insert (This, This, Into => Result);

            when Directory =>
               begin
                  for Item of Iterators.Iterate (This) loop
                     Insert (Item, This / Item, Into => Result);
                  end loop;
               exception
                  when Ada.IO_Exceptions.Use_Error =>
                     if not Options.Skip_Unreadable then
                        raise;
                     end if;
               end;
         end case;
      end return;
   end Ls;

end Den.Walk;
