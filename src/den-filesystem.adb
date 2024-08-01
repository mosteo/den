with Ada.Directories;

package body Den.Filesystem is

   package Dirs renames Ada.Directories;

   use Den.Operators;

   --------------
   -- Absolute --
   --------------

   function Absolute (This : Path) return Absolute_Path
   is (if Is_Absolute (This)
       then This
       else Current_Dir / This);

   -----------------
   -- Current_Dir --
   -----------------

   function Current_Dir return Path
   is (Dirs.Current_Directory);

   ---------------------
   -- Pseudocanonical --
   ---------------------

   function Pseudocanonical (This : Path) return Absolute_Path
   is
   begin
      if Canonizable (This) then
         return Canonical (This);
      end if;

      declare
         Parted : Path_Parts := Parts (Absolute (This));
         I      : Integer := Parted.First_Index;
      begin
         while I <= Parted.Last_Index loop
            if Eat_Dots (This, Parted, I) then
               null; -- Eat_Dots does what has to be done
            else
               declare
                  Phead : constant Path_Parts    := Parted.Up_To (I);
                  Head  : constant Absolute_Path := Phead.To_Path;
                  Ptail : constant Path_Parts    := Parted.From (I + 1);
               begin
                  --  At this point Head is absnormal, though may be a softlink
                  if Is_Softlink (Head) then
                     if Is_Broken (Head) then
                        if Can_Scrub (Target (Head)) then
                           Parted :=
                             Parts (Parent (Head)) -- Parent of broken link
                             & Parts (Scrub (Target (Head))) -- Link target
                             & Ptail; -- Remainder
                        else
                           --  We cannot replace the link with its target, as
                           --  it is a bad path, so simply leave as is.
                           I := I + 1;
                        end if;
                     elsif Is_Recursive (Head) then
                        --  Leave as is
                        I := I + 1;
                     else -- Valid link
                        --  Substitute target plus tail
                        Parted := Parts (Canonical (Head)) & Ptail;
                        --  No need to go over parts guaranteed to be good
                        I := Integer (Parts (Canonical (Head)).Length) + 1;
                     end if;
                  else
                     --  Just move on to the next bit
                     I := I + 1;
                  end if;
               end;
            end if;
         end loop;

         return Parted.To_Path;
      end;
   end Pseudocanonical;

   --------------
   -- Relative --
   --------------

   function Relative (From, Into : Path) return Path is
      F : Path_Parts := Parts (Absnormal (From));
      T : Path_Parts := Parts (Absnormal (Into));
   begin
      --  Trivial case: the same path
      if F = T then
         raise Bad_Operation with
           "Both paths are the same in Relative_Path: "
           & From & " --> " & Into;
      end if;

      --  If the first element is different, these are paths into different
      --  drives:
      if F.First_Element /= T.First_Element then
         return Absnormal (Into);
      end if;

      --  Remove the common prefix
      while F.First_Element = T.First_Element loop
         F.Delete_First;
         T.Delete_First;

         exit when F.Is_Empty or else T.Is_Empty;
      end loop;

      --  The paths are now at their divergence point; we must ascend as many
      --  times as parts remain in From.
      for I in 1 .. F.Length loop
         T.Prepend (Dots_Parent_Dir);
      end loop;

      return T.To_Path;
   end Relative;

end Den.Filesystem;
