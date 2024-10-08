with Ada.Directories;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with C_Strings;

with Den.Iterators;
with Den.Walk;

with GNAT.IO;
--  with GNAT.OS_Lib;
with GNAT.Source_Info; use GNAT.Source_Info;

package body Den.Filesystem is

   package Dirs renames Ada.Directories;
   --  package OS   renames GNAT.OS_Lib;

   use Den.Operators;

   ---------
   -- Log --
   ---------

   procedure Log (Message  : String;
                  Location : String := Source_Location)
   is
   begin
      if Debug then
         GNAT.IO.Put_Line ("[DEN] " & Location & ": " & Message);
      end if;
   end Log;

   -----------
   -- Error --
   -----------

   function Error (Info     : String;
                   Location : String := Source_Location)
                   return String
   is (Location & ": " & Info);

   --------------
   -- Absolute --
   --------------

   function Absolute (This : Path) return Absolute_Path
   is (if Is_Absolute (This)
       then This
       else Current_Dir / This);

   ----------
   -- Copy --
   ----------

   procedure Copy (Src, Dst : Path;
                   Options  : Copy_Options := (others => <>))
   is

      ---------------
      -- Copy_File --
      ---------------

      procedure Copy_File is
      begin
         case Kind (Dst) is
            when Nothing =>
               Dirs.Copy_File (Src, Dst);
            when Directory =>
               Dirs.Copy_File (Src, Dst / Name (Src));
            when File =>
               if Options.Overwrite_Files then
                  Dirs.Delete_File (Dst);
                  Copy_File;
               else
                  raise Use_Error with
                    Error ("not overwriting exising target: " & Dst);
               end if;
            when Softlink | Special =>
               raise Use_Error with
                 Error ("not overwriting existing link/special target: "
                        & Dst);
         end case;
      end Copy_File;

      --------------
      -- Copy_Dir --
      --------------

      procedure Copy_Dir is
      begin
         case Kind (Dst) is
            when Nothing =>
               raise Name_Error with
                 Error ("non-existent destination: " & Dst);
            when File | Softlink | Special =>
               raise Use_Error with
                 Error ("not overwriting existing target (" & Kind (Dst)'Image
                        & "): " & Dst);
            when Directory =>
               if not Options.Merge_Dirs and then
                 Walk.Ls (Dst).Length not in 0
               then
                  raise Use_Error with
                    Error ("not merging (disabled) into existing target: "
                           & Dst);
               end if;
         end case;

         --  Actually copy/merge

         for Item of Iterators.Iterate (Src) loop
            case Kind (Src / Item) is
               when Nothing =>
                  raise Program_Error with Error ("item missing?: "
                                                  & (Src / Item));
               when Directory =>
                  if Kind (Dst / Item) = Nothing then
                     Dirs.Create_Directory (Dst / Item);
                  end if;
                  Copy (Src / Item, Dst / Item);

               when others =>
                  Copy (Src / Item, Dst);
            end case;
         end loop;
      end Copy_Dir;

      ---------------
      -- Copy_Link --
      ---------------

      procedure Copy_Link is
         function C_Copy_Link (Target, Name : C_Strings.Chars_Ptr)
                               return C_Strings.C.int
           with Import, Convention => C;
         use C_Strings;
      begin
         case Kind (Dst) is
            when Nothing =>
               null;
            when Directory =>
               Copy (Src, Dst / Name (Src));
               return;
            when File =>
               if Options.Overwrite_Files then
                  Dirs.Delete_File (Dst);
               else
                  raise Use_Error with
                  Error ("not overwriting exising target: " & Dst);
               end if;
            when Softlink | Special =>
               raise Use_Error with
                 Error ("not overwriting existing link/special target: "
                        & Dst);
         end case;

         --  Here we must copy

         declare
            Result : constant Integer :=
                       Integer
                         (C_Copy_Link
                            (To_C (Target (Src)).To_Ptr,
                             To_C (Dst).To_Ptr));
         begin
            if Result /= 0 then
               raise Use_Error
                 with Error ("cannot create softlink "
                             & Dst & " --> " & Target (Src)
                             & " (error: " & Result'Image & ")");
            end if;
         end;
      end Copy_Link;

      ------------------
      -- Copy_Special --
      ------------------

      procedure Copy_Special is
      begin
         raise Use_Error with Error ("cannot copy special file: " & Src);
      end Copy_Special;
   begin
      Log ("Copy " & Src & P (Kind (Src)'Image)
           & " --> "
           & Dst & P (Kind (Dst)'Image) & " ...");

      case Kind (Src) is
         when Nothing =>
            raise Name_Error with Error ("non-existent source: " & Src);
         when File =>
            Copy_File;
         when Directory =>
            Copy_Dir;
         when Softlink =>
            if Options.Resolve_Links and then Is_Resolvable (Src) then
               Copy (Target (Src), Dst);
            else
               Copy_Link;
            end if;
         when Special =>
            Copy_Special;
      end case;

      Log ("Copy " & Src & P (Kind (Src)'Image)
           & " --> "
           & Dst & P (Kind (Dst)'Image) & " OK");
   end Copy;

   ----------------------
   -- Create_Directory --
   ----------------------

   procedure Create_Directory
     (Target           : Path;
      Options          : Create_Directory_Options := (others => <>))
   is
   begin
      case Target_Kind (Target) is
         when Directory =>
            if Options.Fail_If_Existing then
               raise Use_Error
                 with Error ("target exists: " & Target);
            else
               return;
            end if;
         when Nothing =>
            if Options.Create_Intermediate then
               Dirs.Create_Path (Target);
            else
               Dirs.Create_Directory (Target);
            end if;
         when others =>
            raise Use_Error
              with Error ("target ("
                          & Target_Kind (Target)'Image & ") exists: "
                          & Target);
      end case;
   end Create_Directory;

   -----------------
   -- Current_Dir --
   -----------------

   function Current_Dir return Path
   is (Dirs.Current_Directory);

   ----------------------
   -- Delete_Directory --
   ----------------------

   procedure Delete_Directory
     (This    : Path;
      Options : Delete_Directory_Options := (others => <>))
   is
   begin
      Log ("deleting: " & This & P (Kind (This)'Image) & " ...");

      --  Diagnostics

      case Kind (This) is
         when Nothing =>
            if not Options.Recursive then
               raise Name_Error with
               Error ("target does not exist: " & This);
            end if;
         when File | Softlink =>
            if not Options.Recursive then
               raise Use_Error with
               Error ("target kind is not deletable without recursive: "
                      & This & P (Kind (This)'Image));
            end if;
         when Special =>
            raise Use_Error with
            Error ("target kind is not deletable: "
                   & This & P (Kind (This)'Image));
         when Directory =>
            null;
      end case;

      --  Actual deletion

      case Kind (This) is
         when Directory =>
            for Item of Iterators.Iterate (This) loop
               Delete_Directory (This / Item, Options);
            end loop;
            Dirs.Delete_Directory (This); -- Should be empty now
         when File =>
            Dirs.Delete_File (This);
         when Softlink =>
            declare
               use C_Strings;
               function C_Delete_Link (Target : C_Strings.Chars_Ptr)
                                       return C_Strings.C.int
                 with Import, Convention => C;
            begin
               if C_Delete_Link (To_C (This).To_Ptr) not in 0 then
                  raise Use_Error with
                  Error ("failed to delete: " & This & P (Kind (This)'Image));
               end if;
            end;
         when others =>
            raise Program_Error with Error ("should be unreachable");
      end case;

      Log ("deleting: " & This & P (Kind (This)'Image) & " OK");
   end Delete_Directory;

   -----------------
   -- Delete_Tree --
   -----------------

   procedure Delete_Tree (This : Path) is
   begin
      Delete_Directory (This, (Recursive => True));
   end Delete_Tree;

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

   function Relative (From, Into   : Path;
                      Canonicalize : Boolean := False)
                      return Path
   is
      F : Path_Parts := Parts
        (if Canonicalize
         then Pseudocanonical (From)
         else Absnormal (From));
      T : Path_Parts := Parts
        (if Canonicalize
         then Pseudocanonical (Into)
         else Absnormal (Into));
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
