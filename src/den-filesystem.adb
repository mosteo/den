with Ada.Directories;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with Den.Iterators;
with Den.OS;
with Den.Walk;

with GNAT.IO;
with GNAT.OS_Lib;

--------------------
-- Den.Filesystem --
--------------------

package body Den.Filesystem is

   package Dirs   renames Ada.Directories;
   package OS_Lib renames GNAT.OS_Lib;

   use Den.Operators;

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (Text : String) is
      use GNAT.IO;
   begin
      Put_Line (Standard_Error, Text);
   end Put_Error;

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
               Copy_Attributes (Src, Dst,
                                Timestamps  => Options.Preserve_Timestamps,
                                Permissions => Options.Preserve_Permissions);
            when Directory =>
               Copy (Src, Dst / Name (Src), Options);
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

         --  Fix permissions if requested

         Copy_Attributes (Src, Dst,
                          Timestamps  => Options.Preserve_Timestamps,
                          Permissions => Options.Preserve_Permissions);
      end Copy_Dir;

      ---------------
      -- Copy_Link --
      ---------------

      procedure Copy_Link is
      begin
         case Kind (Dst) is
            when Nothing =>
               null; -- done below
            when Directory =>
               Copy (Src, Dst / Name (Src));
               return;
            when File =>
               if Options.Overwrite_Files then
                  Dirs.Delete_File (Dst);
                  --  Copy done below
               else
                  raise Use_Error with
                  Error ("not overwriting exising target: " & Dst);
               end if;
            when Softlink | Special =>
               raise Use_Error with
                 Error ("not overwriting existing link/special target: "
                        & Dst);
         end case;

         --  Here we recreate the link. Target might not yet be in place.
         OS.Create_Link
           (Name   => Dst,
            Target => Target (Src),
            Is_Dir => Kind (Src, Resolve_Links => True) = Directory);
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
           & Dst & " ...");

      case Kind (Src) is
         when Nothing =>
            raise Name_Error with Error ("non-existent source: " & Src);
         when File =>
            Copy_File;
         when Directory =>
            Copy_Dir;
         when Softlink =>
            if Options.Resolve_Links and then Canonizable (Src) then
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

   ---------------------
   -- Copy_Attributes --
   ---------------------

   procedure Copy_Attributes (Src, Dst    : Path;
                              Permissions : Boolean;
                              Timestamps  : Boolean)
   is
      OK : Boolean := False;
   begin
      if not (Permissions or else Timestamps) then
         return;
      end if;

      OS_Lib.Copy_File_Attributes
        (From             => Src,
         To               => Dst,
         Success          => OK,
         Copy_Timestamp   => Timestamps,
         Copy_Permissions => Permissions);

      if not OK then
         Put_Error
           ("When copying attributes from " & Src & " to " & Dst & ":");
         if Permissions then
            Put_Error ("- permissions");
         end if;
         if Timestamps then
            Put_Error ("- timestamps");
         end if;
         raise Dirs.Status_Error with
           Error ("Could not copy attributes");
      end if;
   end Copy_Attributes;

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
         when File | Softlink =>
            Delete_File (This, Options.Delete_Files);
         when others =>
            raise Program_Error with Error ("should be unreachable");
      end case;

      Log ("deleting: " & This & P (Kind (This)'Image) & " OK");
   end Delete_Directory;

   ----------
   -- Link --
   ----------

   procedure Link (From, Target : Path;
                   Options      : Link_Options := (others => <>))
   is
      Abs_Target : constant Path :=
        (if Is_Absolute (Target) then
            Target
         else
            Absnormal (Parent (Absnormal (From)) / Target));

      Rel_Target : constant Path :=
         (if Options.Relative_Target_From_Absolute_Path then
               Relative (Parent (Absolute (From)), Abs_Target)
          else
               Abs_Target);
   begin
      Log ("linking: " & From
           & " --> "
           & Target
           & P ("absolute target: " & Abs_Target & P (Kind (Abs_Target)'Image))
           & " ...");

      --  Divert if the relative path safety is enabled
      if Options.Relative_Target_From_Absolute_Path then
         if not Is_Absolute (Target) then
            raise Use_Error with
              Error ("target must be absolute with safety option but it is: "
                     & Target);
         elsif Is_Absolute (Rel_Target) then
            raise Use_Error with
              Error ("could not find a relative path from " & From
                     & " to " & Target);
         else
            declare
               New_Options : Link_Options := Options;
            begin
               New_Options.Relative_Target_From_Absolute_Path := False;
               Link
                 (From    => From,
                  Target  => Rel_Target,
                  Options => New_Options);
               return; -- EARLY EXIT
            end;
         end if;
      end if;

      if Kind (From) /= Nothing then
         if Options.Overwrite_Existing and then Kind (From) = Softlink
         then
            Delete_File (From,
               (Delete_Softlinks => Delete_Link, others => <>));
         else
               --  We don't want to overwrite an existing link
            raise Use_Error with
               Error ("item at new link path already exists: " & From &
                        " (kind: " & Kind (From)'Image & ")");
         end if;
      end if;

      if Kind (Abs_Target) = Nothing and then not Options.Allow_Missing_Target
      then
         raise Use_Error with
           Error ("target does not exist: " & Abs_Target);
      end if;

      OS.Create_Link (Target => Target,
                      Name   => From,
                      Is_Dir => Kind (Abs_Target) = Directory);

      pragma Assert (Kind (From) = Softlink,
                     "link not created: " & From & P (Kind (From)'Image));

      Log ("linking: " & From & P (Kind (From)'Image)
           & " --> "
           & Target & P (Kind (Target)'Image) & " OK");
   end Link;

   -----------
   -- Unlink --
   -----------

   procedure Unlink (This : Path) is
   begin
      Log ("unlinking: " & This & P (Kind (This)'Image) & " ...");

      if Kind (This) /= Softlink then
         raise Use_Error with
           Error ("target is not a softlink: " & This &
                  " (kind: " & Kind (This)'Image & ")");
      end if;

      OS.Delete_Link (This);

      Log ("unlinking: " & This & P (Kind (This)'Image) & " OK");
   end Unlink;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File
     (This    : Path;
      Options : Delete_File_Options := (others => <>)) is

      -----------------------------------
      -- Delete_Target_If_Regular_File --
      -----------------------------------

      procedure Delete_Target_If_Regular_File (Target_Path : Path) is
      begin
         if Kind (Target_Path) = File then
            Delete_File (Target_Path, Options);
         elsif Options.Do_Not_Fail then
            Log ("skipping non-file target: " & Target_Path &
                 " (kind: " & Kind (Target_Path)'Image & ")");
         else
            raise Use_Error with
              Error ("target is not a regular file: " & Target_Path &
                     " (kind: " & Kind (Target_Path)'Image & ")");
         end if;
      end Delete_Target_If_Regular_File;

   begin
      Log ("deleting file: " & This & P (Kind (This)'Image) & " ...");

      case Kind (This) is
         when Nothing =>
            if Options.Do_Not_Fail then
               Log ("skipping non-existent file: " & This);
               return;
            else
               raise Name_Error with
                 Error ("target does not exist: " & This);
            end if;
         when Directory =>
            if Options.Do_Not_Fail then
               Log ("skipping directory: " & This);
               return;
            else
               raise Use_Error with
                 Error ("target is directory, use Delete_Directory: " & This);
            end if;
         when File =>
            Dirs.Delete_File (This);
         when Softlink =>
            case Options.Delete_Softlinks is
               when Fail =>
                  raise Use_Error with
                    Error ("target is a softlink, use Unlink: " & This);
               when Delete_Link =>
                  Unlink (This);
               when Delete_Target =>
                  if Canonizable (This) then
                      Delete_Target_If_Regular_File (Canonical (This));
                  elsif Options.Do_Not_Fail then
                     Log ("skipping unresolvable link target: " & This);
                  else
                     raise Use_Error with
                       Error ("cannot delete target: " & Target (This)
                              & " of unresolvable link: "
                              & This);
                  end if;
               when Delete_Both =>
                    if Canonizable (This) then
                      Delete_Target_If_Regular_File (Canonical (This));
                  elsif not Options.Do_Not_Fail then
                     raise Use_Error with
                       Error ("cannot delete target of unresolvable link: "
                              & This);
                  end if;
                  Unlink (This);
            end case;
         when Special =>
            if Options.Do_Not_Fail then
               Log ("skipping special file: " & This);
               return;
            else
               raise Use_Error with
                 Error ("cannot delete special file: " & This);
            end if;
      end case;

      Log ("deleting file: " & This & P (Kind (This)'Image) & " OK");
   end Delete_File;

   -----------------
   -- Delete_Tree --
   -----------------

   procedure Delete_Tree (This : Path) is
   begin
      Delete_Directory (This, (Delete_Files => (others => <>),
                               Recursive    => True));
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
                     elsif not Canonizable (Head) then
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
      Log ("Relative: " & From & " --> " & Into & " ...");

      --  Start by using the parent if From is not a folder
      if Exists (From) and then Kind (From) /= Directory then
         raise Use_Error with
           Error ("From is not a directory: " & From
                  & " (kind: " & Kind (From)'Image & ")");
      end if;

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
