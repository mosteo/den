with AAA.Strings; use AAA.Strings;

with Ada.Containers;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Den.Du;
with Den.Filesystem;
with Den.Iterators;
with Den.Walk;

--  with GNAT.OS_Lib;

procedure Den.Selftest is
   use Den;
   use Den.Filesystem;
   use Den.Operators;
   use Den.Walk;

   use type Ada.Containers.Count_Type;

   --  package Adirs renames Ada.Directories;

   R : constant Path := Driveless_Root;

   function F (S : String) return String
   is (if Dir_Separator /= '/'
       then Replace (S, "/", "" & Dir_Separator)
       else S);

   function "+" (S : String) return String renames F;

   Cases : constant Relative_Path := +"../cases";
   --  This path must be kept relative for some tests to be effective

   Canary : constant String := "canary";
   --  We use this file to ascertain if the check out has created softlinks

   --  package Dirs renames Ada.Directories;
   --  package OS renames GNAT.OS_Lib;

   Timed_Out : Boolean := True;

   --  Adjust some checks depending on whether git checkout created softlinks
   --  or not. Windows does support softlinks on NTFS at least, and on GH
   --  runners they're created. Developer mode must be enabled in Windows.
   Supported : constant Boolean := Kind ("canary") = Softlink;
   --  This is a link to "src"

begin
   Put_Line ("Testing with Supported = " & Supported'Image
             & " from dir " & Ada.Directories.Current_Directory);

   select
      delay 66.6;

   then abort
      --  Verify some membership tests

      --  Any path
      pragma Assert ("relative" in Path);
      pragma Assert (CWD / "name" in Path);
      pragma Assert (+"relative/" not in Path);
      pragma Assert (Scrub (+"relative/") in Path);
      pragma Assert (+"rel/a/tive" in Path);
      pragma Assert (+"rel/a//tive" not in Path);
      pragma Assert (Scrub (+"rel/a//tive") in Path);

      --  Absolute path
      pragma Assert (Root (CWD) in Absolute_Path);
      pragma Assert (CWD in Absolute_Path);
      pragma Assert (Is_Absolute (CWD));
      pragma Assert ("a" not in Absolute_Path);
      pragma Assert (not Is_Absolute ("a"));

      --  Relative path
      pragma Assert (CWD not in Relative_Path);
      pragma Assert ("relative" in Relative_Path);
      pragma Assert (+"rel/a/tive" in Relative_Path);

      --  Normal path
      pragma Assert (+"rel/a/tive" in Normal_Path);
      pragma Assert (+"rel/./tive" not in Normal_Path);
      pragma Assert (+"rel/../tive" not in Normal_Path);
      pragma Assert ("." not in Normal_Path);
      pragma Assert (".." not in Normal_Path);

      --  Hard path
      pragma Assert ("bin" in Hard_Path);
      pragma Assert -- name is softlink
        ((Canonical ("..") / "cases" / "links" / "b"
          not in Hard_Path) = Supported,
          (Canonical ("..") / "cases" / "links" / "b") & " = "
          & Kind ((Canonical ("..") / "cases" / "links" / "b"))'Image);
      pragma Assert -- middle path is softlink (i)
        ((Canonical ("..") / "cases" / "links" / "i" / "h"
         not in Hard_Path));
      --  If Supported, not because of softlink; otherwise not because
      --  non-existent.

      --  Canonical path
      pragma Assert (R in Canonical_Path);
      pragma Assert (Root (CWD) in Canonical_Path);
      pragma Assert (R / "asdf" not in Canonical_Path); -- non-existing
      pragma Assert (R / "." not in Canonical_Path);
      pragma Assert (CWD / ".." not in Canonical_Path);
      pragma Assert -- middle path is softlink (i)
        ((Canonical ("..") / "cases" / "links" / "i" / "h"
         not in Canonical_Path));
      --  If Supported, not because of softlink; otherwise not because
      --  non-existent.

      --  Root path
      pragma Assert (R in Root_Path);
      pragma Assert (Root (CWD) in Root_Path);
      pragma Assert (Is_Root (Root (CWD)));
      pragma Assert (Is_Root (R));
      pragma Assert ("a" not in Root_Path);
      pragma Assert ("a:" not in Root_Path); -- missing separator on Windows

      Put_Line ("OK types");

      --  Scrub
      pragma Assert (Scrub ("a") = "a");
      pragma Assert (Scrub ("a" / "b") = "a" / "b");
      pragma Assert (Scrub (CWD) = CWD);
      pragma Assert (Scrub (Root (CWD)) = Root (CWD));
      pragma Assert (Scrub (R) = R);
      pragma Assert (Scrub (+"a/") = "a");
      pragma Assert (Scrub (+"a//a") = +"a/a");

      --  Parts
      pragma Assert (R in Part);
      pragma Assert ("part" in Part);
      pragma Assert ("part" / "part" not in Part);
      pragma Assert (Parts ("a") = To_Vector ("a"));
      pragma Assert (Parts ("a" / "b") = To_Vector ("a").Append ("b"));
      pragma Assert (Parts (R) = To_Vector (R));
      pragma Assert (Parts (R / "a") = To_Vector (R).Append ("a"));
      pragma Assert ("." in Relative_Parts);
      pragma Assert (".." in Relative_Parts);
      pragma Assert ("a" not in Relative_Parts);

      --  Parts on Windows
      if Den.Dir_Separator = '\' then
         pragma Assert (Is_Absolute ("\\a"), "\\a not considered absolute?");
         pragma Assert (Is_Root ("\\"));

         pragma Assert (Parts ("\\") = Den.To_Vector ("\\"));
         pragma Assert (Parts ("\\a") = Den.To_Vector ("\\").Append ("a"));
         pragma Assert (Den.To_Vector ("\\").Append ("a").To_Path = "\\a");

         Put_Line (Boolean'("\\?\" in Path)'Image);
         Put_Line (Parts ("\\?\").Flatten (":"));
         pragma Assert (Parts ("\\?\") = Den.To_Vector ("\\?\"));
         pragma Assert (Parts ("\\?\a") = Den.To_Vector ("\\?\").Append ("a"));
         pragma Assert (Den.To_Vector ("\\?\").Append ("a").To_Path = "\\?\a");

         pragma Assert (Parts ("\\.\") = Den.To_Vector ("\\.\"));
         pragma Assert (Parts ("\\.\a") = Den.To_Vector ("\\.\").Append ("a"));
         pragma Assert (Den.To_Vector ("\\.\").Append ("a").To_Path = "\\.\a");
      end if;

      --  Ancestry
      pragma Assert (Ancestors ("a").Is_Empty);
      pragma Assert (Ancestors (R / "a") = To_Set (R));
      pragma Assert (Ancestors ("a" / "b" / "c") =
                       To_Set ("a").Union (To_Set ("a" / "b")));
      pragma Assert (Ancestors (R / "a" / "b" / "c") =
                       To_Set (R)
                     .Union (To_Set (R / "a"))
                     .Union (To_Set (R / "a" / "b")));
      pragma Assert (Ancestors (Root (CWD)).Is_Empty);

      --  Other Subprograms

      --  Is_Broken
      pragma Assert (Is_Broken (+"../example/broken") = Supported);
      pragma Assert (not Is_Broken (+"../example"));
      pragma Assert (not Is_Broken ("unobtanium"));

      --  Is_Recursive
      for Part of AAA.Strings
        .To_Set ("self")
        .Union ("tic").Union ("toc")
        .Union ("human").Union ("centi").Union ("pede")
      loop
         pragma Assert (Is_Recursive (Cases / "loops" / Part) = Supported,
                        OS_Canonical (Cases / "loops" / Part));
      end loop;

      --  Canonical & Canonizable
      begin
         pragma Assert
           (Canonical (Cases / "loops" / "self") in Path); -- raises
         raise Program_Error with "Unexpected pass";
      exception
         when others => null;
      end;
      pragma Assert (not Canonizable ("not_a_real_file"));
      --  Strange, but "..." is a valid filename (!) (not in Windows)
      pragma Assert (Canonizable (R / ".."));
      pragma Assert (Canonical (R / "..") = Root (CWD));
      --  Strange, but /.. is resolved to / by the OS (any excess .. I guess)
      pragma Assert (not Canonizable (Cases / "loops" / "self") = Supported);
      if Supported then
         pragma Assert (Canonical (Cases / "links" / "b") = -- target file
                          Canonical (Cases) / "links" / "a");
         pragma Assert (Canonical (Cases / "links" / "h") = -- target dir
                          Canonical (Cases) / "links" / "g" / "h" / "deep");
         pragma Assert (Canonical (Cases / "links" / "i" / "h") = -- mid dir
                          Canonical (Cases) / "links" / "g" / "h");
      end if;

      --  Pseudocanonical
      pragma Assert (Kind (Pseudocanonical (Cases / "loops" / "self"))
                     = (if Supported then Softlink else File));
      if Supported then
         pragma Assert (not Canonizable (Cases / "loops" / "self"));
         pragma Assert (Pseudocanonical (Cases / "loops" / "self")
                        = Canonical (Cases) / "loops" / "self"); -- self target
         pragma Assert (not Canonizable (Cases / "loops" / "tic"));
         pragma Assert (Pseudocanonical (Cases / "loops" / "tic")
                        = Canonical (Cases) / "loops" / "tic"); -- cycle target
         pragma Assert (Pseudocanonical (Cases / "links" / "e") -- brken target
                        = Canonical (Cases) / "links" / "missing");
         pragma Assert (Pseudocanonical (Cases / "links" / "malformed")
                        = Canonical (Cases) / "links" / "mal" / "formed");
         pragma Assert (not Canonizable (Cases / "links" / "d")); -- self
         pragma Assert (Pseudocanonical (Cases / "loops" / "d" / "what") -- mid
                        = Canonical (Cases) / "loops" / "d" / "what");
         --  Broken links
         pragma Assert (Pseudocanonical (Cases / "links" / "e")
                        = Canonical (Cases) / "links" / "missing");
         pragma Assert (Pseudocanonical (Cases / "links" / "malformed")
                        = Canonical (Cases) / "links" / "mal" / "formed");
         pragma Assert (Pseudocanonical (Cases / "links" / "i" / "what") -- mid
                        = Canonical (Cases) / "links" / "g" / "what");
      end if;
      --  With non-existing targets
      pragma Assert (Pseudocanonical ("asdf") = CWD / "asdf");
      if Dir_Separator /= '\' then
         --  This is not a valid Windows path so OS_Canonical behaves oddly
         pragma Assert (Pseudocanonical ("...") = CWD / "...");
      end if;

      --  Absolute
      pragma Assert (Absolute (".") = CWD / ".");
      pragma Assert (Absolute ("a") = CWD / "a");

      --  Normal
      pragma Assert (Normal ("a" / ".") = "a");
      pragma Assert (Normal ("." / "a") = "a");
      pragma Assert (Normal (R / "." / "a") = R / "a");
      pragma Assert (Normal (CWD / "a" / "..") = CWD);
      pragma Assert (Normal (CWD / ".." / "a") = Parent (CWD) / "a");
      pragma Assert (Normal (R / "a" / ".." / "a") = R / "a");
      begin
         pragma Assert (Normal (R / "..") = R); -- should raise
         raise Program_Error with "Unexpected pass";
      exception
            when others => null;
      end;

      --  Absnormal
      pragma Assert (Absnormal (".") = CWD);
      pragma Assert (Absnormal ("a") = CWD / "a");
      pragma Assert (Absnormal ("a" / ".") = CWD / "a");
      pragma Assert (Absnormal ("." / "a") = CWD / "a");

      --  Name
      pragma Assert (Name ("a") = "a");
      pragma Assert (Name ("a" / "b") = "b");
      pragma Assert (Name (R / "a") = "a");
      pragma Assert (Name (R) = Driveless_Root);
      pragma Assert (Name (Root (CWD)) = Root (CWD));

      --  Has_Parent
      pragma Assert (Has_Parent ("a" / "b"));
      pragma Assert (Has_Parent (R / "b"));
      pragma Assert (not Has_Parent (R));
      pragma Assert (not Has_Parent ("a"));
      pragma Assert (not Has_Parent (Root (CWD)));

      --  Parent
      pragma Assert (Parent ("a" / "b") = "a");
      pragma Assert (Parent ("a" / "b" / "c") = "a" / "b");
      pragma Assert (Parent (R / "a") = R);
      pragma Assert (Parent (CWD / "bin") = CWD);

      --  Canonical_Parent
      pragma Assert (Kind (Canonical_Parent (".") / "tests") = Directory);
      begin
         pragma Assert (Kind (Parent (".")) = Directory, "must raise");
         raise Program_Error with "Unexpected pass";
      exception
         when others => null;
      end;
      begin
         pragma Assert (Kind (Parent (Root (CWD))) = Directory, "must raise");
         raise Program_Error with "Unexpected pass";
      exception
         when others => null;
      end;

      --  Relative
      for Canon in Boolean'Range loop
         pragma Assert (Relative ("parent", +"parent/child", Canon) = "child");
         pragma Assert (Relative ("parent", +"parent/child/grand", Canon) =
                        +"child/grand");
         pragma Assert (Relative (+"parent/child", "parent", Canon) = "..");
         pragma Assert (Relative (+"parent/child/grand", "parent", Canon) =
                        +"../..");
         pragma Assert (Relative (+"common/left", +"common/right", Canon) =
                        +"../right");
      end loop;

      --  Resolve
      if Supported then
         pragma Assert
           (Resolve (Cases / "links" / "b") = Cases / "links" / "a");
         pragma Assert
           (Resolve (Cases / "links" / "e") = Cases / "links" / "missing");
         pragma Assert
           (Resolve (Cases / "links" / "f") =
                Cases / "links" / "not" / "found");
         pragma Assert
           (Resolve (Cases / "links" / "malformed") =
                Cases / "links" / "mal" / "formed");
         pragma Assert
           (Resolve (Cases / "loops" / "tic") = Cases / "loops" / "toc");
         pragma Assert (Resolve (Canary) = "src");
         pragma Assert (Resolve ("src") = "src");

         --  Target & friends
         pragma Assert (Target_Length (Canary) = 3);
         begin
            pragma Assert (Target_Length ("src") = 3, "must raise");
            raise Program_Error with "Unexpected pass";
         exception
            when others => null;
         end;
         pragma Assert (Target (Cases / "links" / "b") = "a");
         pragma Assert
           (Target (Cases / "links" / "malformed") = +"mal//formed",
            "Unexpected target: " & Target (Cases / "links" / "malformed"));
      end if;

      --  "/"
      pragma Assert ("a" / "b" = +"a/b");
      begin
         if "a" / Canonical (".") /= "" then -- Should raise
            raise Program_Error with "Unexpected pass";
         end if;
      exception
         when others => null;
      end;

      Put_Line ("OK subprograms");

      declare
         Targets : constant String := "abc";
         Seen    : Sorted_Paths;
      begin
         for F of Iterators.Iterate ("test_iterator") loop
            Seen.Insert (F);
            pragma Assert (F'Length = 1 and then
                             (for some Char of Targets => Char = F (F'First)));
         end loop;
         pragma Assert (Seen.Length = 3);
      end;
      Put_Line ("OK iterators");

      --  Ls
      for Canon in Boolean'Range loop
         for P of Ls (".", (Canonicalize => Canon)) loop
            pragma Assert (Is_Absolute (P) = Canon);
         end loop;
      end loop;
      Put_Line ("OK ls");

      --  Verify that enumeration of troublesome softlinks doesn't bomb.
      for Canon in Canonical_Parts'Range loop
         --  Plain enumerating
         pragma Assert
           (Find
              (Cases,
               Options => (Canonicalize => Canon, others => <>))
            .Length > 1);
         Put_Line ("OK find (" & Canon'Image & ")");
      end loop;
      --  Verify filtering
      for K in Kinds'Range loop
         for F of Find (".." / "cases", Filter  => Kind_Is (K))
         loop
            pragma Assert (Kind (F.Path) = K,
                           "Expected: " & K'Image
                           & "; got: " & Kind (F.Path)'Image
                           & "; path: " & F.Path);
         end loop;
         Put_Line ("OK find (" & K'Image & ")");
      end loop;
      --  TODO: we should have some exact comparisons of output traversals with
      --  all the Find options combinations. Lotsa work...
      Put_Line ("OK find");

      declare
         package Du is new Den.Du;
         use type Du.Sizes;
         Root : constant Du.Tree := Du.List (".." / "cases");
      begin
         --  Minimal check that listing happens and finds some size and it is
         --  not confused by bad links.
         pragma Assert (Root.First_Element.Tree_Size > 0);
         --  Also check the number of children is correct.
         pragma Assert (Root.Length in 1);
         pragma Assert (Du.Item (Root.First_Element).Children.Length in 2,
                        Du.Item (Root.First_Element).Children.Length'Image);
         --  Tnis number may change if items are added to the crate root!
      end;
      Put_Line ("OK du");

      Timed_Out := False;
   end select;

   --  COPY

   if Kind ("cases") = Directory then
      Delete_Tree ("cases");
   end if;
   Mkdir ("cases");
   Copy (Cases, "cases");
   for Item of Walk.Find ("cases") loop
      declare
         Src : constant Path := Item.Path;
         Dst : constant Path := ".." / Item.Path;
      begin
         pragma Assert
           (Kind (Src) = Kind (Dst),
            "item mismatch: " & Src
            & " (" & Kind (Src)'Image & ") /= "
            & Dst
            & " (" & Kind (Dst)'Image & ")");
      end;
   end loop;
   Delete_Tree ("cases");
   Put_Line ("OK copy");

   pragma Assert (not Timed_Out, "Timed out");

end Den.Selftest;
