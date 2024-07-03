with AAA.Strings; use AAA.Strings;

with Ada.Containers;
--  with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Den; use Den;

--  with GNAT.OS_Lib;

procedure Selftest is
   use Den.Operators;
   use type Ada.Containers.Count_Type;

   R : constant Path := Driveless_Root;

   function F (S : String) return String
   is (if Dir_Separator /= '/'
       then Replace (S, "/", "" & Dir_Separator)
       else S);

   function "+" (S : String) return String renames F;

   Cases : constant Path := +"../example/cases";

   --  package Dirs renames Ada.Directories;
   --  package OS renames GNAT.OS_Lib;
begin

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
           (".." / "example" / "cases",
            Options => (Canonicalize => Canon, others => <>))
         .Length > 1);
      --  Verify filtering
      for K in Kinds'Range loop
         for F of Find ("..",
                        Options => (Canonicalize => Canon, others => <>),
                        Filter  => Kind_Is (K))
         loop
            pragma Assert (Kind (F.Path) = K,
                           "Expected: " & K'Image
                           & "; got: " & Kind (F.Path)'Image
                           & "; path: " & F.Path);
         end loop;
      end loop;
   end loop;
   --  TODO: we should have some exact comparisons of output traversals with
   --  all the Find options combinations. Lotsa work...
   Put_Line ("OK find");

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
     (Canonical ("..") / "example" / "cases" / "links" / "b" not in Hard_Path);
   pragma Assert -- middle path is softlink (i)
     (Canonical ("..") / "example" / "cases" / "links" / "i" / "h"
      not in Hard_Path);

   --  Canonical path
   pragma Assert (R in Canonical_Path);
   pragma Assert (Root (CWD) in Canonical_Path);
   pragma Assert (R / "asdf" in Canonical_Path);
   pragma Assert (R / "." / "asdf" not in Canonical_Path);
   pragma Assert (R / ".." / "asdf" not in Canonical_Path);
   pragma Assert -- middle path is softlink (i)
     (Canonical ("..") / "example" / "cases" / "links" / "i" / "h"
      not in Canonical_Path);

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
   pragma Assert (Is_Broken (+"../example/broken"));
   pragma Assert (not Is_Broken (+"../example"));
   pragma Assert (not Is_Broken ("unobtanium"));

   --  Is_Recursive
   for Part of AAA.Strings.Sets.Set'(["self",
                                     "tic", "toc",
                                     "human", "centi", "pede"])
   loop
      pragma Assert (Is_Recursive (Cases / "loops" / Part));
   end loop;

   --  Canonical
   begin
      pragma Assert (Canonical (Cases / "loops" / "self") in Path); -- raises
      raise Program_Error with "Unexpected pass";
   exception
      when others => null;
   end;

   --  Canonical_Or_Same
   pragma Assert (Kind (Canonical_Or_Same (Cases / "loops" / "self"))
                  = Softlink);

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
   pragma Assert (Kind (Canonical_Parent (".") / "selftest") = Directory);
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

   --  Resolve
   pragma Assert (Resolve (Cases / "links" / "b") = Cases / "links" / "a");
   pragma Assert
     (Resolve (Cases / "links" / "e") = Cases / "links" / "missing");
   pragma Assert
     (Resolve (Cases / "links" / "f") = Cases / "links" / "not" / "found");
   pragma Assert
     (Resolve (Cases / "links" / "malformed") =
          Cases / "links" / "mal" / "formed");
   pragma Assert (Resolve (Cases / "loops" / "tic") = Cases / "loops" / "toc");
   pragma Assert (Resolve ("source") = "src");
   pragma Assert (Resolve ("src") = "src");

   --  Target & friends
   pragma Assert (Target_Length ("source") = 3);
   begin
      pragma Assert (Target_Length ("src") = 3, "must raise");
      raise Program_Error with "Unexpected pass";
   exception
      when others => null;
   end;
   pragma Assert (Target (Cases / "links" / "b") = "a");
   pragma Assert
     (Target (Cases / "links" / "malformed") = "mal//formed",
      "Unexpected target: " & Target (Cases / "links" / "malformed"));

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

end Selftest;
