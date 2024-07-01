with Ada.Containers;
--  with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Den; use Den;

--  with GNAT.OS_Lib;

procedure Selftest is
   use Den.Operators;
   use type Ada.Containers.Count_Type;
   use type Den.Sorted_Paths;

   --  package Dirs renames Ada.Directories;
   --  package OS renames GNAT.OS_Lib;
begin
   --  Verify that enumeration of troublesome softlinks doesn't bomb.
   for Canon in Canonical_Parts'Range loop
      pragma Assert
        (Find
           (".." / "example" / "cases",
            Options => (Canonicalize => Canon, others => <>))
         .Length > 1);
   end loop;
   Put_Line ("Find OK");

   --  Verify some membership tests

   --  Any path
   pragma Assert ("relative" in Path);
   pragma Assert (CWD / "name" in Path);
   pragma Assert ("relative/" not in Path);
   pragma Assert (Scrub ("relative/") in Path);
   pragma Assert ("rel/a/tive" in Path);
   pragma Assert ("rel/a//tive" not in Path);
   pragma Assert (Scrub ("rel/a//tive") in Path);

   --  Absolute path
   pragma Assert (CWD in Absolute_Path);

   --  Relative path
   pragma Assert (CWD not in Relative_Path);
   pragma Assert ("relative" in Relative_Path);
   pragma Assert ("rel/a/tive" in Relative_Path);

   --  Normal path
   pragma Assert ("rel/a/tive" in Normal_Path);
   pragma Assert ("rel/./tive" not in Normal_Path);
   pragma Assert ("rel/../tive" not in Normal_Path);
   pragma Assert ("." not in Normal_Path);
   pragma Assert (".." not in Normal_Path);

   --  Hard path
   pragma Assert ("bin" in Hard_Path);
   pragma Assert -- name is softlink
     (Canonical ("..") / "example" / "cases" / "links" / "b" not in Hard_Path);
   pragma Assert -- middle path is softlink (i)
     (Canonical ("..") / "example" / "cases" / "links" / "i" / "h"
      not in Hard_Path);

   --  Ancestry
   pragma Assert (Ancestors ("a").Is_Empty);
   pragma Assert (Ancestors (Driveless_Root / "a") = [1 => Driveless_Root]);
   pragma Assert (Ancestors (Root (CWD)).Is_Empty);

   --  Canonical path
   pragma Assert (Root (CWD) in Canonical_Path);
   pragma Assert (Root (CWD) / "asdf" in Canonical_Path);

   --  Parts
   pragma Assert ("part" in Part);
   pragma Assert ("part" / "part" not in Part);

   Put_Line ("Types OK");
end Selftest;
