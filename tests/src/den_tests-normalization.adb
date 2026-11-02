pragma Alire_Test;

with AAA.Strings;

with Ada.Text_IO;

with Den.Filesystem;

procedure Den_Tests.Normalization is

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir : constant Path := "normalization_test_dir";

   Top : constant Path := Root (Absnormal ("."));

   ----------------
   -- Bad_Normal --
   ----------------

   --  This has no normal form, and it must say so even without assertions

   procedure Bad_Normal (This : Path) is
   begin
      declare
         Result : constant String := String (Normal (This));
      begin
         raise Program_Error with
           "Normal (" & This & ") returned: [" & Result & "]";
      end;
   exception
      when Bad_Path =>
         null;
   end Bad_Normal;

   ------------------
   -- Check_Normal --
   ------------------

   procedure Check_Normal is
   begin
      --  Going up from a root

      Bad_Normal (Top / "..");
      Bad_Normal (Top / "a" / ".." / "..");

      --  Nothing left

      Bad_Normal (".");
      Bad_Normal ("a" / "..");
      Bad_Normal ("a" / "." / "..");

      --  Going up from where a relative path starts

      Bad_Normal ("..");
      Bad_Normal ("a" / ".." / "..");

      --  The root is what remains

      Assert (Normal (Top / "a" / "..") = Top);
      Assert (Normal (Top / ".") = Top);

      if Dir_Separator = '\' then
         Bad_Normal ("C:..");
         Bad_Normal ("C:..\foo");
         Bad_Normal ("C:\..");
         Bad_Normal ("\..");
      end if;
   end Check_Normal;

   ----------------------------
   -- Check_Too_Many_Parents --
   ----------------------------

   --  Dropped silently when the path isn't resolved by the OS

   procedure Check_Too_Many_Parents is
   begin
      Assert (Pseudocanonical (Top / "den_missing" / ".." / "..") = Top);
      Assert (Pseudocanonical (Top / "den_missing" / ".." / ".." / "x")
              = Top / "x");
      Assert (Pseudocanonical (Top / "den_missing" / ".." / ".." / ".." / "y")
              = Top / "y");
   end Check_Too_Many_Parents;

   ---------------------------
   -- Check_Absolute_Target --
   ---------------------------

   --  The target of a broken link replaces the path up to it, when absolute

   procedure Check_Absolute_Target is
      Real : constant Path := Canonical (Test_Dir);
   begin
      Link (Test_Dir / "abs_broken", Real / "missing" / "file",
            Options => (Allow_Missing_Target => True, others => <>));

      Assert (Pseudocanonical (Test_Dir / "abs_broken")
              = Real / "missing" / "file");
      Assert (Pseudocanonical (Test_Dir / "abs_broken" / "tail")
              = Real / "missing" / "file" / "tail");

      Unlink (Test_Dir / "abs_broken");
   end Check_Absolute_Target;

   ------------------------
   -- Check_Growing_Link --
   ------------------------

   --  A broken link whose target goes through itself would expand forever

   procedure Check_Growing_Link is
      Real : constant Path := Canonical (Test_Dir);
   begin
      Link (Test_Dir / "grow", "grow" / "a",
            Options => (Allow_Missing_Target => True, others => <>));

      Assert (AAA.Strings.Has_Prefix (Pseudocanonical (Test_Dir / "grow"),
                                      Real / "grow"));

      Unlink (Test_Dir / "grow");
   end Check_Growing_Link;

   ----------------------
   -- Check_Predicates --
   ----------------------

   --  Membership tests are for asking, so they cannot raise

   procedure Check_Predicates is
   begin
      Assert ("." not in Hard_Path);
      Assert (".." not in Hard_Path);
      Assert ("a" / ".." not in Hard_Path);
      Assert (Top / "." not in Hard_Path);

      Assert (Top in Hard_Path);
      Assert (("a" & Dir_Separator) not in Part);
      Assert (("a" & Dir_Separator) not in Path);
      Assert ("." not in Normal_Path);
      Assert ("." not in Canonical_Path);
   end Check_Predicates;

begin
   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   Check_Normal;
   Check_Too_Many_Parents;
   Check_Predicates;

   if Kind ("canary") = Softlink then
      Check_Absolute_Target;
      Check_Growing_Link;
   else
      Ada.Text_IO.Put_Line ("Skipping link tests as they are not supported");
   end if;

   Delete_Tree (Test_Dir);
end Den_Tests.Normalization;
