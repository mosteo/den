with Ada.Text_IO;

with Den.Filesystem;

pragma Warnings (Off);

procedure Den_Tests.Relative_Function is
   use Ada.Text_IO;
   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir : constant Path := "relative_test_dir";

   ----------------------
   -- Create_Test_File --
   ----------------------

   procedure Create_Test_File (File_Path : Path) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, File_Path);
      Ada.Text_IO.Put_Line (F, "This is a test file");
      Ada.Text_IO.Close (F);
   end Create_Test_File;

   ------------------
   -- Reset_Test_Dir --
   ------------------

   procedure Reset_Test_Dir is
   begin
      -- Ensure the directory doesn't exist (in case of previous test failure)
      if Exists (Test_Dir) then
         Delete_Tree (Test_Dir);
      end if;

      -- Create the test directory
      Create_Directory (Test_Dir);
   end Reset_Test_Dir;

begin
   -- Create a temporary test directory
   Put_Line ("Creating temporary test directory: " & Test_Dir);
   Reset_Test_Dir;

   -- Test 1: Relative path between existing directories at the same level
   Reset_Test_Dir;
   declare
      Dir1 : constant Path := Test_Dir / "dir1";
      Dir2 : constant Path := Test_Dir / "dir2";
   begin
      Put_Line ("Test 1: Relative path between existing directories " &
                "at the same level");

      -- Create test directories
      Create_Directory (Dir1);
      Create_Directory (Dir2);

      -- Get relative path from Dir1 to Dir2
      declare
         Rel_Path : constant Path := Relative (Dir1, Dir2);
      begin
         -- Check that Rel_Path equals ".." / "dir2"
         if Rel_Path /= ".." / "dir2" then
            Put_Line ("ERROR: Incorrect relative path: " & Rel_Path &
                      " instead of " & ".." / "dir2");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path is correct: " & Rel_Path);
         end if;
      end;
   end;

   -- Test 2: Relative path between existing nested directories
   Reset_Test_Dir;
   declare
      Parent_Dir : constant Path := Test_Dir / "parent";
      Child_Dir  : constant Path := Parent_Dir / "child";
   begin
      Put_Line ("Test 2: Relative path between existing nested directories");

      -- Create test directories
      Create_Directory (Parent_Dir);
      Create_Directory (Child_Dir);

      -- Get relative path from parent to child
      declare
         Rel_Path1 : constant Path := Relative (Parent_Dir, Child_Dir);
      begin
         -- Check that Rel_Path1 equals "child"
         if Rel_Path1 /= "child" then
            Put_Line ("ERROR: Incorrect relative path from parent to child: " &
                      Rel_Path1 & " instead of " & "child");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path from parent to child is correct: " &
                      Rel_Path1);
         end if;
      end;

      -- Get relative path from child to parent
      declare
         Rel_Path2 : constant Path := Relative (Child_Dir, Parent_Dir);
      begin
         -- Check that Rel_Path2 equals ".."
         if Rel_Path2 /= ".." then
            Put_Line ("ERROR: Incorrect relative path from child to parent: " &
                      Rel_Path2 & " instead of " & "..");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path from child to parent is correct: " &
                      Rel_Path2);
         end if;
      end;
   end;

   -- Test 3: Relative path between existing directories with common ancestor
   Reset_Test_Dir;
   declare
      Common_Dir : constant Path := Test_Dir / "common";
      Branch1    : constant Path := Common_Dir / "branch1";
      Branch2    : constant Path := Common_Dir / "branch2";
      Leaf1      : constant Path := Branch1 / "leaf1";
      Leaf2      : constant Path := Branch2 / "leaf2";
   begin
      Put_Line ("Test 3: Relative path between existing directories " &
                "with common ancestor");

      -- Create test directories
      Create_Directory (Common_Dir);
      Create_Directory (Branch1);
      Create_Directory (Branch2);
      Create_Directory (Leaf1);
      Create_Directory (Leaf2);

      -- Get relative path from Leaf1 to Leaf2
      declare
         Rel_Path : constant Path := Relative (Leaf1, Leaf2);
      begin
         -- Check that Rel_Path equals ".." / ".." / "branch2" / "leaf2"
         if Rel_Path /= ".." / ".." / "branch2" / "leaf2" then
            Put_Line ("ERROR: Incorrect relative path: " & Rel_Path &
                      " instead of " & ".." / ".." / "branch2" / "leaf2");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path is correct: " & Rel_Path);
         end if;
      end;
   end;

   -- Test 4: Relative path with Canonicalize = True
   Reset_Test_Dir;
   declare
      Dir1      : constant Path := Test_Dir / "dir1";
      Dir2      : constant Path := Test_Dir / "dir2";
      Link_Dir  : constant Path := Test_Dir / "link_to_dir2";
   begin
      Put_Line ("Test 4: Relative path with Canonicalize = True");

      -- Create test directories
      Create_Directory (Dir1);
      Create_Directory (Dir2);

      -- Create a symbolic link if supported
      if Kind ("canary") = Softlink then
         -- Create a link from link_to_dir2 to dir2
         Link (Link_Dir, "dir2");

         -- Get relative path from Dir1 to Link_Dir with Canonicalize = False
         declare
            Rel_Path1 : constant Path := Relative (Dir1, Link_Dir);
         begin
         -- Check that Rel_Path1 equals ".." / "link_to_dir2"
         if Rel_Path1 /= ".." / "link_to_dir2" then
            Put_Line ("ERROR: Incorrect relative path with Canonicalize = False: " &
                      Rel_Path1 & " instead of " & ".." / "link_to_dir2");
               raise Program_Error with "Relative path calculation failed";
            else
               Put_Line ("SUCCESS: Relative path with Canonicalize = False is correct: " &
                         Rel_Path1);
            end if;
         end;

         -- Get relative path from Dir1 to Link_Dir with Canonicalize = True
         declare
            Rel_Path2 : constant Path := Relative (Dir1, Link_Dir,
                                                  Canonicalize => True);
         begin
         -- Check that Rel_Path2 equals ".." / "dir2"
         if Rel_Path2 /= ".." / "dir2" then
            Put_Line ("ERROR: Incorrect relative path with Canonicalize = True: " &
                      Rel_Path2 & " instead of " & ".." / "dir2");
               raise Program_Error with "Relative path calculation failed";
            else
               Put_Line ("SUCCESS: Relative path with Canonicalize = True is correct: " &
                         Rel_Path2);
            end if;
         end;
      else
         Put_Line ("Skipping symlink tests as they are not supported");
      end if;
   end;

   -- Test 5: Relative path to non-existing paths
   Reset_Test_Dir;
   declare
      Dir1             : constant Path := Test_Dir / "dir1";
      Non_Existent_Dir : constant Path := Test_Dir / "non_existent";
   begin
      Put_Line ("Test 5: Relative path to non-existing paths");

      -- Create test directory
      Create_Directory (Dir1);

      -- Get relative path from Dir1 to Non_Existent_Dir
      declare
         Rel_Path : constant Path := Relative (Dir1, Non_Existent_Dir);
      begin
         -- Check that Rel_Path equals ".." / "non_existent"
         if Rel_Path /= ".." / "non_existent" then
            Put_Line ("ERROR: Incorrect relative path to non-existing path: " &
                      Rel_Path & " instead of " & ".." / "non_existent");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path to non-existing path is correct: " &
                      Rel_Path);
         end if;
      end;
   end;

   -- Test 6: Relative path from non-existing paths
   Reset_Test_Dir;
   declare
      Dir1             : constant Path := Test_Dir / "dir1";
      Non_Existent_Dir : constant Path := Test_Dir / "non_existent";
   begin
      Put_Line ("Test 6: Relative path from non-existing paths");

      -- Create test directory
      Create_Directory (Dir1);

      -- Get relative path from Non_Existent_Dir to Dir1
      declare
         Rel_Path : constant Path := Relative (Non_Existent_Dir, Dir1);
      begin
         -- Check that Rel_Path equals ".." / "dir1"
         if Rel_Path /= ".." / "dir1" then
            Put_Line ("ERROR: Incorrect relative path from non-existing path: " &
                      Rel_Path & " instead of " & ".." / "dir1");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path from non-existing path is correct: " &
                      Rel_Path);
         end if;
      end;
   end;

   -- Test 7: Relative path between absolute paths
   declare
      Abs_Path1 : constant Path := Absolute (Current_Dir / Test_Dir / "dir1");
      Abs_Path2 : constant Path := Absolute (Current_Dir / Test_Dir / "dir2");
   begin
      Put_Line ("Test 7: Relative path between absolute paths");

      -- Create test directories
      Reset_Test_Dir;
      Create_Directory (Test_Dir / "dir1");
      Create_Directory (Test_Dir / "dir2");

      -- Get relative path from Abs_Path1 to Abs_Path2
      declare
         Rel_Path : constant Path := Relative (Abs_Path1, Abs_Path2);
      begin
         -- Check that Rel_Path equals ".." / "dir2"
         if Rel_Path /= ".." / "dir2" then
            Put_Line ("ERROR: Incorrect relative path between absolute paths: " &
                      Rel_Path & " instead of " & ".." / "dir2");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path between absolute paths is correct: " &
                      Rel_Path);
         end if;
      end;
   end;

   -- Test 8: Relative path with complex paths containing ".." and "."
   Reset_Test_Dir;
   declare
      Dir1     : constant Path := Test_Dir / "dir1";
      Dir2     : constant Path := Test_Dir / "dir2";
      Complex1 : constant Path := Dir1 / ".." / "dir1" / "." / "subdir";
      Complex2 : constant Path := Dir2 / ".." / "dir2" / "." / "subdir";
   begin
      Put_Line ("Test 8: Relative path with complex paths containing " &
                "'..' and '.'");

      -- Create test directories
      Create_Directory (Dir1);
      Create_Directory (Dir2);
      Create_Directory (Dir1 / "subdir");
      Create_Directory (Dir2 / "subdir");

      -- Get relative path from Complex1 to Complex2
      declare
         Rel_Path : constant Path := Relative (Complex1, Complex2);
      begin
         -- Check that Rel_Path equals ".." / ".." / "dir2" / "subdir"
         if Rel_Path /= ".." / ".." / "dir2" / "subdir" then
            Put_Line ("ERROR: Incorrect relative path with complex paths: " &
                      Rel_Path & " instead of " & ".." / ".." / "dir2" / "subdir");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path with complex paths is correct: " &
                      Rel_Path);
         end if;
      end;
   end;

   -- Test 9: Relative path to a file (not a directory)
   Reset_Test_Dir;
   declare
      Dir1      : constant Path := Test_Dir / "dir1";
      Dir2      : constant Path := Test_Dir / "dir2";
      File_Path : constant Path := Dir2 / "test_file.txt";
   begin
      Put_Line ("Test 9: Relative path to a file (not a directory)");

      -- Create test directories and file
      Create_Directory (Dir1);
      Create_Directory (Dir2);
      Create_Test_File (File_Path);

      -- Get relative path from Dir1 to File_Path (from "uncle" directory)
      declare
         Rel_Path : constant Path := Relative (Dir1, File_Path);
      begin
         -- Check that Rel_Path equals ".." / "dir2" / "test_file.txt"
         if Rel_Path /= ".." / "dir2" / "test_file.txt" then
            Put_Line ("ERROR: Incorrect relative path to file from uncle dir: " &
                      Rel_Path & " instead of " & ".." / "dir2" / "test_file.txt");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path to file from uncle dir is correct: " &
                      Rel_Path);
         end if;
      end;

      -- Get relative path from parent directory to file
      declare
         Rel_Path : constant Path := Relative (Dir2, File_Path);
      begin
         -- Check that Rel_Path equals "test_file.txt"
         if Rel_Path /= "test_file.txt" then
            Put_Line ("ERROR: Incorrect relative path to file from parent dir: " &
                      Rel_Path & " instead of " & "test_file.txt");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path to file from parent dir is correct: " &
                      Rel_Path);
         end if;
      end;

      -- Get relative path from grandparent directory to file
      declare
         Rel_Path : constant Path := Relative (Test_Dir, File_Path);
      begin
         -- Check that Rel_Path equals "dir2" / "test_file.txt"
         if Rel_Path /= "dir2" / "test_file.txt" then
            Put_Line ("ERROR: Incorrect relative path to file from grandparent dir: " &
                      Rel_Path & " instead of " & "dir2" / "test_file.txt");
            raise Program_Error with "Relative path calculation failed";
         else
            Put_Line ("SUCCESS: Relative path to file from grandparent dir is correct: " &
                      Rel_Path);
         end if;
      end;
   end;

   -- Test 10: Relative path when the first element is a file (should be rejected)
   Reset_Test_Dir;
   declare
      Dir1      : constant Path := Test_Dir / "dir1";
      File_Path : constant Path := Test_Dir / "test_file.txt";
      Exception_Raised : Boolean := False;
   begin
      Put_Line ("Test 10: Relative path when the first element is a file " &
                "(should be rejected)");

      -- Create test directory and file
      Create_Directory (Dir1);
      Create_Test_File (File_Path);

      -- Try to get relative path from File_Path to Dir1
      -- This should raise a Use_Error exception
      begin
         declare
            Rel_Path : constant Path := Relative (File_Path, Dir1);
         begin
            Put_Line ("ERROR: No exception raised when using a file as " &
                      "the first parameter. Got path: " & Rel_Path);
            raise Program_Error with "Relative path calculation did not fail as expected";
         end;
      exception
         when Use_Error =>
            Exception_Raised := True;
            Put_Line ("SUCCESS: Use_Error exception raised as expected " &
                      "when using a file as the first parameter");
         when others =>
            Put_Line ("ERROR: Wrong exception type raised when using a file " &
                      "as the first parameter");
            raise Program_Error with "Wrong exception type raised";
      end;

      -- Verify that the exception was raised
      if not Exception_Raised then
         Put_Line ("ERROR: No exception raised when using a file as " &
                   "the first parameter");
         raise Program_Error with "No exception raised";
      end if;
   end;

   -- Clean up
   Put_Line ("Cleaning up test directory");
   Delete_Tree (Test_Dir);

   Put_Line ("Relative path tests completed successfully");
end Den_Tests.Relative_Function;
