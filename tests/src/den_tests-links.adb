with Ada.Text_IO;

with Den.Filesystem;

pragma Warnings (Off);

procedure Den_Tests.Links is
   use Ada.Text_IO;
   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir  : constant Path := "links_test_dir";

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
   --  Skip this test if softlinks are not supported
   if Kind ("canary") /= Softlink then
      Put_Line ("Skipping tests as softlinks are not supported");
      return;
   end if;

   -- Create a temporary test directory
   Put_Line ("Creating temporary test directory: " & Test_Dir);
   Reset_Test_Dir;

   -- Test 1: Create a link to an existing file
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "link_to_file";
      Target_File : constant Path := Test_Dir / "target_file.txt";
      Link_Target : constant Path := Relative (Test_Dir, Target_File);
      --  Relative to parent of Target_File, to match Unix symlink semantics
   begin
      Put_Line ("Test 1: Create a link to an existing file");

      -- Check the relative path
      if Link_Target /= Simple_Name (Target_File) then
         Put_Line ("ERROR: Relative path is incorrect: " &
                   Link_Target & " instead of " &
                   Simple_Name (Target_File));
         raise Program_Error with "Relative path calculation failed";
      else
         Put_Line ("SUCCESS: Relative path is correct");
      end if;

      -- Create a target file
      Create_Test_File (Target_File);

      -- Verify that the target file exists
      if not Exists (Target_File) then
         Put_Line ("ERROR: Failed to create target file");
         raise Program_Error with "Target file creation failed";
      end if;

      -- Create a link to the target file
      Link (Link_Path, Link_Target);

      -- Verify that the link exists
      if not Exists (Target_File) then
         Put_Line ("ERROR: Link was not created");
         raise Program_Error with "Link creation failed";
      else
         Put_Line ("SUCCESS: Link was successfully created");
      end if;

      -- Verify that the link is a softlink
      if Kind (Link_Path) /= Softlink then
         Put_Line ("ERROR: Created link is not a softlink");
         raise Program_Error with "Created link is not a softlink";
      else
         Put_Line ("SUCCESS: Created link is a softlink");
      end if;

      -- Verify the link contents match what was asked for
      if Target (Link_Path) /= Link_Target then
         Put_Line ("ERROR: Link points to " & Target (Link_Path) &
                   " instead of " & Link_Target);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;

      -- Verify that the link points to the target file
      if Resolve (Link_Path) /= Target_File then
         Put_Line ("ERROR: Link points to " & Resolve (Link_Path) &
                   " instead of " & Target_File);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;
   end;

   -- Test 2: Create a link to an existing directory
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "link_to_dir";
      Target_Dir  : constant Path := Test_Dir / "target_dir";
      Link_Target : constant Path := Relative (Test_Dir, Target_Dir);
   begin
      Put_Line ("Test 2: Create a link to an existing directory");

      -- Check the relative path
      if Link_Target /= Simple_Name (Target_Dir) then
         Put_Line ("ERROR: Relative path is incorrect: " &
                   Link_Target & " instead of " &
                   Simple_Name (Target_Dir));
         raise Program_Error with "Relative path calculation failed";
      else
         Put_Line ("SUCCESS: Relative path is correct");
      end if;

      -- Create a target directory
      Create_Directory (Target_Dir);

      -- Verify that the target directory exists
      if not Exists (Target_Dir) then
         Put_Line ("ERROR: Failed to create target directory");
         raise Program_Error with "Target directory creation failed";
      end if;

      -- Create a link to the target directory
      Link (Link_Path, Link_Target);

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link was not created");
         raise Program_Error with "Link creation failed";
      else
         Put_Line ("SUCCESS: Link was successfully created");
      end if;

      -- Verify that the link is a softlink
      if Kind (Link_Path) /= Softlink then
         Put_Line ("ERROR: Created link is not a softlink");
         raise Program_Error with "Created link is not a softlink";
      else
         Put_Line ("SUCCESS: Created link is a softlink");
      end if;

      -- Verify the link contents match what was asked for
      if Target (Link_Path) /= Link_Target then
         Put_Line ("ERROR: Link points to " & Target (Link_Path) &
                   " instead of " & Link_Target);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;

      -- Verify that the link points to the target directory
      if Resolve (Link_Path) /= Target_Dir then
         Put_Line ("ERROR: Link points to " & Resolve (Link_Path) &
                   " instead of " & Target_Dir);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;
   end;

   -- Test 3: Create a link to a non-existent target with Allow_Missing_Target => True
   Reset_Test_Dir;
   declare
      Link_Path           : constant Path := Test_Dir / "link_to_missing";
      Non_Existent_Target : constant Path := Test_Dir / "non_existent_target";
      Link_Target         : constant Path := Relative (Test_Dir, Non_Existent_Target);
      Options             : constant Link_Options := (Allow_Missing_Target => True);
   begin
      Put_Line ("Test 3: Create a link to a non-existent target with " &
                "Allow_Missing_Target => True");

      -- Verify that the target does not exist
      if Exists (Non_Existent_Target) then
         Put_Line ("ERROR: Target unexpectedly exists");
         raise Program_Error with "Target unexpectedly exists";
      end if;

      -- Create a link to the non-existent target with Allow_Missing_Target => True
      Link (Link_Path, Link_Target, Options);

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link was not created");
         raise Program_Error with "Link creation failed";
      else
         Put_Line ("SUCCESS: Link was successfully created");
      end if;

      -- Verify that the link is a softlink
      if Kind (Link_Path) /= Softlink then
         Put_Line ("ERROR: Created link is not a softlink");
         raise Program_Error with "Created link is not a softlink";
      else
         Put_Line ("SUCCESS: Created link is a softlink");
      end if;

      -- Verify the link contents match what was asked for
      if Target (Link_Path) /= Link_Target then
         Put_Line ("ERROR: Link points to " & Target (Link_Path) &
                   " instead of " & Link_Target);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;

      -- Verify that the link is broken (not resolvable)
      if Is_Resolvable (Link_Path) then
         Put_Line ("ERROR: Link is unexpectedly resolvable");
         raise Program_Error with "Link is unexpectedly resolvable";
      else
         Put_Line ("SUCCESS: Link is correctly not resolvable");
      end if;
   end;

   -- Test 4: Create a link to a non-existent target with Allow_Missing_Target => False
   Reset_Test_Dir;
   declare
      Link_Path           : constant Path := Test_Dir / "link_to_missing";
      Non_Existent_Target : constant Path := Test_Dir / "non_existent_target";
      Link_Target         : constant Path := Relative (Test_Dir, Non_Existent_Target);
      Options             : constant Link_Options := (Allow_Missing_Target => False);
   begin
      Put_Line ("Test 4: Create a link to a non-existent target with " &
                "Allow_Missing_Target => False");

      -- Verify that the target does not exist
      if Exists (Non_Existent_Target) then
         Put_Line ("ERROR: Target unexpectedly exists");
         raise Program_Error with "Target unexpectedly exists";
      end if;

      -- Try to create a link to the non-existent target with Allow_Missing_Target => False
      -- This should raise an exception
      begin
         Link (Link_Path, Link_Target, Options);
         Put_Line ("ERROR: No exception raised when creating link to " &
                   "non-existent target with Allow_Missing_Target => False");
         raise Program_Error with "Link creation did not raise an exception";
      exception
         when others =>
            Put_Line ("SUCCESS: Exception raised when creating link to " &
                      "non-existent target with Allow_Missing_Target => False");
      end;

      -- Verify that the link was not created
      if Exists (Link_Path) then
         Put_Line ("ERROR: Link was unexpectedly created");
         raise Program_Error with "Link was unexpectedly created";
      else
         Put_Line ("SUCCESS: Link was not created");
      end if;
   end;

   -- Test 5: Create a link at a path that already exists
   Reset_Test_Dir;
   declare
      Target_File   : constant Path := Test_Dir / "target_file.txt";
      Existing_File : constant Path := Test_Dir / "existing_file.txt";
      Link_Target   : constant Path := Relative (Test_Dir, Target_File);
   begin
      Put_Line ("Test 5: Create a link at a path that already exists");

      -- Create a target file
      Create_Test_File (Target_File);

      -- Create an existing file at the intended link path
      Create_Test_File (Existing_File);

      -- Verify that both files exist
      if not Exists (Target_File) then
         Put_Line ("ERROR: Failed to create target file");
         raise Program_Error with "Target file creation failed";
      end if;

      if not Exists (Existing_File) then
         Put_Line ("ERROR: Failed to create existing file");
         raise Program_Error with "Existing file creation failed";
      end if;

      -- Try to create a link at the path of the existing file
      -- This should raise an exception
      begin
         Link (Existing_File, Link_Target);
         Put_Line ("ERROR: No exception raised when creating link at " &
                   "a path that already exists");
         raise Program_Error with "Link creation did not raise an exception";
      exception
         when others =>
            Put_Line ("SUCCESS: Exception raised when creating link at " &
                      "a path that already exists");
      end;

      -- Verify that the existing file still exists and was not replaced by a link
      if not Exists (Existing_File) then
         Put_Line ("ERROR: Existing file was unexpectedly deleted");
         raise Program_Error with "Existing file was unexpectedly deleted";
      else
         Put_Line ("SUCCESS: Existing file still exists");
      end if;

      -- Verify that the existing file is still a regular file
      if Kind (Existing_File) /= File then
         Put_Line ("ERROR: Existing file was unexpectedly changed to a " &
                   Kind (Existing_File)'Image);
         raise Program_Error with "Existing file was unexpectedly changed";
      else
         Put_Line ("SUCCESS: Existing file is still a regular file");
      end if;
   end;

   -- Test 6: Create a link with an absolute target
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "link_to_abs_file";
      Target_File : constant Path := Test_Dir / "abs_target_file.txt";
      Abs_Target  : constant Path := Absolute (Target_File);
   begin
      Put_Line ("Test 6: Create a link with an absolute target");

      -- Create a target file
      Create_Test_File (Target_File);

      -- Verify that the target file exists
      if not Exists (Target_File) then
         Put_Line ("ERROR: Failed to create target file");
         raise Program_Error with "Target file creation failed";
      end if;

      -- Create a link to the target file using an absolute path
      Link (Link_Path, Abs_Target);

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link was not created");
         raise Program_Error with "Link creation failed";
      else
         Put_Line ("SUCCESS: Link was successfully created");
      end if;

      -- Verify that the link is a softlink
      if Kind (Link_Path) /= Softlink then
         Put_Line ("ERROR: Created link is not a softlink");
         raise Program_Error with "Created link is not a softlink";
      else
         Put_Line ("SUCCESS: Created link is a softlink");
      end if;

      -- Verify the link contents match what was asked for
      if Target (Link_Path) /= Abs_Target then
         Put_Line ("ERROR: Link points to " & Target (Link_Path) &
                   " instead of " & Abs_Target);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;

      -- Verify that the link points to the target file
      if Resolve (Link_Path) /= Abs_Target then
         Put_Line ("ERROR: Link points to " & Resolve (Link_Path) &
                   " instead of " & Abs_Target);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;
   end;

   -- Test 7: Create a link with a non-existent absolute target
   Reset_Test_Dir;
   declare
      Link_Path           : constant Path := Test_Dir / "link_to_abs_missing";
      Non_Existent_Target : constant Path := Test_Dir / "abs_non_existent_target";
      Abs_Target          : constant Path := Absolute (Non_Existent_Target);
      Options             : constant Link_Options := (Allow_Missing_Target => True);
   begin
      Put_Line ("Test 7: Create a link with a non-existent absolute target");

      -- Verify that the target does not exist
      if Exists (Non_Existent_Target) then
         Put_Line ("ERROR: Target unexpectedly exists");
         raise Program_Error with "Target unexpectedly exists";
      end if;

      -- Create a link to the non-existent target with Allow_Missing_Target => True
      Link (Link_Path, Abs_Target, Options);

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link was not created");
         raise Program_Error with "Link creation failed";
      else
         Put_Line ("SUCCESS: Link was successfully created");
      end if;

      -- Verify that the link is a softlink
      if Kind (Link_Path) /= Softlink then
         Put_Line ("ERROR: Created link is not a softlink");
         raise Program_Error with "Created link is not a softlink";
      else
         Put_Line ("SUCCESS: Created link is a softlink");
      end if;

      -- Verify the link contents match what was asked for
      if Target (Link_Path) /= Abs_Target then
         Put_Line ("ERROR: Link points to " & Target (Link_Path) &
                   " instead of " & Abs_Target);
         raise Program_Error with "Link points to the wrong target";
      else
         Put_Line ("SUCCESS: Link points to the correct target");
      end if;

      -- Verify that the link is broken (not resolvable)
      if Is_Resolvable (Link_Path) then
         Put_Line ("ERROR: Link is unexpectedly resolvable");
         raise Program_Error with "Link is unexpectedly resolvable";
      else
         Put_Line ("SUCCESS: Link is correctly not resolvable");
      end if;
   end;

   -- Clean up
   Put_Line ("Cleaning up test directory");
   Delete_Tree (Test_Dir);

   Put_Line ("Link tests completed successfully");
end Den_Tests.Links;
