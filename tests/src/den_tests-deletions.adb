with Ada.Text_IO;

with Den.Filesystem;

procedure Den_Tests.Deletions is
   use Ada.Text_IO;
   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir  : constant Path := "deletions_test_dir";
   Cases_Dir : constant Path := ".." / "cases"; -- Relative to tests directory

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

      -- Copy the cases directory to our test directory
      Put_Line ("Copying cases directory to test directory");
      Copy (Cases_Dir, Test_Dir, (Merge_Dirs => False, others => <>));
   end Reset_Test_Dir;

begin
   -- Create a temporary test directory
   Put_Line ("Creating temporary test directory: " & Test_Dir);
   Reset_Test_Dir;

   -- Test 1: Delete a regular file
   Put_Line ("Test 1: Delete a regular file");
   Reset_Test_Dir;

   declare
      Test_File : constant Path := Test_Dir / "regular_file.txt";
   begin
      Put_Line ("Test 1: Delete a regular file");

      -- Create a regular file
      declare
         F : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Test_File);
         Ada.Text_IO.Put_Line (F, "This is a test file");
         Ada.Text_IO.Close (F);
      end;

      -- Verify that the file exists
      if not Exists (Test_File) then
         Put_Line ("ERROR: Failed to create test file");
         raise Program_Error with "Test file creation failed";
      end if;

      -- Delete the file using Delete_File
      Delete_File (Test_File);

      -- Verify that the file no longer exists
      if Exists (Test_File) then
         Put_Line ("ERROR: File still exists after Delete_File");
         raise Program_Error with "Delete_File failed to delete the file";
      else
         Put_Line ("SUCCESS: File was successfully deleted");
      end if;
   end;

   -- Test 2: Unlink a symbolic link
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "links" / "b";
      Target_Path : constant Path := Test_Dir / "links" / "a";
   begin
      Put_Line ("Test 2: Unlink a symbolic link");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & String (Link_Path));
         raise Program_Error with "Link does not exist";
      end if;

      -- Verify that the target exists
      if not Exists (Target_Path) then
         Put_Line ("ERROR: Target does not exist: " & String (Target_Path));
         raise Program_Error with "Target does not exist";
      end if;

      -- Unlink the symbolic link
      Unlink (Link_Path);

      -- Verify that the link no longer exists
      if Exists (Link_Path) then
         Put_Line ("ERROR: Link still exists after Unlink");
         raise Program_Error with "Unlink failed to remove the link";
      else
         Put_Line ("SUCCESS: Link was successfully unlinked");
      end if;

      -- Verify that the target still exists
      if not Exists (Target_Path) then
         Put_Line ("ERROR: Target was deleted when unlinking the link");
         raise Program_Error with "Target was unexpectedly deleted";
      else
         Put_Line ("SUCCESS: Target still exists after unlinking the link");
      end if;
   end;

   -- Test 3: Delete a symbolic link's target using Delete_File with
   -- Delete_Target option
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "links" / "b";
      Target_Path : constant Path := Test_Dir / "links" / "a";
      Options     : constant Delete_File_Options :=
        (Delete_Softlinks => Delete_Target, others => <>);
   begin
      Put_Line
        ("Test 3: Delete a symbolic link's target using Delete_File with Delete_Target option");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & Link_Path);
         raise Program_Error with "Link does not exist";
      end if;

      -- Verify that the target exists
      if not Exists (Target_Path) then
         Put_Line ("ERROR: Target does not exist: " & Target_Path);
         raise Program_Error with "Target does not exist";
      end if;

      -- Delete the link's target using Delete_File with Delete_Target option
      Delete_File (Link_Path, Options);

      -- Verify that the link still exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link was deleted when using Delete_Target option");
         raise Program_Error with "Link was unexpectedly deleted";
      else
         Put_Line ("SUCCESS: Link still exists after deleting its target");
      end if;

      -- Verify that the target no longer exists
      if Exists (Target_Path) then
         Put_Line
           ("ERROR: Target still exists after Delete_File with Delete_Target option");
         raise Program_Error
           with
             "Delete_File with Delete_Target option failed to delete the target";
      else
         Put_Line ("SUCCESS: Target was successfully deleted");
      end if;
   end;

   -- Test 4: Delete both a symbolic link and its target using Delete_File with Delete_Both option
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "links" / "b";
      Target_Path : constant Path := Test_Dir / "links" / "a";
      Options     : constant Delete_File_Options :=
        (Delete_Softlinks => Delete_Both, others => <>);
   begin
      Put_Line
        ("Test 4: Delete both a symbolic link and its target using Delete_File with Delete_Both option");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & String (Link_Path));
         raise Program_Error with "Link does not exist";
      end if;

      -- Verify that the target exists
      if not Exists (Target_Path) then
         Put_Line ("ERROR: Target does not exist: " & String (Target_Path));
         raise Program_Error with "Target does not exist";
      end if;

      -- Delete both the link and its target using Delete_File with Delete_Both option
      Delete_File (Link_Path, Options);

      -- Verify that the link no longer exists
      if Exists (Link_Path) then
         Put_Line
           ("ERROR: Link still exists after Delete_File with Delete_Both option");
         raise Program_Error
           with
             "Delete_File with Delete_Both option failed to delete the link";
      else
         Put_Line ("SUCCESS: Link was successfully deleted");
      end if;

      -- Verify that the target no longer exists
      if Exists (Target_Path) then
         Put_Line
           ("ERROR: Target still exists after Delete_File with Delete_Both option");
         raise Program_Error
           with
             "Delete_File with Delete_Both option failed to delete the target";
      else
         Put_Line ("SUCCESS: Target was successfully deleted");
      end if;
   end;

   -- Test 5: Test Delete_File with Do_Not_Fail option on broken links
   Reset_Test_Dir;
   declare
      -- We'll use a broken link 'e' which points to a non-existent target 'missing'
      Link_Path : constant Path := Test_Dir / "links" / "e";
      Options   : constant Delete_File_Options :=
        (Delete_Softlinks => Delete_Target, Do_Not_Fail => True);
   begin
      Put_Line
        ("Test 5: Test Delete_File with Do_Not_Fail option on broken links");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & String (Link_Path));
         raise Program_Error with "Link does not exist";
      end if;

      -- Try to delete the target of a broken link with Do_Not_Fail option
      -- This should not raise an exception
      begin
         Delete_File (Link_Path, Options);
         Put_Line
           ("SUCCESS: No exception raised when deleting target of broken link with Do_Not_Fail option");
      exception
         when others =>
            Put_Line
              ("ERROR: Exception raised when deleting target of broken link with Do_Not_Fail option");
            raise Program_Error
              with "Delete_File with Do_Not_Fail option raised an exception";
      end;

      -- Verify that the link still exists (since we only tried to delete the target)
      if not Exists (Link_Path) then
         Put_Line
           ("ERROR: Link was deleted when using Delete_Target option on broken link");
         raise Program_Error with "Link was unexpectedly deleted";
      else
         Put_Line
           ("SUCCESS: Link still exists after attempting to delete its target");
      end if;
   end;

   -- Test 6: Unlink a self-referential symbolic link
   Reset_Test_Dir;
   declare
      Link_Path : constant Path := Test_Dir / "links" / "d";
   begin
      Put_Line ("Test 6: Unlink a self-referential symbolic link");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & String (Link_Path));
         raise Program_Error with "Link does not exist";
      end if;

      -- Unlink the self-referential symbolic link
      Unlink (Link_Path);

      -- Verify that the link no longer exists
      if Exists (Link_Path) then
         Put_Line ("ERROR: Link still exists after Unlink");
         raise Program_Error with "Unlink failed to remove the self-referential link";
      else
         Put_Line ("SUCCESS: Self-referential link was successfully unlinked");
      end if;
   end;

   -- Test 7: Unlink a symbolic link to a directory
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "links" / "i";
      Target_Path : constant Path := Test_Dir / "links" / "g";
   begin
      Put_Line ("Test 7: Unlink a symbolic link to a directory");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & String (Link_Path));
         raise Program_Error with "Link does not exist";
      end if;

      -- Verify that the target directory exists
      if not Exists (Target_Path) then
         Put_Line ("ERROR: Target directory does not exist: " & String (Target_Path));
         raise Program_Error with "Target directory does not exist";
      end if;

      -- Unlink the symbolic link to a directory
      Unlink (Link_Path);

      -- Verify that the link no longer exists
      if Exists (Link_Path) then
         Put_Line ("ERROR: Link still exists after Unlink");
         raise Program_Error with "Unlink failed to remove the link to directory";
      else
         Put_Line ("SUCCESS: Link to directory was successfully unlinked");
      end if;

      -- Verify that the target directory still exists
      if not Exists (Target_Path) then
         Put_Line ("ERROR: Target directory was deleted when unlinking the link");
         raise Program_Error with "Target directory was unexpectedly deleted";
      else
         Put_Line ("SUCCESS: Target directory still exists after unlinking the link");
      end if;
   end;

   -- Test 8: Test Delete_File with Fail option on broken links
   Reset_Test_Dir;
   declare
      -- We'll use a broken link 'f' which points to a non-existent path 'not/found'
      Link_Path : constant Path := Test_Dir / "links" / "f";
      Options   : constant Delete_File_Options :=
        (Delete_Softlinks => Fail, others => <>);
   begin
      Put_Line
        ("Test 8: Test Delete_File with Fail option on broken links");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & String (Link_Path));
         raise Program_Error with "Link does not exist";
      end if;

      -- Try to delete a broken link with Fail option
      -- This should raise an exception
      begin
         Delete_File (Link_Path, Options);
         Put_Line
           ("ERROR: No exception raised when deleting broken link with Fail option");
         raise Program_Error
           with "Delete_File with Fail option did not raise an exception";
      exception
         when others =>
            Put_Line
              ("SUCCESS: Exception raised when deleting broken link with Fail option");
      end;

      -- Verify that the link still exists (since the operation failed)
      if not Exists (Link_Path) then
         Put_Line
           ("ERROR: Link was deleted despite exception with Fail option");
         raise Program_Error with "Link was unexpectedly deleted";
      else
         Put_Line
           ("SUCCESS: Link still exists after failed Delete_File operation");
      end if;
   end;

   -- Test 9: Delete a symbolic link to a directory using Delete_File with Delete_Link option
   Reset_Test_Dir;
   declare
      Link_Path   : constant Path := Test_Dir / "links" / "i";
      Target_Path : constant Path := Test_Dir / "links" / "g";
      Options     : constant Delete_File_Options :=
        (Delete_Softlinks => Delete_Link, others => <>);
   begin
      Put_Line
        ("Test 9: Delete a symbolic link to a directory using Delete_File with Delete_Link option");

      -- Verify that the link exists
      if not Exists (Link_Path) then
         Put_Line ("ERROR: Link does not exist: " & String (Link_Path));
         raise Program_Error with "Link does not exist";
      end if;

      -- Verify that the target directory exists
      if not Exists (Target_Path) then
         Put_Line ("ERROR: Target directory does not exist: " & String (Target_Path));
         raise Program_Error with "Target directory does not exist";
      end if;

      -- Delete the link using Delete_File with Delete_Link option
      Delete_File (Link_Path, Options);

      -- Verify that the link no longer exists
      if Exists (Link_Path) then
         Put_Line
           ("ERROR: Link still exists after Delete_File with Delete_Link option");
         raise Program_Error
           with "Delete_File with Delete_Link option failed to delete the link";
      else
         Put_Line ("SUCCESS: Link was successfully deleted");
      end if;

      -- Verify that the target directory still exists
      if not Exists (Target_Path) then
         Put_Line
           ("ERROR: Target directory was deleted when using Delete_Link option");
         raise Program_Error with "Target directory was unexpectedly deleted";
      else
         Put_Line
           ("SUCCESS: Target directory still exists after deleting the link");
      end if;
   end;

   -- Clean up
   Put_Line ("Cleaning up test directory");
   Delete_Tree (Test_Dir);

   Put_Line ("Delete_File and Unlink tests completed successfully");
end Den_Tests.Deletions;