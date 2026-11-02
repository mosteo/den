pragma Alire_Test;

with AAA.Strings;

with Ada.Environment_Variables;
with Ada.Text_IO;

with Den.Du;
with Den.Filesystem;
with Den.Iterators;
with Den.Walk;

with GNAT.OS_Lib;

procedure Den_Tests.Windows is

   --  Behavior that only exists on Windows

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir : constant Path := "windows_test_dir";

   -----------
   -- Touch --
   -----------

   procedure Touch (File_Path : Path) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, File_Path);
      Ada.Text_IO.Close (F);
   end Touch;

   -------------------------
   -- Check_Slashed_Target --
   -------------------------

   --  Targets are stored as given, and a stored '/' is not a separator for
   --  Windows, so such a link would be broken unless it is converted.

   procedure Check_Slashed_Target is
      Real     : constant Path := Test_Dir / "sub" / "file";
      The_Link : constant Path := Test_Dir / "slashed";
   begin
      Create_Directory (Parent (Real));
      Touch (Real);

      Link (The_Link, "sub/file");

      Assert (Kind (The_Link) = Softlink);
      Assert (Target (The_Link) = "sub\file");
      Assert (Canonical (The_Link) = Canonical (Real));
   end Check_Slashed_Target;

   --------------------------
   -- Check_Classification --
   --------------------------

   procedure Check_Classification is
      Drive : constant String := CWD (CWD'First .. CWD'First + 1);
   begin
      --  These depend on the current drive or directory

      Assert (not Is_Absolute ("\foo"));
      Assert (not Is_Absolute ("/foo"));
      Assert (not Is_Absolute ("C:foo"));
      Assert (not Is_Absolute ("C:"));
      Assert (not Is_Root ("C:"));
      Assert (Absolute ("\foo") = Root (CWD) & "foo");
      Assert (Absolute (Drive & "foo") = CWD / "foo");

      --  These don't

      Assert (Is_Absolute ("C:\foo"));
      Assert (Is_Absolute ("C:/foo"));
      Assert (Is_Absolute ("\\srv\share\x"));
      Assert (Is_Root ("C:\"));
      Assert (Is_Root ("C:/"));
      Assert (Is_Root ("\\srv\share"));
      Assert (Is_Root ("\\srv\share\"));

      Assert (Root ("C:/foo") = "C:\");
      Assert (Root ("\\srv\share\x") = "\\srv\share\");

      --  Only plain relative paths can be appended

      begin
         if "x" / "\foo" /= "" then
            raise Program_Error with "Appended a rooted path";
         end if;
      exception
         when Assertion_Error =>
            null;
      end;

      Assert ("C:\" / "y" = "C:\y");
      Assert ("C:/" / "y" = "C:/y");
      Assert ("C:" / "y" = "C:y");
      Assert ("\\srv\share" / "y" = "\\srv\share\y");
   end Check_Classification;

   -----------------
   -- Check_Scrub --
   -----------------

   procedure Check_Scrub is
   begin
      Assert (Scrub ("c:\") = "C:\");
      Assert (Scrub ("c:\foo") = "C:\foo");
      Assert (Scrub ("C:\foo\") = "C:\foo");

      --  Forward slashes are left alone, as git produces such paths

      Assert (Scrub ("c:/foo") = "C:/foo");
      Assert (Scrub ("foo/bar") = "foo/bar");
   end Check_Scrub;

   ---------------------------
   -- Check_Forward_Slashes --
   ---------------------------

   --  Paths from git use these, and Den must understand them as the OS does

   procedure Check_Forward_Slashes is
      Real    : constant Path := Absnormal (Test_Dir / "slashes");
      Forward : constant Path := AAA.Strings.Replace (Real, "\", "/");
   begin
      Assert ("a/b" not in Part);
      Assert (Name ("C:/x/y") = "y");
      Assert (Has_Parent ("x/y"));
      Assert (Parent ("x/y") = "x");
      Assert (Parent ("C:/x/y") = "C:\x");
      Assert (Parent ("C:/x") = "C:\");
      Assert (Normal ("a/b/../c") = "a\c");
      Assert (Relative ("C:/a/b", "C:/a/c") = "..\c");

      Create_Directory (Forward);
      Assert (Kind (Real) = Directory);

      Touch (Real / "file");
      Assert (Kind (Forward & "/file") = File);
      Assert (Canonical (Forward & "/file") = Canonical (Real / "file"));
      Assert (Parent (Forward & "/file") = Real);
      Assert (Relative (Forward, Forward & "/file") = "file");
      Assert (Natural (Walk.Find (Forward).Length) = 1);

      Copy (Forward & "/file", Forward & "/copy");
      Assert (Kind (Real / "copy") = File);

      Delete_Tree (Forward);
      Assert (Kind (Real) = Nothing);
   end Check_Forward_Slashes;

   --------------------
   -- Check_Relative --
   --------------------

   procedure Check_Relative is
   begin
      --  Drive letters are the same in any case

      Assert (Relative ("C:\a\b", "c:\a\c") = "..\c");
      begin
         Assert (Relative ("C:\a", "c:\a") = "");
         raise Program_Error with "Unexpected pass";
      exception
         when Bad_Operation =>
            null;
      end;

      --  Nothing relative across drives or network locations

      Assert (Relative ("C:\a", "D:\b") = "D:\b");
      Assert (Relative ("\\srv\sh\a", "\\srv\other\b") = "\\srv\other\b");
      Assert (Relative ("\\srv1\sh\a", "\\srv2\sh\b") = "\\srv2\sh\b");

      Assert (Relative ("\\srv\sh\a", "\\srv\sh\b") = "..\b");
   end Check_Relative;

   ---------------
   -- Check_UNC --
   ---------------

   procedure Check_UNC is
      use AAA.Strings;

      Share : constant Path := "\\localhost\C$\Windows";
   begin
      if Kind (Share) /= Directory then
         --  This must not go unnoticed where it should be working
         Assert (not Ada.Environment_Variables.Exists ("GITHUB_ACTIONS"),
                 "Administrative share not available in CI");
         Ada.Text_IO.Put_Line ("Skipping as the share is not available");
         return;
      end if;

      Assert (To_Lower_Case (Canonical (Share)) = To_Lower_Case (Share));
      Assert (Kind (Root (Share)) = Directory);
      Assert (Root (Share) = "\\localhost\C$\");
      Assert (Parent (Share) = "\\localhost\C$\");
      Assert (Name (Share) = "Windows");
      Assert (not Walk.Ls (Share).Is_Empty);
   end Check_UNC;

   ----------------------
   -- Check_Long_Paths --
   ----------------------

   --  Paths over the limit of 260 characters of the Windows API without the
   --  verbatim prefix, which works in every operation and with either separator

   procedure Check_Long_Paths (Top : Path) is
      use AAA.Strings;

      function Deep (Levels : Positive) return Path
      is (if Levels = 1
          then Top / "a_quite_long_folder_name_01"
          else Deep (Levels - 1) / "a_quite_long_folder_name_01");

      Dir  : constant Path := Deep (12);
      File : constant Path := Dir / "file";

      Slashed : constant String := Replace (File, "\", "/");

      package Du is new Den.Du;
      use type Du.Sizes;
   begin
      Assert (File'Length > 300);

      Create_Directory (Dir, (Create_Intermediate => True, others => <>));
      Assert (Kind (Dir) = Directory);

      --  The source is short, as test files are created by the runtime

      declare
         F : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, "short_file");
         Ada.Text_IO.Put_Line (F, "contents");
         Ada.Text_IO.Close (F);
      end;

      Copy ("short_file", File);
      Assert (Kind (File) = Den.File);
      Assert (Kind (Slashed) = Den.File);
      Copy (File, Dir / "copied");
      Assert (Kind (Dir / "copied") = Den.File);
      Copy (Dir / "copied", "short_back");
      Assert (Kind ("short_back") = Den.File);

      Assert (Canonical (File) = Canonical (Slashed));
      Assert (Name (Canonical (File)) = "file");

      Assert (Natural (Walk.Ls (Dir).Length) = 2);
      Assert (Walk.Ls (Dir).Contains ("file"));
      Assert (Natural (Walk.Find (Dir).Length) = 2);

      declare
         Seen : Natural := 0;
      begin
         for Item of Iterators.Iterate (Dir) loop
            Seen := Seen + 1;
         end loop;
         Assert (Seen = 2);
      end;

      declare
         Sizes : constant Du.Tree := Du.List (Dir);
      begin
         Assert (Sizes.First_Element.Tree_Size > 0);
      end;

      Delete_File (Dir / "copied");
      Assert (Kind (Dir / "copied") = Nothing);

      Delete_Tree (Top);
      Assert (Kind (Top) = Nothing);

      Delete_File ("short_file");
      Delete_File ("short_back");
   end Check_Long_Paths;

   ---------------------
   -- Check_Long_Link --
   ---------------------

   --  A link in a long path, which works as in any other

   procedure Check_Long_Link (Top : Path) is
      function Deep (Levels : Positive) return Path
      is (if Levels = 1
          then Top / "a_quite_long_folder_name_01"
          else Deep (Levels - 1) / "a_quite_long_folder_name_01");

      Dir : constant Path := Deep (12);
   begin
      Create_Directory (Dir, (Create_Intermediate => True, others => <>));

      Link (Dir / "link", "..", (others => <>));
      Assert (Kind (Dir / "link") = Softlink);
      Assert (Target (Dir / "link") = "..");
      Assert (Canonical (Dir / "link") = Canonical (Parent (Dir)));

      Unlink (Dir / "link");
      Assert (Kind (Dir / "link") = Nothing);

      Delete_Tree (Top);
   end Check_Long_Link;

   -------------------------
   -- Check_Read_Only_Dir --
   -------------------------

   --  Folders can also be read-only there

   procedure Check_Read_Only_Dir is
      Dir : constant Path := Test_Dir / "read_only_dir";
   begin
      Create_Directory (Dir);
      GNAT.OS_Lib.Set_Read_Only (Dir);
      Delete_Directory (Dir);
      Assert (Kind (Dir) = Nothing);
   end Check_Read_Only_Dir;

   -------------------
   -- Make_Junction --
   -------------------

   procedure Make_Junction (Name, Into : Path) is
      use GNAT.OS_Lib;

      Cmd     : String_Access := Locate_Exec_On_Path ("cmd.exe");
      Args    : Argument_List :=
                  (new String'("/c"),
                   new String'("mklink"),
                   new String'("/J"),
                   new String'(Name),
                   new String'(Into));
      Success : Boolean;
   begin
      Spawn (Cmd.all, Args, Success);
      Free (Cmd);
      for Arg of Args loop
         Free (Arg);
      end loop;
      Assert (Success, "Could not create junction: " & Name);
   end Make_Junction;

   ---------------
   -- Dir_Shows --
   ---------------

   --  The dir command tells apart the kinds of links in a folder, showing them
   --  as <JUNCTION>, <SYMLINKD> for directories, or <SYMLINK> for files

   function Dir_Shows (Folder : Path; Text : String) return Boolean is
      use GNAT.OS_Lib;

      Cmd     : String_Access := Locate_Exec_On_Path ("cmd.exe");
      Args    : Argument_List :=
                  (new String'("/c"),
                   new String'("dir /AL " & Folder & " | findstr " & Text));
      Success : Boolean;
   begin
      Spawn (Cmd.all, Args, Success);
      Free (Cmd);
      for Arg of Args loop
         Free (Arg);
      end loop;
      return Success;
   end Dir_Shows;

   -------------------------
   -- Check_Copy_Junction --
   -------------------------

   --  A junction is copied as such, as creating a symlink would need privileges
   --  that the original didn't, and its target isn't copied along

   procedure Check_Copy_Junction is
      Real   : constant Path := Test_Dir / "copy_real";
      Tree   : constant Path := Test_Dir / "copy_tree";
      Copied : constant Path := Test_Dir / "copied_tree";
   begin
      Create_Directory (Real);
      Touch (Real / "precious");
      Create_Directory (Tree);
      Create_Directory (Copied);
      Make_Junction (Tree / "junction", Absnormal (Real));

      Copy (Tree, Copied);

      Assert (Kind (Copied / "junction") = Softlink);
      Assert (Dir_Shows (Copied, "JUNCTION"));
      Assert (Target (Copied / "junction") = Target (Tree / "junction"));
      Assert (Canonical (Copied / "junction") = Canonical (Real));

      Delete_Tree (Copied);
      Delete_Tree (Tree);
      Assert (Kind (Real / "precious") = File);
      Delete_Tree (Real);
   end Check_Copy_Junction;

   --------------------------------
   -- Check_Copy_Broken_Dir_Link --
   --------------------------------

   --  A broken link has no target to tell what kind of link it is

   procedure Check_Copy_Broken_Dir_Link is
      Real   : constant Path := Test_Dir / "gone";
      Tree   : constant Path := Test_Dir / "broken_tree";
      Copied : constant Path := Test_Dir / "broken_copy";
   begin
      Create_Directory (Real);
      Create_Directory (Tree);
      Create_Directory (Copied);
      Link (Tree / "dir_link", ".." / "gone");
      Delete_Directory (Real);
      Assert (Is_Broken (Tree / "dir_link"));
      Assert (Dir_Shows (Tree, "SYMLINKD"));

      Copy (Tree / "dir_link", Copied / "dir_link");

      Assert (Kind (Copied / "dir_link") = Softlink);
      Assert (Dir_Shows (Copied, "SYMLINKD"));

      Delete_Tree (Copied);
      Delete_Tree (Tree);
   end Check_Copy_Broken_Dir_Link;

   --------------------
   -- Check_Junction --
   --------------------

   --  A junction is a link to another directory, and in particular deleting
   --  the tree that holds it must not delete the contents of its target.
   --  Creating one takes no privileges, so this doesn't depend on the checkout
   --  having created softlinks.

   procedure Check_Junction is
      Real     : constant Path := Test_Dir / "real";
      Tree     : constant Path := Test_Dir / "tree";
      Junction : constant Path := Tree / "junction";
   begin
      Create_Directory (Real);
      Touch (Real / "precious");
      Create_Directory (Tree);
      Make_Junction (Junction, Absnormal (Real));

      Assert (Kind (Junction) = Softlink);
      Assert (Target_Kind (Junction) = Directory);
      Assert (Target (Junction) = Absnormal (Real));
      Assert (Resolve (Junction) = Absnormal (Real));
      Assert (Canonical (Junction) = Canonical (Real));
      Assert (not Is_Broken (Junction));

      --  Not entered unless requested, as any other link

      declare
         Found : constant Walk.Items := Walk.Find (Tree);
      begin
         Assert (Natural (Found.Length) = 1);
         Assert (Found.First_Element.Path = Junction);
      end;

      declare
         Found : constant Walk.Items :=
                   Walk.Find (Tree, (Enter_Softlinked_Dirs => True,
                                     others                => <>));
      begin
         Assert (Natural (Found.Length) = 2);
      end;

      --  Goes away on its own

      Delete_Tree (Tree);
      Assert (Kind (Tree) = Nothing);
      Assert (Kind (Real / "precious") = File);

      Create_Directory (Tree);
      Make_Junction (Junction, Absnormal (Real));
      Unlink (Junction);
      Assert (Kind (Junction) = Nothing);
      Assert (Kind (Real / "precious") = File);

      --  Still a link when its target goes away

      Make_Junction (Junction, Absnormal (Real));
      Delete_Tree (Real);
      Assert (Kind (Junction) = Softlink);
      Assert (Target_Kind (Junction) = Nothing);
      Assert (Is_Broken (Junction));
      Unlink (Junction);
      Assert (Kind (Junction) = Nothing);
   end Check_Junction;

begin
   if Dir_Separator /= '\' then
      Ada.Text_IO.Put_Line ("Skipping as this is not Windows");
      return;
   end if;

   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   --  Devices are not files, even if they look like one. Which other names are
   --  devices, like the same one in a folder, depends on the Windows version.

   Assert (Kind ("NUL") = Special);

   Check_Classification;
   Check_Scrub;
   Check_Forward_Slashes;
   Check_Relative;
   Check_UNC;

   Check_Read_Only_Dir;

   Check_Long_Paths (Absnormal (Test_Dir / "abs"));
   Check_Long_Paths (Test_Dir / "rel");

   Check_Junction;
   Check_Copy_Junction;

   if Kind ("canary") = Softlink then
      Check_Slashed_Target;
      Check_Copy_Broken_Dir_Link;
      Check_Long_Link (Test_Dir / "link");
   else
      Ada.Text_IO.Put_Line ("Skipping link tests as they are not supported");
   end if;

   Delete_Tree (Test_Dir);
end Den_Tests.Windows;
