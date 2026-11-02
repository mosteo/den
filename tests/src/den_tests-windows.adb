pragma Alire_Test;

with Ada.Text_IO;

with Den.Filesystem;
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

   Check_Junction;

   if Kind ("canary") = Softlink then
      Check_Slashed_Target;
   else
      Ada.Text_IO.Put_Line ("Skipping link tests as they are not supported");
   end if;

   Delete_Tree (Test_Dir);
end Den_Tests.Windows;
