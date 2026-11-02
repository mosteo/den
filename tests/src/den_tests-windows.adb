pragma Alire_Test;

with Ada.Text_IO;

with Den.Filesystem;

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

begin
   if Dir_Separator /= '\' then
      Ada.Text_IO.Put_Line ("Skipping as this is not Windows");
      return;
   end if;

   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   if Kind ("canary") = Softlink then
      Check_Slashed_Target;
   else
      Ada.Text_IO.Put_Line ("Skipping link tests as they are not supported");
   end if;

   Delete_Tree (Test_Dir);
end Den_Tests.Windows;
