pragma Alire_Test;

with Ada.Text_IO;

with Den.Filesystem;

with GNAT.OS_Lib;

procedure Den_Tests.Delete_Semantics is

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir : constant Path := "delete_semantics_test_dir";

   -----------
   -- Touch --
   -----------

   procedure Touch (File_Path : Path) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, File_Path);
      Ada.Text_IO.Close (F);
   end Touch;

   ---------------------
   -- Check_Read_Only --
   ---------------------

   --  Windows refuses to delete read-only items, unlike other systems

   procedure Check_Read_Only is
      Tree : constant Path := Test_Dir / "tree";
   begin
      Create_Directory (Tree);

      Touch (Test_Dir / "file");
      GNAT.OS_Lib.Set_Read_Only (Test_Dir / "file");
      Delete_File (Test_Dir / "file");
      Assert (Kind (Test_Dir / "file") = Nothing);

      Touch (Tree / "file");
      GNAT.OS_Lib.Set_Read_Only (Tree / "file");
      Delete_Tree (Tree);
      Assert (Kind (Tree) = Nothing);
   end Check_Read_Only;

begin
   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   Check_Read_Only;

   Delete_Tree (Test_Dir);
end Den_Tests.Delete_Semantics;
