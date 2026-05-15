pragma Warnings (Off);

with Ada.Text_IO;

with Den.Filesystem;

procedure Den_Tests.Delete_Empty_Tree is
   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Tmp : constant Path := "delete_empty_tree_tmp";

   procedure Touch (P : Path) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, P);
      Ada.Text_IO.Close (F);
   end Touch;
begin
   if Exists (Tmp) then
      Delete_Tree (Tmp);
   end if;
   Create_Directory (Tmp);

   --  Truly empty directory is deleted; returns True
   declare
      Dir : constant Path := Tmp / "empty";
   begin
      Create_Directory (Dir);
      Assert (Filesystem.Delete_Empty_Tree (Dir));
      Assert (Kind (Dir) = Nothing);
   end;

   --  Directory containing only empty subdirectories is deleted; returns True
   declare
      Dir : constant Path := Tmp / "nested_empty";
   begin
      Create_Directory (Dir);
      Create_Directory (Dir / "a");
      Create_Directory (Dir / "a" / "b");
      Assert (Filesystem.Delete_Empty_Tree (Dir));
      Assert (Kind (Dir) = Nothing);
   end;

   --  Directory containing a file is left intact; returns False
   declare
      Dir  : constant Path := Tmp / "with_file";
      Leaf : constant Path := Dir / "f";
   begin
      Create_Directory (Dir);
      Touch (Leaf);
      Assert (not Filesystem.Delete_Empty_Tree (Dir));
      Assert (Kind (Dir) = Directory);
      Assert (Kind (Leaf) = File);
   end;

   --  Non-directory path returns False without touching the file
   declare
      Leaf : constant Path := Tmp / "plain_file";
   begin
      Touch (Leaf);
      Assert (not Filesystem.Delete_Empty_Tree (Leaf));
      Assert (Kind (Leaf) = File);
   end;

   --  Non-existent path returns False
   Assert (not Filesystem.Delete_Empty_Tree (Tmp / "nonexistent"));

   Delete_Tree (Tmp);
end Den_Tests.Delete_Empty_Tree;
