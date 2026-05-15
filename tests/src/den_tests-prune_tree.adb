pragma Warnings (Off);

with Ada.Text_IO;

with Den.Filesystem;

procedure Den_Tests.Prune_Tree is
   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Tmp : constant Path := "prune_tree_tmp";

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

   --  Delete_Root => False (default is True, so pass explicitly):
   --  empty subdirectories are removed; the root survives even when empty
   declare
      Dir : constant Path := Tmp / "keep_root";
   begin
      Create_Directory (Dir);
      Create_Directory (Dir / "a");
      Create_Directory (Dir / "b");
      Filesystem.Prune_Tree (Dir, Delete_Root => False);
      Assert (Kind (Dir) = Directory);
      Assert (Kind (Dir / "a") = Nothing);
      Assert (Kind (Dir / "b") = Nothing);
   end;

   --  Delete_Root => True (the default):
   --  all subdirs are empty, so the root is also deleted
   declare
      Dir : constant Path := Tmp / "delete_root";
   begin
      Create_Directory (Dir);
      Create_Directory (Dir / "a");
      Create_Directory (Dir / "a" / "b");
      Filesystem.Prune_Tree (Dir);
      Assert (Kind (Dir) = Nothing);
   end;

   --  Delete_Root => True but root has a file: root survives
   declare
      Dir  : constant Path := Tmp / "root_has_file";
      Leaf : constant Path := Dir / "f";
   begin
      Create_Directory (Dir);
      Create_Directory (Dir / "empty");
      Touch (Leaf);
      Filesystem.Prune_Tree (Dir);
      Assert (Kind (Dir) = Directory);
      Assert (Kind (Dir / "empty") = Nothing);
      Assert (Kind (Leaf) = File);
   end;

   --  A subdir containing a file is preserved; an adjacent empty subdir is removed
   declare
      Dir   : constant Path := Tmp / "mixed";
      Empty : constant Path := Dir / "empty";
      Full  : constant Path := Dir / "full";
   begin
      Create_Directory (Dir);
      Create_Directory (Empty);
      Create_Directory (Full);
      Touch (Full / "f");
      Filesystem.Prune_Tree (Dir, Delete_Root => False);
      Assert (Kind (Dir) = Directory);
      Assert (Kind (Empty) = Nothing);
      Assert (Kind (Full) = Directory);
   end;

   --  Non-directory input is silently ignored
   declare
      Leaf : constant Path := Tmp / "just_a_file";
   begin
      Touch (Leaf);
      Filesystem.Prune_Tree (Leaf);
      Assert (Kind (Leaf) = File);
   end;

   Delete_Tree (Tmp);
end Den_Tests.Prune_Tree;
