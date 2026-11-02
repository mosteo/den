pragma Alire_Test;

with Ada.Text_IO;

with Den.Filesystem;
with Den.Iterators;

procedure Den_Tests.Copy_Links is

   --  A copied link keeps the target as stored, whether it is broken or not

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Links    : constant Path := ".." / "cases" / "links";
   Test_Dir : constant Path := "copy_links_test_dir";

   Copied : Natural := 0;
begin
   if Kind ("canary") /= Softlink then
      Ada.Text_IO.Put_Line ("Skipping tests as softlinks are not supported");
      return;
   end if;

   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   for Item of Iterators.Iterate (Links) loop
      if Kind (Links / Item) = Softlink then
         Copy (Links / Item, Test_Dir / Item);
         Assert (Kind (Test_Dir / Item) = Softlink);
         Assert (Target (Test_Dir / Item) = Target (Links / Item));
         Copied := Copied + 1;
      end if;
   end loop;

   Assert (Copied > 0);

   Delete_Tree (Test_Dir);
end Den_Tests.Copy_Links;
