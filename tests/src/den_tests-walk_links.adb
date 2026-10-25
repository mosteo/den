with Ada.Text_IO;

with Den.Filesystem;
with Den.Walk;

procedure Den_Tests.Walk_Links is

   --  Walking a softlinked directory lists the contents of its target, both
   --  when it is the start of the walk and when entered with
   --  Enter_Softlinked_Dirs. Children are reported below the link.

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir : constant Path := "walk_links_test_dir";
   Real     : constant Path := Test_Dir / "real";
   Link_Dir : constant Path := Test_Dir / "link";

   function Found (Start : Path; Options : Walk.Find_Options)
                   return Sorted_Paths
   is
      Result : Sorted_Paths;
   begin
      for Item of Walk.Find (Start, Options) loop
         Result.Insert (Item.Path);
      end loop;
      return Result;
   end Found;

begin
   if Kind ("canary") /= Softlink then
      Ada.Text_IO.Put_Line ("Skipping tests as softlinks are not supported");
      return;
   end if;

   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);
   Create_Directory (Real);
   Create_Directory (Real / "sub");
   declare
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Real / "file");
      Ada.Text_IO.Close (F);
   end;
   Link (Link_Dir, "real");

   --  The link as the start of a walk

   declare
      Result : constant Sorted_Paths := Found (Link_Dir, (others => <>));
   begin
      pragma Assert (Result.Contains (Link_Dir / "file"));
      pragma Assert (Result.Contains (Link_Dir / "sub"));
      pragma Assert (Natural (Result.Length) = 2);
   end;

   --  The link entered when found, only when asked

   declare
      Result : constant Sorted_Paths :=
                 Found (Test_Dir, (Enter_Softlinked_Dirs => True,
                                   others                => <>));
   begin
      pragma Assert (Result.Contains (Link_Dir / "file"));
      pragma Assert (Result.Contains (Real / "file"));
   end;

   declare
      Result : constant Sorted_Paths := Found (Test_Dir, (others => <>));
   begin
      pragma Assert (Result.Contains (Link_Dir));
      pragma Assert (not Result.Contains (Link_Dir / "file"));
   end;

   Delete_Tree (Test_Dir);
end Den_Tests.Walk_Links;
