pragma Alire_Test;

with Ada.IO_Exceptions;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO;

with Den.Filesystem;
with Den.Iterators;
with Den.Walk;

procedure Den_Tests.Unicode is

   --  Names are opaque to Den, so they must come back from every operation
   --  with the exact bytes that went in.

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   --  Tests are compiled with -gnatW8, which makes a non-ASCII String literal
   --  hold Latin-1 characters instead of the UTF-8 encoding of its text.
   function U8 (Text : Wide_Wide_String) return String
   is (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Text));

   --  macOS may store composed characters decomposed, so names that are
   --  compared by their bytes must not have any
   CJK   : constant String := U8 ("日本語");
   Greek : constant String := U8 ("Ελληνικα");
   Emoji : constant String := U8 ("😀"); -- A surrogate pair in UTF-16

   Test_Dir : constant Path := "unicode_test_dir";

   Supported : constant Boolean := Kind ("canary") = Softlink;

   -----------
   -- Touch --
   -----------

   procedure Touch (File_Path : Path) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, File_Path);
      Ada.Text_IO.Close (F);
   end Touch;

   ------------
   -- Listed --
   ------------

   function Listed (Dir : Path; Name : String) return Boolean is
   begin
      for Item of Iterators.Iterate (Dir) loop
         if Item = Name then
            return True;
         end if;
      end loop;
      return False;
   end Listed;

   ----------------
   -- Check_Name --
   ----------------

   procedure Check_Name (Name : String) is
      Dir      : constant Path := Test_Dir / Name;
      The_File : constant Path := Dir / Name;
   begin
      Create_Directory (Dir);
      Assert (Kind (Dir) = Directory);
      Assert (Listed (Test_Dir, Name));

      Touch (The_File);
      Assert (Kind (The_File) = File);
      Assert (Listed (Dir, Name));

      Assert (Canonical (The_File) = Canonical (Dir) / Name);
      Assert (Den.Name (Canonical (The_File)) = Name);

      Assert (Walk.Ls (Dir).Contains (Name));
      declare
         Found : constant Walk.Items := Walk.Find (Dir);
      begin
         Assert (Natural (Found.Length) = 1);
         Assert (Found.First_Element.Path = The_File);
      end;
   end Check_Name;

   ----------------
   -- Check_Link --
   ----------------

   procedure Check_Link (Name : String) is
      The_Link : constant Path := Test_Dir / ("link_" & Name);
      Dir      : constant Path := Test_Dir / Name;
   begin
      Link (The_Link, Name);

      Assert (Kind (The_Link) = Softlink);
      Assert (Target (The_Link) = Name);
      Assert (Target_Length (The_Link) = Name'Length);
      Assert (Target_Kind (The_Link) = Directory);
      Assert (not Is_Broken (The_Link));
      Assert (Resolve (The_Link) = Dir);
      Assert (Canonical (The_Link) = Canonical (Dir));
   end Check_Link;

   -----------------------------
   -- Check_Link_Not_Followed --
   -----------------------------

   --  Having a non-ASCII name must not stop a link from being one, as deleting
   --  the tree that holds it would then delete the contents of its target.

   procedure Check_Link_Not_Followed (Name : String) is
      Kept : constant Path := Test_Dir / ("kept_" & Name);
      Tree : constant Path := Test_Dir / ("tree_" & Name);
   begin
      Create_Directory (Kept);
      Touch (Kept / "precious");
      Create_Directory (Tree);
      Link (Tree / Name, ".." / ("kept_" & Name));
      Assert (Kind (Tree / Name) = Softlink);

      Delete_Tree (Tree);

      Assert (Kind (Tree) = Nothing);
      Assert (Kind (Kept / "precious") = File);
   end Check_Link_Not_Followed;

   -------------------------
   -- Check_Composed_Name --
   -------------------------

   --  A composed name may come back decomposed from the listing on macOS, but
   --  it has to designate the same file either way.

   procedure Check_Composed_Name is
      Dir  : constant Path := Test_Dir / "composed";
      Name : constant String := U8 ("café");
   begin
      Create_Directory (Dir);
      Touch (Dir / Name);
      Assert (Kind (Dir / Name) = File);

      for Item of Iterators.Iterate (Dir) loop
         Assert (Kind (Dir / Item) = File);
         Assert (Canonical (Dir / Item) = Canonical (Dir / Name));
      end loop;
   end Check_Composed_Name;

   -----------------------
   -- Check_Invalid_UTF8 --
   -----------------------

   --  Names are just bytes where the filesystem allows it

   procedure Check_Invalid_UTF8 is
      Name : constant String := Character'Val (16#FF#) & "x";
      Dir  : constant Path := Test_Dir / Name;
   begin
      --  Windows names are UTF-16, so these cannot name anything there

      if Dir_Separator /= '/' then
         Assert (Kind (Dir) = Nothing);
         begin
            Create_Directory (Dir);
            raise Program_Error with "Created an impossible name";
         exception
            when Ada.IO_Exceptions.Use_Error =>
               return;
         end;
      end if;

      begin
         Create_Directory (Dir);
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Ada.Text_IO.Put_Line ("Skipping as this filesystem rejects it");
            return;
      end;

      Assert (Kind (Dir) = Directory);
      Assert (Listed (Test_Dir, Name));
      Assert (Den.Name (Canonical (Dir)) = Name);

      Delete_Tree (Dir);
   end Check_Invalid_UTF8;

   ------------------------------
   -- Check_Unpaired_Surrogate --
   ------------------------------

   --  NTFS allows these in names, and they must round-trip like any other

   procedure Check_Unpaired_Surrogate is
      Name : constant String :=
               Character'Val (16#ED#)
               & Character'Val (16#A0#)
               & Character'Val (16#80#)
               & "x";
      Dir  : constant Path := Test_Dir / Name;
   begin
      if Dir_Separator /= '\' then
         return;
      end if;

      Create_Directory (Dir);
      Assert (Kind (Dir) = Directory);
      Assert (Listed (Test_Dir, Name));
      Assert (Den.Name (Canonical (Dir)) = Name);
      Delete_Tree (Dir);
      Assert (Kind (Dir) = Nothing);
   end Check_Unpaired_Surrogate;

   ----------------------
   -- Check_Magic_Link --
   ----------------------

   --  These report a size of zero, and the length must still be the true one

   procedure Check_Magic_Link is
      Magic : constant Path := "/proc/self/exe";
   begin
      if Dir_Separator = '/' and then Kind (Magic) = Softlink then
         Assert (Target_Length (Magic) = Target (Magic)'Length);
      end if;
   end Check_Magic_Link;

begin
   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   Check_Name (CJK);
   Check_Name (Greek);
   Check_Name (Emoji);

   if Supported then
      Check_Link (CJK);
      Check_Link (Greek);
      Check_Link (Emoji);

      Check_Link_Not_Followed (CJK);
      Check_Link_Not_Followed (Emoji);
   else
      Ada.Text_IO.Put_Line ("Skipping link tests as they are not supported");
   end if;

   Assert (Relative (Test_Dir / CJK / "from", Test_Dir / Greek / "into") =
             ".." / ".." / Greek / "into");

   Check_Composed_Name;
   Check_Invalid_UTF8;
   Check_Unpaired_Surrogate;
   Check_Magic_Link;

   Delete_Tree (Test_Dir);
end Den_Tests.Unicode;
