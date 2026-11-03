pragma Alire_Test;

with Ada.IO_Exceptions;
with Ada.Text_IO;

with Den.Filesystem;

with GNAT.OS_Lib;

procedure Den_Tests.Copy_Options is

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir : constant Path := "copy_options_test_dir";

   -----------
   -- Write --
   -----------

   procedure Write (File_Path : Path; Text : String) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, File_Path);
      Ada.Text_IO.Put_Line (F, Text);
      Ada.Text_IO.Close (F);
   end Write;

   ----------
   -- Read --
   ----------

   function Read (File_Path : Path) return String is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (F, Ada.Text_IO.In_File, File_Path);
      return Text : constant String := Ada.Text_IO.Get_Line (F) do
         Ada.Text_IO.Close (F);
      end return;
   end Read;

   -----------
   -- Fresh --
   -----------

   --  An empty folder for a check

   function Fresh (Name : Path) return Path is
   begin
      Create_Directory (Test_Dir / Name);
      return Test_Dir / Name;
   end Fresh;

   -------------------
   -- Check_Options --
   -------------------

   --  Options apply to the whole tree, not only to the first level

   procedure Check_Options is
      Top : constant Path := Fresh ("options");
      Src : constant Path := Top / "src";
      Dst : constant Path := Top / "dst";
   begin
      Create_Directory (Src / "sub", (Create_Intermediate => True, others => <>));
      Create_Directory (Dst / "sub", (Create_Intermediate => True, others => <>));
      Write (Src / "file", "new");
      Write (Src / "sub" / "file", "new");
      Write (Src / "sub" / "extra", "new");
      Write (Dst / "file", "old");
      Write (Dst / "sub" / "file", "old");
      Write (Dst / "sub" / "kept", "old");

      Copy (Src, Dst, (Merge_Dirs      => True,
                       Overwrite_Files => True,
                       others          => <>));

      Assert (Read (Dst / "file") = "new");
      Assert (Read (Dst / "sub" / "file") = "new");
      Assert (Read (Dst / "sub" / "extra") = "new");
      Assert (Read (Dst / "sub" / "kept") = "old");
   end Check_Options;

   ---------------------
   -- Check_Same_File --
   ---------------------

   --  Overwriting a file with itself must not lose it

   procedure Check_Same_File is
      Top   : constant Path := Fresh ("same");
      F     : constant Path := Top / "file";
      Over  : constant Den.Filesystem.Copy_Options :=
                (Overwrite_Files => True, others => <>);

      procedure Refused (Src, Dst : Path) is
      begin
         Copy (Src, Dst, Over);
         raise Program_Error with "Copied onto itself: " & Src & " " & Dst;
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Assert (Read (F) = "contents", "Lost by " & Src & " " & Dst);
      end Refused;

   begin
      Write (F, "contents");

      Refused (F, F);
      Refused (F, Top); -- Would end at the same file too

      if Kind ("canary") = Softlink then
         Link (Top / "link", "file");
         Refused (Top / "link", F);
      end if;
   end Check_Same_File;

   ------------------
   -- Check_Inside --
   ------------------

   --  A tree can't be copied into itself

   procedure Check_Inside is
      Top : constant Path := Fresh ("inside");
      Src : constant Path := Top / "src";

      procedure Refused (Dst : Path) is
      begin
         Copy (Src, Dst, (Merge_Dirs => True, others => <>));
         raise Program_Error with "Copied into itself: " & Dst;
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end Refused;

   begin
      Create_Directory (Src / "sub", (Create_Intermediate => True, others => <>));
      Write (Src / "file", "contents");

      Refused (Src);
      Refused (Src / "sub");
      Assert (Kind (Src / "sub" / "sub") = Nothing);
      Assert (Kind (Src / "sub" / "file") = Nothing);

      --  Sharing the start of the name doesn't make it a child

      Create_Directory (Top / "src_2");
      Copy (Src, Top / "src_2");
      Assert (Read (Top / "src_2" / "file") = "contents");
   end Check_Inside;

   ---------------------
   -- Check_Following --
   ---------------------

   --  Following links, wherever they are in relation to the current directory

   procedure Check_Following is
      Top     : constant Path := Fresh ("following");
      Follow  : constant Den.Filesystem.Copy_Options :=
                  (Resolve_Links => True, others => <>);
   begin
      Create_Directory (Top / "real_dir");
      Write (Top / "real_dir" / "inner", "inner");
      Write (Top / "real_file", "file");
      Link (Top / "to_file", "real_file");
      Link (Top / "to_dir", "real_dir");

      --  A link to a file

      Copy (Top / "to_file", Top / "out_file", Follow);
      Assert (Kind (Top / "out_file") = File);
      Assert (Read (Top / "out_file") = "file");

      --  Into a folder, under the name of the link

      Create_Directory (Top / "out_dir");
      Copy (Top / "to_file", Top / "out_dir", Follow);
      Assert (Kind (Top / "out_dir" / "to_file") = File);
      Assert (Kind (Top / "out_dir" / "real_file") = Nothing);

      --  A link to a folder, which is created if needed

      Copy (Top / "to_dir", Top / "out_new", Follow);
      Assert (Kind (Top / "out_new") = Directory);
      Assert (Read (Top / "out_new" / "inner") = "inner");

      --  Inside a tree, keeping the name of the link

      Create_Directory (Top / "tree" / "sub",
                        (Create_Intermediate => True, others => <>));
      Link (Top / "tree" / "sub" / "to_file", ".." / ".." / "real_file");
      Link (Top / "tree" / "sub" / "to_dir", ".." / ".." / "real_dir");
      Create_Directory (Top / "out_tree");

      Copy (Top / "tree", Top / "out_tree", Follow);

      Assert (Kind (Top / "out_tree" / "sub" / "to_file") = File);
      Assert (Read (Top / "out_tree" / "sub" / "to_file") = "file");
      Assert (Kind (Top / "out_tree" / "sub" / "to_dir") = Directory);
      Assert (Read (Top / "out_tree" / "sub" / "to_dir" / "inner") = "inner");
   end Check_Following;

   ----------------------
   -- Check_Attributes --
   ----------------------

   --  What to bring along is up to the options, for files and for folders

   procedure Check_Attributes is
      use GNAT.OS_Lib;

      Top : constant Path := Fresh ("attributes");
      Src : constant Path := Top / "src";

      Only_Times  : constant Den.Filesystem.Copy_Options :=
                      (Preserve_Timestamps  => True,
                       Preserve_Permissions => False,
                       others               => <>);
      Only_Perms  : constant Den.Filesystem.Copy_Options :=
                      (Preserve_Timestamps  => False,
                       Preserve_Permissions => True,
                       others               => <>);
   begin
      Create_Directory (Src);
      Write (Src / "file", "contents");
      Set_Read_Only (Src / "file");

      --  So a new time can be told apart, with room for file systems that only
      --  keep two seconds of precision

      delay 2.5;

      Create_Directory (Top / "times");
      Copy (Src, Top / "times", Only_Times);
      Assert (File_Time_Stamp (Top / "times" / "file")
              = File_Time_Stamp (Src / "file"), "Timestamp of file");
      Assert (File_Time_Stamp (Top / "times")
              = File_Time_Stamp (Src), "Timestamp of folder");
      Assert (Is_Writable_File (Top / "times" / "file"), "Not asked to copy");

      Create_Directory (Top / "perms");
      Copy (Src, Top / "perms", Only_Perms);
      Assert (File_Time_Stamp (Top / "perms" / "file")
              /= File_Time_Stamp (Src / "file"), "Not asked to copy");
      Assert (not Is_Writable_File (Top / "perms" / "file"), "Permissions");
   end Check_Attributes;

   -----------------
   -- Check_Cycle --
   -----------------

   --  Following a link back to a folder being copied would never end

   procedure Check_Cycle is
      Top : constant Path := Fresh ("cycle");
   begin
      Create_Directory (Top / "src");
      Create_Directory (Top / "dst");
      Write (Top / "src" / "file", "contents");
      Link (Top / "src" / "back", ".");

      Copy (Top / "src", Top / "dst", (Resolve_Links => True, others => <>));
      raise Program_Error with "Copied a cycle";
   exception
      when Ada.IO_Exceptions.Use_Error =>
         null;
   end Check_Cycle;

begin
   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   Check_Options;
   Check_Same_File;
   Check_Inside;
   Check_Attributes;

   if Kind ("canary") = Softlink then
      Check_Following;
      Check_Cycle;
   else
      Ada.Text_IO.Put_Line ("Skipping link tests as they are not supported");
   end if;

   Delete_Tree (Test_Dir);
end Den_Tests.Copy_Options;
