pragma Alire_Test;

with Ada.IO_Exceptions;
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

   -------------------------
   -- Check_Not_Recursive --
   -------------------------

   --  Without Recursive, only an empty directory goes away, and nothing else
   --  is touched if it isn't empty

   procedure Check_Not_Recursive is
      Dir : constant Path := Test_Dir / "not_recursive";
   begin
      Create_Directory (Dir);
      Delete_Directory (Dir);
      Assert (Kind (Dir) = Nothing);

      --  Only an empty folder inside

      Create_Directory (Dir / "sub", (Create_Intermediate => True, others => <>));
      begin
         Delete_Directory (Dir);
         raise Program_Error with "Deleted a non-empty directory";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;
      Assert (Kind (Dir / "sub") = Directory);

      --  Also a file, which gets in the way too

      Touch (Dir / "file");
      begin
         Delete_Directory (Dir);
         raise Program_Error with "Deleted a non-empty directory";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;
      Assert (Kind (Dir / "sub") = Directory);
      Assert (Kind (Dir / "file") = File);

      --  Not directories

      begin
         Delete_Directory (Dir / "file");
         raise Program_Error with "Deleted a file as a directory";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;

      begin
         Delete_Directory (Dir / "missing");
         raise Program_Error with "Deleted a missing directory";
      exception
         when Ada.IO_Exceptions.Name_Error =>
            null;
      end;

      Delete_Tree (Dir);
      Assert (Kind (Dir) = Nothing);
   end Check_Not_Recursive;

   -------------------
   -- Check_Missing --
   -------------------

   --  There's nothing to do for a tree that is already gone

   procedure Check_Missing is
   begin
      Delete_Tree (Test_Dir / "missing");
   end Check_Missing;

   -------------------
   -- Check_Special --
   -------------------

   --  A special file doesn't stop a tree from being deleted, although it isn't
   --  deleted by itself unless asked for

   procedure Check_Special is
      use GNAT.OS_Lib;

      Tree : constant Path := Test_Dir / "special";
      Fifo : constant Path := Tree / "fifo";

      Mkfifo  : String_Access := Locate_Exec_On_Path ("mkfifo");
      Args    : Argument_List := (1 => new String'(Fifo));
      Success : Boolean := False;
   begin
      Create_Directory (Tree);

      if Mkfifo /= null then
         Spawn (Mkfifo.all, Args, Success);
         Free (Mkfifo);
      end if;
      Free (Args (1));

      if not Success then
         Ada.Text_IO.Put_Line ("Skipping as a FIFO cannot be created");
         return;
      end if;

      Assert (Kind (Fifo) = Special);

      begin
         Delete_File (Fifo);
         raise Program_Error with "Deleted a special file";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;

      begin
         Delete_Directory (Fifo);
         raise Program_Error with "Deleted a special file";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;

      Assert (Kind (Fifo) = Special);

      Touch (Tree / "file");
      Delete_Tree (Tree);
      Assert (Kind (Tree) = Nothing);
   end Check_Special;

begin
   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;
   Create_Directory (Test_Dir);

   Check_Read_Only;
   Check_Not_Recursive;
   Check_Missing;

   --  Windows has no FIFOs, and those that its Unix-like tools emulate, which
   --  may be in the PATH, are nothing special for the OS

   if Dir_Separator = '/' then
      Check_Special;
   end if;

   Delete_Tree (Test_Dir);
end Den_Tests.Delete_Semantics;
