pragma Alire_Test;

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with Den.Du;
with Den.Filesystem;
with Den.Walk;

with GNAT.OS_Lib;

procedure Den_Tests.Walk_Options is

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   use type Walk.Canonical_Parts;

   Test_Dir : constant Path := "walk_options_test_dir";

   Sub  : constant Path := Test_Dir / "sub";
   Deep : constant Path := Sub / "deep";

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

   ---------
   -- Has --
   ---------

   function Has (Found : Walk.Items; This : Path) return Boolean
   is (for some Item of Found => Item.Path = This);

   ------------------------
   -- Except_Root_Filter --
   ------------------------

   --  Matches anything but the start of the walk

   type Except_Root_Filter (Length : Natural) is new Walk.Filters with record
      Root : String (1 .. Length);
   end record;

   overriding function Match (This : Except_Root_Filter;
                              Item : Path)
                              return Boolean
   is (Item /= This.Root);

   ------------------
   -- Check_Filter --
   ------------------

   --  A filter says what is reported, not what is entered

   procedure Check_Filter is
      Found : constant Walk.Items :=
                Walk.Find (Test_Dir, Filter => Walk.Kind_Is (File));
   begin
      Assert (Has (Found, Test_Dir / "file"));
      Assert (Has (Found, Sub / "file"));
      Assert (Has (Found, Deep / "file"));
      Assert (not Has (Found, Sub));
   end Check_Filter;

   ----------------
   -- Check_Root --
   ----------------

   procedure Check_Root is
   begin
      --  Nothing is reported for a start that doesn't exist

      Assert (Walk.Find (Test_Dir / "missing").Is_Empty);

      --  A start that is not a directory is only reported, never entered

      Assert (Natural (Walk.Find (Test_Dir / "file").Length) = 1);
      Assert (Walk.Find (Test_Dir / "file").First_Element.Depth = 0);
      Assert
        (Walk.Find
           (Test_Dir / "file",
            Filter => Except_Root_Filter'
                        (Length => Natural'(Test_Dir'Length + 5),
                         Root   => Test_Dir / "file")).Is_Empty);
   end Check_Root;

   ---------------------
   -- Check_From_Root --
   ---------------------

   --  The separator of a root must not be duplicated

   procedure Check_From_Root is
      Top   : constant Path := Root (Absnormal ("."));
      Found : Boolean := False;

      procedure Check (This  : Walk.Item;
                       Enter : in out Boolean;
                       Stop  : in out Boolean)
      is
         pragma Unreferenced (Enter);
      begin
         Assert (This.Path = Top / Name (This.Path),
                 "Unexpected path: " & This.Path);
         Found := True;
         Stop  := True;
      end Check;
   begin
      Walk.Find (Top, Check'Access);
      Assert (Found);
   end Check_From_Root;

   -----------------
   -- Check_Enter --
   -----------------

   procedure Check_Enter is
      Found : constant Walk.Items :=
                Walk.Find (Test_Dir,
                           (Enter_Regular_Dirs    => False,
                            Enter_Softlinked_Dirs => True,
                            others                => <>));
   begin
      Assert (Has (Found, Test_Dir / "link_dir" / "file"));
      Assert (Has (Found, Sub));
      Assert (not Has (Found, Sub / "file"));
   end Check_Enter;

   -----------------
   -- Check_Visit --
   -----------------

   procedure Check_Visit is
      Plain   : constant Walk.Items :=
                  Walk.Find (Test_Dir, (Visit_Softlinks => False,
                                        others          => <>));
      Entered : constant Walk.Items :=
                  Walk.Find (Test_Dir, (Visit_Softlinks       => False,
                                        Enter_Softlinked_Dirs => True,
                                        others                => <>));
   begin
      Assert (Has (Plain, Sub));
      Assert (not Has (Plain, Test_Dir / "link_dir"));
      Assert (not Has (Plain, Test_Dir / "link_file"));
      Assert (not Has (Plain, Test_Dir / "broken"));

      --  Not reporting a link doesn't stop it from being entered

      Assert (not Has (Entered, Test_Dir / "link_dir"));
      Assert (Has (Entered, Test_Dir / "link_dir" / "file"));

      --  Also for the start of the walk

      Assert (Walk.Find (Test_Dir / "broken",
                         (Visit_Softlinks => False,
                          others          => <>)).Is_Empty);
      Assert (not Walk.Find (Test_Dir / "broken").Is_Empty);
   end Check_Visit;

   -------------------
   -- Assert_Unique --
   -------------------

   procedure Assert_Unique (Found : Walk.Items) is
      Seen : Sorted_Paths;
   begin
      for Item of Found loop
         Assert (not Seen.Contains (Item.Path), "Duplicated: " & Item.Path);
         Seen.Insert (Item.Path);
      end loop;
   end Assert_Unique;

   ------------------
   -- Check_Dedupe --
   ------------------

   procedure Check_Dedupe is
      Unique  : constant Walk.Items :=
                  Walk.Find (Test_Dir, (Canonicalize => Walk.All_Deduped,
                                        others       => <>));
      Entered : constant Walk.Items :=
                  Walk.Find (Test_Dir, (Canonicalize          => Walk.All_Deduped,
                                        Enter_Softlinked_Dirs => True,
                                        others                => <>));
      Real    : constant Path := Canonical (Test_Dir);
   begin
      --  The link is seen first, but that must not hide what's in its target

      Assert (Has (Unique, Real / "sub" / "file"));
      Assert (Has (Unique, Real / "sub" / "deep" / "file"));

      --  Everything just once

      Assert_Unique (Unique);
      Assert_Unique (Entered);

      Assert (Has (Entered, Real / "sub" / "file"));
   end Check_Dedupe;

   -----------------
   -- Check_Loops --
   -----------------

   --  A link back into a folder being walked is reported, but not entered

   procedure Check_Loops is
      Back : constant Path := Sub / "back";
   begin
      Link (Back, "..");

      for Mode in Walk.Canonical_Parts loop
         declare
            Found : constant Walk.Items :=
                      Walk.Find (Test_Dir, (Enter_Softlinked_Dirs => True,
                                            Canonicalize          => Mode,
                                            others                => <>));
         begin
            if Mode in Walk.None | Walk.Base then
               Assert (not Has (Found, Back / "file"));
            end if;
            if Mode = Walk.None then
               Assert (Has (Found, Back));
               Assert (Has (Found, Test_Dir / "link_dir" / "file"));
            end if;
         end;
      end loop;

      Unlink (Back);
   end Check_Loops;

   --------------
   -- Check_Ls --
   --------------

   procedure Check_Ls is
      The_File : constant Path := Test_Dir / "file";
   begin
      Assert (Walk.Ls (The_File).Contains (The_File));
      Assert (Walk.Ls (The_File, (Canonicalize => True, others => <>))
              .Contains (Canonical (The_File)));
   end Check_Ls;

   ----------------------
   -- Check_Unreadable --
   ----------------------

   --  A directory that can't be read stops a walk, unless asked otherwise. The
   --  results are checked after restoring it so nothing is left behind.

   procedure Check_Unreadable is
      package Du is new Den.Du;

      Top    : constant Path := Test_Dir / "unreadable";
      Locked : constant Path := Top / "locked";

      Ls_Raised, Find_Raised, Du_Raised : Boolean := False;

      Skip_Find : Walk.Items;
      Skip_Ls   : Sorted_Paths;
      Du_Items  : Natural := 0;

      -------------------
      -- Change_Access --
      -------------------

      --  Windows has no read permission for GNAT.OS_Lib to change, but it can
      --  deny listing the folder to everyone, even when elevated.

      procedure Change_Access (Deny : Boolean) is
         use GNAT.OS_Lib;
      begin
         if Dir_Separator = '/' then
            if Deny then
               Set_Non_Readable (Locked);
            else
               Set_Readable (Locked);
            end if;
         else
            declare
               Tool    : String_Access := Locate_Exec_On_Path ("icacls.exe");
               Args    : Argument_List :=
                           (new String'(Locked),
                            new String'((if Deny then "/deny" else "/remove:d")),
                            new String'
                              ((if Deny
                                then "*S-1-1-0:(RD)"
                                else "*S-1-1-0")));
               Success : Boolean := False;
            begin
               if Tool /= null then
                  Spawn (Tool.all, Args, Success);
                  Free (Tool);
               end if;
               for Arg of Args loop
                  Free (Arg);
               end loop;
            end;
         end if;
      end Change_Access;

   begin
      Create_Directory (Locked, (Create_Intermediate => True, others => <>));
      Touch (Locked / "hidden");
      Touch (Top / "visible");
      Change_Access (Deny => True);

      begin
         Assert (Natural (Walk.Ls (Locked).Length) <= 1);
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Ls_Raised := True;
      end;

      if not Ls_Raised then
         Change_Access (Deny => False);

         --  This must not go unnoticed where it should be working, which isn't
         --  the elevated CI runners on Windows, where it has no effect.
         Assert (Dir_Separator /= '/'
                 or else not Ada.Environment_Variables.Exists ("GITHUB_ACTIONS"),
                 "Could not deny access to a folder in CI");
         Ada.Text_IO.Put_Line ("Skipping as permissions don't apply (root?)");
         return;
      end if;

      begin
         Assert (Walk.Find (Top).Is_Empty);
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Find_Raised := True;
      end;

      begin
         Assert (Natural (Du.List (Top).Length) = 0);
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Du_Raised := True;
      end;

      Skip_Ls   := Walk.Ls (Locked, (Skip_Unreadable => True, others => <>));
      Skip_Find := Walk.Find (Top, (Skip_Unreadable => True, others => <>));
      Du_Items  := Natural (Du.List (Top, Skip_Unreadable => True).Length);

      Change_Access (Deny => False);

      Assert (Find_Raised);
      Assert (Du_Raised);
      Assert (Skip_Ls.Is_Empty);
      Assert (Has (Skip_Find, Locked));
      Assert (Has (Skip_Find, Top / "visible"));
      Assert (not Has (Skip_Find, Locked / "hidden"));
      Assert (Du_Items = 1);

      Delete_Tree (Top);
   end Check_Unreadable;

   --------------------
   -- Check_Too_Long --
   --------------------

   --  On Unix, a tree can go deeper than the longest path that the OS accepts
   --  by descending into it. That must not look like the end of the tree.

   Long_Name : constant String (1 .. 200) := (others => 'd');

   procedure Check_Too_Long is
      Top  : constant Path := "walk_options_too_long";
      Dest : constant Path := "woc"; -- Shorter, so the source fails first

      package Du is new Den.Du;

      --  A path past the limit, even though it's not known to exist
      function Long_Path (Levels : Positive) return Path
      is (if Levels = 1
          then Top / Long_Name
          else Long_Path (Levels - 1) / Long_Name);

      procedure Remove (Name : Path) is
      begin
         if Kind (Name) = Directory then
            Ada.Directories.Set_Directory (Name);
            Remove (Long_Name);
            Ada.Directories.Set_Directory ("..");
            Delete_Directory (Name);
         end if;
      end Remove;

      procedure Grow (Levels : Natural) is
      begin
         if Levels > 0 then
            Create_Directory (Long_Name);
            Ada.Directories.Set_Directory (Long_Name);
            Grow (Levels - 1);
            Ada.Directories.Set_Directory ("..");
         end if;
      end Grow;

      ------------
      -- Raises --
      ------------

      --  Says if the operation raises Use_Error instead of carrying on silently

      function Raises (Operation : access procedure) return Boolean is
      begin
         Operation.all;
         return False;
      exception
         when Ada.IO_Exceptions.Use_Error =>
            return True;
      end Raises;

      procedure Do_Find is
      begin
         Assert (not Walk.Find (Top).Is_Empty);
      end Do_Find;

      procedure Do_Find_Start is
      begin
         Assert (Walk.Find (Long_Path (30)).Is_Empty);
      end Do_Find_Start;

      procedure Do_Ls is
      begin
         Assert (Walk.Ls (Long_Path (30)).Is_Empty);
      end Do_Ls;

      procedure Do_Du is
      begin
         Assert (Natural (Du.List (Top).Length) = 1);
      end Do_Du;

      procedure Do_Copy is
      begin
         Copy (Top, Dest);
      end Do_Copy;

      procedure Do_Copy_Start is
      begin
         Copy (Long_Path (30), Dest);
      end Do_Copy_Start;

      procedure Do_Delete_File is
      begin
         Delete_File (Long_Path (30));
      end Do_Delete_File;

      procedure Do_Delete_Directory is
      begin
         Delete_Directory (Long_Path (30));
      end Do_Delete_Directory;

      procedure Do_Delete_Tree is
      begin
         Delete_Tree (Top);
      end Do_Delete_Tree;

      Raised_Find, Raised_Start, Raised_Ls, Raised_Du, Raised_Copy,
      Raised_Copy_Start, Raised_Del_File, Raised_Del_Dir, Raised_Del_Tree :
      Boolean;
   begin
      if Dir_Separator /= '/' then
         return;
      end if;

      Remove (Top);
      Remove (Dest);

      Create_Directory (Top);
      Ada.Directories.Set_Directory (Top);
      Grow (30);
      Ada.Directories.Set_Directory ("..");

      Raised_Find  := Raises (Do_Find'Access);
      Raised_Start := Raises (Do_Find_Start'Access);
      Raised_Ls    := Raises (Do_Ls'Access);
      Raised_Du    := Raises (Do_Du'Access);

      Create_Directory (Dest);
      Raised_Copy  := Raises (Do_Copy'Access);

      Raised_Copy_Start := Raises (Do_Copy_Start'Access);
      Raised_Del_File   := Raises (Do_Delete_File'Access);
      Raised_Del_Dir    := Raises (Do_Delete_Directory'Access);
      Raised_Del_Tree   := Raises (Do_Delete_Tree'Access);

      Remove (Top);
      Remove (Dest);

      Assert (Raised_Find, "Find stopped silently at the limit of the OS");
      Assert (Raised_Start, "Find took a long start as missing");
      Assert (Raised_Ls, "Ls took a long path as missing");
      Assert (Raised_Du, "Du stopped silently at the limit of the OS");
      Assert (Raised_Copy, "Copy failed unclearly at the limit of the OS");
      Assert (Raised_Copy_Start, "Copy took a long start as missing");
      Assert (Raised_Del_File, "Delete_File took a long path as missing");
      Assert (Raised_Del_Dir, "Delete_Directory took a long path as missing");
      Assert (Raised_Del_Tree, "Delete_Tree failed unclearly at the limit");
   end Check_Too_Long;

begin
   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;

   Create_Directory (Deep, (Create_Intermediate => True, others => <>));
   Touch (Test_Dir / "file");
   Touch (Sub / "file");
   Touch (Deep / "file");

   Check_Filter;
   Check_Root;
   Check_From_Root;
   Check_Ls;
   Check_Unreadable;
   Check_Too_Long;

   if Supported then
      --  These sort before what they point to

      Link (Test_Dir / "broken", "missing",
            Options => (Allow_Missing_Target => True, others => <>));
      Link (Test_Dir / "link_dir", "sub");
      Link (Test_Dir / "link_file", "file");

      Check_Enter;
      Check_Visit;
      Check_Dedupe;
      Check_Loops;
   else
      Ada.Text_IO.Put_Line ("Skipping link tests as they are not supported");
   end if;

   Delete_Tree (Test_Dir);
end Den_Tests.Walk_Options;
