pragma Alire_Test;

with Ada.IO_Exceptions;
with Ada.Text_IO;

with Den.Filesystem;

procedure Den_Tests.Link_Semantics is

   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Test_Dir : constant Path := "link_semantics_test_dir";

   Base   : constant Path := Test_Dir / "base";
   Real   : constant Path := Base / "real";
   Deep   : constant Path := Real / "deep";
   Target_File : constant Path := Real / "sibling";

   --  Through this, the parent of Deep is Real and not Base
   Shortcut : constant Path := Base / "shortcut";

   Relative_Link : constant Link_Options :=
                     (Relative_Target_From_Absolute_Path => True,
                      others                             => <>);

   -----------
   -- Touch --
   -----------

   procedure Touch (File_Path : Path) is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, File_Path);
      Ada.Text_IO.Close (F);
   end Touch;

   -------------------
   -- Check_Parents --
   -------------------

   --  A relative target starts at where the link really is, which isn't what
   --  the path to the link says when that path goes through another link.

   procedure Check_Parents is
   begin
      --  It exists, even if it doesn't seem so from the path given

      Link (Shortcut / "good", ".." / "sibling");
      Assert (Kind (Shortcut / "good") = Softlink);
      Assert (Canonical (Shortcut / "good") = Canonical (Target_File));

      --  It doesn't exist, even if it seems so

      Touch (Base / "decoy");
      begin
         Link (Shortcut / "bad", ".." / "decoy");
         raise Program_Error with "Created a broken link without asking";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Assert (Kind (Shortcut / "bad") = Nothing);
      end;

      --  Same when finding the relative path

      Link (Shortcut / "relative", Absnormal (Target_File), Relative_Link);
      Assert (Target (Shortcut / "relative") = ".." / "sibling");
      Assert (Canonical (Shortcut / "relative") = Canonical (Target_File));
   end Check_Parents;

   -----------------------
   -- Check_Link_Chains --
   -----------------------

   --  A link to a link to a folder is also a link to a folder

   procedure Check_Link_Chains is
   begin
      Link (Base / "first", "real");
      Link (Base / "second", "first");
      Assert (Target_Kind (Base / "second") = Directory);
      Assert (Kind (Base / "second" / "deep") = Directory);
   end Check_Link_Chains;

   ---------------------
   -- Check_Overwrite --
   ---------------------

   --  An existing link is not lost for a replacement that can't be created

   procedure Check_Overwrite is
      The_Link : constant Path := Base / "overwritten";
   begin
      Link (The_Link, "real");

      begin
         Link (The_Link, "missing", (Overwrite_Existing => True,
                                     others             => <>));
         raise Program_Error with "Linked to a missing target";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            Assert (Kind (The_Link) = Softlink);
            Assert (Target (The_Link) = "real");
      end;

      Touch (Base / "replacement");
      Link (The_Link, "replacement", (Overwrite_Existing => True,
                                      others             => <>));
      Assert (Target (The_Link) = "replacement");
   end Check_Overwrite;

   ----------------------
   -- Check_Own_Parent --
   ----------------------

   --  A link can point to the folder that holds it

   procedure Check_Own_Parent is
   begin
      Link (Real / "here", Absnormal (Real), Relative_Link);
      Assert (Target (Real / "here") = ".");
      Assert (Canonical (Real / "here") = Canonical (Real));
   end Check_Own_Parent;

   --------------------
   -- Check_Relative --
   --------------------

   procedure Check_Relative is
   begin
      --  A link to a folder is a good place to start from

      Assert (Relative (Shortcut, Base) = "..");
      Assert (Relative (Shortcut, Base, Canonicalize => True) = ".." / "..");

      --  Not so anything else

      begin
         Assert (Relative (Target_File, Base) = "");
         raise Program_Error with "Started from a file";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;

      Link (Base / "broken", "missing",
            Options => (Allow_Missing_Target => True, others => <>));
      begin
         Assert (Relative (Base / "broken", Base) = "");
         raise Program_Error with "Started from a broken link";
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;
      end;

      --  The same place twice has no relative path

      begin
         Assert (Relative (Base, Base) = "");
         raise Program_Error with "Relative path to the same place";
      exception
         when Bad_Operation =>
            null;
      end;
   end Check_Relative;

begin
   if Kind ("canary") /= Softlink then
      Ada.Text_IO.Put_Line ("Skipping tests as softlinks are not supported");
      return;
   end if;

   if Exists (Test_Dir) then
      Delete_Tree (Test_Dir);
   end if;

   Create_Directory (Deep, (Create_Intermediate => True, others => <>));
   Touch (Target_File);
   Link (Shortcut, "real" / "deep");

   Check_Parents;
   Check_Link_Chains;
   Check_Overwrite;
   Check_Own_Parent;
   Check_Relative;

   Delete_Tree (Test_Dir);
end Den_Tests.Link_Semantics;
