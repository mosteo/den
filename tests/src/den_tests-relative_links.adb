pragma Warnings (Off);

with Ada.Assertions;
with Ada.Text_IO;
with Den.Filesystem;

procedure Den_Tests.Relative_Links is
   use Ada.Text_IO;
   use Den;
   use Den.Filesystem;
   use Den.Operators;

   Cases : constant Path := ".." / "cases";
begin
   Assert (Canonizable (Cases / "links" / "b"));

   --  Plain link to file
   declare
      Test_Link   : constant Path := "mylink";
      Test_Target : constant Path := Cases / "links" / "a";
   begin
      Link (Test_Link, Test_Target);
      Assert (Canonizable (Test_Link));
      Assert (Canonical (Test_Link) = Canonical (Test_Target));
      Unlink (Test_Link);
   end;

   --  Link in nested dir to file
   declare
      Test_Link   : constant Path := "mydir" / "mylink";
      Test_Target : constant Path := ".." / Cases / "links" / "a";
   begin
      Create_Directory ("mydir");
      Link (Test_Link, Test_Target);
      Assert (Canonizable (Test_Link));
      Assert (Canonical (Test_Link) = Canonical (Parent (Test_Link) / Test_Target),
              " Unexpected: " & Canonical (Test_Link) & " /= " & Canonical (Parent (Test_Link) / Test_Target));
      Unlink (Test_Link);
      Delete_Directory ("mydir");
   end;

   --  Plain link to folder
   declare
      Test_Link   : constant Path := "mydirlink";
      Test_Target : constant Path := Cases / "links";
   begin
      Link (Test_Link, Test_Target);
      Assert (Canonizable (Test_Link));
      Assert (Canonical (Test_Link) = Canonical (Test_Target));
      Unlink (Test_Link);
   end;

   --  Link in nested dir to folder
   declare
      Test_Link   : constant Path := "mydir" / "mydirlink";
      Test_Target : constant Path := ".." / Cases / "links";
   begin
      Create_Directory ("mydir");
      Link (Test_Link, Test_Target);
      Assert (Canonizable (Test_Link));
      Assert (Canonical (Test_Link) = Canonical (Parent (Test_Link) / Test_Target),
              " Unexpected: " & Canonical (Test_Link) & " /= " & Canonical (Parent (Test_Link) / Test_Target));
      Unlink (Test_Link);
      Delete_Directory ("mydir");
   end;

   --  Broken link
   declare
      Test_Link   : constant Path := "mylink";
      Test_Target : constant Path := "faketarget";
   begin
      Link (Test_Link, Test_Target,
            Options => (Allow_Missing_Target => True, others => <>));
      Assert (not Canonizable (Test_Link));
      Assert (Resolve (Test_Link) = "faketarget");
      Unlink (Test_Link);
   end;

   --  Broken link in nested dir that would be correct but for an extra ".."
   declare
      Test_Link   : constant Path := "mydir" / "mylink";
      Test_Target : constant Path := Cases / "links" / "a";
   begin
      Create_Directory ("mydir");
      Link (Test_Link, Test_Target,
            Options => (Allow_Missing_Target => True, others => <>));
      Assert (not Canonizable (Test_Link));
      Assert (Resolve (Test_Link) = "mydir" / Cases / "links" / "a");
      Unlink (Test_Link);
      Delete_Directory ("mydir");
   end;

end Den_Tests.Relative_Links;
