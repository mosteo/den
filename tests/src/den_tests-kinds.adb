with Ada.Text_IO;

with Den;
--  with Den.Informer;

pragma Warnings (Off);

procedure Den_Tests.Kinds is
   use Ada.Text_IO;
   use Den;
   use Den.Operators;

   Cases : constant Path := ".." / "cases";
   Canon : constant Path := Canonical (Cases);
begin
   Assert (Kind (Cases) = Directory);
   Assert (Kind (Cases / "non-existent") = Nothing);
   Assert (Kind (Cases / "links" / "a") = File);

   --  Skip this test if softlinks are not supported
   if Kind ("canary") /= Softlink then
      Put_Line ("Skipping remaining tests as softlinks are not supported");
      return;
   end if;

   --  Valid link
   declare
      The_Path : constant Path := Cases / "links" / "b";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (not Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "links" / "a");
      Assert (Canonizable (The_Path));
      Assert (Canonical (The_Path) = Canon / "links" / "a");
      Assert (Kind (The_Path, Resolve_Links => True) = File);
   end;

   --  Valid 2-step link
   declare
      The_Path : constant Path := Cases / "links" / "c";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (not Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "links" / "b");
      Assert (Canonizable (The_Path));
      Assert (Canonical (The_Path) = Canon / "links" / "a");
      Assert (Kind (The_Path, Resolve_Links => True) = File);
   end;

   --  Link pointing to itself
   declare
      The_Path : constant Path := Cases / "links" / "d";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path)); -- Points to itself
      --  Assert (Is_Recursive (The_Path)); -- Points to itself
      Assert (Resolve (The_Path) = The_Path);
      Assert (not Canonizable (The_Path));
      Assert (Kind (The_Path, Resolve_Links => True) = Nothing);
   end;

   --  Broken link
   declare
      The_Path : constant Path := Cases / "links" / "e";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (Is_Broken (The_Path));
      --  Assert (not Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "links" / "missing");
      Assert (not Canonizable (The_Path));
      Assert (Kind (The_Path, Resolve_Links => True) = Nothing);
   end;

   --  Valid link to broken link
   declare
      The_Path : constant Path := Cases / "links" / "j";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (not Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "links" / "f");
      Assert (not Canonizable (The_Path));
      Assert (Kind (The_Path, Resolve_Links => True) = Nothing);
   end;

   --  Indirect looping link
   declare
      The_Path : constant Path := Cases / "loops" / "centi";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "loops" / "human");
      Assert (not Canonizable (The_Path));
      Assert (Kind (The_Path, Resolve_Links => True) = Nothing);
   end;

   --  Looping link through "."
   declare
      The_Path : constant Path := Cases / "loops" / "direct";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "loops" / "." / "direct");
      Assert (not Canonizable (The_Path));
      Assert (Kind (The_Path, Resolve_Links => True) = Nothing);
   end;

   --  Looping link through ".."
   declare
      The_Path : constant Path := Cases / "loops" / "loop";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "loops" / ".." / "loops" / "loop",
              "got: " & Resolve (The_Path));
      Assert (not Canonizable (The_Path));
      Assert (Kind (The_Path, Resolve_Links => True) = Nothing);
   end;

   --  Valid to "."
   declare
      The_Path : constant Path := Cases / "loops" / "this";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "loops" / ".");
      Assert (Canonizable (The_Path));
      Assert (Canonical (The_Path) = Canon / "loops");
      Assert (Kind (The_Path, Resolve_Links => True) = Directory);
   end;

   --  Valid to ".."
   declare
      The_Path : constant Path := Cases / "loops" / "parent";
   begin
      Assert (Kind (The_Path) = Softlink);
      Assert (not Is_Broken (The_Path));
      --  Assert (Is_Recursive (The_Path));
      Assert (Resolve (The_Path) = Cases / "loops" / "..");
      Assert (Canonizable (The_Path));
      Assert (Canonical (The_Path) = Canon);
      Assert (Kind (The_Path, Resolve_Links => True) = Directory);
   end;

end Den_Tests.Kinds;
