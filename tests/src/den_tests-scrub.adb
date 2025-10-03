with Den; use Den;

procedure Den_Tests.Scrub is
   use Den.Operators;

   Bad_Sep : constant Character :=
               (case Den.Dir_Separator is
                   when '/' => '\',
                   when '\' => '/',
                   when others =>
                     raise Program_Error with "Unsupported platform");

begin
   Assert ("a" = Den.Scrub ("a")); -- identity
   Assert ("a" = Den.Scrub ("a/")); -- trailing removal

   --  Duped separator simplification
   Assert ("a" / "b" = Den.Scrub ("a" & Dir_Separator & Dir_Separator & "b"));

   --  Don't correct other OSs separators (treat as regular char) (!!!)
   Assert ("a" & Bad_Sep & "b" = Scrub ("a" & Bad_Sep & "b"));

   --  Preserve initial \\ on Windows (and other platforms, but for other reasons)
   Assert ("\\a" = Scrub ("\\a"));

   --  No special treatment for initial "//"
   if Dir_Separator = '/' then
      Assert (Scrub ("//a") = "/a");
   end if;
end Den_Tests.Scrub;
