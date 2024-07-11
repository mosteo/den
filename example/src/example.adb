with Den.Walk;

with GNAT.IO; use GNAT.IO;

procedure Example is
   use Den;
begin
   Put_Line ("LS: " & Den.Current);
   for Path of Walk.Ls (".") loop
      Put_Line (Path & Explain (Path));
   end loop;

   for Canon in Walk.Canonical_Parts'Range loop
      New_Line;
      Put_Line ("CASES (" & Canon'Image & "):");
      for Item of Walk.Find ("../cases",
                            Options => (Canonicalize => Canon, others => <>))
      loop
         Put_Line (Item.Path & Explain (Item.Path));
      end loop;
   end loop;
end Example;
