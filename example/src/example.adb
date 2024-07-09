with Den;

with GNAT.IO; use GNAT.IO;
with GNAT.OS_Lib;

procedure Example is
   package OS renames GNAT.OS_Lib;
   use Den;
begin
   Put_Line ("LS: " & Den.Current);
   for Path of Den.Ls (".") loop
      Put_Line (Path & Explain (Path));
   end loop;

   for Canon in Den.Canonical_Parts'Range loop
      New_Line;
      Put_Line ("CASES (" & Canon'Image & "):");
      for Item of Den.Find ("cases",
                            Options => (Canonicalize => Canon, others => <>))
      loop
         Put_Line (Item.Path & Explain (Item.Path));
      end loop;
   end loop;

   New_Line;
   Put_Line ("Canonicalize custom:");
   Put_Line (OS.Normalize_Pathname ("cases/links/f", Resolve_Links => False));
   Put_Line (Canonical ("cases/links/f")
             & Explain (Canonical ("cases/links/f")));

   Put_Line (Kind ("there")'Image);
   Put_Line (Target_Length ("there")'Image);
   Put_Line (Target ("there"));
end Example;
