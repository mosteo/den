with Den;

with GNAT.IO; use GNAT.IO;
with GNAT.OS_Lib;

procedure Example is
   package OS renames GNAT.OS_Lib;
   use Den;

   -------------
   -- Explain --
   -------------

   function Explain (S : Path) return String
   is (if Kind (S) = Softlink
       then " --> " & Target (S) &
         (if Is_Broken (S) then " (broken)" else "") &
         (if Is_Recursive (S) then " (recursive)" else "")
       elsif Kind (S) = Nothing
       then " (not found)"
       else "");

begin
   Put_Line ("CURRENT DIR LS: " & Den.Current);
   for Path of Den.Ls (".") loop
      Put_Line (Path);
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
end Example;
