with Den;

with GNAT.IO; use GNAT.IO;
with GNAT.OS_Lib;

procedure Example is
   package OS renames GNAT.OS_Lib;
   use Den;
begin
   Put_Line ("CURRENT DIR LS: " & Den.Current);
   for Path of Den.Ls (".") loop
      Put_Line (Path);
   end loop;

   Put_Line ("CASES (recursive):");
   for Item of Den.Find ("cases") loop
      Put_Line (Item.Path);
   end loop;

   Put_Line ("CASES (normalized):");
   for Item of Den.Find ("cases/",
                         Options => (Normalize_Paths => True,
                                     others          => <>))
   loop
      Put_Line (Item.Path);
   end loop;

   Put_Line (OS.Normalize_Pathname ("cases/links/f", Resolve_Links => False));
   Put_Line (Full_Path ("cases/links/f", False));
   Put_Line (Full_Path ("cases/links/f", True));
end Example;
