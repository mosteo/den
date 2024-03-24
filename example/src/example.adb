with Den;

with GNAT.IO; use GNAT.IO;

procedure Example is
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

   Put_Line (Den.Target_Length ("cases/links/e")'Image);
end Example;
