with Den;

with GNAT.IO; use GNAT.IO;

procedure Example is
begin
   Put_Line ("CURRENT DIR LS: " & Den.Current);
   for Path of Den.Ls (".") loop
      Put_Line (Path);
   end loop;
end Example;
