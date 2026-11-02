with Ada.Text_IO; use Ada.Text_IO;

procedure Den.Root_Parsing is

   --  Path parsing rules for all platforms, so the Windows ones are checked
   --  even when not running on Windows.

   -----------
   -- Check --
   -----------

   procedure Check (Input    : String;
                    Windows  : Boolean;
                    Prefix   : Natural;
                    Absolute : Boolean;
                    Parts    : String)
   is
      Got_Parts : constant String := Split_Path (Input, Windows).Flatten ("|");
   begin
      Put_Line ("Checking " & Input & " (windows: " & Windows'Image & ")");
      pragma Assert (Prefix_Length (Input, Windows) = Prefix,
                     "prefix length:" & Prefix_Length (Input, Windows)'Image);
      pragma Assert (Absolute_For (Input, Windows) = Absolute,
                     "absolute: " & Absolute_For (Input, Windows)'Image);
      pragma Assert (Got_Parts = Parts, "parts: " & Got_Parts);
   end Check;

   -------------
   -- Windows --
   -------------

   procedure Windows (Input    : String;
                      Prefix   : Natural;
                      Absolute : Boolean;
                      Parts    : String)
   is
   begin
      Check (Input, True, Prefix, Absolute, Parts);
   end Windows;

   ----------
   -- Unix --
   ----------

   procedure Unix (Input    : String;
                   Prefix   : Natural;
                   Absolute : Boolean;
                   Parts    : String)
   is
   begin
      Check (Input, False, Prefix, Absolute, Parts);
   end Unix;

begin
   --  Plain relative paths

   Windows ("foo",         0, False, "foo");
   Windows ("foo\bar",     0, False, "foo|bar");
   Windows ("..\..",       0, False, "..|..");

   --  Both separators count, in any combination, and leave no empty parts

   Windows ("foo/bar",     0, False, "foo|bar");
   Windows ("foo/bar\baz", 0, False, "foo|bar|baz");
   Windows ("foo/",        0, False, "foo");
   Windows ("foo//bar",    0, False, "foo|bar");

   --  Rooted in the current drive, which isn't absolute

   Windows ("\",           1, False, "\");
   Windows ("/",           1, False, "\");
   Windows ("\foo",        1, False, "\|foo");
   Windows ("/foo/bar",    1, False, "\|foo|bar");

   --  Relative to the current directory of a drive, which isn't absolute

   Windows ("C:",          2, False, "C:");
   Windows ("c:foo",       2, False, "c:|foo");
   Windows ("C:foo\bar",   2, False, "C:|foo|bar");
   Windows ("C:foo/bar",   2, False, "C:|foo|bar");

   --  Drives

   Windows ("C:\",         3, True,  "C:\");
   Windows ("C:/",         3, True,  "C:\");
   Windows ("C:\foo",      3, True,  "C:\|foo");
   Windows ("C:/foo/bar",  3, True,  "C:\|foo|bar");
   Windows ("C:/foo\bar",  3, True,  "C:\|foo|bar");

   --  Not a drive letter

   Windows ("1:\foo",      0, False, "1:|foo");

   --  Network paths

   Windows ("\\",               2,  True, "\\");
   Windows ("\\srv",            2,  True, "\\|srv");
   Windows ("\\srv\",           2,  True, "\\|srv");
   Windows ("\\srv\share",      11, True, "\\srv\share\");
   Windows ("\\srv\share\",     12, True, "\\srv\share\");
   Windows ("\\srv\share\x\y",  12, True, "\\srv\share\|x|y");
   Windows ("//srv/share/x",    12, True, "\\srv\share\|x");

   --  Verbatim paths, where '/' isn't a separator

   Windows ("\\?\",             4,  True, "\\?\");
   Windows ("\\?\foo",          4,  True, "\\?\|foo");
   Windows ("\\?\C:",           6,  True, "\\?\C:\");
   Windows ("\\?\C:\",          7,  True, "\\?\C:\");
   Windows ("\\?\C:\foo\bar",   7,  True, "\\?\C:\|foo|bar");
   Windows ("\\?\C:\foo/bar",   7,  True, "\\?\C:\|foo/bar");
   Windows ("\\?\UNC\srv\sh",   14, True, "\\?\UNC\srv\sh\");
   Windows ("\\?\UNC\srv\sh\x", 15, True, "\\?\UNC\srv\sh\|x");
   Windows ("\\?\Volume{1}\x",  14, True, "\\?\Volume{1}\|x");

   --  Devices

   Windows ("\\.\",             4,  True, "\\.\");
   Windows ("\\.\COM1",         4,  True, "\\.\|COM1");

   --  Everywhere else, only a '/' is special

   Unix ("foo",     0, False, "foo");
   Unix ("foo/bar", 0, False, "foo|bar");
   Unix ("foo\bar", 0, False, "foo\bar");
   Unix ("/",       1, True,  "/");
   Unix ("/foo",    1, True,  "/|foo");
   Unix ("/foo/b",  1, True,  "/|foo|b");
   Unix ("\foo",    0, False, "\foo");
   Unix ("\\srv",   0, False, "\\srv");
   Unix ("C:",      0, False, "C:");
   Unix ("C:\foo",  0, False, "C:\foo");
end Den.Root_Parsing;
