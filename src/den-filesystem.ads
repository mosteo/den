package Den.Filesystem is

   --  Basic subprograms that require actual access to the filesystem and hence
   --  cannot be preelaborable.

   function Absolute (This : Path) return Absolute_Path with
     Post => (if Is_Absolute (This) then Absolute'Result = This);

   function Absnormal (This : Path) return Absnormal_Path
   is (Normal (Absolute (This)));

   function Current_Dir return Path;
   function CWD         return Path renames Current_Dir;

   function Pseudocanonical (This : Path) return Absolute_Path with
     Post => (if Canonizable (This)
                then Pseudocanonical'Result = Canonical (This));
   --  For broken links, the path will be canonical up to that point, with the
   --  link target appended. For recursive links, the path will be canonical
   --  and the simple name will remain the same. For paths with an intermediate
   --  softlink, it will be resolved if resolvable. When there are too many
   --  "..", they're dropped silently. If a broken link contains a string that
   --  is an invalid path, the link will not be resolved. SHOULD NEVER RAISE.

   --  Both Canonical and Pseudocanonical are expensive as they can make
   --  several system calls. For a cheaper alternative, when absolute normal
   --  paths suffice, use Absnormal, which does the same but resolving links.

   function Full (This : Path) return Absolute_Path renames Pseudocanonical;
   function Full_Name (This : Path) return Absolute_Path renames Full;

   function Relative (From, Into   : Path;
                      Canonicalize : Boolean := False)
                      return Path;
   --  Try to find a relative path from From into Into; this may be impossible
   --  on Windows for paths in different drive letters. From and Into are
   --  Absnormalized prior to search, unless Canonicalize, in which case
   --  they're Pseudocanonicalized. If no relative path can be found, an
   --  absolute path to Into will be returned. TODO/WARNING: consider whether
   --  the filesystem is case-insensitive or case-preserving. Currently no case
   --  transformations will be applied and case-sensitive will be presumed.

end Den.Filesystem;
