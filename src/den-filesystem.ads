package Den.Filesystem is

   --  Basic subprograms that require actual access to the filesystem and hence
   --  cannot be preelaborable.

   function Absolute (This : Path) return Absolute_Path with
     Post => (if Is_Absolute (This) then Absolute'Result = This);

   function Absnormal (This : Path) return Absnormal_Path
   is (Normal (Absolute (This)));

   type Copy_Options is record
      Overwrite_Files : Boolean := False; -- When dst is a file
      Merge_Dirs      : Boolean := False; -- When both src/dst are dirs
      Resolve_Links   : Boolean := False; -- Copy target instead of link
   end record;

   procedure Copy (Src, Dst : Path;
                   Options  : Copy_Options := (others => <>));
   --  Make a copy of a file/dir/link. For dirs, this acts as rsync when paths
   --  end in '/' (this is also rclone behavior).

   type Create_Directory_Options is record
      Fail_If_Existing    : Boolean := False;
      Create_Intermediate : Boolean := False; -- like mkdir -p
   end record;

   procedure Create_Directory
     (Target           : Path;
      Options          : Create_Directory_Options := (others => <>));

   procedure Mkdir
     (Target           : Path;
      Options          : Create_Directory_Options := (others => <>))
      renames Create_Directory;

   function Current_Dir       return Path;
   function Current_Directory return Path renames Current_Dir;
   function CWD               return Path renames Current_Dir;

   type Delete_Directory_Options is record
      Recursive : Boolean := False;
   end record;

   procedure Delete_Directory
     (This : Path;
      Options : Delete_Directory_Options := (others => <>));

   procedure Rmdir
     (This    : Path;
      Options : Delete_Directory_Options := (others => <>))
      renames Delete_Directory;

   procedure Delete_Tree (This : Path);
   --  Recursive deletion

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
