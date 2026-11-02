private package Den.OS with Preelaborate is

   --  Imports for the common C functions in all supported OSes

   --  A path can't contain a NUL, which would cut it short for the OS, so such
   --  paths designate nothing for Kind_Of, and raise Bad_Path for the others.

   function Kind_Of (Path : String) return Kinds;
   --  Says what is at Path, without following it if it is a softlink

   procedure Check_Not_Too_Long (Path : String);
   --  Raises Use_Error if the OS rejects Path for being too long. Kind_Of gives
   --  Nothing for those, which must not be taken as if nothing was there by
   --  whatever is walking a tree.

   function Canonical (Input_Path : String) return String;
   --  Returns the canonical path for Input_Path
   --  Raises Use_Error if operation fails

   function Full_Path (Path : String) return String;
   --  Returns the absolute form that Windows gives to Path, which may depend
   --  on the current drive or the current directory of a drive. Only meant for
   --  the paths that have those dependencies, which Windows alone has.
   --  Raises Use_Error if operation fails

   function Current_Dir return String;
   --  Raises Use_Error if operation fails

   function File_Size (Path : String) return Long_Long_Integer;
   --  Raises Use_Error if operation fails

   procedure List (Path   : String;
                   Action : not null access procedure (Name : String));
   --  Calls Action with the name of every entry in the directory at Path,
   --  including "." and "..". Raises Use_Error if operation fails

   procedure Create_Directory (Path : String);
   --  Raises Use_Error if operation fails

   procedure Remove_Directory (Path : String);
   --  Raises Use_Error if operation fails

   procedure Delete_File (Path : String);
   --  Raises Use_Error if operation fails

   procedure Copy_File (Source, Target : String);
   --  Only on Windows. Elsewhere, the runtime does this as well without limits.
   --  Copies just the contents. Raises Use_Error if operation fails

   procedure Copy_Attributes (Source, Target : String;
                              Permissions    : Boolean;
                              Timestamps     : Boolean);
   --  Only on Windows, for the same reason. Raises Status_Error if it fails

   function Link_Target (Path : String) return String;
   --  Returns the target of the softlink at Path
   --  Raises Use_Error if operation fails

   procedure Delete_Link (Path : String);
   --  Delete the softlink at Path
   --  Raises Use_Error if operation fails

   procedure Copy_Link (Source, Name : String);
   --  Create at Name a link of the same kind as the one at Source and with the
   --  same target, whether that target exists or not.
   --  Raises Use_Error if operation fails

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean);
   --  Create a softlink at Name pointing to Target, flagging it as Is_Dir

end Den.OS;
