private package Den.OS with Preelaborate is

   --  Imports for the common C functions in all supported OSes

   --  A path can't contain a NUL, which would cut it short for the OS, so such
   --  paths designate nothing for Kind_Of, and raise Bad_Path for the others.

   function Kind_Of (Path : String) return Kinds;
   --  Says what is at Path, without following it if it is a softlink

   function Canonical (Input_Path : String) return String;
   --  Returns the canonical path for Input_Path
   --  Raises Use_Error if operation fails

   function Full_Path (Path : String) return String;
   --  Returns the absolute form that Windows gives to Path, which may depend
   --  on the current drive or the current directory of a drive. Only meant for
   --  the paths that have those dependencies, which Windows alone has.
   --  Raises Use_Error if operation fails

   function Link_Target (Path : String) return String;
   --  Returns the target of the softlink at Path
   --  Raises Use_Error if operation fails

   procedure Delete_Link (Path : String);
   --  Delete the softlink at Path
   --  Raises Use_Error if operation fails

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean);
   --  Create a softlink at Name pointing to Target, flagging it as Is_Dir

end Den.OS;
