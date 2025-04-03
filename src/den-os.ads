private package Den.OS with Preelaborate is

   type C_bool is new Boolean with
      Convention => C;
   --  Not available in old GNATs

   --  Imports for the common C functions in all supported OSes

   function Canonical (Input_Path : String) return String;
   --  Returns the canonical path for Input_Path
   --  Raises Use_Error if operation fails

   function Is_Softlink (Path : String) return Boolean;
   --  Returns True if Path is a softlink

   function Link_Length (Path : String) return Natural;
   --  Returns the length of the target of the softlink at Path
   --  Raises Constraint_Error if Path is not a softlink

   function Link_Target (Path : String) return String;
   --  Returns the target of the softlink at Path
   --  Raises Use_Error if operation fails

   procedure Delete_Link (Path : String);
   --  Delete the softlink at Path
   --  Raises Use_Error if operation fails

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean);
   --  Create a softlink at Name pointing to Target, flagging it as Is_Dir

end Den.OS;
