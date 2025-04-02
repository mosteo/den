with Ada.IO_Exceptions; use Ada.IO_Exceptions;

package body Den.OS is

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean) is
      use C;

      Result : constant C.int :=
         C_Create_Link (Target => To_C (Target).To_Ptr,
                        Name   => To_C (Name).To_Ptr,
                        Is_Dir => C.C_bool (Is_Dir));
   begin
      if Result /= 0 then
         raise Use_Error with
            Error ("cannot create softlink "
                   & Name & " --> " & Target & P (Kind (Target)'Image)
                   & " (error: " & Result'Image & ")");
      end if;
   end Create_Link;

end Den.OS;