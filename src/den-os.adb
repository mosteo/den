with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with C_Strings; use C_Strings;

package body Den.OS is

   ------------------
   -- C_Canonical --
   ------------------

   function C_Canonical (Input_Path, Full_Path : Chars_Ptr;
                         Bufsiz               : C.size_t)
                         return C.int
      with Import, Convention => C;

   -------------------
   -- C_Is_Softlink --
   -------------------

   function C_Is_Softlink (Path : Chars_Ptr)
                           return C.int
      with Import, Convention => C;

   ----------------
   -- C_Link_Len --
   ----------------

   function C_Link_Len (Path : Chars_Ptr)
                        return C.int
      with Import, Convention => C;

   -------------------
   -- C_Link_Target --
   -------------------

   function C_Link_Target (Path, Buffer : Chars_Ptr;
                           Bufsiz       : C.size_t)
                           return C.int
      with Import, Convention => C;

   -------------------
   -- C_Delete_Link --
   -------------------

   function C_Delete_Link (Path : Chars_Ptr)
                           return C.int
      with Import, Convention => C;

   -------------------
   -- C_Create_Link --
   -------------------

   function C_Create_Link (Target,
                           Name   : Chars_Ptr;
                           Is_Dir : C_bool)
                           return C.int
      with Import, Convention => C;

   ---------------
   -- Canonical --
   ---------------

   function Canonical (Input_Path : String) return String is
      use type C.int;

      Bufsize : Integer := 32768; -- Theoretically, this is Windows max
   begin
      loop
         declare
            Cbuf : C_String := Buffer (Bufsize);
            Code : constant C.int
              := C_Canonical (To_C (Input_Path).To_Ptr,
                              Cbuf.To_Ptr,
                              Cbuf.C_Size);
         begin
            case Code is
               when 0 =>
                  return Cbuf.To_Ada;
               when -1 =>
                  if Integer'Last / 2 < Bufsize then
                     raise Use_Error with
                       "Buffer too small for canonical path: " & Input_Path;
                  else
                     Bufsize := Bufsize * 2;
                  end if;
                  --  And try again
               when others =>
                  raise Use_Error with
                    "Cannot canonicalize path: " & Input_Path &
                    " (error: " & Code'Image & ")";
            end case;
         end;
      end loop;
   end Canonical;

   -----------------
   -- Is_Softlink --
   -----------------

   function Is_Softlink (Path : String) return Boolean is
      use type C.int;
   begin
      return C_Is_Softlink (To_C (Path).To_Ptr) /= 0;
   end Is_Softlink;

   -----------------
   -- Link_Length --
   -----------------

   function Link_Length (Path : String) return Natural is
      use type C.int;

      Result : constant C.int := C_Link_Len (To_C (Path).To_Ptr);
   begin
      if Result < 0 then
         raise Constraint_Error with
           "Cannot get link length: " & Path &
           " (error: " & Result'Image & ")";
      end if;

      return Natural (Result);
   end Link_Length;

   -----------------
   -- Link_Target --
   -----------------

   function Link_Target (Path : String) return String is
      use type C.int;

      Initial_Length : constant Natural := Link_Length (Path);
      Bufsize : Integer := Initial_Length + 1;
   begin
      loop
         declare
            Cbuf : C_String := Buffer (Bufsize);
            Code : constant C.int
              := C_Link_Target (To_C (Path).To_Ptr,
                                Cbuf.To_Ptr,
                                Cbuf.C_Size);
         begin
            case Code is
               when 0 =>
                  return Cbuf.To_Ada;
               when -1 =>
                  if Integer'Last / 2 < Bufsize then
                     raise Use_Error with
                       "Buffer too small for link target: " & Path;
                  else
                     Bufsize := Bufsize * 2;
                  end if;
                  --  And try again
               when others =>
                  raise Use_Error with
                    "Cannot get link target: " & Path &
                    " (error: " & Code'Image & ")";
            end case;
         end;
      end loop;
   end Link_Target;

   -----------------
   -- Delete_Link --
   -----------------

   procedure Delete_Link (Path : String) is
      use type C.int;

      Result : constant C.int := C_Delete_Link (To_C (Path).To_Ptr);
   begin
      if Result /= 0 then
         raise Use_Error with
           "Cannot delete link: " & Path &
           " (error: " & Result'Image & ")";
      end if;
   end Delete_Link;

   -----------------
   -- Create_Link --
   -----------------

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean) is
      use C;

      Result : constant C.int :=
         C_Create_Link (Target => To_C (Target).To_Ptr,
                        Name   => To_C (Name).To_Ptr,
                        Is_Dir => C_bool (Is_Dir));
   begin
      if Result /= 0 then
         raise Use_Error with
            Error ("cannot create softlink "
                   & Name & " --> " & Target & P (Kind (Target)'Image)
                   & " (error: " & Result'Image & ")");
      end if;
   end Create_Link;

end Den.OS;
