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

   -----------------
   -- C_Full_Path --
   -----------------

   function C_Full_Path (Input_Path, Full_Path : Chars_Ptr;
                         Bufsiz               : C.size_t)
                         return C.int
      with Import, Convention => C;

   ------------
   -- C_Kind --
   ------------

   function C_Kind (Path : Chars_Ptr)
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
                           Is_Dir : C.int)
                           return C.int
      with Import, Convention => C;

   -------------
   -- Has_NUL --
   -------------

   function Has_NUL (Path : String) return Boolean
   is (for some Char of Path => Char = ASCII.NUL);

   ----------------
   -- Check_Path --
   ----------------

   procedure Check_Path (Path : String) is
   begin
      if Has_NUL (Path) then
         raise Bad_Path with "Path contains a NUL character: " & Path;
      end if;
   end Check_Path;

   -------------
   -- Kind_Of --
   -------------

   function Kind_Of (Path : String) return Kinds
   is (if Has_NUL (Path)
       then Nothing
       else Kinds'Val (C_Kind (To_C (Path).To_Ptr)));

   ---------------
   -- Canonical --
   ---------------

   function Canonical (Input_Path : String) return String is
      use type C.int;

      Bufsize : Integer := 32768; -- Theoretically, this is Windows max
   begin
      Check_Path (Input_Path);

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

   ---------------
   -- Full_Path --
   ---------------

   function Full_Path (Path : String) return String is
      use type C.int;

      Bufsize : Integer := 32768; -- Theoretically, this is Windows max
   begin
      Check_Path (Path);

      loop
         declare
            Cbuf : C_String := Buffer (Bufsize);
            Code : constant C.int
              := C_Full_Path (To_C (Path).To_Ptr,
                              Cbuf.To_Ptr,
                              Cbuf.C_Size);
         begin
            case Code is
               when 0 =>
                  return Cbuf.To_Ada;
               when -1 =>
                  if Integer'Last / 2 < Bufsize then
                     raise Use_Error with
                       "Buffer too small for full path: " & Path;
                  else
                     Bufsize := Bufsize * 2;
                  end if;
                  --  And try again
               when others =>
                  raise Use_Error with
                    "Cannot get full path: " & Path &
                    " (error: " & Code'Image & ")";
            end case;
         end;
      end loop;
   end Full_Path;

   -----------------
   -- Link_Target --
   -----------------

   function Link_Target (Path : String) return String is
      use type C.int;

      Bufsize : Integer := 1024; -- Enough almost always
   begin
      Check_Path (Path);

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
   begin
      Check_Path (Path);

      declare
         Result : constant C.int := C_Delete_Link (To_C (Path).To_Ptr);
      begin
         if Result /= 0 then
            raise Use_Error with
              "Cannot delete link: " & Path &
              " (error: " & Result'Image & ")";
         end if;
      end;
   end Delete_Link;

   -----------------
   -- Create_Link --
   -----------------

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean) is
      use C;
   begin
      Check_Path (Target);
      Check_Path (Name);

      declare
         Result : constant C.int :=
            C_Create_Link (Target => To_C (Target).To_Ptr,
                           Name   => To_C (Name).To_Ptr,
                           Is_Dir => Boolean'Pos (Is_Dir));
      begin
         if Result /= 0 then
            raise Use_Error with
               Error ("cannot create softlink "
                      & Name & " --> " & Target & P (Kind (Target)'Image)
                      & " (error: " & Result'Image & ")");
         end if;
      end;
   end Create_Link;

end Den.OS;
