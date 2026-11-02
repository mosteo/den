with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with C_Strings; use C_Strings;

with System;

package body Den.OS is

   use type C.int;

   --  Imports from denc.h

   function C_Canonical (Input_Path, Full_Path : Chars_Ptr;
                         Bufsiz                : C.size_t)
                         return C.int
      with Import, Convention => C;

   function C_Full_Path (Input_Path, Full_Path : Chars_Ptr;
                         Bufsiz                : C.size_t)
                         return C.int
      with Import, Convention => C;

   function C_Kind (Path : Chars_Ptr)
                    return C.int
      with Import, Convention => C;

   function C_Is_Too_Long (Path : Chars_Ptr)
                           return C.int
      with Import, Convention => C;

   function C_File_Size (Path : Chars_Ptr)
                         return Long_Long_Integer
      with Import, Convention => C;

   function C_Getcwd (Buffer : Chars_Ptr;
                      Bufsiz : C.size_t)
                      return C.int
      with Import, Convention => C;

   function C_Mkdir (Path : Chars_Ptr)
                     return C.int
      with Import, Convention => C;

   function C_Rmdir (Path : Chars_Ptr)
                     return C.int
      with Import, Convention => C;

   function C_Unlink (Path : Chars_Ptr)
                      return C.int
      with Import, Convention => C;

   function C_Dir_Open (Path  : Chars_Ptr;
                        Error : access C.int)
                        return System.Address
      with Import, Convention => C;

   function C_Dir_Read (Dir    : System.Address;
                        Buffer : Chars_Ptr;
                        Bufsiz : C.size_t)
                        return C.int
      with Import, Convention => C;

   procedure C_Dir_Close (Dir : System.Address)
      with Import, Convention => C;

   function C_Copy_File (Source, Target : Chars_Ptr)
                         return C.int
      with Import, Convention => C;

   function C_Copy_Attributes (Source,
                               Target      : Chars_Ptr;
                               Permissions,
                               Timestamps  : C.int)
                               return C.int
      with Import, Convention => C;

   function C_Link_Target (Path, Buffer : Chars_Ptr;
                           Bufsiz       : C.size_t)
                           return C.int
      with Import, Convention => C;

   function C_Delete_Link (Path : Chars_Ptr)
                           return C.int
      with Import, Convention => C;

   function C_Copy_Link (Source, Name : Chars_Ptr)
                         return C.int
      with Import, Convention => C;

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

   -----------
   -- Check --
   -----------

   procedure Check (Code : C.int; Action : String; Path : String) is
   begin
      if Code /= 0 then
         raise Use_Error with
           "Cannot " & Action & ": " & Path & " (error: " & Code'Image & ")";
      end if;
   end Check;

   ------------
   -- Filled --
   ------------

   --  Calls Fill with a growing buffer until it fits, and returns its contents

   function Filled (Action : String;
                    Path   : String;
                    Size   : Positive;
                    Fill   : not null access function (Buffer : Chars_Ptr;
                                                       Bufsiz : C.size_t)
                                                       return C.int)
                    return String
   is
      Bufsize : Integer := Size;
   begin
      Check_Path (Path);

      loop
         declare
            Cbuf : C_String := Buffer (Bufsize);
            Code : constant C.int := Fill (Cbuf.To_Ptr, Cbuf.C_Size);
         begin
            if Code = 0 then
               return Cbuf.To_Ada;
            elsif Code /= -1 then
               Check (Code, Action, Path);
            elsif Integer'Last / 2 < Bufsize then
               raise Use_Error with
                 "Cannot " & Action & ": " & Path & " (buffer too small)";
            else
               Bufsize := Bufsize * 2; -- And try again
            end if;
         end;
      end loop;
   end Filled;

   -------------
   -- Kind_Of --
   -------------

   function Kind_Of (Path : String) return Kinds
   is (if Has_NUL (Path)
       then Nothing
       else Kinds'Val (C_Kind (To_C (Path).To_Ptr)));

   ------------------------
   -- Check_Not_Too_Long --
   ------------------------

   procedure Check_Not_Too_Long (Path : String) is
   begin
      if not Has_NUL (Path) and then C_Is_Too_Long (To_C (Path).To_Ptr) /= 0
      then
         raise Use_Error with "Path too long for the OS: " & Path;
      end if;
   end Check_Not_Too_Long;

   ---------------
   -- Canonical --
   ---------------

   function Canonical (Input_Path : String) return String is

      function Fill (Buffer : Chars_Ptr; Bufsiz : C.size_t) return C.int
      is (C_Canonical (To_C (Input_Path).To_Ptr, Buffer, Bufsiz));

   begin
      return Filled ("canonicalize path", Input_Path, 32768, Fill'Access);
   end Canonical;

   ---------------
   -- Full_Path --
   ---------------

   function Full_Path (Path : String) return String is

      function Fill (Buffer : Chars_Ptr; Bufsiz : C.size_t) return C.int
      is (C_Full_Path (To_C (Path).To_Ptr, Buffer, Bufsiz));

   begin
      return Filled ("get full path", Path, 32768, Fill'Access);
   end Full_Path;

   -----------------
   -- Current_Dir --
   -----------------

   function Current_Dir return String is

      function Fill (Buffer : Chars_Ptr; Bufsiz : C.size_t) return C.int
      is (C_Getcwd (Buffer, Bufsiz));

   begin
      return Filled ("get current directory", "", 32768, Fill'Access);
   end Current_Dir;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (Path : String) return Long_Long_Integer is
   begin
      Check_Path (Path);

      declare
         Result : constant Long_Long_Integer :=
                    C_File_Size (To_C (Path).To_Ptr);
      begin
         if Result < 0 then
            Check (C.int (-Result), "get size of file", Path);
         end if;

         return Result;
      end;
   end File_Size;

   ----------
   -- List --
   ----------

   procedure List (Path   : String;
                   Action : not null access procedure (Name : String))
   is
      use type System.Address;

      Error : aliased C.int := 0;
      Dir   : System.Address;
      Cbuf  : C_String := Buffer (4096); -- Far over any limit on name lengths
   begin
      Check_Path (Path);

      Dir := C_Dir_Open (To_C (Path).To_Ptr, Error'Access);

      if Dir = System.Null_Address then
         Check (Error, "list directory", Path);
      end if;

      begin
         loop
            Check (C_Dir_Read (Dir, Cbuf.To_Ptr, Cbuf.C_Size),
                   "list directory", Path);

            declare
               Name : constant String := Cbuf.To_Ada;
            begin
               exit when Name = "";
               Action (Name);
            end;
         end loop;
      exception
         when others =>
            C_Dir_Close (Dir);
            raise;
      end;

      C_Dir_Close (Dir);
   end List;

   ----------------------
   -- Create_Directory --
   ----------------------

   procedure Create_Directory (Path : String) is
   begin
      Check_Path (Path);
      Check (C_Mkdir (To_C (Path).To_Ptr), "create directory", Path);
   end Create_Directory;

   ----------------------
   -- Remove_Directory --
   ----------------------

   procedure Remove_Directory (Path : String) is
   begin
      Check_Path (Path);
      Check (C_Rmdir (To_C (Path).To_Ptr), "remove directory", Path);
   end Remove_Directory;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Path : String) is
   begin
      Check_Path (Path);
      Check (C_Unlink (To_C (Path).To_Ptr), "delete file", Path);
   end Delete_File;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (Source, Target : String) is
   begin
      Check_Path (Source);
      Check_Path (Target);
      Check (C_Copy_File (To_C (Source).To_Ptr, To_C (Target).To_Ptr),
             "copy file", Source & " --> " & Target);
   end Copy_File;

   ---------------------
   -- Copy_Attributes --
   ---------------------

   procedure Copy_Attributes (Source, Target : String;
                              Permissions    : Boolean;
                              Timestamps     : Boolean)
   is
   begin
      Check_Path (Source);
      Check_Path (Target);

      declare
         Code : constant C.int :=
                  C_Copy_Attributes (To_C (Source).To_Ptr,
                                     To_C (Target).To_Ptr,
                                     Boolean'Pos (Permissions),
                                     Boolean'Pos (Timestamps));
      begin
         if Code /= 0 then
            raise Status_Error with
              "Cannot copy attributes: " & Source & " --> " & Target
              & " (error: " & Code'Image & ")";
         end if;
      end;
   end Copy_Attributes;

   -----------------
   -- Link_Target --
   -----------------

   function Link_Target (Path : String) return String is

      function Fill (Buffer : Chars_Ptr; Bufsiz : C.size_t) return C.int
      is (C_Link_Target (To_C (Path).To_Ptr, Buffer, Bufsiz));

   begin
      return Filled ("get link target", Path, 1024, Fill'Access);
   end Link_Target;

   -----------------
   -- Delete_Link --
   -----------------

   procedure Delete_Link (Path : String) is
   begin
      Check_Path (Path);
      Check (C_Delete_Link (To_C (Path).To_Ptr), "delete link", Path);
   end Delete_Link;

   ---------------
   -- Copy_Link --
   ---------------

   procedure Copy_Link (Source, Name : String) is
   begin
      Check_Path (Source);
      Check_Path (Name);
      Check (C_Copy_Link (To_C (Source).To_Ptr, To_C (Name).To_Ptr),
             "copy link", Source & " --> " & Name);
   end Copy_Link;

   -----------------
   -- Create_Link --
   -----------------

   procedure Create_Link (Target, Name : String; Is_Dir : Boolean) is
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
