pragma Alire_Test;

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

procedure Den_Tests.C_Interface is
   -- Import the C functions from denc.h
   function C_Canonical (Input_Path : chars_ptr;
                         Full_Path  : chars_ptr;
                         Bufsiz     : size_t) return int
     with Import, Convention => C, External_Name => "c_canonical";

   function C_Link_Target (Path   : chars_ptr;
                           Buffer : chars_ptr;
                           Bufsiz : size_t) return int
     with Import, Convention => C, External_Name => "c_link_target";

   function C_Delete_Link (Path : chars_ptr) return int
     with Import, Convention => C, External_Name => "c_delete_link";

   function C_Create_Link (Target : chars_ptr;
                           Name   : chars_ptr;
                           Is_Dir : int) return int
     with Import, Convention => C, External_Name => "c_create_link";

   -- Test variables
   Current_Dir : constant String := ".";

   -- Output buffers must not contain any nul, because New_Char_Array only
   -- allocates up to the first one

   -- Test the c_canonical function with a small buffer
   procedure Test_Canonical_Small_Buffer is
      Input_Path_Ptr : chars_ptr := New_String (Current_Dir);
      Buffer_Size    : constant size_t := 5; -- Intentionally small
      Buffer         : constant char_array (1 .. Buffer_Size) := (others => 'x');
      Buffer_Ptr     : chars_ptr := new_Char_Array (Buffer);
      Result         : Integer;
   begin
      Put_Line ("Testing c_canonical with small buffer (size=" &
                Buffer_Size'Image & ")");

      Result := Integer (C_Canonical (Input_Path_Ptr,
                                      Buffer_Ptr,
                                      Buffer_Size));

      Put_Line ("Result: " & Result'Image);

      -- Should return ERR_BUFFER_TOO_SMALL (-1) for a buffer that's too small
      pragma Assert (Result = -1,
                     "Expected -1 (ERR_BUFFER_TOO_SMALL) but got " &
                     Result'Image);

      Free (Input_Path_Ptr);
      Free (Buffer_Ptr);
   end Test_Canonical_Small_Buffer;

   -- Failures must report a positive system error, and in particular not -1,
   -- which asks the caller to grow the buffer and try again
   procedure Test_Errors_Are_Positive is
      Missing : chars_ptr := New_String ("den_tests_missing_path");
      Dir     : chars_ptr := New_String (Current_Dir);
      Target  : chars_ptr := New_String ("den_tests_missing_target");
      Buffer  : constant char_array (1 .. 64) := (others => 'x');
      Output  : chars_ptr := New_Char_Array (Buffer);

      procedure Check (Name : String; Result : int) is
      begin
         Put_Line (Name & " returned" & Result'Image);
         pragma Assert (Result > 0,
                        Name & " must return a positive error but got" &
                        Result'Image);
      end Check;
   begin
      Check ("c_canonical (missing)",
             C_Canonical (Missing, Output, Buffer'Length));
      Check ("c_link_target (missing)",
             C_Link_Target (Missing, Output, Buffer'Length));
      Check ("c_link_target (not a link)",
             C_Link_Target (Dir, Output, Buffer'Length));
      Check ("c_delete_link (missing)", C_Delete_Link (Missing));
      Check ("c_delete_link (not a link)", C_Delete_Link (Dir));
      Check ("c_create_link (existing)", C_Create_Link (Target, Dir, 0));

      Free (Missing);
      Free (Dir);
      Free (Target);
      Free (Output);
   end Test_Errors_Are_Positive;

   -- A file that is open for writing can still be queried
   procedure Test_Canonical_Of_Open_File is
      Name   : constant String := "c_interface_open_file";
      File   : File_Type;
      Input  : chars_ptr := New_String (Name);
      Buffer : constant char_array (1 .. 32768) := (others => 'x');
      Output : chars_ptr := New_Char_Array (Buffer);
      Result : int;
   begin
      Create (File, Out_File, Name);
      Result := C_Canonical (Input, Output, Buffer'Length);
      Delete (File);

      pragma Assert (Result = 0,
                     "Cannot canonicalize an open file, error" & Result'Image);

      Free (Input);
      Free (Output);
   end Test_Canonical_Of_Open_File;

   -- A UNC path must not come back relative, as "UNC\server\share"
   procedure Test_Canonical_Of_UNC is
      Input  : chars_ptr := New_String ("\\localhost\C$\Windows");
      Buffer : constant char_array (1 .. 32768) := (others => 'x');
      Output : chars_ptr := New_Char_Array (Buffer);
      Result : int;
   begin
      if GNAT.OS_Lib.Directory_Separator = '\' then
         Result := C_Canonical (Input, Output, Buffer'Length);

         if Result = 0 then
            Put_Line ("UNC path resolved to: " & Value (Output));
            pragma Assert (String'(Value (Output, 2)) = "\\",
                           "Not a UNC path: " & Value (Output));
         else
            Put_Line ("Skipping UNC test as the share is not available:"
                      & Result'Image);
         end if;
      end if;

      Free (Input);
      Free (Output);
   end Test_Canonical_Of_UNC;

begin
   Put_Line ("Starting C Interface Tests");

   -- Test c_canonical with a buffer that's too small
   Test_Canonical_Small_Buffer;

   Test_Canonical_Of_Open_File;

   Test_Canonical_Of_UNC;

   Test_Errors_Are_Positive;

   Put_Line ("C Interface Tests Completed Successfully");
end Den_Tests.C_Interface;
