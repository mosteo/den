with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

procedure Den_Tests.C_Interface is
   -- Import the C functions from denc.h
   function C_Canonical (Input_Path : chars_ptr;
                         Full_Path  : chars_ptr;
                         Bufsiz     : size_t) return int
     with Import, Convention => C, External_Name => "c_canonical";

   function C_Link_Target (Path   : chars_ptr;
                           Buf    : chars_ptr;
                           Bufsiz : size_t) return int
     with Import, Convention => C, External_Name => "c_link_target";

   function C_Copy_Link (Target : chars_ptr;
                         Name   : chars_ptr) return int
     with Import, Convention => C, External_Name => "c_copy_link";

   function C_Delete_Link (Path : chars_ptr) return int
     with Import, Convention => C, External_Name => "c_delete_link";

   -- Test variables
   Current_Dir : constant String := ".";
   Test_Link   : constant String := "test_link";
   Test_Target : constant String := "test_target";

   -- Test the c_canonical function with a small buffer
   procedure Test_Canonical_Small_Buffer is
      Input_Path_Ptr : chars_ptr := New_String (Current_Dir);
      Buffer_Size    : constant size_t := 5; -- Intentionally small
      Buffer         : constant char_array (1 .. Buffer_Size) := (others => nul);
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

   -- Test the c_link_target function with a small buffer
   procedure Test_Link_Target_Small_Buffer is
      -- Create a test link
      Target_Ptr : chars_ptr := New_String (Test_Target);
      Link_Ptr   : chars_ptr := New_String (Test_Link);
      Create_Result : Integer;

      -- Test variables
      Buffer_Size : constant size_t := 5; -- Intentionally small
      Buffer      : constant char_array (1 .. Buffer_Size) := (others => nul);
      Buffer_Ptr  : chars_ptr := new_Char_Array (Buffer);
      Result      : Integer;
   begin
      Put_Line ("Testing c_link_target with small buffer");

      -- Create a test link
      Create_Result := Integer (C_Copy_Link (Target_Ptr, Link_Ptr));

      -- Only proceed if link creation was successful
      if Create_Result = 0 then
         -- Test with a small buffer
         Result := Integer (C_Link_Target (Link_Ptr,
                                          Buffer_Ptr,
                                          Buffer_Size));

         Put_Line ("Result: " & Result'Image);

         -- Should return ERR_BUFFER_TOO_SMALL (-1) for a buffer that's too small
         pragma Assert (Result = -1,
                        "Expected -1 (ERR_BUFFER_TOO_SMALL) but got " &
                        Result'Image);

         -- Clean up the link
         declare
            pragma Warnings (Off);
            Unused : Integer := Integer (C_Delete_Link (Link_Ptr));
            pragma Warnings (On);
         begin
            null;
         end;
      else
         Put_Line ("Warning: Could not create test link, skipping test");
      end if;

      Free (Target_Ptr);
      Free (Link_Ptr);
      Free (Buffer_Ptr);
   end Test_Link_Target_Small_Buffer;

begin
   Put_Line ("Starting C Interface Tests");

   -- Test c_canonical with a buffer that's too small
   Test_Canonical_Small_Buffer;

   -- Test c_link_target with a buffer that's too small
   Test_Link_Target_Small_Buffer;

   Put_Line ("C Interface Tests Completed Successfully");
end Den_Tests.C_Interface;
