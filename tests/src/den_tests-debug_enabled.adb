with Ada.Environment_Variables;
with Ada.Text_IO;

with Den.Informer;

procedure Den_Tests.Debug_Enabled is
   use Ada.Text_IO;
begin
   Put_Line ("Test: Verifying debug logging is enabled");

   -- Now set the environment variable and check that Debug is True
   Ada.Environment_Variables.Set ("DEN_DEBUG", "1");

   if Den.Informer.Debug then
      Put_Line ("SUCCESS: Debug logging is enabled when DEN_DEBUG is set");
   else
      Put_Line ("ERROR: Debug is False even though DEN_DEBUG is set");
      raise Program_Error
        with "Debug logging test failed - Debug is False with DEN_DEBUG";
   end if;

   Put_Line ("Debug logging test completed successfully");
end Den_Tests.Debug_Enabled;
