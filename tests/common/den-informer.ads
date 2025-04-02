package Den.Informer is

   function Debug return Boolean;
   function OS_Canonical (This : Path) return String;

private

   function Debug return Boolean
   is (Den.Debug);

   function OS_Canonical (This : Path) return String
   is (Den.OS_Canonical (This));

end Den.Informer;