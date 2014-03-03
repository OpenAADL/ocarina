with GNATCOLL.Scripts;

package Ocarina.Python_Cmd is

   --  This package provides a central access to register functions to
   --  the Python bindings

   function Register_Scripts_And_Functions
     return GNATCOLL.Scripts.Scripts_Repository;
   --  Register the Python scripting language, and the functions we
   --  export

   procedure Initialize_Lib;
   pragma Export (C, Initialize_Lib, "initlibocarina_python");

   procedure Initialize;

private

   Repo : GNATCOLL.Scripts.Scripts_Repository;

end Ocarina.Python_Cmd;
