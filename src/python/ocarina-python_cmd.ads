------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                   O C A R I N A . P Y T H O N _ C M D                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2014-2015 ESA & ISAE.                    --
--                                                                          --
-- Ocarina  is free software; you can redistribute it and/or modify under   --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. Ocarina is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY; without even the implied warranty of               --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
--  Silence all warnings

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
