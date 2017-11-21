------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L . P A R S E R . N A M E S P A C E S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

--  This package gathers all parsing functions related to AADL
--  packages

with Locations;

package Ocarina.FE_AADL.Parser.Namespaces is

   function P_AADL_Declaration
     (AADL_Specification : Types.Node_Id) return Node_Id;

   function P_AADL_Specification
     (AADL_Specification : Types.Node_Id) return Node_Id;
   --  Parse an AADL specification. Append it to AADL_Specification if
   --  it is not No_Node

   function P_Package_Specification (Namespace : Types.Node_Id) return Node_Id;

   function P_Package_Name (Container : Types.Node_Id) return Node_Id;
   --  Parse a package name and returns a new identifier containing
   --  this name.

   function P_Import_Declaration
     (Namespace            : Types.Node_Id;
      Start_Loc            : Locations.Location;
      Private_Declarations : Boolean := False) return Types.Node_Id;
   --  Parse an import declaration and returns a node containing a
   --  list of package_name or identifier

end Ocarina.FE_AADL.Parser.Namespaces;
