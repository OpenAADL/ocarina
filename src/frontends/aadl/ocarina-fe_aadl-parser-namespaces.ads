------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L . P A R S E R . N A M E S P A C E S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2014 ESA & ISAE.      --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
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
