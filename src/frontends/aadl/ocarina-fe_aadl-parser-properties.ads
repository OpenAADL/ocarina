------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L . P A R S E R . P R O P E R T I E S     --
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

--  This package gathers all functions dealing with properties. Some
--  parts of this package would fit in a Parser.Components.Properties
--  subpackage.

with Locations;       use Locations;

package Ocarina.FE_AADL.Parser.Properties is

   type Property_Association_Type is
     (PAT_Simple, PAT_Access, PAT_Simple_Or_Contained);
   --  This type is used for P_Property_Association parameter
   --     PAT_Simple              : parse only property_association
   --     PAT_Access              : parse only access_property_association
   --     PAT_Simple_Or_Contained : parse property_association or
   --                                     contained_property_association

   function P_Property_Association (Container : Ocarina.Types.Node_Id) return Node_Id;
   pragma Inline (P_Property_Association);
   --  Call P_Property_Association (PAT_Simple)

   function P_Property_Association_In_Component_Implementation
     (Container : Ocarina.Types.Node_Id)
     return Node_Id;
   pragma Inline (P_Property_Association_In_Component_Implementation);
   --  Call P_Property_Association (PAT_Simple_Or_Contained)

   function P_Property_Association
     (Container : Ocarina.Types.Node_Id;
      Property_Type : Property_Association_Type)
     return Node_Id;
   --  Parse Property_Association, property type depends on parameter
   --     PAT_Simple              : parse only property_association
   --     PAT_Access              : parse only access_property_association
   --     PAT_Simple_Or_Contained : parse property_association or
   --                                     contained_property_association

   function P_Property_Associations
     (Container : Ocarina.Types.Node_Id;
      Optional : Boolean;
      Property_Type : Property_Association_Type;
      Code : Parsing_Code)
     return Boolean;
   --  if Optional = TRUE then parse ( [ { { Property_Association }+ } ] )
   --                     else parse (   { { Property_Association }+ }   )

   function P_Property_Set
     (AADL_Spec : Ocarina.Types.Node_Id;
      Start_Loc : Location)
     return Node_Id;
   --  Current token must be 'set'

end Ocarina.FE_AADL.Parser.Properties;
