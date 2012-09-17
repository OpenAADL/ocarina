------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.FE_AADL.PARSER.PROPERTIES.VALUES                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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

--  This package contains the functions that deal with the property
--  values parsing

package Ocarina.FE_AADL.Parser.Properties.Values is

   type Number_Category is (NC_Integer, NC_Real, NC_Unknown);

   function P_Multi_Valued_Property return Node_Id;
   --  Current token must be 'list'

   function P_Property_Constant
     (Identifier   : Node_Id;
      Property_Set : Node_Id)
     return Node_Id;
   --  Current token must be 'constant'

   function P_Property_Expression (Container : Node_Id) return Node_Id;

   function P_Property_Type_Declaration
     (Identifier   : Node_Id;
      Property_Set : Node_Id)
     return Node_Id;
   --  Current token must be 'type'

   function P_Single_Valued_Property return Node_Id;

   function P_Named_Element return Node_Id;
   --  Parse Element of Named_Element

   function P_Signed_AADLNumber
     (Number_Cat : Number_Category;
      Code       : Parsing_Code)
     return Node_Id;
   --  If Number_Cat = NC_Real then parse Signed_AADLReal else if
   --  Number_Cat = NC_Integer then parse Signed_AADLInteger else
   --  parse Signed_AADLReal or Signed_AADLInteger.

   function P_Contained_Element_Path (Container : Node_Id) return Node_Id;

   function P_Numeric_Term (Code : Parsing_Code) return Node_Id;

end Ocarina.FE_AADL.Parser.Properties.Values;
