------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.FE_AADL_BA.PARSER.EXPRESSIONS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Locations; use Locations;

package Ocarina.FE_AADL_BA.Parser.Expressions is

   function P_Value_Holder (Container : Types.Node_Id) return Node_Id;
   --  parse an Value_Holder node, current token is ...

   function P_Value_Expression (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Relation (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Simple_Expressions (Start_Loc : Location) return List_Id;
   --  return a list of simple_expression and operator nodes

   function P_Simple_Expression (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Term (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Factor (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Primary (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Operator (Container : Types.Node_Id) return Node_Id;
   --  parse and return an operator node, current token is the operator

   function P_Property_Constant (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Integer_Range (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Integer_Value (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Behavior_Time (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

end Ocarina.FE_AADL_BA.Parser.Expressions;
