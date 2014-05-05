------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.FE_AADL_BA.PARSER.EXPRESSIONS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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

with Locations; use Locations;

package Ocarina.FE_AADL_BA.Parser.Expressions is

   function P_Value_Holder (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  parse an Value_Holder node, current token is ...

   function P_Value_Expression (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Relation (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Simple_Expressions (Start_Loc : Location) return List_Id;
   --  return a list of simple_expression and operator nodes

   function P_Simple_Expression (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Term (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Factor (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Primary (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Operator (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  parse and return an operator node, current token is the operator

   function P_Property_Constant (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Integer_Range (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Integer_Value (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Behavior_Time (Container : Ocarina.Types.Node_Id) return Node_Id;
   --  fixme : todo comment

end Ocarina.FE_AADL_BA.Parser.Expressions;
