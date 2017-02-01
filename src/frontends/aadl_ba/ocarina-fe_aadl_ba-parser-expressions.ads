------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.FE_AADL_BA.PARSER.EXPRESSIONS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

with Locations; use Locations;

package Ocarina.FE_AADL_BA.Parser.Expressions is

   function P_Value_Variable (Container : Types.Node_Id) return Node_Id;
   --  parse an Value_Variable node, current token is ...

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

   function P_Value (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Operator (Container : Types.Node_Id) return Node_Id;
   --  parse and return an operator node, current token is the operator

   function P_Property_Constant (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Property_Ref (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Property_Name (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Property_Field (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Component_Element_Ref (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Integer_Range (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Integer_Value (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

   function P_Behavior_Time (Container : Types.Node_Id) return Node_Id;
   --  fixme : todo comment

end Ocarina.FE_AADL_BA.Parser.Expressions;
