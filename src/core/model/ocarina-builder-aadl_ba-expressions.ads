------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B U I L D E R . A A D L _ B A . E X P R E S S I O N S   --
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

with Ocarina.Types;
with Locations;

with Ocarina.ME_AADL_BA;

package Ocarina.Builder.AADL_BA.Expressions is

   use Ocarina.Types;
   use Locations;
   use Ocarina.ME_AADL_BA;

   function Add_New_Value_Variable
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Is_A_Count   : Boolean  := False;
      Is_A_Fresh   : Boolean  := False;
      Is_A_Updated : Boolean  := False;
      Is_A_Interro : Boolean  := False)
     return Node_Id;

   function Add_New_Value_Expression
     (Loc           : Location;
      Container     : Node_Id;
      Relation_List : List_Id)
     return Node_Id;

   function Add_New_Relation
     (Loc           : Location;
      Container     : Node_Id;
      Spl_Expr_List : List_Id   := No_List)
     return Node_Id;

   function Add_New_Simple_Expression
     (Loc              : Location;
      Container        : Node_Id;
      Simple_Expr_List : List_Id)
     return Node_Id;

   function Add_New_Term
     (Loc         : Location;
      Container   : Node_Id;
      Factor_List : List_Id)
     return Node_Id;

   function Add_New_Factor
     (Loc         : Location;
      Container   : Node_Id;
      Is_Abs_Bool : Boolean;
      Is_Not_Bool : Boolean;
      Low_Value : Node_Id;
      Upp_Value : Node_Id)
     return Node_Id;

   function Add_New_Property_Constant
     (Loc             : Location;
      Container       : Node_Id;
      Property_Set_Id : Node_Id;
      Property_Cst_Id : Node_Id)
     return Node_Id;

   function Add_New_Integer_Range
     (Loc         : Location;
      Container   : Node_Id;
      Lower_Bound : Node_Id;
      Upper_Bound : Node_Id)
     return Node_Id;

   function Add_New_Integer_Value
     (Loc         : Location;
      Container   : Node_Id;
      Entity_Node : Node_Id)
     return Node_Id;

   function Add_New_Behavior_Time
     (Loc         : Location;
      Container   : Node_Id;
      Integer_Val : Node_Id;
      Unit_Ident  : Node_Id)
     return Node_Id;

   function Add_New_Operator
     (Loc         : Location;
      Container   : Node_Id;
      Operat_Kind : Operator_Kind)
     return Node_Id;

   function Add_New_Property_Reference
     (Loc             : Location;
      Container       : Node_Id;
      Property_Set_Id : Node_Id;
      Entity          : Node_Id;
      Property_Name   : Node_Id)
     return Node_Id;

   function Add_New_Property_Name
     (Loc            : Location;
      Container      : Node_Id;
      Property_Idt   : Node_Id;
      Property_Fields : List_Id)
     return Node_Id;

   function Add_New_Component_Element_Ref
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Is_Self_Bool : Boolean := False)
     return Node_Id;

   function Add_New_Property_Field
     (Loc            : Location;
      Container      : Node_Id;
      Ident          : Node_Id;
      Is_Upper_Bound : Boolean := False;
      Is_Lower_Bound : Boolean := False)
     return Node_Id;

end Ocarina.Builder.AADL_BA.Expressions;
