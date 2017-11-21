------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . L N T . C O M P O N E N T S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2016 ESA & ISAE.                       --
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

with Ocarina.Backends.Lnt.Nodes;
use Ocarina.Backends.Lnt.Nodes;

package Ocarina.Backends.Lnt.Components is

   function Make_Module_Definition
    (Defining_Identifier : Node_Id;
     Modules : List_Id := No_List;
     Predefined_Functions : List_Id := No_List;
     Module_Pragma : List_Id := No_List;
     Definitions : List_Id := No_List)
     return Node_Id;

   function Make_Type_Def
    (Defining_Identifier : Node_Id;
     Type_Exp : Node_Id;
     Type_Pragma : List_Id := No_List;
     Predefined_Functions : List_Id := No_List)
     return Node_Id;

   function Make_Type_Exp
    (Defining_Identifier : Node_Id := No_Node;
     Type_constructors : List_Id := No_List;
     Is_Set : boolean := false;
     Is_Sorted_Set : boolean := false;
     Is_List : boolean := false;
     Is_Sorted_List : boolean := false;
     Is_Array : boolean := false;
     Is_Range : boolean := false;
     RangeLNT : Node_Id := No_Node)
     return Node_Id;

   function Make_RangeLNT
     (Low_Bound  : Node_Id;
      High_Bound : Node_Id) return Node_Id;

   function Make_Type_Constructor
     (Defining_Identifier : Node_Id;
      Constructor_Parameters : List_Id := No_List;
      Constructor_Pragma : List_Id := No_List)
      return Node_Id;

   function Make_Parameter_Specification
     (Parameter_Var : Node_Id;
      Parameter_Type : Node_Id;
      Parameter_Mode : Mode_Id := Mode_In)
      return Node_Id;

   function Make_Function_Definition
    (Defining_Identifier : Node_Id;
     Function_Parameters : List_Id := No_List;
     Function_Return_Type : Node_Id := No_Node;
     Function_Exceptions : List_Id := No_List;
     Function_Pragma : List_Id := No_List;
     Statements : List_Id)
     return Node_Id;

   function Make_Actual_Parameter
     (Expression : Node_Id;
      Is_Out : boolean := false;
      Is_InOut : boolean := false)
      return Node_Id;

   function Make_Null_Statement return Node_Id;
   function Make_Return_Statement
    (Expression : Node_Id := No_Node;
     Is_Function : boolean := false)
    return Node_Id;

   function Make_Predefined_Function
    (Predefined_Function : Node_Kind;
     Is_With_Clause : boolean := false)
     return Node_Id;

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id)
     return Node_Id;

   function Make_Array_Element_Assignment_Statement
     (Defining_Identifier : Node_Id;
      Expression_Index    : Node_Id;
      Expression          : Node_Id)
     return Node_Id;

   function Make_Case_Statement
     (Expression                  : Node_Id;
      Variable_Declarations : List_Id := No_List;
      Case_Statement_Alternatives : List_Id)
     return Node_Id;

   function Make_Case_Statement_Alternative
     (Pattern_List : List_Id := No_List;
      Statements          : List_Id)
     return Node_Id;

   function Make_If_Statement
     (Condition        : Node_Id;
      Then_Statements  : List_Id;
      Elsif_Statements : List_Id := No_List;
      Else_Statements  : List_Id := No_List)
     return Node_Id;

   function Make_Elsif_Statement
     (Condition       : Node_Id;
      Then_Statements : List_Id)
     return Node_Id;

   function Make_Loop_Statement
      (Statements : List_Id)
     return Node_Id;

   function Make_While_Statement
     (Expression : Node_Id;
      Statements : List_Id)
     return Node_Id;

   function Make_Var_Declaration
     (Defining_Identifier : Node_Id;
      Var_Type : Node_Id)
    return Node_Id;

   function Make_Var_Statement
      (Variable_Declarations : List_Id;
       Statements : List_Id)
     return Node_Id;

   function Make_Nat (Value : Integer) return Node_Id;

   function Make_Expressions (Expression_List : List_Id)
     return Node_Id;

   function Make_Parenthesized_Expression (Expression : Node_Id)
     return Node_Id;

   function Make_Function_Call_Expression
     (Defining_Identifier : Node_Id;
      Parameters : List_Id := No_List)
     return Node_Id;

   function Make_Infix_Function_Call_Expression
     (Operator : Node_Id;
      Left_Part : Node_Id := No_Node;
      Right_Part : Node_Id)
     return Node_Id;

   function Make_Field_Selection_Expression
    (Expression : Node_Id;
     Field : Node_Id)
     return Node_Id;

   function Make_Field_Update_Expression
     (Expression : Node_Id;
      Field_Association : List_Id)
     return Node_Id;

   function Make_Element_Association
    (Index : Node_Id;
     Expression : Node_Id)
     return Node_Id;

   function Make_Array_Elt_Access_Expression
     (Index : Node_Id;
      Expression : Node_Id)
     return Node_Id;

   function Make_Pattern
    (Sub_Pattern : Node_Id := No_Node;
     Pattern_Type : Node_Id := No_Node;
     Is_Any : boolean := false;
     Is_Of : boolean := false)
     return Node_Id;

   function Make_Patterns (Pattern_List : List_Id)
     return Node_Id;

   function Make_Parenthesized_Pattern (Pattern : Node_Id)
     return Node_Id;

   function Make_Constructed_Pattern
     (Defining_Identifier : Node_Id;
      Parameters : List_Id := No_List)
     return Node_Id;

   function Make_Channel
     (Defining_Identifier : Node_Id;
      Gate_Profiles : List_Id)
     return Node_Id;

   function Make_Gate_Profile
     (Gate_Types : List_Id)
     return Node_Id;

   function Make_Process_Definition
    (Corresponding_Component : Node_Id := No_Node;
     Defining_Identifier : Node_Id;
     Process_Parameters : List_Id := No_List;
     Process_Gate_Declarations : List_Id := No_List;
     Process_Exceptions : List_Id := No_List;
     Process_Pragma : List_Id := No_List;
     Statements : List_Id)
     return Node_Id;

   function Make_Gate_Declaration
    (Channel_Name : Node_Id := No_Node;
     Gate : Node_Id;
     Is_Any : boolean := false)
     return Node_Id;

   function Make_Stop_Statement return Node_Id;

   function Make_Process_Instantiation_Statement
     (Defining_Identifier : Node_Id;
      Actual_Gates : List_Id := No_List;
      Actual_Parameters : List_Id := No_List;
      Is_Not_Periodic : Boolean := false)
     return Node_Id;

   function Make_Communication_Statement
     (Defining_Identifier : Node_Id;
      Offers : List_Id := No_List;
      Has_Where : boolean := false;
      Expression : Node_Id := No_Node)
     return Node_Id;

   function Make_Offer_Statement
    (Expression : Node_Id := No_Node;
     Pattern : Node_Id;
     Is_Input : boolean := false)
     return Node_Id;

   function Make_Select_Statement
     (Select_Statement_Alternatives : List_Id)
     return Node_Id;

   function Make_Select_Statement_Alternative
      (Statements : List_Id)
     return Node_Id;

   function Make_Parallel_Composition_Statement
     (Global_Synchronisation_Gates : List_Id := No_List;
      Interface_Synchronisations : List_Id)
     return Node_Id;

   function Make_Interface_Synchronisation
      (Interface_Synchronisation_Gates : List_Id := No_List;
       Statements : List_Id)
     return Node_Id;

   function Make_Module_Pragma
    (P_Type : Node_Kind;
     N_Constant : Integer)
    return Node_Id;

   function Make_Hide_Statement
      (Hide_Gate_Declarations : List_Id;
       Statements : List_Id)
     return Node_Id;

   function Make_Var_Loop_Select (Var_Dec : List_Id;
                                 Out_Loop : List_Id;
                                In_Select : List_Id;
                                With_Select : Boolean := true)
     return Node_Id;
end Ocarina.Backends.Lnt.Components;
