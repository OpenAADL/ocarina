------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . L N T . C O M P O N E N T S       --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Backends.Lnt.Nutils; use Ocarina.Backends.Lnt.Nutils;

with Ocarina.Namet; use Ocarina.Namet;
with Utils;         use Utils;

package body Ocarina.Backends.Lnt.Components is

   ---------------------------------
   --    Make_Module_Definition   --
   ---------------------------------
   function Make_Module_Definition
    (Defining_Identifier : Node_Id;
     Modules : List_Id := No_List;
     Predefined_Functions : List_Id := No_List;
     Module_Pragma : List_Id := No_List;
     Definitions : List_Id := No_List)
     return Node_Id
   is
      pragma Assert (Defining_Identifier /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Module_Definition);
      Set_Identifier (N, Defining_Identifier);
      Set_Modules (N, Modules);
      Set_Predefined_Functions (N, Predefined_Functions);
      Set_Module_Pragmas (N, Module_Pragma);
      Set_Definitions (N, Definitions);
      return N;
   end Make_Module_Definition;

   function Make_Type_Def
    (Defining_Identifier : Node_Id;
     Type_Exp : Node_Id;
     Type_Pragma : List_Id := No_List;
     Predefined_Functions : List_Id := No_List)
     return Node_Id
   is
      pragma Assert ((Defining_Identifier /= No_Node) and then
                    (Type_Exp /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Type_Def);
      Set_Identifier (N, Defining_Identifier);
      Set_Type_Exp (N, Type_Exp);
      Set_Predefined_Functions (N, Predefined_Functions);
      Set_Type_Pragma (N, Type_Pragma);
      Set_Predefined_Functions (N, Predefined_Functions);
      return N;
   end Make_Type_Def;

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
     return Node_Id
   is
      pragma Assert ((Defining_Identifier /= No_Node) or else
                    (Type_constructors /= No_List));
      N : Node_Id;
   begin
      N := New_Node (K_Type_Exp);
      Set_Identifier (N, Defining_Identifier);
      Set_Type_constructors (N, Type_constructors);
      Set_Is_Set (N, Is_Set);
      Set_Is_Sorted_Set (N, Is_Sorted_Set);
      Set_Is_List (N, Is_List);
      Set_Is_Sorted_List (N, Is_Sorted_List);
      Set_Is_Array (N, Is_Array);
      Set_Is_Range (N, Is_Range);
      Set_RangeLNT (N, RangeLNT);
      return N;
   end Make_Type_Exp;

   -------------------
   -- Make_RangeLNT --
   -------------------
   function Make_RangeLNT
     (Low_Bound  : Node_Id;
      High_Bound : Node_Id) return Node_Id
   is
      pragma Assert ((Low_Bound /= No_Node) and then
                    (High_Bound /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_RangeLNT);
      Set_Low_Bound  (N, Low_Bound);
      Set_High_Bound (N, High_Bound);
      return N;
   end Make_RangeLNT;

   function Make_Type_Constructor
    (Defining_Identifier : Node_Id;
     Constructor_Parameters : List_Id := No_List;
     Constructor_Pragma : List_Id := No_List)
     return Node_Id is
      pragma Assert (Defining_Identifier /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Type_constructor);
      Set_Identifier (N, Defining_Identifier);
      Set_Constructor_Parameters (N, Constructor_Parameters);
      Set_Constructor_Pragma (N, Constructor_Pragma);
      return N;
   end Make_Type_Constructor;

   ----------------------------------
   -- Make_Parameter_Specification --
   ----------------------------------
   function Make_Parameter_Specification
     (Parameter_Var : Node_Id;
      Parameter_Type : Node_Id;
      Parameter_Mode : Mode_Id := Mode_In)
    return Node_Id is
      pragma Assert ((Parameter_Type /= No_Node) and then
                    (Parameter_Var /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Parameter_Specification);
      Set_Parameter_Mode (N, Parameter_Mode);
      Set_Parameter_Var (N, Parameter_Var);
      Set_Parameter_Type (N, Parameter_Type);
      return N;
   end Make_Parameter_Specification;

   -------------------------------
   -- Make_Predefined_Function  --
   -------------------------------
   function Make_Predefined_Function
    (Predefined_Function : Node_Kind;
     Is_With_Clause : boolean := false)
      return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Predefined_Function);
      Set_LNT_Function (N, New_Node (Predefined_Function));
      Set_Is_With_Clause (N, Is_With_Clause);
      return N;
   end Make_Predefined_Function;

   -------------------------
   -- Make_Module_Pragma  --
   -------------------------
   function Make_Module_Pragma
    (P_Type : Node_Kind;
     N_Constant : Integer)
      return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Module_Pragma);
      Set_Module_Pragma_Type (N, New_Node (P_Type));
      Set_Number_Constant (N, Make_Nat (N_Constant));
      return N;
   end Make_Module_Pragma;

   --------------
   -- Make_Nat --
   --------------
   function Make_Nat (Value : Integer) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Base_Type);
      Set_Type_Name (N, New_Node (K_Nat));
      Set_Image (N, Remove_Prefix_From_Name (
           " ", Get_String_Name (Integer'Image (Value))));
      return N;
   end Make_Nat;

   ------------------------------
   -- Make_Function_Definition --
   ------------------------------
   function Make_Function_Definition
    (Defining_Identifier : Node_Id;
     Function_Parameters : List_Id := No_List;
     Function_Return_Type : Node_Id := No_Node;
     Function_Exceptions : List_Id := No_List;
     Function_Pragma : List_Id := No_List;
     Statements : List_Id)
     return Node_Id
   is
      pragma Assert ((Defining_Identifier /= No_Node) and then
                    (Statements /= No_List));
      N : Node_Id;
   begin
      N := New_Node (K_Function_Definition);
      Set_Identifier (N, Defining_Identifier);
      Set_Function_Parameters (N, Function_Parameters);
      Set_Function_Return_Type (N, Function_Return_Type);
      Set_Function_Exceptions (N, Function_Exceptions);
      Set_Function_Pragma (N, Function_Pragma);
      Set_Statements (N, Statements);
      return N;
   end Make_Function_Definition;

   ---------------------------
   -- Make_Actual_Parameter --
   ---------------------------
   function Make_Actual_Parameter
     (Expression : Node_Id;
      Is_Out : boolean := false;
      Is_InOut : boolean := false)
      return Node_Id
   is
      pragma Assert (Expression /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Actual_Parameter);
      Set_Expression (N, Expression);
      Set_Is_Out (N, Is_Out);
      Set_Is_InOut (N, Is_InOut);
      return N;
   end Make_Actual_Parameter;

   ------------------------------
   -- Make_Null_Statement --
   ------------------------------
   function Make_Null_Statement return Node_Id is
   begin
      return New_Node (K_Null_Statement);
   end Make_Null_Statement;

   ---------------------------
   -- Make_Return_Statement --
   ---------------------------
   function Make_Return_Statement
    (Expression : Node_Id := No_Node;
     Is_Function : boolean := false) return Node_Id
   is
      pragma Assert ((Expression /= No_Node) or else
                    (Is_Function /= false));
      N : Node_Id;
   begin
      N := New_Node (K_Return_Statement);
      Set_Expression (N, Expression);
      Set_Is_Function (N, Is_Function);
      return N;
   end Make_Return_Statement;

   -------------------------------
   -- Make_Assignment_Statement --
   -------------------------------
   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id)
     return Node_Id
   is
      pragma Assert ((Variable_Identifier /= No_Node) and then
                    (Expression /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Assignment_Statement);
      Set_Identifier (N, Variable_Identifier);
      Set_Expression (N, Expression);
      return N;
   end Make_Assignment_Statement;

   ---------------------------------------------
   -- Make_Array_Element_Assignment_Statement --
   ---------------------------------------------
   function Make_Array_Element_Assignment_Statement
     (Defining_Identifier : Node_Id;
      Expression_Index    : Node_Id;
      Expression          : Node_Id)
     return Node_Id
   is
      pragma Assert ((Defining_Identifier /= No_Node) and then
                    (Expression_Index /= No_Node) and then
                    (Expression /= No_Node)
      );
      N : Node_Id;
   begin
      N := New_Node (K_Array_Element_Assignment_Statement);
      Set_Identifier (N, Defining_Identifier);
      Set_Expression_Index (N, Expression_Index);
      Set_Expression (N, Expression);
      return N;
   end Make_Array_Element_Assignment_Statement;

   -------------------------
   -- Make_Case_Statement --
   -------------------------
   function Make_Case_Statement
     (Expression                  : Node_Id;
      Variable_Declarations : List_Id := No_List;
      Case_Statement_Alternatives : List_Id)
     return Node_Id
   is
      pragma Assert ((Case_Statement_Alternatives /= No_List)
        and then (Expression /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Case_Statement);
      Set_Expression (N, Expression);
      Set_Variable_Declarations (N, Variable_Declarations);
      Set_Case_Statement_Alternatives (N, Case_Statement_Alternatives);
      return N;
   end Make_Case_Statement;

   -------------------------------------
   -- Make_Case_Statement_Alternative --
   -------------------------------------
   function Make_Case_Statement_Alternative
     (Pattern_List : List_Id := No_List;
      Statements          : List_Id)
     return Node_Id
   is
      pragma Assert (Statements /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Case_Statement_Alternative);
      Set_Pattern_List (N, Pattern_List);
      Set_Statements (N, Statements);
      return N;
   end Make_Case_Statement_Alternative;

   -----------------------
   -- Make_If_Statement --
   -----------------------
   function Make_If_Statement
     (Condition        : Node_Id;
      Then_Statements  : List_Id;
      Elsif_Statements : List_Id := No_List;
      Else_Statements  : List_Id := No_List)
     return Node_Id
   is
      pragma Assert ((Then_Statements /= No_List)
        and then (Condition /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_If_Statement);
      Set_Condition (N, Condition);
      Set_Then_Statements (N, Then_Statements);
      Set_Elsif_Statements (N, Elsif_Statements);
      Set_Else_Statements (N, Else_Statements);
      return N;
   end Make_If_Statement;

   --------------------------
   -- Make_Elsif_Statement --
   --------------------------
   function Make_Elsif_Statement
     (Condition       : Node_Id;
      Then_Statements : List_Id)
     return Node_Id
   is
      pragma Assert ((Then_Statements /= No_List)
        and then (Condition /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Elsif_Statement);
      Set_Condition (N, Condition);
      Set_Then_Statements (N, Then_Statements);
      return N;
   end Make_Elsif_Statement;

   -------------------------
   -- Make_While_Statement --
   -------------------------
   function Make_While_Statement
     (Expression : Node_Id;
      Statements : List_Id)
     return Node_Id
   is
      pragma Assert ((Statements /= No_List)
        and then (Expression /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_While_Statement);
      Set_Expression (N, Expression);
      Set_Statements (N, Statements);
      return N;
   end Make_While_Statement;

   -------------------------
   -- Make_Loop_Statement --
   -------------------------
   function Make_Loop_Statement
     (Statements : List_Id)
     return Node_Id
   is
      pragma Assert (Statements /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Loop_Statement);
      Set_Statements (N, Statements);
      return N;
   end Make_Loop_Statement;

   --------------------------
   -- Make_Var_Declaration --
   --------------------------
   function Make_Var_Declaration
     (Defining_Identifier : Node_Id;
      Var_Type : Node_Id)
    return Node_Id is
      pragma Assert ((Var_Type /= No_Node) and then
                    (Defining_Identifier /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Var_Declaration);
      Set_Identifier (N, Defining_Identifier);
      Set_Var_Type (N, Var_Type);
      return N;
   end Make_Var_Declaration;

   ------------------------
   -- Make_Var_Statement --
   ------------------------
   function Make_Var_Statement
      (Variable_Declarations : List_Id;
       Statements : List_Id)
     return Node_Id
   is
      pragma Assert ((Statements /= No_List)
        and then (Variable_Declarations /= No_List));
      N : Node_Id;
   begin
      N := New_Node (K_Var_Statement);
      Set_Variable_Declarations (N, Variable_Declarations);
      Set_Statements (N, Statements);
      return N;
   end Make_Var_Statement;

   ----------------------
   -- Make_Expressions --
   ----------------------
   function Make_Expressions (Expression_List : List_Id)
     return Node_Id
   is
      pragma Assert (Expression_List /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Expressions);
      Set_The_List (N, Expression_List);
      return N;
   end Make_Expressions;

   -----------------------------------
   -- Make_Parenthesized_Expression --
   -----------------------------------
   function Make_Parenthesized_Expression (Expression : Node_Id)
     return Node_Id
   is
      pragma Assert (Expression /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Parenthesized_Expression);
      Set_Variable (N, Expression);
      return N;
   end Make_Parenthesized_Expression;

   -----------------------------------
   -- Make_Function_Call_Expression --
   -----------------------------------
   function Make_Function_Call_Expression
     (Defining_Identifier : Node_Id;
      Parameters : List_Id := No_List)
     return Node_Id
   is
      pragma Assert (Defining_Identifier /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Function_Call_Expression);
      Set_Identifier (N, Defining_Identifier);
      Set_Parameters (N, Parameters);
      return N;
   end Make_Function_Call_Expression;

   -----------------------------------
   -- Make_Function_Call_Expression --
   -----------------------------------
   function Make_Infix_Function_Call_Expression
     (Operator : Node_Id;
      Left_Part : Node_Id := No_Node;
      Right_Part : Node_Id)
     return Node_Id
   is
      pragma Assert ((Operator /= No_Node) and then
                    (Right_Part /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Infix_Function_Call_Expression);
      Set_Operator (N, Operator);
      Set_Left_Part (N, Left_Part);
      Set_Right_Part (N, Right_Part);
      return N;
   end Make_Infix_Function_Call_Expression;

   -------------------------------------
   -- Make_Field_Selection_Expression --
   -------------------------------------
   function Make_Field_Selection_Expression
    (Expression : Node_Id;
     Field : Node_Id)
     return Node_Id
   is
      pragma Assert ((Expression /= No_Node) and then
                    (Field /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Field_Selection_Expression);
      Set_Expression (N, Expression);
      Set_Field (N, Field);
      return N;
   end Make_Field_Selection_Expression;

   ----------------------------------
   -- Make_Field_Update_Expression --
   ----------------------------------
   function Make_Field_Update_Expression
     (Expression : Node_Id;
      Field_Association : List_Id)
     return Node_Id
   is
      pragma Assert ((Expression /= No_Node) and then
                     (Field_Association /= No_List));
      N : Node_Id;
   begin
      N := New_Node (K_Field_Update_Expression);
      Set_Expression (N, Expression);
      Set_Field_Association (N, Field_Association);
      return N;
   end Make_Field_Update_Expression;

   ------------------------------
   -- Make_Element_Association --
   ------------------------------
   function Make_Element_Association
    (Index : Node_Id;
     Expression : Node_Id)
     return Node_Id
   is
      pragma Assert ((Expression /= No_Node) and then
                     (Index /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Element_Association);
      Set_Expression (N, Expression);
      Set_Index (N, Index);
      return N;
   end Make_Element_Association;

   --------------------------------------
   -- Make_Array_Elt_Access_Expression --
   --------------------------------------
   function Make_Array_Elt_Access_Expression
     (Index : Node_Id;
      Expression : Node_Id)
     return Node_Id
   is
      pragma Assert ((Expression /= No_Node) and then
                     (Index /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Array_Elt_Access_Expression);
      Set_Expression (N, Expression);
      Set_Index (N, Index);
      return N;
   end Make_Array_Elt_Access_Expression;

   ----------------------
   -- Make_Patterns --
   ----------------------
   function Make_Patterns (Pattern_List : List_Id)
     return Node_Id
   is
      pragma Assert (Pattern_List /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Patterns);
      Set_The_List (N, Pattern_List);
      return N;
   end Make_Patterns;

   -----------------------------------
   -- Make_Parenthesized_Pattern --
   -----------------------------------
   function Make_Parenthesized_Pattern (Pattern : Node_Id)
     return Node_Id
   is
      pragma Assert (Pattern /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Parenthesized_Pattern);
      Set_Variable (N, Pattern);
      return N;
   end Make_Parenthesized_Pattern;

   ------------------------------
   -- Make_Constructed_Pattern --
   ------------------------------
   function Make_Constructed_Pattern
     (Defining_Identifier : Node_Id;
      Parameters : List_Id := No_List)
     return Node_Id
   is
      pragma Assert (Defining_Identifier /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Constructed_Pattern);
      Set_Identifier (N, Defining_Identifier);
      Set_Parameters (N, Parameters);
      return N;
   end Make_Constructed_Pattern;

   ------------------
   -- Make_Pattern --
   ------------------
   function Make_Pattern
    (Sub_Pattern : Node_Id := No_Node;
     Pattern_Type : Node_Id := No_Node;
     Is_Any : boolean := false;
     Is_Of : boolean := false)
     return Node_Id
   is
      pragma Assert ((Sub_Pattern /= No_Node) or else
                    (Pattern_Type /= No_Node));
      N : Node_Id;
   begin
      N := New_Node (K_Pattern);
      Set_Sub_Pattern (N, Sub_Pattern);
      Set_Pattern_Type (N, Pattern_Type);
      Set_Is_Any (N, Is_Any);
      Set_Is_Of (N, Is_Of);
      return N;
   end Make_Pattern;

   ------------------
   -- Make_Pattern --
   ------------------
   function Make_Channel
     (Defining_Identifier : Node_Id;
      Gate_Profiles : List_Id)
     return Node_Id is
      pragma Assert ((Defining_Identifier /= No_Node) and then
                    (Gate_Profiles /= No_List));
      N : Node_Id;
   begin
      N := New_Node (K_Channel);
      Set_Identifier (N, Defining_Identifier);
      Set_Gate_Profiles (N, Gate_Profiles);
      return N;
   end Make_Channel;

   -----------------------
   -- Make_Gate_Profile --
   -----------------------
   function Make_Gate_Profile
     (Gate_Types : List_Id)
     return Node_Id
   is
      pragma Assert (Gate_Types /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Gate_Profile);
      Set_Gate_Types (N, Gate_Types);
      return N;
   end Make_Gate_Profile;

   ------------------------------
   -- Make_Process_Definition --
   ------------------------------
   function Make_Process_Definition
    (Corresponding_Component : Node_Id := No_Node;
     Defining_Identifier : Node_Id;
     Process_Parameters : List_Id := No_List;
     Process_Gate_Declarations : List_Id := No_List;
     Process_Exceptions : List_Id := No_List;
     Process_Pragma : List_Id := No_List;
     Statements : List_Id)
     return Node_Id
   is
      pragma Assert ((Defining_Identifier /= No_Node) and then
                    (Statements /= No_List));
      N : Node_Id;
   begin
      N := New_Node (K_Process_Definition);
      Set_Corresponding_Component (N, Corresponding_Component);
      Set_Identifier (N, Defining_Identifier);
      Set_Process_Parameters (N, Process_Parameters);
      Set_Process_Gate_Declarations (N, Process_Gate_Declarations);
      Set_Process_Exceptions (N, Process_Exceptions);
      Set_Process_Pragma (N, Process_Pragma);
      Set_Statements (N, Statements);
      return N;
   end Make_Process_Definition;

   ------------------
   -- Make_Pattern --
   ------------------
   function Make_Gate_Declaration
    (Channel_Name : Node_Id := No_Node;
     Gate : Node_Id;
     Is_Any : boolean := false)
     return Node_Id
   is
      pragma Assert ((Channel_Name /= No_Node) or else
                    (Is_Any /= false));
      N : Node_Id;
   begin
      N := New_Node (K_Gate_Declaration);
      Set_Channel_Name (N, Channel_Name);
      Set_Gate (N, Gate);
      Set_Is_Any (N, Is_Any);
      return N;
   end Make_Gate_Declaration;

   ------------------------------
   -- Make_Stop_Statement --
   ------------------------------
   function Make_Stop_Statement return Node_Id is
   begin
      return New_Node (K_Stop_Statement);
   end Make_Stop_Statement;

   ------------------------------------------
   -- Make_Process_Instantiation_Statement --
   ------------------------------------------
   function Make_Process_Instantiation_Statement
     (Defining_Identifier : Node_Id;
      Actual_Gates : List_Id := No_List;
      Actual_Parameters : List_Id := No_List;
      Is_Not_Periodic : Boolean := false)
     return Node_Id
   is
      pragma Assert (Defining_Identifier /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Process_Instantiation_Statement);
      Set_Identifier (N, Defining_Identifier);
      Set_Actual_Gates (N, Actual_Gates);
      Set_Actual_Parameters (N, Actual_Parameters);
      Set_Is_Not_Periodic (N, Is_Not_Periodic);
      return N;
   end Make_Process_Instantiation_Statement;

   ----------------------------------
   -- Make_Communication_Statement --
   ----------------------------------
   function Make_Communication_Statement
     (Defining_Identifier : Node_Id;
      Offers : List_Id := No_List;
      Has_Where : boolean := false;
      Expression : Node_Id := No_Node)
     return Node_Id
   is
      pragma Assert ((Defining_Identifier /= No_Node) and then
                    ((Expression /= No_Node) or else
                    (Has_Where = false)));
      N : Node_Id;
   begin
      N := New_Node (K_Communication_Statement);
      Set_Identifier (N, Defining_Identifier);
      Set_Offers (N, Offers);
      Set_Has_Where (N, Has_Where);
      Set_Expression (N, Expression);
      return N;
   end Make_Communication_Statement;

   --------------------------
   -- Make_Offer_Statement --
   --------------------------
   function Make_Offer_Statement
    (Expression : Node_Id := No_Node;
     Pattern : Node_Id;
     Is_Input : boolean := false)
     return Node_Id
   is
      pragma Assert (Pattern /= No_Node);
      N : Node_Id;
   begin
      N := New_Node (K_Offer_Statement);
      Set_Expression (N, Expression);
      Set_Is_Input (N, Is_Input);
      Set_Pattern (N, Pattern);
      return N;
   end Make_Offer_Statement;

   ---------------------------
   -- Make_Select_Statement --
   ---------------------------
   function Make_Select_Statement
     (Select_Statement_Alternatives : List_Id)
     return Node_Id
   is
      pragma Assert (Select_Statement_Alternatives /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Select_Statement);
      Set_Select_Statement_Alternatives (N, Select_Statement_Alternatives);
      return N;
   end Make_Select_Statement;

   ---------------------------------------
   -- Make_Select_Statement_Alternative --
   ---------------------------------------
   function Make_Select_Statement_Alternative
      (Statements          : List_Id)
      return Node_Id
   is
      pragma Assert (Statements /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Select_Statement_Alternative);
      Set_Statements (N, Statements);
      return N;
   end Make_Select_Statement_Alternative;

   -------------------------
   -- Make_Parallel_Composition_Statement --
   -------------------------
   function Make_Parallel_Composition_Statement
     (Global_Synchronisation_Gates : List_Id := No_List;
      Interface_Synchronisations : List_Id)
     return Node_Id
   is
      pragma Assert (Interface_Synchronisations /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Parallel_Composition_Statement);
      Set_Global_Synchronisation_Gates (N, Global_Synchronisation_Gates);
      Set_Interface_Synchronisations (N, Interface_Synchronisations);
      return N;
   end Make_Parallel_Composition_Statement;

   ------------------------------------
   -- Make_Interface_Synchronisation --
   ------------------------------------
   function Make_Interface_Synchronisation
      (Interface_Synchronisation_Gates : List_Id := No_List;
       Statements : List_Id)
     return Node_Id
   is
      pragma Assert (Statements /= No_List);
      N : Node_Id;
   begin
      N := New_Node (K_Interface_Synchronisation);
      Set_Interface_Synchronisation_Gates (N, Interface_Synchronisation_Gates);
      Set_Statements (N, Statements);
      return N;
   end Make_Interface_Synchronisation;

   -------------------------
   -- Make_Hide_Statement --
   -------------------------
   function Make_Hide_Statement
      (Hide_Gate_Declarations : List_Id;
       Statements : List_Id)
     return Node_Id
   is
      pragma Assert ((Statements /= No_List) and then
                     (Hide_Gate_Declarations /= No_List));
      N : Node_Id;
   begin
      N := New_Node (K_Hide_Statement);
      Set_Hide_Gate_Declarations (N, Hide_Gate_Declarations);
      Set_Statements (N, Statements);
      return N;
   end Make_Hide_Statement;

   ---------------------------
   --  Make_Var_Loop_Select --
   ---------------------------
   function Make_Var_Loop_Select (Var_Dec : List_Id;
                                 Out_Loop : List_Id;
                                 In_Select : List_Id;
                                 With_Select : Boolean := true)
     return Node_Id is
      N : Node_Id;
      N_Loop : Node_Id;
      L_Var : List_Id;
   begin
      L_Var := Out_Loop;
      if With_Select then
         N_Loop := Make_Loop_Statement
           (New_List (Make_Select_Statement (In_Select)));
      else
         N_Loop := Make_Loop_Statement (In_Select);
      end if;
      Append_Node_To_List (N_Loop, L_Var);
      N := Make_Var_Statement (Var_Dec, L_Var);
      return N;
   end Make_Var_Loop_Select;

end Ocarina.Backends.Lnt.Components;
