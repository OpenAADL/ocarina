pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.ME_REAL.REAL_Tree.Debug; use Ocarina.ME_REAL.REAL_Tree.Debug;

package body Ocarina.ME_REAL.REAL_Tree.Nodes is

   pragma Warnings (Off);
   use Entries;

   function Kind (N : Node_Id) return Node_Kind is
   begin
      return Table (Types.Node_Id (N)).Kind;
   end Kind;

   procedure Set_Kind (N : Node_Id; V : Node_Kind) is
   begin
      Table (Types.Node_Id (N)).Kind := V;
   end Set_Kind;

   function Loc (N : Node_Id) return Location is
   begin
      return Table (Types.Node_Id (N)).Loc;
   end Loc;

   procedure Set_Loc (N : Node_Id; V : Location) is
   begin
      Table (Types.Node_Id (N)).Loc := V;
   end Set_Loc;

   function Next_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Value_Node
        or else Table (Types.Node_Id (N)).Kind = K_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Theorem_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Return_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Required_Theorem
        or else Table (Types.Node_Id (N)).Kind = K_Theorem
        or else Table (Types.Node_Id (N)).Kind = K_Root_Node
        or else Table (Types.Node_Id (N)).Kind = K_Annotation);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Value_Node
        or else Table (Types.Node_Id (N)).Kind = K_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Theorem_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Return_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Required_Theorem
        or else Table (Types.Node_Id (N)).Kind = K_Theorem
        or else Table (Types.Node_Id (N)).Kind = K_Root_Node
        or else Table (Types.Node_Id (N)).Kind = K_Annotation);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Next_Node;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Name;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Item (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Item;

   procedure Set_Item (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Item;

   function Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Identifier;

   function Element_Type (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition);

      return Value_Id (Table (Types.Node_Id (N)).L (3));
   end Element_Type;

   procedure Set_Element_Type (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Element_Type;

   function Set_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Set_Reference;

   procedure Set_Set_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Set_Reference;

   function Set_Type (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression);

      return Value_Id (Table (Types.Node_Id (N)).L (3));
   end Set_Type;

   procedure Set_Set_Type (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Set_Type;

   function Annotation (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Annotation;

   procedure Set_Annotation (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Annotation;

   function Set_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Set_Expression;

   procedure Set_Set_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Set_Expression;

   function Predefined_Type (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Value_Id (Table (Types.Node_Id (N)).L (6));
   end Predefined_Type;

   procedure Set_Predefined_Type (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Predefined_Type;

   function Dependant (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Value_Id (Table (Types.Node_Id (N)).L (7));
   end Dependant;

   procedure Set_Dependant (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Dependant;

   function Referenced_Set (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Referenced_Set;

   procedure Set_Referenced_Set (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Referenced_Set;

   function Item_Val (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Node);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Item_Val;

   procedure Set_Item_Val (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Node);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Item_Val;

   function Var_Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      return Value_Id (Table (Types.Node_Id (N)).L (3));
   end Var_Value;

   procedure Set_Var_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Var_Value;

   function Var_Type (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      return Value_Id (Table (Types.Node_Id (N)).L (4));
   end Var_Type;

   procedure Set_Var_Type (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Var_Type;

   function Theorem_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Required_Theorem);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Theorem_Name;

   procedure Set_Theorem_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Required_Theorem);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Theorem_Name;

   function Related_Theorem (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Required_Theorem);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Related_Theorem;

   procedure Set_Related_Theorem (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Required_Theorem);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Related_Theorem;

   function Referenced_Var (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Referenced_Var;

   procedure Set_Referenced_Var (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Referenced_Var;

   function Returned_Type (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      return Value_Id (Table (Types.Node_Id (N)).L (4));
   end Returned_Type;

   procedure Set_Returned_Type (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Returned_Type;

   function Var_Ref (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Var_Ref;

   procedure Set_Var_Ref (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Var_Ref;

   function Is_Global (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression);

      return Value_Id (Table (Types.Node_Id (N)).L (5));
   end Is_Global;

   procedure Set_Is_Global (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Is_Global;

   function True_params (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end True_params;

   procedure Set_True_params (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_True_params;

   function Domain (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Domain;

   procedure Set_Domain (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Domain;

   function Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      return List_Id (Table (Types.Node_Id (N)).L (8));
   end Parameters;

   procedure Set_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Compute
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Parameters;

   function Return_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Return_Expr;

   procedure Set_Return_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Decl_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Return_Expr;

   function Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Value;

   procedure Set_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value;

   function Code (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      return Value_Id (Table (Types.Node_Id (N)).L (3));
   end Code;

   procedure Set_Code (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parametrized_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Code;

   function Right_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Right_Expr;

   procedure Set_Right_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Right_Expr;

   function Left_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Left_Expr;

   procedure Set_Left_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Left_Expr;

   function Operator (N : Node_Id) return Operator_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      return Operator_Id (Table (Types.Node_Id (N)).O (1));
   end Operator;

   procedure Set_Operator (N : Node_Id; V : Operator_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Set_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Check_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Operator;

   function Referenced_Sets (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Referenced_Sets;

   procedure Set_Referenced_Sets (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Referenced_Sets;

   function True_Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end True_Parameters;

   procedure Set_True_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_True_Parameters;

   function Variable_Position (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      return Value_Id (Table (Types.Node_Id (N)).L (7));
   end Variable_Position;

   procedure Set_Variable_Position (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Check_Subprogram_Call);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Variable_Position;

   function Third_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Third_Expr;

   procedure Set_Third_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ternary_Expression);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Third_Expr;

   function Check_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Return_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Check_Expression;

   procedure Set_Check_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Return_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Check_Expression;

   function Range_Function (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Return_Expression);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Range_Function;

   procedure Set_Range_Function (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Return_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Range_Function;

   function Variable_Ref (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Variable_Ref;

   procedure Set_Variable_Ref (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Variable_Ref;

   function Range_Variable (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Range_Variable;

   procedure Set_Range_Variable (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Range_Variable;

   function Range_Set (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Range_Set;

   procedure Set_Range_Set (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Declaration);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Range_Set;

   function In_Set (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end In_Set;

   procedure Set_In_Set (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Local_Variable_Definition);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_In_Set;

   function Parametrized_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Parametrized_Expr;

   procedure Set_Parametrized_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Parametrized_Expr;

   function Corresponding_Element (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Corresponding_Element;

   procedure Set_Corresponding_Element (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Corresponding_Element;

   function Selection_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Selection_Expression;

   procedure Set_Selection_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Selection_Expression;

   function Local_Variable (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Local_Variable;

   procedure Set_Local_Variable (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Local_Variable;

   function Local_Set (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Local_Set;

   procedure Set_Local_Set (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Local_Set;

   function Local_Set_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Local_Set_Expression;

   procedure Set_Local_Set_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Set_Declaration);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Local_Set_Expression;

   function Range_Declaration (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Range_Declaration;

   procedure Set_Range_Declaration (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Range_Declaration;

   function Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Declarations;

   procedure Set_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Declarations;

   function Required_Theorems (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Required_Theorems;

   procedure Set_Required_Theorems (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Required_Theorems;

   function Return_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Return_Expression;

   procedure Set_Return_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Return_Expression;

   function Used_Set (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return List_Id (Table (Types.Node_Id (N)).L (8));
   end Used_Set;

   procedure Set_Used_Set (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Used_Set;

   function Used_Var (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return List_Id (Table (Types.Node_Id (N)).L (9));
   end Used_Var;

   procedure Set_Used_Var (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Used_Var;

   function Local_Var (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return List_Id (Table (Types.Node_Id (N)).L (10));
   end Local_Var;

   procedure Set_Local_Var (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Local_Var;

   function Related_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Related_Entity;

   procedure Set_Related_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Theorem);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Related_Entity;

   function Theorems (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Root_Node);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Theorems;

   procedure Set_Theorems (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Root_Node);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Theorems;

   function Index (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annotation);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Index;

   procedure Set_Index (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annotation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Index;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_Node_Container =>
            W_Node_Container
              (Node_Id (N));
         when K_Element =>
            W_Element
              (Node_Id (N));
         when K_Set =>
            W_Set
              (Node_Id (N));
         when K_Set_Reference =>
            W_Set_Reference
              (Node_Id (N));
         when K_Value_Node =>
            W_Value_Node
              (Node_Id (N));
         when K_Variable =>
            W_Variable
              (Node_Id (N));
         when K_Theorem_Reference =>
            W_Theorem_Reference
              (Node_Id (N));
         when K_Var_Reference =>
            W_Var_Reference
              (Node_Id (N));
         when K_Variable_Declaration =>
            W_Variable_Declaration
              (Node_Id (N));
         when K_Variable_Decl_Compute =>
            W_Variable_Decl_Compute
              (Node_Id (N));
         when K_Variable_Decl_Expression =>
            W_Variable_Decl_Expression
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_Parametrized_Identifier =>
            W_Parametrized_Identifier
              (Node_Id (N));
         when K_Expression =>
            W_Expression
              (Node_Id (N));
         when K_Set_Expression =>
            W_Set_Expression
              (Node_Id (N));
         when K_Check_Subprogram_Call =>
            W_Check_Subprogram_Call
              (Node_Id (N));
         when K_Check_Expression =>
            W_Check_Expression
              (Node_Id (N));
         when K_Ternary_Expression =>
            W_Ternary_Expression
              (Node_Id (N));
         when K_Return_Expression =>
            W_Return_Expression
              (Node_Id (N));
         when K_Range_Declaration =>
            W_Range_Declaration
              (Node_Id (N));
         when K_Local_Variable_Definition =>
            W_Local_Variable_Definition
              (Node_Id (N));
         when K_Set_Declaration =>
            W_Set_Declaration
              (Node_Id (N));
         when K_Required_Theorem =>
            W_Required_Theorem
              (Node_Id (N));
         when K_Theorem =>
            W_Theorem
              (Node_Id (N));
         when K_Root_Node =>
            W_Root_Node
              (Node_Id (N));
         when K_Annotation =>
            W_Annotation
              (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

   procedure W_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
   end W_Identifier;

   procedure W_Node_Container (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Item",
         "Node_Id",
         Image (Item (N)),
         Int (Item (N)));
   end W_Node_Container;

   procedure W_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Element_Type",
         "Value_Id",
         Image (Element_Type (N)));
      W_Node_Attribute
        ("Set_Reference",
         "Node_Id",
         Image (Set_Reference (N)),
         Int (Set_Reference (N)));
   end W_Element;

   procedure W_Set (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Set_Type",
         "Value_Id",
         Image (Set_Type (N)));
      W_Node_Attribute
        ("Annotation",
         "Node_Id",
         Image (Annotation (N)),
         Int (Annotation (N)));
      W_Node_Attribute
        ("Set_Expression",
         "Node_Id",
         Image (Set_Expression (N)),
         Int (Set_Expression (N)));
      W_Node_Attribute
        ("Predefined_Type",
         "Value_Id",
         Image (Predefined_Type (N)));
      W_Node_Attribute
        ("Dependant",
         "Value_Id",
         Image (Dependant (N)));
   end W_Set;

   procedure W_Set_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Predefined_Type",
         "Value_Id",
         Image (Predefined_Type (N)));
      W_Node_Attribute
        ("Referenced_Set",
         "Node_Id",
         Image (Referenced_Set (N)),
         Int (Referenced_Set (N)));
   end W_Set_Reference;

   procedure W_Value_Node (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Item_Val",
         "Value_Id",
         Image (Item_Val (N)));
   end W_Value_Node;

   procedure W_Variable (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Var_Value",
         "Value_Id",
         Image (Var_Value (N)));
      W_Node_Attribute
        ("Var_Type",
         "Value_Id",
         Image (Var_Type (N)));
   end W_Variable;

   procedure W_Theorem_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Theorem_Name",
         "Name_Id",
         Image (Theorem_Name (N)));
      W_Node_Attribute
        ("Related_Theorem",
         "Node_Id",
         Image (Related_Theorem (N)),
         Int (Related_Theorem (N)));
   end W_Theorem_Reference;

   procedure W_Var_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Referenced_Var",
         "Node_Id",
         Image (Referenced_Var (N)),
         Int (Referenced_Var (N)));
      W_Node_Attribute
        ("Returned_Type",
         "Value_Id",
         Image (Returned_Type (N)));
   end W_Var_Reference;

   procedure W_Variable_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Var_Ref",
         "Node_Id",
         Image (Var_Ref (N)),
         Int (Var_Ref (N)));
      W_Node_Attribute
        ("Is_Global",
         "Value_Id",
         Image (Is_Global (N)));
   end W_Variable_Declaration;

   procedure W_Variable_Decl_Compute (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Theorem_Name",
         "Name_Id",
         Image (Theorem_Name (N)));
      W_Node_Attribute
        ("Related_Theorem",
         "Node_Id",
         Image (Related_Theorem (N)),
         Int (Related_Theorem (N)));
      W_Node_Attribute
        ("Var_Ref",
         "Node_Id",
         Image (Var_Ref (N)),
         Int (Var_Ref (N)));
      W_Node_Attribute
        ("Is_Global",
         "Value_Id",
         Image (Is_Global (N)));
      W_Node_Attribute
        ("True_params",
         "List_Id",
         Image (True_params (N)),
         Int (True_params (N)));
      W_Node_Attribute
        ("Domain",
         "Node_Id",
         Image (Domain (N)),
         Int (Domain (N)));
      W_Node_Attribute
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
   end W_Variable_Decl_Compute;

   procedure W_Variable_Decl_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Var_Ref",
         "Node_Id",
         Image (Var_Ref (N)),
         Int (Var_Ref (N)));
      W_Node_Attribute
        ("Is_Global",
         "Value_Id",
         Image (Is_Global (N)));
      W_Node_Attribute
        ("Return_Expr",
         "Node_Id",
         Image (Return_Expr (N)),
         Int (Return_Expr (N)));
   end W_Variable_Decl_Expression;

   procedure W_Literal (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Returned_Type",
         "Value_Id",
         Image (Returned_Type (N)));
      W_Node_Attribute
        ("Value",
         "Value_Id",
         Image (Value (N)));
   end W_Literal;

   procedure W_Parametrized_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
      W_Node_Attribute
        ("Code",
         "Value_Id",
         Image (Code (N)));
   end W_Parametrized_Identifier;

   procedure W_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Right_Expr",
         "Node_Id",
         Image (Right_Expr (N)),
         Int (Right_Expr (N)));
      W_Node_Attribute
        ("Left_Expr",
         "Node_Id",
         Image (Left_Expr (N)),
         Int (Left_Expr (N)));
      W_Node_Attribute
        ("Operator",
         "Operator_Id",
         Image (Operator (N)));
   end W_Expression;

   procedure W_Set_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Set_Type",
         "Value_Id",
         Image (Set_Type (N)));
      W_Node_Attribute
        ("Right_Expr",
         "Node_Id",
         Image (Right_Expr (N)),
         Int (Right_Expr (N)));
      W_Node_Attribute
        ("Left_Expr",
         "Node_Id",
         Image (Left_Expr (N)),
         Int (Left_Expr (N)));
      W_Node_Attribute
        ("Operator",
         "Operator_Id",
         Image (Operator (N)));
   end W_Set_Expression;

   procedure W_Check_Subprogram_Call (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Returned_Type",
         "Value_Id",
         Image (Returned_Type (N)));
      W_Node_Attribute
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
      W_Node_Attribute
        ("Code",
         "Value_Id",
         Image (Code (N)));
      W_Node_Attribute
        ("Referenced_Sets",
         "List_Id",
         Image (Referenced_Sets (N)),
         Int (Referenced_Sets (N)));
      W_Node_Attribute
        ("True_Parameters",
         "List_Id",
         Image (True_Parameters (N)),
         Int (True_Parameters (N)));
      W_Node_Attribute
        ("Variable_Position",
         "Value_Id",
         Image (Variable_Position (N)));
   end W_Check_Subprogram_Call;

   procedure W_Check_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Returned_Type",
         "Value_Id",
         Image (Returned_Type (N)));
      W_Node_Attribute
        ("Right_Expr",
         "Node_Id",
         Image (Right_Expr (N)),
         Int (Right_Expr (N)));
      W_Node_Attribute
        ("Left_Expr",
         "Node_Id",
         Image (Left_Expr (N)),
         Int (Left_Expr (N)));
      W_Node_Attribute
        ("Operator",
         "Operator_Id",
         Image (Operator (N)));
   end W_Check_Expression;

   procedure W_Ternary_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Returned_Type",
         "Value_Id",
         Image (Returned_Type (N)));
      W_Node_Attribute
        ("Right_Expr",
         "Node_Id",
         Image (Right_Expr (N)),
         Int (Right_Expr (N)));
      W_Node_Attribute
        ("Left_Expr",
         "Node_Id",
         Image (Left_Expr (N)),
         Int (Left_Expr (N)));
      W_Node_Attribute
        ("Operator",
         "Operator_Id",
         Image (Operator (N)));
      W_Node_Attribute
        ("Third_Expr",
         "Node_Id",
         Image (Third_Expr (N)),
         Int (Third_Expr (N)));
   end W_Ternary_Expression;

   procedure W_Return_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Check_Expression",
         "Node_Id",
         Image (Check_Expression (N)),
         Int (Check_Expression (N)));
      W_Node_Attribute
        ("Range_Function",
         "Value_Id",
         Image (Range_Function (N)));
   end W_Return_Expression;

   procedure W_Range_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Variable_Ref",
         "Node_Id",
         Image (Variable_Ref (N)),
         Int (Variable_Ref (N)));
      W_Node_Attribute
        ("Range_Variable",
         "Node_Id",
         Image (Range_Variable (N)),
         Int (Range_Variable (N)));
      W_Node_Attribute
        ("Range_Set",
         "Node_Id",
         Image (Range_Set (N)),
         Int (Range_Set (N)));
   end W_Range_Declaration;

   procedure W_Local_Variable_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Element_Type",
         "Value_Id",
         Image (Element_Type (N)));
      W_Node_Attribute
        ("Set_Reference",
         "Node_Id",
         Image (Set_Reference (N)),
         Int (Set_Reference (N)));
      W_Node_Attribute
        ("In_Set",
         "Node_Id",
         Image (In_Set (N)),
         Int (In_Set (N)));
   end W_Local_Variable_Definition;

   procedure W_Set_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Predefined_Type",
         "Value_Id",
         Image (Predefined_Type (N)));
      W_Node_Attribute
        ("Dependant",
         "Value_Id",
         Image (Dependant (N)));
      W_Node_Attribute
        ("Referenced_Set",
         "Node_Id",
         Image (Referenced_Set (N)),
         Int (Referenced_Set (N)));
      W_Node_Attribute
        ("Parametrized_Expr",
         "Node_Id",
         Image (Parametrized_Expr (N)),
         Int (Parametrized_Expr (N)));
      W_Node_Attribute
        ("Corresponding_Element",
         "Node_Id",
         Image (Corresponding_Element (N)),
         Int (Corresponding_Element (N)));
      W_Node_Attribute
        ("Selection_Expression",
         "Node_Id",
         Image (Selection_Expression (N)),
         Int (Selection_Expression (N)));
      W_Node_Attribute
        ("Local_Variable",
         "Node_Id",
         Image (Local_Variable (N)),
         Int (Local_Variable (N)));
      W_Node_Attribute
        ("Local_Set",
         "Node_Id",
         Image (Local_Set (N)),
         Int (Local_Set (N)));
      W_Node_Attribute
        ("Local_Set_Expression",
         "Node_Id",
         Image (Local_Set_Expression (N)),
         Int (Local_Set_Expression (N)));
   end W_Set_Declaration;

   procedure W_Required_Theorem (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Theorem_Name",
         "Name_Id",
         Image (Theorem_Name (N)));
      W_Node_Attribute
        ("Related_Theorem",
         "Node_Id",
         Image (Related_Theorem (N)),
         Int (Related_Theorem (N)));
   end W_Required_Theorem;

   procedure W_Theorem (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Check_Expression",
         "Node_Id",
         Image (Check_Expression (N)),
         Int (Check_Expression (N)));
      W_Node_Attribute
        ("Range_Declaration",
         "Node_Id",
         Image (Range_Declaration (N)),
         Int (Range_Declaration (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
      W_Node_Attribute
        ("Required_Theorems",
         "List_Id",
         Image (Required_Theorems (N)),
         Int (Required_Theorems (N)));
      W_Node_Attribute
        ("Return_Expression",
         "Node_Id",
         Image (Return_Expression (N)),
         Int (Return_Expression (N)));
      W_Node_Attribute
        ("Used_Set",
         "List_Id",
         Image (Used_Set (N)),
         Int (Used_Set (N)));
      W_Node_Attribute
        ("Used_Var",
         "List_Id",
         Image (Used_Var (N)),
         Int (Used_Var (N)));
      W_Node_Attribute
        ("Local_Var",
         "List_Id",
         Image (Local_Var (N)),
         Int (Local_Var (N)));
      W_Node_Attribute
        ("Related_Entity",
         "Node_Id",
         Image (Related_Entity (N)),
         Int (Related_Entity (N)));
   end W_Theorem;

   procedure W_Root_Node (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Theorems",
         "List_Id",
         Image (Theorems (N)),
         Int (Theorems (N)));
   end W_Root_Node;

   procedure W_Annotation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Index",
         "Value_Id",
         Image (Index (N)));
   end W_Annotation;

end Ocarina.ME_REAL.REAL_Tree.Nodes;
