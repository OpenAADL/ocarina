pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.ME_AO4AADL.AO4AADL_Tree.Debug; use Ocarina.ME_AO4AADL.AO4AADL_Tree.Debug;

package body Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes is

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
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Wildcard
        or else Table (Types.Node_Id (N)).Kind = K_Star
        or else Table (Types.Node_Id (N)).Kind = K_Interval
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Components_Applied_To
        or else Table (Types.Node_Id (N)).Kind = K_Component
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Precedence
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Caller
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Execution
        or else Table (Types.Node_Id (N)).Kind = K_Callee
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call
        or else Table (Types.Node_Id (N)).Kind = K_Args
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action
        or else Table (Types.Node_Id (N)).Kind = K_Variables_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Initialisation
        or else Table (Types.Node_Id (N)).Kind = K_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Proceed_Action
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Wildcard
        or else Table (Types.Node_Id (N)).Kind = K_Star
        or else Table (Types.Node_Id (N)).Kind = K_Interval
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Components_Applied_To
        or else Table (Types.Node_Id (N)).Kind = K_Component
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Precedence
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Caller
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Execution
        or else Table (Types.Node_Id (N)).Kind = K_Callee
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call
        or else Table (Types.Node_Id (N)).Kind = K_Args
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action
        or else Table (Types.Node_Id (N)).Kind = K_Variables_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Initialisation
        or else Table (Types.Node_Id (N)).Kind = K_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Proceed_Action
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Next_Node;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

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
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Callee
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Component
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Callee
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Identifier;

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

   function Aspect_Expressions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Annex);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Aspect_Expressions;

   procedure Set_Aspect_Expressions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Annex);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Aspect_Expressions;

   function Components_Applied_Tos (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Components_Applied_Tos;

   procedure Set_Components_Applied_Tos (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Components_Applied_Tos;

   function Aspect_Precedence (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Aspect_Precedence;

   procedure Set_Aspect_Precedence (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Aspect_Precedence;

   function Pointcut_Specification (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Pointcut_Specification;

   procedure Set_Pointcut_Specification (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Pointcut_Specification;

   function Advice_Specification (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Advice_Specification;

   procedure Set_Advice_Specification (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Expression);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Advice_Specification;

   function Components (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Components_Applied_To);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Components;

   procedure Set_Components (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Components_Applied_To);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Components;

   function Component_Category (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Component_Category;

   procedure Set_Component_Category (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Component_Category;

   function Identifiers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Precedence
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Identifiers;

   procedure Set_Identifiers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Aspect_Precedence
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Identifiers;

   function Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Callee
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Declaration);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Parameters;

   procedure Set_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Callee
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Declaration);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Parameters;

   function Pointcut_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Pointcut_Expression;

   procedure Set_Pointcut_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Pointcut_Expression;

   function Type_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Type_Identifier;

   procedure Set_Type_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Variable);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Type_Identifier;

   function Caller (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Caller;

   procedure Set_Caller (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Caller;

   function Operator (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Operator;

   procedure Set_Operator (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Expression);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Operator;

   function Caller_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Caller
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Execution);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Caller_Kind;

   procedure Set_Caller_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Caller
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Execution);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Caller_Kind;

   function Callee (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Caller
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Execution);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Callee;

   procedure Set_Callee (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Caller
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Execution);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Callee;

   function Mode (N : Node_Id) return Mode_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call);

      return Mode_Id (Table (Types.Node_Id (N)).O (1));
   end Mode;

   procedure Set_Mode (N : Node_Id; V : Mode_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Call);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Mode;

   function Arguments (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Args);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Arguments;

   procedure Set_Arguments (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Args);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Arguments;

   function Advice_Declaration (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Advice_Declaration;

   procedure Set_Advice_Declaration (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Advice_Declaration;

   function Pointcut_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Pointcut_Reference;

   procedure Set_Pointcut_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Pointcut_Reference;

   function Advice_Action (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Advice_Action;

   procedure Set_Advice_Action (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Specification);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Advice_Action;

   function Advice_Category (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Declaration);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Advice_Category;

   procedure Set_Advice_Category (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Declaration);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Advice_Category;

   function Parameter_Profile (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Proceed_Action);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Parameter_Profile;

   procedure Set_Parameter_Profile (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pointcut_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Proceed_Action);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Parameter_Profile;

   function Variables_Declaration (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Variables_Declaration;

   procedure Set_Variables_Declaration (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Variables_Declaration;

   function Initialisation (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Initialisation;

   procedure Set_Initialisation (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Initialisation;

   function Actions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Actions;

   procedure Set_Actions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Advice_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Actions;

   function Variables (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variables_Declaration);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Variables;

   procedure Set_Variables (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variables_Declaration);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Variables;

   function Assignments (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initialisation);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Assignments;

   procedure Set_Assignments (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initialisation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Assignments;

   function Action_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Action_Node;

   procedure Set_Action_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Action);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Action_Node;

   function Behavior_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Behavior_Expression;

   procedure Set_Behavior_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Behavior_Expression;

   function If_Cond (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end If_Cond;

   procedure Set_If_Cond (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_If_Cond;

   function Elsif_Cond (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Elsif_Cond;

   procedure Set_Elsif_Cond (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Elsif_Cond;

   function Else_Cond (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Else_Cond;

   procedure Set_Else_Cond (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Else_Cond;

   function Conditional_Statement (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Conditional_Statement;

   procedure Set_Conditional_Statement (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Conditional_Statement;

   function Loop_Variable_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Loop_Variable_Identifier;

   procedure Set_Loop_Variable_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Loop_Variable_Identifier;

   function Integer_Range (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Integer_Range;

   procedure Set_Integer_Range (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Integer_Range;

   function Reference_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Reference_Expression;

   procedure Set_Reference_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Reference_Expression;

   function Com_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Com_Kind;

   procedure Set_Com_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Com_Kind;

   function Is_Computation (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Computation;

   procedure Set_Is_Computation (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Computation;

   function Fst_Behavior_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Fst_Behavior_Time;

   procedure Set_Fst_Behavior_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Fst_Behavior_Time;

   function Scd_Behavior_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Scd_Behavior_Time;

   procedure Set_Scd_Behavior_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Scd_Behavior_Time;

   function Distrib_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      return Byte (Table (Types.Node_Id (N)).O (2));
   end Distrib_Kind;

   procedure Set_Distrib_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Action);

      Table (Types.Node_Id (N)).O (2) := Byte (V);
   end Set_Distrib_Kind;

   function Relations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Relations;

   procedure Set_Relations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Relations;

   function String_Literal (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end String_Literal;

   procedure Set_String_Literal (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_String_Literal;

   function Character_Literal (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Character_Literal;

   procedure Set_Character_Literal (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Expression);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Character_Literal;

   function Boolean_Value (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Relation);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Boolean_Value;

   procedure Set_Boolean_Value (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Relation);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Boolean_Value;

   function Simple_Exprs (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Relation);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Simple_Exprs;

   procedure Set_Simple_Exprs (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Relation);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Simple_Exprs;

   function Term_And_Operator (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Term_And_Operator;

   procedure Set_Term_And_Operator (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Term_And_Operator;

   function Factors (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Term);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Factors;

   procedure Set_Factors (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Term);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Factors;

   function Is_Not (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Not;

   procedure Set_Is_Not (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Not;

   function Lower_Primary (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Lower_Primary;

   procedure Set_Lower_Primary (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Lower_Primary;

   function Property_Set (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Set;

   procedure Set_Property_Set (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Set;

   function Operator_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operator);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Operator_Kind;

   procedure Set_Operator_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operator);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Operator_Kind;

   function Is_True (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_True;

   procedure Set_Is_True (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_True;

   function Lower_Int_Val (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Lower_Int_Val;

   procedure Set_Lower_Int_Val (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Lower_Int_Val;

   function Upper_Int_Val (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Upper_Int_Val;

   procedure Set_Upper_Int_Val (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Upper_Int_Val;

   function Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Entity;

   procedure Set_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Entity;

   function Integer_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Integer_Value;

   procedure Set_Integer_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Integer_Value;

   function Unit_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Unit_Identifier;

   procedure Set_Unit_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Unit_Identifier;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_Node_Container =>
            W_Node_Container
              (Node_Id (N));
         when K_Definition =>
            W_Definition
              (Node_Id (N));
         when K_Wildcard =>
            W_Wildcard
              (Node_Id (N));
         when K_Star =>
            W_Star
              (Node_Id (N));
         when K_Interval =>
            W_Interval
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_Aspect_Annex =>
            W_Aspect_Annex
              (Node_Id (N));
         when K_Aspect_Expression =>
            W_Aspect_Expression
              (Node_Id (N));
         when K_Components_Applied_To =>
            W_Components_Applied_To
              (Node_Id (N));
         when K_Component =>
            W_Component
              (Node_Id (N));
         when K_Aspect_Precedence =>
            W_Aspect_Precedence
              (Node_Id (N));
         when K_Pointcut_Specification =>
            W_Pointcut_Specification
              (Node_Id (N));
         when K_Parameter_Specification =>
            W_Parameter_Specification
              (Node_Id (N));
         when K_Pointcut_Expression =>
            W_Pointcut_Expression
              (Node_Id (N));
         when K_Caller =>
            W_Caller
              (Node_Id (N));
         when K_Call =>
            W_Call
              (Node_Id (N));
         when K_Execution =>
            W_Execution
              (Node_Id (N));
         when K_Callee =>
            W_Callee
              (Node_Id (N));
         when K_Subprogram_Call =>
            W_Subprogram_Call
              (Node_Id (N));
         when K_Port_Call =>
            W_Port_Call
              (Node_Id (N));
         when K_Args =>
            W_Args
              (Node_Id (N));
         when K_Advice_Specification =>
            W_Advice_Specification
              (Node_Id (N));
         when K_Advice_Declaration =>
            W_Advice_Declaration
              (Node_Id (N));
         when K_Pointcut_Reference =>
            W_Pointcut_Reference
              (Node_Id (N));
         when K_Advice_Action =>
            W_Advice_Action
              (Node_Id (N));
         when K_Variables_Declaration =>
            W_Variables_Declaration
              (Node_Id (N));
         when K_Variable =>
            W_Variable
              (Node_Id (N));
         when K_Initialisation =>
            W_Initialisation
              (Node_Id (N));
         when K_Action =>
            W_Action
              (Node_Id (N));
         when K_Conditional_Statement =>
            W_Conditional_Statement
              (Node_Id (N));
         when K_If_Statement =>
            W_If_Statement
              (Node_Id (N));
         when K_While_Statement =>
            W_While_Statement
              (Node_Id (N));
         when K_For_Statement =>
            W_For_Statement
              (Node_Id (N));
         when K_Assignment_Action =>
            W_Assignment_Action
              (Node_Id (N));
         when K_Communication_Action =>
            W_Communication_Action
              (Node_Id (N));
         when K_Timed_Action =>
            W_Timed_Action
              (Node_Id (N));
         when K_Reference_Expression =>
            W_Reference_Expression
              (Node_Id (N));
         when K_Behavior_Expression =>
            W_Behavior_Expression
              (Node_Id (N));
         when K_Relation =>
            W_Relation
              (Node_Id (N));
         when K_Simple_Expression =>
            W_Simple_Expression
              (Node_Id (N));
         when K_Term =>
            W_Term
              (Node_Id (N));
         when K_Factor =>
            W_Factor
              (Node_Id (N));
         when K_Property_Constant =>
            W_Property_Constant
              (Node_Id (N));
         when K_Operator =>
            W_Operator
              (Node_Id (N));
         when K_Boolean_Literal =>
            W_Boolean_Literal
              (Node_Id (N));
         when K_Integer_Range =>
            W_Integer_Range
              (Node_Id (N));
         when K_Integer_Value =>
            W_Integer_Value
              (Node_Id (N));
         when K_Behavior_Time =>
            W_Behavior_Time
              (Node_Id (N));
         when K_Proceed_Action =>
            W_Proceed_Action
              (Node_Id (N));
         when K_Parameter_Profile =>
            W_Parameter_Profile
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

   procedure W_Definition (N : Node_Id) is
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
   end W_Definition;

   procedure W_Wildcard (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Wildcard;

   procedure W_Star (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Star;

   procedure W_Interval (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Interval;

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
        ("Value",
         "Value_Id",
         Image (Value (N)));
   end W_Literal;

   procedure W_Aspect_Annex (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Aspect_Expressions",
         "List_Id",
         Image (Aspect_Expressions (N)),
         Int (Aspect_Expressions (N)));
   end W_Aspect_Annex;

   procedure W_Aspect_Expression (N : Node_Id) is
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
        ("Components_Applied_Tos",
         "Node_Id",
         Image (Components_Applied_Tos (N)),
         Int (Components_Applied_Tos (N)));
      W_Node_Attribute
        ("Aspect_Precedence",
         "Node_Id",
         Image (Aspect_Precedence (N)),
         Int (Aspect_Precedence (N)));
      W_Node_Attribute
        ("Pointcut_Specification",
         "List_Id",
         Image (Pointcut_Specification (N)),
         Int (Pointcut_Specification (N)));
      W_Node_Attribute
        ("Advice_Specification",
         "List_Id",
         Image (Advice_Specification (N)),
         Int (Advice_Specification (N)));
   end W_Aspect_Expression;

   procedure W_Components_Applied_To (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Components",
         "List_Id",
         Image (Components (N)),
         Int (Components (N)));
   end W_Components_Applied_To;

   procedure W_Component (N : Node_Id) is
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
        ("Component_Category",
         "Byte",
         Image (Component_Category (N)));
   end W_Component;

   procedure W_Aspect_Precedence (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
   end W_Aspect_Precedence;

   procedure W_Pointcut_Specification (N : Node_Id) is
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
        ("Pointcut_Expression",
         "Node_Id",
         Image (Pointcut_Expression (N)),
         Int (Pointcut_Expression (N)));
   end W_Pointcut_Specification;

   procedure W_Parameter_Specification (N : Node_Id) is
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
        ("Type_Identifier",
         "Node_Id",
         Image (Type_Identifier (N)),
         Int (Type_Identifier (N)));
   end W_Parameter_Specification;

   procedure W_Pointcut_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Pointcut_Expression",
         "Node_Id",
         Image (Pointcut_Expression (N)),
         Int (Pointcut_Expression (N)));
      W_Node_Attribute
        ("Caller",
         "Node_Id",
         Image (Caller (N)),
         Int (Caller (N)));
      W_Node_Attribute
        ("Operator",
         "Node_Id",
         Image (Operator (N)),
         Int (Operator (N)));
   end W_Pointcut_Expression;

   procedure W_Caller (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Caller_Kind",
         "Byte",
         Image (Caller_Kind (N)));
      W_Node_Attribute
        ("Callee",
         "Node_Id",
         Image (Callee (N)),
         Int (Callee (N)));
   end W_Caller;

   procedure W_Call (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Caller_Kind",
         "Byte",
         Image (Caller_Kind (N)));
      W_Node_Attribute
        ("Callee",
         "Node_Id",
         Image (Callee (N)),
         Int (Callee (N)));
   end W_Call;

   procedure W_Execution (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Caller_Kind",
         "Byte",
         Image (Caller_Kind (N)));
      W_Node_Attribute
        ("Callee",
         "Node_Id",
         Image (Callee (N)),
         Int (Callee (N)));
   end W_Execution;

   procedure W_Callee (N : Node_Id) is
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
   end W_Callee;

   procedure W_Subprogram_Call (N : Node_Id) is
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
   end W_Subprogram_Call;

   procedure W_Port_Call (N : Node_Id) is
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
        ("Mode",
         "Mode_Id",
         Image (Mode (N)));
   end W_Port_Call;

   procedure W_Args (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Arguments",
         "List_Id",
         Image (Arguments (N)),
         Int (Arguments (N)));
   end W_Args;

   procedure W_Advice_Specification (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Advice_Declaration",
         "Node_Id",
         Image (Advice_Declaration (N)),
         Int (Advice_Declaration (N)));
      W_Node_Attribute
        ("Pointcut_Reference",
         "Node_Id",
         Image (Pointcut_Reference (N)),
         Int (Pointcut_Reference (N)));
      W_Node_Attribute
        ("Advice_Action",
         "Node_Id",
         Image (Advice_Action (N)),
         Int (Advice_Action (N)));
   end W_Advice_Specification;

   procedure W_Advice_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
      W_Node_Attribute
        ("Advice_Category",
         "Byte",
         Image (Advice_Category (N)));
   end W_Advice_Declaration;

   procedure W_Pointcut_Reference (N : Node_Id) is
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
        ("Parameter_Profile",
         "List_Id",
         Image (Parameter_Profile (N)),
         Int (Parameter_Profile (N)));
   end W_Pointcut_Reference;

   procedure W_Advice_Action (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Variables_Declaration",
         "Node_Id",
         Image (Variables_Declaration (N)),
         Int (Variables_Declaration (N)));
      W_Node_Attribute
        ("Initialisation",
         "Node_Id",
         Image (Initialisation (N)),
         Int (Initialisation (N)));
      W_Node_Attribute
        ("Actions",
         "List_Id",
         Image (Actions (N)),
         Int (Actions (N)));
   end W_Advice_Action;

   procedure W_Variables_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Variables",
         "List_Id",
         Image (Variables (N)),
         Int (Variables (N)));
   end W_Variables_Declaration;

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
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
      W_Node_Attribute
        ("Type_Identifier",
         "Node_Id",
         Image (Type_Identifier (N)),
         Int (Type_Identifier (N)));
   end W_Variable;

   procedure W_Initialisation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Assignments",
         "List_Id",
         Image (Assignments (N)),
         Int (Assignments (N)));
   end W_Initialisation;

   procedure W_Action (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Action_Node",
         "Node_Id",
         Image (Action_Node (N)),
         Int (Action_Node (N)));
   end W_Action;

   procedure W_Conditional_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Actions",
         "List_Id",
         Image (Actions (N)),
         Int (Actions (N)));
      W_Node_Attribute
        ("Behavior_Expression",
         "Node_Id",
         Image (Behavior_Expression (N)),
         Int (Behavior_Expression (N)));
   end W_Conditional_Statement;

   procedure W_If_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("If_Cond",
         "Node_Id",
         Image (If_Cond (N)),
         Int (If_Cond (N)));
      W_Node_Attribute
        ("Elsif_Cond",
         "Node_Id",
         Image (Elsif_Cond (N)),
         Int (Elsif_Cond (N)));
      W_Node_Attribute
        ("Else_Cond",
         "Node_Id",
         Image (Else_Cond (N)),
         Int (Else_Cond (N)));
   end W_If_Statement;

   procedure W_While_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Conditional_Statement",
         "Node_Id",
         Image (Conditional_Statement (N)),
         Int (Conditional_Statement (N)));
   end W_While_Statement;

   procedure W_For_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Actions",
         "List_Id",
         Image (Actions (N)),
         Int (Actions (N)));
      W_Node_Attribute
        ("Loop_Variable_Identifier",
         "Node_Id",
         Image (Loop_Variable_Identifier (N)),
         Int (Loop_Variable_Identifier (N)));
      W_Node_Attribute
        ("Integer_Range",
         "Node_Id",
         Image (Integer_Range (N)),
         Int (Integer_Range (N)));
   end W_For_Statement;

   procedure W_Assignment_Action (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Behavior_Expression",
         "Node_Id",
         Image (Behavior_Expression (N)),
         Int (Behavior_Expression (N)));
      W_Node_Attribute
        ("Reference_Expression",
         "Node_Id",
         Image (Reference_Expression (N)),
         Int (Reference_Expression (N)));
   end W_Assignment_Action;

   procedure W_Communication_Action (N : Node_Id) is
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
        ("Parameter_Profile",
         "List_Id",
         Image (Parameter_Profile (N)),
         Int (Parameter_Profile (N)));
      W_Node_Attribute
        ("Com_Kind",
         "Byte",
         Image (Com_Kind (N)));
   end W_Communication_Action;

   procedure W_Timed_Action (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Is_Computation",
         "Boolean",
         Image (Is_Computation (N)));
      W_Node_Attribute
        ("Fst_Behavior_Time",
         "Node_Id",
         Image (Fst_Behavior_Time (N)),
         Int (Fst_Behavior_Time (N)));
      W_Node_Attribute
        ("Scd_Behavior_Time",
         "Node_Id",
         Image (Scd_Behavior_Time (N)),
         Int (Scd_Behavior_Time (N)));
      W_Node_Attribute
        ("Distrib_Kind",
         "Byte",
         Image (Distrib_Kind (N)));
   end W_Timed_Action;

   procedure W_Reference_Expression (N : Node_Id) is
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
   end W_Reference_Expression;

   procedure W_Behavior_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Relations",
         "List_Id",
         Image (Relations (N)),
         Int (Relations (N)));
      W_Node_Attribute
        ("String_Literal",
         "Node_Id",
         Image (String_Literal (N)),
         Int (String_Literal (N)));
      W_Node_Attribute
        ("Character_Literal",
         "Node_Id",
         Image (Character_Literal (N)),
         Int (Character_Literal (N)));
   end W_Behavior_Expression;

   procedure W_Relation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Boolean_Value",
         "Boolean",
         Image (Boolean_Value (N)));
      W_Node_Attribute
        ("Simple_Exprs",
         "List_Id",
         Image (Simple_Exprs (N)),
         Int (Simple_Exprs (N)));
   end W_Relation;

   procedure W_Simple_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Term_And_Operator",
         "List_Id",
         Image (Term_And_Operator (N)),
         Int (Term_And_Operator (N)));
   end W_Simple_Expression;

   procedure W_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Factors",
         "List_Id",
         Image (Factors (N)),
         Int (Factors (N)));
   end W_Term;

   procedure W_Factor (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Is_Not",
         "Boolean",
         Image (Is_Not (N)));
      W_Node_Attribute
        ("Lower_Primary",
         "Node_Id",
         Image (Lower_Primary (N)),
         Int (Lower_Primary (N)));
   end W_Factor;

   procedure W_Property_Constant (N : Node_Id) is
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
        ("Property_Set",
         "Node_Id",
         Image (Property_Set (N)),
         Int (Property_Set (N)));
   end W_Property_Constant;

   procedure W_Operator (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Operator_Kind",
         "Byte",
         Image (Operator_Kind (N)));
   end W_Operator;

   procedure W_Boolean_Literal (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Is_True",
         "Boolean",
         Image (Is_True (N)));
   end W_Boolean_Literal;

   procedure W_Integer_Range (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Lower_Int_Val",
         "Node_Id",
         Image (Lower_Int_Val (N)),
         Int (Lower_Int_Val (N)));
      W_Node_Attribute
        ("Upper_Int_Val",
         "Node_Id",
         Image (Upper_Int_Val (N)),
         Int (Upper_Int_Val (N)));
   end W_Integer_Range;

   procedure W_Integer_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
   end W_Integer_Value;

   procedure W_Behavior_Time (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Integer_Value",
         "Node_Id",
         Image (Integer_Value (N)),
         Int (Integer_Value (N)));
      W_Node_Attribute
        ("Unit_Identifier",
         "Node_Id",
         Image (Unit_Identifier (N)),
         Int (Unit_Identifier (N)));
   end W_Behavior_Time;

   procedure W_Proceed_Action (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Parameter_Profile",
         "List_Id",
         Image (Parameter_Profile (N)),
         Int (Parameter_Profile (N)));
   end W_Proceed_Action;

   procedure W_Parameter_Profile (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Behavior_Expression",
         "Node_Id",
         Image (Behavior_Expression (N)),
         Int (Behavior_Expression (N)));
   end W_Parameter_Profile;

end Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes;
