pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.ME_AADL_BA.BA_Tree.Debug; use Ocarina.ME_AADL_BA.BA_Tree.Debug;

package body Ocarina.ME_AADL_BA.BA_Tree.Nodes is

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
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction
        or else Table (Types.Node_Id (N)).Kind = K_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Name
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction
        or else Table (Types.Node_Id (N)).Kind = K_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Name
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Next_Node;

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

   function Extra_Item (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Extra_Item;

   procedure Set_Extra_Item (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Extra_Item;

   function BE_Container (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction
        or else Table (Types.Node_Id (N)).Kind = K_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Name
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end BE_Container;

   procedure Set_BE_Container (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction
        or else Table (Types.Node_Id (N)).Kind = K_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Name
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_BE_Container;

   function Next_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction
        or else Table (Types.Node_Id (N)).Kind = K_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Name
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Next_Entity;

   procedure Set_Next_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction
        or else Table (Types.Node_Id (N)).Kind = K_Logical_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Name
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Relation
        or else Table (Types.Node_Id (N)).Kind = K_Simple_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Term
        or else Table (Types.Node_Id (N)).Kind = K_Factor
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Range
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Time
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Next_Entity;

   function Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_Behavior_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Identifier;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      return Name_Id (Table (Types.Node_Id (N)).L (2));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Name;

   function Display_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      return Name_Id (Table (Types.Node_Id (N)).L (3));
   end Display_Name;

   procedure Set_Display_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Display_Name;

   function Corresponding_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Corresponding_Entity;

   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Corresponding_Entity;

   function Scope_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Scope_Entity;

   procedure Set_Scope_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Scope_Entity;

   function Homonym (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Homonym;

   procedure Set_Homonym (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Homonym;

   function Is_Others (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Others;

   procedure Set_Is_Others (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Others;

   function Value_Constant (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Value_Constant;

   procedure Set_Value_Constant (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier_With_Value);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Value_Constant;

   function Variables (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Variables;

   procedure Set_Variables (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Variables;

   function States (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end States;

   procedure Set_States (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_States;

   function Transitions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Transitions;

   procedure Set_Transitions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Annex);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Transitions;

   function Package_Name (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Package_Name;

   procedure Set_Package_Name (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Package_Name;

   function Component_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Component_Type;

   procedure Set_Component_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Component_Type;

   function Component_Impl (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Component_Impl;

   procedure Set_Component_Impl (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Component_Impl;

   function Full_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Full_Identifier;

   procedure Set_Full_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Ref);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Full_Identifier;

   function Identifiers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Identifiers;

   procedure Set_Identifiers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Data_Component_Reference);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Identifiers;

   function Classifier_Ref (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Classifier_Ref;

   procedure Set_Classifier_Ref (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Variable
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Classifier_Ref;

   function State_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end State_Kind;

   procedure Set_State_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_State);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_State_Kind;

   function Transition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Transition;

   procedure Set_Transition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Transition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Transition;

   function Sources (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Sources;

   procedure Set_Sources (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Sources;

   function Destination (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Destination;

   procedure Set_Destination (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Destination;

   function Behavior_Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Behavior_Condition;

   procedure Set_Behavior_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Behavior_Condition;

   function Behavior_Action_Block (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Behavior_Action_Block;

   procedure Set_Behavior_Action_Block (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Behavior_Action_Block;

   function Behavior_Transition_Idt (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Behavior_Transition_Idt;

   procedure Set_Behavior_Transition_Idt (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Behavior_Transition_Idt;

   function Behavior_Transition_Priority (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Behavior_Transition_Priority;

   procedure Set_Behavior_Transition_Priority (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execution_Behavior_Transition);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Behavior_Transition_Priority;

   function Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Condition;

   procedure Set_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Condition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Condition;

   function Value_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Value_Expression;

   procedure Set_Value_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Value_Expression;

   function Is_Otherwise (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Otherwise;

   procedure Set_Is_Otherwise (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Execute_Condition);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Otherwise;

   function Trigger_Logical_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Trigger_Logical_Expr;

   procedure Set_Trigger_Logical_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Condition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Trigger_Logical_Expr;

   function Event_Triggers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Event_Triggers;

   procedure Set_Event_Triggers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Trigger_Logical_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Event_Triggers;

   function Port_Component_Ref (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Port_Component_Ref;

   procedure Set_Port_Component_Ref (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Port_Component_Ref;

   function Trigger_Log_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Trigger_Log_Expr;

   procedure Set_Trigger_Log_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Trigger);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Trigger_Log_Expr;

   function Subcomponent_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Subcomponent_Name;

   procedure Set_Subcomponent_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Subcomponent_Name;

   function Port_Idt (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Port_Idt;

   procedure Set_Port_Idt (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Component_Reference);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Port_Idt;

   function Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Source;

   procedure Set_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Source;

   function Mode_Transition_Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Mode_Transition_Condition;

   procedure Set_Mode_Transition_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Mode_Transition_Condition;

   function Behavior_Actions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Behavior_Actions;

   procedure Set_Behavior_Actions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Behavior_Actions;

   function Dispatch_Trigger_Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Dispatch_Trigger_Condition;

   procedure Set_Dispatch_Trigger_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Dispatch_Trigger_Condition;

   function Frozen_Ports (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Frozen_Ports;

   procedure Set_Frozen_Ports (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Condition_Thread);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Frozen_Ports;

   function Dispatch_Conjunction (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Dispatch_Conjunction;

   procedure Set_Dispatch_Conjunction (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Dispatch_Conjunction;

   function Behavior_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Behavior_Time;

   procedure Set_Behavior_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Behavior_Time;

   function Trigger_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Trigger_Kind;

   procedure Set_Trigger_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Trigger_Condition);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Trigger_Kind;

   function Dispatch_Triggers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Dispatch_Triggers;

   procedure Set_Dispatch_Triggers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Dispatch_Conjunction);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Dispatch_Triggers;

   function Behav_Acts (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Behav_Acts;

   procedure Set_Behav_Acts (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action_Block
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Behav_Acts;

   function Behavior_Action (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Behavior_Action;

   procedure Set_Behavior_Action (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Behavior_Action;

   function Behavior_Action_Sequence (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Behavior_Action_Sequence;

   procedure Set_Behavior_Action_Sequence (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Behavior_Action_Sequence;

   function Behavior_Action_Set (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Behavior_Action_Set;

   procedure Set_Behavior_Action_Set (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Actions);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Behavior_Action_Set;

   function Action (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Action;

   procedure Set_Action (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Behavior_Action);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Action;

   function Logical_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Logical_Expr;

   procedure Set_Logical_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Conditional_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_DoUntil_Cond_Structure);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Logical_Expr;

   function If_Statement (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end If_Statement;

   procedure Set_If_Statement (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_If_Statement;

   function Elsif_Statement (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Elsif_Statement;

   procedure Set_Elsif_Statement (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Elsif_Statement;

   function Else_Statement (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Else_Statement;

   procedure Set_Else_Statement (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Cond_Struct);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Else_Statement;

   function Element_Idt (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Element_Idt;

   procedure Set_Element_Idt (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Element_Idt;

   function In_Element_Values (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end In_Element_Values;

   procedure Set_In_Element_Values (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Cond_Structure
        or else Table (Types.Node_Id (N)).Kind = K_ForAll_Cond_Structure);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_In_Element_Values;

   function Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Entity;

   procedure Set_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element_Values
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Value);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Entity;

   function Target (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Target;

   procedure Set_Target (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Target;

   function Is_Any (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Any;

   procedure Set_Is_Any (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Action);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Any;

   function Subprogram_Parameter_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Subprogram_Parameter_List;

   procedure Set_Subprogram_Parameter_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Subprogram_Parameter_List;

   function Comm_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Comm_Kind;

   procedure Set_Comm_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Action);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Comm_Kind;

   function Fst_Behavior_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Fst_Behavior_Time;

   procedure Set_Fst_Behavior_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Fst_Behavior_Time;

   function Scd_Behavior_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Scd_Behavior_Time;

   procedure Set_Scd_Behavior_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Scd_Behavior_Time;

   function Is_InBinding (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_InBinding;

   procedure Set_Is_InBinding (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_InBinding;

   function Processor_Idt (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Processor_Idt;

   procedure Set_Processor_Idt (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Timed_Act);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Processor_Idt;

   function Parameter (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Parameter;

   procedure Set_Parameter (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Label);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Parameter;

   function Idt (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Name);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Idt;

   procedure Set_Idt (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Name);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Idt;

   function Array_Index (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Name);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Array_Index;

   procedure Set_Array_Index (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Name);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Array_Index;

   function Is_Count (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Count;

   procedure Set_Is_Count (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Count;

   function Is_Fresh (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Fresh;

   procedure Set_Is_Fresh (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Fresh;

   function Is_Updated (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Updated;

   procedure Set_Is_Updated (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Updated;

   function Is_Interrogative (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Interrogative;

   procedure Set_Is_Interrogative (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Variable);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Interrogative;

   function Relations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Relations;

   procedure Set_Relations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Value_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Relations;

   function Simple_Exprs (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Relation);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Simple_Exprs;

   procedure Set_Simple_Exprs (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Relation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
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

   function Is_Abs (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Abs;

   procedure Set_Is_Abs (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Abs;

   function Is_Not (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Not;

   procedure Set_Is_Not (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Not;

   function Lower_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Lower_Value;

   procedure Set_Lower_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Lower_Value;

   function Upper_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Upper_Value;

   procedure Set_Upper_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Factor);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Upper_Value;

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

   function Property_Set_Idt (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Set_Idt;

   procedure Set_Property_Set_Idt (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Set_Idt;

   function Property_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Property_Name;

   procedure Set_Property_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Reference);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Property_Name;

   function Property_Idt (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Idt;

   procedure Set_Property_Idt (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Idt;

   function Property_Field (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Property_Field;

   procedure Set_Property_Field (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Name);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Property_Field;

   function Ident (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Ident;

   procedure Set_Ident (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Ident;

   function Is_Self (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Self;

   procedure Set_Is_Self (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Element_Reference);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Self;

   function Is_Upper_Bound (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Upper_Bound;

   procedure Set_Is_Upper_Bound (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Upper_Bound;

   function Is_Lower_Bound (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Lower_Bound;

   procedure Set_Is_Lower_Bound (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Field);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Lower_Bound;

   function Operator_Category (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operator);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Operator_Category;

   procedure Set_Operator_Category (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operator);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Operator_Category;

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

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Node_Container =>
            W_Node_Container
              (Node_Id (N));
         when K_Behavior_Entity =>
            W_Behavior_Entity
              (Node_Id (N));
         when K_Named_Behavior_Entity =>
            W_Named_Behavior_Entity
              (Node_Id (N));
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_Identifier_With_Value =>
            W_Identifier_With_Value
              (Node_Id (N));
         when K_Behavior_Annex =>
            W_Behavior_Annex
              (Node_Id (N));
         when K_Component_Classifier_Ref =>
            W_Component_Classifier_Ref
              (Node_Id (N));
         when K_Behavior_Variable =>
            W_Behavior_Variable
              (Node_Id (N));
         when K_Behavior_State =>
            W_Behavior_State
              (Node_Id (N));
         when K_Behavior_Transition =>
            W_Behavior_Transition
              (Node_Id (N));
         when K_Execution_Behavior_Transition =>
            W_Execution_Behavior_Transition
              (Node_Id (N));
         when K_Behavior_Condition =>
            W_Behavior_Condition
              (Node_Id (N));
         when K_Execute_Condition =>
            W_Execute_Condition
              (Node_Id (N));
         when K_Mode_Condition =>
            W_Mode_Condition
              (Node_Id (N));
         when K_Trigger_Logical_Expression =>
            W_Trigger_Logical_Expression
              (Node_Id (N));
         when K_Event_Trigger =>
            W_Event_Trigger
              (Node_Id (N));
         when K_Port_Component_Reference =>
            W_Port_Component_Reference
              (Node_Id (N));
         when K_Mode_Transition =>
            W_Mode_Transition
              (Node_Id (N));
         when K_Dispatch_Condition_Thread =>
            W_Dispatch_Condition_Thread
              (Node_Id (N));
         when K_Dispatch_Trigger_Condition =>
            W_Dispatch_Trigger_Condition
              (Node_Id (N));
         when K_Dispatch_Conjunction =>
            W_Dispatch_Conjunction
              (Node_Id (N));
         when K_Logical_Expression =>
            W_Logical_Expression
              (Node_Id (N));
         when K_Behavior_Action_Block =>
            W_Behavior_Action_Block
              (Node_Id (N));
         when K_Behavior_Actions =>
            W_Behavior_Actions
              (Node_Id (N));
         when K_Behavior_Action =>
            W_Behavior_Action
              (Node_Id (N));
         when K_Conditional_Statement =>
            W_Conditional_Statement
              (Node_Id (N));
         when K_If_Cond_Struct =>
            W_If_Cond_Struct
              (Node_Id (N));
         when K_For_Cond_Structure =>
            W_For_Cond_Structure
              (Node_Id (N));
         when K_While_Cond_Structure =>
            W_While_Cond_Structure
              (Node_Id (N));
         when K_ForAll_Cond_Structure =>
            W_ForAll_Cond_Structure
              (Node_Id (N));
         when K_DoUntil_Cond_Structure =>
            W_DoUntil_Cond_Structure
              (Node_Id (N));
         when K_Element_Values =>
            W_Element_Values
              (Node_Id (N));
         when K_Assignment_Action =>
            W_Assignment_Action
              (Node_Id (N));
         when K_Communication_Action =>
            W_Communication_Action
              (Node_Id (N));
         when K_Timed_Act =>
            W_Timed_Act
              (Node_Id (N));
         when K_Parameter_Label =>
            W_Parameter_Label
              (Node_Id (N));
         when K_Data_Component_Reference =>
            W_Data_Component_Reference
              (Node_Id (N));
         when K_Name =>
            W_Name
              (Node_Id (N));
         when K_Value_Variable =>
            W_Value_Variable
              (Node_Id (N));
         when K_Value_Expression =>
            W_Value_Expression
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
         when K_Property_Reference =>
            W_Property_Reference
              (Node_Id (N));
         when K_Property_Name =>
            W_Property_Name
              (Node_Id (N));
         when K_Component_Element_Reference =>
            W_Component_Element_Reference
              (Node_Id (N));
         when K_Property_Field =>
            W_Property_Field
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
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

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
      W_Node_Attribute
        ("Extra_Item",
         "Node_Id",
         Image (Extra_Item (N)),
         Int (Extra_Item (N)));
   end W_Node_Container;

   procedure W_Behavior_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
   end W_Behavior_Entity;

   procedure W_Named_Behavior_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
   end W_Named_Behavior_Entity;

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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Display_Name",
         "Name_Id",
         Image (Display_Name (N)));
      W_Node_Attribute
        ("Corresponding_Entity",
         "Node_Id",
         Image (Corresponding_Entity (N)),
         Int (Corresponding_Entity (N)));
      W_Node_Attribute
        ("Scope_Entity",
         "Node_Id",
         Image (Scope_Entity (N)),
         Int (Scope_Entity (N)));
      W_Node_Attribute
        ("Homonym",
         "Node_Id",
         Image (Homonym (N)),
         Int (Homonym (N)));
   end W_Identifier;

   procedure W_Identifier_With_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Display_Name",
         "Name_Id",
         Image (Display_Name (N)));
      W_Node_Attribute
        ("Corresponding_Entity",
         "Node_Id",
         Image (Corresponding_Entity (N)),
         Int (Corresponding_Entity (N)));
      W_Node_Attribute
        ("Scope_Entity",
         "Node_Id",
         Image (Scope_Entity (N)),
         Int (Scope_Entity (N)));
      W_Node_Attribute
        ("Homonym",
         "Node_Id",
         Image (Homonym (N)),
         Int (Homonym (N)));
      W_Node_Attribute
        ("Is_Others",
         "Boolean",
         Image (Is_Others (N)));
      W_Node_Attribute
        ("Value_Constant",
         "Node_Id",
         Image (Value_Constant (N)),
         Int (Value_Constant (N)));
   end W_Identifier_With_Value;

   procedure W_Behavior_Annex (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Variables",
         "List_Id",
         Image (Variables (N)),
         Int (Variables (N)));
      W_Node_Attribute
        ("States",
         "List_Id",
         Image (States (N)),
         Int (States (N)));
      W_Node_Attribute
        ("Transitions",
         "List_Id",
         Image (Transitions (N)),
         Int (Transitions (N)));
   end W_Behavior_Annex;

   procedure W_Component_Classifier_Ref (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Package_Name",
         "List_Id",
         Image (Package_Name (N)),
         Int (Package_Name (N)));
      W_Node_Attribute
        ("Component_Type",
         "Node_Id",
         Image (Component_Type (N)),
         Int (Component_Type (N)));
      W_Node_Attribute
        ("Component_Impl",
         "Node_Id",
         Image (Component_Impl (N)),
         Int (Component_Impl (N)));
      W_Node_Attribute
        ("Full_Identifier",
         "Node_Id",
         Image (Full_Identifier (N)),
         Int (Full_Identifier (N)));
   end W_Component_Classifier_Ref;

   procedure W_Behavior_Variable (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
      W_Node_Attribute
        ("Classifier_Ref",
         "Node_Id",
         Image (Classifier_Ref (N)),
         Int (Classifier_Ref (N)));
   end W_Behavior_Variable;

   procedure W_Behavior_State (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
      W_Node_Attribute
        ("State_Kind",
         "Byte",
         Image (State_Kind (N)));
   end W_Behavior_State;

   procedure W_Behavior_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Transition",
         "Node_Id",
         Image (Transition (N)),
         Int (Transition (N)));
   end W_Behavior_Transition;

   procedure W_Execution_Behavior_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
      W_Node_Attribute
        ("Destination",
         "Node_Id",
         Image (Destination (N)),
         Int (Destination (N)));
      W_Node_Attribute
        ("Behavior_Condition",
         "Node_Id",
         Image (Behavior_Condition (N)),
         Int (Behavior_Condition (N)));
      W_Node_Attribute
        ("Behavior_Action_Block",
         "Node_Id",
         Image (Behavior_Action_Block (N)),
         Int (Behavior_Action_Block (N)));
      W_Node_Attribute
        ("Behavior_Transition_Idt",
         "Node_Id",
         Image (Behavior_Transition_Idt (N)),
         Int (Behavior_Transition_Idt (N)));
      W_Node_Attribute
        ("Behavior_Transition_Priority",
         "Node_Id",
         Image (Behavior_Transition_Priority (N)),
         Int (Behavior_Transition_Priority (N)));
   end W_Execution_Behavior_Transition;

   procedure W_Behavior_Condition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
   end W_Behavior_Condition;

   procedure W_Execute_Condition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Value_Expression",
         "Node_Id",
         Image (Value_Expression (N)),
         Int (Value_Expression (N)));
      W_Node_Attribute
        ("Is_Otherwise",
         "Boolean",
         Image (Is_Otherwise (N)));
   end W_Execute_Condition;

   procedure W_Mode_Condition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Trigger_Logical_Expr",
         "Node_Id",
         Image (Trigger_Logical_Expr (N)),
         Int (Trigger_Logical_Expr (N)));
   end W_Mode_Condition;

   procedure W_Trigger_Logical_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Event_Triggers",
         "List_Id",
         Image (Event_Triggers (N)),
         Int (Event_Triggers (N)));
   end W_Trigger_Logical_Expression;

   procedure W_Event_Trigger (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Port_Component_Ref",
         "Node_Id",
         Image (Port_Component_Ref (N)),
         Int (Port_Component_Ref (N)));
      W_Node_Attribute
        ("Trigger_Log_Expr",
         "Node_Id",
         Image (Trigger_Log_Expr (N)),
         Int (Trigger_Log_Expr (N)));
   end W_Event_Trigger;

   procedure W_Port_Component_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Subcomponent_Name",
         "Node_Id",
         Image (Subcomponent_Name (N)),
         Int (Subcomponent_Name (N)));
      W_Node_Attribute
        ("Port_Idt",
         "Node_Id",
         Image (Port_Idt (N)),
         Int (Port_Idt (N)));
   end W_Port_Component_Reference;

   procedure W_Mode_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Destination",
         "Node_Id",
         Image (Destination (N)),
         Int (Destination (N)));
      W_Node_Attribute
        ("Source",
         "Node_Id",
         Image (Source (N)),
         Int (Source (N)));
      W_Node_Attribute
        ("Mode_Transition_Condition",
         "Node_Id",
         Image (Mode_Transition_Condition (N)),
         Int (Mode_Transition_Condition (N)));
      W_Node_Attribute
        ("Behavior_Actions",
         "List_Id",
         Image (Behavior_Actions (N)),
         Int (Behavior_Actions (N)));
   end W_Mode_Transition;

   procedure W_Dispatch_Condition_Thread (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Dispatch_Trigger_Condition",
         "Node_Id",
         Image (Dispatch_Trigger_Condition (N)),
         Int (Dispatch_Trigger_Condition (N)));
      W_Node_Attribute
        ("Frozen_Ports",
         "List_Id",
         Image (Frozen_Ports (N)),
         Int (Frozen_Ports (N)));
   end W_Dispatch_Condition_Thread;

   procedure W_Dispatch_Trigger_Condition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Dispatch_Conjunction",
         "List_Id",
         Image (Dispatch_Conjunction (N)),
         Int (Dispatch_Conjunction (N)));
      W_Node_Attribute
        ("Behavior_Time",
         "Node_Id",
         Image (Behavior_Time (N)),
         Int (Behavior_Time (N)));
      W_Node_Attribute
        ("Trigger_Kind",
         "Byte",
         Image (Trigger_Kind (N)));
   end W_Dispatch_Trigger_Condition;

   procedure W_Dispatch_Conjunction (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Dispatch_Triggers",
         "List_Id",
         Image (Dispatch_Triggers (N)),
         Int (Dispatch_Triggers (N)));
   end W_Dispatch_Conjunction;

   procedure W_Logical_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
   end W_Logical_Expression;

   procedure W_Behavior_Action_Block (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Behavior_Time",
         "Node_Id",
         Image (Behavior_Time (N)),
         Int (Behavior_Time (N)));
      W_Node_Attribute
        ("Behav_Acts",
         "Node_Id",
         Image (Behav_Acts (N)),
         Int (Behav_Acts (N)));
   end W_Behavior_Action_Block;

   procedure W_Behavior_Actions (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Behavior_Action",
         "Node_Id",
         Image (Behavior_Action (N)),
         Int (Behavior_Action (N)));
      W_Node_Attribute
        ("Behavior_Action_Sequence",
         "List_Id",
         Image (Behavior_Action_Sequence (N)),
         Int (Behavior_Action_Sequence (N)));
      W_Node_Attribute
        ("Behavior_Action_Set",
         "List_Id",
         Image (Behavior_Action_Set (N)),
         Int (Behavior_Action_Set (N)));
   end W_Behavior_Actions;

   procedure W_Behavior_Action (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Action",
         "Node_Id",
         Image (Action (N)),
         Int (Action (N)));
   end W_Behavior_Action;

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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Behav_Acts",
         "Node_Id",
         Image (Behav_Acts (N)),
         Int (Behav_Acts (N)));
      W_Node_Attribute
        ("Logical_Expr",
         "Node_Id",
         Image (Logical_Expr (N)),
         Int (Logical_Expr (N)));
   end W_Conditional_Statement;

   procedure W_If_Cond_Struct (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("If_Statement",
         "Node_Id",
         Image (If_Statement (N)),
         Int (If_Statement (N)));
      W_Node_Attribute
        ("Elsif_Statement",
         "Node_Id",
         Image (Elsif_Statement (N)),
         Int (Elsif_Statement (N)));
      W_Node_Attribute
        ("Else_Statement",
         "Node_Id",
         Image (Else_Statement (N)),
         Int (Else_Statement (N)));
   end W_If_Cond_Struct;

   procedure W_For_Cond_Structure (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Classifier_Ref",
         "Node_Id",
         Image (Classifier_Ref (N)),
         Int (Classifier_Ref (N)));
      W_Node_Attribute
        ("Behav_Acts",
         "Node_Id",
         Image (Behav_Acts (N)),
         Int (Behav_Acts (N)));
      W_Node_Attribute
        ("Element_Idt",
         "Node_Id",
         Image (Element_Idt (N)),
         Int (Element_Idt (N)));
      W_Node_Attribute
        ("In_Element_Values",
         "Node_Id",
         Image (In_Element_Values (N)),
         Int (In_Element_Values (N)));
   end W_For_Cond_Structure;

   procedure W_While_Cond_Structure (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Behav_Acts",
         "Node_Id",
         Image (Behav_Acts (N)),
         Int (Behav_Acts (N)));
      W_Node_Attribute
        ("Logical_Expr",
         "Node_Id",
         Image (Logical_Expr (N)),
         Int (Logical_Expr (N)));
   end W_While_Cond_Structure;

   procedure W_ForAll_Cond_Structure (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Classifier_Ref",
         "Node_Id",
         Image (Classifier_Ref (N)),
         Int (Classifier_Ref (N)));
      W_Node_Attribute
        ("Behav_Acts",
         "Node_Id",
         Image (Behav_Acts (N)),
         Int (Behav_Acts (N)));
      W_Node_Attribute
        ("Element_Idt",
         "Node_Id",
         Image (Element_Idt (N)),
         Int (Element_Idt (N)));
      W_Node_Attribute
        ("In_Element_Values",
         "Node_Id",
         Image (In_Element_Values (N)),
         Int (In_Element_Values (N)));
   end W_ForAll_Cond_Structure;

   procedure W_DoUntil_Cond_Structure (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Behav_Acts",
         "Node_Id",
         Image (Behav_Acts (N)),
         Int (Behav_Acts (N)));
      W_Node_Attribute
        ("Logical_Expr",
         "Node_Id",
         Image (Logical_Expr (N)),
         Int (Logical_Expr (N)));
   end W_DoUntil_Cond_Structure;

   procedure W_Element_Values (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
   end W_Element_Values;

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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Value_Expression",
         "Node_Id",
         Image (Value_Expression (N)),
         Int (Value_Expression (N)));
      W_Node_Attribute
        ("Target",
         "Node_Id",
         Image (Target (N)),
         Int (Target (N)));
      W_Node_Attribute
        ("Is_Any",
         "Boolean",
         Image (Is_Any (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Target",
         "Node_Id",
         Image (Target (N)),
         Int (Target (N)));
      W_Node_Attribute
        ("Subprogram_Parameter_List",
         "List_Id",
         Image (Subprogram_Parameter_List (N)),
         Int (Subprogram_Parameter_List (N)));
      W_Node_Attribute
        ("Comm_Kind",
         "Byte",
         Image (Comm_Kind (N)));
   end W_Communication_Action;

   procedure W_Timed_Act (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("Is_InBinding",
         "Boolean",
         Image (Is_InBinding (N)));
      W_Node_Attribute
        ("Processor_Idt",
         "List_Id",
         Image (Processor_Idt (N)),
         Int (Processor_Idt (N)));
   end W_Timed_Act;

   procedure W_Parameter_Label (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Parameter",
         "Node_Id",
         Image (Parameter (N)),
         Int (Parameter (N)));
   end W_Parameter_Label;

   procedure W_Data_Component_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
   end W_Data_Component_Reference;

   procedure W_Name (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Idt",
         "List_Id",
         Image (Idt (N)),
         Int (Idt (N)));
      W_Node_Attribute
        ("Array_Index",
         "List_Id",
         Image (Array_Index (N)),
         Int (Array_Index (N)));
   end W_Name;

   procedure W_Value_Variable (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_Count",
         "Boolean",
         Image (Is_Count (N)));
      W_Node_Attribute
        ("Is_Fresh",
         "Boolean",
         Image (Is_Fresh (N)));
      W_Node_Attribute
        ("Is_Updated",
         "Boolean",
         Image (Is_Updated (N)));
      W_Node_Attribute
        ("Is_Interrogative",
         "Boolean",
         Image (Is_Interrogative (N)));
   end W_Value_Variable;

   procedure W_Value_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Relations",
         "List_Id",
         Image (Relations (N)),
         Int (Relations (N)));
   end W_Value_Expression;

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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Is_Abs",
         "Boolean",
         Image (Is_Abs (N)));
      W_Node_Attribute
        ("Is_Not",
         "Boolean",
         Image (Is_Not (N)));
      W_Node_Attribute
        ("Lower_Value",
         "Node_Id",
         Image (Lower_Value (N)),
         Int (Lower_Value (N)));
      W_Node_Attribute
        ("Upper_Value",
         "Node_Id",
         Image (Upper_Value (N)),
         Int (Upper_Value (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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

   procedure W_Property_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Property_Set_Idt",
         "Node_Id",
         Image (Property_Set_Idt (N)),
         Int (Property_Set_Idt (N)));
      W_Node_Attribute
        ("Property_Name",
         "Node_Id",
         Image (Property_Name (N)),
         Int (Property_Name (N)));
   end W_Property_Reference;

   procedure W_Property_Name (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Property_Idt",
         "Node_Id",
         Image (Property_Idt (N)),
         Int (Property_Idt (N)));
      W_Node_Attribute
        ("Property_Field",
         "List_Id",
         Image (Property_Field (N)),
         Int (Property_Field (N)));
   end W_Property_Name;

   procedure W_Component_Element_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Ident",
         "Node_Id",
         Image (Ident (N)),
         Int (Ident (N)));
      W_Node_Attribute
        ("Is_Self",
         "Boolean",
         Image (Is_Self (N)));
   end W_Component_Element_Reference;

   procedure W_Property_Field (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Is_Upper_Bound",
         "Boolean",
         Image (Is_Upper_Bound (N)));
      W_Node_Attribute
        ("Is_Lower_Bound",
         "Boolean",
         Image (Is_Lower_Bound (N)));
   end W_Property_Field;

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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Operator_Category",
         "Byte",
         Image (Operator_Category (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
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
        ("BE_Container",
         "Node_Id",
         Image (BE_Container (N)),
         Int (BE_Container (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Value",
         "Value_Id",
         Image (Value (N)));
   end W_Literal;

end Ocarina.ME_AADL_BA.BA_Tree.Nodes;
