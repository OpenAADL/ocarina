pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.ME_AADL_BA.BA_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_List_Id,
      K_Node_Container,
      K_Behavior_Entity,
      K_Named_Behavior_Entity,
      K_Identifier,
      K_Identifier_With_Value,
      K_Behavior_Annex,
      K_Component_Classifier_Ref,
      K_Behavior_Variable,
      K_Behavior_State,
      K_Behavior_Transition,
      K_Execution_Behavior_Transition,
      K_Behavior_Condition,
      K_Execute_Condition,
      K_Mode_Condition,
      K_Trigger_Logical_Expression,
      K_Event_Trigger,
      K_Port_Component_Reference,
      K_Mode_Transition,
      K_Dispatch_Condition_Thread,
      K_Dispatch_Trigger_Condition,
      K_Dispatch_Conjunction,
      K_Logical_Expression,
      K_Behavior_Action_Block,
      K_Behavior_Actions,
      K_Behavior_Action,
      K_Conditional_Statement,
      K_If_Cond_Struct,
      K_For_Cond_Structure,
      K_While_Cond_Structure,
      K_ForAll_Cond_Structure,
      K_DoUntil_Cond_Structure,
      K_Element_Values,
      K_Assignment_Action,
      K_Communication_Action,
      K_Timed_Act,
      K_Parameter_Label,
      K_Data_Component_Reference,
      K_Name,
      K_Value_Variable,
      K_Value_Expression,
      K_Relation,
      K_Simple_Expression,
      K_Term,
      K_Factor,
      K_Property_Constant,
      K_Property_Reference,
      K_Property_Name,
      K_Component_Element_Reference,
      K_Property_Field,
      K_Operator,
      K_Boolean_Literal,
      K_Integer_Range,
      K_Integer_Value,
      K_Behavior_Time,
      K_Literal);

   --
   --  Node_Id
   --
   --    Next_Node                : Node_Id
   --

   --
   --  List_Id
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   --
   --  Node_Container
   --
   --    Next_Node                : Node_Id
   --    Item                     : Node_Id
   --    Extra_Item               : Node_Id
   --

   procedure W_Node_Container (N : Node_Id);

   --
   --  Behavior_Entity
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --

   procedure W_Behavior_Entity (N : Node_Id);

   --
   --  Named_Behavior_Entity
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Named_Behavior_Entity (N : Node_Id);

   --
   --  Identifier
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Name                     : Name_Id
   --    Display_Name             : Name_Id
   --    Corresponding_Entity     : Node_Id
   --    Scope_Entity             : Node_Id
   --    Homonym                  : Node_Id
   --

   procedure W_Identifier (N : Node_Id);

   --
   --  Identifier_With_Value
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Name                     : Name_Id
   --    Display_Name             : Name_Id
   --    Corresponding_Entity     : Node_Id
   --    Scope_Entity             : Node_Id
   --    Homonym                  : Node_Id
   --    Is_Others                : Boolean
   --    Value_Constant           : Node_Id
   --

   procedure W_Identifier_With_Value (N : Node_Id);

   --
   --  Behavior_Annex
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Variables                : List_Id
   --    States                   : List_Id
   --    Transitions              : List_Id
   --

   procedure W_Behavior_Annex (N : Node_Id);

   --
   --  Component_Classifier_Ref
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Package_Name             : List_Id
   --    Component_Type           : Node_Id
   --    Component_Impl           : Node_Id
   --    Full_Identifier          : Node_Id
   --

   procedure W_Component_Classifier_Ref (N : Node_Id);

   --
   --  Behavior_Variable
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifiers              : List_Id
   --    Classifier_Ref           : Node_Id
   --

   procedure W_Behavior_Variable (N : Node_Id);

   --
   --  Behavior_State
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifiers              : List_Id
   --    State_Kind               : Byte
   --

   procedure W_Behavior_State (N : Node_Id);

   --
   --  Behavior_Transition
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Transition               : Node_Id
   --

   procedure W_Behavior_Transition (N : Node_Id);

   --
   --  Execution_Behavior_Transition
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Sources                  : List_Id
   --    Destination              : Node_Id
   --    Behavior_Condition       : Node_Id
   --    Behavior_Action_Block    : Node_Id
   --    Behavior_Transition_Idt  : Node_Id
   --    Behavior_Transition_Priority: Node_Id
   --

   procedure W_Execution_Behavior_Transition (N : Node_Id);

   --
   --  Behavior_Condition
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Condition                : Node_Id
   --

   procedure W_Behavior_Condition (N : Node_Id);

   --
   --  Execute_Condition
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Value_Expression         : Node_Id
   --    Is_Otherwise             : Boolean
   --

   procedure W_Execute_Condition (N : Node_Id);

   --
   --  Mode_Condition
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Trigger_Logical_Expr     : Node_Id
   --

   procedure W_Mode_Condition (N : Node_Id);

   --
   --  Trigger_Logical_Expression
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Event_Triggers           : List_Id
   --

   procedure W_Trigger_Logical_Expression (N : Node_Id);

   --
   --  Event_Trigger
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Port_Component_Ref       : Node_Id
   --    Trigger_Log_Expr         : Node_Id
   --

   procedure W_Event_Trigger (N : Node_Id);

   --
   --  Port_Component_Reference
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Subcomponent_Name        : Node_Id
   --    Port_Idt                 : Node_Id
   --

   procedure W_Port_Component_Reference (N : Node_Id);

   --
   --  Mode_Transition
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Source                   : Node_Id
   --    Destination              : Node_Id
   --    Mode_Transition_Condition: Node_Id
   --    Behavior_Actions         : List_Id
   --

   procedure W_Mode_Transition (N : Node_Id);

   --
   --  Dispatch_Condition_Thread
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Dispatch_Trigger_Condition: Node_Id
   --    Frozen_Ports             : List_Id
   --

   procedure W_Dispatch_Condition_Thread (N : Node_Id);

   --
   --  Dispatch_Trigger_Condition
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Dispatch_Conjunction     : List_Id
   --    Behavior_Time            : Node_Id
   --    Trigger_Kind             : Byte
   --

   procedure W_Dispatch_Trigger_Condition (N : Node_Id);

   --
   --  Dispatch_Conjunction
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Dispatch_Triggers        : List_Id
   --

   procedure W_Dispatch_Conjunction (N : Node_Id);

   --
   --  Logical_Expression
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --

   procedure W_Logical_Expression (N : Node_Id);

   --
   --  Behavior_Action_Block
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Behav_Acts               : Node_Id
   --    Behavior_Time            : Node_Id
   --

   procedure W_Behavior_Action_Block (N : Node_Id);

   --
   --  Behavior_Actions
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Behavior_Action          : Node_Id
   --    Behavior_Action_Sequence : List_Id
   --    Behavior_Action_Set      : List_Id
   --

   procedure W_Behavior_Actions (N : Node_Id);

   --
   --  Behavior_Action
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Action                   : Node_Id
   --

   procedure W_Behavior_Action (N : Node_Id);

   --
   --  Conditional_Statement
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Logical_Expr             : Node_Id
   --    Behav_Acts               : Node_Id
   --

   procedure W_Conditional_Statement (N : Node_Id);

   --
   --  If_Cond_Struct
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    If_Statement             : Node_Id
   --    Elsif_Statement          : Node_Id
   --    Else_Statement           : Node_Id
   --

   procedure W_If_Cond_Struct (N : Node_Id);

   --
   --  For_Cond_Structure
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Element_Idt              : Node_Id
   --    Classifier_Ref           : Node_Id
   --    In_Element_Values        : Node_Id
   --    Behav_Acts               : Node_Id
   --

   procedure W_For_Cond_Structure (N : Node_Id);

   --
   --  While_Cond_Structure
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Logical_Expr             : Node_Id
   --    Behav_Acts               : Node_Id
   --

   procedure W_While_Cond_Structure (N : Node_Id);

   --
   --  ForAll_Cond_Structure
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Element_Idt              : Node_Id
   --    Classifier_Ref           : Node_Id
   --    In_Element_Values        : Node_Id
   --    Behav_Acts               : Node_Id
   --

   procedure W_ForAll_Cond_Structure (N : Node_Id);

   --
   --  DoUntil_Cond_Structure
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Logical_Expr             : Node_Id
   --    Behav_Acts               : Node_Id
   --

   procedure W_DoUntil_Cond_Structure (N : Node_Id);

   --
   --  Element_Values
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Element_Values (N : Node_Id);

   --
   --  Assignment_Action
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Target                   : Node_Id
   --    Value_Expression         : Node_Id
   --    Is_Any                   : Boolean
   --

   procedure W_Assignment_Action (N : Node_Id);

   --
   --  Communication_Action
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Subprogram_Parameter_List: List_Id
   --    Target                   : Node_Id
   --    Comm_Kind                : Byte
   --

   procedure W_Communication_Action (N : Node_Id);

   --
   --  Timed_Act
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Fst_Behavior_Time        : Node_Id
   --    Scd_Behavior_Time        : Node_Id
   --    Is_InBinding             : Boolean
   --    Processor_Idt            : List_Id
   --

   procedure W_Timed_Act (N : Node_Id);

   --
   --  Parameter_Label
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Parameter                : Node_Id
   --

   procedure W_Parameter_Label (N : Node_Id);

   --
   --  Data_Component_Reference
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifiers              : List_Id
   --

   procedure W_Data_Component_Reference (N : Node_Id);

   --
   --  Name
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Idt                      : List_Id
   --    Array_Index              : List_Id
   --

   procedure W_Name (N : Node_Id);

   --
   --  Value_Variable
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Is_Count                 : Boolean
   --    Is_Fresh                 : Boolean
   --    Is_Updated               : Boolean
   --    Is_Interrogative         : Boolean
   --

   procedure W_Value_Variable (N : Node_Id);

   --
   --  Value_Expression
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Relations                : List_Id
   --

   procedure W_Value_Expression (N : Node_Id);

   --
   --  Relation
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Simple_Exprs             : List_Id
   --

   procedure W_Relation (N : Node_Id);

   --
   --  Simple_Expression
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Term_And_Operator        : List_Id
   --

   procedure W_Simple_Expression (N : Node_Id);

   --
   --  Term
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Factors                  : List_Id
   --

   procedure W_Term (N : Node_Id);

   --
   --  Factor
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Is_Abs                   : Boolean
   --    Is_Not                   : Boolean
   --    Lower_Value              : Node_Id
   --    Upper_Value              : Node_Id
   --

   procedure W_Factor (N : Node_Id);

   --
   --  Property_Constant
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set             : Node_Id
   --

   procedure W_Property_Constant (N : Node_Id);

   --
   --  Property_Reference
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Property_Set_Idt         : Node_Id
   --    Entity                   : Node_Id
   --    Property_Name            : Node_Id
   --

   procedure W_Property_Reference (N : Node_Id);

   --
   --  Property_Name
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Property_Idt             : Node_Id
   --    Property_Field           : List_Id
   --

   procedure W_Property_Name (N : Node_Id);

   --
   --  Component_Element_Reference
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Ident                    : Node_Id
   --    Is_Self                  : Boolean
   --

   procedure W_Component_Element_Reference (N : Node_Id);

   --
   --  Property_Field
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Is_Upper_Bound           : Boolean
   --    Is_Lower_Bound           : Boolean
   --    Entity                   : Node_Id
   --

   procedure W_Property_Field (N : Node_Id);

   --
   --  Operator
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Operator_Category        : Byte
   --

   procedure W_Operator (N : Node_Id);

   --
   --  Boolean_Literal
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Is_True                  : Boolean
   --

   procedure W_Boolean_Literal (N : Node_Id);

   --
   --  Integer_Range
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Lower_Int_Val            : Node_Id
   --    Upper_Int_Val            : Node_Id
   --

   procedure W_Integer_Range (N : Node_Id);

   --
   --  Integer_Value
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Integer_Value (N : Node_Id);

   --
   --  Behavior_Time
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Integer_Value            : Node_Id
   --    Unit_Identifier          : Node_Id
   --

   procedure W_Behavior_Time (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    BE_Container             : Node_Id
   --    Next_Entity              : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Item (N : Node_Id) return Node_Id;
   procedure Set_Item (N : Node_Id; V : Node_Id);

   function Extra_Item (N : Node_Id) return Node_Id;
   procedure Set_Extra_Item (N : Node_Id; V : Node_Id);

   function BE_Container (N : Node_Id) return Node_Id;
   procedure Set_BE_Container (N : Node_Id; V : Node_Id);

   function Next_Entity (N : Node_Id) return Node_Id;
   procedure Set_Next_Entity (N : Node_Id; V : Node_Id);

   function Identifier (N : Node_Id) return Node_Id;
   procedure Set_Identifier (N : Node_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Display_Name (N : Node_Id) return Name_Id;
   procedure Set_Display_Name (N : Node_Id; V : Name_Id);

   function Corresponding_Entity (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id);

   function Scope_Entity (N : Node_Id) return Node_Id;
   procedure Set_Scope_Entity (N : Node_Id; V : Node_Id);

   function Homonym (N : Node_Id) return Node_Id;
   procedure Set_Homonym (N : Node_Id; V : Node_Id);

   function Is_Others (N : Node_Id) return Boolean;
   procedure Set_Is_Others (N : Node_Id; V : Boolean);

   function Value_Constant (N : Node_Id) return Node_Id;
   procedure Set_Value_Constant (N : Node_Id; V : Node_Id);

   function Variables (N : Node_Id) return List_Id;
   procedure Set_Variables (N : Node_Id; V : List_Id);

   function States (N : Node_Id) return List_Id;
   procedure Set_States (N : Node_Id; V : List_Id);

   function Transitions (N : Node_Id) return List_Id;
   procedure Set_Transitions (N : Node_Id; V : List_Id);

   function Package_Name (N : Node_Id) return List_Id;
   procedure Set_Package_Name (N : Node_Id; V : List_Id);

   function Component_Type (N : Node_Id) return Node_Id;
   procedure Set_Component_Type (N : Node_Id; V : Node_Id);

   function Component_Impl (N : Node_Id) return Node_Id;
   procedure Set_Component_Impl (N : Node_Id; V : Node_Id);

   function Full_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Full_Identifier (N : Node_Id; V : Node_Id);

   function Identifiers (N : Node_Id) return List_Id;
   procedure Set_Identifiers (N : Node_Id; V : List_Id);

   function Classifier_Ref (N : Node_Id) return Node_Id;
   procedure Set_Classifier_Ref (N : Node_Id; V : Node_Id);

   function State_Kind (N : Node_Id) return Byte;
   procedure Set_State_Kind (N : Node_Id; V : Byte);

   function Transition (N : Node_Id) return Node_Id;
   procedure Set_Transition (N : Node_Id; V : Node_Id);

   function Sources (N : Node_Id) return List_Id;
   procedure Set_Sources (N : Node_Id; V : List_Id);

   function Destination (N : Node_Id) return Node_Id;
   procedure Set_Destination (N : Node_Id; V : Node_Id);

   function Behavior_Condition (N : Node_Id) return Node_Id;
   procedure Set_Behavior_Condition (N : Node_Id; V : Node_Id);

   function Behavior_Action_Block (N : Node_Id) return Node_Id;
   procedure Set_Behavior_Action_Block (N : Node_Id; V : Node_Id);

   function Behavior_Transition_Idt (N : Node_Id) return Node_Id;
   procedure Set_Behavior_Transition_Idt (N : Node_Id; V : Node_Id);

   function Behavior_Transition_Priority (N : Node_Id) return Node_Id;
   procedure Set_Behavior_Transition_Priority (N : Node_Id; V : Node_Id);

   function Condition (N : Node_Id) return Node_Id;
   procedure Set_Condition (N : Node_Id; V : Node_Id);

   function Value_Expression (N : Node_Id) return Node_Id;
   procedure Set_Value_Expression (N : Node_Id; V : Node_Id);

   function Is_Otherwise (N : Node_Id) return Boolean;
   procedure Set_Is_Otherwise (N : Node_Id; V : Boolean);

   function Trigger_Logical_Expr (N : Node_Id) return Node_Id;
   procedure Set_Trigger_Logical_Expr (N : Node_Id; V : Node_Id);

   function Event_Triggers (N : Node_Id) return List_Id;
   procedure Set_Event_Triggers (N : Node_Id; V : List_Id);

   function Port_Component_Ref (N : Node_Id) return Node_Id;
   procedure Set_Port_Component_Ref (N : Node_Id; V : Node_Id);

   function Trigger_Log_Expr (N : Node_Id) return Node_Id;
   procedure Set_Trigger_Log_Expr (N : Node_Id; V : Node_Id);

   function Subcomponent_Name (N : Node_Id) return Node_Id;
   procedure Set_Subcomponent_Name (N : Node_Id; V : Node_Id);

   function Port_Idt (N : Node_Id) return Node_Id;
   procedure Set_Port_Idt (N : Node_Id; V : Node_Id);

   function Source (N : Node_Id) return Node_Id;
   procedure Set_Source (N : Node_Id; V : Node_Id);

   function Mode_Transition_Condition (N : Node_Id) return Node_Id;
   procedure Set_Mode_Transition_Condition (N : Node_Id; V : Node_Id);

   function Behavior_Actions (N : Node_Id) return List_Id;
   procedure Set_Behavior_Actions (N : Node_Id; V : List_Id);

   function Dispatch_Trigger_Condition (N : Node_Id) return Node_Id;
   procedure Set_Dispatch_Trigger_Condition (N : Node_Id; V : Node_Id);

   function Frozen_Ports (N : Node_Id) return List_Id;
   procedure Set_Frozen_Ports (N : Node_Id; V : List_Id);

   function Dispatch_Conjunction (N : Node_Id) return List_Id;
   procedure Set_Dispatch_Conjunction (N : Node_Id; V : List_Id);

   function Behavior_Time (N : Node_Id) return Node_Id;
   procedure Set_Behavior_Time (N : Node_Id; V : Node_Id);

   function Trigger_Kind (N : Node_Id) return Byte;
   procedure Set_Trigger_Kind (N : Node_Id; V : Byte);

   function Dispatch_Triggers (N : Node_Id) return List_Id;
   procedure Set_Dispatch_Triggers (N : Node_Id; V : List_Id);

   function Behav_Acts (N : Node_Id) return Node_Id;
   procedure Set_Behav_Acts (N : Node_Id; V : Node_Id);

   function Behavior_Action (N : Node_Id) return Node_Id;
   procedure Set_Behavior_Action (N : Node_Id; V : Node_Id);

   function Behavior_Action_Sequence (N : Node_Id) return List_Id;
   procedure Set_Behavior_Action_Sequence (N : Node_Id; V : List_Id);

   function Behavior_Action_Set (N : Node_Id) return List_Id;
   procedure Set_Behavior_Action_Set (N : Node_Id; V : List_Id);

   function Action (N : Node_Id) return Node_Id;
   procedure Set_Action (N : Node_Id; V : Node_Id);

   function Logical_Expr (N : Node_Id) return Node_Id;
   procedure Set_Logical_Expr (N : Node_Id; V : Node_Id);

   function If_Statement (N : Node_Id) return Node_Id;
   procedure Set_If_Statement (N : Node_Id; V : Node_Id);

   function Elsif_Statement (N : Node_Id) return Node_Id;
   procedure Set_Elsif_Statement (N : Node_Id; V : Node_Id);

   function Else_Statement (N : Node_Id) return Node_Id;
   procedure Set_Else_Statement (N : Node_Id; V : Node_Id);

   function Element_Idt (N : Node_Id) return Node_Id;
   procedure Set_Element_Idt (N : Node_Id; V : Node_Id);

   function In_Element_Values (N : Node_Id) return Node_Id;
   procedure Set_In_Element_Values (N : Node_Id; V : Node_Id);

   function Entity (N : Node_Id) return Node_Id;
   procedure Set_Entity (N : Node_Id; V : Node_Id);

   function Target (N : Node_Id) return Node_Id;
   procedure Set_Target (N : Node_Id; V : Node_Id);

   function Is_Any (N : Node_Id) return Boolean;
   procedure Set_Is_Any (N : Node_Id; V : Boolean);

   function Subprogram_Parameter_List (N : Node_Id) return List_Id;
   procedure Set_Subprogram_Parameter_List (N : Node_Id; V : List_Id);

   function Comm_Kind (N : Node_Id) return Byte;
   procedure Set_Comm_Kind (N : Node_Id; V : Byte);

   function Fst_Behavior_Time (N : Node_Id) return Node_Id;
   procedure Set_Fst_Behavior_Time (N : Node_Id; V : Node_Id);

   function Scd_Behavior_Time (N : Node_Id) return Node_Id;
   procedure Set_Scd_Behavior_Time (N : Node_Id; V : Node_Id);

   function Is_InBinding (N : Node_Id) return Boolean;
   procedure Set_Is_InBinding (N : Node_Id; V : Boolean);

   function Processor_Idt (N : Node_Id) return List_Id;
   procedure Set_Processor_Idt (N : Node_Id; V : List_Id);

   function Parameter (N : Node_Id) return Node_Id;
   procedure Set_Parameter (N : Node_Id; V : Node_Id);

   function Idt (N : Node_Id) return List_Id;
   procedure Set_Idt (N : Node_Id; V : List_Id);

   function Array_Index (N : Node_Id) return List_Id;
   procedure Set_Array_Index (N : Node_Id; V : List_Id);

   function Is_Count (N : Node_Id) return Boolean;
   procedure Set_Is_Count (N : Node_Id; V : Boolean);

   function Is_Fresh (N : Node_Id) return Boolean;
   procedure Set_Is_Fresh (N : Node_Id; V : Boolean);

   function Is_Updated (N : Node_Id) return Boolean;
   procedure Set_Is_Updated (N : Node_Id; V : Boolean);

   function Is_Interrogative (N : Node_Id) return Boolean;
   procedure Set_Is_Interrogative (N : Node_Id; V : Boolean);

   function Relations (N : Node_Id) return List_Id;
   procedure Set_Relations (N : Node_Id; V : List_Id);

   function Simple_Exprs (N : Node_Id) return List_Id;
   procedure Set_Simple_Exprs (N : Node_Id; V : List_Id);

   function Term_And_Operator (N : Node_Id) return List_Id;
   procedure Set_Term_And_Operator (N : Node_Id; V : List_Id);

   function Factors (N : Node_Id) return List_Id;
   procedure Set_Factors (N : Node_Id; V : List_Id);

   function Is_Abs (N : Node_Id) return Boolean;
   procedure Set_Is_Abs (N : Node_Id; V : Boolean);

   function Is_Not (N : Node_Id) return Boolean;
   procedure Set_Is_Not (N : Node_Id; V : Boolean);

   function Lower_Value (N : Node_Id) return Node_Id;
   procedure Set_Lower_Value (N : Node_Id; V : Node_Id);

   function Upper_Value (N : Node_Id) return Node_Id;
   procedure Set_Upper_Value (N : Node_Id; V : Node_Id);

   function Property_Set (N : Node_Id) return Node_Id;
   procedure Set_Property_Set (N : Node_Id; V : Node_Id);

   function Property_Set_Idt (N : Node_Id) return Node_Id;
   procedure Set_Property_Set_Idt (N : Node_Id; V : Node_Id);

   function Property_Name (N : Node_Id) return Node_Id;
   procedure Set_Property_Name (N : Node_Id; V : Node_Id);

   function Property_Idt (N : Node_Id) return Node_Id;
   procedure Set_Property_Idt (N : Node_Id; V : Node_Id);

   function Property_Field (N : Node_Id) return List_Id;
   procedure Set_Property_Field (N : Node_Id; V : List_Id);

   function Ident (N : Node_Id) return Node_Id;
   procedure Set_Ident (N : Node_Id; V : Node_Id);

   function Is_Self (N : Node_Id) return Boolean;
   procedure Set_Is_Self (N : Node_Id; V : Boolean);

   function Is_Upper_Bound (N : Node_Id) return Boolean;
   procedure Set_Is_Upper_Bound (N : Node_Id; V : Boolean);

   function Is_Lower_Bound (N : Node_Id) return Boolean;
   procedure Set_Is_Lower_Bound (N : Node_Id; V : Boolean);

   function Operator_Category (N : Node_Id) return Byte;
   procedure Set_Operator_Category (N : Node_Id; V : Byte);

   function Is_True (N : Node_Id) return Boolean;
   procedure Set_Is_True (N : Node_Id; V : Boolean);

   function Lower_Int_Val (N : Node_Id) return Node_Id;
   procedure Set_Lower_Int_Val (N : Node_Id; V : Node_Id);

   function Upper_Int_Val (N : Node_Id) return Node_Id;
   procedure Set_Upper_Int_Val (N : Node_Id; V : Node_Id);

   function Integer_Value (N : Node_Id) return Node_Id;
   procedure Set_Integer_Value (N : Node_Id; V : Node_Id);

   function Unit_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Unit_Identifier (N : Node_Id; V : Node_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 4) of Boolean;
   type Byte_Array is array (1 .. 1) of Byte;
   type Int_Array is array (1 .. 10) of Int;

   type Node_Entry is record
      Kind : Node_Kind;
      B : Boolean_Array;
      O : Byte_Array;
      L : Int_Array;
      Loc : Location;
   end record;

   Default_Node : constant Node_Entry :=
     (Node_Kind'First,
      (others => False),
      (others => 0),
      (others => 0),
      No_Location);

   package Entries is new GNAT.Table
     (Node_Entry, Node_Id, No_Node + 1, 1000, 100);

end Ocarina.ME_AADL_BA.BA_Tree.Nodes;
