pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.ME_AADL_EMA.EMA_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_List_Id,
      K_Definition,
      K_Literal,
      K_EMA_Entity,
      K_Identifier,
      K_EMA_Annex,
      K_Annex_Library,
      K_Error_Type_Library,
      K_Error_Behavior_State_Machine,
      K_Error_Type_Mappings,
      K_Error_Type_Transformations,
      K_Error_Type_Library_Element,
      K_Error_Type_Transformation_Set_Reference,
      K_Error_Behavior_Event,
      K_Error_Behavior_State,
      K_Error_Behavior_Transition,
      K_Error_Type_Mapping,
      K_Use_Error_Types,
      K_Error_Type_Transformation,
      K_Error_Type_Library_Element_Node,
      K_Error_Type_Definition,
      K_Error_Type_Alias,
      K_Error_Type_Set_Definition,
      K_Error_Type_Set_Alias,
      K_Error_Event,
      K_Recover_Event,
      K_Repair_Event,
      K_Error_Type_Set_Or_No_Error,
      K_Target_Error_Type_Instance,
      K_Transition,
      K_Branching_Transition,
      K_Type_Set_Element,
      K_Error_Type_Or_Set_Reference,
      K_Package_Reference,
      K_Error_Model_Library_Reference,
      K_Error_Type_Reference,
      K_Error_Type_Set_Reference,
      K_Initiator_Reference,
      K_Port_Reference,
      K_Self_Event_Reference,
      K_Mode_Transition_Reference,
      K_Event_Initiation,
      K_Error_Source_State,
      K_Operator,
      K_Error_Condition,
      K_Error_Transition_Target,
      K_Error_Transition_Branch,
      K_Branch_Probability,
      K_Fixed_Probability_Value,
      K_Error_Condition_Trigger,
      K_Error_Propagation_Point,
      K_Feature_Reference,
      K_Binding_Reference,
      K_Annex_Subclause,
      K_Emv2_Contained_Property_Association,
      K_Emv2_Containment_Path,
      K_Property_Identifier,
      K_Assignment,
      K_Property_Value,
      K_AADL_Integer,
      K_AADL_Real,
      K_Numeric_Term,
      K_Range_Type,
      K_Error_Type_Mappings_Reference,
      K_Error_Behavior_State_Machine_Reference,
      K_Error_Propagations,
      K_Error_Propagations_Element,
      K_Propagation,
      K_Error_Propagation,
      K_Error_Containment,
      K_Error_Flow,
      K_Fault_Source,
      K_Error_Sink,
      K_Error_Path,
      K_Component_Error_Behavior,
      K_Component_Error_Behavior_Node,
      K_Outgoing_Propagation_Condition,
      K_Propagation_Target,
      K_Error_Detection,
      K_Error_Detection_Effect,
      K_Internal_Event_Reference,
      K_Error_Code_Value,
      K_Property_Constant_Term,
      K_Error_State_To_Mode_Mapping,
      K_Composite_Error_State,
      K_Composite_State_Expression,
      K_Composite_State_Element,
      K_Subcomponent_Error_State,
      K_Connection_Error_Behavior,
      K_Error_Source_Parent,
      K_Error_Source,
      K_Connection_Error_Source,
      K_Propagation_Paths,
      K_Propagation_Point,
      K_Propagation_Path,
      K_Qualified_Propagation_Point);

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
   --  Definition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Definition (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  EMA_Entity
   --
   --    Next_Node                : Node_Id
   --    EMA_Container            : Node_Id
   --

   procedure W_EMA_Entity (N : Node_Id);

   --
   --  Identifier
   --
   --    Next_Node                : Node_Id
   --    EMA_Container            : Node_Id
   --    Name                     : Name_Id
   --    Display_Name             : Name_Id
   --    Corresponding_Entity     : Node_Id
   --    Scope_Entity             : Node_Id
   --    Homonym                  : Node_Id
   --

   procedure W_Identifier (N : Node_Id);

   --
   --  EMA_Annex
   --
   --    Next_Node                : Node_Id
   --

   procedure W_EMA_Annex (N : Node_Id);

   --
   --  Annex_Library
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Library       : Node_Id
   --    Error_Behavior_State_Machine_List: List_Id
   --    Error_Type_Mappings_List : List_Id
   --    Error_Type_Transformations_List: List_Id
   --

   procedure W_Annex_Library (N : Node_Id);

   --
   --  Error_Type_Library
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Library_List_Used: List_Id
   --    Error_Type_Library_List_Extended: List_Id
   --    Error_Type_Library_Element_List: List_Id
   --    Properties               : List_Id
   --

   procedure W_Error_Type_Library (N : Node_Id);

   --
   --  Error_Behavior_State_Machine
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Library_List  : List_Id
   --    Error_Type_Transformation_Set_Reference: Node_Id
   --    Error_Behavior_Event_List: List_Id
   --    Error_Behavior_State_List: List_Id
   --    Error_Behavior_Transition_List: List_Id
   --    Properties               : List_Id
   --

   procedure W_Error_Behavior_State_Machine (N : Node_Id);

   --
   --  Error_Type_Mappings
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Library_List  : List_Id
   --    Error_Type_Mapping_List  : List_Id
   --

   procedure W_Error_Type_Mappings (N : Node_Id);

   --
   --  Error_Type_Transformations
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Use_Error_Types          : Node_Id
   --    Error_Type_Transformation_List: List_Id
   --

   procedure W_Error_Type_Transformations (N : Node_Id);

   --
   --  Error_Type_Library_Element
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Definition    : Node_Id
   --    Error_Type_Alias         : Node_Id
   --    Error_Type_Set_Definition: Node_Id
   --    Error_Type_Set_Alias     : Node_Id
   --

   procedure W_Error_Type_Library_Element (N : Node_Id);

   --
   --  Error_Type_Transformation_Set_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Package_Reference        : Node_Id
   --

   procedure W_Error_Type_Transformation_Set_Reference (N : Node_Id);

   --
   --  Error_Behavior_Event
   --
   --    Next_Node                : Node_Id
   --    Error_Event              : Node_Id
   --    Recover_Event            : Node_Id
   --    Repair_Event             : Node_Id
   --

   procedure W_Error_Behavior_Event (N : Node_Id);

   --
   --  Error_Behavior_State
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Set           : List_Id
   --

   procedure W_Error_Behavior_State (N : Node_Id);

   --
   --  Error_Behavior_Transition
   --
   --    Next_Node                : Node_Id
   --    Transition               : Node_Id
   --    Branching_Transition     : Node_Id
   --

   procedure W_Error_Behavior_Transition (N : Node_Id);

   --
   --  Error_Type_Mapping
   --
   --    Next_Node                : Node_Id
   --    Source_Error_Type_Set    : List_Id
   --    Target_Error_Type_Instance: Node_Id
   --

   procedure W_Error_Type_Mapping (N : Node_Id);

   --
   --  Use_Error_Types
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Library_List  : List_Id
   --

   procedure W_Use_Error_Types (N : Node_Id);

   --
   --  Error_Type_Transformation
   --
   --    Next_Node                : Node_Id
   --    Source_Error_Type_Set_Or_Noerror: Node_Id
   --    Target_Error_Type_Instance: Node_Id
   --    Contributor_Error_Type_Set_Or_Noerror: Node_Id
   --

   procedure W_Error_Type_Transformation (N : Node_Id);

   --
   --  Error_Type_Library_Element_Node
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Reference     : Node_Id
   --

   procedure W_Error_Type_Library_Element_Node (N : Node_Id);

   --
   --  Error_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Reference     : Node_Id
   --

   procedure W_Error_Type_Definition (N : Node_Id);

   --
   --  Error_Type_Alias
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Reference     : Node_Id
   --

   procedure W_Error_Type_Alias (N : Node_Id);

   --
   --  Error_Type_Set_Definition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Set           : List_Id
   --

   procedure W_Error_Type_Set_Definition (N : Node_Id);

   --
   --  Error_Type_Set_Alias
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Set_Reference : Node_Id
   --

   procedure W_Error_Type_Set_Alias (N : Node_Id);

   --
   --  Error_Event
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Set           : List_Id
   --    Error_Event_Condition    : Node_Id
   --

   procedure W_Error_Event (N : Node_Id);

   --
   --  Recover_Event
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Event_Condition    : Node_Id
   --    Recover_Event_Initiators : List_Id
   --

   procedure W_Recover_Event (N : Node_Id);

   --
   --  Repair_Event
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Repair_Event_Initiation  : Node_Id
   --

   procedure W_Repair_Event (N : Node_Id);

   --
   --  Error_Type_Set_Or_No_Error
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Set           : List_Id
   --

   procedure W_Error_Type_Set_Or_No_Error (N : Node_Id);

   --
   --  Target_Error_Type_Instance
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Reference     : Node_Id
   --    Error_Type_Product       : List_Id
   --

   procedure W_Target_Error_Type_Instance (N : Node_Id);

   --
   --  Transition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Source_State       : Node_Id
   --    Error_Condition          : Node_Id
   --    Error_Transition_Target  : Node_Id
   --    Error_Transition_Branch  : Node_Id
   --

   procedure W_Transition (N : Node_Id);

   --
   --  Branching_Transition
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Branching_Transition (N : Node_Id);

   --
   --  Type_Set_Element
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Or_Set_Reference: Node_Id
   --    Error_Type_Product       : List_Id
   --

   procedure W_Type_Set_Element (N : Node_Id);

   --
   --  Error_Type_Or_Set_Reference
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Set_Reference : Node_Id
   --    Error_Type_Reference     : Node_Id
   --

   procedure W_Error_Type_Or_Set_Reference (N : Node_Id);

   --
   --  Package_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifiers              : List_Id
   --    AADL_Package_Reference   : Node_Id
   --

   procedure W_Package_Reference (N : Node_Id);

   --
   --  Error_Model_Library_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifiers              : List_Id
   --    AADL_Package_Reference   : Node_Id
   --

   procedure W_Error_Model_Library_Reference (N : Node_Id);

   --
   --  Error_Type_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Model_Library_Reference: Node_Id
   --

   procedure W_Error_Type_Reference (N : Node_Id);

   --
   --  Error_Type_Set_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Model_Library_Reference: Node_Id
   --

   procedure W_Error_Type_Set_Reference (N : Node_Id);

   --
   --  Initiator_Reference
   --
   --    Next_Node                : Node_Id
   --    Mode_Transition_Reference: Node_Id
   --    Port_Reference           : Node_Id
   --    Self_Event_Reference     : Node_Id
   --

   procedure W_Initiator_Reference (N : Node_Id);

   --
   --  Port_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Featuregroup_Identifier_List: List_Id
   --

   procedure W_Port_Reference (N : Node_Id);

   --
   --  Self_Event_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Self_Event_Reference (N : Node_Id);

   --
   --  Mode_Transition_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Package_Reference        : Node_Id
   --

   procedure W_Mode_Transition_Reference (N : Node_Id);

   --
   --  Event_Initiation
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Event_Initiation (N : Node_Id);

   --
   --  Error_Source_State
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Source_Error_Type_Set    : List_Id
   --

   procedure W_Error_Source_State (N : Node_Id);

   --
   --  Operator
   --
   --    Next_Node                : Node_Id
   --    Operator_Kind            : Byte
   --

   procedure W_Operator (N : Node_Id);

   --
   --  Error_Condition
   --
   --    Next_Node                : Node_Id
   --    Error_Condition_Trigger  : Node_Id
   --    Error_Condition_Node     : Node_Id
   --    Numeric_Literal          : Node_Id
   --    Operator                 : Node_Id
   --    Error_Condition_Trigger_List: List_Id
   --

   procedure W_Error_Condition (N : Node_Id);

   --
   --  Error_Transition_Target
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Target_Error_Type_Instance: Node_Id
   --

   procedure W_Error_Transition_Target (N : Node_Id);

   --
   --  Error_Transition_Branch
   --
   --    Next_Node                : Node_Id
   --    Error_Transition_Target_List: List_Id
   --    Branch_Probability_List  : List_Id
   --

   procedure W_Error_Transition_Branch (N : Node_Id);

   --
   --  Branch_Probability
   --
   --    Next_Node                : Node_Id
   --    Fixed_Probability_Value  : Node_Id
   --

   procedure W_Branch_Probability (N : Node_Id);

   --
   --  Fixed_Probability_Value
   --
   --    Next_Node                : Node_Id
   --    Property_Set_Identifier  : Node_Id
   --    Real_Property_Identifier : Node_Id
   --

   procedure W_Fixed_Probability_Value (N : Node_Id);

   --
   --  Error_Condition_Trigger
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Type_Set           : List_Id
   --    Incoming_Error_Propagation_Point: Node_Id
   --    Error_Type_Set_Or_Noerror: Node_Id
   --    Outgoing_Error_Propagation_Point: Node_Id
   --

   procedure W_Error_Condition_Trigger (N : Node_Id);

   --
   --  Error_Propagation_Point
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Feature_Reference        : Node_Id
   --    Binding_Reference        : Node_Id
   --

   procedure W_Error_Propagation_Point (N : Node_Id);

   --
   --  Feature_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Featuregroup_Identifier_List: List_Id
   --

   procedure W_Feature_Reference (N : Node_Id);

   --
   --  Binding_Reference
   --
   --    Next_Node                : Node_Id
   --    Binding_Kind             : Byte
   --

   procedure W_Binding_Reference (N : Node_Id);

   --
   --  Annex_Subclause
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Library_List  : List_Id
   --    Error_Type_Mappings_Reference_Equivalence: Node_Id
   --    Error_Type_Mappings_Reference_Mappings: Node_Id
   --    Error_Behavior_State_Machine_Reference: Node_Id
   --    Error_Propagations       : Node_Id
   --    Component_Error_Behavior : Node_Id
   --    Composite_Error_Behavior : List_Id
   --    Connection_Error_Behavior: Node_Id
   --    Propagation_Paths        : Node_Id
   --    EMV2_Properties_Section  : List_Id
   --    Next_Entity              : Node_Id
   --

   procedure W_Annex_Subclause (N : Node_Id);

   --
   --  Emv2_Contained_Property_Association
   --
   --    Next_Node                : Node_Id
   --    Emv2_Containment_Path_List: List_Id
   --    Assignment               : Node_Id
   --    Is_Constant              : Boolean
   --    Property_Identifier      : Node_Id
   --

   procedure W_Emv2_Contained_Property_Association (N : Node_Id);

   --
   --  Emv2_Containment_Path
   --
   --    Next_Node                : Node_Id
   --    Aadl2_Core_Path_List     : List_Id
   --    Emv2_Annex_Specific_Path_List: List_Id
   --

   procedure W_Emv2_Containment_Path (N : Node_Id);

   --
   --  Property_Identifier
   --
   --    Next_Node                : Node_Id
   --    Property_Set             : Node_Id
   --    Property_Name            : Node_Id
   --

   procedure W_Property_Identifier (N : Node_Id);

   --
   --  Assignment
   --
   --    Next_Node                : Node_Id
   --    Prop_Value               : Node_Id
   --    Prop_Value_List          : List_Id
   --    Prop_Name_List           : List_Id
   --    In_Modes_Properties      : List_Id
   --

   procedure W_Assignment (N : Node_Id);

   --
   --  Property_Value
   --
   --    Next_Node                : Node_Id
   --    List_Of_Strings          : List_Id
   --

   procedure W_Property_Value (N : Node_Id);

   --
   --  AADL_Integer
   --
   --    Next_Node                : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_AADL_Integer (N : Node_Id);

   --
   --  AADL_Real
   --
   --    Next_Node                : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_AADL_Real (N : Node_Id);

   --
   --  Numeric_Term
   --
   --    Next_Node                : Node_Id
   --    Number_Value             : Node_Id
   --    Unit_Identifier          : Node_Id
   --

   procedure W_Numeric_Term (N : Node_Id);

   --
   --  Range_Type
   --
   --    Next_Node                : Node_Id
   --    Lower_Bound              : Node_Id
   --    Upper_Bound              : Node_Id
   --

   procedure W_Range_Type (N : Node_Id);

   --
   --  Error_Type_Mappings_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Package_Reference        : Node_Id
   --

   procedure W_Error_Type_Mappings_Reference (N : Node_Id);

   --
   --  Error_Behavior_State_Machine_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Model_Library_Reference: Node_Id
   --

   procedure W_Error_Behavior_State_Machine_Reference (N : Node_Id);

   --
   --  Error_Propagations
   --
   --    Next_Node                : Node_Id
   --    Error_Propagation_List   : List_Id
   --    Error_Containment_List   : List_Id
   --    Error_Flow_List          : List_Id
   --

   procedure W_Error_Propagations (N : Node_Id);

   --
   --  Error_Propagations_Element
   --
   --    Next_Node                : Node_Id
   --    Error_Propagation_Point  : Node_Id
   --    Propagation              : Node_Id
   --    Error_Type_Set           : List_Id
   --

   procedure W_Error_Propagations_Element (N : Node_Id);

   --
   --  Propagation
   --
   --    Next_Node                : Node_Id
   --    Propagation_Kind         : Byte
   --

   procedure W_Propagation (N : Node_Id);

   --
   --  Error_Propagation
   --
   --    Next_Node                : Node_Id
   --    Error_Propagation_Point  : Node_Id
   --    Propagation              : Node_Id
   --    Error_Type_Set           : List_Id
   --

   procedure W_Error_Propagation (N : Node_Id);

   --
   --  Error_Containment
   --
   --    Next_Node                : Node_Id
   --    Error_Propagation_Point  : Node_Id
   --    Propagation              : Node_Id
   --    Error_Type_Set           : List_Id
   --

   procedure W_Error_Containment (N : Node_Id);

   --
   --  Error_Flow
   --
   --    Next_Node                : Node_Id
   --    Error_Source             : Node_Id
   --    Error_Sink               : Node_Id
   --    Error_Path               : Node_Id
   --

   procedure W_Error_Flow (N : Node_Id);

   --
   --  Fault_Source
   --
   --    Next_Node                : Node_Id
   --    Error_Behavior_State     : Node_Id
   --    Error_Type_Set           : List_Id
   --    Failure_Mode_Description : Node_Id
   --

   procedure W_Fault_Source (N : Node_Id);

   --
   --  Error_Sink
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Incoming_Error_Propagation_Point: Node_Id
   --    Error_Type_Set           : List_Id
   --

   procedure W_Error_Sink (N : Node_Id);

   --
   --  Error_Path
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Incoming_Error_Propagation_Point: Node_Id
   --    Error_Type_Set           : List_Id
   --    Outgoing_Error_Propagation_Point: Node_Id
   --    Target_Error_Type_Instance: Node_Id
   --

   procedure W_Error_Path (N : Node_Id);

   --
   --  Component_Error_Behavior
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Transformation_Set_Reference: Node_Id
   --    Error_Behavior_Event_List: List_Id
   --    Component_Specific_Error_Behavior_Transition_List: List_Id
   --    Outgoing_Propagation_Condition_List: List_Id
   --    Error_Detection_List     : List_Id
   --    Error_State_To_Mode_Mapping_List: List_Id
   --

   procedure W_Component_Error_Behavior (N : Node_Id);

   --
   --  Component_Error_Behavior_Node
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Source_State       : Node_Id
   --    Error_Condition          : Node_Id
   --

   procedure W_Component_Error_Behavior_Node (N : Node_Id);

   --
   --  Outgoing_Propagation_Condition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Source_State       : Node_Id
   --    Error_Condition          : Node_Id
   --    Propagation_Target       : Node_Id
   --

   procedure W_Outgoing_Propagation_Condition (N : Node_Id);

   --
   --  Propagation_Target
   --
   --    Next_Node                : Node_Id
   --    Error_Propagation_Point  : Node_Id
   --    Propagated_Target_Error_Type_Instance: Node_Id
   --

   procedure W_Propagation_Target (N : Node_Id);

   --
   --  Error_Detection
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Error_Source_State       : Node_Id
   --    Error_Condition          : Node_Id
   --    Error_Detection_Effect   : Node_Id
   --

   procedure W_Error_Detection (N : Node_Id);

   --
   --  Error_Detection_Effect
   --
   --    Next_Node                : Node_Id
   --    Outgoing_Port_Reference  : Node_Id
   --    Internal_Event_Reference : Node_Id
   --    Error_Code_Value         : Node_Id
   --

   procedure W_Error_Detection_Effect (N : Node_Id);

   --
   --  Internal_Event_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Internal_Event_Reference (N : Node_Id);

   --
   --  Error_Code_Value
   --
   --    Next_Node                : Node_Id
   --    Enumeration_Identifier   : Node_Id
   --    Property_Constant_Term   : Node_Id
   --

   procedure W_Error_Code_Value (N : Node_Id);

   --
   --  Property_Constant_Term
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set             : Node_Id
   --

   procedure W_Property_Constant_Term (N : Node_Id);

   --
   --  Error_State_To_Mode_Mapping
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Target_Error_Type_Instance: Node_Id
   --    Mode_Name_List           : List_Id
   --

   procedure W_Error_State_To_Mode_Mapping (N : Node_Id);

   --
   --  Composite_Error_State
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Composite_State_Identifier: Node_Id
   --    Subcomponent_State_Expression: Node_Id
   --    Target_Error_Type_Instance: Node_Id
   --

   procedure W_Composite_Error_State (N : Node_Id);

   --
   --  Composite_State_Expression
   --
   --    Next_Node                : Node_Id
   --    Composite_State_Element  : Node_Id
   --    Composite_State_Expression_Node: Node_Id
   --    Numeric_Literal          : Node_Id
   --    Operator                 : Node_Id
   --    Composite_State_Element_List: List_Id
   --

   procedure W_Composite_State_Expression (N : Node_Id);

   --
   --  Composite_State_Element
   --
   --    Next_Node                : Node_Id
   --    Subcomponent_Error_State : Node_Id
   --    Error_Type_Set           : List_Id
   --    Incoming_Error_Propagation_Point: Node_Id
   --    Error_Type_Set_Or_Noerror: Node_Id
   --

   procedure W_Composite_State_Element (N : Node_Id);

   --
   --  Subcomponent_Error_State
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Subcomponent_Identifier_List: List_Id
   --

   procedure W_Subcomponent_Error_State (N : Node_Id);

   --
   --  Connection_Error_Behavior
   --
   --    Next_Node                : Node_Id
   --    Error_Type_Transformation_Set_Reference: Node_Id
   --    Connection_Error_Source_List: List_Id
   --

   procedure W_Connection_Error_Behavior (N : Node_Id);

   --
   --  Error_Source_Parent
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Effect_Error_Type_Set    : List_Id
   --    Fault_Source             : Node_Id
   --    Fault_Condition          : Node_Id
   --

   procedure W_Error_Source_Parent (N : Node_Id);

   --
   --  Error_Source
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Effect_Error_Type_Set    : List_Id
   --    Fault_Source             : Node_Id
   --    Fault_Condition          : Node_Id
   --    Outgoing_Error_Propagation_Point: Node_Id
   --

   procedure W_Error_Source (N : Node_Id);

   --
   --  Connection_Error_Source
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Connection_Identifier    : Node_Id
   --    Effect_Error_Type_Set    : List_Id
   --    Fault_Source_Error_Type_Set: List_Id
   --    Fault_Condition          : Node_Id
   --    Failure_Mode_Description : Node_Id
   --

   procedure W_Connection_Error_Source (N : Node_Id);

   --
   --  Propagation_Paths
   --
   --    Next_Node                : Node_Id
   --    Propagation_Point_List   : List_Id
   --    Propagation_Path_List    : List_Id
   --

   procedure W_Propagation_Paths (N : Node_Id);

   --
   --  Propagation_Point
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Propagation_Point (N : Node_Id);

   --
   --  Propagation_Path
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Source_Qualified_Propagation_Point: Node_Id
   --    Target_Qualified_Propagation_Point: Node_Id
   --

   procedure W_Propagation_Path (N : Node_Id);

   --
   --  Qualified_Propagation_Point
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Subcomponent_Identifier_List: List_Id
   --

   procedure W_Qualified_Propagation_Point (N : Node_Id);

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

   function Identifier (N : Node_Id) return Node_Id;
   procedure Set_Identifier (N : Node_Id; V : Node_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function EMA_Container (N : Node_Id) return Node_Id;
   procedure Set_EMA_Container (N : Node_Id; V : Node_Id);

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

   function Error_Type_Library (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Library (N : Node_Id; V : Node_Id);

   function Error_Behavior_State_Machine_List (N : Node_Id) return List_Id;
   procedure Set_Error_Behavior_State_Machine_List (N : Node_Id; V : List_Id);

   function Error_Type_Mappings_List (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Mappings_List (N : Node_Id; V : List_Id);

   function Error_Type_Transformations_List (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Transformations_List (N : Node_Id; V : List_Id);

   function Error_Type_Library_List_Used (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Library_List_Used (N : Node_Id; V : List_Id);

   function Error_Type_Library_List_Extended (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Library_List_Extended (N : Node_Id; V : List_Id);

   function Error_Type_Library_Element_List (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Library_Element_List (N : Node_Id; V : List_Id);

   function Properties (N : Node_Id) return List_Id;
   procedure Set_Properties (N : Node_Id; V : List_Id);

   function Error_Type_Library_List (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Library_List (N : Node_Id; V : List_Id);

   function Error_Type_Transformation_Set_Reference (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Transformation_Set_Reference (N : Node_Id; V : Node_Id);

   function Error_Behavior_Event_List (N : Node_Id) return List_Id;
   procedure Set_Error_Behavior_Event_List (N : Node_Id; V : List_Id);

   function Error_Behavior_State_List (N : Node_Id) return List_Id;
   procedure Set_Error_Behavior_State_List (N : Node_Id; V : List_Id);

   function Error_Behavior_Transition_List (N : Node_Id) return List_Id;
   procedure Set_Error_Behavior_Transition_List (N : Node_Id; V : List_Id);

   function Error_Type_Mapping_List (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Mapping_List (N : Node_Id; V : List_Id);

   function Use_Error_Types (N : Node_Id) return Node_Id;
   procedure Set_Use_Error_Types (N : Node_Id; V : Node_Id);

   function Error_Type_Transformation_List (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Transformation_List (N : Node_Id; V : List_Id);

   function Error_Type_Definition (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Definition (N : Node_Id; V : Node_Id);

   function Error_Type_Alias (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Alias (N : Node_Id; V : Node_Id);

   function Error_Type_Set_Definition (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Set_Definition (N : Node_Id; V : Node_Id);

   function Error_Type_Set_Alias (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Set_Alias (N : Node_Id; V : Node_Id);

   function Package_Reference (N : Node_Id) return Node_Id;
   procedure Set_Package_Reference (N : Node_Id; V : Node_Id);

   function Error_Event (N : Node_Id) return Node_Id;
   procedure Set_Error_Event (N : Node_Id; V : Node_Id);

   function Recover_Event (N : Node_Id) return Node_Id;
   procedure Set_Recover_Event (N : Node_Id; V : Node_Id);

   function Repair_Event (N : Node_Id) return Node_Id;
   procedure Set_Repair_Event (N : Node_Id; V : Node_Id);

   function Error_Type_Set (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Set (N : Node_Id; V : List_Id);

   function Transition (N : Node_Id) return Node_Id;
   procedure Set_Transition (N : Node_Id; V : Node_Id);

   function Branching_Transition (N : Node_Id) return Node_Id;
   procedure Set_Branching_Transition (N : Node_Id; V : Node_Id);

   function Source_Error_Type_Set (N : Node_Id) return List_Id;
   procedure Set_Source_Error_Type_Set (N : Node_Id; V : List_Id);

   function Target_Error_Type_Instance (N : Node_Id) return Node_Id;
   procedure Set_Target_Error_Type_Instance (N : Node_Id; V : Node_Id);

   function Source_Error_Type_Set_Or_Noerror (N : Node_Id) return Node_Id;
   procedure Set_Source_Error_Type_Set_Or_Noerror (N : Node_Id; V : Node_Id);

   function Contributor_Error_Type_Set_Or_Noerror (N : Node_Id) return Node_Id;
   procedure Set_Contributor_Error_Type_Set_Or_Noerror (N : Node_Id; V : Node_Id);

   function Error_Type_Reference (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Reference (N : Node_Id; V : Node_Id);

   function Error_Type_Set_Reference (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Set_Reference (N : Node_Id; V : Node_Id);

   function Error_Event_Condition (N : Node_Id) return Node_Id;
   procedure Set_Error_Event_Condition (N : Node_Id; V : Node_Id);

   function Recover_Event_Initiators (N : Node_Id) return List_Id;
   procedure Set_Recover_Event_Initiators (N : Node_Id; V : List_Id);

   function Repair_Event_Initiation (N : Node_Id) return Node_Id;
   procedure Set_Repair_Event_Initiation (N : Node_Id; V : Node_Id);

   function Error_Type_Product (N : Node_Id) return List_Id;
   procedure Set_Error_Type_Product (N : Node_Id; V : List_Id);

   function Error_Source_State (N : Node_Id) return Node_Id;
   procedure Set_Error_Source_State (N : Node_Id; V : Node_Id);

   function Error_Condition (N : Node_Id) return Node_Id;
   procedure Set_Error_Condition (N : Node_Id; V : Node_Id);

   function Error_Transition_Target (N : Node_Id) return Node_Id;
   procedure Set_Error_Transition_Target (N : Node_Id; V : Node_Id);

   function Error_Transition_Branch (N : Node_Id) return Node_Id;
   procedure Set_Error_Transition_Branch (N : Node_Id; V : Node_Id);

   function Error_Type_Or_Set_Reference (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Or_Set_Reference (N : Node_Id; V : Node_Id);

   function Identifiers (N : Node_Id) return List_Id;
   procedure Set_Identifiers (N : Node_Id; V : List_Id);

   function AADL_Package_Reference (N : Node_Id) return Node_Id;
   procedure Set_AADL_Package_Reference (N : Node_Id; V : Node_Id);

   function Error_Model_Library_Reference (N : Node_Id) return Node_Id;
   procedure Set_Error_Model_Library_Reference (N : Node_Id; V : Node_Id);

   function Mode_Transition_Reference (N : Node_Id) return Node_Id;
   procedure Set_Mode_Transition_Reference (N : Node_Id; V : Node_Id);

   function Port_Reference (N : Node_Id) return Node_Id;
   procedure Set_Port_Reference (N : Node_Id; V : Node_Id);

   function Self_Event_Reference (N : Node_Id) return Node_Id;
   procedure Set_Self_Event_Reference (N : Node_Id; V : Node_Id);

   function Featuregroup_Identifier_List (N : Node_Id) return List_Id;
   procedure Set_Featuregroup_Identifier_List (N : Node_Id; V : List_Id);

   function Operator_Kind (N : Node_Id) return Byte;
   procedure Set_Operator_Kind (N : Node_Id; V : Byte);

   function Error_Condition_Trigger (N : Node_Id) return Node_Id;
   procedure Set_Error_Condition_Trigger (N : Node_Id; V : Node_Id);

   function Error_Condition_Node (N : Node_Id) return Node_Id;
   procedure Set_Error_Condition_Node (N : Node_Id; V : Node_Id);

   function Numeric_Literal (N : Node_Id) return Node_Id;
   procedure Set_Numeric_Literal (N : Node_Id; V : Node_Id);

   function Operator (N : Node_Id) return Node_Id;
   procedure Set_Operator (N : Node_Id; V : Node_Id);

   function Error_Condition_Trigger_List (N : Node_Id) return List_Id;
   procedure Set_Error_Condition_Trigger_List (N : Node_Id; V : List_Id);

   function Error_Transition_Target_List (N : Node_Id) return List_Id;
   procedure Set_Error_Transition_Target_List (N : Node_Id; V : List_Id);

   function Branch_Probability_List (N : Node_Id) return List_Id;
   procedure Set_Branch_Probability_List (N : Node_Id; V : List_Id);

   function Fixed_Probability_Value (N : Node_Id) return Node_Id;
   procedure Set_Fixed_Probability_Value (N : Node_Id; V : Node_Id);

   function Property_Set_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Property_Set_Identifier (N : Node_Id; V : Node_Id);

   function Real_Property_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Real_Property_Identifier (N : Node_Id; V : Node_Id);

   function Incoming_Error_Propagation_Point (N : Node_Id) return Node_Id;
   procedure Set_Incoming_Error_Propagation_Point (N : Node_Id; V : Node_Id);

   function Error_Type_Set_Or_Noerror (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Set_Or_Noerror (N : Node_Id; V : Node_Id);

   function Outgoing_Error_Propagation_Point (N : Node_Id) return Node_Id;
   procedure Set_Outgoing_Error_Propagation_Point (N : Node_Id; V : Node_Id);

   function Feature_Reference (N : Node_Id) return Node_Id;
   procedure Set_Feature_Reference (N : Node_Id; V : Node_Id);

   function Binding_Reference (N : Node_Id) return Node_Id;
   procedure Set_Binding_Reference (N : Node_Id; V : Node_Id);

   function Binding_Kind (N : Node_Id) return Byte;
   procedure Set_Binding_Kind (N : Node_Id; V : Byte);

   function Error_Type_Mappings_Reference_Equivalence (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Mappings_Reference_Equivalence (N : Node_Id; V : Node_Id);

   function Error_Type_Mappings_Reference_Mappings (N : Node_Id) return Node_Id;
   procedure Set_Error_Type_Mappings_Reference_Mappings (N : Node_Id; V : Node_Id);

   function Error_Behavior_State_Machine_Reference (N : Node_Id) return Node_Id;
   procedure Set_Error_Behavior_State_Machine_Reference (N : Node_Id; V : Node_Id);

   function Error_Propagations (N : Node_Id) return Node_Id;
   procedure Set_Error_Propagations (N : Node_Id; V : Node_Id);

   function Component_Error_Behavior (N : Node_Id) return Node_Id;
   procedure Set_Component_Error_Behavior (N : Node_Id; V : Node_Id);

   function Composite_Error_Behavior (N : Node_Id) return List_Id;
   procedure Set_Composite_Error_Behavior (N : Node_Id; V : List_Id);

   function Connection_Error_Behavior (N : Node_Id) return Node_Id;
   procedure Set_Connection_Error_Behavior (N : Node_Id; V : Node_Id);

   function Propagation_Paths (N : Node_Id) return Node_Id;
   procedure Set_Propagation_Paths (N : Node_Id; V : Node_Id);

   function EMV2_Properties_Section (N : Node_Id) return List_Id;
   procedure Set_EMV2_Properties_Section (N : Node_Id; V : List_Id);

   function Next_Entity (N : Node_Id) return Node_Id;
   procedure Set_Next_Entity (N : Node_Id; V : Node_Id);

   function Emv2_Containment_Path_List (N : Node_Id) return List_Id;
   procedure Set_Emv2_Containment_Path_List (N : Node_Id; V : List_Id);

   function Assignment (N : Node_Id) return Node_Id;
   procedure Set_Assignment (N : Node_Id; V : Node_Id);

   function Is_Constant (N : Node_Id) return Boolean;
   procedure Set_Is_Constant (N : Node_Id; V : Boolean);

   function Property_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Property_Identifier (N : Node_Id; V : Node_Id);

   function Aadl2_Core_Path_List (N : Node_Id) return List_Id;
   procedure Set_Aadl2_Core_Path_List (N : Node_Id; V : List_Id);

   function Emv2_Annex_Specific_Path_List (N : Node_Id) return List_Id;
   procedure Set_Emv2_Annex_Specific_Path_List (N : Node_Id; V : List_Id);

   function Property_Set (N : Node_Id) return Node_Id;
   procedure Set_Property_Set (N : Node_Id; V : Node_Id);

   function Property_Name (N : Node_Id) return Node_Id;
   procedure Set_Property_Name (N : Node_Id; V : Node_Id);

   function Prop_Value (N : Node_Id) return Node_Id;
   procedure Set_Prop_Value (N : Node_Id; V : Node_Id);

   function Prop_Value_List (N : Node_Id) return List_Id;
   procedure Set_Prop_Value_List (N : Node_Id; V : List_Id);

   function Prop_Name_List (N : Node_Id) return List_Id;
   procedure Set_Prop_Name_List (N : Node_Id; V : List_Id);

   function In_Modes_Properties (N : Node_Id) return List_Id;
   procedure Set_In_Modes_Properties (N : Node_Id; V : List_Id);

   function List_Of_Strings (N : Node_Id) return List_Id;
   procedure Set_List_Of_Strings (N : Node_Id; V : List_Id);

   function Number_Value (N : Node_Id) return Node_Id;
   procedure Set_Number_Value (N : Node_Id; V : Node_Id);

   function Unit_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Unit_Identifier (N : Node_Id; V : Node_Id);

   function Lower_Bound (N : Node_Id) return Node_Id;
   procedure Set_Lower_Bound (N : Node_Id; V : Node_Id);

   function Upper_Bound (N : Node_Id) return Node_Id;
   procedure Set_Upper_Bound (N : Node_Id; V : Node_Id);

   function Error_Propagation_List (N : Node_Id) return List_Id;
   procedure Set_Error_Propagation_List (N : Node_Id; V : List_Id);

   function Error_Containment_List (N : Node_Id) return List_Id;
   procedure Set_Error_Containment_List (N : Node_Id; V : List_Id);

   function Error_Flow_List (N : Node_Id) return List_Id;
   procedure Set_Error_Flow_List (N : Node_Id; V : List_Id);

   function Error_Propagation_Point (N : Node_Id) return Node_Id;
   procedure Set_Error_Propagation_Point (N : Node_Id; V : Node_Id);

   function Propagation (N : Node_Id) return Node_Id;
   procedure Set_Propagation (N : Node_Id; V : Node_Id);

   function Propagation_Kind (N : Node_Id) return Byte;
   procedure Set_Propagation_Kind (N : Node_Id; V : Byte);

   function Error_Source (N : Node_Id) return Node_Id;
   procedure Set_Error_Source (N : Node_Id; V : Node_Id);

   function Error_Sink (N : Node_Id) return Node_Id;
   procedure Set_Error_Sink (N : Node_Id; V : Node_Id);

   function Error_Path (N : Node_Id) return Node_Id;
   procedure Set_Error_Path (N : Node_Id; V : Node_Id);

   function Error_Behavior_State (N : Node_Id) return Node_Id;
   procedure Set_Error_Behavior_State (N : Node_Id; V : Node_Id);

   function Failure_Mode_Description (N : Node_Id) return Node_Id;
   procedure Set_Failure_Mode_Description (N : Node_Id; V : Node_Id);

   function Component_Specific_Error_Behavior_Transition_List (N : Node_Id) return List_Id;
   procedure Set_Component_Specific_Error_Behavior_Transition_List (N : Node_Id; V : List_Id);

   function Outgoing_Propagation_Condition_List (N : Node_Id) return List_Id;
   procedure Set_Outgoing_Propagation_Condition_List (N : Node_Id; V : List_Id);

   function Error_Detection_List (N : Node_Id) return List_Id;
   procedure Set_Error_Detection_List (N : Node_Id; V : List_Id);

   function Error_State_To_Mode_Mapping_List (N : Node_Id) return List_Id;
   procedure Set_Error_State_To_Mode_Mapping_List (N : Node_Id; V : List_Id);

   function Propagation_Target (N : Node_Id) return Node_Id;
   procedure Set_Propagation_Target (N : Node_Id; V : Node_Id);

   function Propagated_Target_Error_Type_Instance (N : Node_Id) return Node_Id;
   procedure Set_Propagated_Target_Error_Type_Instance (N : Node_Id; V : Node_Id);

   function Error_Detection_Effect (N : Node_Id) return Node_Id;
   procedure Set_Error_Detection_Effect (N : Node_Id; V : Node_Id);

   function Outgoing_Port_Reference (N : Node_Id) return Node_Id;
   procedure Set_Outgoing_Port_Reference (N : Node_Id; V : Node_Id);

   function Internal_Event_Reference (N : Node_Id) return Node_Id;
   procedure Set_Internal_Event_Reference (N : Node_Id; V : Node_Id);

   function Error_Code_Value (N : Node_Id) return Node_Id;
   procedure Set_Error_Code_Value (N : Node_Id; V : Node_Id);

   function Enumeration_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Enumeration_Identifier (N : Node_Id; V : Node_Id);

   function Property_Constant_Term (N : Node_Id) return Node_Id;
   procedure Set_Property_Constant_Term (N : Node_Id; V : Node_Id);

   function Mode_Name_List (N : Node_Id) return List_Id;
   procedure Set_Mode_Name_List (N : Node_Id; V : List_Id);

   function Composite_State_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Composite_State_Identifier (N : Node_Id; V : Node_Id);

   function Subcomponent_State_Expression (N : Node_Id) return Node_Id;
   procedure Set_Subcomponent_State_Expression (N : Node_Id; V : Node_Id);

   function Composite_State_Element (N : Node_Id) return Node_Id;
   procedure Set_Composite_State_Element (N : Node_Id; V : Node_Id);

   function Composite_State_Expression_Node (N : Node_Id) return Node_Id;
   procedure Set_Composite_State_Expression_Node (N : Node_Id; V : Node_Id);

   function Composite_State_Element_List (N : Node_Id) return List_Id;
   procedure Set_Composite_State_Element_List (N : Node_Id; V : List_Id);

   function Subcomponent_Error_State (N : Node_Id) return Node_Id;
   procedure Set_Subcomponent_Error_State (N : Node_Id; V : Node_Id);

   function Subcomponent_Identifier_List (N : Node_Id) return List_Id;
   procedure Set_Subcomponent_Identifier_List (N : Node_Id; V : List_Id);

   function Connection_Error_Source_List (N : Node_Id) return List_Id;
   procedure Set_Connection_Error_Source_List (N : Node_Id; V : List_Id);

   function Effect_Error_Type_Set (N : Node_Id) return List_Id;
   procedure Set_Effect_Error_Type_Set (N : Node_Id; V : List_Id);

   function Fault_Source (N : Node_Id) return Node_Id;
   procedure Set_Fault_Source (N : Node_Id; V : Node_Id);

   function Fault_Condition (N : Node_Id) return Node_Id;
   procedure Set_Fault_Condition (N : Node_Id; V : Node_Id);

   function Connection_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Connection_Identifier (N : Node_Id; V : Node_Id);

   function Fault_Source_Error_Type_Set (N : Node_Id) return List_Id;
   procedure Set_Fault_Source_Error_Type_Set (N : Node_Id; V : List_Id);

   function Propagation_Point_List (N : Node_Id) return List_Id;
   procedure Set_Propagation_Point_List (N : Node_Id; V : List_Id);

   function Propagation_Path_List (N : Node_Id) return List_Id;
   procedure Set_Propagation_Path_List (N : Node_Id; V : List_Id);

   function Source_Qualified_Propagation_Point (N : Node_Id) return Node_Id;
   procedure Set_Source_Qualified_Propagation_Point (N : Node_Id; V : Node_Id);

   function Target_Qualified_Propagation_Point (N : Node_Id) return Node_Id;
   procedure Set_Target_Qualified_Propagation_Point (N : Node_Id; V : Node_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 1) of Boolean;
   type Byte_Array is array (1 .. 1) of Byte;
   type Int_Array is array (1 .. 12) of Int;

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

end Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
