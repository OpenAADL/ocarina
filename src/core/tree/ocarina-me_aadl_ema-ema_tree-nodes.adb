pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.ME_AADL_EMA.EMA_Tree.Debug; use Ocarina.ME_AADL_EMA.EMA_Tree.Debug;

package body Ocarina.ME_AADL_EMA.EMA_Tree.Nodes is

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
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_EMA_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_EMA_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Use_Error_Types
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element_Node
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event
        or else Table (Types.Node_Id (N)).Kind = K_Repair_Event
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Or_No_Error
        or else Table (Types.Node_Id (N)).Kind = K_Target_Error_Type_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Branching_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Type_Set_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Or_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Package_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Model_Library_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Port_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Self_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Event_Initiation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_State
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Branch
        or else Table (Types.Node_Id (N)).Kind = K_Branch_Probability
        or else Table (Types.Node_Id (N)).Kind = K_Fixed_Probability_Value
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Containment_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Assignment
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Integer
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Real
        or else Table (Types.Node_Id (N)).Kind = K_Numeric_Term
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect
        or else Table (Types.Node_Id (N)).Kind = K_Internal_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Code_Value
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant_Term
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Behavior
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Paths
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Propagation_Point);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_EMA_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_EMA_Annex
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Use_Error_Types
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element_Node
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event
        or else Table (Types.Node_Id (N)).Kind = K_Repair_Event
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Or_No_Error
        or else Table (Types.Node_Id (N)).Kind = K_Target_Error_Type_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Branching_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Type_Set_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Or_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Package_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Model_Library_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Port_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Self_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Event_Initiation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_State
        or else Table (Types.Node_Id (N)).Kind = K_Operator
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Branch
        or else Table (Types.Node_Id (N)).Kind = K_Branch_Probability
        or else Table (Types.Node_Id (N)).Kind = K_Fixed_Probability_Value
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Containment_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Assignment
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Integer
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Real
        or else Table (Types.Node_Id (N)).Kind = K_Numeric_Term
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect
        or else Table (Types.Node_Id (N)).Kind = K_Internal_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Code_Value
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant_Term
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Behavior
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Paths
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Propagation_Point);

      Table (Types.Node_Id (N)).L (2) := Int (V);
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

   function Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element_Node
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event
        or else Table (Types.Node_Id (N)).Kind = K_Repair_Event
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Port_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Self_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection
        or else Table (Types.Node_Id (N)).Kind = K_Internal_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant_Term
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Propagation_Point);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element_Node
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event
        or else Table (Types.Node_Id (N)).Kind = K_Repair_Event
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Port_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Self_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection
        or else Table (Types.Node_Id (N)).Kind = K_Internal_Event_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant_Term
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Point
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Propagation_Point);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Identifier;

   function Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Integer
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Real);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Value;

   procedure Set_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Integer
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Real);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value;

   function EMA_Container (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_EMA_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end EMA_Container;

   procedure Set_EMA_Container (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_EMA_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_EMA_Container;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (3));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Name;

   function Display_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (4));
   end Display_Name;

   procedure Set_Display_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Display_Name;

   function Corresponding_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Corresponding_Entity;

   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Corresponding_Entity;

   function Scope_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Scope_Entity;

   procedure Set_Scope_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Scope_Entity;

   function Homonym (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Homonym;

   procedure Set_Homonym (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Homonym;

   function Error_Type_Library (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Type_Library;

   procedure Set_Error_Type_Library (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Type_Library;

   function Error_Behavior_State_Machine_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Behavior_State_Machine_List;

   procedure Set_Error_Behavior_State_Machine_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Behavior_State_Machine_List;

   function Error_Type_Mappings_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Mappings_List;

   procedure Set_Error_Type_Mappings_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Mappings_List;

   function Error_Type_Transformations_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Error_Type_Transformations_List;

   procedure Set_Error_Type_Transformations_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Error_Type_Transformations_List;

   function Error_Type_Library_List_Used (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Type_Library_List_Used;

   procedure Set_Error_Type_Library_List_Used (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Type_Library_List_Used;

   function Error_Type_Library_List_Extended (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Type_Library_List_Extended;

   procedure Set_Error_Type_Library_List_Extended (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Type_Library_List_Extended;

   function Error_Type_Library_Element_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Library_Element_List;

   procedure Set_Error_Type_Library_Element_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Library_Element_List;

   function Properties (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Properties;

   procedure Set_Properties (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Properties;

   function Error_Type_Library_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings
        or else Table (Types.Node_Id (N)).Kind = K_Use_Error_Types
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Type_Library_List;

   procedure Set_Error_Type_Library_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings
        or else Table (Types.Node_Id (N)).Kind = K_Use_Error_Types
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Type_Library_List;

   function Error_Type_Transformation_Set_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Behavior);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Transformation_Set_Reference;

   procedure Set_Error_Type_Transformation_Set_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Behavior);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Transformation_Set_Reference;

   function Error_Behavior_Event_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Error_Behavior_Event_List;

   procedure Set_Error_Behavior_Event_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Error_Behavior_Event_List;

   function Error_Behavior_State_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine);

      return List_Id (Table (Types.Node_Id (N)).L (7));
   end Error_Behavior_State_List;

   procedure Set_Error_Behavior_State_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Error_Behavior_State_List;

   function Error_Behavior_Transition_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine);

      return List_Id (Table (Types.Node_Id (N)).L (8));
   end Error_Behavior_Transition_List;

   procedure Set_Error_Behavior_Transition_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Error_Behavior_Transition_List;

   function Error_Type_Mapping_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Mapping_List;

   procedure Set_Error_Type_Mapping_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Mapping_List;

   function Use_Error_Types (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Use_Error_Types;

   procedure Set_Use_Error_Types (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Use_Error_Types;

   function Error_Type_Transformation_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Transformation_List;

   procedure Set_Error_Type_Transformation_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformations);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Transformation_List;

   function Error_Type_Definition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Type_Definition;

   procedure Set_Error_Type_Definition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Type_Definition;

   function Error_Type_Alias (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Type_Alias;

   procedure Set_Error_Type_Alias (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Type_Alias;

   function Error_Type_Set_Definition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Set_Definition;

   procedure Set_Error_Type_Set_Definition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Set_Definition;

   function Error_Type_Set_Alias (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Error_Type_Set_Alias;

   procedure Set_Error_Type_Set_Alias (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Error_Type_Set_Alias;

   function Package_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Package_Reference;

   procedure Set_Package_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mappings_Reference);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Package_Reference;

   function Error_Event (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Event;

   procedure Set_Error_Event (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Event;

   function Recover_Event (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Recover_Event;

   procedure Set_Recover_Event (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Recover_Event;

   function Repair_Event (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Repair_Event;

   procedure Set_Repair_Event (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Event);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Repair_Event;

   function Error_Type_Set (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Or_No_Error
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Type_Set;

   procedure Set_Error_Type_Set (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Or_No_Error
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Type_Set;

   function Transition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Transition;

   procedure Set_Transition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Transition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Transition;

   function Branching_Transition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Branching_Transition;

   procedure Set_Branching_Transition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_Transition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Branching_Transition;

   function Source_Error_Type_Set (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_State);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Source_Error_Type_Set;

   procedure Set_Source_Error_Type_Set (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_State);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Source_Error_Type_Set;

   function Target_Error_Type_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Target_Error_Type_Instance;

   procedure Set_Target_Error_Type_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Target
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Target_Error_Type_Instance;

   function Source_Error_Type_Set_Or_Noerror (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Source_Error_Type_Set_Or_Noerror;

   procedure Set_Source_Error_Type_Set_Or_Noerror (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Source_Error_Type_Set_Or_Noerror;

   function Contributor_Error_Type_Set_Or_Noerror (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Contributor_Error_Type_Set_Or_Noerror;

   procedure Set_Contributor_Error_Type_Set_Or_Noerror (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Transformation);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Contributor_Error_Type_Set_Or_Noerror;

   function Error_Type_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element_Node
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Target_Error_Type_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Or_Set_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Type_Reference;

   procedure Set_Error_Type_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Library_Element_Node
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Target_Error_Type_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Or_Set_Reference);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Type_Reference;

   function Error_Type_Set_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Or_Set_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Set_Reference;

   procedure Set_Error_Type_Set_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Alias
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Or_Set_Reference);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Set_Reference;

   function Error_Event_Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Event_Condition;

   procedure Set_Error_Event_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Event
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Event_Condition;

   function Recover_Event_Initiators (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Recover_Event_Initiators;

   procedure Set_Recover_Event_Initiators (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Recover_Event);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Recover_Event_Initiators;

   function Repair_Event_Initiation (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Repair_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Repair_Event_Initiation;

   procedure Set_Repair_Event_Initiation (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Repair_Event);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Repair_Event_Initiation;

   function Error_Type_Product (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Target_Error_Type_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Type_Set_Element);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Type_Product;

   procedure Set_Error_Type_Product (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Target_Error_Type_Instance
        or else Table (Types.Node_Id (N)).Kind = K_Type_Set_Element);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Type_Product;

   function Error_Source_State (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Source_State;

   procedure Set_Error_Source_State (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Source_State;

   function Error_Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Condition;

   procedure Set_Error_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior_Node
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Condition;

   function Error_Transition_Target (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Error_Transition_Target;

   procedure Set_Error_Transition_Target (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Error_Transition_Target;

   function Error_Transition_Branch (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Error_Transition_Branch;

   procedure Set_Error_Transition_Branch (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transition);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Error_Transition_Branch;

   function Error_Type_Or_Set_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Set_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Type_Or_Set_Reference;

   procedure Set_Error_Type_Or_Set_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Set_Element);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Type_Or_Set_Reference;

   function Identifiers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Model_Library_Reference);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Identifiers;

   procedure Set_Identifiers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Model_Library_Reference);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Identifiers;

   function AADL_Package_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Model_Library_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end AADL_Package_Reference;

   procedure Set_AADL_Package_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Model_Library_Reference);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_AADL_Package_Reference;

   function Error_Model_Library_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Model_Library_Reference;

   procedure Set_Error_Model_Library_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Type_Set_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Error_Behavior_State_Machine_Reference);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Model_Library_Reference;

   function Mode_Transition_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Mode_Transition_Reference;

   procedure Set_Mode_Transition_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Mode_Transition_Reference;

   function Port_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Port_Reference;

   procedure Set_Port_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Port_Reference;

   function Self_Event_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Self_Event_Reference;

   procedure Set_Self_Event_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Initiator_Reference);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Self_Event_Reference;

   function Featuregroup_Identifier_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Reference);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Featuregroup_Identifier_List;

   procedure Set_Featuregroup_Identifier_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Reference);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Featuregroup_Identifier_List;

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

   function Error_Condition_Trigger (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Condition_Trigger;

   procedure Set_Error_Condition_Trigger (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Condition_Trigger;

   function Error_Condition_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Condition_Node;

   procedure Set_Error_Condition_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Condition_Node;

   function Numeric_Literal (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Numeric_Literal;

   procedure Set_Numeric_Literal (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Numeric_Literal;

   function Operator (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Operator;

   procedure Set_Operator (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Operator;

   function Error_Condition_Trigger_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Error_Condition_Trigger_List;

   procedure Set_Error_Condition_Trigger_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Error_Condition_Trigger_List;

   function Error_Transition_Target_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Branch);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Transition_Target_List;

   procedure Set_Error_Transition_Target_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Branch);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Transition_Target_List;

   function Branch_Probability_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Branch);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Branch_Probability_List;

   procedure Set_Branch_Probability_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Transition_Branch);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Branch_Probability_List;

   function Fixed_Probability_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Branch_Probability);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Fixed_Probability_Value;

   procedure Set_Fixed_Probability_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Branch_Probability);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Fixed_Probability_Value;

   function Property_Set_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fixed_Probability_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Set_Identifier;

   procedure Set_Property_Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fixed_Probability_Value);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Set_Identifier;

   function Real_Property_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fixed_Probability_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Real_Property_Identifier;

   procedure Set_Real_Property_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fixed_Probability_Value);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Real_Property_Identifier;

   function Incoming_Error_Propagation_Point (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Incoming_Error_Propagation_Point;

   procedure Set_Incoming_Error_Propagation_Point (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Sink
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Incoming_Error_Propagation_Point;

   function Error_Type_Set_Or_Noerror (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Set_Or_Noerror;

   procedure Set_Error_Type_Set_Or_Noerror (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Set_Or_Noerror;

   function Outgoing_Error_Propagation_Point (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Outgoing_Error_Propagation_Point;

   procedure Set_Outgoing_Error_Propagation_Point (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Condition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Error_Path
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Outgoing_Error_Propagation_Point;

   function Feature_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Feature_Reference;

   procedure Set_Feature_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Feature_Reference;

   function Binding_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Binding_Reference;

   procedure Set_Binding_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation_Point);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Binding_Reference;

   function Binding_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Reference);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Binding_Kind;

   procedure Set_Binding_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Reference);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Binding_Kind;

   function Error_Type_Mappings_Reference_Equivalence (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Type_Mappings_Reference_Equivalence;

   procedure Set_Error_Type_Mappings_Reference_Equivalence (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Type_Mappings_Reference_Equivalence;

   function Error_Type_Mappings_Reference_Mappings (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Type_Mappings_Reference_Mappings;

   procedure Set_Error_Type_Mappings_Reference_Mappings (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Type_Mappings_Reference_Mappings;

   function Error_Behavior_State_Machine_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Error_Behavior_State_Machine_Reference;

   procedure Set_Error_Behavior_State_Machine_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Error_Behavior_State_Machine_Reference;

   function Error_Propagations (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Error_Propagations;

   procedure Set_Error_Propagations (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Error_Propagations;

   function Component_Error_Behavior (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Component_Error_Behavior;

   procedure Set_Component_Error_Behavior (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Component_Error_Behavior;

   function Composite_Error_Behavior (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return List_Id (Table (Types.Node_Id (N)).L (8));
   end Composite_Error_Behavior;

   procedure Set_Composite_Error_Behavior (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Composite_Error_Behavior;

   function Connection_Error_Behavior (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Connection_Error_Behavior;

   procedure Set_Connection_Error_Behavior (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Connection_Error_Behavior;

   function Propagation_Paths (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Propagation_Paths;

   procedure Set_Propagation_Paths (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Propagation_Paths;

   function EMV2_Properties_Section (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return List_Id (Table (Types.Node_Id (N)).L (11));
   end EMV2_Properties_Section;

   procedure Set_EMV2_Properties_Section (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_EMV2_Properties_Section;

   function Next_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Next_Entity;

   procedure Set_Next_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Next_Entity;

   function Emv2_Containment_Path_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Emv2_Containment_Path_List;

   procedure Set_Emv2_Containment_Path_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Emv2_Containment_Path_List;

   function Assignment (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Assignment;

   procedure Set_Assignment (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Assignment;

   function Is_Constant (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Constant;

   procedure Set_Is_Constant (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Constant;

   function Property_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Property_Identifier;

   procedure Set_Property_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Contained_Property_Association);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Property_Identifier;

   function Aadl2_Core_Path_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Containment_Path);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Aadl2_Core_Path_List;

   procedure Set_Aadl2_Core_Path_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Containment_Path);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Aadl2_Core_Path_List;

   function Emv2_Annex_Specific_Path_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Containment_Path);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Emv2_Annex_Specific_Path_List;

   procedure Set_Emv2_Annex_Specific_Path_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Emv2_Containment_Path);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Emv2_Annex_Specific_Path_List;

   function Property_Set (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Property_Set;

   procedure Set_Property_Set (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Property_Constant_Term);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Property_Set;

   function Property_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Name;

   procedure Set_Property_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Identifier);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Name;

   function Prop_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Prop_Value;

   procedure Set_Prop_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Prop_Value;

   function Prop_Value_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Prop_Value_List;

   procedure Set_Prop_Value_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Prop_Value_List;

   function Prop_Name_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Prop_Name_List;

   procedure Set_Prop_Name_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Prop_Name_List;

   function In_Modes_Properties (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end In_Modes_Properties;

   procedure Set_In_Modes_Properties (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_In_Modes_Properties;

   function List_Of_Strings (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end List_Of_Strings;

   procedure Set_List_Of_Strings (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_List_Of_Strings;

   function Number_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Numeric_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Number_Value;

   procedure Set_Number_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Numeric_Term);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Number_Value;

   function Unit_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Numeric_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Unit_Identifier;

   procedure Set_Unit_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Numeric_Term);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Unit_Identifier;

   function Lower_Bound (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Lower_Bound;

   procedure Set_Lower_Bound (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Lower_Bound;

   function Upper_Bound (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Upper_Bound;

   procedure Set_Upper_Bound (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Upper_Bound;

   function Error_Propagation_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Propagation_List;

   procedure Set_Error_Propagation_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Propagation_List;

   function Error_Containment_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Containment_List;

   procedure Set_Error_Containment_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Containment_List;

   function Error_Flow_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Flow_List;

   procedure Set_Error_Flow_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Flow_List;

   function Error_Propagation_Point (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Target);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Propagation_Point;

   procedure Set_Error_Propagation_Point (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Target);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Propagation_Point;

   function Propagation (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Propagation;

   procedure Set_Propagation (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagations_Element
        or else Table (Types.Node_Id (N)).Kind = K_Error_Propagation
        or else Table (Types.Node_Id (N)).Kind = K_Error_Containment);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Propagation;

   function Propagation_Kind (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Propagation_Kind;

   procedure Set_Propagation_Kind (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Propagation_Kind;

   function Error_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Source;

   procedure Set_Error_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Source;

   function Error_Sink (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Error_Sink;

   procedure Set_Error_Sink (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Error_Sink;

   function Error_Path (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Path;

   procedure Set_Error_Path (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Flow);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Path;

   function Error_Behavior_State (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Error_Behavior_State;

   procedure Set_Error_Behavior_State (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Error_Behavior_State;

   function Failure_Mode_Description (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Failure_Mode_Description;

   procedure Set_Failure_Mode_Description (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Fault_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Failure_Mode_Description;

   function Component_Specific_Error_Behavior_Transition_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Component_Specific_Error_Behavior_Transition_List;

   procedure Set_Component_Specific_Error_Behavior_Transition_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Component_Specific_Error_Behavior_Transition_List;

   function Outgoing_Propagation_Condition_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Outgoing_Propagation_Condition_List;

   procedure Set_Outgoing_Propagation_Condition_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Outgoing_Propagation_Condition_List;

   function Error_Detection_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Error_Detection_List;

   procedure Set_Error_Detection_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Error_Detection_List;

   function Error_State_To_Mode_Mapping_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      return List_Id (Table (Types.Node_Id (N)).L (7));
   end Error_State_To_Mode_Mapping_List;

   procedure Set_Error_State_To_Mode_Mapping_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Error_Behavior);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Error_State_To_Mode_Mapping_List;

   function Propagation_Target (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Propagation_Target;

   procedure Set_Propagation_Target (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Outgoing_Propagation_Condition);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Propagation_Target;

   function Propagated_Target_Error_Type_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Target);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Propagated_Target_Error_Type_Instance;

   procedure Set_Propagated_Target_Error_Type_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Target);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Propagated_Target_Error_Type_Instance;

   function Error_Detection_Effect (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Error_Detection_Effect;

   procedure Set_Error_Detection_Effect (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Error_Detection_Effect;

   function Outgoing_Port_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Outgoing_Port_Reference;

   procedure Set_Outgoing_Port_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Outgoing_Port_Reference;

   function Internal_Event_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Internal_Event_Reference;

   procedure Set_Internal_Event_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Internal_Event_Reference;

   function Error_Code_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Error_Code_Value;

   procedure Set_Error_Code_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Detection_Effect);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Error_Code_Value;

   function Enumeration_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Code_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Enumeration_Identifier;

   procedure Set_Enumeration_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Code_Value);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Enumeration_Identifier;

   function Property_Constant_Term (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Code_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Property_Constant_Term;

   procedure Set_Property_Constant_Term (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Code_Value);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Property_Constant_Term;

   function Mode_Name_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Mode_Name_List;

   procedure Set_Mode_Name_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_State_To_Mode_Mapping);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Mode_Name_List;

   function Composite_State_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Composite_State_Identifier;

   procedure Set_Composite_State_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Composite_State_Identifier;

   function Subcomponent_State_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Subcomponent_State_Expression;

   procedure Set_Subcomponent_State_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_Error_State);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Subcomponent_State_Expression;

   function Composite_State_Element (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Composite_State_Element;

   procedure Set_Composite_State_Element (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Composite_State_Element;

   function Composite_State_Expression_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Composite_State_Expression_Node;

   procedure Set_Composite_State_Expression_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Composite_State_Expression_Node;

   function Composite_State_Element_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Composite_State_Element_List;

   procedure Set_Composite_State_Element_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Expression);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Composite_State_Element_List;

   function Subcomponent_Error_State (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Subcomponent_Error_State;

   procedure Set_Subcomponent_Error_State (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Composite_State_Element);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Subcomponent_Error_State;

   function Subcomponent_Identifier_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Propagation_Point);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Subcomponent_Identifier_List;

   procedure Set_Subcomponent_Identifier_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Error_State
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Propagation_Point);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Subcomponent_Identifier_List;

   function Connection_Error_Source_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Behavior);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Connection_Error_Source_List;

   procedure Set_Connection_Error_Source_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Behavior);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Connection_Error_Source_List;

   function Effect_Error_Type_Set (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Effect_Error_Type_Set;

   procedure Set_Effect_Error_Type_Set (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Effect_Error_Type_Set;

   function Fault_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Fault_Source;

   procedure Set_Fault_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Fault_Source;

   function Fault_Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Fault_Condition;

   procedure Set_Fault_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source_Parent
        or else Table (Types.Node_Id (N)).Kind = K_Error_Source
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Fault_Condition;

   function Connection_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Connection_Identifier;

   procedure Set_Connection_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Connection_Identifier;

   function Fault_Source_Error_Type_Set (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      return List_Id (Table (Types.Node_Id (N)).L (7));
   end Fault_Source_Error_Type_Set;

   procedure Set_Fault_Source_Error_Type_Set (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection_Error_Source);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Fault_Source_Error_Type_Set;

   function Propagation_Point_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Paths);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Propagation_Point_List;

   procedure Set_Propagation_Point_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Paths);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Propagation_Point_List;

   function Propagation_Path_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Paths);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Propagation_Path_List;

   procedure Set_Propagation_Path_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Paths);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Propagation_Path_List;

   function Source_Qualified_Propagation_Point (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Source_Qualified_Propagation_Point;

   procedure Set_Source_Qualified_Propagation_Point (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Source_Qualified_Propagation_Point;

   function Target_Qualified_Propagation_Point (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Target_Qualified_Propagation_Point;

   procedure Set_Target_Qualified_Propagation_Point (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Propagation_Path);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Target_Qualified_Propagation_Point;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Definition =>
            W_Definition
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_EMA_Entity =>
            W_EMA_Entity
              (Node_Id (N));
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_EMA_Annex =>
            W_EMA_Annex
              (Node_Id (N));
         when K_Annex_Library =>
            W_Annex_Library
              (Node_Id (N));
         when K_Error_Type_Library =>
            W_Error_Type_Library
              (Node_Id (N));
         when K_Error_Behavior_State_Machine =>
            W_Error_Behavior_State_Machine
              (Node_Id (N));
         when K_Error_Type_Mappings =>
            W_Error_Type_Mappings
              (Node_Id (N));
         when K_Error_Type_Transformations =>
            W_Error_Type_Transformations
              (Node_Id (N));
         when K_Error_Type_Library_Element =>
            W_Error_Type_Library_Element
              (Node_Id (N));
         when K_Error_Type_Transformation_Set_Reference =>
            W_Error_Type_Transformation_Set_Reference
              (Node_Id (N));
         when K_Error_Behavior_Event =>
            W_Error_Behavior_Event
              (Node_Id (N));
         when K_Error_Behavior_State =>
            W_Error_Behavior_State
              (Node_Id (N));
         when K_Error_Behavior_Transition =>
            W_Error_Behavior_Transition
              (Node_Id (N));
         when K_Error_Type_Mapping =>
            W_Error_Type_Mapping
              (Node_Id (N));
         when K_Use_Error_Types =>
            W_Use_Error_Types
              (Node_Id (N));
         when K_Error_Type_Transformation =>
            W_Error_Type_Transformation
              (Node_Id (N));
         when K_Error_Type_Library_Element_Node =>
            W_Error_Type_Library_Element_Node
              (Node_Id (N));
         when K_Error_Type_Definition =>
            W_Error_Type_Definition
              (Node_Id (N));
         when K_Error_Type_Alias =>
            W_Error_Type_Alias
              (Node_Id (N));
         when K_Error_Type_Set_Definition =>
            W_Error_Type_Set_Definition
              (Node_Id (N));
         when K_Error_Type_Set_Alias =>
            W_Error_Type_Set_Alias
              (Node_Id (N));
         when K_Error_Event =>
            W_Error_Event
              (Node_Id (N));
         when K_Recover_Event =>
            W_Recover_Event
              (Node_Id (N));
         when K_Repair_Event =>
            W_Repair_Event
              (Node_Id (N));
         when K_Error_Type_Set_Or_No_Error =>
            W_Error_Type_Set_Or_No_Error
              (Node_Id (N));
         when K_Target_Error_Type_Instance =>
            W_Target_Error_Type_Instance
              (Node_Id (N));
         when K_Transition =>
            W_Transition
              (Node_Id (N));
         when K_Branching_Transition =>
            W_Branching_Transition
              (Node_Id (N));
         when K_Type_Set_Element =>
            W_Type_Set_Element
              (Node_Id (N));
         when K_Error_Type_Or_Set_Reference =>
            W_Error_Type_Or_Set_Reference
              (Node_Id (N));
         when K_Package_Reference =>
            W_Package_Reference
              (Node_Id (N));
         when K_Error_Model_Library_Reference =>
            W_Error_Model_Library_Reference
              (Node_Id (N));
         when K_Error_Type_Reference =>
            W_Error_Type_Reference
              (Node_Id (N));
         when K_Error_Type_Set_Reference =>
            W_Error_Type_Set_Reference
              (Node_Id (N));
         when K_Initiator_Reference =>
            W_Initiator_Reference
              (Node_Id (N));
         when K_Port_Reference =>
            W_Port_Reference
              (Node_Id (N));
         when K_Self_Event_Reference =>
            W_Self_Event_Reference
              (Node_Id (N));
         when K_Mode_Transition_Reference =>
            W_Mode_Transition_Reference
              (Node_Id (N));
         when K_Event_Initiation =>
            W_Event_Initiation
              (Node_Id (N));
         when K_Error_Source_State =>
            W_Error_Source_State
              (Node_Id (N));
         when K_Operator =>
            W_Operator
              (Node_Id (N));
         when K_Error_Condition =>
            W_Error_Condition
              (Node_Id (N));
         when K_Error_Transition_Target =>
            W_Error_Transition_Target
              (Node_Id (N));
         when K_Error_Transition_Branch =>
            W_Error_Transition_Branch
              (Node_Id (N));
         when K_Branch_Probability =>
            W_Branch_Probability
              (Node_Id (N));
         when K_Fixed_Probability_Value =>
            W_Fixed_Probability_Value
              (Node_Id (N));
         when K_Error_Condition_Trigger =>
            W_Error_Condition_Trigger
              (Node_Id (N));
         when K_Error_Propagation_Point =>
            W_Error_Propagation_Point
              (Node_Id (N));
         when K_Feature_Reference =>
            W_Feature_Reference
              (Node_Id (N));
         when K_Binding_Reference =>
            W_Binding_Reference
              (Node_Id (N));
         when K_Annex_Subclause =>
            W_Annex_Subclause
              (Node_Id (N));
         when K_Emv2_Contained_Property_Association =>
            W_Emv2_Contained_Property_Association
              (Node_Id (N));
         when K_Emv2_Containment_Path =>
            W_Emv2_Containment_Path
              (Node_Id (N));
         when K_Property_Identifier =>
            W_Property_Identifier
              (Node_Id (N));
         when K_Assignment =>
            W_Assignment
              (Node_Id (N));
         when K_Property_Value =>
            W_Property_Value
              (Node_Id (N));
         when K_AADL_Integer =>
            W_AADL_Integer
              (Node_Id (N));
         when K_AADL_Real =>
            W_AADL_Real
              (Node_Id (N));
         when K_Numeric_Term =>
            W_Numeric_Term
              (Node_Id (N));
         when K_Range_Type =>
            W_Range_Type
              (Node_Id (N));
         when K_Error_Type_Mappings_Reference =>
            W_Error_Type_Mappings_Reference
              (Node_Id (N));
         when K_Error_Behavior_State_Machine_Reference =>
            W_Error_Behavior_State_Machine_Reference
              (Node_Id (N));
         when K_Error_Propagations =>
            W_Error_Propagations
              (Node_Id (N));
         when K_Error_Propagations_Element =>
            W_Error_Propagations_Element
              (Node_Id (N));
         when K_Propagation =>
            W_Propagation
              (Node_Id (N));
         when K_Error_Propagation =>
            W_Error_Propagation
              (Node_Id (N));
         when K_Error_Containment =>
            W_Error_Containment
              (Node_Id (N));
         when K_Error_Flow =>
            W_Error_Flow
              (Node_Id (N));
         when K_Fault_Source =>
            W_Fault_Source
              (Node_Id (N));
         when K_Error_Sink =>
            W_Error_Sink
              (Node_Id (N));
         when K_Error_Path =>
            W_Error_Path
              (Node_Id (N));
         when K_Component_Error_Behavior =>
            W_Component_Error_Behavior
              (Node_Id (N));
         when K_Component_Error_Behavior_Node =>
            W_Component_Error_Behavior_Node
              (Node_Id (N));
         when K_Outgoing_Propagation_Condition =>
            W_Outgoing_Propagation_Condition
              (Node_Id (N));
         when K_Propagation_Target =>
            W_Propagation_Target
              (Node_Id (N));
         when K_Error_Detection =>
            W_Error_Detection
              (Node_Id (N));
         when K_Error_Detection_Effect =>
            W_Error_Detection_Effect
              (Node_Id (N));
         when K_Internal_Event_Reference =>
            W_Internal_Event_Reference
              (Node_Id (N));
         when K_Error_Code_Value =>
            W_Error_Code_Value
              (Node_Id (N));
         when K_Property_Constant_Term =>
            W_Property_Constant_Term
              (Node_Id (N));
         when K_Error_State_To_Mode_Mapping =>
            W_Error_State_To_Mode_Mapping
              (Node_Id (N));
         when K_Composite_Error_State =>
            W_Composite_Error_State
              (Node_Id (N));
         when K_Composite_State_Expression =>
            W_Composite_State_Expression
              (Node_Id (N));
         when K_Composite_State_Element =>
            W_Composite_State_Element
              (Node_Id (N));
         when K_Subcomponent_Error_State =>
            W_Subcomponent_Error_State
              (Node_Id (N));
         when K_Connection_Error_Behavior =>
            W_Connection_Error_Behavior
              (Node_Id (N));
         when K_Error_Source_Parent =>
            W_Error_Source_Parent
              (Node_Id (N));
         when K_Error_Source =>
            W_Error_Source
              (Node_Id (N));
         when K_Connection_Error_Source =>
            W_Connection_Error_Source
              (Node_Id (N));
         when K_Propagation_Paths =>
            W_Propagation_Paths
              (Node_Id (N));
         when K_Propagation_Point =>
            W_Propagation_Point
              (Node_Id (N));
         when K_Propagation_Path =>
            W_Propagation_Path
              (Node_Id (N));
         when K_Qualified_Propagation_Point =>
            W_Qualified_Propagation_Point
              (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

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

   procedure W_EMA_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("EMA_Container",
         "Node_Id",
         Image (EMA_Container (N)),
         Int (EMA_Container (N)));
   end W_EMA_Entity;

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
        ("EMA_Container",
         "Node_Id",
         Image (EMA_Container (N)),
         Int (EMA_Container (N)));
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

   procedure W_EMA_Annex (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_EMA_Annex;

   procedure W_Annex_Library (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Library",
         "Node_Id",
         Image (Error_Type_Library (N)),
         Int (Error_Type_Library (N)));
      W_Node_Attribute
        ("Error_Behavior_State_Machine_List",
         "List_Id",
         Image (Error_Behavior_State_Machine_List (N)),
         Int (Error_Behavior_State_Machine_List (N)));
      W_Node_Attribute
        ("Error_Type_Mappings_List",
         "List_Id",
         Image (Error_Type_Mappings_List (N)),
         Int (Error_Type_Mappings_List (N)));
      W_Node_Attribute
        ("Error_Type_Transformations_List",
         "List_Id",
         Image (Error_Type_Transformations_List (N)),
         Int (Error_Type_Transformations_List (N)));
   end W_Annex_Library;

   procedure W_Error_Type_Library (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Library_List_Used",
         "List_Id",
         Image (Error_Type_Library_List_Used (N)),
         Int (Error_Type_Library_List_Used (N)));
      W_Node_Attribute
        ("Error_Type_Library_List_Extended",
         "List_Id",
         Image (Error_Type_Library_List_Extended (N)),
         Int (Error_Type_Library_List_Extended (N)));
      W_Node_Attribute
        ("Error_Type_Library_Element_List",
         "List_Id",
         Image (Error_Type_Library_Element_List (N)),
         Int (Error_Type_Library_Element_List (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
   end W_Error_Type_Library;

   procedure W_Error_Behavior_State_Machine (N : Node_Id) is
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
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Error_Type_Library_List",
         "List_Id",
         Image (Error_Type_Library_List (N)),
         Int (Error_Type_Library_List (N)));
      W_Node_Attribute
        ("Error_Type_Transformation_Set_Reference",
         "Node_Id",
         Image (Error_Type_Transformation_Set_Reference (N)),
         Int (Error_Type_Transformation_Set_Reference (N)));
      W_Node_Attribute
        ("Error_Behavior_Event_List",
         "List_Id",
         Image (Error_Behavior_Event_List (N)),
         Int (Error_Behavior_Event_List (N)));
      W_Node_Attribute
        ("Error_Behavior_State_List",
         "List_Id",
         Image (Error_Behavior_State_List (N)),
         Int (Error_Behavior_State_List (N)));
      W_Node_Attribute
        ("Error_Behavior_Transition_List",
         "List_Id",
         Image (Error_Behavior_Transition_List (N)),
         Int (Error_Behavior_Transition_List (N)));
   end W_Error_Behavior_State_Machine;

   procedure W_Error_Type_Mappings (N : Node_Id) is
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
        ("Error_Type_Library_List",
         "List_Id",
         Image (Error_Type_Library_List (N)),
         Int (Error_Type_Library_List (N)));
      W_Node_Attribute
        ("Error_Type_Mapping_List",
         "List_Id",
         Image (Error_Type_Mapping_List (N)),
         Int (Error_Type_Mapping_List (N)));
   end W_Error_Type_Mappings;

   procedure W_Error_Type_Transformations (N : Node_Id) is
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
        ("Use_Error_Types",
         "Node_Id",
         Image (Use_Error_Types (N)),
         Int (Use_Error_Types (N)));
      W_Node_Attribute
        ("Error_Type_Transformation_List",
         "List_Id",
         Image (Error_Type_Transformation_List (N)),
         Int (Error_Type_Transformation_List (N)));
   end W_Error_Type_Transformations;

   procedure W_Error_Type_Library_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Definition",
         "Node_Id",
         Image (Error_Type_Definition (N)),
         Int (Error_Type_Definition (N)));
      W_Node_Attribute
        ("Error_Type_Alias",
         "Node_Id",
         Image (Error_Type_Alias (N)),
         Int (Error_Type_Alias (N)));
      W_Node_Attribute
        ("Error_Type_Set_Definition",
         "Node_Id",
         Image (Error_Type_Set_Definition (N)),
         Int (Error_Type_Set_Definition (N)));
      W_Node_Attribute
        ("Error_Type_Set_Alias",
         "Node_Id",
         Image (Error_Type_Set_Alias (N)),
         Int (Error_Type_Set_Alias (N)));
   end W_Error_Type_Library_Element;

   procedure W_Error_Type_Transformation_Set_Reference (N : Node_Id) is
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
        ("Package_Reference",
         "Node_Id",
         Image (Package_Reference (N)),
         Int (Package_Reference (N)));
   end W_Error_Type_Transformation_Set_Reference;

   procedure W_Error_Behavior_Event (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Event",
         "Node_Id",
         Image (Error_Event (N)),
         Int (Error_Event (N)));
      W_Node_Attribute
        ("Recover_Event",
         "Node_Id",
         Image (Recover_Event (N)),
         Int (Recover_Event (N)));
      W_Node_Attribute
        ("Repair_Event",
         "Node_Id",
         Image (Repair_Event (N)),
         Int (Repair_Event (N)));
   end W_Error_Behavior_Event;

   procedure W_Error_Behavior_State (N : Node_Id) is
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
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
   end W_Error_Behavior_State;

   procedure W_Error_Behavior_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Transition",
         "Node_Id",
         Image (Transition (N)),
         Int (Transition (N)));
      W_Node_Attribute
        ("Branching_Transition",
         "Node_Id",
         Image (Branching_Transition (N)),
         Int (Branching_Transition (N)));
   end W_Error_Behavior_Transition;

   procedure W_Error_Type_Mapping (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Source_Error_Type_Set",
         "List_Id",
         Image (Source_Error_Type_Set (N)),
         Int (Source_Error_Type_Set (N)));
      W_Node_Attribute
        ("Target_Error_Type_Instance",
         "Node_Id",
         Image (Target_Error_Type_Instance (N)),
         Int (Target_Error_Type_Instance (N)));
   end W_Error_Type_Mapping;

   procedure W_Use_Error_Types (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Library_List",
         "List_Id",
         Image (Error_Type_Library_List (N)),
         Int (Error_Type_Library_List (N)));
   end W_Use_Error_Types;

   procedure W_Error_Type_Transformation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Target_Error_Type_Instance",
         "Node_Id",
         Image (Target_Error_Type_Instance (N)),
         Int (Target_Error_Type_Instance (N)));
      W_Node_Attribute
        ("Source_Error_Type_Set_Or_Noerror",
         "Node_Id",
         Image (Source_Error_Type_Set_Or_Noerror (N)),
         Int (Source_Error_Type_Set_Or_Noerror (N)));
      W_Node_Attribute
        ("Contributor_Error_Type_Set_Or_Noerror",
         "Node_Id",
         Image (Contributor_Error_Type_Set_Or_Noerror (N)),
         Int (Contributor_Error_Type_Set_Or_Noerror (N)));
   end W_Error_Type_Transformation;

   procedure W_Error_Type_Library_Element_Node (N : Node_Id) is
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
        ("Error_Type_Reference",
         "Node_Id",
         Image (Error_Type_Reference (N)),
         Int (Error_Type_Reference (N)));
   end W_Error_Type_Library_Element_Node;

   procedure W_Error_Type_Definition (N : Node_Id) is
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
        ("Error_Type_Reference",
         "Node_Id",
         Image (Error_Type_Reference (N)),
         Int (Error_Type_Reference (N)));
   end W_Error_Type_Definition;

   procedure W_Error_Type_Alias (N : Node_Id) is
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
        ("Error_Type_Reference",
         "Node_Id",
         Image (Error_Type_Reference (N)),
         Int (Error_Type_Reference (N)));
   end W_Error_Type_Alias;

   procedure W_Error_Type_Set_Definition (N : Node_Id) is
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
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
   end W_Error_Type_Set_Definition;

   procedure W_Error_Type_Set_Alias (N : Node_Id) is
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
        ("Error_Type_Set_Reference",
         "Node_Id",
         Image (Error_Type_Set_Reference (N)),
         Int (Error_Type_Set_Reference (N)));
   end W_Error_Type_Set_Alias;

   procedure W_Error_Event (N : Node_Id) is
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
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Error_Event_Condition",
         "Node_Id",
         Image (Error_Event_Condition (N)),
         Int (Error_Event_Condition (N)));
   end W_Error_Event;

   procedure W_Recover_Event (N : Node_Id) is
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
        ("Error_Event_Condition",
         "Node_Id",
         Image (Error_Event_Condition (N)),
         Int (Error_Event_Condition (N)));
      W_Node_Attribute
        ("Recover_Event_Initiators",
         "List_Id",
         Image (Recover_Event_Initiators (N)),
         Int (Recover_Event_Initiators (N)));
   end W_Recover_Event;

   procedure W_Repair_Event (N : Node_Id) is
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
        ("Repair_Event_Initiation",
         "Node_Id",
         Image (Repair_Event_Initiation (N)),
         Int (Repair_Event_Initiation (N)));
   end W_Repair_Event;

   procedure W_Error_Type_Set_Or_No_Error (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
   end W_Error_Type_Set_Or_No_Error;

   procedure W_Target_Error_Type_Instance (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Reference",
         "Node_Id",
         Image (Error_Type_Reference (N)),
         Int (Error_Type_Reference (N)));
      W_Node_Attribute
        ("Error_Type_Product",
         "List_Id",
         Image (Error_Type_Product (N)),
         Int (Error_Type_Product (N)));
   end W_Target_Error_Type_Instance;

   procedure W_Transition (N : Node_Id) is
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
        ("Error_Source_State",
         "Node_Id",
         Image (Error_Source_State (N)),
         Int (Error_Source_State (N)));
      W_Node_Attribute
        ("Error_Condition",
         "Node_Id",
         Image (Error_Condition (N)),
         Int (Error_Condition (N)));
      W_Node_Attribute
        ("Error_Transition_Target",
         "Node_Id",
         Image (Error_Transition_Target (N)),
         Int (Error_Transition_Target (N)));
      W_Node_Attribute
        ("Error_Transition_Branch",
         "Node_Id",
         Image (Error_Transition_Branch (N)),
         Int (Error_Transition_Branch (N)));
   end W_Transition;

   procedure W_Branching_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Branching_Transition;

   procedure W_Type_Set_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Product",
         "List_Id",
         Image (Error_Type_Product (N)),
         Int (Error_Type_Product (N)));
      W_Node_Attribute
        ("Error_Type_Or_Set_Reference",
         "Node_Id",
         Image (Error_Type_Or_Set_Reference (N)),
         Int (Error_Type_Or_Set_Reference (N)));
   end W_Type_Set_Element;

   procedure W_Error_Type_Or_Set_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Reference",
         "Node_Id",
         Image (Error_Type_Reference (N)),
         Int (Error_Type_Reference (N)));
      W_Node_Attribute
        ("Error_Type_Set_Reference",
         "Node_Id",
         Image (Error_Type_Set_Reference (N)),
         Int (Error_Type_Set_Reference (N)));
   end W_Error_Type_Or_Set_Reference;

   procedure W_Package_Reference (N : Node_Id) is
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
        ("AADL_Package_Reference",
         "Node_Id",
         Image (AADL_Package_Reference (N)),
         Int (AADL_Package_Reference (N)));
   end W_Package_Reference;

   procedure W_Error_Model_Library_Reference (N : Node_Id) is
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
        ("AADL_Package_Reference",
         "Node_Id",
         Image (AADL_Package_Reference (N)),
         Int (AADL_Package_Reference (N)));
   end W_Error_Model_Library_Reference;

   procedure W_Error_Type_Reference (N : Node_Id) is
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
        ("Error_Model_Library_Reference",
         "Node_Id",
         Image (Error_Model_Library_Reference (N)),
         Int (Error_Model_Library_Reference (N)));
   end W_Error_Type_Reference;

   procedure W_Error_Type_Set_Reference (N : Node_Id) is
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
        ("Error_Model_Library_Reference",
         "Node_Id",
         Image (Error_Model_Library_Reference (N)),
         Int (Error_Model_Library_Reference (N)));
   end W_Error_Type_Set_Reference;

   procedure W_Initiator_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Mode_Transition_Reference",
         "Node_Id",
         Image (Mode_Transition_Reference (N)),
         Int (Mode_Transition_Reference (N)));
      W_Node_Attribute
        ("Port_Reference",
         "Node_Id",
         Image (Port_Reference (N)),
         Int (Port_Reference (N)));
      W_Node_Attribute
        ("Self_Event_Reference",
         "Node_Id",
         Image (Self_Event_Reference (N)),
         Int (Self_Event_Reference (N)));
   end W_Initiator_Reference;

   procedure W_Port_Reference (N : Node_Id) is
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
        ("Featuregroup_Identifier_List",
         "List_Id",
         Image (Featuregroup_Identifier_List (N)),
         Int (Featuregroup_Identifier_List (N)));
   end W_Port_Reference;

   procedure W_Self_Event_Reference (N : Node_Id) is
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
   end W_Self_Event_Reference;

   procedure W_Mode_Transition_Reference (N : Node_Id) is
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
        ("Package_Reference",
         "Node_Id",
         Image (Package_Reference (N)),
         Int (Package_Reference (N)));
   end W_Mode_Transition_Reference;

   procedure W_Event_Initiation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Event_Initiation;

   procedure W_Error_Source_State (N : Node_Id) is
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
        ("Source_Error_Type_Set",
         "List_Id",
         Image (Source_Error_Type_Set (N)),
         Int (Source_Error_Type_Set (N)));
   end W_Error_Source_State;

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

   procedure W_Error_Condition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Condition_Trigger",
         "Node_Id",
         Image (Error_Condition_Trigger (N)),
         Int (Error_Condition_Trigger (N)));
      W_Node_Attribute
        ("Error_Condition_Node",
         "Node_Id",
         Image (Error_Condition_Node (N)),
         Int (Error_Condition_Node (N)));
      W_Node_Attribute
        ("Numeric_Literal",
         "Node_Id",
         Image (Numeric_Literal (N)),
         Int (Numeric_Literal (N)));
      W_Node_Attribute
        ("Operator",
         "Node_Id",
         Image (Operator (N)),
         Int (Operator (N)));
      W_Node_Attribute
        ("Error_Condition_Trigger_List",
         "List_Id",
         Image (Error_Condition_Trigger_List (N)),
         Int (Error_Condition_Trigger_List (N)));
   end W_Error_Condition;

   procedure W_Error_Transition_Target (N : Node_Id) is
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
        ("Target_Error_Type_Instance",
         "Node_Id",
         Image (Target_Error_Type_Instance (N)),
         Int (Target_Error_Type_Instance (N)));
   end W_Error_Transition_Target;

   procedure W_Error_Transition_Branch (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Transition_Target_List",
         "List_Id",
         Image (Error_Transition_Target_List (N)),
         Int (Error_Transition_Target_List (N)));
      W_Node_Attribute
        ("Branch_Probability_List",
         "List_Id",
         Image (Branch_Probability_List (N)),
         Int (Branch_Probability_List (N)));
   end W_Error_Transition_Branch;

   procedure W_Branch_Probability (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Fixed_Probability_Value",
         "Node_Id",
         Image (Fixed_Probability_Value (N)),
         Int (Fixed_Probability_Value (N)));
   end W_Branch_Probability;

   procedure W_Fixed_Probability_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Property_Set_Identifier",
         "Node_Id",
         Image (Property_Set_Identifier (N)),
         Int (Property_Set_Identifier (N)));
      W_Node_Attribute
        ("Real_Property_Identifier",
         "Node_Id",
         Image (Real_Property_Identifier (N)),
         Int (Real_Property_Identifier (N)));
   end W_Fixed_Probability_Value;

   procedure W_Error_Condition_Trigger (N : Node_Id) is
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
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Incoming_Error_Propagation_Point",
         "Node_Id",
         Image (Incoming_Error_Propagation_Point (N)),
         Int (Incoming_Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Error_Type_Set_Or_Noerror",
         "Node_Id",
         Image (Error_Type_Set_Or_Noerror (N)),
         Int (Error_Type_Set_Or_Noerror (N)));
      W_Node_Attribute
        ("Outgoing_Error_Propagation_Point",
         "Node_Id",
         Image (Outgoing_Error_Propagation_Point (N)),
         Int (Outgoing_Error_Propagation_Point (N)));
   end W_Error_Condition_Trigger;

   procedure W_Error_Propagation_Point (N : Node_Id) is
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
        ("Feature_Reference",
         "Node_Id",
         Image (Feature_Reference (N)),
         Int (Feature_Reference (N)));
      W_Node_Attribute
        ("Binding_Reference",
         "Node_Id",
         Image (Binding_Reference (N)),
         Int (Binding_Reference (N)));
   end W_Error_Propagation_Point;

   procedure W_Feature_Reference (N : Node_Id) is
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
        ("Featuregroup_Identifier_List",
         "List_Id",
         Image (Featuregroup_Identifier_List (N)),
         Int (Featuregroup_Identifier_List (N)));
   end W_Feature_Reference;

   procedure W_Binding_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Binding_Kind",
         "Byte",
         Image (Binding_Kind (N)));
   end W_Binding_Reference;

   procedure W_Annex_Subclause (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Library_List",
         "List_Id",
         Image (Error_Type_Library_List (N)),
         Int (Error_Type_Library_List (N)));
      W_Node_Attribute
        ("Error_Type_Mappings_Reference_Equivalence",
         "Node_Id",
         Image (Error_Type_Mappings_Reference_Equivalence (N)),
         Int (Error_Type_Mappings_Reference_Equivalence (N)));
      W_Node_Attribute
        ("Error_Type_Mappings_Reference_Mappings",
         "Node_Id",
         Image (Error_Type_Mappings_Reference_Mappings (N)),
         Int (Error_Type_Mappings_Reference_Mappings (N)));
      W_Node_Attribute
        ("Error_Behavior_State_Machine_Reference",
         "Node_Id",
         Image (Error_Behavior_State_Machine_Reference (N)),
         Int (Error_Behavior_State_Machine_Reference (N)));
      W_Node_Attribute
        ("Error_Propagations",
         "Node_Id",
         Image (Error_Propagations (N)),
         Int (Error_Propagations (N)));
      W_Node_Attribute
        ("Component_Error_Behavior",
         "Node_Id",
         Image (Component_Error_Behavior (N)),
         Int (Component_Error_Behavior (N)));
      W_Node_Attribute
        ("Composite_Error_Behavior",
         "List_Id",
         Image (Composite_Error_Behavior (N)),
         Int (Composite_Error_Behavior (N)));
      W_Node_Attribute
        ("Connection_Error_Behavior",
         "Node_Id",
         Image (Connection_Error_Behavior (N)),
         Int (Connection_Error_Behavior (N)));
      W_Node_Attribute
        ("Propagation_Paths",
         "Node_Id",
         Image (Propagation_Paths (N)),
         Int (Propagation_Paths (N)));
      W_Node_Attribute
        ("EMV2_Properties_Section",
         "List_Id",
         Image (EMV2_Properties_Section (N)),
         Int (EMV2_Properties_Section (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
   end W_Annex_Subclause;

   procedure W_Emv2_Contained_Property_Association (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Emv2_Containment_Path_List",
         "List_Id",
         Image (Emv2_Containment_Path_List (N)),
         Int (Emv2_Containment_Path_List (N)));
      W_Node_Attribute
        ("Assignment",
         "Node_Id",
         Image (Assignment (N)),
         Int (Assignment (N)));
      W_Node_Attribute
        ("Is_Constant",
         "Boolean",
         Image (Is_Constant (N)));
      W_Node_Attribute
        ("Property_Identifier",
         "Node_Id",
         Image (Property_Identifier (N)),
         Int (Property_Identifier (N)));
   end W_Emv2_Contained_Property_Association;

   procedure W_Emv2_Containment_Path (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Aadl2_Core_Path_List",
         "List_Id",
         Image (Aadl2_Core_Path_List (N)),
         Int (Aadl2_Core_Path_List (N)));
      W_Node_Attribute
        ("Emv2_Annex_Specific_Path_List",
         "List_Id",
         Image (Emv2_Annex_Specific_Path_List (N)),
         Int (Emv2_Annex_Specific_Path_List (N)));
   end W_Emv2_Containment_Path;

   procedure W_Property_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Property_Set",
         "Node_Id",
         Image (Property_Set (N)),
         Int (Property_Set (N)));
      W_Node_Attribute
        ("Property_Name",
         "Node_Id",
         Image (Property_Name (N)),
         Int (Property_Name (N)));
   end W_Property_Identifier;

   procedure W_Assignment (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Prop_Value",
         "Node_Id",
         Image (Prop_Value (N)),
         Int (Prop_Value (N)));
      W_Node_Attribute
        ("Prop_Value_List",
         "List_Id",
         Image (Prop_Value_List (N)),
         Int (Prop_Value_List (N)));
      W_Node_Attribute
        ("Prop_Name_List",
         "List_Id",
         Image (Prop_Name_List (N)),
         Int (Prop_Name_List (N)));
      W_Node_Attribute
        ("In_Modes_Properties",
         "List_Id",
         Image (In_Modes_Properties (N)),
         Int (In_Modes_Properties (N)));
   end W_Assignment;

   procedure W_Property_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("List_Of_Strings",
         "List_Id",
         Image (List_Of_Strings (N)),
         Int (List_Of_Strings (N)));
   end W_Property_Value;

   procedure W_AADL_Integer (N : Node_Id) is
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
   end W_AADL_Integer;

   procedure W_AADL_Real (N : Node_Id) is
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
   end W_AADL_Real;

   procedure W_Numeric_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Number_Value",
         "Node_Id",
         Image (Number_Value (N)),
         Int (Number_Value (N)));
      W_Node_Attribute
        ("Unit_Identifier",
         "Node_Id",
         Image (Unit_Identifier (N)),
         Int (Unit_Identifier (N)));
   end W_Numeric_Term;

   procedure W_Range_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Lower_Bound",
         "Node_Id",
         Image (Lower_Bound (N)),
         Int (Lower_Bound (N)));
      W_Node_Attribute
        ("Upper_Bound",
         "Node_Id",
         Image (Upper_Bound (N)),
         Int (Upper_Bound (N)));
   end W_Range_Type;

   procedure W_Error_Type_Mappings_Reference (N : Node_Id) is
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
        ("Package_Reference",
         "Node_Id",
         Image (Package_Reference (N)),
         Int (Package_Reference (N)));
   end W_Error_Type_Mappings_Reference;

   procedure W_Error_Behavior_State_Machine_Reference (N : Node_Id) is
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
        ("Error_Model_Library_Reference",
         "Node_Id",
         Image (Error_Model_Library_Reference (N)),
         Int (Error_Model_Library_Reference (N)));
   end W_Error_Behavior_State_Machine_Reference;

   procedure W_Error_Propagations (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Propagation_List",
         "List_Id",
         Image (Error_Propagation_List (N)),
         Int (Error_Propagation_List (N)));
      W_Node_Attribute
        ("Error_Containment_List",
         "List_Id",
         Image (Error_Containment_List (N)),
         Int (Error_Containment_List (N)));
      W_Node_Attribute
        ("Error_Flow_List",
         "List_Id",
         Image (Error_Flow_List (N)),
         Int (Error_Flow_List (N)));
   end W_Error_Propagations;

   procedure W_Error_Propagations_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Error_Propagation_Point",
         "Node_Id",
         Image (Error_Propagation_Point (N)),
         Int (Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Propagation",
         "Node_Id",
         Image (Propagation (N)),
         Int (Propagation (N)));
   end W_Error_Propagations_Element;

   procedure W_Propagation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Propagation_Kind",
         "Byte",
         Image (Propagation_Kind (N)));
   end W_Propagation;

   procedure W_Error_Propagation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Error_Propagation_Point",
         "Node_Id",
         Image (Error_Propagation_Point (N)),
         Int (Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Propagation",
         "Node_Id",
         Image (Propagation (N)),
         Int (Propagation (N)));
   end W_Error_Propagation;

   procedure W_Error_Containment (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Error_Propagation_Point",
         "Node_Id",
         Image (Error_Propagation_Point (N)),
         Int (Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Propagation",
         "Node_Id",
         Image (Propagation (N)),
         Int (Propagation (N)));
   end W_Error_Containment;

   procedure W_Error_Flow (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Source",
         "Node_Id",
         Image (Error_Source (N)),
         Int (Error_Source (N)));
      W_Node_Attribute
        ("Error_Sink",
         "Node_Id",
         Image (Error_Sink (N)),
         Int (Error_Sink (N)));
      W_Node_Attribute
        ("Error_Path",
         "Node_Id",
         Image (Error_Path (N)),
         Int (Error_Path (N)));
   end W_Error_Flow;

   procedure W_Fault_Source (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Error_Behavior_State",
         "Node_Id",
         Image (Error_Behavior_State (N)),
         Int (Error_Behavior_State (N)));
      W_Node_Attribute
        ("Failure_Mode_Description",
         "Node_Id",
         Image (Failure_Mode_Description (N)),
         Int (Failure_Mode_Description (N)));
   end W_Fault_Source;

   procedure W_Error_Sink (N : Node_Id) is
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
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Incoming_Error_Propagation_Point",
         "Node_Id",
         Image (Incoming_Error_Propagation_Point (N)),
         Int (Incoming_Error_Propagation_Point (N)));
   end W_Error_Sink;

   procedure W_Error_Path (N : Node_Id) is
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
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Target_Error_Type_Instance",
         "Node_Id",
         Image (Target_Error_Type_Instance (N)),
         Int (Target_Error_Type_Instance (N)));
      W_Node_Attribute
        ("Incoming_Error_Propagation_Point",
         "Node_Id",
         Image (Incoming_Error_Propagation_Point (N)),
         Int (Incoming_Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Outgoing_Error_Propagation_Point",
         "Node_Id",
         Image (Outgoing_Error_Propagation_Point (N)),
         Int (Outgoing_Error_Propagation_Point (N)));
   end W_Error_Path;

   procedure W_Component_Error_Behavior (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Transformation_Set_Reference",
         "Node_Id",
         Image (Error_Type_Transformation_Set_Reference (N)),
         Int (Error_Type_Transformation_Set_Reference (N)));
      W_Node_Attribute
        ("Error_Behavior_Event_List",
         "List_Id",
         Image (Error_Behavior_Event_List (N)),
         Int (Error_Behavior_Event_List (N)));
      W_Node_Attribute
        ("Component_Specific_Error_Behavior_Transition_List",
         "List_Id",
         Image (Component_Specific_Error_Behavior_Transition_List (N)),
         Int (Component_Specific_Error_Behavior_Transition_List (N)));
      W_Node_Attribute
        ("Outgoing_Propagation_Condition_List",
         "List_Id",
         Image (Outgoing_Propagation_Condition_List (N)),
         Int (Outgoing_Propagation_Condition_List (N)));
      W_Node_Attribute
        ("Error_Detection_List",
         "List_Id",
         Image (Error_Detection_List (N)),
         Int (Error_Detection_List (N)));
      W_Node_Attribute
        ("Error_State_To_Mode_Mapping_List",
         "List_Id",
         Image (Error_State_To_Mode_Mapping_List (N)),
         Int (Error_State_To_Mode_Mapping_List (N)));
   end W_Component_Error_Behavior;

   procedure W_Component_Error_Behavior_Node (N : Node_Id) is
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
        ("Error_Source_State",
         "Node_Id",
         Image (Error_Source_State (N)),
         Int (Error_Source_State (N)));
      W_Node_Attribute
        ("Error_Condition",
         "Node_Id",
         Image (Error_Condition (N)),
         Int (Error_Condition (N)));
   end W_Component_Error_Behavior_Node;

   procedure W_Outgoing_Propagation_Condition (N : Node_Id) is
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
        ("Error_Source_State",
         "Node_Id",
         Image (Error_Source_State (N)),
         Int (Error_Source_State (N)));
      W_Node_Attribute
        ("Error_Condition",
         "Node_Id",
         Image (Error_Condition (N)),
         Int (Error_Condition (N)));
      W_Node_Attribute
        ("Propagation_Target",
         "Node_Id",
         Image (Propagation_Target (N)),
         Int (Propagation_Target (N)));
   end W_Outgoing_Propagation_Condition;

   procedure W_Propagation_Target (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Propagation_Point",
         "Node_Id",
         Image (Error_Propagation_Point (N)),
         Int (Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Propagated_Target_Error_Type_Instance",
         "Node_Id",
         Image (Propagated_Target_Error_Type_Instance (N)),
         Int (Propagated_Target_Error_Type_Instance (N)));
   end W_Propagation_Target;

   procedure W_Error_Detection (N : Node_Id) is
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
        ("Error_Source_State",
         "Node_Id",
         Image (Error_Source_State (N)),
         Int (Error_Source_State (N)));
      W_Node_Attribute
        ("Error_Condition",
         "Node_Id",
         Image (Error_Condition (N)),
         Int (Error_Condition (N)));
      W_Node_Attribute
        ("Error_Detection_Effect",
         "Node_Id",
         Image (Error_Detection_Effect (N)),
         Int (Error_Detection_Effect (N)));
   end W_Error_Detection;

   procedure W_Error_Detection_Effect (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Outgoing_Port_Reference",
         "Node_Id",
         Image (Outgoing_Port_Reference (N)),
         Int (Outgoing_Port_Reference (N)));
      W_Node_Attribute
        ("Internal_Event_Reference",
         "Node_Id",
         Image (Internal_Event_Reference (N)),
         Int (Internal_Event_Reference (N)));
      W_Node_Attribute
        ("Error_Code_Value",
         "Node_Id",
         Image (Error_Code_Value (N)),
         Int (Error_Code_Value (N)));
   end W_Error_Detection_Effect;

   procedure W_Internal_Event_Reference (N : Node_Id) is
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
   end W_Internal_Event_Reference;

   procedure W_Error_Code_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Enumeration_Identifier",
         "Node_Id",
         Image (Enumeration_Identifier (N)),
         Int (Enumeration_Identifier (N)));
      W_Node_Attribute
        ("Property_Constant_Term",
         "Node_Id",
         Image (Property_Constant_Term (N)),
         Int (Property_Constant_Term (N)));
   end W_Error_Code_Value;

   procedure W_Property_Constant_Term (N : Node_Id) is
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
   end W_Property_Constant_Term;

   procedure W_Error_State_To_Mode_Mapping (N : Node_Id) is
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
        ("Target_Error_Type_Instance",
         "Node_Id",
         Image (Target_Error_Type_Instance (N)),
         Int (Target_Error_Type_Instance (N)));
      W_Node_Attribute
        ("Mode_Name_List",
         "List_Id",
         Image (Mode_Name_List (N)),
         Int (Mode_Name_List (N)));
   end W_Error_State_To_Mode_Mapping;

   procedure W_Composite_Error_State (N : Node_Id) is
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
        ("Target_Error_Type_Instance",
         "Node_Id",
         Image (Target_Error_Type_Instance (N)),
         Int (Target_Error_Type_Instance (N)));
      W_Node_Attribute
        ("Composite_State_Identifier",
         "Node_Id",
         Image (Composite_State_Identifier (N)),
         Int (Composite_State_Identifier (N)));
      W_Node_Attribute
        ("Subcomponent_State_Expression",
         "Node_Id",
         Image (Subcomponent_State_Expression (N)),
         Int (Subcomponent_State_Expression (N)));
   end W_Composite_Error_State;

   procedure W_Composite_State_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Numeric_Literal",
         "Node_Id",
         Image (Numeric_Literal (N)),
         Int (Numeric_Literal (N)));
      W_Node_Attribute
        ("Operator",
         "Node_Id",
         Image (Operator (N)),
         Int (Operator (N)));
      W_Node_Attribute
        ("Composite_State_Element",
         "Node_Id",
         Image (Composite_State_Element (N)),
         Int (Composite_State_Element (N)));
      W_Node_Attribute
        ("Composite_State_Expression_Node",
         "Node_Id",
         Image (Composite_State_Expression_Node (N)),
         Int (Composite_State_Expression_Node (N)));
      W_Node_Attribute
        ("Composite_State_Element_List",
         "List_Id",
         Image (Composite_State_Element_List (N)),
         Int (Composite_State_Element_List (N)));
   end W_Composite_State_Expression;

   procedure W_Composite_State_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Set",
         "List_Id",
         Image (Error_Type_Set (N)),
         Int (Error_Type_Set (N)));
      W_Node_Attribute
        ("Incoming_Error_Propagation_Point",
         "Node_Id",
         Image (Incoming_Error_Propagation_Point (N)),
         Int (Incoming_Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Error_Type_Set_Or_Noerror",
         "Node_Id",
         Image (Error_Type_Set_Or_Noerror (N)),
         Int (Error_Type_Set_Or_Noerror (N)));
      W_Node_Attribute
        ("Subcomponent_Error_State",
         "Node_Id",
         Image (Subcomponent_Error_State (N)),
         Int (Subcomponent_Error_State (N)));
   end W_Composite_State_Element;

   procedure W_Subcomponent_Error_State (N : Node_Id) is
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
        ("Subcomponent_Identifier_List",
         "List_Id",
         Image (Subcomponent_Identifier_List (N)),
         Int (Subcomponent_Identifier_List (N)));
   end W_Subcomponent_Error_State;

   procedure W_Connection_Error_Behavior (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Error_Type_Transformation_Set_Reference",
         "Node_Id",
         Image (Error_Type_Transformation_Set_Reference (N)),
         Int (Error_Type_Transformation_Set_Reference (N)));
      W_Node_Attribute
        ("Connection_Error_Source_List",
         "List_Id",
         Image (Connection_Error_Source_List (N)),
         Int (Connection_Error_Source_List (N)));
   end W_Connection_Error_Behavior;

   procedure W_Error_Source_Parent (N : Node_Id) is
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
        ("Effect_Error_Type_Set",
         "List_Id",
         Image (Effect_Error_Type_Set (N)),
         Int (Effect_Error_Type_Set (N)));
      W_Node_Attribute
        ("Fault_Source",
         "Node_Id",
         Image (Fault_Source (N)),
         Int (Fault_Source (N)));
      W_Node_Attribute
        ("Fault_Condition",
         "Node_Id",
         Image (Fault_Condition (N)),
         Int (Fault_Condition (N)));
   end W_Error_Source_Parent;

   procedure W_Error_Source (N : Node_Id) is
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
        ("Outgoing_Error_Propagation_Point",
         "Node_Id",
         Image (Outgoing_Error_Propagation_Point (N)),
         Int (Outgoing_Error_Propagation_Point (N)));
      W_Node_Attribute
        ("Effect_Error_Type_Set",
         "List_Id",
         Image (Effect_Error_Type_Set (N)),
         Int (Effect_Error_Type_Set (N)));
      W_Node_Attribute
        ("Fault_Source",
         "Node_Id",
         Image (Fault_Source (N)),
         Int (Fault_Source (N)));
      W_Node_Attribute
        ("Fault_Condition",
         "Node_Id",
         Image (Fault_Condition (N)),
         Int (Fault_Condition (N)));
   end W_Error_Source;

   procedure W_Connection_Error_Source (N : Node_Id) is
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
        ("Failure_Mode_Description",
         "Node_Id",
         Image (Failure_Mode_Description (N)),
         Int (Failure_Mode_Description (N)));
      W_Node_Attribute
        ("Effect_Error_Type_Set",
         "List_Id",
         Image (Effect_Error_Type_Set (N)),
         Int (Effect_Error_Type_Set (N)));
      W_Node_Attribute
        ("Fault_Condition",
         "Node_Id",
         Image (Fault_Condition (N)),
         Int (Fault_Condition (N)));
      W_Node_Attribute
        ("Connection_Identifier",
         "Node_Id",
         Image (Connection_Identifier (N)),
         Int (Connection_Identifier (N)));
      W_Node_Attribute
        ("Fault_Source_Error_Type_Set",
         "List_Id",
         Image (Fault_Source_Error_Type_Set (N)),
         Int (Fault_Source_Error_Type_Set (N)));
   end W_Connection_Error_Source;

   procedure W_Propagation_Paths (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Propagation_Point_List",
         "List_Id",
         Image (Propagation_Point_List (N)),
         Int (Propagation_Point_List (N)));
      W_Node_Attribute
        ("Propagation_Path_List",
         "List_Id",
         Image (Propagation_Path_List (N)),
         Int (Propagation_Path_List (N)));
   end W_Propagation_Paths;

   procedure W_Propagation_Point (N : Node_Id) is
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
   end W_Propagation_Point;

   procedure W_Propagation_Path (N : Node_Id) is
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
        ("Source_Qualified_Propagation_Point",
         "Node_Id",
         Image (Source_Qualified_Propagation_Point (N)),
         Int (Source_Qualified_Propagation_Point (N)));
      W_Node_Attribute
        ("Target_Qualified_Propagation_Point",
         "Node_Id",
         Image (Target_Qualified_Propagation_Point (N)),
         Int (Target_Qualified_Propagation_Point (N)));
   end W_Propagation_Path;

   procedure W_Qualified_Propagation_Point (N : Node_Id) is
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
        ("Subcomponent_Identifier_List",
         "List_Id",
         Image (Subcomponent_Identifier_List (N)),
         Int (Subcomponent_Identifier_List (N)));
   end W_Qualified_Propagation_Point;

end Ocarina.ME_AADL_EMA.EMA_Tree.Nodes;
