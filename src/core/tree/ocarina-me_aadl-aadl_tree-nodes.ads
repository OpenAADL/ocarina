pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.ME_AADL.AADL_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_List_Id,
      K_Invalid_Node,
      K_Scope_Definition,
      K_Identifier,
      K_AADL_Entity,
      K_Named_AADL_Entity,
      K_AADL_Declaration,
      K_Entity_Reference,
      K_Pair_Of_Entity_References,
      K_Identifiers_List,
      K_AADL_Specification,
      K_AADL_Declarations_List,
      K_Package_Name,
      K_Package_Specification,
      K_Name_Visibility_Declaration,
      K_Import_Declaration,
      K_Alias_Declaration,
      K_Component_Category,
      K_Component_Type,
      K_Component_Implementation,
      K_Contained_Entity,
      K_Subclause,
      K_Prototype,
      K_Binding_Prototype,
      K_Feature,
      K_Refinable_Feature,
      K_Port_Spec,
      K_Feature_Group_Spec,
      K_Subprogram_Spec,
      K_Parameter,
      K_Subcomponent_Access,
      K_Flow_Spec,
      K_Mode,
      K_Mode_Transition,
      K_In_Modes,
      K_Mode_Transition_Trigger,
      K_Flow_Implementation,
      K_End_To_End_Flow_Spec,
      K_Flow_Implementation_Refinement,
      K_End_To_End_Flow_Refinement,
      K_Subprogram_Call,
      K_Subprogram_Call_Sequence,
      K_Subcomponent,
      K_Feature_Group_Type,
      K_Connection,
      K_Property_Set,
      K_Contained_Element_Path,
      K_Property_Type,
      K_Property_Type_Declaration,
      K_Single_Valued_Property,
      K_Multi_Valued_Property,
      K_Constant_Property_Declaration,
      K_Property_Value,
      K_Property_Definition_Declaration,
      K_Property_List_Value,
      K_In_Binding,
      K_Property_Association,
      K_Named_Element,
      K_Literal,
      K_Signed_AADLNumber,
      K_Not_Boolean_Term,
      K_And_Boolean_Term,
      K_Or_Boolean_Term,
      K_Parenthesis_Boolean_Term,
      K_Minus_Numeric_Term,
      K_Property_Term,
      K_Enumeration_Term,
      K_Unit_Term,
      K_Number_Range_Term,
      K_Component_Classifier_Term,
      K_Reference_Term,
      K_Record_Term,
      K_Record_Term_Element,
      K_Computed_Term,
      K_Boolean_Type,
      K_String_Type,
      K_Real_Type,
      K_Integer_Type,
      K_Enumeration_Type,
      K_Number_Range,
      K_Unit_Definition,
      K_Units_Type,
      K_Range_Type,
      K_Classifier_Type,
      K_Classifier_Category_Ref,
      K_Referable_Element_Category,
      K_Reference_Type,
      K_Reference_Category,
      K_Record_Type,
      K_Record_Type_Element,
      K_Unique_Property_Type_Identifier,
      K_Applies_To,
      K_Unique_Property_Const_Identifier,
      K_Annex_Content,
      K_Annex_Subclause,
      K_Annex_Library,
      K_Annex_Path,
      K_Array_Dimensions,
      K_Array_Dimension_Size,
      K_Array_Selection,
      K_Range_Selection,
      K_Node_Container);

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
   --  Invalid_Node
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Invalid_Node (N : Node_Id);

   --
   --  Scope_Definition
   --
   --    Next_Node                : Node_Id
   --    Corresponding_Entity     : Node_Id
   --

   procedure W_Scope_Definition (N : Node_Id);

   --
   --  Identifier
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Display_Name             : Name_Id
   --    Corresponding_Entity     : Node_Id
   --    Scope_Entity             : Node_Id
   --    Homonym                  : Node_Id
   --    Visible                  : Boolean
   --    Backend_Node             : Node_Id
   --

   procedure W_Identifier (N : Node_Id);

   --
   --  AADL_Entity
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --

   procedure W_AADL_Entity (N : Node_Id);

   --
   --  Named_AADL_Entity
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Named_AADL_Entity (N : Node_Id);

   --
   --  AADL_Declaration
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Entity_Scope             : Node_Id
   --    Property_Scope           : Node_Id
   --    Is_Private               : Boolean
   --    First_Visited_Node       : Node_Id
   --    Namespace                : Node_Id
   --    Default_Instance         : Node_Id
   --

   procedure W_AADL_Declaration (N : Node_Id);

   --
   --  Entity_Reference
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Path                     : List_Id
   --    Namespace_Path           : List_Id
   --    Namespace_Identifier     : Node_Id
   --    Full_Identifier          : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Entity_Reference (N : Node_Id);

   --
   --  Pair_Of_Entity_References
   --
   --    Next_Node                : Node_Id
   --    First_Reference          : Node_Id
   --    Second_Reference         : Node_Id
   --

   procedure W_Pair_Of_Entity_References (N : Node_Id);

   --
   --  Identifiers_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Identifiers_List (N : List_Id);

   --
   --  AADL_Specification
   --
   --    Next_Node                : Node_Id
   --    Declarations             : List_Id
   --    Entity_Scope             : Node_Id
   --

   procedure W_AADL_Specification (N : Node_Id);

   --
   --  AADL_Declarations_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_AADL_Declarations_List (N : List_Id);

   --
   --  Package_Name
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifiers              : List_Id
   --

   procedure W_Package_Name (N : Node_Id);

   --
   --  Package_Specification
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Has_Private_Part         : Boolean
   --    Has_Public_Part          : Boolean
   --    Entity_Scope             : Node_Id
   --    Declarations             : List_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Package_Name             : Node_Id
   --    Parent                   : Node_Id
   --    Annexes                  : List_Id
   --

   procedure W_Package_Specification (N : Node_Id);

   --
   --  Name_Visibility_Declaration
   --
   --    Next_Node                : Node_Id
   --    Is_Private               : Boolean
   --    Parent                   : Node_Id
   --    List_Items               : List_Id
   --

   procedure W_Name_Visibility_Declaration (N : Node_Id);

   --
   --  Import_Declaration
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Is_Private               : Boolean
   --    List_Items               : List_Id
   --

   procedure W_Import_Declaration (N : Node_Id);

   --
   --  Alias_Declaration
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Is_Private               : Boolean
   --    Is_All                   : Boolean
   --    Category                 : Byte
   --    Entity_Category          : Byte
   --    Package_Name             : Node_Id
   --    Reference                : Node_Id
   --    Renamed_Entity           : Node_Id
   --    Parent                   : Node_Id
   --

   procedure W_Alias_Declaration (N : Node_Id);

   --
   --  Component_Category
   --
   --    Next_Node                : Node_Id
   --    Category                 : Byte
   --

   procedure W_Component_Category (N : Node_Id);

   --
   --  Component_Type
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Entity_Scope             : Node_Id
   --    Property_Scope           : Node_Id
   --    Is_Private               : Boolean
   --    First_Visited_Node       : Node_Id
   --    Namespace                : Node_Id
   --    Default_Instance         : Node_Id
   --    Category                 : Byte
   --    Features                 : List_Id
   --    Flows                    : List_Id
   --    Properties               : List_Id
   --    Annexes                  : List_Id
   --    Parent                   : Node_Id
   --    Instances                : List_Id
   --    Modes                    : List_Id
   --    Prototypes               : List_Id
   --    Prototype_Bindings       : List_Id
   --

   procedure W_Component_Type (N : Node_Id);

   --
   --  Component_Implementation
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Entity_Scope             : Node_Id
   --    Property_Scope           : Node_Id
   --    Is_Private               : Boolean
   --    First_Visited_Node       : Node_Id
   --    Namespace                : Node_Id
   --    Default_Instance         : Node_Id
   --    Component_Type_Identifier: Node_Id
   --    Category                 : Byte
   --    Refines_Type             : List_Id
   --    Subcomponents            : List_Id
   --    Calls                    : List_Id
   --    Connections              : List_Id
   --    Flows                    : List_Id
   --    Modes                    : List_Id
   --    Properties               : List_Id
   --    Annexes                  : List_Id
   --    Parent                   : Node_Id
   --    Instances                : List_Id
   --    Prototypes               : List_Id
   --    Prototype_Bindings       : List_Id
   --

   procedure W_Component_Implementation (N : Node_Id);

   --
   --  Contained_Entity
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --

   procedure W_Contained_Entity (N : Node_Id);

   --
   --  Subclause
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --

   procedure W_Subclause (N : Node_Id);

   --
   --  Prototype
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Category                 : Byte
   --    Is_Refinement            : Boolean
   --

   procedure W_Prototype (N : Node_Id);

   --
   --  Binding_Prototype
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Category                 : Byte
   --    Entity_Ref               : Node_Id
   --

   procedure W_Binding_Prototype (N : Node_Id);

   --
   --  Feature
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --

   procedure W_Feature (N : Node_Id);

   --
   --  Refinable_Feature
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --

   procedure W_Refinable_Feature (N : Node_Id);

   --
   --  Port_Spec
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Is_In                    : Boolean
   --    Is_Out                   : Boolean
   --    Is_Feature               : Boolean
   --    Is_Event                 : Boolean
   --    Is_Data                  : Boolean
   --    Array_Dimensions         : Node_Id
   --

   procedure W_Port_Spec (N : Node_Id);

   --
   --  Feature_Group_Spec
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Inverse_Of               : Node_Id
   --

   procedure W_Feature_Group_Spec (N : Node_Id);

   --
   --  Subprogram_Spec
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Is_Server                : Boolean
   --

   procedure W_Subprogram_Spec (N : Node_Id);

   --
   --  Parameter
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Is_In                    : Boolean
   --    Is_Out                   : Boolean
   --

   procedure W_Parameter (N : Node_Id);

   --
   --  Subcomponent_Access
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Is_Provided              : Boolean
   --    Subcomponent_Category    : Byte
   --

   procedure W_Subcomponent_Access (N : Node_Id);

   --
   --  Flow_Spec
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Category                 : Byte
   --    Source_Flow              : Node_Id
   --    Sink_Flow                : Node_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Flow_Spec (N : Node_Id);

   --
   --  Mode
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Is_Requires              : Boolean
   --    Is_Initial               : Boolean
   --

   procedure W_Mode (N : Node_Id);

   --
   --  Mode_Transition
   --
   --    Next_Node                : Node_Id
   --    Source_Modes             : List_Id
   --    Triggers                 : List_Id
   --    Properties               : List_Id
   --    Destination_Mode         : Node_Id
   --    Container_Component      : Node_Id
   --

   procedure W_Mode_Transition (N : Node_Id);

   --
   --  In_Modes
   --
   --    Next_Node                : Node_Id
   --    Modes                    : List_Id
   --

   procedure W_In_Modes (N : Node_Id);

   --
   --  Mode_Transition_Trigger
   --
   --    Next_Node                : Node_Id
   --    Is_Self                  : Boolean
   --    Is_Processor             : Boolean
   --    Identifier               : Node_Id
   --

   procedure W_Mode_Transition_Trigger (N : Node_Id);

   --
   --  Flow_Implementation
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Category                 : Byte
   --    Source_Flow              : Node_Id
   --    Sink_Flow                : Node_Id
   --    Connections              : List_Id
   --    Corresponding_Flow_Spec  : Node_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Flow_Implementation (N : Node_Id);

   --
   --  End_To_End_Flow_Spec
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Source_Flow              : Node_Id
   --    Sink_Flow                : Node_Id
   --    Connections              : List_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_End_To_End_Flow_Spec (N : Node_Id);

   --
   --  Flow_Implementation_Refinement
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Category                 : Byte
   --    In_Modes                 : Node_Id
   --

   procedure W_Flow_Implementation_Refinement (N : Node_Id);

   --
   --  End_To_End_Flow_Refinement
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_End_To_End_Flow_Refinement (N : Node_Id);

   --
   --  Subprogram_Call
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    In_Modes                 : Node_Id
   --    Parent_Sequence          : Node_Id
   --

   procedure W_Subprogram_Call (N : Node_Id);

   --
   --  Subprogram_Call_Sequence
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Subprogram_Calls         : List_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Subprogram_Call_Sequence (N : Node_Id);

   --
   --  Subcomponent
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Implicit_Inverse      : Boolean
   --    Inversed_Entity          : Node_Id
   --    Is_Refinement            : Boolean
   --    Category                 : Byte
   --    In_Modes                 : Node_Id
   --    Array_Dimensions         : Node_Id
   --    Prototype_Bindings       : List_Id
   --

   procedure W_Subcomponent (N : Node_Id);

   --
   --  Feature_Group_Type
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Entity_Scope             : Node_Id
   --    Property_Scope           : Node_Id
   --    Is_Private               : Boolean
   --    First_Visited_Node       : Node_Id
   --    Namespace                : Node_Id
   --    Default_Instance         : Node_Id
   --    Features                 : List_Id
   --    Inverse_Of               : Node_Id
   --    Properties               : List_Id
   --    Annexes                  : List_Id
   --    Parent                   : Node_Id
   --    Prototypes               : List_Id
   --    Prototype_Bindings       : List_Id
   --

   procedure W_Feature_Group_Type (N : Node_Id);

   --
   --  Connection
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Property_Scope           : Node_Id
   --    Properties               : List_Id
   --    Entity_Ref               : Node_Id
   --    Is_Refinement            : Boolean
   --    Is_Bidirectional         : Boolean
   --    Category                 : Byte
   --    Source                   : Node_Id
   --    Destination              : Node_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Connection (N : Node_Id);

   --
   --  Property_Set
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Declarations             : List_Id
   --    Entity_Scope             : Node_Id
   --    Property_Set_Context     : Node_Id
   --    Imports_List             : List_Id
   --

   procedure W_Property_Set (N : Node_Id);

   --
   --  Contained_Element_Path
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    List_Items               : List_Id
   --    Annex_Path               : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Contained_Element_Path (N : Node_Id);

   --
   --  Property_Type
   --
   --    Next_Node                : Node_Id
   --    Is_List                  : Boolean
   --    Multiplicity             : Int
   --    Property_Type_Designator : Node_Id
   --    Expanded_Type_Designator : Node_Id
   --

   procedure W_Property_Type (N : Node_Id);

   --
   --  Property_Type_Declaration
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Type_Designator : Node_Id
   --

   procedure W_Property_Type_Declaration (N : Node_Id);

   --
   --  Single_Valued_Property
   --
   --    Next_Node                : Node_Id
   --    Property_Type_Designator : Node_Id
   --    Property_Expression      : Node_Id
   --    Parent                   : Node_Id
   --

   procedure W_Single_Valued_Property (N : Node_Id);

   --
   --  Multi_Valued_Property
   --
   --    Next_Node                : Node_Id
   --    Property_Type_Designator : Node_Id
   --    Property_Expressions     : List_Id
   --    Multiplicity             : Int
   --

   procedure W_Multi_Valued_Property (N : Node_Id);

   --
   --  Constant_Property_Declaration
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Constant_Type            : Node_Id
   --    Unique_Unit_Identifier   : Node_Id
   --    Constant_Value           : Node_Id
   --    Multiplicity             : Int
   --

   procedure W_Constant_Property_Declaration (N : Node_Id);

   --
   --  Property_Value
   --
   --    Next_Node                : Node_Id
   --    Value_Container          : Node_Id
   --    Single_Value             : Node_Id
   --    Multi_Value              : List_Id
   --    Expanded_Single_Value    : Node_Id
   --    Expanded_Multi_Value     : List_Id
   --

   procedure W_Property_Value (N : Node_Id);

   --
   --  Property_Definition_Declaration
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Is_Access                : Boolean
   --    Is_Inherit               : Boolean
   --    Property_Name_Type       : Node_Id
   --    Default_Value            : Node_Id
   --    Applies_To               : Node_Id
   --

   procedure W_Property_Definition_Declaration (N : Node_Id);

   --
   --  Property_List_Value
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Property_List_Value (N : List_Id);

   --
   --  In_Binding
   --
   --    Next_Node                : Node_Id
   --    Binding                  : List_Id
   --

   procedure W_In_Binding (N : Node_Id);

   --
   --  Property_Association
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Name            : Node_Id
   --    Is_Additive_Association  : Boolean
   --    Is_Constant              : Boolean
   --    Is_Private               : Boolean
   --    Is_Access                : Boolean
   --    Property_Association_Type: Node_Id
   --    Property_Association_Value: Node_Id
   --    Applies_To_Prop          : List_Id
   --    In_Binding               : Node_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Property_Association (N : Node_Id);

   --
   --  Named_Element
   --
   --    Next_Node                : Node_Id
   --    Category                 : Byte
   --    Component_Cat            : Byte
   --    Classifier_Ref           : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Named_Element (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  Signed_AADLNumber
   --
   --    Next_Node                : Node_Id
   --    Number_Value             : Node_Id
   --    Unit_Identifier          : Node_Id
   --

   procedure W_Signed_AADLNumber (N : Node_Id);

   --
   --  Not_Boolean_Term
   --
   --    Next_Node                : Node_Id
   --    Boolean_Term             : Node_Id
   --

   procedure W_Not_Boolean_Term (N : Node_Id);

   --
   --  And_Boolean_Term
   --
   --    Next_Node                : Node_Id
   --    First_Term               : Node_Id
   --    Second_Term              : Node_Id
   --

   procedure W_And_Boolean_Term (N : Node_Id);

   --
   --  Or_Boolean_Term
   --
   --    Next_Node                : Node_Id
   --    First_Term               : Node_Id
   --    Second_Term              : Node_Id
   --

   procedure W_Or_Boolean_Term (N : Node_Id);

   --
   --  Parenthesis_Boolean_Term
   --
   --    Next_Node                : Node_Id
   --    Boolean_Term             : Node_Id
   --

   procedure W_Parenthesis_Boolean_Term (N : Node_Id);

   --
   --  Minus_Numeric_Term
   --
   --    Next_Node                : Node_Id
   --    Numeric_Term             : Node_Id
   --

   procedure W_Minus_Numeric_Term (N : Node_Id);

   --
   --  Property_Term
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set_Identifier  : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Property_Term (N : Node_Id);

   --
   --  Enumeration_Term
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set_Identifier  : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Enumeration_Term (N : Node_Id);

   --
   --  Unit_Term
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set_Identifier  : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Unit_Term (N : Node_Id);

   --
   --  Number_Range_Term
   --
   --    Next_Node                : Node_Id
   --    Lower_Bound              : Node_Id
   --    Upper_Bound              : Node_Id
   --    Delta_Term               : Node_Id
   --

   procedure W_Number_Range_Term (N : Node_Id);

   --
   --  Component_Classifier_Term
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Path                     : List_Id
   --    Namespace_Path           : List_Id
   --    Namespace_Identifier     : Node_Id
   --    Full_Identifier          : Node_Id
   --    Entity                   : Node_Id
   --    Component_Cat            : Byte
   --

   procedure W_Component_Classifier_Term (N : Node_Id);

   --
   --  Reference_Term
   --
   --    Next_Node                : Node_Id
   --    Reference_Term           : Node_Id
   --

   procedure W_Reference_Term (N : Node_Id);

   --
   --  Record_Term
   --
   --    Next_Node                : Node_Id
   --    List_Items               : List_Id
   --

   procedure W_Record_Term (N : Node_Id);

   --
   --  Record_Term_Element
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Property_Expression      : Node_Id
   --

   procedure W_Record_Term_Element (N : Node_Id);

   --
   --  Computed_Term
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Computed_Term (N : Node_Id);

   --
   --  Boolean_Type
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Boolean_Type (N : Node_Id);

   --
   --  String_Type
   --
   --    Next_Node                : Node_Id
   --

   procedure W_String_Type (N : Node_Id);

   --
   --  Real_Type
   --
   --    Next_Node                : Node_Id
   --    Type_Range               : Node_Id
   --    Unit_Designator          : Node_Id
   --

   procedure W_Real_Type (N : Node_Id);

   --
   --  Integer_Type
   --
   --    Next_Node                : Node_Id
   --    Type_Range               : Node_Id
   --    Unit_Designator          : Node_Id
   --

   procedure W_Integer_Type (N : Node_Id);

   --
   --  Enumeration_Type
   --
   --    Next_Node                : Node_Id
   --    Identifiers              : List_Id
   --    Enumeration_Context      : Node_Id
   --

   procedure W_Enumeration_Type (N : Node_Id);

   --
   --  Number_Range
   --
   --    Next_Node                : Node_Id
   --    Lower_Bound              : Node_Id
   --    Upper_Bound              : Node_Id
   --

   procedure W_Number_Range (N : Node_Id);

   --
   --  Unit_Definition
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Unit_Identifier          : Node_Id
   --    Numeric_Literal          : Node_Id
   --

   procedure W_Unit_Definition (N : Node_Id);

   --
   --  Units_Type
   --
   --    Next_Node                : Node_Id
   --    Base_Identifier          : Node_Id
   --    Unit_Definitions         : List_Id
   --    Units_Context            : Node_Id
   --

   procedure W_Units_Type (N : Node_Id);

   --
   --  Range_Type
   --
   --    Next_Node                : Node_Id
   --    Number_Type              : Node_Id
   --

   procedure W_Range_Type (N : Node_Id);

   --
   --  Classifier_Type
   --
   --    Next_Node                : Node_Id
   --    List_Items               : List_Id
   --

   procedure W_Classifier_Type (N : Node_Id);

   --
   --  Classifier_Category_Ref
   --
   --    Next_Node                : Node_Id
   --    Category                 : Byte
   --    Component_Cat            : Byte
   --    Classifier_Ref           : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Classifier_Category_Ref (N : Node_Id);

   --
   --  Referable_Element_Category
   --
   --    Next_Node                : Node_Id
   --    Component_Cat            : Byte
   --    Category                 : Byte
   --    Identifier               : Node_Id
   --

   procedure W_Referable_Element_Category (N : Node_Id);

   --
   --  Reference_Type
   --
   --    Next_Node                : Node_Id
   --    List_Items               : List_Id
   --

   procedure W_Reference_Type (N : Node_Id);

   --
   --  Reference_Category
   --
   --    Next_Node                : Node_Id
   --    Category                 : Byte
   --    Component_Cat            : Byte
   --    Classifier_Ref           : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Reference_Category (N : Node_Id);

   --
   --  Record_Type
   --
   --    Next_Node                : Node_Id
   --    List_Items               : List_Id
   --

   procedure W_Record_Type (N : Node_Id);

   --
   --  Record_Type_Element
   --
   --    Next_Node                : Node_Id
   --    Is_List                  : Boolean
   --    Identifier               : Node_Id
   --    Property_Type_Designator : Node_Id
   --

   procedure W_Record_Type_Element (N : Node_Id);

   --
   --  Unique_Property_Type_Identifier
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set_Identifier  : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Unique_Property_Type_Identifier (N : Node_Id);

   --
   --  Applies_To
   --
   --    Next_Node                : Node_Id
   --    Is_All                   : Boolean
   --    Owner_Categories         : List_Id
   --

   procedure W_Applies_To (N : Node_Id);

   --
   --  Unique_Property_Const_Identifier
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set_Identifier  : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Unique_Property_Const_Identifier (N : Node_Id);

   --
   --  Annex_Content
   --
   --    Next_Node                : Node_Id
   --    Raw_Text                 : Name_Id
   --

   procedure W_Annex_Content (N : Node_Id);

   --
   --  Annex_Subclause
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Annex_Content            : Node_Id
   --    In_Modes                 : Node_Id
   --    Corresponding_Annex      : Node_Id
   --

   procedure W_Annex_Subclause (N : Node_Id);

   --
   --  Annex_Library
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Is_Private               : Boolean
   --    Annex_Content            : Node_Id
   --    Container_Package        : Node_Id
   --    Corresponding_Annex      : Node_Id
   --

   procedure W_Annex_Library (N : Node_Id);

   --
   --  Annex_Path
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Container_Component      : Node_Id
   --    Identifiers              : List_Id
   --

   procedure W_Annex_Path (N : Node_Id);

   --
   --  Array_Dimensions
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Array_List_Dim           : List_Id
   --

   procedure W_Array_Dimensions (N : Node_Id);

   --
   --  Array_Dimension_Size
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Size                     : Node_Id
   --    Parent                   : Node_Id
   --

   procedure W_Array_Dimension_Size (N : Node_Id);

   --
   --  Array_Selection
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Range_Selections         : List_Id
   --

   procedure W_Array_Selection (N : Node_Id);

   --
   --  Range_Selection
   --
   --    Next_Node                : Node_Id
   --    Lower_Bound              : Node_Id
   --    Upper_Bound              : Node_Id
   --

   procedure W_Range_Selection (N : Node_Id);

   --
   --  Node_Container
   --
   --    Next_Node                : Node_Id
   --    Item                     : Node_Id
   --    Extra_Item               : Node_Id
   --

   procedure W_Node_Container (N : Node_Id);

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

   function Corresponding_Entity (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Display_Name (N : Node_Id) return Name_Id;
   procedure Set_Display_Name (N : Node_Id; V : Name_Id);

   function Scope_Entity (N : Node_Id) return Node_Id;
   procedure Set_Scope_Entity (N : Node_Id; V : Node_Id);

   function Homonym (N : Node_Id) return Node_Id;
   procedure Set_Homonym (N : Node_Id; V : Node_Id);

   function Visible (N : Node_Id) return Boolean;
   procedure Set_Visible (N : Node_Id; V : Boolean);

   function Backend_Node (N : Node_Id) return Node_Id;
   procedure Set_Backend_Node (N : Node_Id; V : Node_Id);

   function Next_Entity (N : Node_Id) return Node_Id;
   procedure Set_Next_Entity (N : Node_Id; V : Node_Id);

   function Identifier (N : Node_Id) return Node_Id;
   procedure Set_Identifier (N : Node_Id; V : Node_Id);

   function Entity_Scope (N : Node_Id) return Node_Id;
   procedure Set_Entity_Scope (N : Node_Id; V : Node_Id);

   function Property_Scope (N : Node_Id) return Node_Id;
   procedure Set_Property_Scope (N : Node_Id; V : Node_Id);

   function Is_Private (N : Node_Id) return Boolean;
   procedure Set_Is_Private (N : Node_Id; V : Boolean);

   function First_Visited_Node (N : Node_Id) return Node_Id;
   procedure Set_First_Visited_Node (N : Node_Id; V : Node_Id);

   function Namespace (N : Node_Id) return Node_Id;
   procedure Set_Namespace (N : Node_Id; V : Node_Id);

   function Default_Instance (N : Node_Id) return Node_Id;
   procedure Set_Default_Instance (N : Node_Id; V : Node_Id);

   function Path (N : Node_Id) return List_Id;
   procedure Set_Path (N : Node_Id; V : List_Id);

   function Namespace_Path (N : Node_Id) return List_Id;
   procedure Set_Namespace_Path (N : Node_Id; V : List_Id);

   function Namespace_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Namespace_Identifier (N : Node_Id; V : Node_Id);

   function Full_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Full_Identifier (N : Node_Id; V : Node_Id);

   function Entity (N : Node_Id) return Node_Id;
   procedure Set_Entity (N : Node_Id; V : Node_Id);

   function First_Reference (N : Node_Id) return Node_Id;
   procedure Set_First_Reference (N : Node_Id; V : Node_Id);

   function Second_Reference (N : Node_Id) return Node_Id;
   procedure Set_Second_Reference (N : Node_Id; V : Node_Id);

   function Declarations (N : Node_Id) return List_Id;
   procedure Set_Declarations (N : Node_Id; V : List_Id);

   function Identifiers (N : Node_Id) return List_Id;
   procedure Set_Identifiers (N : Node_Id; V : List_Id);

   function Has_Private_Part (N : Node_Id) return Boolean;
   procedure Set_Has_Private_Part (N : Node_Id; V : Boolean);

   function Has_Public_Part (N : Node_Id) return Boolean;
   procedure Set_Has_Public_Part (N : Node_Id; V : Boolean);

   function Properties (N : Node_Id) return List_Id;
   procedure Set_Properties (N : Node_Id; V : List_Id);

   function Package_Name (N : Node_Id) return Node_Id;
   procedure Set_Package_Name (N : Node_Id; V : Node_Id);

   function Parent (N : Node_Id) return Node_Id;
   procedure Set_Parent (N : Node_Id; V : Node_Id);

   function Annexes (N : Node_Id) return List_Id;
   procedure Set_Annexes (N : Node_Id; V : List_Id);

   function List_Items (N : Node_Id) return List_Id;
   procedure Set_List_Items (N : Node_Id; V : List_Id);

   function Is_All (N : Node_Id) return Boolean;
   procedure Set_Is_All (N : Node_Id; V : Boolean);

   function Category (N : Node_Id) return Byte;
   procedure Set_Category (N : Node_Id; V : Byte);

   function Entity_Category (N : Node_Id) return Byte;
   procedure Set_Entity_Category (N : Node_Id; V : Byte);

   function Reference (N : Node_Id) return Node_Id;
   procedure Set_Reference (N : Node_Id; V : Node_Id);

   function Renamed_Entity (N : Node_Id) return Node_Id;
   procedure Set_Renamed_Entity (N : Node_Id; V : Node_Id);

   function Features (N : Node_Id) return List_Id;
   procedure Set_Features (N : Node_Id; V : List_Id);

   function Flows (N : Node_Id) return List_Id;
   procedure Set_Flows (N : Node_Id; V : List_Id);

   function Instances (N : Node_Id) return List_Id;
   procedure Set_Instances (N : Node_Id; V : List_Id);

   function Modes (N : Node_Id) return List_Id;
   procedure Set_Modes (N : Node_Id; V : List_Id);

   function Prototypes (N : Node_Id) return List_Id;
   procedure Set_Prototypes (N : Node_Id; V : List_Id);

   function Prototype_Bindings (N : Node_Id) return List_Id;
   procedure Set_Prototype_Bindings (N : Node_Id; V : List_Id);

   function Component_Type_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Component_Type_Identifier (N : Node_Id; V : Node_Id);

   function Refines_Type (N : Node_Id) return List_Id;
   procedure Set_Refines_Type (N : Node_Id; V : List_Id);

   function Subcomponents (N : Node_Id) return List_Id;
   procedure Set_Subcomponents (N : Node_Id; V : List_Id);

   function Calls (N : Node_Id) return List_Id;
   procedure Set_Calls (N : Node_Id; V : List_Id);

   function Connections (N : Node_Id) return List_Id;
   procedure Set_Connections (N : Node_Id; V : List_Id);

   function Container_Component (N : Node_Id) return Node_Id;
   procedure Set_Container_Component (N : Node_Id; V : Node_Id);

   function Entity_Ref (N : Node_Id) return Node_Id;
   procedure Set_Entity_Ref (N : Node_Id; V : Node_Id);

   function Is_Refinement (N : Node_Id) return Boolean;
   procedure Set_Is_Refinement (N : Node_Id; V : Boolean);

   function Is_Implicit_Inverse (N : Node_Id) return Boolean;
   procedure Set_Is_Implicit_Inverse (N : Node_Id; V : Boolean);

   function Inversed_Entity (N : Node_Id) return Node_Id;
   procedure Set_Inversed_Entity (N : Node_Id; V : Node_Id);

   function Is_In (N : Node_Id) return Boolean;
   procedure Set_Is_In (N : Node_Id; V : Boolean);

   function Is_Out (N : Node_Id) return Boolean;
   procedure Set_Is_Out (N : Node_Id; V : Boolean);

   function Is_Feature (N : Node_Id) return Boolean;
   procedure Set_Is_Feature (N : Node_Id; V : Boolean);

   function Is_Event (N : Node_Id) return Boolean;
   procedure Set_Is_Event (N : Node_Id; V : Boolean);

   function Is_Data (N : Node_Id) return Boolean;
   procedure Set_Is_Data (N : Node_Id; V : Boolean);

   function Array_Dimensions (N : Node_Id) return Node_Id;
   procedure Set_Array_Dimensions (N : Node_Id; V : Node_Id);

   function Inverse_Of (N : Node_Id) return Node_Id;
   procedure Set_Inverse_Of (N : Node_Id; V : Node_Id);

   function Is_Server (N : Node_Id) return Boolean;
   procedure Set_Is_Server (N : Node_Id; V : Boolean);

   function Is_Provided (N : Node_Id) return Boolean;
   procedure Set_Is_Provided (N : Node_Id; V : Boolean);

   function Subcomponent_Category (N : Node_Id) return Byte;
   procedure Set_Subcomponent_Category (N : Node_Id; V : Byte);

   function Source_Flow (N : Node_Id) return Node_Id;
   procedure Set_Source_Flow (N : Node_Id; V : Node_Id);

   function Sink_Flow (N : Node_Id) return Node_Id;
   procedure Set_Sink_Flow (N : Node_Id; V : Node_Id);

   function In_Modes (N : Node_Id) return Node_Id;
   procedure Set_In_Modes (N : Node_Id; V : Node_Id);

   function Is_Requires (N : Node_Id) return Boolean;
   procedure Set_Is_Requires (N : Node_Id; V : Boolean);

   function Is_Initial (N : Node_Id) return Boolean;
   procedure Set_Is_Initial (N : Node_Id; V : Boolean);

   function Source_Modes (N : Node_Id) return List_Id;
   procedure Set_Source_Modes (N : Node_Id; V : List_Id);

   function Triggers (N : Node_Id) return List_Id;
   procedure Set_Triggers (N : Node_Id; V : List_Id);

   function Destination_Mode (N : Node_Id) return Node_Id;
   procedure Set_Destination_Mode (N : Node_Id; V : Node_Id);

   function Is_Self (N : Node_Id) return Boolean;
   procedure Set_Is_Self (N : Node_Id; V : Boolean);

   function Is_Processor (N : Node_Id) return Boolean;
   procedure Set_Is_Processor (N : Node_Id; V : Boolean);

   function Corresponding_Flow_Spec (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Flow_Spec (N : Node_Id; V : Node_Id);

   function Parent_Sequence (N : Node_Id) return Node_Id;
   procedure Set_Parent_Sequence (N : Node_Id; V : Node_Id);

   function Subprogram_Calls (N : Node_Id) return List_Id;
   procedure Set_Subprogram_Calls (N : Node_Id; V : List_Id);

   function Is_Bidirectional (N : Node_Id) return Boolean;
   procedure Set_Is_Bidirectional (N : Node_Id; V : Boolean);

   function Source (N : Node_Id) return Node_Id;
   procedure Set_Source (N : Node_Id; V : Node_Id);

   function Destination (N : Node_Id) return Node_Id;
   procedure Set_Destination (N : Node_Id; V : Node_Id);

   function Property_Set_Context (N : Node_Id) return Node_Id;
   procedure Set_Property_Set_Context (N : Node_Id; V : Node_Id);

   function Imports_List (N : Node_Id) return List_Id;
   procedure Set_Imports_List (N : Node_Id; V : List_Id);

   function Annex_Path (N : Node_Id) return Node_Id;
   procedure Set_Annex_Path (N : Node_Id; V : Node_Id);

   function Is_List (N : Node_Id) return Boolean;
   procedure Set_Is_List (N : Node_Id; V : Boolean);

   function Multiplicity (N : Node_Id) return Int;
   procedure Set_Multiplicity (N : Node_Id; V : Int);

   function Property_Type_Designator (N : Node_Id) return Node_Id;
   procedure Set_Property_Type_Designator (N : Node_Id; V : Node_Id);

   function Expanded_Type_Designator (N : Node_Id) return Node_Id;
   procedure Set_Expanded_Type_Designator (N : Node_Id; V : Node_Id);

   function Property_Expression (N : Node_Id) return Node_Id;
   procedure Set_Property_Expression (N : Node_Id; V : Node_Id);

   function Property_Expressions (N : Node_Id) return List_Id;
   procedure Set_Property_Expressions (N : Node_Id; V : List_Id);

   function Constant_Type (N : Node_Id) return Node_Id;
   procedure Set_Constant_Type (N : Node_Id; V : Node_Id);

   function Unique_Unit_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Unique_Unit_Identifier (N : Node_Id; V : Node_Id);

   function Constant_Value (N : Node_Id) return Node_Id;
   procedure Set_Constant_Value (N : Node_Id; V : Node_Id);

   function Value_Container (N : Node_Id) return Node_Id;
   procedure Set_Value_Container (N : Node_Id; V : Node_Id);

   function Single_Value (N : Node_Id) return Node_Id;
   procedure Set_Single_Value (N : Node_Id; V : Node_Id);

   function Multi_Value (N : Node_Id) return List_Id;
   procedure Set_Multi_Value (N : Node_Id; V : List_Id);

   function Expanded_Single_Value (N : Node_Id) return Node_Id;
   procedure Set_Expanded_Single_Value (N : Node_Id; V : Node_Id);

   function Expanded_Multi_Value (N : Node_Id) return List_Id;
   procedure Set_Expanded_Multi_Value (N : Node_Id; V : List_Id);

   function Is_Access (N : Node_Id) return Boolean;
   procedure Set_Is_Access (N : Node_Id; V : Boolean);

   function Is_Inherit (N : Node_Id) return Boolean;
   procedure Set_Is_Inherit (N : Node_Id; V : Boolean);

   function Property_Name_Type (N : Node_Id) return Node_Id;
   procedure Set_Property_Name_Type (N : Node_Id; V : Node_Id);

   function Default_Value (N : Node_Id) return Node_Id;
   procedure Set_Default_Value (N : Node_Id; V : Node_Id);

   function Applies_To (N : Node_Id) return Node_Id;
   procedure Set_Applies_To (N : Node_Id; V : Node_Id);

   function Binding (N : Node_Id) return List_Id;
   procedure Set_Binding (N : Node_Id; V : List_Id);

   function Property_Name (N : Node_Id) return Node_Id;
   procedure Set_Property_Name (N : Node_Id; V : Node_Id);

   function Is_Additive_Association (N : Node_Id) return Boolean;
   procedure Set_Is_Additive_Association (N : Node_Id; V : Boolean);

   function Is_Constant (N : Node_Id) return Boolean;
   procedure Set_Is_Constant (N : Node_Id; V : Boolean);

   function Property_Association_Type (N : Node_Id) return Node_Id;
   procedure Set_Property_Association_Type (N : Node_Id; V : Node_Id);

   function Property_Association_Value (N : Node_Id) return Node_Id;
   procedure Set_Property_Association_Value (N : Node_Id; V : Node_Id);

   function Applies_To_Prop (N : Node_Id) return List_Id;
   procedure Set_Applies_To_Prop (N : Node_Id; V : List_Id);

   function In_Binding (N : Node_Id) return Node_Id;
   procedure Set_In_Binding (N : Node_Id; V : Node_Id);

   function Component_Cat (N : Node_Id) return Byte;
   procedure Set_Component_Cat (N : Node_Id; V : Byte);

   function Classifier_Ref (N : Node_Id) return Node_Id;
   procedure Set_Classifier_Ref (N : Node_Id; V : Node_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function Number_Value (N : Node_Id) return Node_Id;
   procedure Set_Number_Value (N : Node_Id; V : Node_Id);

   function Unit_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Unit_Identifier (N : Node_Id; V : Node_Id);

   function Boolean_Term (N : Node_Id) return Node_Id;
   procedure Set_Boolean_Term (N : Node_Id; V : Node_Id);

   function First_Term (N : Node_Id) return Node_Id;
   procedure Set_First_Term (N : Node_Id; V : Node_Id);

   function Second_Term (N : Node_Id) return Node_Id;
   procedure Set_Second_Term (N : Node_Id; V : Node_Id);

   function Numeric_Term (N : Node_Id) return Node_Id;
   procedure Set_Numeric_Term (N : Node_Id; V : Node_Id);

   function Property_Set_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Property_Set_Identifier (N : Node_Id; V : Node_Id);

   function Lower_Bound (N : Node_Id) return Node_Id;
   procedure Set_Lower_Bound (N : Node_Id; V : Node_Id);

   function Upper_Bound (N : Node_Id) return Node_Id;
   procedure Set_Upper_Bound (N : Node_Id; V : Node_Id);

   function Delta_Term (N : Node_Id) return Node_Id;
   procedure Set_Delta_Term (N : Node_Id; V : Node_Id);

   function Reference_Term (N : Node_Id) return Node_Id;
   procedure Set_Reference_Term (N : Node_Id; V : Node_Id);

   function Type_Range (N : Node_Id) return Node_Id;
   procedure Set_Type_Range (N : Node_Id; V : Node_Id);

   function Unit_Designator (N : Node_Id) return Node_Id;
   procedure Set_Unit_Designator (N : Node_Id; V : Node_Id);

   function Enumeration_Context (N : Node_Id) return Node_Id;
   procedure Set_Enumeration_Context (N : Node_Id; V : Node_Id);

   function Numeric_Literal (N : Node_Id) return Node_Id;
   procedure Set_Numeric_Literal (N : Node_Id; V : Node_Id);

   function Base_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Base_Identifier (N : Node_Id; V : Node_Id);

   function Unit_Definitions (N : Node_Id) return List_Id;
   procedure Set_Unit_Definitions (N : Node_Id; V : List_Id);

   function Units_Context (N : Node_Id) return Node_Id;
   procedure Set_Units_Context (N : Node_Id; V : Node_Id);

   function Number_Type (N : Node_Id) return Node_Id;
   procedure Set_Number_Type (N : Node_Id; V : Node_Id);

   function Owner_Categories (N : Node_Id) return List_Id;
   procedure Set_Owner_Categories (N : Node_Id; V : List_Id);

   function Raw_Text (N : Node_Id) return Name_Id;
   procedure Set_Raw_Text (N : Node_Id; V : Name_Id);

   function Annex_Content (N : Node_Id) return Node_Id;
   procedure Set_Annex_Content (N : Node_Id; V : Node_Id);

   function Corresponding_Annex (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Annex (N : Node_Id; V : Node_Id);

   function Container_Package (N : Node_Id) return Node_Id;
   procedure Set_Container_Package (N : Node_Id; V : Node_Id);

   function Array_List_Dim (N : Node_Id) return List_Id;
   procedure Set_Array_List_Dim (N : Node_Id; V : List_Id);

   function Size (N : Node_Id) return Node_Id;
   procedure Set_Size (N : Node_Id; V : Node_Id);

   function Range_Selections (N : Node_Id) return List_Id;
   procedure Set_Range_Selections (N : Node_Id; V : List_Id);

   function Item (N : Node_Id) return Node_Id;
   procedure Set_Item (N : Node_Id; V : Node_Id);

   function Extra_Item (N : Node_Id) return Node_Id;
   procedure Set_Extra_Item (N : Node_Id; V : Node_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 7) of Boolean;
   type Byte_Array is array (1 .. 4) of Byte;
   type Int_Array is array (1 .. 23) of Int;

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

end Ocarina.ME_AADL.AADL_Tree.Nodes;
