pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.Backends.Ada_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_Definition,
      K_List_Id,
      K_Defining_Identifier,
      K_Designator,
      K_Attribute_Designator,
      K_Explicit_Dereference,
      K_Used_Type,
      K_Declaration_List,
      K_Statement_List,
      K_Withed_Package,
      K_Used_Package,
      K_Withed_Packages,
      K_Package_Headers,
      K_Package_Specification,
      K_Package_Implementation,
      K_Package_Declaration,
      K_Main_Subprogram_Implementation,
      K_Packages,
      K_QoS_Distributed_Application,
      K_QoS_Node,
      K_HI_Distributed_Application,
      K_HI_Node,
      K_API_Unit,
      K_QoS_Unit,
      K_HI_Unit,
      K_Parameter_Specification,
      K_Parameter_Profile,
      K_Subprogram_Specification,
      K_Subprogram_Implementation,
      K_Subprogram_Call,
      K_Parameter_Association,
      K_Selected_Component,
      K_Full_Type_Declaration,
      K_Attribute_Definition_Clause,
      K_Enumeration_Literals,
      K_Enumeration_Type_Definition,
      K_Enumeration_Representation_Clause,
      K_Decimal_Type_Definition,
      K_Record_Aggregate,
      K_Component_Association,
      K_Protected_Object_Spec,
      K_Protected_Object_Body,
      K_Block_Statement,
      K_Elsif_Statement,
      K_If_Statement,
      K_Exit_When_Statement,
      K_Assignment_Statement,
      K_Delay_Statement,
      K_Return_Statement,
      K_For_Statement,
      K_Loop_Statement,
      K_Case_Statement_Alternative,
      K_Case_Statement,
      K_Case_Label,
      K_Pragma_Statement,
      K_Null_Statement,
      K_Package_Instantiation,
      K_Raise_Statement,
      K_Ada_Comment,
      K_Access_Type_Definition,
      K_Derived_Type_Definition,
      K_Record_Type_Definition,
      K_Private_Type_Definition,
      K_Component_Declaration,
      K_Component_List,
      K_Record_Definition,
      K_Range_Constraints,
      K_Array_Type_Definition,
      K_Range_Constraint,
      K_Variant_List,
      K_Variant_Part,
      K_Discrete_Choice_List,
      K_Variant,
      K_Object_Declaration,
      K_Literal,
      K_Element_Association,
      K_Element_List,
      K_Array_Aggregate,
      K_Indexed_Component,
      K_Exception_Declaration,
      K_Expression,
      K_Qualified_Expression,
      K_Type_Conversion,
      K_Object_Instantiation,
      K_Base_Type,
      K_Boolean,
      K_Float,
      K_Integer,
      K_String,
      K_Wide_String,
      K_Character,
      K_Wide_Character,
      K_Tree_Bindings,
      K_QoS_Tree_Bindings,
      K_HI_Tree_Bindings);

   --
   --  Node_Id
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --

   --
   --  Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --

   procedure W_Definition (N : Node_Id);

   --
   --  List_Id
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   --
   --  Defining_Identifier
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Corresponding_Node       : Node_Id
   --    Parent_Unit_Name         : Node_Id
   --

   procedure W_Defining_Identifier (N : Node_Id);

   --
   --  Designator
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Is_All                   : Boolean
   --    Defining_Identifier      : Node_Id
   --    Parent_Unit_Name         : Node_Id
   --

   procedure W_Designator (N : Node_Id);

   --
   --  Attribute_Designator
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Corresponding_Node       : Node_Id
   --    Parent_Unit_Name         : Node_Id
   --    Prefix                   : Node_Id
   --

   procedure W_Attribute_Designator (N : Node_Id);

   --
   --  Explicit_Dereference
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Prefix                   : Node_Id
   --

   procedure W_Explicit_Dereference (N : Node_Id);

   --
   --  Used_Type
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    The_Used_Entity          : Node_Id
   --

   procedure W_Used_Type (N : Node_Id);

   --
   --  Declaration_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Declaration_List (N : List_Id);

   --
   --  Statement_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Statement_List (N : List_Id);

   --
   --  Withed_Package
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Used                     : Boolean
   --    Warnings_Off             : Boolean
   --    Elaborated               : Boolean
   --

   procedure W_Withed_Package (N : Node_Id);

   --
   --  Used_Package
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    The_Used_Entity          : Node_Id
   --

   procedure W_Used_Package (N : Node_Id);

   --
   --  Withed_Packages
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Withed_Packages (N : List_Id);

   --
   --  Package_Headers
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Package_Headers (N : List_Id);

   --
   --  Package_Specification
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Package_Declaration      : Node_Id
   --    Withed_Packages          : List_Id
   --    Visible_Part             : List_Id
   --    Private_Part             : List_Id
   --    Is_Runtime_Package       : Boolean
   --    Is_Subunit_Package       : Boolean
   --    Is_Instantiated_Package  : Boolean
   --    Package_Instantiation    : Node_Id
   --    Package_Headers          : List_Id
   --

   procedure W_Package_Specification (N : Node_Id);

   --
   --  Package_Implementation
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Package_Declaration      : Node_Id
   --    Withed_Packages          : List_Id
   --    Declarations             : List_Id
   --    Statements               : List_Id
   --    Package_Initialization   : List_Id
   --    Package_Headers          : List_Id
   --

   procedure W_Package_Implementation (N : Node_Id);

   --
   --  Package_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Distributed_Application_Unit: Node_Id
   --    Package_Specification    : Node_Id
   --    Package_Implementation   : Node_Id
   --    Has_Custom_File_Name     : Boolean
   --    File_Name                : Name_Id
   --

   procedure W_Package_Declaration (N : Node_Id);

   --
   --  Main_Subprogram_Implementation
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Distributed_Application_Unit: Node_Id
   --    Subprogram_Specification : Node_Id
   --    Subprogram_Implementation: Node_Id
   --    Has_Custom_File_Name     : Boolean
   --    File_Name                : Name_Id
   --

   procedure W_Main_Subprogram_Implementation (N : Node_Id);

   --
   --  Packages
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Packages (N : List_Id);

   --
   --  QoS_Distributed_Application
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    QoS_Nodes                : List_Id
   --

   procedure W_QoS_Distributed_Application (N : Node_Id);

   --
   --  QoS_Node
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Units                    : List_Id
   --    Distributed_Application  : Node_Id
   --

   procedure W_QoS_Node (N : Node_Id);

   --
   --  HI_Distributed_Application
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Units                    : List_Id
   --    HI_Nodes                 : List_Id
   --

   procedure W_HI_Distributed_Application (N : Node_Id);

   --
   --  HI_Node
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Units                    : List_Id
   --    Distributed_Application  : Node_Id
   --

   procedure W_HI_Node (N : Node_Id);

   --
   --  API_Unit
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Main_Subprogram          : Node_Id
   --    Packages                 : List_Id
   --    Entity                   : Node_Id
   --

   procedure W_API_Unit (N : Node_Id);

   --
   --  QoS_Unit
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Main_Subprogram          : Node_Id
   --    Packages                 : List_Id
   --    Entity                   : Node_Id
   --    Helpers_Package          : Node_Id
   --    Servants_Package         : Node_Id
   --    Parameters_Package       : Node_Id
   --    Obj_Adapters_Package     : Node_Id
   --    Setup_Package            : Node_Id
   --    Namespaces_Package       : Node_Id
   --

   procedure W_QoS_Unit (N : Node_Id);

   --
   --  HI_Unit
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Main_Subprogram          : Node_Id
   --    Packages                 : List_Id
   --    Entity                   : Node_Id
   --    Marshallers_Package      : Node_Id
   --    Activity_Package         : Node_Id
   --    Subprograms_Package      : Node_Id
   --    Transport_Package        : Node_Id
   --    Types_Package            : Node_Id
   --    Deployment_Package       : Node_Id
   --    Naming_Package           : Node_Id
   --

   procedure W_HI_Unit (N : Node_Id);

   --
   --  Parameter_Specification
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Parameter_Mode           : Mode_Id
   --    Parameter_Type           : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Parameter_Specification (N : Node_Id);

   --
   --  Parameter_Profile
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Parameter_Profile (N : List_Id);

   --
   --  Subprogram_Specification
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Withed_Packages          : List_Id
   --    Parameter_Profile        : List_Id
   --    Return_Type              : Node_Id
   --    Renamed_Entity           : Node_Id
   --    Instantiated_Entity      : Node_Id
   --    Main_Subprogram_Unit     : Node_Id
   --    Package_Headers          : List_Id
   --

   procedure W_Subprogram_Specification (N : Node_Id);

   --
   --  Subprogram_Implementation
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Withed_Packages          : List_Id
   --    Specification            : Node_Id
   --    Declarations             : List_Id
   --    Statements               : List_Id
   --    Main_Subprogram_Unit     : Node_Id
   --    Package_Headers          : List_Id
   --

   procedure W_Subprogram_Implementation (N : Node_Id);

   --
   --  Subprogram_Call
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Actual_Parameter_Part    : List_Id
   --

   procedure W_Subprogram_Call (N : Node_Id);

   --
   --  Parameter_Association
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Selector_Name            : Node_Id
   --    Actual_Parameter         : Node_Id
   --

   procedure W_Parameter_Association (N : Node_Id);

   --
   --  Selected_Component
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Prefix                   : Node_Id
   --    Selector_Name            : Node_Id
   --

   procedure W_Selected_Component (N : Node_Id);

   --
   --  Full_Type_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Type_Definition          : Node_Id
   --    Discriminant_Spec        : Node_Id
   --    Is_Subtype               : Boolean
   --

   procedure W_Full_Type_Declaration (N : Node_Id);

   --
   --  Attribute_Definition_Clause
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Attribute_Designator     : Name_Id
   --    Expression               : Node_Id
   --

   procedure W_Attribute_Definition_Clause (N : Node_Id);

   --
   --  Enumeration_Literals
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Enumeration_Literals (N : List_Id);

   --
   --  Enumeration_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Enumeration_Literals     : List_Id
   --

   procedure W_Enumeration_Type_Definition (N : Node_Id);

   --
   --  Enumeration_Representation_Clause
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Array_Aggregate          : Node_Id
   --

   procedure W_Enumeration_Representation_Clause (N : Node_Id);

   --
   --  Decimal_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Scale                    : Node_Id
   --    Total                    : Value_Id
   --

   procedure W_Decimal_Type_Definition (N : Node_Id);

   --
   --  Record_Aggregate
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Component_Association_List: List_Id
   --

   procedure W_Record_Aggregate (N : Node_Id);

   --
   --  Component_Association
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Component_Association (N : Node_Id);

   --
   --  Protected_Object_Spec
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Visible_Part             : List_Id
   --    Private_Part             : List_Id
   --    Is_Type                  : Boolean
   --

   procedure W_Protected_Object_Spec (N : Node_Id);

   --
   --  Protected_Object_Body
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Statements               : List_Id
   --

   procedure W_Protected_Object_Body (N : Node_Id);

   --
   --  Block_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Declarative_Part         : List_Id
   --    Statements               : List_Id
   --    Exception_Handler        : List_Id
   --

   procedure W_Block_Statement (N : Node_Id);

   --
   --  Elsif_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Condition                : Node_Id
   --    Then_Statements          : List_Id
   --

   procedure W_Elsif_Statement (N : Node_Id);

   --
   --  If_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Condition                : Node_Id
   --    Then_Statements          : List_Id
   --    Elsif_Statements         : List_Id
   --    Else_Statements          : List_Id
   --

   procedure W_If_Statement (N : Node_Id);

   --
   --  Exit_When_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Condition                : Node_Id
   --

   procedure W_Exit_When_Statement (N : Node_Id);

   --
   --  Assignment_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Assignment_Statement (N : Node_Id);

   --
   --  Delay_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Is_Until                 : Boolean
   --    Expression               : Node_Id
   --

   procedure W_Delay_Statement (N : Node_Id);

   --
   --  Return_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Return_Statement (N : Node_Id);

   --
   --  For_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Range_Constraint         : Node_Id
   --    Statements               : List_Id
   --

   procedure W_For_Statement (N : Node_Id);

   --
   --  Loop_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Statements               : List_Id
   --

   procedure W_Loop_Statement (N : Node_Id);

   --
   --  Case_Statement_Alternative
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Discret_Choice_List      : List_Id
   --    Statements               : List_Id
   --

   procedure W_Case_Statement_Alternative (N : Node_Id);

   --
   --  Case_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Expression               : Node_Id
   --    Case_Statement_Alternatives: List_Id
   --

   procedure W_Case_Statement (N : Node_Id);

   --
   --  Case_Label
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Expression               : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Case_Label (N : Node_Id);

   --
   --  Pragma_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Argument_List            : List_Id
   --

   procedure W_Pragma_Statement (N : Node_Id);

   --
   --  Null_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --

   procedure W_Null_Statement (N : Node_Id);

   --
   --  Package_Instantiation
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Generic_Package          : Node_Id
   --    Parameter_List           : List_Id
   --

   procedure W_Package_Instantiation (N : Node_Id);

   --
   --  Raise_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Raised_Error             : Node_Id
   --

   procedure W_Raise_Statement (N : Node_Id);

   --
   --  Ada_Comment
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Has_Header_Spaces        : Boolean
   --

   procedure W_Ada_Comment (N : Node_Id);

   --
   --  Access_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Is_All                   : Boolean
   --    Is_Constant              : Boolean
   --    Is_Not_Null              : Boolean
   --    Subtype_Indication       : Node_Id
   --

   procedure W_Access_Type_Definition (N : Node_Id);

   --
   --  Derived_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Is_Private_Extention     : Boolean
   --    Is_Abstract_Type         : Boolean
   --    Subtype_Indication       : Node_Id
   --    Record_Extension_Part    : Node_Id
   --    Is_Subtype               : Boolean
   --

   procedure W_Derived_Type_Definition (N : Node_Id);

   --
   --  Record_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Is_Abstract_Type         : Boolean
   --    Is_Tagged_Type           : Boolean
   --    Is_Limited_Type          : Boolean
   --    Record_Definition        : Node_Id
   --

   procedure W_Record_Type_Definition (N : Node_Id);

   --
   --  Private_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --

   procedure W_Private_Type_Definition (N : Node_Id);

   --
   --  Component_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Subtype_Indication       : Node_Id
   --    Expression               : Node_Id
   --    Aliased_Present          : Boolean
   --

   procedure W_Component_Declaration (N : Node_Id);

   --
   --  Component_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Component_List (N : List_Id);

   --
   --  Record_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Component_List           : List_Id
   --

   procedure W_Record_Definition (N : Node_Id);

   --
   --  Range_Constraints
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Range_Constraints (N : List_Id);

   --
   --  Array_Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Range_Constraints        : List_Id
   --    Component_Definition     : Node_Id
   --    Aliased_Present          : Boolean
   --

   procedure W_Array_Type_Definition (N : Node_Id);

   --
   --  Range_Constraint
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    First                    : Node_Id
   --    Last                     : Node_Id
   --    Index_Type               : Node_Id
   --

   procedure W_Range_Constraint (N : Node_Id);

   --
   --  Variant_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Variant_List (N : List_Id);

   --
   --  Variant_Part
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Variants                 : List_Id
   --    Discriminant             : Node_Id
   --

   procedure W_Variant_Part (N : Node_Id);

   --
   --  Discrete_Choice_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Discrete_Choice_List (N : List_Id);

   --
   --  Variant
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Discrete_Choices         : List_Id
   --    Component_List           : List_Id
   --

   procedure W_Variant (N : Node_Id);

   --
   --  Object_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Constant_Present         : Boolean
   --    Aliased_Present          : Boolean
   --    Object_Definition        : Node_Id
   --    Expression               : Node_Id
   --    Renamed_Entity           : Node_Id
   --    Discriminant_Spec        : Node_Id
   --

   procedure W_Object_Declaration (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Value                    : Value_Id
   --    Parent_Designator        : Node_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  Element_Association
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Index                    : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Element_Association (N : Node_Id);

   --
   --  Element_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Element_List (N : List_Id);

   --
   --  Array_Aggregate
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Elements                 : List_Id
   --

   procedure W_Array_Aggregate (N : Node_Id);

   --
   --  Indexed_Component
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Prefix                   : Node_Id
   --    Expressions              : List_Id
   --

   procedure W_Indexed_Component (N : Node_Id);

   --
   --  Exception_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parent                   : Node_Id
   --    Renamed_Entity           : Node_Id
   --

   procedure W_Exception_Declaration (N : Node_Id);

   --
   --  Expression
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Operator                 : Operator_Id
   --    Left_Expr                : Node_Id
   --    Right_Expr               : Node_Id
   --

   procedure W_Expression (N : Node_Id);

   --
   --  Qualified_Expression
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Subtype_Mark             : Node_Id
   --    Aggregate                : Node_Id
   --

   procedure W_Qualified_Expression (N : Node_Id);

   --
   --  Type_Conversion
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Subtype_Mark             : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Type_Conversion (N : Node_Id);

   --
   --  Object_Instantiation
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Qualified_Expression     : Node_Id
   --

   procedure W_Object_Instantiation (N : Node_Id);

   --
   --  Base_Type
   --
   --    Image                    : Name_Id
   --

   --
   --  Boolean
   --
   --    Image                    : Name_Id
   --

   procedure W_Boolean (N : Base_Type);

   --
   --  Float
   --
   --    Image                    : Name_Id
   --

   procedure W_Float (N : Base_Type);

   --
   --  Integer
   --
   --    Image                    : Name_Id
   --

   procedure W_Integer (N : Base_Type);

   --
   --  String
   --
   --    Image                    : Name_Id
   --

   procedure W_String (N : Base_Type);

   --
   --  Wide_String
   --
   --    Image                    : Name_Id
   --

   procedure W_Wide_String (N : Base_Type);

   --
   --  Character
   --
   --    Image                    : Name_Id
   --

   procedure W_Character (N : Base_Type);

   --
   --  Wide_Character
   --
   --    Image                    : Name_Id
   --

   procedure W_Wide_Character (N : Base_Type);

   --
   --  Tree_Bindings
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Main_Node                : Node_Id
   --    Type_Definition_Node     : Node_Id
   --    Feature_Subprogram_Node  : Node_Id
   --    Subprogram_Node          : Node_Id
   --

   procedure W_Tree_Bindings (N : Node_Id);

   --
   --  QoS_Tree_Bindings
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Main_Node                : Node_Id
   --    Type_Definition_Node     : Node_Id
   --    Feature_Subprogram_Node  : Node_Id
   --    Subprogram_Node          : Node_Id
   --    Helpers_Node             : Node_Id
   --    Servants_Node            : Node_Id
   --    Parameters_Node          : Node_Id
   --    Obj_Adapters_Node        : Node_Id
   --    Setup_Node               : Node_Id
   --    Namespaces_Node          : Node_Id
   --    TypeCode_Node            : Node_Id
   --    From_Any_Node            : Node_Id
   --    To_Any_Node              : Node_Id
   --    Initialize_Node          : Node_Id
   --    Thread_Controller_Node   : Node_Id
   --    Execute_Servant_Node     : Node_Id
   --    Put_Node                 : Node_Id
   --    Push_Back_Node           : Node_Id
   --    Get_Node                 : Node_Id
   --    Package_Node             : Node_Id
   --    Reference_Node           : Node_Id
   --    Set_Node                 : Node_Id
   --    Build_Node               : Node_Id
   --

   procedure W_QoS_Tree_Bindings (N : Node_Id);

   --
   --  HI_Tree_Bindings
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Main_Node                : Node_Id
   --    Type_Definition_Node     : Node_Id
   --    Feature_Subprogram_Node  : Node_Id
   --    Subprogram_Node          : Node_Id
   --    Marshallers_Node         : Node_Id
   --    Activity_Node            : Node_Id
   --    Subprograms_Node         : Node_Id
   --    Types_Node               : Node_Id
   --    Transport_Node           : Node_Id
   --    Deployment_Node          : Node_Id
   --    Naming_Node              : Node_Id
   --    Job_Node                 : Node_Id
   --    Enumerator_Node          : Node_Id
   --    Marshall_Node            : Node_Id
   --    Unmarshall_Node          : Node_Id
   --    Port_Interface_Node      : Node_Id
   --    Port_Enumeration_Node    : Node_Id
   --    Deliver_Node             : Node_Id
   --    Send_Node                : Node_Id
   --    Put_Value_Node           : Node_Id
   --    Get_Value_Node           : Node_Id
   --    Get_Count_Node           : Node_Id
   --    Next_Value_Node          : Node_Id
   --    Store_Received_Message_Node: Node_Id
   --    Default_Value_Node       : Node_Id
   --    Object_Node              : Node_Id
   --

   procedure W_HI_Tree_Bindings (N : Node_Id);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function Frontend_Node (N : Node_Id) return Node_Id;
   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id);

   function Defining_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id);

   function Parent (N : Node_Id) return Node_Id;
   procedure Set_Parent (N : Node_Id; V : Node_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Corresponding_Node (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id);

   function Parent_Unit_Name (N : Node_Id) return Node_Id;
   procedure Set_Parent_Unit_Name (N : Node_Id; V : Node_Id);

   function Is_All (N : Node_Id) return Boolean;
   procedure Set_Is_All (N : Node_Id; V : Boolean);

   function Prefix (N : Node_Id) return Node_Id;
   procedure Set_Prefix (N : Node_Id; V : Node_Id);

   function The_Used_Entity (N : Node_Id) return Node_Id;
   procedure Set_The_Used_Entity (N : Node_Id; V : Node_Id);

   function Used (N : Node_Id) return Boolean;
   procedure Set_Used (N : Node_Id; V : Boolean);

   function Warnings_Off (N : Node_Id) return Boolean;
   procedure Set_Warnings_Off (N : Node_Id; V : Boolean);

   function Elaborated (N : Node_Id) return Boolean;
   procedure Set_Elaborated (N : Node_Id; V : Boolean);

   function Package_Declaration (N : Node_Id) return Node_Id;
   procedure Set_Package_Declaration (N : Node_Id; V : Node_Id);

   function Withed_Packages (N : Node_Id) return List_Id;
   procedure Set_Withed_Packages (N : Node_Id; V : List_Id);

   function Visible_Part (N : Node_Id) return List_Id;
   procedure Set_Visible_Part (N : Node_Id; V : List_Id);

   function Private_Part (N : Node_Id) return List_Id;
   procedure Set_Private_Part (N : Node_Id; V : List_Id);

   function Is_Runtime_Package (N : Node_Id) return Boolean;
   procedure Set_Is_Runtime_Package (N : Node_Id; V : Boolean);

   function Is_Subunit_Package (N : Node_Id) return Boolean;
   procedure Set_Is_Subunit_Package (N : Node_Id; V : Boolean);

   function Is_Instantiated_Package (N : Node_Id) return Boolean;
   procedure Set_Is_Instantiated_Package (N : Node_Id; V : Boolean);

   function Package_Instantiation (N : Node_Id) return Node_Id;
   procedure Set_Package_Instantiation (N : Node_Id; V : Node_Id);

   function Package_Headers (N : Node_Id) return List_Id;
   procedure Set_Package_Headers (N : Node_Id; V : List_Id);

   function Declarations (N : Node_Id) return List_Id;
   procedure Set_Declarations (N : Node_Id; V : List_Id);

   function Statements (N : Node_Id) return List_Id;
   procedure Set_Statements (N : Node_Id; V : List_Id);

   function Package_Initialization (N : Node_Id) return List_Id;
   procedure Set_Package_Initialization (N : Node_Id; V : List_Id);

   function Distributed_Application_Unit (N : Node_Id) return Node_Id;
   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id);

   function Package_Specification (N : Node_Id) return Node_Id;
   procedure Set_Package_Specification (N : Node_Id; V : Node_Id);

   function Package_Implementation (N : Node_Id) return Node_Id;
   procedure Set_Package_Implementation (N : Node_Id; V : Node_Id);

   function Has_Custom_File_Name (N : Node_Id) return Boolean;
   procedure Set_Has_Custom_File_Name (N : Node_Id; V : Boolean);

   function File_Name (N : Node_Id) return Name_Id;
   procedure Set_File_Name (N : Node_Id; V : Name_Id);

   function Subprogram_Specification (N : Node_Id) return Node_Id;
   procedure Set_Subprogram_Specification (N : Node_Id; V : Node_Id);

   function Subprogram_Implementation (N : Node_Id) return Node_Id;
   procedure Set_Subprogram_Implementation (N : Node_Id; V : Node_Id);

   function QoS_Nodes (N : Node_Id) return List_Id;
   procedure Set_QoS_Nodes (N : Node_Id; V : List_Id);

   function Units (N : Node_Id) return List_Id;
   procedure Set_Units (N : Node_Id; V : List_Id);

   function Distributed_Application (N : Node_Id) return Node_Id;
   procedure Set_Distributed_Application (N : Node_Id; V : Node_Id);

   function HI_Nodes (N : Node_Id) return List_Id;
   procedure Set_HI_Nodes (N : Node_Id; V : List_Id);

   function Main_Subprogram (N : Node_Id) return Node_Id;
   procedure Set_Main_Subprogram (N : Node_Id; V : Node_Id);

   function Packages (N : Node_Id) return List_Id;
   procedure Set_Packages (N : Node_Id; V : List_Id);

   function Entity (N : Node_Id) return Node_Id;
   procedure Set_Entity (N : Node_Id; V : Node_Id);

   function Helpers_Package (N : Node_Id) return Node_Id;
   procedure Set_Helpers_Package (N : Node_Id; V : Node_Id);

   function Servants_Package (N : Node_Id) return Node_Id;
   procedure Set_Servants_Package (N : Node_Id; V : Node_Id);

   function Parameters_Package (N : Node_Id) return Node_Id;
   procedure Set_Parameters_Package (N : Node_Id; V : Node_Id);

   function Obj_Adapters_Package (N : Node_Id) return Node_Id;
   procedure Set_Obj_Adapters_Package (N : Node_Id; V : Node_Id);

   function Setup_Package (N : Node_Id) return Node_Id;
   procedure Set_Setup_Package (N : Node_Id; V : Node_Id);

   function Namespaces_Package (N : Node_Id) return Node_Id;
   procedure Set_Namespaces_Package (N : Node_Id; V : Node_Id);

   function Marshallers_Package (N : Node_Id) return Node_Id;
   procedure Set_Marshallers_Package (N : Node_Id; V : Node_Id);

   function Activity_Package (N : Node_Id) return Node_Id;
   procedure Set_Activity_Package (N : Node_Id; V : Node_Id);

   function Subprograms_Package (N : Node_Id) return Node_Id;
   procedure Set_Subprograms_Package (N : Node_Id; V : Node_Id);

   function Transport_Package (N : Node_Id) return Node_Id;
   procedure Set_Transport_Package (N : Node_Id; V : Node_Id);

   function Types_Package (N : Node_Id) return Node_Id;
   procedure Set_Types_Package (N : Node_Id; V : Node_Id);

   function Deployment_Package (N : Node_Id) return Node_Id;
   procedure Set_Deployment_Package (N : Node_Id; V : Node_Id);

   function Naming_Package (N : Node_Id) return Node_Id;
   procedure Set_Naming_Package (N : Node_Id; V : Node_Id);

   function Parameter_Mode (N : Node_Id) return Mode_Id;
   procedure Set_Parameter_Mode (N : Node_Id; V : Mode_Id);

   function Parameter_Type (N : Node_Id) return Node_Id;
   procedure Set_Parameter_Type (N : Node_Id; V : Node_Id);

   function Expression (N : Node_Id) return Node_Id;
   procedure Set_Expression (N : Node_Id; V : Node_Id);

   function Parameter_Profile (N : Node_Id) return List_Id;
   procedure Set_Parameter_Profile (N : Node_Id; V : List_Id);

   function Return_Type (N : Node_Id) return Node_Id;
   procedure Set_Return_Type (N : Node_Id; V : Node_Id);

   function Renamed_Entity (N : Node_Id) return Node_Id;
   procedure Set_Renamed_Entity (N : Node_Id; V : Node_Id);

   function Instantiated_Entity (N : Node_Id) return Node_Id;
   procedure Set_Instantiated_Entity (N : Node_Id; V : Node_Id);

   function Main_Subprogram_Unit (N : Node_Id) return Node_Id;
   procedure Set_Main_Subprogram_Unit (N : Node_Id; V : Node_Id);

   function Specification (N : Node_Id) return Node_Id;
   procedure Set_Specification (N : Node_Id; V : Node_Id);

   function Actual_Parameter_Part (N : Node_Id) return List_Id;
   procedure Set_Actual_Parameter_Part (N : Node_Id; V : List_Id);

   function Selector_Name (N : Node_Id) return Node_Id;
   procedure Set_Selector_Name (N : Node_Id; V : Node_Id);

   function Actual_Parameter (N : Node_Id) return Node_Id;
   procedure Set_Actual_Parameter (N : Node_Id; V : Node_Id);

   function Type_Definition (N : Node_Id) return Node_Id;
   procedure Set_Type_Definition (N : Node_Id; V : Node_Id);

   function Discriminant_Spec (N : Node_Id) return Node_Id;
   procedure Set_Discriminant_Spec (N : Node_Id; V : Node_Id);

   function Is_Subtype (N : Node_Id) return Boolean;
   procedure Set_Is_Subtype (N : Node_Id; V : Boolean);

   function Attribute_Designator (N : Node_Id) return Name_Id;
   procedure Set_Attribute_Designator (N : Node_Id; V : Name_Id);

   function Enumeration_Literals (N : Node_Id) return List_Id;
   procedure Set_Enumeration_Literals (N : Node_Id; V : List_Id);

   function Array_Aggregate (N : Node_Id) return Node_Id;
   procedure Set_Array_Aggregate (N : Node_Id; V : Node_Id);

   function Scale (N : Node_Id) return Node_Id;
   procedure Set_Scale (N : Node_Id; V : Node_Id);

   function Total (N : Node_Id) return Value_Id;
   procedure Set_Total (N : Node_Id; V : Value_Id);

   function Component_Association_List (N : Node_Id) return List_Id;
   procedure Set_Component_Association_List (N : Node_Id; V : List_Id);

   function Is_Type (N : Node_Id) return Boolean;
   procedure Set_Is_Type (N : Node_Id; V : Boolean);

   function Declarative_Part (N : Node_Id) return List_Id;
   procedure Set_Declarative_Part (N : Node_Id; V : List_Id);

   function Exception_Handler (N : Node_Id) return List_Id;
   procedure Set_Exception_Handler (N : Node_Id; V : List_Id);

   function Condition (N : Node_Id) return Node_Id;
   procedure Set_Condition (N : Node_Id; V : Node_Id);

   function Then_Statements (N : Node_Id) return List_Id;
   procedure Set_Then_Statements (N : Node_Id; V : List_Id);

   function Elsif_Statements (N : Node_Id) return List_Id;
   procedure Set_Elsif_Statements (N : Node_Id; V : List_Id);

   function Else_Statements (N : Node_Id) return List_Id;
   procedure Set_Else_Statements (N : Node_Id; V : List_Id);

   function Is_Until (N : Node_Id) return Boolean;
   procedure Set_Is_Until (N : Node_Id; V : Boolean);

   function Range_Constraint (N : Node_Id) return Node_Id;
   procedure Set_Range_Constraint (N : Node_Id; V : Node_Id);

   function Discret_Choice_List (N : Node_Id) return List_Id;
   procedure Set_Discret_Choice_List (N : Node_Id; V : List_Id);

   function Case_Statement_Alternatives (N : Node_Id) return List_Id;
   procedure Set_Case_Statement_Alternatives (N : Node_Id; V : List_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function Argument_List (N : Node_Id) return List_Id;
   procedure Set_Argument_List (N : Node_Id; V : List_Id);

   function Generic_Package (N : Node_Id) return Node_Id;
   procedure Set_Generic_Package (N : Node_Id; V : Node_Id);

   function Parameter_List (N : Node_Id) return List_Id;
   procedure Set_Parameter_List (N : Node_Id; V : List_Id);

   function Raised_Error (N : Node_Id) return Node_Id;
   procedure Set_Raised_Error (N : Node_Id; V : Node_Id);

   function Has_Header_Spaces (N : Node_Id) return Boolean;
   procedure Set_Has_Header_Spaces (N : Node_Id; V : Boolean);

   function Is_Constant (N : Node_Id) return Boolean;
   procedure Set_Is_Constant (N : Node_Id; V : Boolean);

   function Is_Not_Null (N : Node_Id) return Boolean;
   procedure Set_Is_Not_Null (N : Node_Id; V : Boolean);

   function Subtype_Indication (N : Node_Id) return Node_Id;
   procedure Set_Subtype_Indication (N : Node_Id; V : Node_Id);

   function Is_Private_Extention (N : Node_Id) return Boolean;
   procedure Set_Is_Private_Extention (N : Node_Id; V : Boolean);

   function Is_Abstract_Type (N : Node_Id) return Boolean;
   procedure Set_Is_Abstract_Type (N : Node_Id; V : Boolean);

   function Record_Extension_Part (N : Node_Id) return Node_Id;
   procedure Set_Record_Extension_Part (N : Node_Id; V : Node_Id);

   function Is_Tagged_Type (N : Node_Id) return Boolean;
   procedure Set_Is_Tagged_Type (N : Node_Id; V : Boolean);

   function Is_Limited_Type (N : Node_Id) return Boolean;
   procedure Set_Is_Limited_Type (N : Node_Id; V : Boolean);

   function Record_Definition (N : Node_Id) return Node_Id;
   procedure Set_Record_Definition (N : Node_Id; V : Node_Id);

   function Aliased_Present (N : Node_Id) return Boolean;
   procedure Set_Aliased_Present (N : Node_Id; V : Boolean);

   function Component_List (N : Node_Id) return List_Id;
   procedure Set_Component_List (N : Node_Id; V : List_Id);

   function Range_Constraints (N : Node_Id) return List_Id;
   procedure Set_Range_Constraints (N : Node_Id; V : List_Id);

   function Component_Definition (N : Node_Id) return Node_Id;
   procedure Set_Component_Definition (N : Node_Id; V : Node_Id);

   function First (N : Node_Id) return Node_Id;
   procedure Set_First (N : Node_Id; V : Node_Id);

   function Last (N : Node_Id) return Node_Id;
   procedure Set_Last (N : Node_Id; V : Node_Id);

   function Index_Type (N : Node_Id) return Node_Id;
   procedure Set_Index_Type (N : Node_Id; V : Node_Id);

   function Variants (N : Node_Id) return List_Id;
   procedure Set_Variants (N : Node_Id; V : List_Id);

   function Discriminant (N : Node_Id) return Node_Id;
   procedure Set_Discriminant (N : Node_Id; V : Node_Id);

   function Discrete_Choices (N : Node_Id) return List_Id;
   procedure Set_Discrete_Choices (N : Node_Id; V : List_Id);

   function Constant_Present (N : Node_Id) return Boolean;
   procedure Set_Constant_Present (N : Node_Id; V : Boolean);

   function Object_Definition (N : Node_Id) return Node_Id;
   procedure Set_Object_Definition (N : Node_Id; V : Node_Id);

   function Parent_Designator (N : Node_Id) return Node_Id;
   procedure Set_Parent_Designator (N : Node_Id; V : Node_Id);

   function Index (N : Node_Id) return Node_Id;
   procedure Set_Index (N : Node_Id; V : Node_Id);

   function Elements (N : Node_Id) return List_Id;
   procedure Set_Elements (N : Node_Id; V : List_Id);

   function Expressions (N : Node_Id) return List_Id;
   procedure Set_Expressions (N : Node_Id; V : List_Id);

   function Operator (N : Node_Id) return Operator_Id;
   procedure Set_Operator (N : Node_Id; V : Operator_Id);

   function Left_Expr (N : Node_Id) return Node_Id;
   procedure Set_Left_Expr (N : Node_Id; V : Node_Id);

   function Right_Expr (N : Node_Id) return Node_Id;
   procedure Set_Right_Expr (N : Node_Id; V : Node_Id);

   function Subtype_Mark (N : Node_Id) return Node_Id;
   procedure Set_Subtype_Mark (N : Node_Id; V : Node_Id);

   function Aggregate (N : Node_Id) return Node_Id;
   procedure Set_Aggregate (N : Node_Id; V : Node_Id);

   function Qualified_Expression (N : Node_Id) return Node_Id;
   procedure Set_Qualified_Expression (N : Node_Id; V : Node_Id);

   function Image (N : Base_Type) return Name_Id;
   procedure Set_Image (N : Base_Type; V : Name_Id);

   function Main_Node (N : Node_Id) return Node_Id;
   procedure Set_Main_Node (N : Node_Id; V : Node_Id);

   function Type_Definition_Node (N : Node_Id) return Node_Id;
   procedure Set_Type_Definition_Node (N : Node_Id; V : Node_Id);

   function Feature_Subprogram_Node (N : Node_Id) return Node_Id;
   procedure Set_Feature_Subprogram_Node (N : Node_Id; V : Node_Id);

   function Subprogram_Node (N : Node_Id) return Node_Id;
   procedure Set_Subprogram_Node (N : Node_Id; V : Node_Id);

   function Helpers_Node (N : Node_Id) return Node_Id;
   procedure Set_Helpers_Node (N : Node_Id; V : Node_Id);

   function Servants_Node (N : Node_Id) return Node_Id;
   procedure Set_Servants_Node (N : Node_Id; V : Node_Id);

   function Parameters_Node (N : Node_Id) return Node_Id;
   procedure Set_Parameters_Node (N : Node_Id; V : Node_Id);

   function Obj_Adapters_Node (N : Node_Id) return Node_Id;
   procedure Set_Obj_Adapters_Node (N : Node_Id; V : Node_Id);

   function Setup_Node (N : Node_Id) return Node_Id;
   procedure Set_Setup_Node (N : Node_Id; V : Node_Id);

   function Namespaces_Node (N : Node_Id) return Node_Id;
   procedure Set_Namespaces_Node (N : Node_Id; V : Node_Id);

   function TypeCode_Node (N : Node_Id) return Node_Id;
   procedure Set_TypeCode_Node (N : Node_Id; V : Node_Id);

   function From_Any_Node (N : Node_Id) return Node_Id;
   procedure Set_From_Any_Node (N : Node_Id; V : Node_Id);

   function To_Any_Node (N : Node_Id) return Node_Id;
   procedure Set_To_Any_Node (N : Node_Id; V : Node_Id);

   function Initialize_Node (N : Node_Id) return Node_Id;
   procedure Set_Initialize_Node (N : Node_Id; V : Node_Id);

   function Thread_Controller_Node (N : Node_Id) return Node_Id;
   procedure Set_Thread_Controller_Node (N : Node_Id; V : Node_Id);

   function Execute_Servant_Node (N : Node_Id) return Node_Id;
   procedure Set_Execute_Servant_Node (N : Node_Id; V : Node_Id);

   function Put_Node (N : Node_Id) return Node_Id;
   procedure Set_Put_Node (N : Node_Id; V : Node_Id);

   function Push_Back_Node (N : Node_Id) return Node_Id;
   procedure Set_Push_Back_Node (N : Node_Id; V : Node_Id);

   function Get_Node (N : Node_Id) return Node_Id;
   procedure Set_Get_Node (N : Node_Id; V : Node_Id);

   function Package_Node (N : Node_Id) return Node_Id;
   procedure Set_Package_Node (N : Node_Id; V : Node_Id);

   function Reference_Node (N : Node_Id) return Node_Id;
   procedure Set_Reference_Node (N : Node_Id; V : Node_Id);

   function Set_Node (N : Node_Id) return Node_Id;
   procedure Set_Set_Node (N : Node_Id; V : Node_Id);

   function Build_Node (N : Node_Id) return Node_Id;
   procedure Set_Build_Node (N : Node_Id; V : Node_Id);

   function Marshallers_Node (N : Node_Id) return Node_Id;
   procedure Set_Marshallers_Node (N : Node_Id; V : Node_Id);

   function Activity_Node (N : Node_Id) return Node_Id;
   procedure Set_Activity_Node (N : Node_Id; V : Node_Id);

   function Subprograms_Node (N : Node_Id) return Node_Id;
   procedure Set_Subprograms_Node (N : Node_Id; V : Node_Id);

   function Types_Node (N : Node_Id) return Node_Id;
   procedure Set_Types_Node (N : Node_Id; V : Node_Id);

   function Transport_Node (N : Node_Id) return Node_Id;
   procedure Set_Transport_Node (N : Node_Id; V : Node_Id);

   function Deployment_Node (N : Node_Id) return Node_Id;
   procedure Set_Deployment_Node (N : Node_Id; V : Node_Id);

   function Naming_Node (N : Node_Id) return Node_Id;
   procedure Set_Naming_Node (N : Node_Id; V : Node_Id);

   function Job_Node (N : Node_Id) return Node_Id;
   procedure Set_Job_Node (N : Node_Id; V : Node_Id);

   function Enumerator_Node (N : Node_Id) return Node_Id;
   procedure Set_Enumerator_Node (N : Node_Id; V : Node_Id);

   function Marshall_Node (N : Node_Id) return Node_Id;
   procedure Set_Marshall_Node (N : Node_Id; V : Node_Id);

   function Unmarshall_Node (N : Node_Id) return Node_Id;
   procedure Set_Unmarshall_Node (N : Node_Id; V : Node_Id);

   function Port_Interface_Node (N : Node_Id) return Node_Id;
   procedure Set_Port_Interface_Node (N : Node_Id; V : Node_Id);

   function Port_Enumeration_Node (N : Node_Id) return Node_Id;
   procedure Set_Port_Enumeration_Node (N : Node_Id; V : Node_Id);

   function Deliver_Node (N : Node_Id) return Node_Id;
   procedure Set_Deliver_Node (N : Node_Id; V : Node_Id);

   function Send_Node (N : Node_Id) return Node_Id;
   procedure Set_Send_Node (N : Node_Id; V : Node_Id);

   function Put_Value_Node (N : Node_Id) return Node_Id;
   procedure Set_Put_Value_Node (N : Node_Id; V : Node_Id);

   function Get_Value_Node (N : Node_Id) return Node_Id;
   procedure Set_Get_Value_Node (N : Node_Id; V : Node_Id);

   function Get_Count_Node (N : Node_Id) return Node_Id;
   procedure Set_Get_Count_Node (N : Node_Id; V : Node_Id);

   function Next_Value_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Value_Node (N : Node_Id; V : Node_Id);

   function Store_Received_Message_Node (N : Node_Id) return Node_Id;
   procedure Set_Store_Received_Message_Node (N : Node_Id; V : Node_Id);

   function Default_Value_Node (N : Node_Id) return Node_Id;
   procedure Set_Default_Value_Node (N : Node_Id; V : Node_Id);

   function Object_Node (N : Node_Id) return Node_Id;
   procedure Set_Object_Node (N : Node_Id; V : Node_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 3) of Boolean;
   type Byte_Array is array (1 .. 1) of Byte;
   type Int_Array is array (1 .. 28) of Int;

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

end Ocarina.Backends.Ada_Tree.Nodes;
