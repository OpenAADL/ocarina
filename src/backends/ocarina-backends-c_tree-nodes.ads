pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.Backends.C_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_Definition,
      K_List_Id,
      K_Defining_Identifier,
      K_Ifdef_Clause,
      K_Include_Clause,
      K_Includes,
      K_Declaration_List,
      K_Header_List,
      K_Statement_List,
      K_Parameter_List,
      K_Enumeration_Literals,
      K_Element_List,
      K_Label_List,
      K_Alternatives_List,
      K_Sources,
      K_Headers,
      K_Array_Values,
      K_Array_Value,
      K_HI_Distributed_Application,
      K_HI_Node,
      K_Header_File,
      K_Source_File,
      K_API_Unit,
      K_HI_Unit,
      K_Parameter_Specification,
      K_Parameter_Profile,
      K_Function_Specification,
      K_Function_Implementation,
      K_Call_Profile,
      K_Macro_Call,
      K_Full_Type_Declaration,
      K_Block_Statement,
      K_If_Statement,
      K_Assignment_Statement,
      K_Return_Statement,
      K_For_Statement,
      K_While_Statement,
      K_Switch_Statement,
      K_Switch_Alternative,
      K_Break_Statement,
      K_Continue_Statement,
      K_C_Comment,
      K_Doxygen_C_Comment,
      K_Define_Statement,
      K_Array_Declaration,
      K_Struct_Aggregate,
      K_Union_Aggregate,
      K_Enum_Aggregate,
      K_Variable_Declaration,
      K_Member_Declaration,
      K_Member_Designator,
      K_Macro_Declaration,
      K_Literal,
      K_Expression,
      K_Type_Conversion,
      K_Base_Type,
      K_Pointer_Type,
      K_Constant_Type,
      K_Variable_Address,
      K_Extern_Entity_Declaration,
      K_Float,
      K_Int,
      K_Char,
      K_Short,
      K_Void,
      K_Pointed_Char,
      K_Tree_Bindings,
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
   --    Compile_Unit             : Node_Id
   --    Is_Pointer               : Boolean
   --

   procedure W_Defining_Identifier (N : Node_Id);

   --
   --  Ifdef_Clause
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Clause                   : Node_Id
   --    Then_Statements          : List_Id
   --    Else_Statements          : List_Id
   --    Negation                 : Boolean
   --

   procedure W_Ifdef_Clause (N : Node_Id);

   --
   --  Include_Clause
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Header_Name              : Node_Id
   --    Is_Local                 : Boolean
   --

   procedure W_Include_Clause (N : Node_Id);

   --
   --  Includes
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Includes (N : List_Id);

   --
   --  Declaration_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Declaration_List (N : List_Id);

   --
   --  Header_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Header_List (N : List_Id);

   --
   --  Statement_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Statement_List (N : List_Id);

   --
   --  Parameter_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Parameter_List (N : List_Id);

   --
   --  Enumeration_Literals
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Enumeration_Literals (N : List_Id);

   --
   --  Element_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Element_List (N : List_Id);

   --
   --  Label_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Label_List (N : List_Id);

   --
   --  Alternatives_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Alternatives_List (N : List_Id);

   --
   --  Sources
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Sources (N : List_Id);

   --
   --  Headers
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Headers (N : List_Id);

   --
   --  Array_Values
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Values                   : List_Id
   --

   procedure W_Array_Values (N : Node_Id);

   --
   --  Array_Value
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Array_Item               : Node_Id
   --

   procedure W_Array_Value (N : Node_Id);

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
   --  Header_File
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Distributed_Application_Unit: Node_Id
   --    Included_Headers         : List_Id
   --    Declarations             : List_Id
   --

   procedure W_Header_File (N : Node_Id);

   --
   --  Source_File
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Distributed_Application_Unit: Node_Id
   --    Included_Headers         : List_Id
   --    Declarations             : List_Id
   --

   procedure W_Source_File (N : Node_Id);

   --
   --  API_Unit
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Sources                  : List_Id
   --    Headers                  : List_Id
   --    Entity                   : Node_Id
   --

   procedure W_API_Unit (N : Node_Id);

   --
   --  HI_Unit
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Sources                  : List_Id
   --    Headers                  : List_Id
   --    Entity                   : Node_Id
   --    Main_Source              : Node_Id
   --    Main_Header              : Node_Id
   --    Activity_Source          : Node_Id
   --    Activity_Header          : Node_Id
   --    Marshallers_Source       : Node_Id
   --    Marshallers_Header       : Node_Id
   --    Request_Source           : Node_Id
   --    Request_Header           : Node_Id
   --    Types_Header             : Node_Id
   --    Types_Source             : Node_Id
   --    Deployment_Header        : Node_Id
   --    Deployment_Source        : Node_Id
   --    Naming_Source            : Node_Id
   --    Naming_Header            : Node_Id
   --    Subprograms_Source       : Node_Id
   --    Subprograms_Header       : Node_Id
   --

   procedure W_HI_Unit (N : Node_Id);

   --
   --  Parameter_Specification
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parameter_Type           : Node_Id
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
   --  Function_Specification
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parameters               : List_Id
   --    Return_Type              : Node_Id
   --    Compile_Unit             : Node_Id
   --

   procedure W_Function_Specification (N : Node_Id);

   --
   --  Function_Implementation
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Specification            : Node_Id
   --    Declarations             : List_Id
   --    Statements               : List_Id
   --    Compile_Unit             : Node_Id
   --

   procedure W_Function_Implementation (N : Node_Id);

   --
   --  Call_Profile
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parameters               : List_Id
   --

   procedure W_Call_Profile (N : Node_Id);

   --
   --  Macro_Call
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Parameters               : List_Id
   --

   procedure W_Macro_Call (N : Node_Id);

   --
   --  Full_Type_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Type_Definition          : Node_Id
   --    Type_Name                : Node_Id
   --

   procedure W_Full_Type_Declaration (N : Node_Id);

   --
   --  Block_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Declarative_Part         : List_Id
   --    Statements               : List_Id
   --

   procedure W_Block_Statement (N : Node_Id);

   --
   --  If_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Condition                : Node_Id
   --    Statements               : List_Id
   --    Else_Statements          : List_Id
   --

   procedure W_If_Statement (N : Node_Id);

   --
   --  Assignment_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Assignment_Statement (N : Node_Id);

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
   --    Pre_Cond                 : Node_Id
   --    Condition                : Node_Id
   --    Post_Cond                : Node_Id
   --    Statements               : List_Id
   --

   procedure W_For_Statement (N : Node_Id);

   --
   --  While_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Condition                : Node_Id
   --    Statements               : List_Id
   --

   procedure W_While_Statement (N : Node_Id);

   --
   --  Switch_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Expression               : Node_Id
   --    Alternatives             : List_Id
   --

   procedure W_Switch_Statement (N : Node_Id);

   --
   --  Switch_Alternative
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Labels                   : List_Id
   --    Statements               : List_Id
   --

   procedure W_Switch_Alternative (N : Node_Id);

   --
   --  Break_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --

   procedure W_Break_Statement (N : Node_Id);

   --
   --  Continue_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --

   procedure W_Continue_Statement (N : Node_Id);

   --
   --  C_Comment
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Has_Header_Spaces        : Boolean
   --

   procedure W_C_Comment (N : Node_Id);

   --
   --  Doxygen_C_Comment
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Has_Header_Spaces        : Boolean
   --    For_Struct               : Boolean
   --    For_Union                : Boolean
   --    For_Enum                 : Boolean
   --    For_Function             : Boolean
   --    For_Variable             : Boolean
   --    For_Define               : Boolean
   --    For_Typedef              : Boolean
   --    For_File                 : Boolean
   --    For_Namespace            : Boolean
   --    For_Package              : Boolean
   --    For_Interface            : Boolean
   --    Summary                  : Node_Id
   --    Description              : Node_Id
   --    Element                  : Node_Id
   --

   procedure W_Doxygen_C_Comment (N : Node_Id);

   --
   --  Define_Statement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Defined_Value            : Node_Id
   --

   procedure W_Define_Statement (N : Node_Id);

   --
   --  Array_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Array_Size               : Node_Id
   --

   procedure W_Array_Declaration (N : Node_Id);

   --
   --  Struct_Aggregate
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Struct_Members           : List_Id
   --

   procedure W_Struct_Aggregate (N : Node_Id);

   --
   --  Union_Aggregate
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Union_Members            : List_Id
   --

   procedure W_Union_Aggregate (N : Node_Id);

   --
   --  Enum_Aggregate
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Enum_Members             : List_Id
   --

   procedure W_Enum_Aggregate (N : Node_Id);

   --
   --  Variable_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Used_Type                : Node_Id
   --    Is_Static                : Boolean
   --

   procedure W_Variable_Declaration (N : Node_Id);

   --
   --  Member_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Used_Type                : Node_Id
   --

   procedure W_Member_Declaration (N : Node_Id);

   --
   --  Member_Designator
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Aggregate_Name           : Node_Id
   --    Is_Pointer               : Boolean
   --

   procedure W_Member_Designator (N : Node_Id);

   --
   --  Macro_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Macro_Declaration (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  Expression
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Operator                 : Operator_Id
   --    Left_Expression          : Node_Id
   --    Right_Expression         : Node_Id
   --

   procedure W_Expression (N : Node_Id);

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
   --  Base_Type
   --
   --    Image                    : Name_Id
   --

   --
   --  Pointer_Type
   --
   --    Used_Type                : Node_Id
   --

   --
   --  Constant_Type
   --
   --    Used_Type                : Node_Id
   --

   --
   --  Variable_Address
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Variable_Address (N : Node_Id);

   --
   --  Extern_Entity_Declaration
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Extern_Entity_Declaration (N : Node_Id);

   --
   --  Float
   --
   --    Image                    : Name_Id
   --

   procedure W_Float (N : Base_Type);

   --
   --  Int
   --
   --    Image                    : Name_Id
   --

   procedure W_Int (N : Base_Type);

   --
   --  Char
   --
   --    Image                    : Name_Id
   --

   procedure W_Char (N : Base_Type);

   --
   --  Short
   --
   --    Image                    : Name_Id
   --

   procedure W_Short (N : Base_Type);

   --
   --  Void
   --
   --    Image                    : Name_Id
   --

   procedure W_Void (N : Base_Type);

   --
   --  Pointed_Char
   --
   --    Image                    : Name_Id
   --

   procedure W_Pointed_Char (N : Base_Type);

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
   --  HI_Tree_Bindings
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Main_Node                : Node_Id
   --    Type_Definition_Node     : Node_Id
   --    Feature_Subprogram_Node  : Node_Id
   --    Subprogram_Node          : Node_Id
   --    Internals_Node           : Node_Id
   --    System_Time_Node         : Node_Id
   --    Marshaller_Node          : Node_Id
   --    Unmarshaller_Node        : Node_Id
   --    Activity_Node            : Node_Id
   --    Functions_Node           : Node_Id
   --    Types_Node               : Node_Id
   --    Object_Node              : Node_Id
   --    Handlers_Node            : Node_Id
   --    Global_Port_Node         : Node_Id
   --    Local_Port_Node          : Node_Id
   --    Global_Names_Node        : Node_Id
   --    Global_Model_Names_Node  : Node_Id
   --    Request_Node             : Node_Id
   --    Deployment_Node          : Node_Id
   --    Entities_Node            : Node_Id
   --    Servers_Node             : Node_Id
   --    Naming_Node              : Node_Id
   --    Processes                : List_Id
   --    Request_Type_Node        : Node_Id
   --    Job_Node                 : Node_Id
   --    Stub_Node                : Node_Id
   --    Enumerator_Node          : Node_Id
   --    Process_Request_Node     : Node_Id
   --    Marshall_Request_Node    : Node_Id
   --    Unmarshall_Request_Node  : Node_Id
   --    Default_Value_Node       : Node_Id
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

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Corresponding_Node (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id);

   function Compile_Unit (N : Node_Id) return Node_Id;
   procedure Set_Compile_Unit (N : Node_Id; V : Node_Id);

   function Is_Pointer (N : Node_Id) return Boolean;
   procedure Set_Is_Pointer (N : Node_Id; V : Boolean);

   function Clause (N : Node_Id) return Node_Id;
   procedure Set_Clause (N : Node_Id; V : Node_Id);

   function Then_Statements (N : Node_Id) return List_Id;
   procedure Set_Then_Statements (N : Node_Id; V : List_Id);

   function Else_Statements (N : Node_Id) return List_Id;
   procedure Set_Else_Statements (N : Node_Id; V : List_Id);

   function Negation (N : Node_Id) return Boolean;
   procedure Set_Negation (N : Node_Id; V : Boolean);

   function Header_Name (N : Node_Id) return Node_Id;
   procedure Set_Header_Name (N : Node_Id; V : Node_Id);

   function Is_Local (N : Node_Id) return Boolean;
   procedure Set_Is_Local (N : Node_Id; V : Boolean);

   function Values (N : Node_Id) return List_Id;
   procedure Set_Values (N : Node_Id; V : List_Id);

   function Array_Item (N : Node_Id) return Node_Id;
   procedure Set_Array_Item (N : Node_Id; V : Node_Id);

   function Units (N : Node_Id) return List_Id;
   procedure Set_Units (N : Node_Id; V : List_Id);

   function HI_Nodes (N : Node_Id) return List_Id;
   procedure Set_HI_Nodes (N : Node_Id; V : List_Id);

   function Distributed_Application (N : Node_Id) return Node_Id;
   procedure Set_Distributed_Application (N : Node_Id; V : Node_Id);

   function Distributed_Application_Unit (N : Node_Id) return Node_Id;
   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id);

   function Included_Headers (N : Node_Id) return List_Id;
   procedure Set_Included_Headers (N : Node_Id; V : List_Id);

   function Declarations (N : Node_Id) return List_Id;
   procedure Set_Declarations (N : Node_Id; V : List_Id);

   function Sources (N : Node_Id) return List_Id;
   procedure Set_Sources (N : Node_Id; V : List_Id);

   function Headers (N : Node_Id) return List_Id;
   procedure Set_Headers (N : Node_Id; V : List_Id);

   function Entity (N : Node_Id) return Node_Id;
   procedure Set_Entity (N : Node_Id; V : Node_Id);

   function Main_Source (N : Node_Id) return Node_Id;
   procedure Set_Main_Source (N : Node_Id; V : Node_Id);

   function Main_Header (N : Node_Id) return Node_Id;
   procedure Set_Main_Header (N : Node_Id; V : Node_Id);

   function Activity_Source (N : Node_Id) return Node_Id;
   procedure Set_Activity_Source (N : Node_Id; V : Node_Id);

   function Activity_Header (N : Node_Id) return Node_Id;
   procedure Set_Activity_Header (N : Node_Id; V : Node_Id);

   function Marshallers_Source (N : Node_Id) return Node_Id;
   procedure Set_Marshallers_Source (N : Node_Id; V : Node_Id);

   function Marshallers_Header (N : Node_Id) return Node_Id;
   procedure Set_Marshallers_Header (N : Node_Id; V : Node_Id);

   function Request_Source (N : Node_Id) return Node_Id;
   procedure Set_Request_Source (N : Node_Id; V : Node_Id);

   function Request_Header (N : Node_Id) return Node_Id;
   procedure Set_Request_Header (N : Node_Id; V : Node_Id);

   function Types_Header (N : Node_Id) return Node_Id;
   procedure Set_Types_Header (N : Node_Id; V : Node_Id);

   function Types_Source (N : Node_Id) return Node_Id;
   procedure Set_Types_Source (N : Node_Id; V : Node_Id);

   function Deployment_Header (N : Node_Id) return Node_Id;
   procedure Set_Deployment_Header (N : Node_Id; V : Node_Id);

   function Deployment_Source (N : Node_Id) return Node_Id;
   procedure Set_Deployment_Source (N : Node_Id; V : Node_Id);

   function Naming_Source (N : Node_Id) return Node_Id;
   procedure Set_Naming_Source (N : Node_Id; V : Node_Id);

   function Naming_Header (N : Node_Id) return Node_Id;
   procedure Set_Naming_Header (N : Node_Id; V : Node_Id);

   function Subprograms_Source (N : Node_Id) return Node_Id;
   procedure Set_Subprograms_Source (N : Node_Id; V : Node_Id);

   function Subprograms_Header (N : Node_Id) return Node_Id;
   procedure Set_Subprograms_Header (N : Node_Id; V : Node_Id);

   function Parameter_Type (N : Node_Id) return Node_Id;
   procedure Set_Parameter_Type (N : Node_Id; V : Node_Id);

   function Parameters (N : Node_Id) return List_Id;
   procedure Set_Parameters (N : Node_Id; V : List_Id);

   function Return_Type (N : Node_Id) return Node_Id;
   procedure Set_Return_Type (N : Node_Id; V : Node_Id);

   function Specification (N : Node_Id) return Node_Id;
   procedure Set_Specification (N : Node_Id; V : Node_Id);

   function Statements (N : Node_Id) return List_Id;
   procedure Set_Statements (N : Node_Id; V : List_Id);

   function Type_Definition (N : Node_Id) return Node_Id;
   procedure Set_Type_Definition (N : Node_Id; V : Node_Id);

   function Type_Name (N : Node_Id) return Node_Id;
   procedure Set_Type_Name (N : Node_Id; V : Node_Id);

   function Declarative_Part (N : Node_Id) return List_Id;
   procedure Set_Declarative_Part (N : Node_Id; V : List_Id);

   function Condition (N : Node_Id) return Node_Id;
   procedure Set_Condition (N : Node_Id; V : Node_Id);

   function Expression (N : Node_Id) return Node_Id;
   procedure Set_Expression (N : Node_Id; V : Node_Id);

   function Pre_Cond (N : Node_Id) return Node_Id;
   procedure Set_Pre_Cond (N : Node_Id; V : Node_Id);

   function Post_Cond (N : Node_Id) return Node_Id;
   procedure Set_Post_Cond (N : Node_Id; V : Node_Id);

   function Alternatives (N : Node_Id) return List_Id;
   procedure Set_Alternatives (N : Node_Id; V : List_Id);

   function Labels (N : Node_Id) return List_Id;
   procedure Set_Labels (N : Node_Id; V : List_Id);

   function Has_Header_Spaces (N : Node_Id) return Boolean;
   procedure Set_Has_Header_Spaces (N : Node_Id; V : Boolean);

   function For_Struct (N : Node_Id) return Boolean;
   procedure Set_For_Struct (N : Node_Id; V : Boolean);

   function For_Union (N : Node_Id) return Boolean;
   procedure Set_For_Union (N : Node_Id; V : Boolean);

   function For_Enum (N : Node_Id) return Boolean;
   procedure Set_For_Enum (N : Node_Id; V : Boolean);

   function For_Function (N : Node_Id) return Boolean;
   procedure Set_For_Function (N : Node_Id; V : Boolean);

   function For_Variable (N : Node_Id) return Boolean;
   procedure Set_For_Variable (N : Node_Id; V : Boolean);

   function For_Define (N : Node_Id) return Boolean;
   procedure Set_For_Define (N : Node_Id; V : Boolean);

   function For_Typedef (N : Node_Id) return Boolean;
   procedure Set_For_Typedef (N : Node_Id; V : Boolean);

   function For_File (N : Node_Id) return Boolean;
   procedure Set_For_File (N : Node_Id; V : Boolean);

   function For_Namespace (N : Node_Id) return Boolean;
   procedure Set_For_Namespace (N : Node_Id; V : Boolean);

   function For_Package (N : Node_Id) return Boolean;
   procedure Set_For_Package (N : Node_Id; V : Boolean);

   function For_Interface (N : Node_Id) return Boolean;
   procedure Set_For_Interface (N : Node_Id; V : Boolean);

   function Summary (N : Node_Id) return Node_Id;
   procedure Set_Summary (N : Node_Id; V : Node_Id);

   function Description (N : Node_Id) return Node_Id;
   procedure Set_Description (N : Node_Id; V : Node_Id);

   function Element (N : Node_Id) return Node_Id;
   procedure Set_Element (N : Node_Id; V : Node_Id);

   function Defined_Value (N : Node_Id) return Node_Id;
   procedure Set_Defined_Value (N : Node_Id; V : Node_Id);

   function Array_Size (N : Node_Id) return Node_Id;
   procedure Set_Array_Size (N : Node_Id; V : Node_Id);

   function Struct_Members (N : Node_Id) return List_Id;
   procedure Set_Struct_Members (N : Node_Id; V : List_Id);

   function Union_Members (N : Node_Id) return List_Id;
   procedure Set_Union_Members (N : Node_Id; V : List_Id);

   function Enum_Members (N : Node_Id) return List_Id;
   procedure Set_Enum_Members (N : Node_Id; V : List_Id);

   function Used_Type (N : Node_Id) return Node_Id;
   procedure Set_Used_Type (N : Node_Id; V : Node_Id);

   function Is_Static (N : Node_Id) return Boolean;
   procedure Set_Is_Static (N : Node_Id; V : Boolean);

   function Aggregate_Name (N : Node_Id) return Node_Id;
   procedure Set_Aggregate_Name (N : Node_Id; V : Node_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function Operator (N : Node_Id) return Operator_Id;
   procedure Set_Operator (N : Node_Id; V : Operator_Id);

   function Left_Expression (N : Node_Id) return Node_Id;
   procedure Set_Left_Expression (N : Node_Id; V : Node_Id);

   function Right_Expression (N : Node_Id) return Node_Id;
   procedure Set_Right_Expression (N : Node_Id; V : Node_Id);

   function Subtype_Mark (N : Node_Id) return Node_Id;
   procedure Set_Subtype_Mark (N : Node_Id; V : Node_Id);

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

   function Internals_Node (N : Node_Id) return Node_Id;
   procedure Set_Internals_Node (N : Node_Id; V : Node_Id);

   function System_Time_Node (N : Node_Id) return Node_Id;
   procedure Set_System_Time_Node (N : Node_Id; V : Node_Id);

   function Marshaller_Node (N : Node_Id) return Node_Id;
   procedure Set_Marshaller_Node (N : Node_Id; V : Node_Id);

   function Unmarshaller_Node (N : Node_Id) return Node_Id;
   procedure Set_Unmarshaller_Node (N : Node_Id; V : Node_Id);

   function Activity_Node (N : Node_Id) return Node_Id;
   procedure Set_Activity_Node (N : Node_Id; V : Node_Id);

   function Functions_Node (N : Node_Id) return Node_Id;
   procedure Set_Functions_Node (N : Node_Id; V : Node_Id);

   function Types_Node (N : Node_Id) return Node_Id;
   procedure Set_Types_Node (N : Node_Id; V : Node_Id);

   function Object_Node (N : Node_Id) return Node_Id;
   procedure Set_Object_Node (N : Node_Id; V : Node_Id);

   function Handlers_Node (N : Node_Id) return Node_Id;
   procedure Set_Handlers_Node (N : Node_Id; V : Node_Id);

   function Global_Port_Node (N : Node_Id) return Node_Id;
   procedure Set_Global_Port_Node (N : Node_Id; V : Node_Id);

   function Local_Port_Node (N : Node_Id) return Node_Id;
   procedure Set_Local_Port_Node (N : Node_Id; V : Node_Id);

   function Global_Names_Node (N : Node_Id) return Node_Id;
   procedure Set_Global_Names_Node (N : Node_Id; V : Node_Id);

   function Global_Model_Names_Node (N : Node_Id) return Node_Id;
   procedure Set_Global_Model_Names_Node (N : Node_Id; V : Node_Id);

   function Request_Node (N : Node_Id) return Node_Id;
   procedure Set_Request_Node (N : Node_Id; V : Node_Id);

   function Deployment_Node (N : Node_Id) return Node_Id;
   procedure Set_Deployment_Node (N : Node_Id; V : Node_Id);

   function Entities_Node (N : Node_Id) return Node_Id;
   procedure Set_Entities_Node (N : Node_Id; V : Node_Id);

   function Servers_Node (N : Node_Id) return Node_Id;
   procedure Set_Servers_Node (N : Node_Id; V : Node_Id);

   function Naming_Node (N : Node_Id) return Node_Id;
   procedure Set_Naming_Node (N : Node_Id; V : Node_Id);

   function Processes (N : Node_Id) return List_Id;
   procedure Set_Processes (N : Node_Id; V : List_Id);

   function Request_Type_Node (N : Node_Id) return Node_Id;
   procedure Set_Request_Type_Node (N : Node_Id; V : Node_Id);

   function Job_Node (N : Node_Id) return Node_Id;
   procedure Set_Job_Node (N : Node_Id; V : Node_Id);

   function Stub_Node (N : Node_Id) return Node_Id;
   procedure Set_Stub_Node (N : Node_Id; V : Node_Id);

   function Enumerator_Node (N : Node_Id) return Node_Id;
   procedure Set_Enumerator_Node (N : Node_Id; V : Node_Id);

   function Process_Request_Node (N : Node_Id) return Node_Id;
   procedure Set_Process_Request_Node (N : Node_Id; V : Node_Id);

   function Marshall_Request_Node (N : Node_Id) return Node_Id;
   procedure Set_Marshall_Request_Node (N : Node_Id; V : Node_Id);

   function Unmarshall_Request_Node (N : Node_Id) return Node_Id;
   procedure Set_Unmarshall_Request_Node (N : Node_Id; V : Node_Id);

   function Default_Value_Node (N : Node_Id) return Node_Id;
   procedure Set_Default_Value_Node (N : Node_Id; V : Node_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 12) of Boolean;
   type Byte_Array is array (1 .. 1) of Byte;
   type Int_Array is array (1 .. 33) of Int;

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

end Ocarina.Backends.C_Tree.Nodes;
