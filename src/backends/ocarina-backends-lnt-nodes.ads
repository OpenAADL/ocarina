pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.Backends.LNT.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_Node_Container,
      K_List_Id,
      K_Identifier,
      K_Module_Definition,
      K_Predefined_Function,
      K_Equality,
      K_Inequality,
      K_Less_Than,
      K_Less_Than_Or_Equal_To,
      K_Greater_Than,
      K_Greater_Than_Or_Equal_To,
      K_Ordinal,
      K_Value,
      K_Field_Selection,
      K_Field_Update,
      K_Type_Cardinality,
      K_First_Element,
      K_Last_Element,
      K_And,
      K_Or,
      K_Module_Pragma,
      K_Nat_Bits,
      K_Type_Def,
      K_Type_Exp,
      K_RangeLNT,
      K_Base_Type,
      K_Float,
      K_Nat,
      K_Int,
      K_Char,
      K_String,
      K_Type_Constructor,
      K_Parameter_Specification,
      K_Function_Definition,
      K_Actual_Parameter,
      K_Null_Statement,
      K_Return_Statement,
      K_Assignment_Statement,
      K_Array_Element_Assignment_Statement,
      K_Var_Statement,
      K_Var_Declaration,
      K_Case_Statement,
      K_Case_Statement_Alternative,
      K_If_Statement,
      K_Elsif_Statement,
      K_Loop_Statement,
      K_While_Statement,
      K_Break_Statement,
      K_Call,
      K_Infix_Call,
      K_List_Of,
      K_Parenthesized,
      K_Expressions,
      K_Parenthesized_Expression,
      K_Function_Call_Expression,
      K_Infix_Function_Call_Expression,
      K_Field_Selection_Expression,
      K_Field_Update_Expression,
      K_Element_Association,
      K_Array_Elt_Access_Expression,
      K_Pattern,
      K_Constructed_Pattern,
      K_Patterns,
      K_Parenthesized_Pattern,
      K_Constant_Pattern_Infixed,
      K_Channel,
      K_Gate_Profile,
      K_Process_Definition,
      K_Gate_Declaration,
      K_Stop_Statement,
      K_Process_Instantiation_Statement,
      K_Communication_Statement,
      K_Offer_Statement,
      K_Select_Statement,
      K_Select_Statement_Alternative,
      K_Parallel_Composition_Statement,
      K_Interface_Synchronisation,
      K_Hide_Statement);

   --
   --  Node_Id
   --
   --    Next_Node                : Node_Id
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
   --  List_Id
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   --
   --  Identifier
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Ocarina_Node             : Node_Id
   --    Corresponding_Entity     : Node_Id
   --

   procedure W_Identifier (N : Node_Id);

   --
   --  Module_Definition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Modules                  : List_Id
   --    Predefined_Functions     : List_Id
   --    Module_Pragmas           : List_Id
   --    Definitions              : List_Id
   --

   procedure W_Module_Definition (N : Node_Id);

   --
   --  Predefined_Function
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Predefined_Function (N : Node_Id);

   --
   --  Equality
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Equality (N : Node_Id);

   --
   --  Inequality
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Inequality (N : Node_Id);

   --
   --  Less_Than
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Less_Than (N : Node_Id);

   --
   --  Less_Than_Or_Equal_To
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Less_Than_Or_Equal_To (N : Node_Id);

   --
   --  Greater_Than
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Greater_Than (N : Node_Id);

   --
   --  Greater_Than_Or_Equal_To
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Greater_Than_Or_Equal_To (N : Node_Id);

   --
   --  Ordinal
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Ordinal (N : Node_Id);

   --
   --  Value
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Value (N : Node_Id);

   --
   --  Field_Selection
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Field_Selection (N : Node_Id);

   --
   --  Field_Update
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Field_Update (N : Node_Id);

   --
   --  Type_Cardinality
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Type_Cardinality (N : Node_Id);

   --
   --  First_Element
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_First_Element (N : Node_Id);

   --
   --  Last_Element
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Last_Element (N : Node_Id);

   --
   --  And
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_And (N : Node_Id);

   --
   --  Or
   --
   --    Next_Node                : Node_Id
   --    LNT_Function             : Node_Id
   --    Is_With_Clause           : Boolean
   --

   procedure W_Or (N : Node_Id);

   --
   --  Module_Pragma
   --
   --    Next_Node                : Node_Id
   --    Module_Pragma_Type       : Node_Id
   --    Number_Constant          : Node_Id
   --

   procedure W_Module_Pragma (N : Node_Id);

   --
   --  Nat_Bits
   --
   --    Next_Node                : Node_Id
   --    Module_Pragma_Type       : Node_Id
   --    Number_Constant          : Node_Id
   --

   procedure W_Nat_Bits (N : Node_Id);

   --
   --  Type_Def
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Type_Exp                 : Node_Id
   --    Type_Pragma              : List_Id
   --    Predefined_Functions     : List_Id
   --

   procedure W_Type_Def (N : Node_Id);

   --
   --  Type_Exp
   --
   --    Next_Node                : Node_Id
   --    Type_Constructors        : List_Id
   --    Identifier               : Node_Id
   --    Is_Set                   : Boolean
   --    Is_Sorted_Set            : Boolean
   --    Is_List                  : Boolean
   --    Is_Sorted_List           : Boolean
   --    Is_Array                 : Boolean
   --    Is_Range                 : Boolean
   --    RangeLNT                 : Node_Id
   --

   procedure W_Type_Exp (N : Node_Id);

   --
   --  RangeLNT
   --
   --    Next_Node                : Node_Id
   --    Low_Bound                : Node_Id
   --    High_Bound               : Node_Id
   --

   procedure W_RangeLNT (N : Node_Id);

   --
   --  Base_Type
   --
   --    Next_Node                : Node_Id
   --    Type_Name                : Node_Id
   --    Image                    : Name_Id
   --

   procedure W_Base_Type (N : Node_Id);

   --
   --  Float
   --
   --    Next_Node                : Node_Id
   --    Type_Name                : Node_Id
   --    Image                    : Name_Id
   --

   procedure W_Float (N : Node_Id);

   --
   --  Nat
   --
   --    Next_Node                : Node_Id
   --    Type_Name                : Node_Id
   --    Image                    : Name_Id
   --

   procedure W_Nat (N : Node_Id);

   --
   --  Int
   --
   --    Next_Node                : Node_Id
   --    Type_Name                : Node_Id
   --    Image                    : Name_Id
   --

   procedure W_Int (N : Node_Id);

   --
   --  Char
   --
   --    Next_Node                : Node_Id
   --    Type_Name                : Node_Id
   --    Image                    : Name_Id
   --

   procedure W_Char (N : Node_Id);

   --
   --  String
   --
   --    Next_Node                : Node_Id
   --    Type_Name                : Node_Id
   --    Image                    : Name_Id
   --

   procedure W_String (N : Node_Id);

   --
   --  Type_Constructor
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Constructor_Parameters   : List_Id
   --    Constructor_Pragma       : List_Id
   --

   procedure W_Type_Constructor (N : Node_Id);

   --
   --  Parameter_Specification
   --
   --    Next_Node                : Node_Id
   --    Parameter_Mode           : Mode_Id
   --    Parameter_Var            : Node_Id
   --    Parameter_Type           : Node_Id
   --

   procedure W_Parameter_Specification (N : Node_Id);

   --
   --  Function_Definition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Function_Parameters      : List_Id
   --    Function_Return_Type     : Node_Id
   --    Function_Exceptions      : List_Id
   --    Function_Pragma          : List_Id
   --    Statements               : List_Id
   --

   procedure W_Function_Definition (N : Node_Id);

   --
   --  Actual_Parameter
   --
   --    Next_Node                : Node_Id
   --    Expression               : Node_Id
   --    Is_Out                   : Boolean
   --    Is_InOut                 : Boolean
   --

   procedure W_Actual_Parameter (N : Node_Id);

   --
   --  Null_Statement
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Null_Statement (N : Node_Id);

   --
   --  Return_Statement
   --
   --    Next_Node                : Node_Id
   --    Is_Function              : Boolean
   --    Expression               : Node_Id
   --

   procedure W_Return_Statement (N : Node_Id);

   --
   --  Assignment_Statement
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Assignment_Statement (N : Node_Id);

   --
   --  Array_Element_Assignment_Statement
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Expression_Index         : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Array_Element_Assignment_Statement (N : Node_Id);

   --
   --  Var_Statement
   --
   --    Next_Node                : Node_Id
   --    Variable_Declarations    : List_Id
   --    Statements               : List_Id
   --

   procedure W_Var_Statement (N : Node_Id);

   --
   --  Var_Declaration
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Var_Type                 : Node_Id
   --

   procedure W_Var_Declaration (N : Node_Id);

   --
   --  Case_Statement
   --
   --    Next_Node                : Node_Id
   --    Expression               : Node_Id
   --    Variable_Declarations    : List_Id
   --    Case_Statement_Alternatives: List_Id
   --

   procedure W_Case_Statement (N : Node_Id);

   --
   --  Case_Statement_Alternative
   --
   --    Next_Node                : Node_Id
   --    Pattern_List             : List_Id
   --    Statements               : List_Id
   --

   procedure W_Case_Statement_Alternative (N : Node_Id);

   --
   --  If_Statement
   --
   --    Next_Node                : Node_Id
   --    Condition                : Node_Id
   --    Then_Statements          : List_Id
   --    Elsif_Statements         : List_Id
   --    Else_Statements          : List_Id
   --

   procedure W_If_Statement (N : Node_Id);

   --
   --  Elsif_Statement
   --
   --    Next_Node                : Node_Id
   --    Condition                : Node_Id
   --    Then_Statements          : List_Id
   --

   procedure W_Elsif_Statement (N : Node_Id);

   --
   --  Loop_Statement
   --
   --    Next_Node                : Node_Id
   --    Loop_Label               : Name_Id
   --    Statements               : List_Id
   --

   procedure W_Loop_Statement (N : Node_Id);

   --
   --  While_Statement
   --
   --    Next_Node                : Node_Id
   --    Expression               : Node_Id
   --    Statements               : List_Id
   --

   procedure W_While_Statement (N : Node_Id);

   --
   --  Break_Statement
   --
   --    Next_Node                : Node_Id
   --    Loop_Label               : Name_Id
   --

   procedure W_Break_Statement (N : Node_Id);

   --
   --  Call
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameters               : List_Id
   --

   procedure W_Call (N : Node_Id);

   --
   --  Infix_Call
   --
   --    Next_Node                : Node_Id
   --    Operator                 : Node_Id
   --    Left_Part                : Node_Id
   --    Right_Part               : Node_Id
   --

   procedure W_Infix_Call (N : Node_Id);

   --
   --  List_Of
   --
   --    Next_Node                : Node_Id
   --    The_List                 : List_Id
   --

   procedure W_List_Of (N : Node_Id);

   --
   --  Parenthesized
   --
   --    Next_Node                : Node_Id
   --    Variable                 : Node_Id
   --

   procedure W_Parenthesized (N : Node_Id);

   --
   --  Expressions
   --
   --    Next_Node                : Node_Id
   --    The_List                 : List_Id
   --

   procedure W_Expressions (N : Node_Id);

   --
   --  Parenthesized_Expression
   --
   --    Next_Node                : Node_Id
   --    Variable                 : Node_Id
   --

   procedure W_Parenthesized_Expression (N : Node_Id);

   --
   --  Function_Call_Expression
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameters               : List_Id
   --

   procedure W_Function_Call_Expression (N : Node_Id);

   --
   --  Infix_Function_Call_Expression
   --
   --    Next_Node                : Node_Id
   --    Operator                 : Node_Id
   --    Left_Part                : Node_Id
   --    Right_Part               : Node_Id
   --

   procedure W_Infix_Function_Call_Expression (N : Node_Id);

   --
   --  Field_Selection_Expression
   --
   --    Next_Node                : Node_Id
   --    Expression               : Node_Id
   --    Field                    : Node_Id
   --

   procedure W_Field_Selection_Expression (N : Node_Id);

   --
   --  Field_Update_Expression
   --
   --    Next_Node                : Node_Id
   --    Expression               : Node_Id
   --    Field_Association        : List_Id
   --

   procedure W_Field_Update_Expression (N : Node_Id);

   --
   --  Element_Association
   --
   --    Next_Node                : Node_Id
   --    Field                    : Node_Id
   --    Expression               : Node_Id
   --

   procedure W_Element_Association (N : Node_Id);

   --
   --  Array_Elt_Access_Expression
   --
   --    Next_Node                : Node_Id
   --    Expression               : Node_Id
   --    Index                    : Node_Id
   --

   procedure W_Array_Elt_Access_Expression (N : Node_Id);

   --
   --  Pattern
   --
   --    Next_Node                : Node_Id
   --    Sub_Pattern              : Node_Id
   --    Pattern_Type             : Node_Id
   --    Is_Any                   : Boolean
   --    Is_Of                    : Boolean
   --

   procedure W_Pattern (N : Node_Id);

   --
   --  Constructed_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameters               : List_Id
   --

   procedure W_Constructed_Pattern (N : Node_Id);

   --
   --  Patterns
   --
   --    Next_Node                : Node_Id
   --    The_List                 : List_Id
   --

   procedure W_Patterns (N : Node_Id);

   --
   --  Parenthesized_Pattern
   --
   --    Next_Node                : Node_Id
   --    Variable                 : Node_Id
   --

   procedure W_Parenthesized_Pattern (N : Node_Id);

   --
   --  Constant_Pattern_Infixed
   --
   --    Next_Node                : Node_Id
   --    Operator                 : Node_Id
   --    Left_Part                : Node_Id
   --    Right_Part               : Node_Id
   --

   procedure W_Constant_Pattern_Infixed (N : Node_Id);

   --
   --  Channel
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Gate_Profiles            : List_Id
   --

   procedure W_Channel (N : Node_Id);

   --
   --  Gate_Profile
   --
   --    Next_Node                : Node_Id
   --    Gate_Types               : List_Id
   --

   procedure W_Gate_Profile (N : Node_Id);

   --
   --  Process_Definition
   --
   --    Next_Node                : Node_Id
   --    Corresponding_Component  : Node_Id
   --    Identifier               : Node_Id
   --    Process_Gate_Declarations: List_Id
   --    Process_Parameters       : List_Id
   --    Process_Exceptions       : List_Id
   --    Process_Pragma           : List_Id
   --    Statements               : List_Id
   --

   procedure W_Process_Definition (N : Node_Id);

   --
   --  Gate_Declaration
   --
   --    Next_Node                : Node_Id
   --    Channel_Name             : Node_Id
   --    Gate                     : Node_Id
   --    Is_Any                   : Boolean
   --

   procedure W_Gate_Declaration (N : Node_Id);

   --
   --  Stop_Statement
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Stop_Statement (N : Node_Id);

   --
   --  Process_Instantiation_Statement
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Actual_Gates             : List_Id
   --    Actual_Parameters        : List_Id
   --    Is_Sporadic              : Boolean
   --

   procedure W_Process_Instantiation_Statement (N : Node_Id);

   --
   --  Communication_Statement
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Offers                   : List_Id
   --    Has_Where                : Boolean
   --    Expression               : Node_Id
   --

   procedure W_Communication_Statement (N : Node_Id);

   --
   --  Offer_Statement
   --
   --    Next_Node                : Node_Id
   --    Is_Input                 : Boolean
   --    Expression               : Node_Id
   --    Pattern                  : Node_Id
   --

   procedure W_Offer_Statement (N : Node_Id);

   --
   --  Select_Statement
   --
   --    Next_Node                : Node_Id
   --    Select_Statement_Alternatives: List_Id
   --

   procedure W_Select_Statement (N : Node_Id);

   --
   --  Select_Statement_Alternative
   --
   --    Next_Node                : Node_Id
   --    Statements               : List_Id
   --

   procedure W_Select_Statement_Alternative (N : Node_Id);

   --
   --  Parallel_Composition_Statement
   --
   --    Next_Node                : Node_Id
   --    Global_Synchronisation_Gates: List_Id
   --    Interface_Synchronisations: List_Id
   --

   procedure W_Parallel_Composition_Statement (N : Node_Id);

   --
   --  Interface_Synchronisation
   --
   --    Next_Node                : Node_Id
   --    Interface_Synchronisation_Gates: List_Id
   --    Statements               : List_Id
   --

   procedure W_Interface_Synchronisation (N : Node_Id);

   --
   --  Hide_Statement
   --
   --    Next_Node                : Node_Id
   --    Hide_Gate_Declarations   : List_Id
   --    Statements               : List_Id
   --

   procedure W_Hide_Statement (N : Node_Id);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function Item (N : Node_Id) return Node_Id;
   procedure Set_Item (N : Node_Id; V : Node_Id);

   function Extra_Item (N : Node_Id) return Node_Id;
   procedure Set_Extra_Item (N : Node_Id; V : Node_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Ocarina_Node (N : Node_Id) return Node_Id;
   procedure Set_Ocarina_Node (N : Node_Id; V : Node_Id);

   function Corresponding_Entity (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id);

   function Identifier (N : Node_Id) return Node_Id;
   procedure Set_Identifier (N : Node_Id; V : Node_Id);

   function Modules (N : Node_Id) return List_Id;
   procedure Set_Modules (N : Node_Id; V : List_Id);

   function Predefined_Functions (N : Node_Id) return List_Id;
   procedure Set_Predefined_Functions (N : Node_Id; V : List_Id);

   function Module_Pragmas (N : Node_Id) return List_Id;
   procedure Set_Module_Pragmas (N : Node_Id; V : List_Id);

   function Definitions (N : Node_Id) return List_Id;
   procedure Set_Definitions (N : Node_Id; V : List_Id);

   function LNT_Function (N : Node_Id) return Node_Id;
   procedure Set_LNT_Function (N : Node_Id; V : Node_Id);

   function Is_With_Clause (N : Node_Id) return Boolean;
   procedure Set_Is_With_Clause (N : Node_Id; V : Boolean);

   function Module_Pragma_Type (N : Node_Id) return Node_Id;
   procedure Set_Module_Pragma_Type (N : Node_Id; V : Node_Id);

   function Number_Constant (N : Node_Id) return Node_Id;
   procedure Set_Number_Constant (N : Node_Id; V : Node_Id);

   function Type_Exp (N : Node_Id) return Node_Id;
   procedure Set_Type_Exp (N : Node_Id; V : Node_Id);

   function Type_Pragma (N : Node_Id) return List_Id;
   procedure Set_Type_Pragma (N : Node_Id; V : List_Id);

   function Type_Constructors (N : Node_Id) return List_Id;
   procedure Set_Type_Constructors (N : Node_Id; V : List_Id);

   function Is_Set (N : Node_Id) return Boolean;
   procedure Set_Is_Set (N : Node_Id; V : Boolean);

   function Is_Sorted_Set (N : Node_Id) return Boolean;
   procedure Set_Is_Sorted_Set (N : Node_Id; V : Boolean);

   function Is_List (N : Node_Id) return Boolean;
   procedure Set_Is_List (N : Node_Id; V : Boolean);

   function Is_Sorted_List (N : Node_Id) return Boolean;
   procedure Set_Is_Sorted_List (N : Node_Id; V : Boolean);

   function Is_Array (N : Node_Id) return Boolean;
   procedure Set_Is_Array (N : Node_Id; V : Boolean);

   function Is_Range (N : Node_Id) return Boolean;
   procedure Set_Is_Range (N : Node_Id; V : Boolean);

   function RangeLNT (N : Node_Id) return Node_Id;
   procedure Set_RangeLNT (N : Node_Id; V : Node_Id);

   function Low_Bound (N : Node_Id) return Node_Id;
   procedure Set_Low_Bound (N : Node_Id; V : Node_Id);

   function High_Bound (N : Node_Id) return Node_Id;
   procedure Set_High_Bound (N : Node_Id; V : Node_Id);

   function Type_Name (N : Node_Id) return Node_Id;
   procedure Set_Type_Name (N : Node_Id; V : Node_Id);

   function Image (N : Node_Id) return Name_Id;
   procedure Set_Image (N : Node_Id; V : Name_Id);

   function Constructor_Parameters (N : Node_Id) return List_Id;
   procedure Set_Constructor_Parameters (N : Node_Id; V : List_Id);

   function Constructor_Pragma (N : Node_Id) return List_Id;
   procedure Set_Constructor_Pragma (N : Node_Id; V : List_Id);

   function Parameter_Mode (N : Node_Id) return Mode_Id;
   procedure Set_Parameter_Mode (N : Node_Id; V : Mode_Id);

   function Parameter_Var (N : Node_Id) return Node_Id;
   procedure Set_Parameter_Var (N : Node_Id; V : Node_Id);

   function Parameter_Type (N : Node_Id) return Node_Id;
   procedure Set_Parameter_Type (N : Node_Id; V : Node_Id);

   function Function_Parameters (N : Node_Id) return List_Id;
   procedure Set_Function_Parameters (N : Node_Id; V : List_Id);

   function Function_Return_Type (N : Node_Id) return Node_Id;
   procedure Set_Function_Return_Type (N : Node_Id; V : Node_Id);

   function Function_Exceptions (N : Node_Id) return List_Id;
   procedure Set_Function_Exceptions (N : Node_Id; V : List_Id);

   function Function_Pragma (N : Node_Id) return List_Id;
   procedure Set_Function_Pragma (N : Node_Id; V : List_Id);

   function Statements (N : Node_Id) return List_Id;
   procedure Set_Statements (N : Node_Id; V : List_Id);

   function Expression (N : Node_Id) return Node_Id;
   procedure Set_Expression (N : Node_Id; V : Node_Id);

   function Is_Out (N : Node_Id) return Boolean;
   procedure Set_Is_Out (N : Node_Id; V : Boolean);

   function Is_InOut (N : Node_Id) return Boolean;
   procedure Set_Is_InOut (N : Node_Id; V : Boolean);

   function Is_Function (N : Node_Id) return Boolean;
   procedure Set_Is_Function (N : Node_Id; V : Boolean);

   function Expression_Index (N : Node_Id) return Node_Id;
   procedure Set_Expression_Index (N : Node_Id; V : Node_Id);

   function Variable_Declarations (N : Node_Id) return List_Id;
   procedure Set_Variable_Declarations (N : Node_Id; V : List_Id);

   function Var_Type (N : Node_Id) return Node_Id;
   procedure Set_Var_Type (N : Node_Id; V : Node_Id);

   function Case_Statement_Alternatives (N : Node_Id) return List_Id;
   procedure Set_Case_Statement_Alternatives (N : Node_Id; V : List_Id);

   function Pattern_List (N : Node_Id) return List_Id;
   procedure Set_Pattern_List (N : Node_Id; V : List_Id);

   function Condition (N : Node_Id) return Node_Id;
   procedure Set_Condition (N : Node_Id; V : Node_Id);

   function Then_Statements (N : Node_Id) return List_Id;
   procedure Set_Then_Statements (N : Node_Id; V : List_Id);

   function Elsif_Statements (N : Node_Id) return List_Id;
   procedure Set_Elsif_Statements (N : Node_Id; V : List_Id);

   function Else_Statements (N : Node_Id) return List_Id;
   procedure Set_Else_Statements (N : Node_Id; V : List_Id);

   function Loop_Label (N : Node_Id) return Name_Id;
   procedure Set_Loop_Label (N : Node_Id; V : Name_Id);

   function Parameters (N : Node_Id) return List_Id;
   procedure Set_Parameters (N : Node_Id; V : List_Id);

   function Operator (N : Node_Id) return Node_Id;
   procedure Set_Operator (N : Node_Id; V : Node_Id);

   function Left_Part (N : Node_Id) return Node_Id;
   procedure Set_Left_Part (N : Node_Id; V : Node_Id);

   function Right_Part (N : Node_Id) return Node_Id;
   procedure Set_Right_Part (N : Node_Id; V : Node_Id);

   function The_List (N : Node_Id) return List_Id;
   procedure Set_The_List (N : Node_Id; V : List_Id);

   function Variable (N : Node_Id) return Node_Id;
   procedure Set_Variable (N : Node_Id; V : Node_Id);

   function Field (N : Node_Id) return Node_Id;
   procedure Set_Field (N : Node_Id; V : Node_Id);

   function Field_Association (N : Node_Id) return List_Id;
   procedure Set_Field_Association (N : Node_Id; V : List_Id);

   function Index (N : Node_Id) return Node_Id;
   procedure Set_Index (N : Node_Id; V : Node_Id);

   function Sub_Pattern (N : Node_Id) return Node_Id;
   procedure Set_Sub_Pattern (N : Node_Id; V : Node_Id);

   function Pattern_Type (N : Node_Id) return Node_Id;
   procedure Set_Pattern_Type (N : Node_Id; V : Node_Id);

   function Is_Any (N : Node_Id) return Boolean;
   procedure Set_Is_Any (N : Node_Id; V : Boolean);

   function Is_Of (N : Node_Id) return Boolean;
   procedure Set_Is_Of (N : Node_Id; V : Boolean);

   function Gate_Profiles (N : Node_Id) return List_Id;
   procedure Set_Gate_Profiles (N : Node_Id; V : List_Id);

   function Gate_Types (N : Node_Id) return List_Id;
   procedure Set_Gate_Types (N : Node_Id; V : List_Id);

   function Corresponding_Component (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Component (N : Node_Id; V : Node_Id);

   function Process_Gate_Declarations (N : Node_Id) return List_Id;
   procedure Set_Process_Gate_Declarations (N : Node_Id; V : List_Id);

   function Process_Parameters (N : Node_Id) return List_Id;
   procedure Set_Process_Parameters (N : Node_Id; V : List_Id);

   function Process_Exceptions (N : Node_Id) return List_Id;
   procedure Set_Process_Exceptions (N : Node_Id; V : List_Id);

   function Process_Pragma (N : Node_Id) return List_Id;
   procedure Set_Process_Pragma (N : Node_Id; V : List_Id);

   function Channel_Name (N : Node_Id) return Node_Id;
   procedure Set_Channel_Name (N : Node_Id; V : Node_Id);

   function Gate (N : Node_Id) return Node_Id;
   procedure Set_Gate (N : Node_Id; V : Node_Id);

   function Actual_Gates (N : Node_Id) return List_Id;
   procedure Set_Actual_Gates (N : Node_Id; V : List_Id);

   function Actual_Parameters (N : Node_Id) return List_Id;
   procedure Set_Actual_Parameters (N : Node_Id; V : List_Id);

   function Is_Sporadic (N : Node_Id) return Boolean;
   procedure Set_Is_Sporadic (N : Node_Id; V : Boolean);

   function Offers (N : Node_Id) return List_Id;
   procedure Set_Offers (N : Node_Id; V : List_Id);

   function Has_Where (N : Node_Id) return Boolean;
   procedure Set_Has_Where (N : Node_Id; V : Boolean);

   function Is_Input (N : Node_Id) return Boolean;
   procedure Set_Is_Input (N : Node_Id; V : Boolean);

   function Pattern (N : Node_Id) return Node_Id;
   procedure Set_Pattern (N : Node_Id; V : Node_Id);

   function Select_Statement_Alternatives (N : Node_Id) return List_Id;
   procedure Set_Select_Statement_Alternatives (N : Node_Id; V : List_Id);

   function Global_Synchronisation_Gates (N : Node_Id) return List_Id;
   procedure Set_Global_Synchronisation_Gates (N : Node_Id; V : List_Id);

   function Interface_Synchronisations (N : Node_Id) return List_Id;
   procedure Set_Interface_Synchronisations (N : Node_Id; V : List_Id);

   function Interface_Synchronisation_Gates (N : Node_Id) return List_Id;
   procedure Set_Interface_Synchronisation_Gates (N : Node_Id; V : List_Id);

   function Hide_Gate_Declarations (N : Node_Id) return List_Id;
   procedure Set_Hide_Gate_Declarations (N : Node_Id; V : List_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 6) of Boolean;
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

end Ocarina.Backends.LNT.Nodes;
