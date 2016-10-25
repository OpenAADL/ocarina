pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.ME_REAL.REAL_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_Identifier,
      K_List_Id,
      K_Node_Container,
      K_Element,
      K_Set,
      K_Set_Reference,
      K_Value_Node,
      K_Variable,
      K_Theorem_Reference,
      K_Var_Reference,
      K_Variable_Declaration,
      K_Variable_Decl_Compute,
      K_Variable_Decl_Expression,
      K_Literal,
      K_Parametrized_Identifier,
      K_Expression,
      K_Set_Expression,
      K_Check_Subprogram_Call,
      K_Check_Expression,
      K_Ternary_Expression,
      K_Return_Expression,
      K_Range_Declaration,
      K_Local_Variable_Definition,
      K_Set_Declaration,
      K_Required_Theorem,
      K_Theorem,
      K_Root_Node,
      K_Annotation);

   --
   --  Node_Id
   --
   --    Next_Node                : Node_Id
   --

   --
   --  Identifier
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --

   procedure W_Identifier (N : Node_Id);

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
   --

   procedure W_Node_Container (N : Node_Id);

   --
   --  Element
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Element_Type             : Value_Id
   --    Set_Reference            : Node_Id
   --

   procedure W_Element (N : Node_Id);

   --
   --  Set
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Set_Type                 : Value_Id
   --    Annotation               : Node_Id
   --    Set_Expression           : Node_Id
   --    Predefined_Type          : Value_Id
   --    Dependant                : Value_Id
   --

   procedure W_Set (N : Node_Id);

   --
   --  Set_Reference
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Referenced_Set           : Node_Id
   --    Predefined_Type          : Value_Id
   --

   procedure W_Set_Reference (N : Node_Id);

   --
   --  Value_Node
   --
   --    Next_Node                : Node_Id
   --    Item_Val                 : Value_Id
   --

   procedure W_Value_Node (N : Node_Id);

   --
   --  Variable
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Var_Value                : Value_Id
   --    Var_Type                 : Value_Id
   --

   procedure W_Variable (N : Node_Id);

   --
   --  Theorem_Reference
   --
   --    Next_Node                : Node_Id
   --    Theorem_Name             : Name_Id
   --    Related_Theorem          : Node_Id
   --

   procedure W_Theorem_Reference (N : Node_Id);

   --
   --  Var_Reference
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Referenced_Var           : Node_Id
   --    Returned_Type            : Value_Id
   --

   procedure W_Var_Reference (N : Node_Id);

   --
   --  Variable_Declaration
   --
   --    Next_Node                : Node_Id
   --    Var_Ref                  : Node_Id
   --    Is_Global                : Value_Id
   --

   procedure W_Variable_Declaration (N : Node_Id);

   --
   --  Variable_Decl_Compute
   --
   --    Next_Node                : Node_Id
   --    Theorem_Name             : Name_Id
   --    Related_Theorem          : Node_Id
   --    Var_Ref                  : Node_Id
   --    True_params              : List_Id
   --    Domain                   : Node_Id
   --    Parameters               : List_Id
   --    Is_Global                : Value_Id
   --

   procedure W_Variable_Decl_Compute (N : Node_Id);

   --
   --  Variable_Decl_Expression
   --
   --    Next_Node                : Node_Id
   --    Var_Ref                  : Node_Id
   --    Is_Global                : Value_Id
   --    Return_Expr              : Node_Id
   --

   procedure W_Variable_Decl_Expression (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Value                    : Value_Id
   --    Returned_Type            : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  Parametrized_Identifier
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Code                     : Value_Id
   --    Parameters               : List_Id
   --

   procedure W_Parametrized_Identifier (N : Node_Id);

   --
   --  Expression
   --
   --    Next_Node                : Node_Id
   --    Right_Expr               : Node_Id
   --    Left_Expr                : Node_Id
   --    Operator                 : Operator_Id
   --

   procedure W_Expression (N : Node_Id);

   --
   --  Set_Expression
   --
   --    Next_Node                : Node_Id
   --    Right_Expr               : Node_Id
   --    Left_Expr                : Node_Id
   --    Operator                 : Operator_Id
   --    Set_Type                 : Value_Id
   --

   procedure W_Set_Expression (N : Node_Id);

   --
   --  Check_Subprogram_Call
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Code                     : Value_Id
   --    Parameters               : List_Id
   --    Referenced_Sets          : List_Id
   --    True_Parameters          : List_Id
   --    Variable_Position        : Value_Id
   --    Returned_Type            : Value_Id
   --

   procedure W_Check_Subprogram_Call (N : Node_Id);

   --
   --  Check_Expression
   --
   --    Next_Node                : Node_Id
   --    Right_Expr               : Node_Id
   --    Left_Expr                : Node_Id
   --    Operator                 : Operator_Id
   --    Returned_Type            : Value_Id
   --

   procedure W_Check_Expression (N : Node_Id);

   --
   --  Ternary_Expression
   --
   --    Next_Node                : Node_Id
   --    Right_Expr               : Node_Id
   --    Left_Expr                : Node_Id
   --    Operator                 : Operator_Id
   --    Returned_Type            : Value_Id
   --    Third_Expr               : Node_Id
   --

   procedure W_Ternary_Expression (N : Node_Id);

   --
   --  Return_Expression
   --
   --    Next_Node                : Node_Id
   --    Check_Expression         : Node_Id
   --    Range_Function           : Value_Id
   --

   procedure W_Return_Expression (N : Node_Id);

   --
   --  Range_Declaration
   --
   --    Next_Node                : Node_Id
   --    Variable_Ref             : Node_Id
   --    Range_Variable           : Node_Id
   --    Range_Set                : Node_Id
   --

   procedure W_Range_Declaration (N : Node_Id);

   --
   --  Local_Variable_Definition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Element_Type             : Value_Id
   --    Set_Reference            : Node_Id
   --    In_Set                   : Node_Id
   --

   procedure W_Local_Variable_Definition (N : Node_Id);

   --
   --  Set_Declaration
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Referenced_Set           : Node_Id
   --    Predefined_Type          : Value_Id
   --    Parametrized_Expr        : Node_Id
   --    Corresponding_Element    : Node_Id
   --    Selection_Expression     : Node_Id
   --    Local_Variable           : Node_Id
   --    Local_Set                : Node_Id
   --    Local_Set_Expression     : Node_Id
   --    Dependant                : Value_Id
   --

   procedure W_Set_Declaration (N : Node_Id);

   --
   --  Required_Theorem
   --
   --    Next_Node                : Node_Id
   --    Theorem_Name             : Name_Id
   --    Related_Theorem          : Node_Id
   --

   procedure W_Required_Theorem (N : Node_Id);

   --
   --  Theorem
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Range_Declaration        : Node_Id
   --    Declarations             : List_Id
   --    Required_Theorems        : List_Id
   --    Check_Expression         : Node_Id
   --    Return_Expression        : Node_Id
   --    Used_Set                 : List_Id
   --    Used_Var                 : List_Id
   --    Local_Var                : List_Id
   --    Related_Entity           : Node_Id
   --

   procedure W_Theorem (N : Node_Id);

   --
   --  Root_Node
   --
   --    Next_Node                : Node_Id
   --    Theorems                 : List_Id
   --

   procedure W_Root_Node (N : Node_Id);

   --
   --  Annotation
   --
   --    Next_Node                : Node_Id
   --    Index                    : Value_Id
   --

   procedure W_Annotation (N : Node_Id);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Item (N : Node_Id) return Node_Id;
   procedure Set_Item (N : Node_Id; V : Node_Id);

   function Identifier (N : Node_Id) return Node_Id;
   procedure Set_Identifier (N : Node_Id; V : Node_Id);

   function Element_Type (N : Node_Id) return Value_Id;
   procedure Set_Element_Type (N : Node_Id; V : Value_Id);

   function Set_Reference (N : Node_Id) return Node_Id;
   procedure Set_Set_Reference (N : Node_Id; V : Node_Id);

   function Set_Type (N : Node_Id) return Value_Id;
   procedure Set_Set_Type (N : Node_Id; V : Value_Id);

   function Annotation (N : Node_Id) return Node_Id;
   procedure Set_Annotation (N : Node_Id; V : Node_Id);

   function Set_Expression (N : Node_Id) return Node_Id;
   procedure Set_Set_Expression (N : Node_Id; V : Node_Id);

   function Predefined_Type (N : Node_Id) return Value_Id;
   procedure Set_Predefined_Type (N : Node_Id; V : Value_Id);

   function Dependant (N : Node_Id) return Value_Id;
   procedure Set_Dependant (N : Node_Id; V : Value_Id);

   function Referenced_Set (N : Node_Id) return Node_Id;
   procedure Set_Referenced_Set (N : Node_Id; V : Node_Id);

   function Item_Val (N : Node_Id) return Value_Id;
   procedure Set_Item_Val (N : Node_Id; V : Value_Id);

   function Var_Value (N : Node_Id) return Value_Id;
   procedure Set_Var_Value (N : Node_Id; V : Value_Id);

   function Var_Type (N : Node_Id) return Value_Id;
   procedure Set_Var_Type (N : Node_Id; V : Value_Id);

   function Theorem_Name (N : Node_Id) return Name_Id;
   procedure Set_Theorem_Name (N : Node_Id; V : Name_Id);

   function Related_Theorem (N : Node_Id) return Node_Id;
   procedure Set_Related_Theorem (N : Node_Id; V : Node_Id);

   function Referenced_Var (N : Node_Id) return Node_Id;
   procedure Set_Referenced_Var (N : Node_Id; V : Node_Id);

   function Returned_Type (N : Node_Id) return Value_Id;
   procedure Set_Returned_Type (N : Node_Id; V : Value_Id);

   function Var_Ref (N : Node_Id) return Node_Id;
   procedure Set_Var_Ref (N : Node_Id; V : Node_Id);

   function Is_Global (N : Node_Id) return Value_Id;
   procedure Set_Is_Global (N : Node_Id; V : Value_Id);

   function True_params (N : Node_Id) return List_Id;
   procedure Set_True_params (N : Node_Id; V : List_Id);

   function Domain (N : Node_Id) return Node_Id;
   procedure Set_Domain (N : Node_Id; V : Node_Id);

   function Parameters (N : Node_Id) return List_Id;
   procedure Set_Parameters (N : Node_Id; V : List_Id);

   function Return_Expr (N : Node_Id) return Node_Id;
   procedure Set_Return_Expr (N : Node_Id; V : Node_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function Code (N : Node_Id) return Value_Id;
   procedure Set_Code (N : Node_Id; V : Value_Id);

   function Right_Expr (N : Node_Id) return Node_Id;
   procedure Set_Right_Expr (N : Node_Id; V : Node_Id);

   function Left_Expr (N : Node_Id) return Node_Id;
   procedure Set_Left_Expr (N : Node_Id; V : Node_Id);

   function Operator (N : Node_Id) return Operator_Id;
   procedure Set_Operator (N : Node_Id; V : Operator_Id);

   function Referenced_Sets (N : Node_Id) return List_Id;
   procedure Set_Referenced_Sets (N : Node_Id; V : List_Id);

   function True_Parameters (N : Node_Id) return List_Id;
   procedure Set_True_Parameters (N : Node_Id; V : List_Id);

   function Variable_Position (N : Node_Id) return Value_Id;
   procedure Set_Variable_Position (N : Node_Id; V : Value_Id);

   function Third_Expr (N : Node_Id) return Node_Id;
   procedure Set_Third_Expr (N : Node_Id; V : Node_Id);

   function Check_Expression (N : Node_Id) return Node_Id;
   procedure Set_Check_Expression (N : Node_Id; V : Node_Id);

   function Range_Function (N : Node_Id) return Value_Id;
   procedure Set_Range_Function (N : Node_Id; V : Value_Id);

   function Variable_Ref (N : Node_Id) return Node_Id;
   procedure Set_Variable_Ref (N : Node_Id; V : Node_Id);

   function Range_Variable (N : Node_Id) return Node_Id;
   procedure Set_Range_Variable (N : Node_Id; V : Node_Id);

   function Range_Set (N : Node_Id) return Node_Id;
   procedure Set_Range_Set (N : Node_Id; V : Node_Id);

   function In_Set (N : Node_Id) return Node_Id;
   procedure Set_In_Set (N : Node_Id; V : Node_Id);

   function Parametrized_Expr (N : Node_Id) return Node_Id;
   procedure Set_Parametrized_Expr (N : Node_Id; V : Node_Id);

   function Corresponding_Element (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Element (N : Node_Id; V : Node_Id);

   function Selection_Expression (N : Node_Id) return Node_Id;
   procedure Set_Selection_Expression (N : Node_Id; V : Node_Id);

   function Local_Variable (N : Node_Id) return Node_Id;
   procedure Set_Local_Variable (N : Node_Id; V : Node_Id);

   function Local_Set (N : Node_Id) return Node_Id;
   procedure Set_Local_Set (N : Node_Id; V : Node_Id);

   function Local_Set_Expression (N : Node_Id) return Node_Id;
   procedure Set_Local_Set_Expression (N : Node_Id; V : Node_Id);

   function Range_Declaration (N : Node_Id) return Node_Id;
   procedure Set_Range_Declaration (N : Node_Id; V : Node_Id);

   function Declarations (N : Node_Id) return List_Id;
   procedure Set_Declarations (N : Node_Id; V : List_Id);

   function Required_Theorems (N : Node_Id) return List_Id;
   procedure Set_Required_Theorems (N : Node_Id; V : List_Id);

   function Return_Expression (N : Node_Id) return Node_Id;
   procedure Set_Return_Expression (N : Node_Id; V : Node_Id);

   function Used_Set (N : Node_Id) return List_Id;
   procedure Set_Used_Set (N : Node_Id; V : List_Id);

   function Used_Var (N : Node_Id) return List_Id;
   procedure Set_Used_Var (N : Node_Id; V : List_Id);

   function Local_Var (N : Node_Id) return List_Id;
   procedure Set_Local_Var (N : Node_Id; V : List_Id);

   function Related_Entity (N : Node_Id) return Node_Id;
   procedure Set_Related_Entity (N : Node_Id; V : Node_Id);

   function Theorems (N : Node_Id) return List_Id;
   procedure Set_Theorems (N : Node_Id; V : List_Id);

   function Index (N : Node_Id) return Value_Id;
   procedure Set_Index (N : Node_Id; V : Value_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 0) of Boolean;
   type Byte_Array is array (1 .. 1) of Byte;
   type Int_Array is array (1 .. 11) of Int;

   type Node_Entry is record
      Kind : Node_Kind;
      O : Byte_Array;
      L : Int_Array;
      Loc : Location;
   end record;

   Default_Node : constant Node_Entry :=
     (Node_Kind'First,
      (others => 0),
      (others => 0),
      No_Location);

   package Entries is new GNAT.Table
     (Node_Entry, Node_Id, No_Node + 1, 1000, 100);

end Ocarina.ME_REAL.REAL_Tree.Nodes;
