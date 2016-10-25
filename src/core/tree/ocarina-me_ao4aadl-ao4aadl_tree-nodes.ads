pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_Identifier,
      K_List_Id,
      K_Node_Container,
      K_Definition,
      K_Wildcard,
      K_Star,
      K_Interval,
      K_Literal,
      K_Aspect_Annex,
      K_Aspect_Expression,
      K_Components_Applied_To,
      K_Component,
      K_Aspect_Precedence,
      K_Pointcut_Specification,
      K_Parameter_Specification,
      K_Pointcut_Expression,
      K_Caller,
      K_Call,
      K_Execution,
      K_Callee,
      K_Subprogram_Call,
      K_Port_Call,
      K_Args,
      K_Advice_Specification,
      K_Advice_Declaration,
      K_Pointcut_Reference,
      K_Advice_Action,
      K_Variables_Declaration,
      K_Variable,
      K_Initialisation,
      K_Action,
      K_Conditional_Statement,
      K_If_Statement,
      K_While_Statement,
      K_For_Statement,
      K_Assignment_Action,
      K_Communication_Action,
      K_Timed_Action,
      K_Reference_Expression,
      K_Behavior_Expression,
      K_Relation,
      K_Simple_Expression,
      K_Term,
      K_Factor,
      K_Property_Constant,
      K_Operator,
      K_Boolean_Literal,
      K_Integer_Range,
      K_Integer_Value,
      K_Behavior_Time,
      K_Proceed_Action,
      K_Parameter_Profile);

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
   --  Definition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Definition (N : Node_Id);

   --
   --  Wildcard
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Wildcard (N : Node_Id);

   --
   --  Star
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Star (N : Node_Id);

   --
   --  Interval
   --
   --    Next_Node                : Node_Id
   --

   procedure W_Interval (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  Aspect_Annex
   --
   --    Next_Node                : Node_Id
   --    Aspect_Expressions       : List_Id
   --

   procedure W_Aspect_Annex (N : Node_Id);

   --
   --  Aspect_Expression
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Components_Applied_Tos   : Node_Id
   --    Aspect_Precedence        : Node_Id
   --    Pointcut_Specification   : List_Id
   --    Advice_Specification     : List_Id
   --

   procedure W_Aspect_Expression (N : Node_Id);

   --
   --  Components_Applied_To
   --
   --    Next_Node                : Node_Id
   --    Components               : List_Id
   --

   procedure W_Components_Applied_To (N : Node_Id);

   --
   --  Component
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Component_Category       : Byte
   --

   procedure W_Component (N : Node_Id);

   --
   --  Aspect_Precedence
   --
   --    Next_Node                : Node_Id
   --    Identifiers              : List_Id
   --

   procedure W_Aspect_Precedence (N : Node_Id);

   --
   --  Pointcut_Specification
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameters               : List_Id
   --    Pointcut_Expression      : Node_Id
   --

   procedure W_Pointcut_Specification (N : Node_Id);

   --
   --  Parameter_Specification
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Type_Identifier          : Node_Id
   --

   procedure W_Parameter_Specification (N : Node_Id);

   --
   --  Pointcut_Expression
   --
   --    Next_Node                : Node_Id
   --    Caller                   : Node_Id
   --    Operator                 : Node_Id
   --    Pointcut_Expression      : Node_Id
   --

   procedure W_Pointcut_Expression (N : Node_Id);

   --
   --  Caller
   --
   --    Next_Node                : Node_Id
   --    Caller_Kind              : Byte
   --    Callee                   : Node_Id
   --

   procedure W_Caller (N : Node_Id);

   --
   --  Call
   --
   --    Next_Node                : Node_Id
   --    Caller_Kind              : Byte
   --    Callee                   : Node_Id
   --

   procedure W_Call (N : Node_Id);

   --
   --  Execution
   --
   --    Next_Node                : Node_Id
   --    Caller_Kind              : Byte
   --    Callee                   : Node_Id
   --

   procedure W_Execution (N : Node_Id);

   --
   --  Callee
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameters               : List_Id
   --

   procedure W_Callee (N : Node_Id);

   --
   --  Subprogram_Call
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameters               : List_Id
   --

   procedure W_Subprogram_Call (N : Node_Id);

   --
   --  Port_Call
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameters               : List_Id
   --    Mode                     : Mode_Id
   --

   procedure W_Port_Call (N : Node_Id);

   --
   --  Args
   --
   --    Next_Node                : Node_Id
   --    Arguments                : List_Id
   --

   procedure W_Args (N : Node_Id);

   --
   --  Advice_Specification
   --
   --    Next_Node                : Node_Id
   --    Advice_Declaration       : Node_Id
   --    Pointcut_Reference       : Node_Id
   --    Advice_Action            : Node_Id
   --

   procedure W_Advice_Specification (N : Node_Id);

   --
   --  Advice_Declaration
   --
   --    Next_Node                : Node_Id
   --    Advice_Category          : Byte
   --    Parameters               : List_Id
   --

   procedure W_Advice_Declaration (N : Node_Id);

   --
   --  Pointcut_Reference
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameter_Profile        : List_Id
   --

   procedure W_Pointcut_Reference (N : Node_Id);

   --
   --  Advice_Action
   --
   --    Next_Node                : Node_Id
   --    Variables_Declaration    : Node_Id
   --    Initialisation           : Node_Id
   --    Actions                  : List_Id
   --

   procedure W_Advice_Action (N : Node_Id);

   --
   --  Variables_Declaration
   --
   --    Next_Node                : Node_Id
   --    Variables                : List_Id
   --

   procedure W_Variables_Declaration (N : Node_Id);

   --
   --  Variable
   --
   --    Next_Node                : Node_Id
   --    Identifiers              : List_Id
   --    Type_Identifier          : Node_Id
   --

   procedure W_Variable (N : Node_Id);

   --
   --  Initialisation
   --
   --    Next_Node                : Node_Id
   --    Assignments              : List_Id
   --

   procedure W_Initialisation (N : Node_Id);

   --
   --  Action
   --
   --    Next_Node                : Node_Id
   --    Action_Node              : Node_Id
   --

   procedure W_Action (N : Node_Id);

   --
   --  Conditional_Statement
   --
   --    Next_Node                : Node_Id
   --    Behavior_Expression      : Node_Id
   --    Actions                  : List_Id
   --

   procedure W_Conditional_Statement (N : Node_Id);

   --
   --  If_Statement
   --
   --    Next_Node                : Node_Id
   --    If_Cond                  : Node_Id
   --    Elsif_Cond               : Node_Id
   --    Else_Cond                : Node_Id
   --

   procedure W_If_Statement (N : Node_Id);

   --
   --  While_Statement
   --
   --    Next_Node                : Node_Id
   --    Conditional_Statement    : Node_Id
   --

   procedure W_While_Statement (N : Node_Id);

   --
   --  For_Statement
   --
   --    Next_Node                : Node_Id
   --    Loop_Variable_Identifier : Node_Id
   --    Integer_Range            : Node_Id
   --    Actions                  : List_Id
   --

   procedure W_For_Statement (N : Node_Id);

   --
   --  Assignment_Action
   --
   --    Next_Node                : Node_Id
   --    Reference_Expression     : Node_Id
   --    Behavior_Expression      : Node_Id
   --

   procedure W_Assignment_Action (N : Node_Id);

   --
   --  Communication_Action
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Parameter_Profile        : List_Id
   --    Com_Kind                 : Byte
   --

   procedure W_Communication_Action (N : Node_Id);

   --
   --  Timed_Action
   --
   --    Next_Node                : Node_Id
   --    Is_Computation           : Boolean
   --    Fst_Behavior_Time        : Node_Id
   --    Scd_Behavior_Time        : Node_Id
   --    Distrib_Kind             : Byte
   --

   procedure W_Timed_Action (N : Node_Id);

   --
   --  Reference_Expression
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Reference_Expression (N : Node_Id);

   --
   --  Behavior_Expression
   --
   --    Next_Node                : Node_Id
   --    Relations                : List_Id
   --    String_Literal           : Node_Id
   --    Character_Literal        : Node_Id
   --

   procedure W_Behavior_Expression (N : Node_Id);

   --
   --  Relation
   --
   --    Next_Node                : Node_Id
   --    Boolean_Value            : Boolean
   --    Simple_Exprs             : List_Id
   --

   procedure W_Relation (N : Node_Id);

   --
   --  Simple_Expression
   --
   --    Next_Node                : Node_Id
   --    Term_And_Operator        : List_Id
   --

   procedure W_Simple_Expression (N : Node_Id);

   --
   --  Term
   --
   --    Next_Node                : Node_Id
   --    Factors                  : List_Id
   --

   procedure W_Term (N : Node_Id);

   --
   --  Factor
   --
   --    Next_Node                : Node_Id
   --    Is_Not                   : Boolean
   --    Lower_Primary            : Node_Id
   --

   procedure W_Factor (N : Node_Id);

   --
   --  Property_Constant
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Property_Set             : Node_Id
   --

   procedure W_Property_Constant (N : Node_Id);

   --
   --  Operator
   --
   --    Next_Node                : Node_Id
   --    Operator_Kind            : Byte
   --

   procedure W_Operator (N : Node_Id);

   --
   --  Boolean_Literal
   --
   --    Next_Node                : Node_Id
   --    Is_True                  : Boolean
   --

   procedure W_Boolean_Literal (N : Node_Id);

   --
   --  Integer_Range
   --
   --    Next_Node                : Node_Id
   --    Lower_Int_Val            : Node_Id
   --    Upper_Int_Val            : Node_Id
   --

   procedure W_Integer_Range (N : Node_Id);

   --
   --  Integer_Value
   --
   --    Next_Node                : Node_Id
   --    Entity                   : Node_Id
   --

   procedure W_Integer_Value (N : Node_Id);

   --
   --  Behavior_Time
   --
   --    Next_Node                : Node_Id
   --    Integer_Value            : Node_Id
   --    Unit_Identifier          : Node_Id
   --

   procedure W_Behavior_Time (N : Node_Id);

   --
   --  Proceed_Action
   --
   --    Next_Node                : Node_Id
   --    Parameter_Profile        : List_Id
   --

   procedure W_Proceed_Action (N : Node_Id);

   --
   --  Parameter_Profile
   --
   --    Next_Node                : Node_Id
   --    Behavior_Expression      : Node_Id
   --

   procedure W_Parameter_Profile (N : Node_Id);

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

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function Aspect_Expressions (N : Node_Id) return List_Id;
   procedure Set_Aspect_Expressions (N : Node_Id; V : List_Id);

   function Components_Applied_Tos (N : Node_Id) return Node_Id;
   procedure Set_Components_Applied_Tos (N : Node_Id; V : Node_Id);

   function Aspect_Precedence (N : Node_Id) return Node_Id;
   procedure Set_Aspect_Precedence (N : Node_Id; V : Node_Id);

   function Pointcut_Specification (N : Node_Id) return List_Id;
   procedure Set_Pointcut_Specification (N : Node_Id; V : List_Id);

   function Advice_Specification (N : Node_Id) return List_Id;
   procedure Set_Advice_Specification (N : Node_Id; V : List_Id);

   function Components (N : Node_Id) return List_Id;
   procedure Set_Components (N : Node_Id; V : List_Id);

   function Component_Category (N : Node_Id) return Byte;
   procedure Set_Component_Category (N : Node_Id; V : Byte);

   function Identifiers (N : Node_Id) return List_Id;
   procedure Set_Identifiers (N : Node_Id; V : List_Id);

   function Parameters (N : Node_Id) return List_Id;
   procedure Set_Parameters (N : Node_Id; V : List_Id);

   function Pointcut_Expression (N : Node_Id) return Node_Id;
   procedure Set_Pointcut_Expression (N : Node_Id; V : Node_Id);

   function Type_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Type_Identifier (N : Node_Id; V : Node_Id);

   function Caller (N : Node_Id) return Node_Id;
   procedure Set_Caller (N : Node_Id; V : Node_Id);

   function Operator (N : Node_Id) return Node_Id;
   procedure Set_Operator (N : Node_Id; V : Node_Id);

   function Caller_Kind (N : Node_Id) return Byte;
   procedure Set_Caller_Kind (N : Node_Id; V : Byte);

   function Callee (N : Node_Id) return Node_Id;
   procedure Set_Callee (N : Node_Id; V : Node_Id);

   function Mode (N : Node_Id) return Mode_Id;
   procedure Set_Mode (N : Node_Id; V : Mode_Id);

   function Arguments (N : Node_Id) return List_Id;
   procedure Set_Arguments (N : Node_Id; V : List_Id);

   function Advice_Declaration (N : Node_Id) return Node_Id;
   procedure Set_Advice_Declaration (N : Node_Id; V : Node_Id);

   function Pointcut_Reference (N : Node_Id) return Node_Id;
   procedure Set_Pointcut_Reference (N : Node_Id; V : Node_Id);

   function Advice_Action (N : Node_Id) return Node_Id;
   procedure Set_Advice_Action (N : Node_Id; V : Node_Id);

   function Advice_Category (N : Node_Id) return Byte;
   procedure Set_Advice_Category (N : Node_Id; V : Byte);

   function Parameter_Profile (N : Node_Id) return List_Id;
   procedure Set_Parameter_Profile (N : Node_Id; V : List_Id);

   function Variables_Declaration (N : Node_Id) return Node_Id;
   procedure Set_Variables_Declaration (N : Node_Id; V : Node_Id);

   function Initialisation (N : Node_Id) return Node_Id;
   procedure Set_Initialisation (N : Node_Id; V : Node_Id);

   function Actions (N : Node_Id) return List_Id;
   procedure Set_Actions (N : Node_Id; V : List_Id);

   function Variables (N : Node_Id) return List_Id;
   procedure Set_Variables (N : Node_Id; V : List_Id);

   function Assignments (N : Node_Id) return List_Id;
   procedure Set_Assignments (N : Node_Id; V : List_Id);

   function Action_Node (N : Node_Id) return Node_Id;
   procedure Set_Action_Node (N : Node_Id; V : Node_Id);

   function Behavior_Expression (N : Node_Id) return Node_Id;
   procedure Set_Behavior_Expression (N : Node_Id; V : Node_Id);

   function If_Cond (N : Node_Id) return Node_Id;
   procedure Set_If_Cond (N : Node_Id; V : Node_Id);

   function Elsif_Cond (N : Node_Id) return Node_Id;
   procedure Set_Elsif_Cond (N : Node_Id; V : Node_Id);

   function Else_Cond (N : Node_Id) return Node_Id;
   procedure Set_Else_Cond (N : Node_Id; V : Node_Id);

   function Conditional_Statement (N : Node_Id) return Node_Id;
   procedure Set_Conditional_Statement (N : Node_Id; V : Node_Id);

   function Loop_Variable_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Loop_Variable_Identifier (N : Node_Id; V : Node_Id);

   function Integer_Range (N : Node_Id) return Node_Id;
   procedure Set_Integer_Range (N : Node_Id; V : Node_Id);

   function Reference_Expression (N : Node_Id) return Node_Id;
   procedure Set_Reference_Expression (N : Node_Id; V : Node_Id);

   function Com_Kind (N : Node_Id) return Byte;
   procedure Set_Com_Kind (N : Node_Id; V : Byte);

   function Is_Computation (N : Node_Id) return Boolean;
   procedure Set_Is_Computation (N : Node_Id; V : Boolean);

   function Fst_Behavior_Time (N : Node_Id) return Node_Id;
   procedure Set_Fst_Behavior_Time (N : Node_Id; V : Node_Id);

   function Scd_Behavior_Time (N : Node_Id) return Node_Id;
   procedure Set_Scd_Behavior_Time (N : Node_Id; V : Node_Id);

   function Distrib_Kind (N : Node_Id) return Byte;
   procedure Set_Distrib_Kind (N : Node_Id; V : Byte);

   function Relations (N : Node_Id) return List_Id;
   procedure Set_Relations (N : Node_Id; V : List_Id);

   function String_Literal (N : Node_Id) return Node_Id;
   procedure Set_String_Literal (N : Node_Id; V : Node_Id);

   function Character_Literal (N : Node_Id) return Node_Id;
   procedure Set_Character_Literal (N : Node_Id; V : Node_Id);

   function Boolean_Value (N : Node_Id) return Boolean;
   procedure Set_Boolean_Value (N : Node_Id; V : Boolean);

   function Simple_Exprs (N : Node_Id) return List_Id;
   procedure Set_Simple_Exprs (N : Node_Id; V : List_Id);

   function Term_And_Operator (N : Node_Id) return List_Id;
   procedure Set_Term_And_Operator (N : Node_Id; V : List_Id);

   function Factors (N : Node_Id) return List_Id;
   procedure Set_Factors (N : Node_Id; V : List_Id);

   function Is_Not (N : Node_Id) return Boolean;
   procedure Set_Is_Not (N : Node_Id; V : Boolean);

   function Lower_Primary (N : Node_Id) return Node_Id;
   procedure Set_Lower_Primary (N : Node_Id; V : Node_Id);

   function Property_Set (N : Node_Id) return Node_Id;
   procedure Set_Property_Set (N : Node_Id; V : Node_Id);

   function Operator_Kind (N : Node_Id) return Byte;
   procedure Set_Operator_Kind (N : Node_Id; V : Byte);

   function Is_True (N : Node_Id) return Boolean;
   procedure Set_Is_True (N : Node_Id; V : Boolean);

   function Lower_Int_Val (N : Node_Id) return Node_Id;
   procedure Set_Lower_Int_Val (N : Node_Id; V : Node_Id);

   function Upper_Int_Val (N : Node_Id) return Node_Id;
   procedure Set_Upper_Int_Val (N : Node_Id; V : Node_Id);

   function Entity (N : Node_Id) return Node_Id;
   procedure Set_Entity (N : Node_Id; V : Node_Id);

   function Integer_Value (N : Node_Id) return Node_Id;
   procedure Set_Integer_Value (N : Node_Id; V : Node_Id);

   function Unit_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Unit_Identifier (N : Node_Id; V : Node_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 1) of Boolean;
   type Byte_Array is array (1 .. 2) of Byte;
   type Int_Array is array (1 .. 6) of Int;

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

end Ocarina.ME_AO4AADL.AO4AADL_Tree.Nodes;
