pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.Backends.LNT.Debug; use Ocarina.Backends.LNT.Debug;

package body Ocarina.Backends.LNT.Nodes is

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
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Predefined_Function
        or else Table (Types.Node_Id (N)).Kind = K_Equality
        or else Table (Types.Node_Id (N)).Kind = K_Inequality
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Ordinal
        or else Table (Types.Node_Id (N)).Kind = K_Value
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update
        or else Table (Types.Node_Id (N)).Kind = K_Type_Cardinality
        or else Table (Types.Node_Id (N)).Kind = K_First_Element
        or else Table (Types.Node_Id (N)).Kind = K_Last_Element
        or else Table (Types.Node_Id (N)).Kind = K_And
        or else Table (Types.Node_Id (N)).Kind = K_Or
        or else Table (Types.Node_Id (N)).Kind = K_Module_Pragma
        or else Table (Types.Node_Id (N)).Kind = K_Nat_Bits
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp
        or else Table (Types.Node_Id (N)).Kind = K_RangeLNT
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Nat
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Null_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Var_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Var_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_List_Of
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized
        or else Table (Types.Node_Id (N)).Kind = K_Expressions
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Elt_Access_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Constructed_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Patterns
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed
        or else Table (Types.Node_Id (N)).Kind = K_Channel
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Stop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Parallel_Composition_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Interface_Synchronisation
        or else Table (Types.Node_Id (N)).Kind = K_Hide_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Predefined_Function
        or else Table (Types.Node_Id (N)).Kind = K_Equality
        or else Table (Types.Node_Id (N)).Kind = K_Inequality
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Ordinal
        or else Table (Types.Node_Id (N)).Kind = K_Value
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update
        or else Table (Types.Node_Id (N)).Kind = K_Type_Cardinality
        or else Table (Types.Node_Id (N)).Kind = K_First_Element
        or else Table (Types.Node_Id (N)).Kind = K_Last_Element
        or else Table (Types.Node_Id (N)).Kind = K_And
        or else Table (Types.Node_Id (N)).Kind = K_Or
        or else Table (Types.Node_Id (N)).Kind = K_Module_Pragma
        or else Table (Types.Node_Id (N)).Kind = K_Nat_Bits
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp
        or else Table (Types.Node_Id (N)).Kind = K_RangeLNT
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Nat
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Null_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Var_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Var_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_List_Of
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized
        or else Table (Types.Node_Id (N)).Kind = K_Expressions
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Elt_Access_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Constructed_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Patterns
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed
        or else Table (Types.Node_Id (N)).Kind = K_Channel
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Stop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Parallel_Composition_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Interface_Synchronisation
        or else Table (Types.Node_Id (N)).Kind = K_Hide_Statement);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Next_Node;

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

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Name;

   function Ocarina_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Ocarina_Node;

   procedure Set_Ocarina_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Ocarina_Node;

   function Corresponding_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Corresponding_Entity;

   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Corresponding_Entity;

   function Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Var_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constructed_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Channel
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Var_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constructed_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Channel
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Identifier;

   function Modules (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Modules;

   procedure Set_Modules (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Modules;

   function Predefined_Functions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Predefined_Functions;

   procedure Set_Predefined_Functions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Predefined_Functions;

   function Module_Pragmas (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Module_Pragmas;

   procedure Set_Module_Pragmas (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Module_Pragmas;

   function Definitions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Definitions;

   procedure Set_Definitions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Definition);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Definitions;

   function LNT_Function (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Predefined_Function
        or else Table (Types.Node_Id (N)).Kind = K_Equality
        or else Table (Types.Node_Id (N)).Kind = K_Inequality
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Ordinal
        or else Table (Types.Node_Id (N)).Kind = K_Value
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update
        or else Table (Types.Node_Id (N)).Kind = K_Type_Cardinality
        or else Table (Types.Node_Id (N)).Kind = K_First_Element
        or else Table (Types.Node_Id (N)).Kind = K_Last_Element
        or else Table (Types.Node_Id (N)).Kind = K_And
        or else Table (Types.Node_Id (N)).Kind = K_Or);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end LNT_Function;

   procedure Set_LNT_Function (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Predefined_Function
        or else Table (Types.Node_Id (N)).Kind = K_Equality
        or else Table (Types.Node_Id (N)).Kind = K_Inequality
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Ordinal
        or else Table (Types.Node_Id (N)).Kind = K_Value
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update
        or else Table (Types.Node_Id (N)).Kind = K_Type_Cardinality
        or else Table (Types.Node_Id (N)).Kind = K_First_Element
        or else Table (Types.Node_Id (N)).Kind = K_Last_Element
        or else Table (Types.Node_Id (N)).Kind = K_And
        or else Table (Types.Node_Id (N)).Kind = K_Or);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_LNT_Function;

   function Is_With_Clause (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Predefined_Function
        or else Table (Types.Node_Id (N)).Kind = K_Equality
        or else Table (Types.Node_Id (N)).Kind = K_Inequality
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Ordinal
        or else Table (Types.Node_Id (N)).Kind = K_Value
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update
        or else Table (Types.Node_Id (N)).Kind = K_Type_Cardinality
        or else Table (Types.Node_Id (N)).Kind = K_First_Element
        or else Table (Types.Node_Id (N)).Kind = K_Last_Element
        or else Table (Types.Node_Id (N)).Kind = K_And
        or else Table (Types.Node_Id (N)).Kind = K_Or);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_With_Clause;

   procedure Set_Is_With_Clause (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Predefined_Function
        or else Table (Types.Node_Id (N)).Kind = K_Equality
        or else Table (Types.Node_Id (N)).Kind = K_Inequality
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than
        or else Table (Types.Node_Id (N)).Kind = K_Less_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than
        or else Table (Types.Node_Id (N)).Kind = K_Greater_Than_Or_Equal_To
        or else Table (Types.Node_Id (N)).Kind = K_Ordinal
        or else Table (Types.Node_Id (N)).Kind = K_Value
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update
        or else Table (Types.Node_Id (N)).Kind = K_Type_Cardinality
        or else Table (Types.Node_Id (N)).Kind = K_First_Element
        or else Table (Types.Node_Id (N)).Kind = K_Last_Element
        or else Table (Types.Node_Id (N)).Kind = K_And
        or else Table (Types.Node_Id (N)).Kind = K_Or);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_With_Clause;

   function Module_Pragma_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Pragma
        or else Table (Types.Node_Id (N)).Kind = K_Nat_Bits);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Module_Pragma_Type;

   procedure Set_Module_Pragma_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Pragma
        or else Table (Types.Node_Id (N)).Kind = K_Nat_Bits);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Module_Pragma_Type;

   function Number_Constant (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Pragma
        or else Table (Types.Node_Id (N)).Kind = K_Nat_Bits);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Number_Constant;

   procedure Set_Number_Constant (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Module_Pragma
        or else Table (Types.Node_Id (N)).Kind = K_Nat_Bits);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Number_Constant;

   function Type_Exp (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Type_Exp;

   procedure Set_Type_Exp (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Type_Exp;

   function Type_Pragma (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Type_Pragma;

   procedure Set_Type_Pragma (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Def);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Type_Pragma;

   function Type_Constructors (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return List_Id (Table (Types.Node_Id (N)).L (9));
   end Type_Constructors;

   procedure Set_Type_Constructors (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Type_Constructors;

   function Is_Set (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Set;

   procedure Set_Is_Set (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Set;

   function Is_Sorted_Set (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Sorted_Set;

   procedure Set_Is_Sorted_Set (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Sorted_Set;

   function Is_List (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_List;

   procedure Set_Is_List (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_List;

   function Is_Sorted_List (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Sorted_List;

   procedure Set_Is_Sorted_List (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Sorted_List;

   function Is_Array (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return Boolean (Table (Types.Node_Id (N)).B (5));
   end Is_Array;

   procedure Set_Is_Array (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).B (5) := Boolean (V);
   end Set_Is_Array;

   function Is_Range (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return Boolean (Table (Types.Node_Id (N)).B (6));
   end Is_Range;

   procedure Set_Is_Range (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).B (6) := Boolean (V);
   end Set_Is_Range;

   function RangeLNT (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end RangeLNT;

   procedure Set_RangeLNT (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Exp);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_RangeLNT;

   function Low_Bound (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_RangeLNT);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Low_Bound;

   procedure Set_Low_Bound (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_RangeLNT);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Low_Bound;

   function High_Bound (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_RangeLNT);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end High_Bound;

   procedure Set_High_Bound (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_RangeLNT);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_High_Bound;

   function Type_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Nat
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Type_Name;

   procedure Set_Type_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Nat
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Type_Name;

   function Image (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Nat
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String);

      return Name_Id (Table (Types.Node_Id (N)).L (2));
   end Image;

   procedure Set_Image (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Nat
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Image;

   function Constructor_Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Constructor_Parameters;

   procedure Set_Constructor_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Constructor_Parameters;

   function Constructor_Pragma (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Constructor_Pragma;

   procedure Set_Constructor_Pragma (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constructor);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Constructor_Pragma;

   function Parameter_Mode (N : Node_Id) return Mode_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      return Mode_Id (Table (Types.Node_Id (N)).O (1));
   end Parameter_Mode;

   procedure Set_Parameter_Mode (N : Node_Id; V : Mode_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Parameter_Mode;

   function Parameter_Var (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Parameter_Var;

   procedure Set_Parameter_Var (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Parameter_Var;

   function Parameter_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Parameter_Type;

   procedure Set_Parameter_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Parameter_Type;

   function Function_Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Function_Parameters;

   procedure Set_Function_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Function_Parameters;

   function Function_Return_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Function_Return_Type;

   procedure Set_Function_Return_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Function_Return_Type;

   function Function_Exceptions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Function_Exceptions;

   procedure Set_Function_Exceptions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Function_Exceptions;

   function Function_Pragma (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Function_Pragma;

   procedure Set_Function_Pragma (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Function_Pragma;

   function Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Var_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Interface_Synchronisation
        or else Table (Types.Node_Id (N)).Kind = K_Hide_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Statements;

   procedure Set_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Var_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Interface_Synchronisation
        or else Table (Types.Node_Id (N)).Kind = K_Hide_Statement);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Statements;

   function Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Elt_Access_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Expression;

   procedure Set_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Elt_Access_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Expression;

   function Is_Out (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Out;

   procedure Set_Is_Out (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Out;

   function Is_InOut (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_InOut;

   procedure Set_Is_InOut (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Actual_Parameter);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_InOut;

   function Is_Function (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Function;

   procedure Set_Is_Function (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Function;

   function Expression_Index (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Expression_Index;

   procedure Set_Expression_Index (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Element_Assignment_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Expression_Index;

   function Variable_Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Variable_Declarations;

   procedure Set_Variable_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Variable_Declarations;

   function Var_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Var_Type;

   procedure Set_Var_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Var_Declaration);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Var_Type;

   function Case_Statement_Alternatives (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Case_Statement_Alternatives;

   procedure Set_Case_Statement_Alternatives (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Case_Statement_Alternatives;

   function Pattern_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Pattern_List;

   procedure Set_Pattern_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Pattern_List;

   function Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Condition;

   procedure Set_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Condition;

   function Then_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Then_Statements;

   procedure Set_Then_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Then_Statements;

   function Elsif_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Elsif_Statements;

   procedure Set_Elsif_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Elsif_Statements;

   function Else_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Else_Statements;

   procedure Set_Else_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Else_Statements;

   function Loop_Label (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Loop_Label;

   procedure Set_Loop_Label (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Loop_Label;

   function Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constructed_Pattern);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Parameters;

   procedure Set_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Call
        or else Table (Types.Node_Id (N)).Kind = K_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constructed_Pattern);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Parameters;

   function Operator (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Operator;

   procedure Set_Operator (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Operator;

   function Left_Part (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Left_Part;

   procedure Set_Left_Part (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Left_Part;

   function Right_Part (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Right_Part;

   procedure Set_Right_Part (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Call
        or else Table (Types.Node_Id (N)).Kind = K_Infix_Function_Call_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Pattern_Infixed);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Right_Part;

   function The_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Of
        or else Table (Types.Node_Id (N)).Kind = K_Expressions
        or else Table (Types.Node_Id (N)).Kind = K_Patterns);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end The_List;

   procedure Set_The_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Of
        or else Table (Types.Node_Id (N)).Kind = K_Expressions
        or else Table (Types.Node_Id (N)).Kind = K_Patterns);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_The_List;

   function Variable (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Variable;

   procedure Set_Variable (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesized_Pattern);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Variable;

   function Field (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Field;

   procedure Set_Field (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Field_Selection_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Field;

   function Field_Association (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update_Expression);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Field_Association;

   procedure Set_Field_Association (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Field_Update_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Field_Association;

   function Index (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Elt_Access_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Index;

   procedure Set_Index (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Elt_Access_Expression);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Index;

   function Sub_Pattern (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Sub_Pattern;

   procedure Set_Sub_Pattern (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Sub_Pattern;

   function Pattern_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Pattern_Type;

   procedure Set_Pattern_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Pattern_Type;

   function Is_Any (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Any;

   procedure Set_Is_Any (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Any;

   function Is_Of (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Of;

   procedure Set_Is_Of (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pattern);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Of;

   function Gate_Profiles (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Channel);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Gate_Profiles;

   procedure Set_Gate_Profiles (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Channel);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Gate_Profiles;

   function Gate_Types (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Profile);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Gate_Types;

   procedure Set_Gate_Types (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Profile);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Gate_Types;

   function Corresponding_Component (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Corresponding_Component;

   procedure Set_Corresponding_Component (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Corresponding_Component;

   function Process_Gate_Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Process_Gate_Declarations;

   procedure Set_Process_Gate_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Process_Gate_Declarations;

   function Process_Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Process_Parameters;

   procedure Set_Process_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Process_Parameters;

   function Process_Exceptions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Process_Exceptions;

   procedure Set_Process_Exceptions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Process_Exceptions;

   function Process_Pragma (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Process_Pragma;

   procedure Set_Process_Pragma (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Definition);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Process_Pragma;

   function Channel_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Channel_Name;

   procedure Set_Channel_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Channel_Name;

   function Gate (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Gate;

   procedure Set_Gate (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Gate_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Gate;

   function Actual_Gates (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Actual_Gates;

   procedure Set_Actual_Gates (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Actual_Gates;

   function Actual_Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Actual_Parameters;

   procedure Set_Actual_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Actual_Parameters;

   function Is_Sporadic (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Sporadic;

   procedure Set_Is_Sporadic (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Process_Instantiation_Statement);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Sporadic;

   function Offers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Offers;

   procedure Set_Offers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Offers;

   function Has_Where (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Has_Where;

   procedure Set_Has_Where (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Communication_Statement);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Has_Where;

   function Is_Input (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Input;

   procedure Set_Is_Input (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Input;

   function Pattern (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Pattern;

   procedure Set_Pattern (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Offer_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Pattern;

   function Select_Statement_Alternatives (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Select_Statement_Alternatives;

   procedure Set_Select_Statement_Alternatives (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Select_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Select_Statement_Alternatives;

   function Global_Synchronisation_Gates (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parallel_Composition_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Global_Synchronisation_Gates;

   procedure Set_Global_Synchronisation_Gates (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parallel_Composition_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Global_Synchronisation_Gates;

   function Interface_Synchronisations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parallel_Composition_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Interface_Synchronisations;

   procedure Set_Interface_Synchronisations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parallel_Composition_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Interface_Synchronisations;

   function Interface_Synchronisation_Gates (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Interface_Synchronisation);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Interface_Synchronisation_Gates;

   procedure Set_Interface_Synchronisation_Gates (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Interface_Synchronisation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Interface_Synchronisation_Gates;

   function Hide_Gate_Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Hide_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Hide_Gate_Declarations;

   procedure Set_Hide_Gate_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Hide_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Hide_Gate_Declarations;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Node_Container =>
            W_Node_Container
              (Node_Id (N));
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_Module_Definition =>
            W_Module_Definition
              (Node_Id (N));
         when K_Predefined_Function =>
            W_Predefined_Function
              (Node_Id (N));
         when K_Equality =>
            W_Equality
              (Node_Id (N));
         when K_Inequality =>
            W_Inequality
              (Node_Id (N));
         when K_Less_Than =>
            W_Less_Than
              (Node_Id (N));
         when K_Less_Than_Or_Equal_To =>
            W_Less_Than_Or_Equal_To
              (Node_Id (N));
         when K_Greater_Than =>
            W_Greater_Than
              (Node_Id (N));
         when K_Greater_Than_Or_Equal_To =>
            W_Greater_Than_Or_Equal_To
              (Node_Id (N));
         when K_Ordinal =>
            W_Ordinal
              (Node_Id (N));
         when K_Value =>
            W_Value
              (Node_Id (N));
         when K_Field_Selection =>
            W_Field_Selection
              (Node_Id (N));
         when K_Field_Update =>
            W_Field_Update
              (Node_Id (N));
         when K_Type_Cardinality =>
            W_Type_Cardinality
              (Node_Id (N));
         when K_First_Element =>
            W_First_Element
              (Node_Id (N));
         when K_Last_Element =>
            W_Last_Element
              (Node_Id (N));
         when K_And =>
            W_And
              (Node_Id (N));
         when K_Or =>
            W_Or
              (Node_Id (N));
         when K_Module_Pragma =>
            W_Module_Pragma
              (Node_Id (N));
         when K_Nat_Bits =>
            W_Nat_Bits
              (Node_Id (N));
         when K_Type_Def =>
            W_Type_Def
              (Node_Id (N));
         when K_Type_Exp =>
            W_Type_Exp
              (Node_Id (N));
         when K_RangeLNT =>
            W_RangeLNT
              (Node_Id (N));
         when K_Base_Type =>
            W_Base_Type
              (Node_Id (N));
         when K_Float =>
            W_Float
              (Node_Id (N));
         when K_Nat =>
            W_Nat
              (Node_Id (N));
         when K_Int =>
            W_Int
              (Node_Id (N));
         when K_Char =>
            W_Char
              (Node_Id (N));
         when K_String =>
            W_String
              (Node_Id (N));
         when K_Type_Constructor =>
            W_Type_Constructor
              (Node_Id (N));
         when K_Parameter_Specification =>
            W_Parameter_Specification
              (Node_Id (N));
         when K_Function_Definition =>
            W_Function_Definition
              (Node_Id (N));
         when K_Actual_Parameter =>
            W_Actual_Parameter
              (Node_Id (N));
         when K_Null_Statement =>
            W_Null_Statement
              (Node_Id (N));
         when K_Return_Statement =>
            W_Return_Statement
              (Node_Id (N));
         when K_Assignment_Statement =>
            W_Assignment_Statement
              (Node_Id (N));
         when K_Array_Element_Assignment_Statement =>
            W_Array_Element_Assignment_Statement
              (Node_Id (N));
         when K_Var_Statement =>
            W_Var_Statement
              (Node_Id (N));
         when K_Var_Declaration =>
            W_Var_Declaration
              (Node_Id (N));
         when K_Case_Statement =>
            W_Case_Statement
              (Node_Id (N));
         when K_Case_Statement_Alternative =>
            W_Case_Statement_Alternative
              (Node_Id (N));
         when K_If_Statement =>
            W_If_Statement
              (Node_Id (N));
         when K_Elsif_Statement =>
            W_Elsif_Statement
              (Node_Id (N));
         when K_Loop_Statement =>
            W_Loop_Statement
              (Node_Id (N));
         when K_While_Statement =>
            W_While_Statement
              (Node_Id (N));
         when K_Break_Statement =>
            W_Break_Statement
              (Node_Id (N));
         when K_Call =>
            W_Call
              (Node_Id (N));
         when K_Infix_Call =>
            W_Infix_Call
              (Node_Id (N));
         when K_List_Of =>
            W_List_Of
              (Node_Id (N));
         when K_Parenthesized =>
            W_Parenthesized
              (Node_Id (N));
         when K_Expressions =>
            W_Expressions
              (Node_Id (N));
         when K_Parenthesized_Expression =>
            W_Parenthesized_Expression
              (Node_Id (N));
         when K_Function_Call_Expression =>
            W_Function_Call_Expression
              (Node_Id (N));
         when K_Infix_Function_Call_Expression =>
            W_Infix_Function_Call_Expression
              (Node_Id (N));
         when K_Field_Selection_Expression =>
            W_Field_Selection_Expression
              (Node_Id (N));
         when K_Field_Update_Expression =>
            W_Field_Update_Expression
              (Node_Id (N));
         when K_Element_Association =>
            W_Element_Association
              (Node_Id (N));
         when K_Array_Elt_Access_Expression =>
            W_Array_Elt_Access_Expression
              (Node_Id (N));
         when K_Pattern =>
            W_Pattern
              (Node_Id (N));
         when K_Constructed_Pattern =>
            W_Constructed_Pattern
              (Node_Id (N));
         when K_Patterns =>
            W_Patterns
              (Node_Id (N));
         when K_Parenthesized_Pattern =>
            W_Parenthesized_Pattern
              (Node_Id (N));
         when K_Constant_Pattern_Infixed =>
            W_Constant_Pattern_Infixed
              (Node_Id (N));
         when K_Channel =>
            W_Channel
              (Node_Id (N));
         when K_Gate_Profile =>
            W_Gate_Profile
              (Node_Id (N));
         when K_Process_Definition =>
            W_Process_Definition
              (Node_Id (N));
         when K_Gate_Declaration =>
            W_Gate_Declaration
              (Node_Id (N));
         when K_Stop_Statement =>
            W_Stop_Statement
              (Node_Id (N));
         when K_Process_Instantiation_Statement =>
            W_Process_Instantiation_Statement
              (Node_Id (N));
         when K_Communication_Statement =>
            W_Communication_Statement
              (Node_Id (N));
         when K_Offer_Statement =>
            W_Offer_Statement
              (Node_Id (N));
         when K_Select_Statement =>
            W_Select_Statement
              (Node_Id (N));
         when K_Select_Statement_Alternative =>
            W_Select_Statement_Alternative
              (Node_Id (N));
         when K_Parallel_Composition_Statement =>
            W_Parallel_Composition_Statement
              (Node_Id (N));
         when K_Interface_Synchronisation =>
            W_Interface_Synchronisation
              (Node_Id (N));
         when K_Hide_Statement =>
            W_Hide_Statement
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
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Ocarina_Node",
         "Node_Id",
         Image (Ocarina_Node (N)),
         Int (Ocarina_Node (N)));
      W_Node_Attribute
        ("Corresponding_Entity",
         "Node_Id",
         Image (Corresponding_Entity (N)),
         Int (Corresponding_Entity (N)));
   end W_Identifier;

   procedure W_Module_Definition (N : Node_Id) is
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
        ("Modules",
         "List_Id",
         Image (Modules (N)),
         Int (Modules (N)));
      W_Node_Attribute
        ("Predefined_Functions",
         "List_Id",
         Image (Predefined_Functions (N)),
         Int (Predefined_Functions (N)));
      W_Node_Attribute
        ("Module_Pragmas",
         "List_Id",
         Image (Module_Pragmas (N)),
         Int (Module_Pragmas (N)));
      W_Node_Attribute
        ("Definitions",
         "List_Id",
         Image (Definitions (N)),
         Int (Definitions (N)));
   end W_Module_Definition;

   procedure W_Predefined_Function (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Predefined_Function;

   procedure W_Equality (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Equality;

   procedure W_Inequality (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Inequality;

   procedure W_Less_Than (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Less_Than;

   procedure W_Less_Than_Or_Equal_To (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Less_Than_Or_Equal_To;

   procedure W_Greater_Than (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Greater_Than;

   procedure W_Greater_Than_Or_Equal_To (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Greater_Than_Or_Equal_To;

   procedure W_Ordinal (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Ordinal;

   procedure W_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Value;

   procedure W_Field_Selection (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Field_Selection;

   procedure W_Field_Update (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Field_Update;

   procedure W_Type_Cardinality (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Type_Cardinality;

   procedure W_First_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_First_Element;

   procedure W_Last_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Last_Element;

   procedure W_And (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_And;

   procedure W_Or (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("LNT_Function",
         "Node_Id",
         Image (LNT_Function (N)),
         Int (LNT_Function (N)));
      W_Node_Attribute
        ("Is_With_Clause",
         "Boolean",
         Image (Is_With_Clause (N)));
   end W_Or;

   procedure W_Module_Pragma (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Module_Pragma_Type",
         "Node_Id",
         Image (Module_Pragma_Type (N)),
         Int (Module_Pragma_Type (N)));
      W_Node_Attribute
        ("Number_Constant",
         "Node_Id",
         Image (Number_Constant (N)),
         Int (Number_Constant (N)));
   end W_Module_Pragma;

   procedure W_Nat_Bits (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Module_Pragma_Type",
         "Node_Id",
         Image (Module_Pragma_Type (N)),
         Int (Module_Pragma_Type (N)));
      W_Node_Attribute
        ("Number_Constant",
         "Node_Id",
         Image (Number_Constant (N)),
         Int (Number_Constant (N)));
   end W_Nat_Bits;

   procedure W_Type_Def (N : Node_Id) is
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
        ("Predefined_Functions",
         "List_Id",
         Image (Predefined_Functions (N)),
         Int (Predefined_Functions (N)));
      W_Node_Attribute
        ("Type_Exp",
         "Node_Id",
         Image (Type_Exp (N)),
         Int (Type_Exp (N)));
      W_Node_Attribute
        ("Type_Pragma",
         "List_Id",
         Image (Type_Pragma (N)),
         Int (Type_Pragma (N)));
   end W_Type_Def;

   procedure W_Type_Exp (N : Node_Id) is
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
        ("Type_Constructors",
         "List_Id",
         Image (Type_Constructors (N)),
         Int (Type_Constructors (N)));
      W_Node_Attribute
        ("Is_Set",
         "Boolean",
         Image (Is_Set (N)));
      W_Node_Attribute
        ("Is_Sorted_Set",
         "Boolean",
         Image (Is_Sorted_Set (N)));
      W_Node_Attribute
        ("Is_List",
         "Boolean",
         Image (Is_List (N)));
      W_Node_Attribute
        ("Is_Sorted_List",
         "Boolean",
         Image (Is_Sorted_List (N)));
      W_Node_Attribute
        ("Is_Array",
         "Boolean",
         Image (Is_Array (N)));
      W_Node_Attribute
        ("Is_Range",
         "Boolean",
         Image (Is_Range (N)));
      W_Node_Attribute
        ("RangeLNT",
         "Node_Id",
         Image (RangeLNT (N)),
         Int (RangeLNT (N)));
   end W_Type_Exp;

   procedure W_RangeLNT (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Low_Bound",
         "Node_Id",
         Image (Low_Bound (N)),
         Int (Low_Bound (N)));
      W_Node_Attribute
        ("High_Bound",
         "Node_Id",
         Image (High_Bound (N)),
         Int (High_Bound (N)));
   end W_RangeLNT;

   procedure W_Base_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Base_Type;

   procedure W_Float (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Float;

   procedure W_Nat (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Nat;

   procedure W_Int (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Int;

   procedure W_Char (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Char;

   procedure W_String (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_String;

   procedure W_Type_Constructor (N : Node_Id) is
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
        ("Constructor_Parameters",
         "List_Id",
         Image (Constructor_Parameters (N)),
         Int (Constructor_Parameters (N)));
      W_Node_Attribute
        ("Constructor_Pragma",
         "List_Id",
         Image (Constructor_Pragma (N)),
         Int (Constructor_Pragma (N)));
   end W_Type_Constructor;

   procedure W_Parameter_Specification (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Parameter_Mode",
         "Mode_Id",
         Image (Parameter_Mode (N)));
      W_Node_Attribute
        ("Parameter_Var",
         "Node_Id",
         Image (Parameter_Var (N)),
         Int (Parameter_Var (N)));
      W_Node_Attribute
        ("Parameter_Type",
         "Node_Id",
         Image (Parameter_Type (N)),
         Int (Parameter_Type (N)));
   end W_Parameter_Specification;

   procedure W_Function_Definition (N : Node_Id) is
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
        ("Function_Parameters",
         "List_Id",
         Image (Function_Parameters (N)),
         Int (Function_Parameters (N)));
      W_Node_Attribute
        ("Function_Return_Type",
         "Node_Id",
         Image (Function_Return_Type (N)),
         Int (Function_Return_Type (N)));
      W_Node_Attribute
        ("Function_Exceptions",
         "List_Id",
         Image (Function_Exceptions (N)),
         Int (Function_Exceptions (N)));
      W_Node_Attribute
        ("Function_Pragma",
         "List_Id",
         Image (Function_Pragma (N)),
         Int (Function_Pragma (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
   end W_Function_Definition;

   procedure W_Actual_Parameter (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Is_Out",
         "Boolean",
         Image (Is_Out (N)));
      W_Node_Attribute
        ("Is_InOut",
         "Boolean",
         Image (Is_InOut (N)));
   end W_Actual_Parameter;

   procedure W_Null_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Null_Statement;

   procedure W_Return_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Is_Function",
         "Boolean",
         Image (Is_Function (N)));
   end W_Return_Statement;

   procedure W_Assignment_Statement (N : Node_Id) is
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
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Assignment_Statement;

   procedure W_Array_Element_Assignment_Statement (N : Node_Id) is
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
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Expression_Index",
         "Node_Id",
         Image (Expression_Index (N)),
         Int (Expression_Index (N)));
   end W_Array_Element_Assignment_Statement;

   procedure W_Var_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Variable_Declarations",
         "List_Id",
         Image (Variable_Declarations (N)),
         Int (Variable_Declarations (N)));
   end W_Var_Statement;

   procedure W_Var_Declaration (N : Node_Id) is
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
        ("Var_Type",
         "Node_Id",
         Image (Var_Type (N)),
         Int (Var_Type (N)));
   end W_Var_Declaration;

   procedure W_Case_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Variable_Declarations",
         "List_Id",
         Image (Variable_Declarations (N)),
         Int (Variable_Declarations (N)));
      W_Node_Attribute
        ("Case_Statement_Alternatives",
         "List_Id",
         Image (Case_Statement_Alternatives (N)),
         Int (Case_Statement_Alternatives (N)));
   end W_Case_Statement;

   procedure W_Case_Statement_Alternative (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Pattern_List",
         "List_Id",
         Image (Pattern_List (N)),
         Int (Pattern_List (N)));
   end W_Case_Statement_Alternative;

   procedure W_If_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
      W_Node_Attribute
        ("Then_Statements",
         "List_Id",
         Image (Then_Statements (N)),
         Int (Then_Statements (N)));
      W_Node_Attribute
        ("Elsif_Statements",
         "List_Id",
         Image (Elsif_Statements (N)),
         Int (Elsif_Statements (N)));
      W_Node_Attribute
        ("Else_Statements",
         "List_Id",
         Image (Else_Statements (N)),
         Int (Else_Statements (N)));
   end W_If_Statement;

   procedure W_Elsif_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
      W_Node_Attribute
        ("Then_Statements",
         "List_Id",
         Image (Then_Statements (N)),
         Int (Then_Statements (N)));
   end W_Elsif_Statement;

   procedure W_Loop_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Loop_Label",
         "Name_Id",
         Image (Loop_Label (N)));
   end W_Loop_Statement;

   procedure W_While_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_While_Statement;

   procedure W_Break_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Loop_Label",
         "Name_Id",
         Image (Loop_Label (N)));
   end W_Break_Statement;

   procedure W_Call (N : Node_Id) is
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
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
   end W_Call;

   procedure W_Infix_Call (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Operator",
         "Node_Id",
         Image (Operator (N)),
         Int (Operator (N)));
      W_Node_Attribute
        ("Left_Part",
         "Node_Id",
         Image (Left_Part (N)),
         Int (Left_Part (N)));
      W_Node_Attribute
        ("Right_Part",
         "Node_Id",
         Image (Right_Part (N)),
         Int (Right_Part (N)));
   end W_Infix_Call;

   procedure W_List_Of (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("The_List",
         "List_Id",
         Image (The_List (N)),
         Int (The_List (N)));
   end W_List_Of;

   procedure W_Parenthesized (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Variable",
         "Node_Id",
         Image (Variable (N)),
         Int (Variable (N)));
   end W_Parenthesized;

   procedure W_Expressions (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("The_List",
         "List_Id",
         Image (The_List (N)),
         Int (The_List (N)));
   end W_Expressions;

   procedure W_Parenthesized_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Variable",
         "Node_Id",
         Image (Variable (N)),
         Int (Variable (N)));
   end W_Parenthesized_Expression;

   procedure W_Function_Call_Expression (N : Node_Id) is
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
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
   end W_Function_Call_Expression;

   procedure W_Infix_Function_Call_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Operator",
         "Node_Id",
         Image (Operator (N)),
         Int (Operator (N)));
      W_Node_Attribute
        ("Left_Part",
         "Node_Id",
         Image (Left_Part (N)),
         Int (Left_Part (N)));
      W_Node_Attribute
        ("Right_Part",
         "Node_Id",
         Image (Right_Part (N)),
         Int (Right_Part (N)));
   end W_Infix_Function_Call_Expression;

   procedure W_Field_Selection_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Field",
         "Node_Id",
         Image (Field (N)),
         Int (Field (N)));
   end W_Field_Selection_Expression;

   procedure W_Field_Update_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Field_Association",
         "List_Id",
         Image (Field_Association (N)),
         Int (Field_Association (N)));
   end W_Field_Update_Expression;

   procedure W_Element_Association (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Field",
         "Node_Id",
         Image (Field (N)),
         Int (Field (N)));
   end W_Element_Association;

   procedure W_Array_Elt_Access_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Index",
         "Node_Id",
         Image (Index (N)),
         Int (Index (N)));
   end W_Array_Elt_Access_Expression;

   procedure W_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Sub_Pattern",
         "Node_Id",
         Image (Sub_Pattern (N)),
         Int (Sub_Pattern (N)));
      W_Node_Attribute
        ("Pattern_Type",
         "Node_Id",
         Image (Pattern_Type (N)),
         Int (Pattern_Type (N)));
      W_Node_Attribute
        ("Is_Any",
         "Boolean",
         Image (Is_Any (N)));
      W_Node_Attribute
        ("Is_Of",
         "Boolean",
         Image (Is_Of (N)));
   end W_Pattern;

   procedure W_Constructed_Pattern (N : Node_Id) is
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
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
   end W_Constructed_Pattern;

   procedure W_Patterns (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("The_List",
         "List_Id",
         Image (The_List (N)),
         Int (The_List (N)));
   end W_Patterns;

   procedure W_Parenthesized_Pattern (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Variable",
         "Node_Id",
         Image (Variable (N)),
         Int (Variable (N)));
   end W_Parenthesized_Pattern;

   procedure W_Constant_Pattern_Infixed (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Operator",
         "Node_Id",
         Image (Operator (N)),
         Int (Operator (N)));
      W_Node_Attribute
        ("Left_Part",
         "Node_Id",
         Image (Left_Part (N)),
         Int (Left_Part (N)));
      W_Node_Attribute
        ("Right_Part",
         "Node_Id",
         Image (Right_Part (N)),
         Int (Right_Part (N)));
   end W_Constant_Pattern_Infixed;

   procedure W_Channel (N : Node_Id) is
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
        ("Gate_Profiles",
         "List_Id",
         Image (Gate_Profiles (N)),
         Int (Gate_Profiles (N)));
   end W_Channel;

   procedure W_Gate_Profile (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Gate_Types",
         "List_Id",
         Image (Gate_Types (N)),
         Int (Gate_Types (N)));
   end W_Gate_Profile;

   procedure W_Process_Definition (N : Node_Id) is
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
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Corresponding_Component",
         "Node_Id",
         Image (Corresponding_Component (N)),
         Int (Corresponding_Component (N)));
      W_Node_Attribute
        ("Process_Gate_Declarations",
         "List_Id",
         Image (Process_Gate_Declarations (N)),
         Int (Process_Gate_Declarations (N)));
      W_Node_Attribute
        ("Process_Parameters",
         "List_Id",
         Image (Process_Parameters (N)),
         Int (Process_Parameters (N)));
      W_Node_Attribute
        ("Process_Exceptions",
         "List_Id",
         Image (Process_Exceptions (N)),
         Int (Process_Exceptions (N)));
      W_Node_Attribute
        ("Process_Pragma",
         "List_Id",
         Image (Process_Pragma (N)),
         Int (Process_Pragma (N)));
   end W_Process_Definition;

   procedure W_Gate_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Is_Any",
         "Boolean",
         Image (Is_Any (N)));
      W_Node_Attribute
        ("Channel_Name",
         "Node_Id",
         Image (Channel_Name (N)),
         Int (Channel_Name (N)));
      W_Node_Attribute
        ("Gate",
         "Node_Id",
         Image (Gate (N)),
         Int (Gate (N)));
   end W_Gate_Declaration;

   procedure W_Stop_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Stop_Statement;

   procedure W_Process_Instantiation_Statement (N : Node_Id) is
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
        ("Actual_Gates",
         "List_Id",
         Image (Actual_Gates (N)),
         Int (Actual_Gates (N)));
      W_Node_Attribute
        ("Actual_Parameters",
         "List_Id",
         Image (Actual_Parameters (N)),
         Int (Actual_Parameters (N)));
      W_Node_Attribute
        ("Is_Sporadic",
         "Boolean",
         Image (Is_Sporadic (N)));
   end W_Process_Instantiation_Statement;

   procedure W_Communication_Statement (N : Node_Id) is
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
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Offers",
         "List_Id",
         Image (Offers (N)),
         Int (Offers (N)));
      W_Node_Attribute
        ("Has_Where",
         "Boolean",
         Image (Has_Where (N)));
   end W_Communication_Statement;

   procedure W_Offer_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Is_Input",
         "Boolean",
         Image (Is_Input (N)));
      W_Node_Attribute
        ("Pattern",
         "Node_Id",
         Image (Pattern (N)),
         Int (Pattern (N)));
   end W_Offer_Statement;

   procedure W_Select_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Select_Statement_Alternatives",
         "List_Id",
         Image (Select_Statement_Alternatives (N)),
         Int (Select_Statement_Alternatives (N)));
   end W_Select_Statement;

   procedure W_Select_Statement_Alternative (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
   end W_Select_Statement_Alternative;

   procedure W_Parallel_Composition_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Global_Synchronisation_Gates",
         "List_Id",
         Image (Global_Synchronisation_Gates (N)),
         Int (Global_Synchronisation_Gates (N)));
      W_Node_Attribute
        ("Interface_Synchronisations",
         "List_Id",
         Image (Interface_Synchronisations (N)),
         Int (Interface_Synchronisations (N)));
   end W_Parallel_Composition_Statement;

   procedure W_Interface_Synchronisation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Interface_Synchronisation_Gates",
         "List_Id",
         Image (Interface_Synchronisation_Gates (N)),
         Int (Interface_Synchronisation_Gates (N)));
   end W_Interface_Synchronisation;

   procedure W_Hide_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Hide_Gate_Declarations",
         "List_Id",
         Image (Hide_Gate_Declarations (N)),
         Int (Hide_Gate_Declarations (N)));
   end W_Hide_Statement;

end Ocarina.Backends.LNT.Nodes;
