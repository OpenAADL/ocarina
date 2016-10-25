pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.Backends.C_Tree.Debug; use Ocarina.Backends.C_Tree.Debug;

package body Ocarina.Backends.C_Tree.Nodes is

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
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Array_Values
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Continue_Statement
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Enum_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Address
        or else Table (Types.Node_Id (N)).Kind = K_Extern_Entity_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Array_Values
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Continue_Statement
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Enum_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Address
        or else Table (Types.Node_Id (N)).Kind = K_Extern_Entity_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Next_Node;

   function Frontend_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Array_Values
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Continue_Statement
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Enum_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Address
        or else Table (Types.Node_Id (N)).Kind = K_Extern_Entity_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end Frontend_Node;

   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Array_Values
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Break_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Continue_Statement
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Enum_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Address
        or else Table (Types.Node_Id (N)).Kind = K_Extern_Entity_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Frontend_Node;

   function Defining_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Defining_Identifier;

   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Defining_Identifier;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Includes
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Header_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_List
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Element_List
        or else Table (Types.Node_Id (N)).Kind = K_Label_List
        or else Table (Types.Node_Id (N)).Kind = K_Alternatives_List
        or else Table (Types.Node_Id (N)).Kind = K_Sources
        or else Table (Types.Node_Id (N)).Kind = K_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Includes
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Header_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_List
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Element_List
        or else Table (Types.Node_Id (N)).Kind = K_Label_List
        or else Table (Types.Node_Id (N)).Kind = K_Alternatives_List
        or else Table (Types.Node_Id (N)).Kind = K_Sources
        or else Table (Types.Node_Id (N)).Kind = K_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Includes
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Header_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_List
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Element_List
        or else Table (Types.Node_Id (N)).Kind = K_Label_List
        or else Table (Types.Node_Id (N)).Kind = K_Alternatives_List
        or else Table (Types.Node_Id (N)).Kind = K_Sources
        or else Table (Types.Node_Id (N)).Kind = K_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Includes
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Header_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_List
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Element_List
        or else Table (Types.Node_Id (N)).Kind = K_Label_List
        or else Table (Types.Node_Id (N)).Kind = K_Alternatives_List
        or else Table (Types.Node_Id (N)).Kind = K_Sources
        or else Table (Types.Node_Id (N)).Kind = K_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return Name_Id (Table (Types.Node_Id (N)).L (2));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Name;

   function Corresponding_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Corresponding_Node;

   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Corresponding_Node;

   function Compile_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Compile_Unit;

   procedure Set_Compile_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Compile_Unit;

   function Is_Pointer (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Pointer;

   procedure Set_Is_Pointer (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Pointer;

   function Clause (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Clause;

   procedure Set_Clause (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Clause;

   function Then_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Then_Statements;

   procedure Set_Then_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Then_Statements;

   function Else_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Else_Statements;

   procedure Set_Else_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Else_Statements;

   function Negation (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Negation;

   procedure Set_Negation (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ifdef_Clause);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Negation;

   function Header_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Header_Name;

   procedure Set_Header_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Header_Name;

   function Is_Local (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Local;

   procedure Set_Is_Local (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Include_Clause);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Local;

   function Values (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Values);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Values;

   procedure Set_Values (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Values);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Values;

   function Array_Item (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Array_Item;

   procedure Set_Array_Item (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Value);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Array_Item;

   function Units (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Units;

   procedure Set_Units (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Units;

   function HI_Nodes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end HI_Nodes;

   procedure Set_HI_Nodes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_HI_Nodes;

   function Distributed_Application (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Distributed_Application;

   procedure Set_Distributed_Application (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Distributed_Application;

   function Distributed_Application_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Distributed_Application_Unit;

   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Distributed_Application_Unit;

   function Included_Headers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Included_Headers;

   procedure Set_Included_Headers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Included_Headers;

   function Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Declarations;

   procedure Set_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Header_File
        or else Table (Types.Node_Id (N)).Kind = K_Source_File
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Declarations;

   function Sources (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Sources;

   procedure Set_Sources (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Sources;

   function Headers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Headers;

   procedure Set_Headers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Headers;

   function Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Extern_Entity_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Entity;

   procedure Set_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Extern_Entity_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Entity;

   function Main_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Main_Source;

   procedure Set_Main_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Main_Source;

   function Main_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Main_Header;

   procedure Set_Main_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Main_Header;

   function Activity_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Activity_Source;

   procedure Set_Activity_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Activity_Source;

   function Activity_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Activity_Header;

   procedure Set_Activity_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Activity_Header;

   function Marshallers_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Marshallers_Source;

   procedure Set_Marshallers_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Marshallers_Source;

   function Marshallers_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Marshallers_Header;

   procedure Set_Marshallers_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Marshallers_Header;

   function Request_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Request_Source;

   procedure Set_Request_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Request_Source;

   function Request_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Request_Header;

   procedure Set_Request_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Request_Header;

   function Types_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Types_Header;

   procedure Set_Types_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Types_Header;

   function Types_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Types_Source;

   procedure Set_Types_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Types_Source;

   function Deployment_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Deployment_Header;

   procedure Set_Deployment_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Deployment_Header;

   function Deployment_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (17));
   end Deployment_Source;

   procedure Set_Deployment_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (17) := Int (V);
   end Set_Deployment_Source;

   function Naming_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (18));
   end Naming_Source;

   procedure Set_Naming_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (18) := Int (V);
   end Set_Naming_Source;

   function Naming_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (19));
   end Naming_Header;

   procedure Set_Naming_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (19) := Int (V);
   end Set_Naming_Header;

   function Subprograms_Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (20));
   end Subprograms_Source;

   procedure Set_Subprograms_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (20) := Int (V);
   end Set_Subprograms_Source;

   function Subprograms_Header (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (21));
   end Subprograms_Header;

   procedure Set_Subprograms_Header (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (21) := Int (V);
   end Set_Subprograms_Header;

   function Parameter_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Parameter_Type;

   procedure Set_Parameter_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Parameter_Type;

   function Parameters (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Parameters;

   procedure Set_Parameters (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Call_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Call);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Parameters;

   function Return_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Return_Type;

   procedure Set_Return_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Specification);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Return_Type;

   function Specification (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Specification;

   procedure Set_Specification (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Specification;

   function Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Statements;

   procedure Set_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Function_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Statements;

   function Type_Definition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Type_Definition;

   procedure Set_Type_Definition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Type_Definition;

   function Type_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Type_Name;

   procedure Set_Type_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Type_Name;

   function Declarative_Part (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Declarative_Part;

   procedure Set_Declarative_Part (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Declarative_Part;

   function Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Condition;

   procedure Set_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_While_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Condition;

   function Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Address);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Expression;

   procedure Set_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Macro_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Address);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Expression;

   function Pre_Cond (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Pre_Cond;

   procedure Set_Pre_Cond (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Pre_Cond;

   function Post_Cond (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Post_Cond;

   procedure Set_Post_Cond (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Post_Cond;

   function Alternatives (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Alternatives;

   procedure Set_Alternatives (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Alternatives;

   function Labels (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Labels;

   procedure Set_Labels (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Switch_Alternative);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Labels;

   function Has_Header_Spaces (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Has_Header_Spaces;

   procedure Set_Has_Header_Spaces (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_C_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Has_Header_Spaces;

   function For_Struct (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end For_Struct;

   procedure Set_For_Struct (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_For_Struct;

   function For_Union (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end For_Union;

   procedure Set_For_Union (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_For_Union;

   function For_Enum (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end For_Enum;

   procedure Set_For_Enum (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_For_Enum;

   function For_Function (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (5));
   end For_Function;

   procedure Set_For_Function (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (5) := Boolean (V);
   end Set_For_Function;

   function For_Variable (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (6));
   end For_Variable;

   procedure Set_For_Variable (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (6) := Boolean (V);
   end Set_For_Variable;

   function For_Define (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (7));
   end For_Define;

   procedure Set_For_Define (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (7) := Boolean (V);
   end Set_For_Define;

   function For_Typedef (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (8));
   end For_Typedef;

   procedure Set_For_Typedef (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (8) := Boolean (V);
   end Set_For_Typedef;

   function For_File (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (9));
   end For_File;

   procedure Set_For_File (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (9) := Boolean (V);
   end Set_For_File;

   function For_Namespace (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (10));
   end For_Namespace;

   procedure Set_For_Namespace (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (10) := Boolean (V);
   end Set_For_Namespace;

   function For_Package (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (11));
   end For_Package;

   procedure Set_For_Package (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (11) := Boolean (V);
   end Set_For_Package;

   function For_Interface (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (12));
   end For_Interface;

   procedure Set_For_Interface (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).B (12) := Boolean (V);
   end Set_For_Interface;

   function Summary (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Summary;

   procedure Set_Summary (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Summary;

   function Description (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Description;

   procedure Set_Description (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Description;

   function Element (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      return Node_Id (Table (Types.Node_Id (N)).L (17));
   end Element;

   procedure Set_Element (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Doxygen_C_Comment);

      Table (Types.Node_Id (N)).L (17) := Int (V);
   end Set_Element;

   function Defined_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Defined_Value;

   procedure Set_Defined_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Define_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Defined_Value;

   function Array_Size (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Array_Size;

   procedure Set_Array_Size (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Declaration);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Array_Size;

   function Struct_Members (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Struct_Members;

   procedure Set_Struct_Members (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Struct_Aggregate);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Struct_Members;

   function Union_Members (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Union_Members;

   procedure Set_Union_Members (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Union_Aggregate);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Union_Members;

   function Enum_Members (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enum_Aggregate);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Enum_Members;

   procedure Set_Enum_Members (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enum_Aggregate);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Enum_Members;

   function Used_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Pointer_Type
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Used_Type;

   procedure Set_Used_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Member_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Pointer_Type
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Type);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Used_Type;

   function Is_Static (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Static;

   procedure Set_Is_Static (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variable_Declaration);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Static;

   function Aggregate_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Aggregate_Name;

   procedure Set_Aggregate_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Member_Designator);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Aggregate_Name;

   function Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Value;

   procedure Set_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value;

   function Operator (N : Node_Id) return Operator_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      return Operator_Id (Table (Types.Node_Id (N)).O (1));
   end Operator;

   procedure Set_Operator (N : Node_Id; V : Operator_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Operator;

   function Left_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Left_Expression;

   procedure Set_Left_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Left_Expression;

   function Right_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Right_Expression;

   procedure Set_Right_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Right_Expression;

   function Subtype_Mark (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Subtype_Mark;

   procedure Set_Subtype_Mark (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Subtype_Mark;

   function Image (N : Base_Type) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_Short
        or else Table (Types.Node_Id (N)).Kind = K_Void
        or else Table (Types.Node_Id (N)).Kind = K_Pointed_Char);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Image;

   procedure Set_Image (N : Base_Type; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_Short
        or else Table (Types.Node_Id (N)).Kind = K_Void
        or else Table (Types.Node_Id (N)).Kind = K_Pointed_Char);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Image;

   function Main_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Main_Node;

   procedure Set_Main_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Main_Node;

   function Type_Definition_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Type_Definition_Node;

   procedure Set_Type_Definition_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Type_Definition_Node;

   function Feature_Subprogram_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Feature_Subprogram_Node;

   procedure Set_Feature_Subprogram_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Feature_Subprogram_Node;

   function Subprogram_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Subprogram_Node;

   procedure Set_Subprogram_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Subprogram_Node;

   function Internals_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Internals_Node;

   procedure Set_Internals_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Internals_Node;

   function System_Time_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end System_Time_Node;

   procedure Set_System_Time_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_System_Time_Node;

   function Marshaller_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Marshaller_Node;

   procedure Set_Marshaller_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Marshaller_Node;

   function Unmarshaller_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Unmarshaller_Node;

   procedure Set_Unmarshaller_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Unmarshaller_Node;

   function Activity_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Activity_Node;

   procedure Set_Activity_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Activity_Node;

   function Functions_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Functions_Node;

   procedure Set_Functions_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Functions_Node;

   function Types_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Types_Node;

   procedure Set_Types_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Types_Node;

   function Object_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Object_Node;

   procedure Set_Object_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Object_Node;

   function Handlers_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Handlers_Node;

   procedure Set_Handlers_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Handlers_Node;

   function Global_Port_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Global_Port_Node;

   procedure Set_Global_Port_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Global_Port_Node;

   function Local_Port_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (17));
   end Local_Port_Node;

   procedure Set_Local_Port_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (17) := Int (V);
   end Set_Local_Port_Node;

   function Global_Names_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (18));
   end Global_Names_Node;

   procedure Set_Global_Names_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (18) := Int (V);
   end Set_Global_Names_Node;

   function Global_Model_Names_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (19));
   end Global_Model_Names_Node;

   procedure Set_Global_Model_Names_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (19) := Int (V);
   end Set_Global_Model_Names_Node;

   function Request_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (20));
   end Request_Node;

   procedure Set_Request_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (20) := Int (V);
   end Set_Request_Node;

   function Deployment_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (21));
   end Deployment_Node;

   procedure Set_Deployment_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (21) := Int (V);
   end Set_Deployment_Node;

   function Entities_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (22));
   end Entities_Node;

   procedure Set_Entities_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (22) := Int (V);
   end Set_Entities_Node;

   function Servers_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (23));
   end Servers_Node;

   procedure Set_Servers_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (23) := Int (V);
   end Set_Servers_Node;

   function Naming_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (24));
   end Naming_Node;

   procedure Set_Naming_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (24) := Int (V);
   end Set_Naming_Node;

   function Processes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return List_Id (Table (Types.Node_Id (N)).L (25));
   end Processes;

   procedure Set_Processes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (25) := Int (V);
   end Set_Processes;

   function Request_Type_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (26));
   end Request_Type_Node;

   procedure Set_Request_Type_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (26) := Int (V);
   end Set_Request_Type_Node;

   function Job_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (27));
   end Job_Node;

   procedure Set_Job_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (27) := Int (V);
   end Set_Job_Node;

   function Stub_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (28));
   end Stub_Node;

   procedure Set_Stub_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (28) := Int (V);
   end Set_Stub_Node;

   function Enumerator_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (29));
   end Enumerator_Node;

   procedure Set_Enumerator_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (29) := Int (V);
   end Set_Enumerator_Node;

   function Process_Request_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (30));
   end Process_Request_Node;

   procedure Set_Process_Request_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (30) := Int (V);
   end Set_Process_Request_Node;

   function Marshall_Request_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (31));
   end Marshall_Request_Node;

   procedure Set_Marshall_Request_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (31) := Int (V);
   end Set_Marshall_Request_Node;

   function Unmarshall_Request_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (32));
   end Unmarshall_Request_Node;

   procedure Set_Unmarshall_Request_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (32) := Int (V);
   end Set_Unmarshall_Request_Node;

   function Default_Value_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (33));
   end Default_Value_Node;

   procedure Set_Default_Value_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (33) := Int (V);
   end Set_Default_Value_Node;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Definition =>
            W_Definition
              (Node_Id (N));
         when K_Defining_Identifier =>
            W_Defining_Identifier
              (Node_Id (N));
         when K_Ifdef_Clause =>
            W_Ifdef_Clause
              (Node_Id (N));
         when K_Include_Clause =>
            W_Include_Clause
              (Node_Id (N));
         when K_Includes =>
            W_Includes
              (List_Id (N));
         when K_Declaration_List =>
            W_Declaration_List
              (List_Id (N));
         when K_Header_List =>
            W_Header_List
              (List_Id (N));
         when K_Statement_List =>
            W_Statement_List
              (List_Id (N));
         when K_Parameter_List =>
            W_Parameter_List
              (List_Id (N));
         when K_Enumeration_Literals =>
            W_Enumeration_Literals
              (List_Id (N));
         when K_Element_List =>
            W_Element_List
              (List_Id (N));
         when K_Label_List =>
            W_Label_List
              (List_Id (N));
         when K_Alternatives_List =>
            W_Alternatives_List
              (List_Id (N));
         when K_Sources =>
            W_Sources
              (List_Id (N));
         when K_Headers =>
            W_Headers
              (List_Id (N));
         when K_Array_Values =>
            W_Array_Values
              (Node_Id (N));
         when K_Array_Value =>
            W_Array_Value
              (Node_Id (N));
         when K_HI_Distributed_Application =>
            W_HI_Distributed_Application
              (Node_Id (N));
         when K_HI_Node =>
            W_HI_Node
              (Node_Id (N));
         when K_Header_File =>
            W_Header_File
              (Node_Id (N));
         when K_Source_File =>
            W_Source_File
              (Node_Id (N));
         when K_API_Unit =>
            W_API_Unit
              (Node_Id (N));
         when K_HI_Unit =>
            W_HI_Unit
              (Node_Id (N));
         when K_Parameter_Specification =>
            W_Parameter_Specification
              (Node_Id (N));
         when K_Parameter_Profile =>
            W_Parameter_Profile
              (List_Id (N));
         when K_Function_Specification =>
            W_Function_Specification
              (Node_Id (N));
         when K_Function_Implementation =>
            W_Function_Implementation
              (Node_Id (N));
         when K_Call_Profile =>
            W_Call_Profile
              (Node_Id (N));
         when K_Macro_Call =>
            W_Macro_Call
              (Node_Id (N));
         when K_Full_Type_Declaration =>
            W_Full_Type_Declaration
              (Node_Id (N));
         when K_Block_Statement =>
            W_Block_Statement
              (Node_Id (N));
         when K_If_Statement =>
            W_If_Statement
              (Node_Id (N));
         when K_Assignment_Statement =>
            W_Assignment_Statement
              (Node_Id (N));
         when K_Return_Statement =>
            W_Return_Statement
              (Node_Id (N));
         when K_For_Statement =>
            W_For_Statement
              (Node_Id (N));
         when K_While_Statement =>
            W_While_Statement
              (Node_Id (N));
         when K_Switch_Statement =>
            W_Switch_Statement
              (Node_Id (N));
         when K_Switch_Alternative =>
            W_Switch_Alternative
              (Node_Id (N));
         when K_Break_Statement =>
            W_Break_Statement
              (Node_Id (N));
         when K_Continue_Statement =>
            W_Continue_Statement
              (Node_Id (N));
         when K_C_Comment =>
            W_C_Comment
              (Node_Id (N));
         when K_Doxygen_C_Comment =>
            W_Doxygen_C_Comment
              (Node_Id (N));
         when K_Define_Statement =>
            W_Define_Statement
              (Node_Id (N));
         when K_Array_Declaration =>
            W_Array_Declaration
              (Node_Id (N));
         when K_Struct_Aggregate =>
            W_Struct_Aggregate
              (Node_Id (N));
         when K_Union_Aggregate =>
            W_Union_Aggregate
              (Node_Id (N));
         when K_Enum_Aggregate =>
            W_Enum_Aggregate
              (Node_Id (N));
         when K_Variable_Declaration =>
            W_Variable_Declaration
              (Node_Id (N));
         when K_Member_Declaration =>
            W_Member_Declaration
              (Node_Id (N));
         when K_Member_Designator =>
            W_Member_Designator
              (Node_Id (N));
         when K_Macro_Declaration =>
            W_Macro_Declaration
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_Expression =>
            W_Expression
              (Node_Id (N));
         when K_Type_Conversion =>
            W_Type_Conversion
              (Node_Id (N));
         when K_Variable_Address =>
            W_Variable_Address
              (Node_Id (N));
         when K_Extern_Entity_Declaration =>
            W_Extern_Entity_Declaration
              (Node_Id (N));
         when K_Float =>
            W_Float
              (Base_Type (N));
         when K_Int =>
            W_Int
              (Base_Type (N));
         when K_Char =>
            W_Char
              (Base_Type (N));
         when K_Short =>
            W_Short
              (Base_Type (N));
         when K_Void =>
            W_Void
              (Base_Type (N));
         when K_Pointed_Char =>
            W_Pointed_Char
              (Base_Type (N));
         when K_Tree_Bindings =>
            W_Tree_Bindings
              (Node_Id (N));
         when K_HI_Tree_Bindings =>
            W_HI_Tree_Bindings
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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
   end W_Definition;

   procedure W_Defining_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Corresponding_Node",
         "Node_Id",
         Image (Corresponding_Node (N)),
         Int (Corresponding_Node (N)));
      W_Node_Attribute
        ("Compile_Unit",
         "Node_Id",
         Image (Compile_Unit (N)),
         Int (Compile_Unit (N)));
      W_Node_Attribute
        ("Is_Pointer",
         "Boolean",
         Image (Is_Pointer (N)));
   end W_Defining_Identifier;

   procedure W_Ifdef_Clause (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Clause",
         "Node_Id",
         Image (Clause (N)),
         Int (Clause (N)));
      W_Node_Attribute
        ("Then_Statements",
         "List_Id",
         Image (Then_Statements (N)),
         Int (Then_Statements (N)));
      W_Node_Attribute
        ("Else_Statements",
         "List_Id",
         Image (Else_Statements (N)),
         Int (Else_Statements (N)));
      W_Node_Attribute
        ("Negation",
         "Boolean",
         Image (Negation (N)));
   end W_Ifdef_Clause;

   procedure W_Include_Clause (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Header_Name",
         "Node_Id",
         Image (Header_Name (N)),
         Int (Header_Name (N)));
      W_Node_Attribute
        ("Is_Local",
         "Boolean",
         Image (Is_Local (N)));
   end W_Include_Clause;

   procedure W_Includes (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Includes;

   procedure W_Declaration_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Declaration_List;

   procedure W_Header_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Header_List;

   procedure W_Statement_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Statement_List;

   procedure W_Parameter_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Parameter_List;

   procedure W_Enumeration_Literals (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Enumeration_Literals;

   procedure W_Element_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Element_List;

   procedure W_Label_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Label_List;

   procedure W_Alternatives_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Alternatives_List;

   procedure W_Sources (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Sources;

   procedure W_Headers (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Headers;

   procedure W_Array_Values (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Values",
         "List_Id",
         Image (Values (N)),
         Int (Values (N)));
   end W_Array_Values;

   procedure W_Array_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Array_Item",
         "Node_Id",
         Image (Array_Item (N)),
         Int (Array_Item (N)));
   end W_Array_Value;

   procedure W_HI_Distributed_Application (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Units",
         "List_Id",
         Image (Units (N)),
         Int (Units (N)));
      W_Node_Attribute
        ("HI_Nodes",
         "List_Id",
         Image (HI_Nodes (N)),
         Int (HI_Nodes (N)));
   end W_HI_Distributed_Application;

   procedure W_HI_Node (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Units",
         "List_Id",
         Image (Units (N)),
         Int (Units (N)));
      W_Node_Attribute
        ("Distributed_Application",
         "Node_Id",
         Image (Distributed_Application (N)),
         Int (Distributed_Application (N)));
   end W_HI_Node;

   procedure W_Header_File (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Distributed_Application_Unit",
         "Node_Id",
         Image (Distributed_Application_Unit (N)),
         Int (Distributed_Application_Unit (N)));
      W_Node_Attribute
        ("Included_Headers",
         "List_Id",
         Image (Included_Headers (N)),
         Int (Included_Headers (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
   end W_Header_File;

   procedure W_Source_File (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Distributed_Application_Unit",
         "Node_Id",
         Image (Distributed_Application_Unit (N)),
         Int (Distributed_Application_Unit (N)));
      W_Node_Attribute
        ("Included_Headers",
         "List_Id",
         Image (Included_Headers (N)),
         Int (Included_Headers (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
   end W_Source_File;

   procedure W_API_Unit (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
      W_Node_Attribute
        ("Headers",
         "List_Id",
         Image (Headers (N)),
         Int (Headers (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
   end W_API_Unit;

   procedure W_HI_Unit (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Sources",
         "List_Id",
         Image (Sources (N)),
         Int (Sources (N)));
      W_Node_Attribute
        ("Headers",
         "List_Id",
         Image (Headers (N)),
         Int (Headers (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Main_Source",
         "Node_Id",
         Image (Main_Source (N)),
         Int (Main_Source (N)));
      W_Node_Attribute
        ("Main_Header",
         "Node_Id",
         Image (Main_Header (N)),
         Int (Main_Header (N)));
      W_Node_Attribute
        ("Activity_Source",
         "Node_Id",
         Image (Activity_Source (N)),
         Int (Activity_Source (N)));
      W_Node_Attribute
        ("Activity_Header",
         "Node_Id",
         Image (Activity_Header (N)),
         Int (Activity_Header (N)));
      W_Node_Attribute
        ("Marshallers_Source",
         "Node_Id",
         Image (Marshallers_Source (N)),
         Int (Marshallers_Source (N)));
      W_Node_Attribute
        ("Marshallers_Header",
         "Node_Id",
         Image (Marshallers_Header (N)),
         Int (Marshallers_Header (N)));
      W_Node_Attribute
        ("Request_Source",
         "Node_Id",
         Image (Request_Source (N)),
         Int (Request_Source (N)));
      W_Node_Attribute
        ("Request_Header",
         "Node_Id",
         Image (Request_Header (N)),
         Int (Request_Header (N)));
      W_Node_Attribute
        ("Types_Header",
         "Node_Id",
         Image (Types_Header (N)),
         Int (Types_Header (N)));
      W_Node_Attribute
        ("Types_Source",
         "Node_Id",
         Image (Types_Source (N)),
         Int (Types_Source (N)));
      W_Node_Attribute
        ("Deployment_Header",
         "Node_Id",
         Image (Deployment_Header (N)),
         Int (Deployment_Header (N)));
      W_Node_Attribute
        ("Deployment_Source",
         "Node_Id",
         Image (Deployment_Source (N)),
         Int (Deployment_Source (N)));
      W_Node_Attribute
        ("Naming_Source",
         "Node_Id",
         Image (Naming_Source (N)),
         Int (Naming_Source (N)));
      W_Node_Attribute
        ("Naming_Header",
         "Node_Id",
         Image (Naming_Header (N)),
         Int (Naming_Header (N)));
      W_Node_Attribute
        ("Subprograms_Source",
         "Node_Id",
         Image (Subprograms_Source (N)),
         Int (Subprograms_Source (N)));
      W_Node_Attribute
        ("Subprograms_Header",
         "Node_Id",
         Image (Subprograms_Header (N)),
         Int (Subprograms_Header (N)));
   end W_HI_Unit;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parameter_Type",
         "Node_Id",
         Image (Parameter_Type (N)),
         Int (Parameter_Type (N)));
   end W_Parameter_Specification;

   procedure W_Parameter_Profile (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Parameter_Profile;

   procedure W_Function_Specification (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Compile_Unit",
         "Node_Id",
         Image (Compile_Unit (N)),
         Int (Compile_Unit (N)));
      W_Node_Attribute
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
      W_Node_Attribute
        ("Return_Type",
         "Node_Id",
         Image (Return_Type (N)),
         Int (Return_Type (N)));
   end W_Function_Specification;

   procedure W_Function_Implementation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Compile_Unit",
         "Node_Id",
         Image (Compile_Unit (N)),
         Int (Compile_Unit (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
      W_Node_Attribute
        ("Specification",
         "Node_Id",
         Image (Specification (N)),
         Int (Specification (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
   end W_Function_Implementation;

   procedure W_Call_Profile (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
   end W_Call_Profile;

   procedure W_Macro_Call (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parameters",
         "List_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
   end W_Macro_Call;

   procedure W_Full_Type_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Type_Definition",
         "Node_Id",
         Image (Type_Definition (N)),
         Int (Type_Definition (N)));
      W_Node_Attribute
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
   end W_Full_Type_Declaration;

   procedure W_Block_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Declarative_Part",
         "List_Id",
         Image (Declarative_Part (N)),
         Int (Declarative_Part (N)));
   end W_Block_Statement;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Else_Statements",
         "List_Id",
         Image (Else_Statements (N)),
         Int (Else_Statements (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
   end W_If_Statement;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Assignment_Statement;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Return_Statement;

   procedure W_For_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
      W_Node_Attribute
        ("Pre_Cond",
         "Node_Id",
         Image (Pre_Cond (N)),
         Int (Pre_Cond (N)));
      W_Node_Attribute
        ("Post_Cond",
         "Node_Id",
         Image (Post_Cond (N)),
         Int (Post_Cond (N)));
   end W_For_Statement;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
   end W_While_Statement;

   procedure W_Switch_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Alternatives",
         "List_Id",
         Image (Alternatives (N)),
         Int (Alternatives (N)));
   end W_Switch_Statement;

   procedure W_Switch_Alternative (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Labels",
         "List_Id",
         Image (Labels (N)),
         Int (Labels (N)));
   end W_Switch_Alternative;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
   end W_Break_Statement;

   procedure W_Continue_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
   end W_Continue_Statement;

   procedure W_C_Comment (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Has_Header_Spaces",
         "Boolean",
         Image (Has_Header_Spaces (N)));
   end W_C_Comment;

   procedure W_Doxygen_C_Comment (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Has_Header_Spaces",
         "Boolean",
         Image (Has_Header_Spaces (N)));
      W_Node_Attribute
        ("For_Struct",
         "Boolean",
         Image (For_Struct (N)));
      W_Node_Attribute
        ("For_Union",
         "Boolean",
         Image (For_Union (N)));
      W_Node_Attribute
        ("For_Enum",
         "Boolean",
         Image (For_Enum (N)));
      W_Node_Attribute
        ("For_Function",
         "Boolean",
         Image (For_Function (N)));
      W_Node_Attribute
        ("For_Variable",
         "Boolean",
         Image (For_Variable (N)));
      W_Node_Attribute
        ("For_Define",
         "Boolean",
         Image (For_Define (N)));
      W_Node_Attribute
        ("For_Typedef",
         "Boolean",
         Image (For_Typedef (N)));
      W_Node_Attribute
        ("For_File",
         "Boolean",
         Image (For_File (N)));
      W_Node_Attribute
        ("For_Namespace",
         "Boolean",
         Image (For_Namespace (N)));
      W_Node_Attribute
        ("For_Package",
         "Boolean",
         Image (For_Package (N)));
      W_Node_Attribute
        ("For_Interface",
         "Boolean",
         Image (For_Interface (N)));
      W_Node_Attribute
        ("Summary",
         "Node_Id",
         Image (Summary (N)),
         Int (Summary (N)));
      W_Node_Attribute
        ("Description",
         "Node_Id",
         Image (Description (N)),
         Int (Description (N)));
      W_Node_Attribute
        ("Element",
         "Node_Id",
         Image (Element (N)),
         Int (Element (N)));
   end W_Doxygen_C_Comment;

   procedure W_Define_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Defined_Value",
         "Node_Id",
         Image (Defined_Value (N)),
         Int (Defined_Value (N)));
   end W_Define_Statement;

   procedure W_Array_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Array_Size",
         "Node_Id",
         Image (Array_Size (N)),
         Int (Array_Size (N)));
   end W_Array_Declaration;

   procedure W_Struct_Aggregate (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Struct_Members",
         "List_Id",
         Image (Struct_Members (N)),
         Int (Struct_Members (N)));
   end W_Struct_Aggregate;

   procedure W_Union_Aggregate (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Union_Members",
         "List_Id",
         Image (Union_Members (N)),
         Int (Union_Members (N)));
   end W_Union_Aggregate;

   procedure W_Enum_Aggregate (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Enum_Members",
         "List_Id",
         Image (Enum_Members (N)),
         Int (Enum_Members (N)));
   end W_Enum_Aggregate;

   procedure W_Variable_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Used_Type",
         "Node_Id",
         Image (Used_Type (N)),
         Int (Used_Type (N)));
      W_Node_Attribute
        ("Is_Static",
         "Boolean",
         Image (Is_Static (N)));
   end W_Variable_Declaration;

   procedure W_Member_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Used_Type",
         "Node_Id",
         Image (Used_Type (N)),
         Int (Used_Type (N)));
   end W_Member_Declaration;

   procedure W_Member_Designator (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Is_Pointer",
         "Boolean",
         Image (Is_Pointer (N)));
      W_Node_Attribute
        ("Aggregate_Name",
         "Node_Id",
         Image (Aggregate_Name (N)),
         Int (Aggregate_Name (N)));
   end W_Member_Designator;

   procedure W_Macro_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Macro_Declaration;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Value",
         "Value_Id",
         Image (Value (N)));
   end W_Literal;

   procedure W_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Operator",
         "Operator_Id",
         Image (Operator (N)));
      W_Node_Attribute
        ("Left_Expression",
         "Node_Id",
         Image (Left_Expression (N)),
         Int (Left_Expression (N)));
      W_Node_Attribute
        ("Right_Expression",
         "Node_Id",
         Image (Right_Expression (N)),
         Int (Right_Expression (N)));
   end W_Expression;

   procedure W_Type_Conversion (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Subtype_Mark",
         "Node_Id",
         Image (Subtype_Mark (N)),
         Int (Subtype_Mark (N)));
   end W_Type_Conversion;

   procedure W_Variable_Address (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Variable_Address;

   procedure W_Extern_Entity_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
   end W_Extern_Entity_Declaration;

   procedure W_Float (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Float;

   procedure W_Int (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Int;

   procedure W_Char (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Char;

   procedure W_Short (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Short;

   procedure W_Void (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Void;

   procedure W_Pointed_Char (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Pointed_Char;

   procedure W_Tree_Bindings (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Node",
         "Node_Id",
         Image (Main_Node (N)),
         Int (Main_Node (N)));
      W_Node_Attribute
        ("Type_Definition_Node",
         "Node_Id",
         Image (Type_Definition_Node (N)),
         Int (Type_Definition_Node (N)));
      W_Node_Attribute
        ("Feature_Subprogram_Node",
         "Node_Id",
         Image (Feature_Subprogram_Node (N)),
         Int (Feature_Subprogram_Node (N)));
      W_Node_Attribute
        ("Subprogram_Node",
         "Node_Id",
         Image (Subprogram_Node (N)),
         Int (Subprogram_Node (N)));
   end W_Tree_Bindings;

   procedure W_HI_Tree_Bindings (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Node",
         "Node_Id",
         Image (Main_Node (N)),
         Int (Main_Node (N)));
      W_Node_Attribute
        ("Type_Definition_Node",
         "Node_Id",
         Image (Type_Definition_Node (N)),
         Int (Type_Definition_Node (N)));
      W_Node_Attribute
        ("Feature_Subprogram_Node",
         "Node_Id",
         Image (Feature_Subprogram_Node (N)),
         Int (Feature_Subprogram_Node (N)));
      W_Node_Attribute
        ("Subprogram_Node",
         "Node_Id",
         Image (Subprogram_Node (N)),
         Int (Subprogram_Node (N)));
      W_Node_Attribute
        ("Internals_Node",
         "Node_Id",
         Image (Internals_Node (N)),
         Int (Internals_Node (N)));
      W_Node_Attribute
        ("System_Time_Node",
         "Node_Id",
         Image (System_Time_Node (N)),
         Int (System_Time_Node (N)));
      W_Node_Attribute
        ("Marshaller_Node",
         "Node_Id",
         Image (Marshaller_Node (N)),
         Int (Marshaller_Node (N)));
      W_Node_Attribute
        ("Unmarshaller_Node",
         "Node_Id",
         Image (Unmarshaller_Node (N)),
         Int (Unmarshaller_Node (N)));
      W_Node_Attribute
        ("Activity_Node",
         "Node_Id",
         Image (Activity_Node (N)),
         Int (Activity_Node (N)));
      W_Node_Attribute
        ("Functions_Node",
         "Node_Id",
         Image (Functions_Node (N)),
         Int (Functions_Node (N)));
      W_Node_Attribute
        ("Types_Node",
         "Node_Id",
         Image (Types_Node (N)),
         Int (Types_Node (N)));
      W_Node_Attribute
        ("Object_Node",
         "Node_Id",
         Image (Object_Node (N)),
         Int (Object_Node (N)));
      W_Node_Attribute
        ("Handlers_Node",
         "Node_Id",
         Image (Handlers_Node (N)),
         Int (Handlers_Node (N)));
      W_Node_Attribute
        ("Global_Port_Node",
         "Node_Id",
         Image (Global_Port_Node (N)),
         Int (Global_Port_Node (N)));
      W_Node_Attribute
        ("Local_Port_Node",
         "Node_Id",
         Image (Local_Port_Node (N)),
         Int (Local_Port_Node (N)));
      W_Node_Attribute
        ("Global_Names_Node",
         "Node_Id",
         Image (Global_Names_Node (N)),
         Int (Global_Names_Node (N)));
      W_Node_Attribute
        ("Global_Model_Names_Node",
         "Node_Id",
         Image (Global_Model_Names_Node (N)),
         Int (Global_Model_Names_Node (N)));
      W_Node_Attribute
        ("Request_Node",
         "Node_Id",
         Image (Request_Node (N)),
         Int (Request_Node (N)));
      W_Node_Attribute
        ("Deployment_Node",
         "Node_Id",
         Image (Deployment_Node (N)),
         Int (Deployment_Node (N)));
      W_Node_Attribute
        ("Entities_Node",
         "Node_Id",
         Image (Entities_Node (N)),
         Int (Entities_Node (N)));
      W_Node_Attribute
        ("Servers_Node",
         "Node_Id",
         Image (Servers_Node (N)),
         Int (Servers_Node (N)));
      W_Node_Attribute
        ("Naming_Node",
         "Node_Id",
         Image (Naming_Node (N)),
         Int (Naming_Node (N)));
      W_Node_Attribute
        ("Processes",
         "List_Id",
         Image (Processes (N)),
         Int (Processes (N)));
      W_Node_Attribute
        ("Request_Type_Node",
         "Node_Id",
         Image (Request_Type_Node (N)),
         Int (Request_Type_Node (N)));
      W_Node_Attribute
        ("Job_Node",
         "Node_Id",
         Image (Job_Node (N)),
         Int (Job_Node (N)));
      W_Node_Attribute
        ("Stub_Node",
         "Node_Id",
         Image (Stub_Node (N)),
         Int (Stub_Node (N)));
      W_Node_Attribute
        ("Enumerator_Node",
         "Node_Id",
         Image (Enumerator_Node (N)),
         Int (Enumerator_Node (N)));
      W_Node_Attribute
        ("Process_Request_Node",
         "Node_Id",
         Image (Process_Request_Node (N)),
         Int (Process_Request_Node (N)));
      W_Node_Attribute
        ("Marshall_Request_Node",
         "Node_Id",
         Image (Marshall_Request_Node (N)),
         Int (Marshall_Request_Node (N)));
      W_Node_Attribute
        ("Unmarshall_Request_Node",
         "Node_Id",
         Image (Unmarshall_Request_Node (N)),
         Int (Unmarshall_Request_Node (N)));
      W_Node_Attribute
        ("Default_Value_Node",
         "Node_Id",
         Image (Default_Value_Node (N)),
         Int (Default_Value_Node (N)));
   end W_HI_Tree_Bindings;

end Ocarina.Backends.C_Tree.Nodes;
