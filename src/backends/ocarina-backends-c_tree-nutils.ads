------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . C _ T R E E . N U T I L S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
--                                                                          --
-- Ocarina  is free software; you can redistribute it and/or modify under   --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. Ocarina is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY; without even the implied warranty of               --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.Backends.C_Tree.Nodes; use Ocarina.Backends.C_Tree.Nodes;

package Ocarina.Backends.C_Tree.Nutils is

   Int0_Val : Value_Id;
   Int1_Val : Value_Id;

   Var_Suffix  : constant String := "_j";
   Initialized : Boolean         := False;

   Output_Unit_Withing : Boolean := False;
   --  Control flags

   type Browsing_Kind is (By_Source, By_Destination, Default);

   type Token_Type is
     (
      --   Token name      Token type
      --   Keywords
      Tok_Null,            -- NULL   **** First Keyword
      Tok_Break,           -- BREAK
      Tok_Case,            -- CASE
      Tok_Const,           -- CONST
      Tok_Define,          -- DEFINE
      Tok_Default,         -- DEFAULT
      Tok_Endif,           -- ENDIF
      Tok_Else,            -- ELSE
      Tok_Enum,            -- ENUM
      Tok_Extern,          -- EXTERN
      Tok_Struct,          -- STRUCT
      Tok_Union,           -- UNION
      Tok_Exit,            -- EXIT
      Tok_Goto,            -- GOTO
      Tok_If,              -- IF
      Tok_Ifdef,           -- IFDEF
      Tok_Ifndef,          -- IFNDEF
      Tok_Include,         -- INCLUDE
      Tok_Return,          -- RETURN
      Tok_Until,           -- UNTIL
      Tok_For,             -- FOR
      Tok_While,           -- WHILE
      Tok_Switch,          -- SWITCH
      Tok_Typedef,         -- TYPEDEF

   --  Graphic Characters
      Tok_Xor,             -- ^
      Tok_Sharp,           -- #
      Tok_Mod,             -- %
      Tok_Not,             -- !
      Tok_Left_Brace,      -- {
      Tok_Right_Brace,     -- }
      Tok_Or,              -- ||
      Tok_And,             -- &&
      Tok_Ampersand,       -- &
      Tok_Minus,           -- -
      Tok_Underscore,      -- _
      Tok_Plus,            -- +
      Tok_Asterisk,        -- *
      Tok_Slash,           -- /
      Tok_Quote,           -- "
      Tok_Dot,             -- .
      Tok_Apostrophe,      -- '
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Left_Hook,       -- [
      Tok_Right_Hook,      -- ]
      Tok_Comma,           -- ,
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Equal_Equal,     -- ==
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Less_Less,       -- <<
      Tok_Semicolon,       -- ;
      Tok_Arrow,           -- ->
      Tok_Vertical_Bar);   -- |

   Token_Image : array (Token_Type) of Name_Id;

   subtype Keyword_Type is Token_Type range Tok_Null .. Tok_Typedef;

   type Operator_Type is
     (Op_Not,             -- not
      Op_And,             -- and
      Op_In,              -- in
      Op_And_Then,        -- and then
      Op_Or,              -- or
      Op_Or_Else,         -- or else
      Op_And_Symbol,      -- &
      Op_Double_Asterisk, -- **
      Op_Minus,           -- -
      Op_Plus,            -- +
      Op_Asterisk,        -- *
      Op_Slash,           -- /
      Op_Less,            -- <
      Op_Equal,           -- =
      Op_Equal_Equal,     -- ==
      Op_Greater,         -- >
      Op_Not_Equal,       -- !=
      Op_Greater_Equal,   -- >=
      Op_Less_Equal,      -- <=
      Op_Greater_Greater, -- >>
      Op_Less_Less,       -- <<
      Op_Semicolon,       -- ;
      Op_Arrow,           -- ->
      Op_Vertical_Bar,    -- |
      Op_None);           -- No operation

   Operator_Image : array
   (Operator_Type'Pos (Op_And) ..
        Operator_Type'Pos (Op_Vertical_Bar)) of Name_Id;

   subtype Keyword_Operator is
     Operator_Type range Operator_Type'First .. Op_Or_Else;

   type Parameter_Id is
     (P_From,
      P_To,
      P_Unused,
      P_Message,
      P_Msg,
      P_Request,
      P_Buffer,
      P_Status,
      P_Entity,
      P_Task,
      P_Partition,
      P_Pkt,
      P_Port,
      P_Error,
      P_Offset,
      P_Self,
      P_Value);

   PN : array (Parameter_Id) of Name_Id;

   type Member_Id is
     (M_Operation,
      M_Protected_Id,
      M_Failed_Process_Id,
      M_Error_Code,
      M_Port,
      M_Entry,
      M_Entry_Point,
      M_Kind,
      M_Msg,
      M_Name,
      M_Base_Priority,
      M_Priority,
      M_Failed_Thread,
      M_Error_Kind,
      M_Time_Capacity,
      M_Stack_Size,
      M_Deadline,
      M_Period,
      M_Flags,
      M_Vars);

   MN : array (Member_Id) of Name_Id;

   type Constant_Id is (C_Null);

   CONST : array (Constant_Id) of Name_Id;

   type Variable_Id is
     (V_Request,
      V_Ret,
      V_Tattr,
      V_Lua_Context,
      V_Next_Period,
      V_Port_Global_To_Entity,
      V_Port_Global_To_Local_Port,
      V_Server_Entity_Table,
      V_Invalid_Server,
      V_Got_Data,
      V_Dev_Id,
      V_Entity,
      V_Period,
      V_Offset,
      V_Port,
      V_Pkt,
      V_Thread,
      V_Error,
      V_Error_Status,
      V_Out,
      V_In,
      V_Message);

   VN : array (Variable_Id) of Name_Id;

   type Function_Id is
     (F_Process_Request, F_Register_Source, F_Init_Lane, F_Sizeof, F_Create);

   FN : array (Function_Id) of Name_Id;

   type Component_Id is
     (C_Address,
      C_Dispatcher,
      C_From,
      C_Los,
      C_Name,
      C_Pid,
      C_Proc_Id,
      C_Switch,
      C_Conf_Table,
      C_Priority,
      C_Operation);

   CN : array (Component_Id) of Name_Id;

   type Attribute_Id is
     (A_Access,
      A_Class,
      A_First,
      A_Pos,
      A_Range,
      A_Val,
      A_Identity,
      A_Adress,
      A_Last);

   AN : array (Attribute_Id) of Name_Id;

   type Type_Id is
     (T_Char,
      T_Float,
      T_Int,
      T_Uint8_T,
      T_Uint32_T,
      T_Int8_T,
      T_Int16_T,
      T_Int32_T,
      T_Int64_T,
      T_Void,
      T_Unsigned);

   TN : array (Type_Id) of Name_Id;

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id;

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;
   --  This function returns a new name without the suffix. If the
   --  suffix does not exist, the returned name is equal to the given
   --  name.

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   procedure Insert_After_Node (E : Node_Id; N : Node_Id);
   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);

   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;
   function Current_Entity return Node_Id;
   function Current_File return Node_Id;

   function Copy_Node (N : Node_Id) return Node_Id;

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return Node_Id;

   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return List_Id;

   function Image (T : Token_Type) return String;
   function Image (O : Operator_Type) return String;

   procedure Initialize;
   procedure Reset;

   procedure New_Token (T : Token_Type; I : String := "");

   function Length (L : List_Id) return Natural;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return True when L is empty

   function Make_C_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True) return Node_Id;
   --  This function does only the fllowing thing: it creates a node
   --  whose name is the full text of the comment. It does not split
   --  the comment into many lines. This is done in the code
   --  generation phase

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id) return Node_Id;

   function Make_Defining_Identifier
     (Name           : Name_Id;
      C_Conversion   : Boolean := True;
      Ada_Conversion : Boolean := False;
      Pointer        : Boolean := False) return Node_Id;

   function Make_Expression
     (Left_Expr  : Node_Id;
      Operator   : Operator_Type := Op_None;
      Right_Expr : Node_Id       := No_Node) return Node_Id;

   function Make_For_Statement
     (Defining_Identifier : Node_Id;
      Pre_Cond            : Node_Id;
      Condition           : Node_Id;
      Post_Cond           : Node_Id;
      Statements          : List_Id) return Node_Id;

   function Make_Variable_Declaration
     (Defining_Identifier : Node_Id;
      Used_Type           : Node_Id) return Node_Id;

   function Make_Member_Declaration
     (Defining_Identifier : Node_Id;
      Used_Type           : Node_Id) return Node_Id;

   function Make_Enum_Aggregate (Members : List_Id) return Node_Id;

   function Make_Struct_Aggregate
     (Defining_Identifier : Node_Id := No_Node;
      Members             : List_Id) return Node_Id;

   function Make_Union_Aggregate
     (Defining_Identifier : Node_Id := No_Node;
      Members             : List_Id) return Node_Id;

   function Make_While_Statement
     (Condition  : Node_Id;
      Statements : List_Id) return Node_Id;

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id) return Node_Id;
   --  No_Node as Type_Definition made type declaration without actual
   --  definition (eg. "type X;").

   function Make_If_Statement
     (Condition       : Node_Id;
      Statements      : List_Id;
      Else_Statements : List_Id := No_List) return Node_Id;

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node) return List_Id;

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Type      : Node_Id := No_Node) return Node_Id;

   function Make_Return_Statement
     (Expression : Node_Id := No_Node) return Node_Id;

   function Make_Call_Profile
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List) return Node_Id;

   function Make_Function_Implementation
     (Specification : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id) return Node_Id;

   function Make_Function_Specification
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List;
      Return_Type         : Node_Id := No_Node) return Node_Id;

   function Make_Type_Attribute
     (Designator : Node_Id;
      Attribute  : Attribute_Id) return Node_Id;

   function Make_Type_Conversion
     (Subtype_Mark : Node_Id;
      Expression   : Node_Id) return Node_Id;

   procedure Make_Comment_Header (Header : List_Id);
   --  This procedure generates a comment header for the generated
   --  packages.

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id;
   --  This function executes Next_Node Num times

   function Message_Comment (M : Name_Id) return Node_Id;
   function Message_Comment (M : String) return Node_Id;
   --  Return a comment message. Used by all the tree
   --  converters

   procedure Set_Activity_Source (N : Node_Id := No_Node);

   procedure Set_Activity_Header (N : Node_Id := No_Node);

   procedure Set_Main_Source (N : Node_Id := No_Node);

   procedure Set_Main_Header (N : Node_Id := No_Node);

   procedure Set_Request_Source (N : Node_Id := No_Node);

   procedure Set_Request_Header (N : Node_Id := No_Node);

   function To_C_Name
     (N             : Name_Id;
      Ada_Style     : Boolean := False;
      Keyword_Check : Boolean := True) return Name_Id;
   --  Convert N to a valid Ada identifier (no clashing with keywords,
   --  no consecutive '_', no heading '_'...).
   --  If Ada_Style is true, '.' is replaced by "__"

   function Conventional_Base_Name (N : Name_Id) return Name_Id;
   --  Return a lower case name of N

   function Make_Source_File (Identifier : Node_Id) return Node_Id;

   function Make_Header_File (Identifier : Node_Id) return Node_Id;

   procedure Set_Deployment_Header (N : Node_Id := No_Node);

   function Make_Literal (Value : Value_Id) return Node_Id;

   function Make_Define_Statement
     (Defining_Identifier : Node_Id;
      Value               : Node_Id) return Node_Id;

   function Make_Pointer_Type (Used_Type : Node_Id) return Node_Id;

   procedure Add_Include (E : Node_Id; Preserve_Case : Boolean := False);

   procedure Set_Types_Header (N : Node_Id := No_Node);

   procedure Set_Types_Source (N : Node_Id := No_Node);

   procedure Set_Naming_Source (N : Node_Id := No_Node);

   procedure Set_Subprograms_Source (N : Node_Id := No_Node);

   procedure Set_Subprograms_Header (N : Node_Id := No_Node);

   procedure Set_Marshallers_Source (N : Node_Id := No_Node);

   procedure Set_Marshallers_Header (N : Node_Id := No_Node);

   function Make_Variable_Address (Expression : Node_Id) return Node_Id;

   function Make_Member_Designator
     (Defining_Identifier : Node_Id;
      Aggregate_Name      : Node_Id;
      Is_Pointer          : Boolean := False) return Node_Id;

   function Make_Switch_Alternative
     (Labels     : List_Id;
      Statements : List_Id) return Node_Id;

   function Make_Switch_Statement
     (Expression   : Node_Id;
      Alternatives : List_Id) return Node_Id;

   function Make_Macro_Call
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List) return Node_Id;

   procedure POK_Add_Return_Assertion
     (Statements      : List_Id;
      Exception_Error : Node_Id := No_Node);

   procedure Handle_Call_Sequence
     (Caller            : Node_Id;
      Call_Seq          : Node_Id;
      Declarations      : List_Id;
      Statements        : List_Id;
      Containing_Device : Node_Id := No_Node);
   --  The Containing_Device argument is used when the call
   --  sequence is generated for a thread that is within
   --  a device. In that case, the first parameter is ALWAYS
   --  the device id of the containing device.

   procedure Set_Deployment_Source (N : Node_Id := No_Node);

   function Make_Array_Declaration
     (Defining_Identifier : Node_Id;
      Array_Size          : Node_Id) return Node_Id;

   function Make_Array_Values (Values : List_Id := No_List) return Node_Id;

   function Make_Extern_Entity_Declaration (Entity : Node_Id) return Node_Id;

   function Make_Constant_Type (Used_Type : Node_Id) return Node_Id;

   procedure Set_Naming_Header (N : Node_Id := No_Node);

   function Get_C_Default_Value (D : Node_Id) return Node_Id;

   function POK_Make_Function_Call_With_Assert
     (Function_Name : Node_Id;
      Parameters    : List_Id) return Node_Id;

   function Make_Include_Clause
     (Header_Name : Node_Id;
      Local       : Boolean := False) return Node_Id;

   function Make_Ifdef_Clause
     (Clause          : Node_Id;
      Negation        : Boolean := False;
      Then_Statements : List_Id;
      Else_Statements : List_Id) return Node_Id;

   procedure Add_Define_Deployment (E : Node_Id);

   function Make_Array_Value
     (Array_Name : Node_Id;
      Array_Item : Node_Id) return Node_Id;

   function Get_Data_Size (Data : Node_Id) return Node_Id;
   --  Returns a node that represent an expression with the size
   --  (in bytes) of a data.

   procedure Add_Return_Variable_In_Parameters (Parameters : List_Id);
   --  Add the name of the return variable in the list.
   --  The primary purpose of this function is to add
   --  the return variable at the end of a function call
   --  for the ARINC653 API.

   procedure POK_Declare_Return_Variable (Declarations : List_Id);
   --  Declare the return variable used for assertions
   --  in function declarations or other list.

   function Get_Inter_Partition_Port_Size (Port : Node_Id) return Node_Id;
   --  Get_Inter_Partition_Port_Size returns the port size of an inter
   --  partition communication. It takes the whole data flow of a connection
   --  and look for virtual bus binding in this flow to see if we need
   --  larger data.

   function Get_Inter_Partition_Port_Type (Port : Node_Id) return Node_Id;
   --  Get_Inter_Partition_Port_Type returns a Node_Id that contains
   --  all information to generate a C data type linked with
   --  virtual bus layers.

   function Make_Doxygen_C_Comment
     (Desc              : String;
      Brief             : String  := "";
      Element_Name      : String  := "";
      Is_Struct         : Boolean := False;
      Is_Union          : Boolean := False;
      Is_Enum           : Boolean := False;
      Is_Function       : Boolean := False;
      Is_Variable       : Boolean := False;
      Is_Define         : Boolean := False;
      Is_Typedef        : Boolean := False;
      Is_File           : Boolean := False;
      Is_Namespace      : Boolean := False;
      Is_Package        : Boolean := False;
      Is_Interface      : Boolean := False;
      Has_Header_Spaces : Boolean := True) return Node_Id;

   --  The Make_Doxygen_C_Comment.

end Ocarina.Backends.C_Tree.Nutils;
