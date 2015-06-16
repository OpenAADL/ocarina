------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . R T S J _ T R E E . N U T I L S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.Backends.RTSJ_Tree.Nodes; use Ocarina.Backends.RTSJ_Tree.Nodes;

package Ocarina.Backends.RTSJ_Tree.Nutils is

   Int0_Val : Value_Id;
   Int1_Val : Value_Id;

   Var_Suffix  : constant String := "_j";
   Initialized : Boolean         := False;

   Output_Unit_Withing : Boolean := False;
   --  Control flags

   type Token_Type is
     (
   --   Token name      Token type
   --   Keywords
   Tok_Null,            -- NULL   **** First Keyword
      Tok_Enum,            -- ENUM
      Tok_Import,          -- IMPORT
      Tok_If,              -- IF
      Tok_Else,            -- ELSE
      Tok_Return,          -- RETURN
      Tok_For,             -- FOR
      Tok_While,           -- WHILE
      Tok_Switch,          -- SWITCH
      Tok_Case,            -- CASE
      Tok_Default,         -- DEFAULT
      Tok_Break,           -- BREAK
      Tok_Class,           -- CLASS
      Tok_Interface,       -- INTERFACE
      Tok_Extends,         -- EXTENDS
      Tok_Implements,      -- IMPLEMENTS
      Tok_New,             -- NEW
      Tok_Throws,          -- THROWS
      Tok_Throw,           -- THROW
      Tok_Try,             -- TRY
      Tok_Catch,           -- CATCH
      Tok_Finally,         -- FINALLY
      Tok_Public,          -- PUBLIC
      Tok_Protected,       -- PROTECTED
      Tok_Private,         -- PRIVATE
      Tok_Abstract,        -- ABSTRACT
      Tok_Static,          -- STATIC
      Tok_Final,           -- FINAL
   --  Tok_Native,          -- NATIVE
   --  Tok_Synchronized,    -- SYNCHRONIZED
   --  Tok_Transient,       -- TRANSIENT
   --  Tok_Volatile,        -- VOLATILE
      Tok_True,            -- TRUE
      Tok_False,           -- FALSE
   --  Tok_Instanceof,      -- INSTANCEOF
      Tok_Package,         -- PACKAGE
      Tok_Super,           -- SUPER
      Tok_This,            -- THIS   **** Last Keyword

   --  Graphic Characters
      Tok_Xor,             -- ^
      Tok_Mod,             -- %
      Tok_Not,             -- !
      Tok_Left_Brace,      -- {
      Tok_Right_Brace,     -- }
      Tok_Or,              -- ||
      Tok_And,             -- &&
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
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Less_Less,       -- <<
      Tok_Semicolon,       -- ;
      Tok_Vertical_Bar);   -- |

   Token_Image : array (Token_Type) of Name_Id;

   subtype Keyword_Type is Token_Type range Tok_Null .. Tok_This;

   type Operator_Type is
     (Op_Not,             -- not
      Op_And,             -- and
      Op_Or,              -- or
      Op_Minus,           -- -
      Op_Plus,            -- +
      Op_Plus_Plus,       -- ++
      Op_Mult,            -- *
      Op_Slash,           -- /
      Op_Less,            -- <
      Op_Equal,           -- =
      Op_Equal_Equal,     -- ==
      Op_Greater,         -- >
      Op_Not_Equal,       -- /=
      Op_Greater_Equal,   -- >=
      Op_Less_Equal,      -- <=
      Op_Greater_Greater, -- >>
      Op_Less_Less,       -- <<
      Op_Semicolon,       -- ;
      Op_Vertical_Bar,    -- |
      Op_None);           -- No operation

   Operator_Image : array
   (Operator_Type'Pos (Op_Not) ..
        Operator_Type'Pos (Op_Vertical_Bar)) of Name_Id;

   subtype Keyword_Operator is
     Operator_Type range Operator_Type'First .. Op_Or;

   type Variable_Id is
     (V_Args,
      V_Initializer,
      V_Init_Logic,
      V_Value,
      V_New_Value,
      V_Msg,
      V_Tmp,
      V_Sender_Entity,
      V_Destination_Entity,
      V_Transport,
      V_Entity,
      V_In_Port,
      V_Length,
      V_I);
   VN : array (Variable_Id) of Name_Id;

   type Method_Id is
     (M_Main,
      M_Run,
      M_Start,
      M_Super,
      M_Instance,
      M_Execute_In_Area,
      M_Initialization,
      M_Store_Received_Message);
   MN : array (Method_Id) of Name_Id;

   type Object_Id is
     (O_Execution_Logic,
      O_Thread_Initializer,
      O_Runnable,
      O_No_Heap_Realtime_Thread,
      O_Utils,
      O_Immortal_Memory,
      O_Activity,
      O_Suspenders,
      O_Context,
      O_Deployment,
      O_Subprograms,
      O_Program_Exception,
      O_Periodic_Task,
      O_Sporadic_Task,
      O_Aperiodic_Task,
      O_Hybrid_Task,
      O_Task_Handler,
      O_Event_Handler,
      O_Time_Unit,
      O_Generated_Types,
      O_Generated_Type,
      O_Message,
      O_Transport_High_Level_Impl,
      O_Transport_High_Level,
      O_Marshallers,
      O_Naming,
      O_Port,
      O_In_Port,
      O_Out_Port,
      O_Ports_Router,
      O_Entry);
   ON : array (Object_Id) of Name_Id;

   --  Functions and procedures

   procedure Initialize;
   procedure Reset;

   function Is_Empty (L : List_Id) return Boolean;
   function Length (L : List_Id) return Natural;

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;
   procedure Add_Import (E : Node_Id);
   function To_RTSJ_Conventional_Name
     (Name   : Name_Id;
      Is_Obj : Boolean) return Name_Id;
   function Conventional_Base_Name (M : Name_Id) return Name_Id;
   function Image (T : Token_Type) return String;
   procedure New_Token (T : Token_Type; I : String := "");
   function Image (O : Operator_Type) return String;
   procedure New_Operator (O : Operator_Type; I : String := "");
   function Current_Entity return Node_Id;
   function Current_File return Node_Id;
   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return Node_Id;
   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return List_Id;

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node) return List_Id;

   --  Add a new node to the list
   procedure Append_Node_To_List (E : Node_Id; L : List_Id);

   --  Delete a node from the list
   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);

   function Copy_Node (N : Node_Id) return Node_Id;
   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;

   procedure Set_Naming_Source (N : Node_Id := No_Node);
   procedure Set_Main_Source (N : Node_Id := No_Node);
   procedure Set_Deployment_Source (N : Node_Id := No_Node);
   procedure Set_Subprograms_Source (N : Node_Id := No_Node);
   procedure Set_Activity_Source (N : Node_Id := No_Node);
   procedure Set_Generated_Types_Source (N : Node_Id := No_Node);
   procedure Set_Transport_High_Level_Source (N : Node_Id := No_Node);

   function Message_Comment (M : String) return Node_Id;

   --  Convert N into a valid Java identifier (no clashing with keywords,
   --  no consecutive '_', no heading '_' ...)
   function To_RTSJ_Name (N : Name_Id) return Name_Id;

   function Make_Assignment_Statement
     (Defining_Identifier : Node_Id;
      Expression          : Node_Id) return Node_Id;

   function Make_Variable_Declaration
     (Visibility          : List_Id := No_List;
      Used_Type           : Node_Id;
      Defining_Identifier : Node_Id;
      Value               : Node_Id := No_Node) return Node_Id;

   function Make_Return_Statement (Expression : Node_Id) return Node_Id;

   function Make_Null_Statement return Node_Id;

   function Make_Try_Statement
     (Statements       : List_Id;
      Catch_Statements : List_Id) return Node_Id;

   function Make_Catch_Statement
     (Exception_Caught : Node_Id;
      Statements       : List_Id) return Node_Id;

   function Make_Package_Statement
     (Defining_Identifier : Node_Id) return Node_Id;

   function Make_Import_Statement (Import_Name : Node_Id) return Node_Id;

   function Make_Class_Statement
     (Visibility          : List_Id := No_List;
      Defining_Identifier : Node_Id;
      Extends             : Node_Id := No_Node;
      Implements          : List_Id := No_List;
      Throws              : List_Id := No_List;
      Attributes          : List_Id := No_List;
      Methods             : List_Id := No_List;
      Classes             : List_Id := No_List) return Node_Id;

   function Make_Java_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True) return Node_Id;

   function Make_New_Statement
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List;
      Is_Array            : Boolean := False) return Node_Id;

   function Make_Public_Statement return Node_Id;
   function Make_Protected_Statement return Node_Id;
   function Make_Private_Statement return Node_Id;
   function Make_Abstract_Statement return Node_Id;
   function Make_Static_Statement return Node_Id;
   function Make_Final_Statement return Node_Id;

   function Make_Enumerator (Enum_Members : List_Id) return Node_Id;

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id;

   function Make_Pointed_Notation
     (Left_Member  : Node_Id;
      Right_Member : Node_Id) return Node_Id;

   function Make_Expression
     (Left_Expression     : Node_Id;
      Operator_Expression : Operator_Type := Op_None;
      Right_Expression    : Node_Id       := No_Node) return Node_Id;

   function Make_Function_Specification
     (Visibility          : List_Id;
      Return_Type         : Node_Id := No_Node;
      Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List;
      Throws              : List_Id := No_List) return Node_Id;

   function Make_Function_Implementation
     (Specification : Node_Id;
      Declarations  : List_Id := No_List;
      Statements    : List_Id := No_List) return Node_Id;

   function Make_Call_Function
     (Defining_Identifier : Node_Id;
      Parameters          : List_Id := No_List) return Node_Id;

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Parameter_Type      : Node_Id) return Node_Id;

   function Make_Array_Declaration
     (Defining_Identifier : Node_Id;
      Array_Size          : Node_Id := No_Node) return Node_Id;

   function Make_Array_Value
     (Defining_Identifier : Node_Id;
      Array_Item          : Node_Id := No_Node) return Node_Id;

   function Make_Full_Array_Declaration
     (Array_Declaration : Node_Id;
      Array_Assignments : List_Id) return Node_Id;

   function Make_Cast_Statement
     (Cast_Type           : Node_Id;
      Defining_Identifier : Node_Id) return Node_Id;

   function Make_Switch_Statement
     (Expr            : Node_Id;
      Case_Statements : List_Id) return Node_Id;

   function Make_Case_Statement
     (Labels     : List_Id;
      Statements : List_Id) return Node_Id;

   function Make_Throw_Statement
     (Defining_Identifier : Node_Id) return Node_Id;

   function Make_For_Statement
     (Init_Statement      : Node_Id := No_Node;
      Iteration_Condition : Node_Id := No_Node;
      Step_Expression     : Node_Id := No_Node;
      Statements          : List_Id) return Node_Id;

   function Make_Literal (Value_Literal : Value_Id) return Node_Id;

   function Make_Source_File (Identifier : Node_Id) return Node_Id;

   procedure Make_Comment_Header (Header : List_Id);

end Ocarina.Backends.RTSJ_Tree.Nutils;
