------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . A D A _ T R E E . N U T I L S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2019 ESA & ISAE.      --
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

with Ocarina.Backends.Ada_Tree.Nodes; use Ocarina.Backends.Ada_Tree.Nodes;

package Ocarina.Backends.Ada_Tree.Nutils is

   Int0_Val : Value_Id;
   Int1_Val : Value_Id;

   Output_Tree_Warnings : Boolean := False;
   Output_Unit_Withing  : Boolean := False;
   --  Control flags

   type Token_Type is
     (
      --   Token name      Token type
      --   Keywords
      Tok_Mod,             -- MOD   **** First Keyword
      Tok_Rem,             -- REM
      Tok_New,             -- NEW
      Tok_Abs,             -- ABS
      Tok_Others,          -- OTHERS
      Tok_Null,            -- NULL
      Tok_Delta,           -- DELTA
      Tok_Digits,          -- DIGITS
      Tok_Range,           -- RANGE
      Tok_And,             -- AND
      Tok_Or,              -- OR
      Tok_Xor,             -- XOR
      Tok_In,              -- IN
      Tok_Not,             -- NOT
      Tok_Abstract,        -- ABSTRACT
      Tok_Access,          -- ACCESS
      Tok_Aliased,         -- ALIASED
      Tok_All,             -- ALL
      Tok_Array,           -- ARRAY
      Tok_At,              -- AT
      Tok_Body,            -- BODY
      Tok_Constant,        -- CONSTANT
      Tok_Do,              -- DO
      Tok_Is,              -- IS
      Tok_Limited,         -- LIMITED
      Tok_Of,              -- OF
      Tok_Out,             -- OUT
      Tok_Record,          -- RECORD
      Tok_Renames,         -- RENAMES
      Tok_Reverse,         -- REVERSE
      Tok_Tagged,          -- TAGGED
      Tok_Then,            -- THEN
      Tok_Abort,           -- ABORT
      Tok_Accept,          -- ACCEPT
      Tok_Case,            -- CASE
      Tok_Delay,           -- DELAY
      Tok_Else,            -- ELSE
      Tok_Elsif,           -- ELSIF
      Tok_End,             -- END
      Tok_Exception,       -- EXCEPTION
      Tok_Exit,            -- EXIT
      Tok_Goto,            -- GOTO
      Tok_If,              -- IF
      Tok_Pragma,          -- PRAGMA
      Tok_Raise,           -- RAISE
      Tok_Requeue,         -- REQUEUE
      Tok_Return,          -- RETURN
      Tok_Select,          -- SELECT
      Tok_Terminate,       -- TERMINATE
      Tok_Until,           -- UNTIL
      Tok_When,            -- WHEN

      Tok_Begin,           -- BEGIN
      Tok_Declare,         -- DECLARE
      Tok_For,             -- FOR
      Tok_Loop,            -- LOOP
      Tok_While,           -- WHILE

      Tok_Entry,           -- ENTRY
      Tok_Protected,       -- PROTECTED
      Tok_Task,            -- TASK
      Tok_Type,            -- TYPE
      Tok_Subtype,         -- SUBTYPE
      Tok_Use,             -- USE

      Tok_Function,        -- FUNCTION
      Tok_Generic,         -- GENERIC
      Tok_Package,         -- PACKAGE
      Tok_Procedure,       -- PROCEDURE

      Tok_Private,         -- PRIVATE
      Tok_With,            -- WITH
      Tok_Separate,        -- SEPARATE **** Last Keyword

      --  Graphic Characters
      Tok_Double_Asterisk, -- **
      Tok_Ampersand,       -- &
      Tok_Minus,           -- -
      Tok_Plus,            -- +
      Tok_Asterisk,        -- *
      Tok_Slash,           -- /
      Tok_Dot,             -- .
      Tok_Apostrophe,      -- '
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Comma,           -- ,
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_Box,             -- <>
      Tok_Colon_Equal,     -- :=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Less_Less,       -- <<
      Tok_Semicolon,       -- ;
      Tok_Arrow,           -- =>
      Tok_Vertical_Bar,    -- |
      Tok_Dot_Dot,         -- ..
      Tok_Minus_Minus      -- --
     );

   Token_Image : array (Token_Type) of Name_Id;

   subtype Keyword_Type is Token_Type range Tok_Mod .. Tok_Separate;

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
      Op_Greater,         -- >
      Op_Not_Equal,       -- /=
      Op_Greater_Equal,   -- >=
      Op_Less_Equal,      -- <=
      Op_Box,             -- <>
      Op_Colon_Equal,     -- :=
      Op_Colon,           -- :
      Op_Greater_Greater, -- >>
      Op_Less_Less,       -- <<
      Op_Semicolon,       -- ;
      Op_Arrow,           -- =>
      Op_Vertical_Bar,    -- |
      Op_None);           -- No operation

   Operator_Image : array
   (Operator_Type'Pos (Op_And) ..
        Operator_Type'Pos (Op_Vertical_Bar)) of Name_Id;

   subtype Keyword_Operator is
     Operator_Type range Operator_Type'First .. Op_Or_Else;

   type Parameter_Id is
     (P_A,
      P_Activate_Entrypoint,
      P_Arg_List,
      P_Argument,
      P_C,
      P_Conflicts,
      P_Current_Entity,
      P_Data,
      P_Depends,
      P_Destinations,
      P_Dispatcher,
      P_Dispatch_Offset,
      P_E_Req,
      P_Elaboration_Check,
      P_Elaborated_Variables,
      P_Entity,
      P_Entity_Table,
      P_Entity_Image,
      P_Error,
      P_From,
      P_Global_Data_Queue_Size,
      P_Got_Data,
      P_Has_Event_Ports,
      P_Id,
      P_Implicit,
      P_Incoming_Message,
      P_Init,
      P_Initialize_Entrypoint,
      P_Index,
      P_Interrupt_Identifier,
      P_Item,
      P_Job,
      P_Key,
      P_Lane_R,
      P_Max_Node_Image_Size,
      P_Max_Entity_Image_Size,
      P_Max_Payload_Size,
      P_Max_Port_Image_Size,
      P_May_Exit,
      P_Message,
      P_Mode,
      P_Msg,
      P_My_Node,
      P_N_Destinations,
      P_Name,
      P_Naming_Table,
      P_Next_Start,
      P_Node,
      P_Node_Image,
      P_Null_Bounded_String,
      P_Null_Bounded_Wide_String,
      P_Obj,
      P_Operation,
      P_Period,
      P_Port,
      P_Port_Image,
      P_Port_Sized_String,
      P_Port_Table,
      P_PortName,
      P_Provides,
      P_Priority,
      P_Priority_Manager,
      P_Recover_Entrypoint,
      P_Ref,
      P_Req,
      P_Result,
      P_Self,
      P_Server_Entity_Table,
      P_Spg_Interface,
      P_Task_Deadline,
      P_Task_Period,
      P_Task_Priority,
      P_Task_Stack_Size,
      P_Thread_Port_Kinds,
      P_Thread_Overflow_Protocols,
      P_Thread_Port_Images,
      P_Thread_Fifo_Sizes,
      P_Thread_Fifo_Offsets,
      P_Thread_Interface,
      P_The_Partition_Source,
      P_Time_Stamp,
      P_Hybrid_Task_Set,
      P_Hybrid_Task_Driver,
      P_To,
      P_Tp,
      P_Type_Code,
      P_Size,
      P_Store,
      P_Source,
      P_Section,
      P_Shutdown,
      P_Storage_Size,
      P_Stack_Size,
      P_Status,
      P_System_Start_Time,
      P_Urgencies,
      P_Valid,
      P_Value);

   PN : array (Parameter_Id) of Name_Id;

   type Variable_Id is
     (V_Argument,
      V_Id,
      V_Index,
      V_Invalid_Server,
      V_Mutex,
      V_Name,
      V_Period_Event,
      V_Present,
      V_Temp,
      V_Req,
      V_Args,
      V_Status,
      V_Result,
      V_Time_Stamp,
      V_Thread_Interface,
      V_Threads_Array,
      V_Threads_Access,
      V_Error);

   VN : array (Variable_Id) of Name_Id;

   type Subprogram_Id is
     (S_Build,
      S_Catch,
      S_Change_Mode,
      S_R_Continue, --  FIXME : bad, but where put it ?
      S_Deferred_Initialization,
      S_Deliver,
      S_Emit_Message,
      S_Execute_Servant,
      S_Found,
      S_From_Any,
      S_Get_Count,
      S_Get_Next_Event,
      S_Get_Time_Stamp,
      S_Get_Value,
      S_Get_Sender,
      S_Initialize,
      S_Length,
      S_Marshall,
      S_Next_Deadline,
      S_Next_Value,
      S_Receive_Input,
      S_Send_Output,
      S_Put_Value,
      S_Send,
      S_Store_Received_Message,
      S_To_Any,
      S_To_Bounded_String,
      S_To_Bounded_Wide_String,
      S_To_String,
      S_To_Wide_String,
      S_True,         --  FIXME : bad, but where put it ?
      S_Unmarshall,
      S_Wait_For_Incoming_Events,
      S_Controller,
      S_Get_Conf,
      S_Process_Request,
      S_Register_Source,
      S_Init_Lane,
      S_Create);

   SN : array (Subprogram_Id) of Name_Id;

   type Component_Id is
     (C_Address,
      C_From,
      C_Los,
      C_Name,
      C_Pid,
      C_Port,
      C_Proc_Id,
      C_Switch,
      C_Conf_Table,
      C_Operation);

   CN : array (Component_Id) of Name_Id;

   type Attribute_Id is
     (A_Access,
      A_Address,
      A_Alignment,
      A_Class,
      A_First,
      A_Length,
      A_Max,
      A_Pos,
      A_Range,
      A_Size,
      A_Val,
      A_Identity,
      A_Last);

   AN : array (Attribute_Id) of Name_Id;

   type Type_Id is
     (T_Bounded_String,
      T_Bounded_Wide_String,
      T_Entity_Type,
      T_Address_Array,
      T_Integer,
      T_Integer_Array,
      T_Node_Type,
      T_Object,
      T_Operations,
      T_Overflow_Protocol_Array,
      T_Port_Kind_Array,
      T_Port_Image_Array,
      T_Ref,
      T_Request,
      T_Server_Entity_Type,
      T_Table,
      T_Thread_Interface_Type,
      T_Partition_Source,
      T_Parameter_Entry,
      T_Port_Type);

   TN : array (Type_Id) of Name_Id;

   type Pragma_Id is
     (Pragma_Debug,
      Pragma_Elaborate_Body,
      Pragma_Import,
      Pragma_Export,
      Pragma_Inline,
      Pragma_No_Return,
      Pragma_Preelaborate,
      Pragma_Priority,
      Pragma_SPARK_Mode,
      Pragma_Style_Checks,
      Pragma_Suppress,
      Pragma_Unreferenced,
      Pragma_Warnings);

   GN : array (Pragma_Id) of Name_Id;

   type Error_Id is (E_Program_Error, E_Constraint_Error, E_NYI);

   EN : array (Error_Id) of Name_Id;

   type Aspect_Id is
     (A_Abstract_State,
      A_Global,
      A_Initializes,
      A_Pre,
      A_Refined_Global,
      A_Refined_State,
      A_Volatile_Function
     );

   ASN : array (Aspect_Id) of Name_Id;

   procedure Add_With_Package
     (E            : Node_Id;
      Used         : Boolean := False;
      Warnings_Off : Boolean := False;
      Elaborated   : Boolean := False);

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   procedure Append_Node_To_Current_Package (N : Node_Id);
   --  Append Node to the current package statements of package
   --  implementation or to the visible part of package specification

   procedure Insert_After_Node (E : Node_Id; N : Node_Id);
   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);

   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;
   function Current_Entity return Node_Id;
   function Current_Package return Node_Id;

   function Copy_Node (N : Node_Id) return Node_Id;

   function Create_Subtype_From_Range_Constraint (R : Node_Id) return Node_Id;
   --  This function takes a range_constraint, creates a node for
   --  the anonymous type of the range constraint and returns it.
   --  It's called only by Remove_Anonymous_Array_Type_Definition

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

   function Copy_Designator
     (Designator : Node_Id;
      Withed     : Boolean := True) return Node_Id;

   function Defining_Identifier_To_Designator
     (N                       : Node_Id;
      Copy                    : Boolean := False;
      Keep_Parent             : Boolean := True;
      Keep_Corresponding_Node : Boolean := True) return Node_Id;

   function Make_Access_Type_Definition
     (Subtype_Indication : Node_Id;
      Is_All             : Boolean := False;
      Is_Constant        : Boolean := False;
      Is_Not_Null        : Boolean := False) return Node_Id;

   function Make_Ada_Comment
     (N                 : Name_Id;
      Has_Header_Spaces : Boolean := True) return Node_Id;
   --  This function does only the fllowing thing: it creates a node
   --  whose name is the full text of the comment. It does not split
   --  the comment into many lines. This is done in the code
   --  generation phase

   function Make_Array_Aggregate (Elements : List_Id) return Node_Id;

   function Make_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id;
      Aliased_Present      : Boolean := False) return Node_Id;
   --  Usually used with Make_Full_Type_Declaration

   function Make_Aspect_Specification
     (Aspects : List_Id) return Node_Id;

   function Make_Aspect
     (Aspect_Mark : Name_Id;
      Aspect_Definition : Node_Id := No_Node) return Node_Id;

   function Make_Pre
     (Subprogram_Call : Node_Id) return Node_Id;

   function Make_Global_Specification
     (Moded_Global_List : List_Id) return Node_Id;

   function Make_Moded_Global_List
     (Mode : Mode_Id; Identifier : Node_Id) return Node_Id;

   function Make_Initialization_Spec
     (Initialization_List : List_Id) return Node_Id;

   function Make_Abstract_State_List
     (State_Name_With_Option : List_Id) return Node_Id;

   function Make_State_Name_With_Option
     (Defining_Identifier : Node_Id;
      Synchronous : Boolean;
      External : Boolean) return Node_Id;

   function Make_Refinement_List
     (Refinement_Clause : List_Id) return Node_Id;

   function Make_Refinement_Clause
     (State_Name : Node_Id;
      Constituent : List_Id) return Node_Id;

   function Make_Assignment_Statement
     (Variable_Identifier : Node_Id;
      Expression          : Node_Id) return Node_Id;

   function Make_Attribute_Definition_Clause
     (Defining_Identifier  : Node_Id;
      Attribute_Designator : Attribute_Id;
      Expression           : Node_Id) return Node_Id;

   function Make_Attribute_Designator
     (Prefix    : Node_Id;
      Attribute : Attribute_Id) return Node_Id;

   function Make_Block_Statement
     (Statement_Identifier : Node_Id := No_Node;
      Declarative_Part     : List_Id;
      Statements           : List_Id;
      Exception_Handler    : List_Id := No_List) return Node_Id;

   function Make_Case_Label (Value : Value_Id) return Node_Id;

   function Make_Case_Statement
     (Expression                  : Node_Id;
      Case_Statement_Alternatives : List_Id) return Node_Id;

   function Make_Case_Statement_Alternative
     (Discret_Choice_List : List_Id;
      Statements          : List_Id) return Node_Id;

   function Make_Component_Association
     (Selector_Name : Node_Id;
      Expression    : Node_Id) return Node_Id;

   function Make_Component_Declaration
     (Defining_Identifier : Node_Id;
      Subtype_Indication  : Node_Id;
      Expression          : Node_Id := No_Node;
      Aliased_Present     : Boolean := False) return Node_Id;

   function Make_Decimal_Type_Definition
     (D_Digits : Unsigned_Long_Long;
      D_Scale  : Unsigned_Long_Long) return Node_Id;

   function Make_Defining_Identifier
     (Name : Name_Id; Normalize : Boolean := True) return Node_Id;

   function Make_Delay_Statement
     (Expression : Node_Id;
      Is_Until   : Boolean := False) return Node_Id;

   function Make_Derived_Type_Definition
     (Subtype_Indication    : Node_Id;
      Record_Extension_Part : Node_Id := No_Node;
      Is_Abstract_Type      : Boolean := False;
      Is_Private_Extention  : Boolean := False;
      Is_Subtype            : Boolean := False) return Node_Id;

   function Make_Designator
     (Designator : Name_Id;
      Parent     : Name_Id := No_Name;
      Is_All     : Boolean := False) return Node_Id;

   function Make_Elsif_Statement
     (Condition       : Node_Id;
      Then_Statements : List_Id) return Node_Id;

   function Make_Element_Association
     (Index      : Node_Id;
      Expression : Node_Id) return Node_Id;
   --  If 'Index' is No_Node, then 'others => <Expression>' will be
   --  generated

   function Make_Enumeration_Type_Definition
     (Enumeration_Literals : List_Id) return Node_Id;

   function Make_Enumeration_Representation_Clause
     (Defining_Identifier : Node_Id;
      Array_Aggregate     : Node_Id) return Node_Id;

   function Make_Exception_Declaration
     (Defining_Identifier : Node_Id;
      Renamed_Exception   : Node_Id := No_Node) return Node_Id;

   function Make_Explicit_Dereference (Prefix : Node_Id) return Node_Id;

   function Make_Expression
     (Left_Expr  : Node_Id;
      Operator   : Operator_Type := Op_None;
      Right_Expr : Node_Id       := No_Node) return Node_Id;

   function Make_For_Statement
     (Defining_Identifier : Node_Id;
      Range_Constraint    : Node_Id;
      Statements          : List_Id) return Node_Id;

   function Make_Loop_Statement (Statements : List_Id) return Node_Id;

   function Make_Full_Type_Declaration
     (Defining_Identifier : Node_Id;
      Type_Definition     : Node_Id;
      Discriminant_Spec   : Node_Id := No_Node;
      Parent              : Node_Id := No_Node;
      Is_Subtype          : Boolean := False) return Node_Id;
   --  No_Node as Type_Definition made type declaration without actual
   --  definition (eg. "type X;").

   function Make_If_Statement
     (Condition        : Node_Id;
      Then_Statements  : List_Id;
      Elsif_Statements : List_Id := No_List;
      Else_Statements  : List_Id := No_List) return Node_Id;

   function Make_Indexed_Component
     (Prefix      : Node_Id;
      Expressions : List_Id) return Node_Id;

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node;
      N4 : Node_Id := No_Node) return List_Id;

   function Make_Literal
     (Value             : Value_Id;
      Parent_Designator : Node_Id := No_Node) return Node_Id;

   function Make_Main_Subprogram_Implementation
     (Identifier : Node_Id;
      Build_Spec : Boolean := False;
      Build_Body : Boolean := True) return Node_Id;
   --  If Build_Body is false generate only the spec of a main
   --  subprogram

   function Make_Null_Statement return Node_Id;

   function Make_Object_Declaration
     (Defining_Identifier : Node_Id;
      Constant_Present    : Boolean := False;
      Object_Definition   : Node_Id;
      Expression          : Node_Id := No_Node;
      Parent              : Node_Id := No_Node;
      Renamed_Object      : Node_Id := No_Node;
      Aliased_Present     : Boolean := False;
      Discriminant_Spec   : Node_Id := No_Node) return Node_Id;

   function Make_Object_Instantiation
     (Qualified_Expression : Node_Id) return Node_Id;

   function Make_Package_Declaration (Identifier : Node_Id) return Node_Id;

   function Make_Package_Instantiation
     (Defining_Identifier : Node_Id;
      Generic_Package     : Node_Id;
      Parameter_List      : List_Id := No_List) return Node_Id;

   function Make_Private_Type_Definition return Node_Id;

   function Make_Parameter_Association
     (Selector_Name    : Node_Id;
      Actual_Parameter : Node_Id) return Node_Id;

   function Make_Parameter_Specification
     (Defining_Identifier : Node_Id;
      Subtype_Mark        : Node_Id;
      Parameter_Mode      : Mode_Id := Mode_In;
      Expression          : Node_Id := No_Node) return Node_Id;

   function Make_Pragma_Statement
     (The_Pragma    : Pragma_Id;
      Argument_List : List_Id := No_List) return Node_Id;

   function Make_Protected_Object_Spec
     (Defining_Identifier : Node_Id;
      Visible_Part        : List_Id;
      Private_Part        : List_Id;
      Parent              : Node_Id := Current_Package;
      Is_Type             : Boolean := False) return Node_Id;

   function Make_Protected_Object_Body
     (Defining_Identifier : Node_Id;
      Statements          : List_Id) return Node_Id;

   function Make_Qualified_Expression
     (Subtype_Mark : Node_Id;
      Aggregate    : Node_Id) return Node_Id;

   function Make_Raise_Statement
     (Raised_Error : Node_Id := No_Node) return Node_Id;

   function Make_Range_Constraint
     (First      : Node_Id;
      Last       : Node_Id;
      Index_Type : Node_Id := No_Node) return Node_Id;

   function Make_Record_Aggregate (L : List_Id) return Node_Id;

   function Make_Record_Definition (Component_List : List_Id) return Node_Id;

   function Make_Record_Type_Definition
     (Record_Definition : Node_Id;
      Is_Abstract_Type  : Boolean := False;
      Is_Tagged_Type    : Boolean := False;
      Is_Limited_Type   : Boolean := False) return Node_Id;

   function Make_Return_Statement (Expression : Node_Id) return Node_Id;

   function Make_Subprogram_Call
     (Defining_Identifier   : Node_Id;
      Actual_Parameter_Part : List_Id := No_List) return Node_Id;

   function Make_Selected_Component
     (Prefix        : Node_Id;
      Selector_Name : Node_Id) return Node_Id;

   function Make_Subprogram_Implementation
     (Specification        : Node_Id;
      Declarations         : List_Id;
      Statements           : List_Id;
      Aspect_Specification : Node_Id := No_Node) return Node_Id;

   function Make_Subprogram_Specification
     (Defining_Identifier     : Node_Id;
      Parameter_Profile       : List_Id;
      Return_Type             : Node_Id := No_Node;
      Aspect_Specification    : Node_Id := No_Node;
      Parent                  : Node_Id := Current_Package;
      Renamed_Subprogram      : Node_Id := No_Node;
      Instantiated_Subprogram : Node_Id := No_Node) return Node_Id;

   function Make_Type_Attribute
     (Designator : Node_Id;
      Attribute  : Attribute_Id) return Node_Id;

   function Make_Type_Conversion
     (Subtype_Mark : Node_Id;
      Expression   : Node_Id) return Node_Id;

   function Make_Withed_Package
     (Defining_Identifier : Node_Id;
      Used                : Boolean := False;
      Warnings_Off        : Boolean := False;
      Elaborated          : Boolean := False) return Node_Id;

   function Make_Exit_When_Statement (Condition : Node_Id) return Node_Id;

   function Make_Used_Package (The_Used_Package : Node_Id) return Node_Id;

   function Make_Used_Type (The_Used_Type : Node_Id) return Node_Id;

   function Make_Variant_Part
     (Discriminant : Node_Id;
      Variant_List : List_Id) return Node_Id;

   procedure Make_Comment_Header (Package_Header : List_Id);
   --  This procedure generates a comment header for the generated
   --  packages.

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id;
   --  This function executes Next_Node Num times

   function Message_Comment (M : Name_Id) return Node_Id;
   function Message_Comment (M : String) return Node_Id;
   --  Return a comment message. Used by all the tree
   --  converters

   function Qualified_Designator (P : Node_Id) return Node_Id;

   function Remove_Anonymous_Array_Type_Definition
     (Range_Constraints    : List_Id;
      Component_Definition : Node_Id;
      Aliased_Present      : Boolean := False;
      Variable_Name        : Node_Id;
      Is_Full_Type         : Boolean := False) return Node_Id;
   --  This function removes the anonymous arrays type definition
   --  by creating subtypes, and returns the identifier of type
   --  replacing the anonymous type. Only Make_Full_Type_Declaration
   --  and Make_Object_Declaration use it.

   procedure Set_Homogeneous_Parent_Unit_Name
     (Child  : Node_Id;
      Parent : Node_Id);
   --  This procedure sets correctly the parent unit name of a node
   --  depending on its kind :

   --  * K_Defining_Identifier : the parent unit name is also a
   --  K_Defining_Identifier

   --  * K_Designator : The parent unit name is a K_Designator and the
   --  parent unit name of its defining identifier is also set up.

   --  Units Setters for the PolyORB-HI module

   procedure Set_Main_Spec (N : Node_Id := No_Node);
   procedure Set_Main_Body (N : Node_Id := No_Node);

   procedure Set_Marshallers_Spec (N : Node_Id := No_Node);
   procedure Set_Marshallers_Body (N : Node_Id := No_Node);

   procedure Set_Activity_Spec (N : Node_Id := No_Node);
   procedure Set_Activity_Body (N : Node_Id := No_Node);

   procedure Set_Job_Spec (N : Node_Id := No_Node);
   procedure Set_Job_Body (N : Node_Id := No_Node);

   procedure Set_Transport_Spec (N : Node_Id := No_Node);
   procedure Set_Transport_Body (N : Node_Id := No_Node);

   procedure Set_Types_Spec (N : Node_Id := No_Node);
   procedure Set_Types_Body (N : Node_Id := No_Node);

   procedure Set_Subprograms_Spec (N : Node_Id := No_Node);
   procedure Set_Subprograms_Body (N : Node_Id := No_Node);

   procedure Set_Deployment_Spec (N : Node_Id := No_Node);

   procedure Set_Naming_Spec (N : Node_Id := No_Node);

   function To_Ada_Name (N : Name_Id) return Name_Id;
   --  Convert N to a valid Ada identifier (no clashing with keywords,
   --  no consecutive '_', no heading '_'...).

   function Unit_Name (N : Name_Id) return Name_Id;
   --  Given an ENTITY fully qualified name A.B.C.D, returns A.B.C
   --  Raises an error if the name does not contains any dot.
   --  Return No_Name is unit name is Standard

   function Local_Name (N : Name_Id) return Name_Id;
   --  Given an ENTITY fully qualified name A.B.C.D, returns D

   function Conventional_Base_Name (N : Name_Id) return Name_Id;
   --  Given a UNIT fully qualified name A.D.C, returns a-b-c

   function Fully_Qualified_Name (N : Node_Id) return Name_Id;

   function Extract_Designator
     (N               : Node_Id;
      Add_With_Clause : Boolean := True) return Node_Id;
   --  Extracts the designator of the *Ada* entity N and return a copy
   --  of it after adding the proper 'with' clause to the current
   --  package if 'Add_With_Clause' is True. N may be:
   --  * a type declaration
   --  * a subprogram specification
   --  * an object declaration
   --  * a package specification
   --  * a package declaration

end Ocarina.Backends.Ada_Tree.Nutils;
