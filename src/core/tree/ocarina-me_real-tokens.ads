------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . M E _ R E A L . T O K E N S                --
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

--  Description of REAL tokens

with Ocarina.Types;
with Locations;
with Ocarina.Files;

package Ocarina.ME_REAL.Tokens is
   use Locations;
   use Ocarina.Types;
   use Ocarina.Files;

   type Token_Type is
     (T_Error,
      T_Identifier,
      T_Pragma,

   --  About template types

      T_Sequence,
      T_String,
      T_Wstring,
      T_Fixed,

   --  Graphic characters

      T_Colon,
      T_Colon_Colon,
      T_Comma,
      T_Left_Brace,
      T_Left_Bracket,
      T_Left_Paren,
      T_Right_Brace,
      T_Right_Bracket,
      T_Right_Paren,
      T_Semi_Colon,

      T_Equal,
      T_Not,
      T_And,
      T_Or,
      T_Less_Equal,
      T_Greater_Equal,
      T_Greater,
      T_Less,
      T_Different,
      T_Minus,
      T_Plus,
      T_Star,
      T_Modulo,
      T_Slash,
      T_Power,
      T_Affect,

   --  pre-defined sets

      T_Processor_Set,
      T_Virtual_Processor_Set,
      T_Virtual_Bus_Set,
      T_Process_Set,
      T_Thread_Set,
      T_Threadgroup_Set,
      T_Subprogram_Call_Set,
      T_Sequence_Call_Set,
      T_Subprogram_Set,
      T_Data_Set,
      T_Memory_Set,
      T_Bus_Set,
      T_Connection_Set,
      T_Device_Set,
      T_End_To_End_Flows_Set,
      T_System_Set,
      T_Abstract_Set,
      T_Root_System_Set,
      T_Unknown_Set,
      T_Local_Set,

   --  Selection function

      T_Is_Subcomponent_Of,
      T_Is_Bound_To,
      T_Is_Connected_To,
      T_Is_Called_By,
      T_Is_Accessed_By,
      T_Is_Accessing_To,
      T_Is_Passing_Through,
      T_Is_Calling,
      T_Is_Provided_Class,
      T_Is_Predecessor_Of,
      T_Is_Connecting_To,

   --  Verification function

      T_Get_Property_Value,
      T_Get_System_Property_Value,
      T_Cardinal,
      T_First,
      T_Property_Exists,
      T_Last,
      T_Expr,
      T_Size,       --  Size of a list
      T_Float,
      T_List,       --  Build a list from set or enumeration
      T_Head,       --  Return the first node of a list
      T_Queue,      --  Return the list minus  the first node
      T_Integer,
      T_Ceil,
      T_Floor,
      T_Max,
      T_Min,
      T_Cos,
      T_Sin,
      T_Tan,
      T_Cosh,
      T_Sinh,
      T_Tanh,
      T_Ln,
      T_Exp,
      T_Sqrt,

      T_Is_In,      --  Checks weither list_1 is included into list_2
      T_LCM,        --  Lowest Common Multiple
      T_GCD,        --  Greatest Common Divisor
      T_Non_Null,   --  N -> [0, 1]
      T_All_Equals,
      T_Product,
      T_Sum,

   --  Ranges set-level functions

      T_MMax,
      T_MProduct,
      T_MMin,
      T_MAll_Equals,
      T_MSum,

   --  Literals

      T_False,
      T_True,
      T_Boolean_Literal,
      T_Integer_Literal,
      T_Fixed_Point_Literal,
      T_Floating_Point_Literal,
      T_Character_Literal,
      T_Wide_Character_Literal,
      T_String_Literal,
      T_Wide_String_Literal,

   --  keywords

      T_Check,
      T_End,
      T_Foreach,
      T_In,
      T_Do,
      T_Sothat,
      T_Requires,
      T_Return,
      T_Var,
      T_Compute,
      T_If,
      T_Then,
      T_Else,
      T_Global,
      T_Theorem,

      T_Anonymous_Set,
      T_EOF);

   First_Token_Pos : constant        := Token_Type'Pos (Token_Type'First);
   Last_Token_Pos  : constant        := Token_Type'Pos (Token_Type'Last);
   Prefix          : constant String := "%real%";

   subtype Literal_Type is Token_Type range T_False .. T_Wide_String_Literal;

   subtype Operator_Type is Token_Type range T_Equal .. T_Affect;

   subtype Predefined_Sets is Token_Type range T_Processor_Set .. T_Local_Set;

   subtype Selection_Function_Type is
     Token_Type range T_Processor_Set .. T_Is_Connecting_To;

   subtype Verification_Function_Type is
     Token_Type range T_Get_Property_Value .. T_Sum;

   subtype Higher_Level_Function_Type is Token_Type range T_MMax .. T_MSum;

   subtype Boolean_Operator_Type is Token_Type range T_Not .. T_Or;

   subtype Keyword_Type is Token_Type range T_Check .. T_Theorem;

   Token_Image : array (Token_Type) of Name_Id;

   ---------------------------------------------------
   -- Global variables updated by the token scanner --
   ---------------------------------------------------

   Token      : Token_Type;
   Token_Name : Name_Id;
   Token_Location : Location renames Ocarina.Files.Buffer_Location;

   Language      : constant String := "real_specification";
   REAL_Language : Name_Id;

   procedure Init_Tokens;

   function Image (T : Token_Type) return String;

   function Quoted_Image (T : Token_Type) return String;

   function Is_Literal (T : Token_Type) return Boolean;

   function Is_Operator (T : Token_Type) return Boolean;

   function Is_Set_Operator (T : Token_Type) return Boolean;

   function Is_Predefined_Set (T : Token_Type) return Boolean;

   function Is_Boolean_Operator (T : Token_Type) return Boolean;

   function Is_Scoped_Name (T : Token_Type) return Boolean;

end Ocarina.ME_REAL.Tokens;
