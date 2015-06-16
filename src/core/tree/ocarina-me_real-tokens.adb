------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . M E _ R E A L . T O K E N S                --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet;
with Utils;

package body Ocarina.ME_REAL.Tokens is
   use Ocarina.Namet;

   procedure New_Token (Token : Token_Type; Image : String);
   --  Compute token image and store it in Token_Image table. When
   --  Token is a graphical character, embrace its image between
   --  single quotes ('<<' and '>>' are considered as graphical
   --  characters). When Token is a keyword, embrace its image between
   --  double quotes. Enter the lower-case form of a keyword image
   --  into the name table and set name table byte to its token
   --  position in order to resolve it easily.

   -----------------
   -- Init_Tokens --
   -----------------

   procedure Init_Tokens is
   begin
      --  Enter all the alphabetic keywords in the name table

      New_Token (T_Check, "check");
      New_Token (T_End, "end");
      New_Token (T_Foreach, "foreach");
      New_Token (T_In, "in");
      New_Token (T_Theorem, "theorem");
      New_Token (T_Sothat, "|");
      New_Token (T_Return, "return");
      New_Token (T_Var, "var");
      New_Token (T_If, "if");
      New_Token (T_Then, "then");
      New_Token (T_Else, "else");
      New_Token (T_Global, "global");
      New_Token (T_Compute, "compute");
      New_Token (T_Requires, "requires");
      New_Token (T_Do, "do");

      New_Token (T_Error, "<error>");
      New_Token (T_Affect, ":=");
      New_Token (T_And, "and");
      New_Token (T_Or, "or");
      New_Token (T_Not, "not");
      New_Token (T_False, "FALSE");
      New_Token (T_True, "TRUE");
      New_Token (T_Wstring, "wstring");
      New_Token (T_Semi_Colon, ";");
      New_Token (T_Left_Brace, "{");
      New_Token (T_Right_Brace, "}");
      New_Token (T_Colon, ":");
      New_Token (T_Comma, ",");
      New_Token (T_Colon_Colon, "'::'");
      New_Token (T_Left_Paren, "(");
      New_Token (T_Right_Paren, ")");
      New_Token (T_Equal, "=");
      New_Token (T_Plus, "+");
      New_Token (T_Modulo, "mod");
      New_Token (T_Star, "*");
      New_Token (T_Slash, "/");
      New_Token (T_Power, "**");
      New_Token (T_Minus, "-");
      New_Token (T_Less_Equal, "<=");
      New_Token (T_Greater_Equal, ">=");
      New_Token (T_Less, "<");
      New_Token (T_Different, "<>");
      New_Token (T_Greater, ">");
      New_Token (T_Left_Bracket, "[");
      New_Token (T_Right_Bracket, "]");
      New_Token (T_Anonymous_Set, "Anonymous_Set");

      New_Token (T_Integer_Literal, "<int literal>");
      New_Token (T_Fixed_Point_Literal, "<fixed point literal>");
      New_Token (T_Floating_Point_Literal, "<floating point literal>");
      New_Token (T_Character_Literal, "<character literal>");
      New_Token (T_Wide_Character_Literal, "<wide character literal>");
      New_Token (T_String_Literal, "<string literal>");
      New_Token (T_Wide_String_Literal, "<wide string literal>");
      New_Token (T_Identifier, "<identifier>");
      New_Token (T_Pragma, "<pragma>");

      New_Token (T_Processor_Set, "Processor_Set");
      New_Token (T_Virtual_Processor_Set, "Virtual_Processor_Set");
      New_Token (T_Process_Set, "Process_Set");
      New_Token (T_Thread_Set, "Thread_Set");
      New_Token (T_Threadgroup_Set, "Threadgroup_Set");
      New_Token (T_Subprogram_Call_Set, "Subprogram_Call_Set");
      New_Token (T_Sequence_Call_Set, "Sequence_Call_Set");
      New_Token (T_Subprogram_Set, "Subprogram_Set");
      New_Token (T_Data_Set, "Data_Set");
      New_Token (T_Memory_Set, "Memory_Set");
      New_Token (T_Bus_Set, "Bus_Set");
      New_Token (T_Virtual_Bus_Set, "Virtual_Bus_Set");
      New_Token (T_End_To_End_Flows_Set, "End_To_End_Flows_Set");
      New_Token (T_Connection_Set, "Connection_Set");
      New_Token (T_Device_Set, "Device_Set");
      New_Token (T_System_Set, "System_Set");
      New_Token (T_Abstract_Set, "Abstract_Set");
      New_Token (T_Root_System_Set, "Root_System_Set");
      New_Token (T_Unknown_Set, "Unknown_Set");
      New_Token (T_Local_Set, "Local");

      New_Token (T_Is_Subcomponent_Of, "Is_Subcomponent_Of");
      New_Token (T_Is_Bound_To, "Is_Bound_To");
      New_Token (T_Is_Connected_To, "Is_Connected_To");
      New_Token (T_Is_Called_By, "Is_Called_By");
      New_Token (T_Is_Calling, "Is_Calling");
      New_Token (T_Is_Accessed_By, "Is_Accessed_By");
      New_Token (T_Is_Accessing_To, "Is_Accessing_To");
      New_Token (T_Is_Passing_Through, "Is_Passing_Through");
      New_Token (T_Is_Predecessor_Of, "Is_Predecessor_Of");
      New_Token (T_Is_Provided_Class, "Is_Provided_Class");
      New_Token (T_Is_Connecting_To, "Is_Connecting_To");

      New_Token (T_Get_Property_Value, "Get_Property_Value");
      New_Token (T_Get_Property_Value, "Property");
      New_Token (T_Get_System_Property_Value, "Get_System_Property_Value");
      New_Token (T_Get_System_Property_Value, "System_Property");
      New_Token (T_Cardinal, "Cardinal");
      New_Token (T_First, "First");
      New_Token (T_Last, "Last");
      New_Token (T_Head, "Head");
      New_Token (T_Queue, "Queue");
      New_Token (T_Property_Exists, "Property_Exists");
      New_Token (T_Property_Exists, "Exists");
      New_Token (T_Expr, "Expr");
      New_Token (T_Size, "Size");
      New_Token (T_Integer, "Integer");
      New_Token (T_Integer, "Int");
      New_Token (T_List, "List");
      New_Token (T_Float, "Float");
      New_Token (T_Max, "Max");
      New_Token (T_Ceil, "ceil");
      New_Token (T_Floor, "floor");
      New_Token (T_Cos, "cos");
      New_Token (T_Sin, "sin");
      New_Token (T_Tan, "tan");
      New_Token (T_Cosh, "cosh");
      New_Token (T_Sinh, "sinh");
      New_Token (T_Tanh, "tanh");
      New_Token (T_Ln, "ln");
      New_Token (T_Exp, "exp");
      New_Token (T_Sqrt, "sqrt");

      New_Token (T_LCM, "LCM");
      New_Token (T_GCD, "GCD");
      New_Token (T_Non_Null, "Non_Null");
      New_Token (T_Is_In, "Is_In");
      New_Token (T_Min, "Min");
      New_Token (T_All_Equals, "All_Equals");
      New_Token (T_Product, "Product");
      New_Token (T_Sum, "Sum");

      New_Token (T_MMax, "MMax");
      New_Token (T_MProduct, "MProduct");
      New_Token (T_MMin, "MMin");
      New_Token (T_MAll_Equals, "MAll_Equals");
      New_Token (T_MSum, "MSum");

      New_Token (T_EOF, "**}");

   end Init_Tokens;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
      S : constant String := Get_Name_String (Token_Image (T));
   begin
      return S (S'First + Prefix'Length .. S'Last);
   end Image;

   ------------------
   -- Quoted_Image --
   ------------------

   function Quoted_Image (T : Token_Type) return String is
   begin
      case T is
         when T_Identifier          |
           T_Integer_Literal        |
           T_String_Literal         |
           T_Fixed_Point_Literal    |
           T_Floating_Point_Literal =>
            return Image (T);

         when others =>
            return "'" & Image (T) & "'";
      end case;
   end Quoted_Image;

   ----------------
   -- Is_Literal --
   ----------------

   function Is_Literal (T : Token_Type) return Boolean is
   begin
      return T in Literal_Type;
   end Is_Literal;

   -----------------
   -- Is_Operator --
   -----------------

   function Is_Operator (T : Token_Type) return Boolean is
   begin
      return T in Operator_Type;
   end Is_Operator;

   -----------------------
   -- Is_Predefined_Set --
   -----------------------

   function Is_Predefined_Set (T : Token_Type) return Boolean is
   begin
      return T in Predefined_Sets;
   end Is_Predefined_Set;

   ---------------------
   -- Is_Set_Operator --
   ---------------------

   function Is_Set_Operator (T : Token_Type) return Boolean is
   begin

      return (T = T_Plus or else T = T_Star or else T = T_Minus);
   end Is_Set_Operator;

   -------------------------
   -- Is_Boolean_Operator --
   -------------------------

   function Is_Boolean_Operator (T : Token_Type) return Boolean is
   begin
      return T in Boolean_Operator_Type or else T = T_Not;
   end Is_Boolean_Operator;

   --------------------
   -- Is_Scoped_Name --
   --------------------

   function Is_Scoped_Name (T : Token_Type) return Boolean is
   begin
      return T = T_Identifier or else T = T_Colon_Colon;
   end Is_Scoped_Name;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token (Token : Token_Type; Image : String) is
      use Utils;

      N : Name_Id;
   begin
      Set_Str_To_Name_Buffer (Prefix & Image);
      Token_Image (Token) := Name_Find;

      if Token in Keyword_Type
        or else Token in Predefined_Sets
        or else Token in Selection_Function_Type
        or else Token in Verification_Function_Type
        or else Token in Higher_Level_Function_Type
        or else Token in T_Equal .. T_Affect
      then
         N := To_Lower (Name_Find);
         Set_Name_Table_Byte (N, Byte (Token_Type'Pos (Token)));
      end if;
   end New_Token;

end Ocarina.ME_REAL.Tokens;
