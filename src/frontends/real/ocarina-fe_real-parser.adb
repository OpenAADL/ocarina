------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . F E _ R E A L . P A R S E R                --
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

with GNAT.Table;
with GNAT.Command_Line;
with Ocarina.FE_REAL.Lexer;
with Ocarina.FE_REAL.Parser_Errors;
with Ocarina.ME_REAL.Tokens;
with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.REAL_Tree.Utils;
with Ocarina.ME_REAL.REAL_Tree.Nutils;
with Ocarina.REAL_Values;
with Ocarina.Builder.REAL;
with Ocarina.Parser;
with Ocarina.Files;
with Ocarina.Analyzer.REAL;
with Ocarina.Namet;

package body Ocarina.FE_REAL.Parser is

   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Utils;
   use Ocarina.ME_REAL.REAL_Tree.Nutils;
   use Ocarina.ME_REAL.Tokens;
   use Ocarina.Analyzer.REAL;
   use Ocarina.Builder.REAL;
   use Ocarina.FE_REAL.Lexer;
   use Ocarina.FE_REAL.Parser_Errors;

   function P_Theorem return Node_Id;

   function P_Set_Range_Declaration return Node_Id;

   procedure P_Declarations (R : Node_Id; Success : out Boolean);
   --  Parse sets and variables declarations

   function Create_Check_Expression return Node_Id;

   function P_Check_Expression return Node_Id;
   --  FIXME :
   --  Bug with 'true' and 'false' boolean literals which
   --  cannot be parsed yet, cause unknown.

   function P_Set_Expression return Node_Id;
   function P_Higher_Level_Function return Value_Id;
   function P_Single_Set_Declaration return Node_Id;
   function P_Create_Set_Identifier return Node_Id;
   function P_Check_Subprogram_Call return Node_Id;
   function P_Ternary_Expression return Node_Id;
   function P_Expression return Node_Id;

   function P_Requirements return List_Id;
   --  Get required theorems.
   --  Does *not* check for existence (cf. analyzer)

   function P_Identifier return Node_Id;
   function Make_Literal (T : Token_Type) return Node_Id;

   procedure Skip_To_Theorem_End (Success : out Boolean);
   --  Whenever an error occurs in parsing, this procedure will
   --  search for the nearest 'end;' keywords, in order to avoid
   --  parsing errors to spread to the following theorems.

   Current_Theorem_Node : Node_Id;
   Current_Theorem_Name : Name_Id;

   package Expressions is new GNAT.Table (Node_Id, Natural, 1, 100, 10);
   package REAL_Libs is new GNAT.Table (Name_Id, Nat, 1, 10, 10);

   Preferences : constant array (OV_Equal .. OV_Power) of Natural :=
     (OV_Power         => 1,
      OV_Not           => 2,
      OV_Star          => 3,
      OV_Slash         => 4,
      OV_Modulo        => 5,
      OV_Minus         => 6,
      OV_Plus          => 7,
      OV_Greater       => 8,
      OV_Less          => 9,
      OV_Greater_Equal => 10,
      OV_Less_Equal    => 11,
      OV_Different     => 12,
      OV_Equal         => 13,
      OV_Or            => 14,
      OV_And           => 15);

   ----------------------
   -- P_Set_Expression --
   ----------------------

   function P_Set_Expression return Node_Id is
      use Expressions;

      function Is_Expression_Completed return Boolean;
      --  Return True when there are no more token to read to complete
      --  the current expression.

      function P_Expression_Part return Node_Id;
      --  Return a node describing an expression. It is either a
      --  binary operator (an operator with no right expression
      --  assigned) or an expression value (a scoped name, a literal
      --  or an expression with an unary operator - that is a binary
      --  operator with a right inner expression and no left inner
      --  expression - or an expression with both inner expressions
      --  assigned). Note that whether an operator is a binary or
      --  unary operator is resolved in this routine. For a unary
      --  operator, we check that the previous token was a binary
      --  operator.

      function Is_Binary_Operator (E : Node_Id) return Boolean;
      --  Return True when N is an operator with the right expression
      --  *still* not assigned. Otherwise, an operator with a right
      --  expression is a value expression.

      function Is_Expression_Value (E : Node_Id) return Boolean;
      --  Return True when N is not an operator (literal or scoped
      --  name) or else when its right expression is assigned (unary
      --  operator).

      function Precede (L, R : Node_Id) return Boolean;
      --  Does operator L precedes operator R

      function Translate_Operator (T : Token_Type) return Operator_Id;
      --  Return the Operator_Id corresponding to the token read

      ------------------------
      -- Translate_Operator --
      ------------------------

      function Translate_Operator (T : Token_Type) return Operator_Id is
      begin
         case T is
            when T_Star =>
               return OV_Star;

            when T_Plus =>
               return OV_Plus;

            when T_Minus =>
               return OV_Minus;

            when others =>
               return OV_Invalid;
         end case;
      end Translate_Operator;

      -----------------------------
      -- Is_Expression_Completed --
      -----------------------------

      function Is_Expression_Completed return Boolean is
         T : constant Token_Type := Next_Token;
      begin
         return T /= T_Identifier
           and then T not in Predefined_Sets
           and then T /= T_Left_Paren
           and then not Is_Set_Operator (T);
      end Is_Expression_Completed;

      -------------------------
      -- Is_Expression_Value --
      -------------------------

      function Is_Expression_Value (E : Node_Id) return Boolean is
      begin
         return Kind (E) = K_Set
           or else
           (Kind (E) = K_Set_Expression and then Present (Right_Expr (E)));
      end Is_Expression_Value;

      ------------------------
      -- Is_Binary_Operator --
      ------------------------

      function Is_Binary_Operator (E : Node_Id) return Boolean is
      begin
         return Kind (E) = K_Set_Expression
           and then Operator (E) in Operator_Values
           and then No (Right_Expr (E));
      end Is_Binary_Operator;

      -----------------------
      -- P_Expression_Part --
      -----------------------

      function P_Expression_Part return Node_Id is
         Expression     : Node_Id;
         Previous_Token : Token_Type;
         Op             : Operator_Id;
      begin
         case Next_Token is

            when T_Identifier =>

               --  We build a set reference with identifier name
               Scan_Token;
               Expression := New_Node (K_Set_Reference, Token_Location);
               Set_Name (Expression, To_Lower (Token_Name));
               Set_Predefined_Type (Expression, SV_No_Type);
               Set_Referenced_Set (Expression, No_Node);

            when T_Processor_Set .. T_Local_Set =>

               --  We build a set reference with predefined name
               Scan_Token;
               Expression := New_Node (K_Set_Reference, Token_Location);
               Set_Name (Expression, To_Lower (Token_Name));
               Set_Predefined_Type
                 (Expression,
                  Translate_Predefined_Sets (Token));
               Set_Referenced_Set (Expression, No_Node);

            when T_Left_Paren =>

               --  Look for a parenthesized expression value

               --  past '(', no error possible

               Scan_Token (T_Left_Paren);

               Expression := P_Set_Expression;

               Scan_Token (T_Right_Paren);
               if Token = T_Error then
                  DPE (PC_Set_Expression, T_Right_Paren);
                  return No_Node;
               end if;

            when T_Plus | T_Minus | T_Star =>

               --  Look for a binary/unary operator

               Previous_Token := Token;
               Scan_Token;  --  past binary/unary operator

               Expression := New_Node (K_Set_Expression, Token_Location);
               Set_Set_Type (Expression, SV_No_Type);

               Op := Translate_Operator (Token);
               if Op = OV_Invalid then
                  DPE (PC_Set_Expression, EMC_Expected_Valid_Operator);
                  Set_Last (First - 1);
                  return No_Node;
               end if;
               Set_Operator (Expression, Op);

               --  Cannot have two following operators except in the
               --  special case above.

               if Is_Operator (Previous_Token) then
                  DPE (PC_Set_Expression, EMC_Expected_Valid_Operator);
                  return No_Node;
               end if;

            when others =>
               DPE (PC_Set_Expression, EMC_Cannot_Parse_Set_Expression);
               return No_Node;
         end case;

         return Expression;
      end P_Expression_Part;

      -------------
      -- Precede --
      -------------

      function Precede (L, R : Node_Id) return Boolean is
         Left_Operator  : constant Operator_Id := Operator (L);
         Right_Operator : constant Operator_Id := Operator (R);
      begin
         return Preferences (Left_Operator) < Preferences (Right_Operator);
      end Precede;

      Expr  : Node_Id;
      First : Natural;
   begin

      --  Read enough expressions to push as first expression a binary
      --  operator with no right expression

      Expr := P_Expression_Part;
      if No (Expr) then
         return No_Node;
      end if;

      --  We must have first an expression value

      if Is_Binary_Operator (Expr) then
         DPE (PC_Set_Expression, EMC_Cannot_Parse_Set_Expression);
         return No_Node;
      end if;

      --  We have only one expression value

      if Is_Expression_Completed then
         return Expr;
      end if;

      Increment_Last;
      Table (Last) := Expr;
      First        := Last;

      Expr := P_Expression_Part;
      if No (Expr) then
         Set_Last (First - 1);
         return No_Node;
      end if;

      --  We must have a binary operator as the first expression is an
      --  expression value.

      if not Is_Binary_Operator (Expr) then
         DPE (PC_Set_Expression, EMC_Cannot_Parse_Set_Expression);
         Set_Last (First - 1);
         return No_Node;
      end if;

      Set_Left_Expr (Expr, Table (Last));
      Table (Last) := Expr;

      --  Push expressions in stack and check that the top of the
      --  stack consists in one or more binary operators with no
      --  right expr and zero or one expression value.

      while not Is_Expression_Completed loop

         Expr := P_Expression_Part;
         if No (Expr) then
            return No_Node;
         end if;

         Increment_Last;
         Table (Last) := Expr;

         --  Check that this new expression is not a binary operator
         --  when the previous one is a binary operator with no right
         --  expression.

         if First < Last
           and then Is_Binary_Operator (Expr)
           and then No (Left_Expr (Expr))
           and then Is_Binary_Operator (Table (Last - 1))
         then
            DPE (PC_Set_Expression, EMC_Cannot_Parse_Set_Expression);
            Set_Last (First - 1);
            return No_Node;
         end if;

         --  Check whether we have a sequence of a binary operator
         --  (left operator), an expression value and another binary
         --  operator (right operator). In this case, if the left
         --  operator has a better precedence than the right one, we
         --  can reduce the global expression by assigning the
         --  expression value to the right expression of the left
         --  operator. Then as the left operator has already a left
         --  expression, it becomes an expression value which can be
         --  assign to the left expression of the right operation.
         --  Recompute the size of the expression stack.

         while First + 1 < Last
           and then Is_Expression_Value (Table (Last - 1))
           and then Precede (Table (Last - 2), Expr)
         loop
            Set_Right_Expr (Table (Last - 2), Table (Last - 1));
            Set_Left_Expr (Table (Last), Table (Last - 2));
            Table (Last - 2) := Table (Last);
            Set_Last (Last - 2);
         end loop;
      end loop;

      --  The last expression is not a value. We cannot reduce the
      --  global expression

      if Is_Binary_Operator (Table (Last)) then
         DPE (PC_Set_Expression, EMC_Cannot_Parse_Set_Expression);
         Set_Last (First - 1);
         return No_Node;
      end if;

      --  Reduce the global expression

      while First < Last loop
         if No (Left_Expr (Table (Last - 1))) then
            Set_Right_Expr (Table (Last - 1), Table (Last));
            Set_Left_Expr (Table (Last - 1), Table (Last - 2));
            Table (Last - 2) := Table (Last - 1);
            Set_Last (Last - 2);

         else
            Set_Right_Expr (Table (Last - 1), Table (Last));
            Set_Last (Last - 1);
         end if;
      end loop;

      Expr := Table (First);
      Set_Last (First - 1);

      return Expr;

   end P_Set_Expression;

   ------------------------
   -- P_Check_Expression --
   ------------------------

   function P_Check_Expression return Node_Id is
      use Expressions;

      function Is_Expression_Completed return Boolean;
      --  Return True when there are no more token to read to complete
      --  the current expression.

      function P_Expression_Part return Node_Id;
      --  Return a node describing an expression. It is either a
      --  binary operator (an operator with no right expression
      --  assigned) or an expression value (a scoped name, a literal
      --  or an expression with an unary operator - that is a binary
      --  operator with a right inner expression and no left inner
      --  expression - or an expression with both inner expressions
      --  assigned). Note that whether an operator is a binary or
      --  unary operator is resolved in this routine. For a unary
      --  operator, we check that the previous token was a binary
      --  operator.

      function Is_Binary_Operator (E : Node_Id) return Boolean;
      --  Return True when N is an operator with the right expression
      --  *still* not assigned. Otherwise, an operator with a right
      --  expression is a value expression.

      function Is_Expression_Value (E : Node_Id) return Boolean;
      --  Return True when N is not an operator (literal or scoped
      --  name) or else when its right expression is assigned (unary
      --  operator).

      function Precede (L, R : Node_Id) return Boolean;
      --  Does operator L precedes operator R

      function Translate_Operator (T : Token_Type) return Operator_Id;
      --  Return the operator corresponding to the parameter-given token

      ------------------------
      -- Translate_Operator --
      ------------------------

      function Translate_Operator (T : Token_Type) return Operator_Id is
      begin
         case T is
            when T_Or =>
               return OV_Or;

            when T_And =>
               return OV_And;

            when T_Not =>
               return OV_Not;

            when T_Equal =>
               return OV_Equal;

            when T_Different =>
               return OV_Different;

            when T_Greater =>
               return OV_Greater;

            when T_Less =>
               return OV_Less;

            when T_Greater_Equal =>
               return OV_Greater_Equal;

            when T_Less_Equal =>
               return OV_Less_Equal;

            when T_Minus =>
               return OV_Minus;

            when T_Plus =>
               return OV_Plus;

            when T_Modulo =>
               return OV_Modulo;

            when T_Star =>
               return OV_Star;

            when T_Slash =>
               return OV_Slash;

            when T_Power =>
               return OV_Power;

            when others =>
               return OV_Invalid;
         end case;
      end Translate_Operator;

      -----------------------------
      -- Is_Expression_Completed --
      -----------------------------

      function Is_Expression_Completed return Boolean is
         T : constant Token_Type := Next_Token;
      begin
         return T not in Literal_Type
           and then T /= T_Identifier
           and then T not in Selection_Function_Type
           and then T not in Verification_Function_Type
           and then T /= T_Left_Paren
           and then T not in T_Equal .. T_Power;
      end Is_Expression_Completed;

      -------------------------
      -- Is_Expression_Value --
      -------------------------

      function Is_Expression_Value (E : Node_Id) return Boolean is
      begin
         return Kind (E) = K_Literal
           or else Kind (E) = K_Identifier
           or else Kind (E) = K_Var_Reference
           or else
           (Operator (E) not in OV_Equal .. OV_Power
            and then Operator (E) /= OV_Equal)
           or else Present (Right_Expr (E));
      end Is_Expression_Value;

      ------------------------
      -- Is_Binary_Operator --
      ------------------------

      function Is_Binary_Operator (E : Node_Id) return Boolean is
      begin
         return Kind (E) = K_Check_Expression
           and then Operator (E) in OV_Equal .. OV_Power
           and then No (Right_Expr (E));
      end Is_Binary_Operator;

      -----------------------
      -- P_Expression_Part --
      -----------------------

      function P_Expression_Part return Node_Id is
         Expression     : Node_Id;
         Right_Expr     : Node_Id;
         Previous_Token : Token_Type;
         Op             : Operator_Id;
      begin
         case Next_Token is

            when T_Is_Subcomponent_Of .. T_Is_Connecting_To =>

               --  We build a check (in this case, verification)
               --  subprogram call

               Expression := P_Check_Subprogram_Call;
               if No (Expression) then
                  return No_Node;
               end if;

            when T_Get_Property_Value .. T_Sum =>

               --  We build a check subprogram call

               Expression := P_Check_Subprogram_Call;
               if No (Expression) then
                  return No_Node;
               end if;

            when T_Left_Paren =>

               --  Look for a parenthesized expression value

               Scan_Token;  --  past '('
               Expression := P_Expression;
               Scan_Token (T_Right_Paren);
               if Token = T_Error then
                  DPE (PC_Check_Expression, T_Right_Paren);
                  return No_Node;
               end if;

            when T_Equal .. T_Power =>

               --  Look for a binary/unary operator

               Previous_Token := Token;
               Scan_Token;  --  past binary/unary operator
               Expression := New_Node (K_Check_Expression, Token_Location);

               Op := Translate_Operator (Token);
               if Op = OV_Invalid then
                  DPE (PC_Check_Expression, EMC_Expected_Valid_Operator);
                  Set_Last (First - 1);
                  return No_Node;
               end if;
               Set_Operator (Expression, Op);

               --  Token is a real unary operator

               if Token = T_Not
                 or else
                 (Token = T_Minus
                  and then Previous_Token /= T_Affect
                  and then Previous_Token /= T_Right_Paren
                  and then
                  (Is_Operator (Previous_Token)
                   or else Previous_Token = T_Left_Paren))
               then
                  case Next_Token is
                     when T_Get_Property_Value .. T_Sum |
                       T_Left_Paren                     |
                       T_Identifier                     =>

                        Right_Expr := P_Expression;
                        if No (Right_Expr) then
                           return No_Node;
                        end if;
                        Set_Right_Expr (Expression, Right_Expr);

                     when others =>
                        DPE (PC_Check_Expression, EMC_Expected_Valid_Operator);
                        return No_Node;
                  end case;

               --  Cannot have two following operators except in the
               --  special case above.

               elsif Is_Operator (Previous_Token) then
                  DPE (PC_Check_Expression, EMC_Unexpected_Operator);
                  return No_Node;
               end if;

            when T_False .. T_Wide_String_Literal =>

               --  Look for a literal

               Scan_Token;
               Expression := Make_Literal (Token);

            when T_Identifier =>

               --  The only identifier allowed are variables names

               Expression := P_Identifier;
               Expression := Make_Var_Reference (Name (Expression));

            when others =>
               DPE (PC_Check_Expression, EMC_Cannot_Parse_Check_Expression);
               return No_Node;
         end case;

         return Expression;
      end P_Expression_Part;

      -------------
      -- Precede --
      -------------

      function Precede (L, R : Node_Id) return Boolean is
         Left_Operator : constant Operator_Id := Operator (L);
--         Right_Operator : constant Operator_Id := Operator (R);
      begin
         if Kind (R) = K_Check_Subprogram_Call then
            return True;
         end if;

         return Preferences (Left_Operator) < Preferences (Operator (R));
      end Precede;

      Expr  : Node_Id;
      First : Natural;
   begin

      --  Read enough expressions to push as first expression a binary
      --  operator with no right expression

      Expr := P_Expression_Part;
      if No (Expr) then
         return No_Node;
      end if;

      --  We must have first an expression value

      if Is_Binary_Operator (Expr) then
         DPE (PC_Check_Expression, EMC_Cannot_Parse_Check_Expression);
         return No_Node;
      end if;

      --  We have only one expression value

      if Is_Expression_Completed then
         return Expr;
      end if;

      Increment_Last;
      Table (Last) := Expr;
      First        := Last;

      Expr := P_Expression_Part;
      if No (Expr) then
         Set_Last (First - 1);
         return No_Node;
      end if;

      --  We must have a binary operator as the first expression is an
      --  expression value.

      if not Is_Binary_Operator (Expr) then
         DPE (PC_Check_Expression, EMC_Cannot_Parse_Check_Expression);
         Set_Last (First - 1);
         return No_Node;
      end if;

      Set_Left_Expr (Expr, Table (Last));
      Table (Last) := Expr;

      --  Push expressions in stack and check that the top of the
      --  stack consists in one or more binary operators with no
      --  right expr and zero or one expression value.

      while not Is_Expression_Completed loop

         Expr := P_Expression_Part;
         if No (Expr) then
            return No_Node;
         end if;

         Increment_Last;
         Table (Last) := Expr;

         --  Check that this new expression is not a binary operator
         --  when the previous one is a binary operator with no right
         --  expression.

         if First < Last
           and then Is_Binary_Operator (Expr)
           and then No (Left_Expr (Expr))
           and then Is_Binary_Operator (Table (Last - 1))
         then
            DPE (PC_Check_Expression, EMC_Cannot_Parse_Check_Expression);
            Set_Last (First - 1);
            return No_Node;
         end if;

         --  Check whether we have a sequence of a binary operator
         --  (left operator), an expression value and another binary
         --  operator (right operator). In this case, if the left
         --  operator has a better precedence than the right one, we
         --  can reduce the global expression by assigning the
         --  expression value to the right expression of the left
         --  operator. Then as the left operator has already a left
         --  expression, it becomes an expression value which can be
         --  assigned to the left expression of the right operation.
         --  Recompute the size of the expression stack.

         while First + 1 < Last
           and then Kind (Table (Last - 1)) /= K_Check_Subprogram_Call
           and then Is_Expression_Value (Table (Last - 1))
           and then Precede (Table (Last - 2), Expr)
         loop
            Set_Right_Expr (Table (Last - 2), Table (Last - 1));
            Set_Left_Expr (Table (Last), Table (Last - 2));
            Table (Last - 2) := Table (Last);
            Set_Last (Last - 2);
         end loop;
      end loop;

      --  The last expression is not a value. We cannot reduce the
      --  global expression

      if Is_Binary_Operator (Table (Last)) then
         DPE (PC_Check_Expression, EMC_Cannot_Parse_Check_Expression);
         Set_Last (First - 1);
         return No_Node;
      end if;

      --  Reduce the global expression

      while First < Last loop
         if No (Left_Expr (Table (Last - 1))) then
            Set_Right_Expr (Table (Last - 1), Table (Last));
            Set_Left_Expr (Table (Last - 1), Table (Last - 2));
            Table (Last - 2) := Table (Last - 1);
            Set_Last (Last - 2);

         else
            Set_Right_Expr (Table (Last - 1), Table (Last));
            Set_Last (Last - 1);
         end if;
      end loop;
      Expr := Table (First);
      Set_Last (First - 1);

      return Expr;
   end P_Check_Expression;

   --------------------------
   -- P_Ternary_Expression --
   --------------------------

   function P_Ternary_Expression return Node_Id is

      function Translate_Operator (T : Token_Type) return Operator_Id;

      ------------------------
      -- Translate_Operator --
      ------------------------

      function Translate_Operator (T : Token_Type) return Operator_Id is
      begin
         case T is
            when T_If =>
               return OV_If_Then_Else;
            when others =>
               return OV_Invalid;
         end case;
      end Translate_Operator;

      N  : Node_Id;
      E1 : Node_Id;
      E2 : Node_Id;
      E3 : Node_Id;
   begin
      N := New_Node (K_Ternary_Expression, Token_Location);

      Scan_Token;
      Set_Operator (N, Translate_Operator (Token));

      case Token is
         when T_If =>
            E1 := P_Expression;
            if No (E1) then
               return No_Node;
            end if;

            Scan_Token (T_Then);
            if Token = T_Error then
               DPE (PC_Ternary_Expression, T_Then);
            end if;

            E2 := P_Expression;
            if No (E2) then
               return No_Node;
            end if;

            Scan_Token (T_Else);
            if Token = T_Error then
               DPE (PC_Ternary_Expression, T_Else);
            end if;

            E3 := P_Expression;
            if No (E3) then
               return No_Node;
            end if;

         when others =>
            return No_Node;
      end case;

      Set_Left_Expr (N, E1);
      Set_Right_Expr (N, E2);
      Set_Third_Expr (N, E3);

      return N;
   end P_Ternary_Expression;

   ------------------
   -- P_Expression --
   ------------------

   function P_Expression return Node_Id is
      L : Location;
   begin
      Save_Lexer (L);
      Scan_Token;
      if Token = T_If then
         Restore_Lexer (L);
         return P_Ternary_Expression;
      else
         Restore_Lexer (L);
         return P_Check_Expression;
      end if;
   end P_Expression;

   -----------------------------
   -- P_Check_Subprogram_Call --
   -----------------------------

   function P_Check_Subprogram_Call return Node_Id is
      N     : Node_Id;
      L     : constant List_Id := New_List (K_List_Id, Token_Location);
      Param : Node_Id;
   begin
      N := New_Node (K_Check_Subprogram_Call, Token_Location);

      if Next_Token not in Verification_Function_Type
        and then Next_Token not in Selection_Function_Type
      then
         Scan_Token;
         DPE (PC_Check_Subprogram_Call, EMC_Expected_Predefined_Function_Name);
         return No_Node;
      end if;
      Set_Variable_Position (N, Value_Id (0));
      Set_Identifier (N, P_Identifier);
      Set_Code (N, Translate_Function_Code (Token));

      --  it's possible that a subprogram was called without args
      --  (eg. when passed as parameter)

      if Next_Token /= T_Left_Paren then
         Set_Parameters (N, No_List);
         return N;
      else
         --  No error possible

         Scan_Token (T_Left_Paren);
      end if;

      --  We parse all parameters

      while Next_Token /= T_Right_Paren loop

         if Next_Token = T_EOF then
            DPE (PC_Check_Subprogram_Call, T_Right_Paren);
            return No_Node;
         end if;

         --  We could have either a literal, a set name (identifier
         --  or predefined set) or a check expression.

         case Next_Token is

            when T_Get_Property_Value .. T_Sum =>

               --  Verification function name

               Param := P_Check_Subprogram_Call;
               Append_Node_To_List (Param, L);

            when T_Is_Subcomponent_Of .. T_Is_Connecting_To =>

               --  selection function name

               Param := P_Check_Subprogram_Call;
               Append_Node_To_List (Param, L);

            when T_Processor_Set .. T_Local_Set =>

               --  We build a set reference with predefined name

               Scan_Token;
               Param := New_Node (K_Set_Reference, Token_Location);
               Set_Name (Param, To_Lower (Token_Name));
               Set_Predefined_Type (Param, Translate_Predefined_Sets (Token));
               Set_Referenced_Set (Param, No_Node);
               Append_Node_To_List (Param, L);

            when T_Identifier =>

               --  Must be a set name or the global set identifier or
               --  a variable name

               Scan_Token;
               Param := New_Node (K_Identifier, Token_Location);
               Set_Name (Param, Token_Name);
               Append_Node_To_List (Param, L);

            when T_Integer_Literal .. T_Wide_String_Literal =>

               --  A literal

               Scan_Token;
               Param := Make_Literal (Token);
               Append_Node_To_List (Param, L);

            when T_Left_Paren =>

               --  A selection expression

               Param := Create_Check_Expression;
               Append_Node_To_List (Param, L);

            when others =>
               DPE
                 (PC_Check_Subprogram_Call,
                  EMC_Expected_Valid_Function_Parameter);
               return No_Node;
         end case;

         if Next_Token /= T_Comma then

            if Next_Token /= T_Right_Paren then
               Scan_Token;
               DPE (PC_Check_Subprogram_Call, T_Right_Paren);
               return No_Node;
            end if;
         else
            --  We pass the comma, no error possible

            Scan_Token (T_Comma);
         end if;

      end loop;

      Scan_Token (T_Right_Paren);
      if Token = T_Error then
         DPE (PC_Check_Subprogram_Call, T_Right_Paren);
         return No_Node;
      end if;

      Set_Parameters (N, L);
      return N;

   end P_Check_Subprogram_Call;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal (T : Token_Type) return Node_Id is
      use Ocarina.REAL_Values;

      Const : constant Node_Id := New_Node (K_Literal, Token_Location);
   begin
      case T is
         when T_String_Literal | T_Wide_String_Literal =>
            Set_Value
              (Const,
               New_String_Value (To_Lower (String_Literal_Value)));

         when T_True =>
            Set_Value (Const, New_Boolean_Value (True));

         when T_False =>
            Set_Value (Const, New_Boolean_Value (False));

         when T_Integer_Literal =>
            Set_Value
              (Const,
               New_Integer_Value
                 (Value => Unsigned_Long_Long (Integer_Literal_Value),
                  Base  => Unsigned_Short_Short (Integer_Literal_Base)));

         when T_Floating_Point_Literal =>
            Set_Value (Const, New_Real_Value (Float_Literal_Value));

         when others =>
            DPE (PC_Make_Literal, EMC_Expected_Literal_Value);
            return No_Node;
      end case;

      return Const;
   end Make_Literal;

   -----------------------------
   -- P_Create_Set_Identifier --
   -----------------------------

   function P_Create_Set_Identifier return Node_Id is
      N     : Node_Id;
      Ident : Node_Id;
      Param : Node_Id := No_Node;
      L     : List_Id := No_List;
   begin
      --  Set identifier can be either parametrized
      --  or not

      --  First we read the set identifier

      Ident := P_Identifier;
      if No (Ident) then
         return No_Node;
      end if;

      --  Then if we have a left parenthesis
      --  the set is parametrized by an identifier

      if Next_Token = T_Left_Paren then

         --  We skip the '('
         Scan_Token;

         Param := P_Identifier;

         if No (Param) then
            return No_Node;
         end if;

         Scan_Token (T_Right_Paren);
         if Token = T_Error then
            DPE (PC_Create_Set_Identifier, T_Right_Paren);
            return No_Node;
         end if;

         L := New_List (K_List_Id, Token_Location);
         Append_Node_To_List (Param, L);
      end if;

      N := New_Node (K_Parametrized_Identifier, Token_Location);
      Set_Identifier (N, Ident);
      Set_Parameters (N, L);

      return N;
   end P_Create_Set_Identifier;

   ------------------------------
   -- P_Single_Var_Declaration --
   ------------------------------

   function P_Single_Var_Declaration return Node_Id is
      Var_Decl : Node_Id;
      Var_Id   : Node_Id;
      Var, P   : Node_Id;
      Trm_Id   : Node_Id;
      Params   : List_Id := No_List;
      Global   : Boolean;
   begin
      --  Parse the variable range keyword (var or global)

      Scan_Token;
      case Token is
         when T_Var =>
            Global := False;
         when T_Global =>
            Global := True;
         when others =>
            DPE (PC_Single_Variable_Declaration, EMC_Variable_Declaration);
            return No_Node;
      end case;

      --  Create variable identifier

      Var_Id := P_Identifier;
      if No (Var_Id) then
         return No_Node;
      end if;
      Var := Make_Var_Reference (Name (Var_Id));

      --  Parse the affectation (":=")

      Scan_Token (T_Affect);
      if Token = T_Error then
         DPE (PC_Single_Variable_Declaration, T_Affect);
         return No_Node;
      end if;

      --  A variable declaration can be done either with
      --  a expression or with a call to another theorem

      if Next_Token = T_Compute then
         Scan_Token (T_Compute);

         Var_Decl := New_Node (K_Variable_Decl_Compute, Token_Location);

         Trm_Id := P_Identifier;
         if No (Trm_Id) then
            return No_Node;
         end if;

         if Next_Token = T_Left_Paren then
            Scan_Token (T_Left_Paren);
            Params := New_List (K_List_Id, Token_Location);

            --  The first parameter must be the sub-theorem domain
            --  Thus a set or a variable

            case Next_Token is

               when T_Identifier =>
                  Scan_Token;
                  P := New_Node (K_Identifier, Token_Location);
                  Set_Name (P, To_Lower (Token_Name));
                  Append_Node_To_List (P, Params);

               when T_Processor_Set .. T_Local_Set =>
                  Scan_Token;
                  P := New_Node (K_Set_Reference, Token_Location);
                  Set_Name (P, To_Lower (Token_Name));
                  Set_Predefined_Type (P, Translate_Predefined_Sets (Token));
                  Set_Referenced_Set (P, No_Node);
                  Append_Node_To_List (P, Params);

               when others =>
                  DPE
                    (PC_Single_Variable_Declaration,
                     EMC_Subtheorem_Parameter);
                  return No_Node;
            end case;

            if Next_Token = T_Comma then
               Scan_Token (T_Comma);

               --  We parse all others parameters

               while Next_Token /= T_Right_Paren loop

                  --  We could have either a literal, or a variable

                  case Next_Token is

                     when T_EOF =>
                        DPE
                          (PC_Single_Variable_Declaration,
                           EMC_Unexpected_Error);
                        return No_Node;

                     when T_Integer_Literal .. T_Wide_String_Literal =>
                        Scan_Token;
                        P := Make_Literal (Token);
                        Append_Node_To_List (P, Params);

                     when T_Identifier =>
                        Scan_Token;
                        P := New_Node (K_Identifier, Token_Location);
                        Set_Name (P, To_Lower (Token_Name));
                        Append_Node_To_List (P, Params);

                     when others =>
                        DPE
                          (PC_Single_Variable_Declaration,
                           EMC_Unexpected_Token);
                        return No_Node;
                  end case;

                  if Next_Token /= T_Comma then

                     if Next_Token /= T_Right_Paren then
                        Scan_Token;
                        DPE (PC_Single_Variable_Declaration, T_Comma);
                        return No_Node;
                     end if;
                  else
                     --  We pass the comma, no error possible
                     Scan_Token (T_Comma);
                  end if;
               end loop;
            end if;

            Scan_Token (T_Right_Paren);
            if Token = T_Error then
               DPE (PC_Single_Variable_Declaration, T_Right_Paren);
               return No_Node;
            end if;
         end if;

         Set_Parameters (Var_Decl, Params);
         Set_True_params (Var_Decl, No_List);
         Set_Domain (Var_Decl, No_Node);
         Set_Var_Ref (Var_Decl, Var);
         Set_Theorem_Name (Var_Decl, Name (Trm_Id));
      else
         declare
            use Ocarina.REAL_Values;
            D, C : Node_Id;
            V    : Value_Id := No_Value;
            L    : Location;
         begin
            Var_Decl := New_Node (K_Variable_Decl_Expression, Token_Location);

            D := New_Node (K_Return_Expression, Token_Location);

            Save_Lexer (L);
            Scan_Token;
            if Token in Higher_Level_Function_Type then
               V := Translate_Function_Code (Token);
               Set_Range_Function (D, V);
            else
               Restore_Lexer (L);
            end if;

            C := P_Expression;

            Set_Check_Expression (D, C);
            Set_Return_Expr (Var_Decl, D);
            Set_Var_Ref (Var_Decl, Var);
         end;
      end if;

      Scan_Token (T_Semi_Colon);
      if Token = T_Error then
         DPE (PC_Single_Variable_Declaration, T_Semi_Colon);
         return No_Node;
      end if;

      --  Set scope status

      if Global then
         Set_Is_Global (Var_Decl, Value_Id (1));
      else
         Set_Is_Global (Var_Decl, Value_Id (0));
      end if;

      return Var_Decl;
   end P_Single_Var_Declaration;

   ------------------------------
   -- P_Single_Set_Declaration --
   ------------------------------

   function P_Single_Set_Declaration return Node_Id is
      Set_Decl     : Node_Id;
      Set_Id       : Node_Id;
      Set_Expr     : Node_Id;
      Set_Def      : Node_Id;
      State        : Location;
      State_2      : Location;
      Is_Dependant : Boolean := False;
   begin

      --  We parse the set name
      --  wich can be parametrized

      Save_Lexer (State);
      Set_Id := P_Create_Set_Identifier;
      if No (Set_Id) then
         return No_Node;
      end if;

      --  We check weither the declared set is parametrized or not

      Save_Lexer (State_2);
      Restore_Lexer (State);
      Scan_Token (T_Identifier);
      Scan_Token;
      if Token = T_Left_Paren then
         Is_Dependant := True;
      end if;
      Restore_Lexer (State_2);

      --  Then we should have ':=' token

      Scan_Token (T_Affect);
      if Token = T_Error then
         DPE (PC_Single_Set_Declaration, T_Affect);
         return No_Node;
      end if;

      --  We enter braces

      Scan_Token (T_Left_Brace);
      if Token = T_Error then
         DPE (PC_Single_Set_Declaration, T_Left_Brace);
         return No_Node;
      end if;

      Set_Decl := New_Node (K_Set_Declaration, Token_Location);
      Set_Parametrized_Expr (Set_Decl, Set_Id);
      if Is_Dependant then
         Set_Dependant (Set_Decl, Value_Id (1));
      else
         Set_Dependant (Set_Decl, Value_Id (0));
      end if;

      --  Parse the local variable

      Set_Local_Variable (Set_Decl, Make_Var_Reference (Name (P_Identifier)));
      if Token = T_Error then
         DPE (PC_Single_Set_Declaration, EMC_Unexpected_Error);
         return No_Node;
      end if;

      --  Then we should have 'in' token

      Scan_Token (T_In);
      if Token = T_Error then
         DPE (PC_Single_Set_Declaration, T_In);
         return No_Node;
      end if;

      --  We parse the set expression

      Set_Expr := P_Set_Expression;
      Set_Local_Set_Expression (Set_Decl, Set_Expr);

      --  We pass the sothat

      Scan_Token (T_Sothat);

      if Token = T_Error then
         DPE (PC_Single_Set_Declaration, T_Sothat);
         return No_Node;
      end if;

      --  We parse set definition

      Set_Def := P_Expression;
      if No (Set_Def) then
         return No_Node;
      end if;

      Set_Selection_Expression (Set_Decl, Set_Def);

      --  We read the right brace (exit of set declaration)

      Scan_Token (T_Right_Brace);
      if Token = T_Error then
         DPE (PC_Single_Set_Declaration, T_Right_Brace);
         return No_Node;
      end if;

      --  We pass the semi-colon

      Scan_Token (T_Semi_Colon);
      if Token = T_Error then
         DPE (PC_Single_Set_Declaration, T_Semi_Colon);
         return No_Node;
      end if;

      return Set_Decl;
   end P_Single_Set_Declaration;

   --------------------
   -- P_Requirements --
   --------------------

   function P_Requirements return List_Id is
      L : constant List_Id := New_List (K_List_Id, Token_Location);
      P : Node_Id;
   begin

      if Next_Token /= T_Requires then
         return L;
      else
         Scan_Token;
      end if;

      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         DPE (PC_Requirements_List, T_Left_Paren);
         return No_List;
      end if;

      loop
         Scan_Token (T_Identifier);
         if Token = T_Error then
            DPE (PC_Requirements_List, T_Identifier);
            return No_List;
         end if;

         P := New_Node (K_Required_Theorem, Token_Location);
         Set_Theorem_Name (P, To_Lower (Token_Name));
         Append_Node_To_List (P, L);

         Scan_Token;

         case Token is
            when T_And =>
               null;

            when T_Right_Paren =>
               exit;

            when others =>
               DPE (PC_Requirements_List, EMC_Wrong_List_Connector);
               return No_List;
         end case;

      end loop;

      Scan_Token (T_Semi_Colon);
      if Token = T_Error then
         DPE (PC_Requirements_List, T_Semi_Colon);
         return No_List;
      end if;

      return L;
   end P_Requirements;

   --------------------
   -- P_Declarations --
   --------------------

   procedure P_Declarations (R : Node_Id; Success : out Boolean) is
      pragma Assert (Kind (R) = K_Theorem);

      Declarations_List : constant List_Id :=
        New_List (K_List_Id, Token_Location);
      Declaration : Node_Id := No_Node;
      Stop        : Boolean := False;
   begin
      Success := True;
      while not Stop and then Success loop

         case Next_Token is

            when T_Identifier =>  --  Set declaration
               Declaration := P_Single_Set_Declaration;
               if No (Declaration) then
                  Success := False;
               else
                  Append_Node_To_List (Declaration, Declarations_List);
               end if;

            when T_Var | T_Global =>  --  Variable declaration
               Declaration := P_Single_Var_Declaration;
               if No (Declaration) then
                  Success := False;
               else
                  Append_Node_To_List (Declaration, Declarations_List);
               end if;

            when T_Return | T_Requires | T_Check =>
               Stop := True;

            when others =>
               DPE (PC_Declarations_List, EMC_Declaration_Parameter);
               Success := False;
         end case;

      end loop;

      if Success then
         Set_Declarations (R, Declarations_List);
      end if;
   end P_Declarations;

   -----------------------------
   -- Create_Check_Expression --
   -----------------------------

   function Create_Check_Expression return Node_Id is
      N : Node_Id;
   begin
      --  We pass the left parenthesis

      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         DPE (PC_Create_Check_Expression, T_Left_Paren);
         return No_Node;
      end if;

      N := P_Expression;
      if No (N) then
         return No_Node;
      end if;

      --  We pass the right parenthesis

      Scan_Token (T_Right_Paren);
      if Token = T_Error then
         DPE (PC_Create_Check_Expression, T_Right_Paren);
         return No_Node;
      end if;

      return N;
   end Create_Check_Expression;

   ------------------
   -- P_Identifier --
   ------------------

   function P_Identifier return Node_Id is
      use Ocarina.REAL_Values;

      Identifier : Node_Id;
   begin
      Scan_Token;

      if Token /= T_Identifier
        and then Token not in Selection_Function_Type
        and then Token not in Verification_Function_Type
      then
         DPE (PC_Identifier_Declaration, EMC_Used_Keyword);
         return No_Node;
      end if;
      Identifier := New_Node (K_Identifier, Token_Location);
      Set_Name (Identifier, To_Lower (Token_Name));
      return Identifier;
   end P_Identifier;

   -----------------------------
   -- P_Set_Range_Declaration --
   -----------------------------

   function P_Set_Range_Declaration return Node_Id is
      Range_Decl : Node_Id;
      Identifier : Node_Id;
      Elem       : Node_Id;
      Set_Expr   : Node_Id;
   begin
      Range_Decl := New_Node (K_Range_Declaration, Token_Location);

      --  Scan 'foreach'

      Scan_Token (T_Foreach);
      if Token = T_Error then
         DPE (PC_Set_Declarations, T_Foreach);
         return No_Node;
      end if;

      --  Scan the element name

      Identifier := P_Identifier;
      if No (Identifier) then
         DPE (PC_Set_Declarations, T_Identifier);
         return No_Node;
      end if;

      Elem := New_Node (K_Element, Token_Location);
      Set_Identifier (Elem, Identifier);
      Set_Element_Type (Elem, SV_No_Type);

      --  Scan 'in'

      Scan_Token (T_In);
      if Token = T_Error then
         DPE (PC_Set_Declarations, T_In);
         return No_Node;
      end if;

      Set_Expr := P_Set_Expression;
      if No (Set_Expr) then
         return No_Node;
      end if;

      --  Scan 'do'

      Scan_Token (T_Do);
      if Token = T_Error then
         DPE (PC_Set_Declarations, T_Do);
         return No_Node;
      end if;

      Set_Range_Variable (Range_Decl, Elem);
      Set_Range_Set (Range_Decl, Set_Expr);
      Set_Variable_Ref
        (Range_Decl,
         New_Node (K_Var_Reference, Token_Location));

      return Range_Decl;
   end P_Set_Range_Declaration;

   -----------------------------
   -- P_Higher_Level_Function --
   -----------------------------

   function P_Higher_Level_Function return Value_Id is
      use Ocarina.REAL_Values;

      State : Location;
      V     : Value_Id := No_Value;
   begin
      Save_Lexer (State);

      Scan_Token (T_Left_Paren);
      if Token = T_Error then
         DPE (PC_Function, T_Left_Paren);
         Restore_Lexer (State);
         return No_Value;
      end if;

      Scan_Token;
      if Token in Higher_Level_Function_Type then
         V := Translate_Function_Code (Token);
      else
         Restore_Lexer (State);
      end if;

      return V;
   end P_Higher_Level_Function;

   ---------------
   -- P_Theorem --
   ---------------

   function P_Theorem return Node_Id is
      use Ocarina.REAL_Values;

      Node         : Node_Id;
      Identifier   : Node_Id;
      Range_Decl   : Node_Id;
      Test         : Node_Id;
      Returns      : Node_Id;
      Requirements : List_Id;
      V            : Value_Id;
      Success      : Boolean;
   begin
      Node := New_Node (K_Theorem, Token_Location);
      Set_Used_Set (Node, New_List (K_List_Id, Token_Location));
      Set_Used_Var (Node, New_List (K_List_Id, Token_Location));
      Set_Local_Var (Node, New_List (K_List_Id, Token_Location));

      Scan_Token;
      Identifier := New_Node (K_Identifier, Token_Location);
      Set_Name (Identifier, To_Lower (Token_Name));
      Set_Identifier (Node, Identifier);
      Set_Related_Entity (Node, Owner_Node);
      Current_Theorem_Name := Name (Identifier);

      --  This line allow to find back the theorem's name

      Current_Theorem_Node := Node;

      Range_Decl := P_Set_Range_Declaration;

      if No (Range_Decl) then
         return No_Node;
      else
         Set_Range_Declaration (Node, Range_Decl);
      end if;

      P_Declarations (Node, Success);

      if not Success then
         return No_Node;
      end if;

      Requirements := P_Requirements;
      if Requirements /= No_List then
         Set_Required_Theorems (Node, Requirements);
      else
         return No_Node;
      end if;

      --  Here we have either a check or a return token
      Scan_Token;
      if Token /= T_Check and then Token /= T_Return then
         DPE (PC_Theorem, EMC_Testing_Token_Expected);
         return No_Node;
      end if;

      if Token = T_Check then
         Test := Create_Check_Expression;
         if No (Test) then
            return No_Node;
         else
            Set_Check_Expression (Node, Test);
         end if;

         Set_Return_Expression (Node, No_Node);
      else
         --  Return expression
         --  "return expressions" are actually the same
         --  as "check expressions"

         Returns := New_Node (K_Return_Expression, Token_Location);
         V       := P_Higher_Level_Function;
         Set_Range_Function (Returns, V);

         Test := Create_Check_Expression;

         if V /= No_Value then
            Scan_Token (T_Right_Paren);
            if Token = T_Error then
               DPE (PC_Theorem, T_Right_Paren);
               return No_Node;
            end if;
         end if;

         if No (Test) then
            return No_Node;
         else
            Set_Check_Expression (Returns, Test);
            Set_Return_Expression (Node, Returns);
         end if;
         Set_Check_Expression (Node, No_Node);
      end if;

      --  We pass the semi-colon

      Scan_Token;
      if Token /= T_Semi_Colon then
         DPE (PC_Theorem, T_Semi_Colon);
         return No_Node;
      end if;

      return Node;
   end P_Theorem;

   -------------------------
   -- Skip_To_Theorem_End --
   -------------------------

   procedure Skip_To_Theorem_End (Success : out Boolean) is
      End_Found : Boolean := (Token = T_End);
   begin
      Success := True;

      --  FIXME :
      --  put back all references to scan_token (x) => scan_token in parser,
      --  in Order To Be Able To Find Back The "end" Keyword when It Was
      --  Unexpectedly Parsed.

      while not End_Found loop
         Scan_Token;

         exit when Token = T_EOF;

         if Token = T_End then
            End_Found := True;
         end if;
      end loop;

      if not End_Found then
         DPE (PC_Main, T_End);
         Success := False;
         return;
      end if;

      Scan_Token;
      case Token is

         when T_Identifier =>
            if To_Lower (Token_Name) /= Current_Theorem_Name then
               DPE (PC_Main, EMC_Non_Matching_Theorem_Name);
               Success := False;
               return;
            end if;
            Scan_Token (T_Semi_Colon);
            if Token = T_Error then
               DPE (PC_Main, T_Semi_Colon);
               Success := False;
               return;
            end if;

         when T_Semi_Colon =>
            return;

         when others =>
            DPE (PC_Main, T_Semi_Colon);
            Success := False;
            return;
      end case;

   end Skip_To_Theorem_End;

   -------------
   -- Process --
   -------------

   function Process
     (AADL_Root : Node_Id;
      From      : Location;
      To        : Location := No_Location) return Node_Id
   is
      pragma Unreferenced (AADL_Root);

      Buffer  : Location;
      Root    : constant Node_Id := New_Node (K_Root_Node, From);
      N       : Node_Id;
      Success : Boolean          := True;
      State   : Location;
   begin
      Buffer := From;

      if To /= No_Location then
         Buffer.EOF := To.Last_Pos;
      end if;

      Restore_Lexer (Buffer);

      Set_Theorems (Root, New_List (K_List_Id, From));

      Scan_Token;

      while Token = T_Theorem loop
         N := P_Theorem;

         Skip_To_Theorem_End (Success);

         if No (N) or else not Success then
            N := Current_Theorem_Node;
            DPE (PC_Main, EMC_Syntax_Error);
            Success := False;
            exit;
         end if;

         Append_Node_To_List (N, Theorems (Root));

         --  Pass to the next theorem

         Save_Lexer (State);
         Scan_Token;
      end loop;

      if Success then
         if Token /= T_EOF then
            DPE (PC_Main, T_EOF);
            return No_Node;
         end if;

         --  the AADL main parser need the EOF ("**}") token to be the
         --  first lexem available after a successful annex parsing

         Restore_Lexer (State);

         return Root;
      else
         DPE (PC_Main, EMC_Syntax_Error);
         return No_Node;
      end if;
   end Process;

   ----------
   -- Init --
   ----------

   procedure Init is
      use GNAT.Command_Line;
      use Ocarina.Parser;
      use Ocarina.Namet;

      C : Character;
   begin
      Current_Theorem_Node := No_Node;

      Initialize_Option_Scan;
      loop
         C := Getopt ("* real_lib: real_theorem: real_continue_eval");
         case C is
            when ASCII.NUL =>
               exit;

            when 'r' =>
               if Full_Switch = "real_lib" then
                  REAL_Libs.Append (Get_String_Name (Parameter));
               end if;

               if Full_Switch = "real_theorem" then
                  Main_Theorem := Get_String_Name (Parameter);
               end if;

               if Full_Switch = "real_continue_eval" then
                  Continue_Evaluation := True;
               end if;

            when others =>
               null;
         end case;
      end loop;

      REAL_Language := Get_String_Name (Language);
      Register_Parser (Ocarina.ME_REAL.Tokens.Language, Process'Access);

      --  If a REAL library file had been defined, we
      --  parse and register it.

      for J in REAL_Libs.First .. REAL_Libs.Last loop
         declare
            use Ocarina.Files;

            Buffer            : Location;
            REAL_External_Lib : Node_Id;
         begin
            Buffer := Load_File (REAL_Libs.Table (J));
            if Buffer = No_Location then
               Display_And_Exit
                 ("could not load file " &
                  Get_Name_String (REAL_Libs.Table (J)));
            end if;

            REAL_External_Lib := Process (No_Node, Buffer);
            if No (REAL_External_Lib) then
               Display_And_Exit ("could not parse REAL specification");
            end if;

            Register_Library_Theorems (REAL_External_Lib);
         end;
      end loop;
   end Init;

end Ocarina.FE_REAL.Parser;
