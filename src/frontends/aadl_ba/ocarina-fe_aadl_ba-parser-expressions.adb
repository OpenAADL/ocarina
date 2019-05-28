------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.FE_AADL_BA.PARSER.EXPRESSIONS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

with Ocarina.FE_AADL_BA.Lexer;
with Ocarina.FE_AADL_BA.Parser.Identifiers;
with Ocarina.FE_AADL_BA.Parser.Actions;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.AADL_Values;

with Ocarina.Builder.Aadl_Ba.Expressions;

package body Ocarina.FE_AADL_BA.Parser.Expressions is

   use Ocarina.FE_AADL_BA.Lexer;
   use Ocarina.FE_AADL_BA.Parser.Identifiers;
   use Ocarina.FE_AADL_BA.Parser.Actions;

   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.Tokens;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   use Ocarina.AADL_Values;

   use Ocarina.Builder.Aadl_Ba.Expressions;

   ----------------------
   -- P_Value_Variable --
   ----------------------

   --  value_variable ::=
   --    incoming_port_name
   --  | incoming_port_name ?
   --  | incoming_subprogram_parameter_identifier
   --  | incoming_port_prototype_name
   --  | data_component_reference
   --  | port_name ’ count
   --  | port_name ’ fresh
   --  | port_name ’ updated

   function P_Value_Variable (Container : Types.Node_Id) return Node_Id is
      Start_Loc    : Location;
      Loc          : Location;
      Ident        : Node_Id;
      Value_Variable : Node_Id;
      Count        : Boolean  := False;
      Fresh        : Boolean  := False;
      Updated      : Boolean  := False;
      Interrog     : Boolean  := False;

   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      if Token /= T_Identifier then
         DPE (PC_Value_Variable, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         Restore_Lexer (Start_Loc);
         Ident := P_Name (Container);
         if No (Ident) then
            DPE (PC_Value_Variable, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Dot then
            Restore_Lexer (Start_Loc);
            Ident := P_Data_Component_Reference (Container);
            if No (Ident) then
               DPE (PC_Value_Variable, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Tick =>
            Scan_Token;
            if Token = T_Count then
               Count := True;
            elsif Token = T_Fresh then
               Fresh := True;
            elsif Token = T_Updated then
               Updated := True;
            else
               DPE (PC_Value_Variable, Expected_Tokens =>
                                   (T_Count, T_Fresh, T_Updated));
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Interrogative =>
            Interrog := True;

         when others =>
            Restore_Lexer (Loc);
      end case;

      Value_Variable := Add_New_Value_Variable (Start_Loc, Container,
                                                Ident, Count, Fresh,
                                                Updated, Interrog);
      if No (Value_Variable) then
         DPE (PC_Value_Variable, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Value_Variable;
      end if;

   end P_Value_Variable;

   ----------------------
   -- Value_Expression --
   ----------------------

   --  value_expression ::= relation { logical_operator relation}*

   function P_Value_Expression (Container : Types.Node_Id) return Node_Id is
      Start_Loc     : Location;
      Loc           : Location;
      Value_Expr    : Node_Id;
      Item          : Node_Id;
      Relation_List : List_Id;
      Escape        : Boolean       := False;
      Operator_Node : Node_Id;
      Operat_Kind   : Operator_Kind := OK_No_Kind;
   begin
      Save_Lexer (Start_Loc);
      Relation_List := New_List (K_List_Id, Token_Location);

      loop
         Item := P_Relation (Container);
         if Present (Item) then
            Append_Node_To_List (Item, Relation_List);
         else
            Relation_List := No_List;
            exit;
         end if;

         Save_Lexer (Loc);
         Scan_Token;

         case Token is
            when T_And =>
               Operat_Kind := OK_And;
               Save_Lexer (Loc);
               Scan_Token;

               if Token = T_Then then
                  Operat_Kind := OK_And_Then;
               else
                  Restore_Lexer (Loc);
               end if;
            when T_Or =>
               Operat_Kind := OK_Or;
               Save_Lexer (Loc);
               Scan_Token;

               if Token = T_Else then
                  Operat_Kind := OK_Or_Else;
               else
                  Restore_Lexer (Loc);
               end if;
            when T_Xor =>
               Operat_Kind := OK_Xor;
            when others =>
               Escape := True;
         end case;

         if Escape then
            Restore_Lexer (Loc);
            exit;
         else
            Operator_Node := Add_New_Operator (Loc, No_Node, Operat_Kind);

            if No (Operator_Node) then
               DPE (PC_Value_Expression, EMC_Failed);
               return No_Node;
            end if;

            Append_Node_To_List (Operator_Node, Relation_List);
         end if;
      end loop;

      if not Is_Empty (Relation_List) then
         Set_Loc (Node_Id (Relation_List),
                  Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                        (Relation_List)));
      else
         DPE (PC_Value_Expression, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Value_Expr := Add_New_Value_Expression (Start_Loc, Container,
                                              Relation_List);
      if No (Value_Expr) then
         DPE (PC_Value_Expression, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Value_Expr;
      end if;

   end P_Value_Expression;

   ----------------
   -- P_Relation --
   ----------------

   --  relation ::=
   --   simple_expression [relational_operator simple_expression]

   function P_Relation (Container : Types.Node_Id) return Node_Id is
      Start_Loc     : Location;
      --  Bool_Val      : Boolean  := False;
      Spl_Expr_List : List_Id  := No_List;
      Node          : Node_Id;
   begin
      Save_Lexer (Start_Loc);
      --  Scan_Token;

      --  case Token is
      --   when T_True =>
      --      Bool_Val := True;

      --   when T_False =>
      --      Bool_Val := False;

      --   when others =>
      Spl_Expr_List := P_Simple_Expressions (Start_Loc);
      if Is_Empty (Spl_Expr_List) then
         DPE (PC_Relation, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;
      --  end case;

      Node := Add_New_Relation (Start_Loc, Container, Spl_Expr_List);
      if No (Node) then
         DPE (PC_Relation, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Node;
      end if;

   end P_Relation;

   --------------------------
   -- P_Simple_Expressions --
   --------------------------

   --  simple_expression [relational_operator simple_expression]

   function P_Simple_Expressions (Start_Loc : Location) return List_Id
   is
      Loc             : Location;
      Item            : Node_Id;
      Expression_List : List_Id;
      First_Parsing   : Boolean   := True;
   begin
      Restore_Lexer (Start_Loc);
      Expression_List := New_List (K_List_Id, Token_Location);

      loop
         if not First_Parsing then
            Save_Lexer (Loc);
            Scan_Token;
            if Token in BA_Relational_Operator then
               Item := P_Operator (No_Node);
               if Present (Item) then
                  Append_Node_To_List (Item, Expression_List);
               else
                  DPE (PC_Simple_Expressions, EMC_Failed);
                  Expression_List := No_List;
                  exit;
               end if;
            else
               Restore_Lexer (Loc);
               exit;
            end if;
         else
            First_Parsing := False;
         end if;

         Item := P_Simple_Expression (No_Node);
         if Present (Item) then
            Append_Node_To_List (Item, Expression_List);
         else
            DPE (PC_Simple_Expressions, EMC_Failed);
            Expression_List := No_List;
            exit;
         end if;

      end loop;

      if not Is_Empty (Expression_List) then
         Set_Loc (Node_Id (Expression_List),
               Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                       (Expression_List)));
      else
         DPE (PC_Simple_Expressions, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_List;
      end if;

      return Expression_List;

   end P_Simple_Expressions;

   -------------------------
   -- P_Simple_Expression --
   -------------------------

   --  simple_expression ::=
   --    [unary_adding_operator] term {binary_adding_operator term}*

   function P_Simple_Expression (Container : Types.Node_Id) return Node_Id is
      Start_Loc        : Location;
      Loc              : Location;
      Simple_Expr_Node : Node_Id;
      Operator_Node    : Node_Id;
      Term_Node        : Node_Id;
      Simple_Expr_List : List_Id;
      Escape           : Boolean   := False;
   begin
      Save_Lexer (Start_Loc);
      Simple_Expr_List := New_List (K_List_Id, Start_Loc);

      Scan_Token;

      if Token in BA_Unary_Adding_Operator then
         Operator_Node := P_Operator (No_Node);
         if Present (Operator_Node) then
            Append_Node_To_List (Operator_Node, Simple_Expr_List);
         else
            DPE (PC_Simple_Expression, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Start_Loc);
      end if;

      Term_Node := P_Term (No_Node);
      if Present (Term_Node) then
         Append_Node_To_List (Term_Node, Simple_Expr_List);
      else
         DPE (PC_Simple_Expression, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      loop
         Save_Lexer (Loc);
         Scan_Token;

         if Token in BA_Binary_Adding_Operator then
            Operator_Node := P_Operator (No_Node);
            if Present (Operator_Node) then
               Append_Node_To_List (Operator_Node, Simple_Expr_List);
            else
               DPE (PC_Simple_Expression, EMC_Failed);
               Simple_Expr_List := No_List;
               Escape := True;
            end if;

            Term_Node := P_Term (No_Node);
            if Present (Term_Node) then
               Append_Node_To_List (Term_Node, Simple_Expr_List);
            else
               DPE (PC_Simple_Expression, EMC_Failed);
               Simple_Expr_List := No_List;
               Escape := True;
            end if;
         else
            Restore_Lexer (Loc);
            Escape := True;
         end if;

         if Escape then
            exit;
         end if;
      end loop;

      if not Is_Empty (Simple_Expr_List) then
         Set_Loc (Node_Id (Simple_Expr_List),
               Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                       (Simple_Expr_List)));
      else
         DPE (PC_Simple_Expressions, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Simple_Expr_Node := Add_New_Simple_Expression (Start_Loc, Container,
                                                     Simple_Expr_List);

      if No (Simple_Expr_Node) then
         DPE (PC_Simple_Expression, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Simple_Expr_Node;
      end if;

   end P_Simple_Expression;

   ------------
   -- P_Term --
   ------------

   --  term ::= factor {multiplying_operator factor}*

   function P_Term (Container : Types.Node_Id) return Node_Id is
      Start_Loc     : Location;
      Loc           : Location;
      Term_Node     : Node_Id;
      Factor_Node   : Node_Id;
      Operator_Node : Node_Id;
      Factor_List   : List_Id;
      First_Parsing : Boolean := True;
   begin
      Save_Lexer (Start_Loc);
      Factor_List := New_List (K_List_Id, Token_Location);

      loop
         if not First_Parsing then
            Save_Lexer (Loc);
            Scan_Token;
            if Token in BA_Multiplying_Operator then
               Operator_Node := P_Operator (No_Node);
               if Present (Operator_Node) then
                  Append_Node_To_List (Operator_Node, Factor_List);
               else
                  DPE (PC_Term, EMC_Failed);
                  Factor_List := No_List;
                  exit;
               end if;
            else
               Restore_Lexer (Loc);
               exit;
            end if;
         else
            First_Parsing := False;
         end if;

         Factor_Node := P_Factor (No_Node);
         if Present (Factor_Node) then
            Append_Node_To_List (Factor_Node, Factor_List);
         else
            DPE (PC_Term, EMC_Failed);
            Factor_List := No_List;
            exit;
         end if;

      end loop;

      if not Is_Empty (Factor_List) then
         Set_Loc (Node_Id (Factor_List),
               Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                       (Factor_List)));
      else
         DPE (PC_Term, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Term_Node := Add_New_Term (Start_Loc, Container, Factor_List);
      if No (Term_Node) then
         DPE (PC_Term, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Term_Node;
      end if;

   end P_Term;

   --------------
   -- P_Factor --
   --------------

   --  factor ::= value [** value] | abs value | not value

   function P_Factor (Container : Types.Node_Id) return Node_Id is
      Start_Loc   : Location;
      Loc         : Location;
      Factor_Node : Node_Id;
      Low_Value : Node_Id;
      Upp_Value : Node_Id  := No_Node;
      Is_Abs_Bool : Boolean  := False;
      Is_Not_Bool : Boolean  := False;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      if Token = T_Abs then
         Is_Abs_Bool := True;
      elsif Token = T_Not then
         Is_Not_Bool := True;
      else
         Restore_Lexer (Start_Loc);
      end if;

      Low_Value := P_Value (No_Node);
      if No (Low_Value) then
         DPE (PC_Factor, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Exponent then
         Upp_Value := P_Value (No_Node);
         if No (Upp_Value) then
            DPE (PC_Factor, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Factor_Node := Add_New_Factor (Start_Loc, Container,
                                     Is_Abs_Bool, Is_Not_Bool,
                                     Low_Value, Upp_Value);
      if No (Factor_Node) then
         DPE (PC_Factor, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Factor_Node;
      end if;

   end P_Factor;

   -------------
   -- P_Value --
   -------------

   --  value ::= value_variable | value_constant | ( value_expression )

   function P_Value (Container : Types.Node_Id) return Node_Id is
      Loc  : Location;
      Node : Node_Id;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_False =>
            Node := New_Node (K_Boolean_Literal, Token_Location);
            Set_Is_True (Node, False);

         when T_True =>
            Node := New_Node (K_Boolean_Literal, Token_Location);
            Set_Is_True (Node, True);

         when T_Real_Literal =>
            Node := New_Node (K_Literal, Token_Location);
            Set_Value (Node,
                       New_Real_Value (Float_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));

         when T_Integer_Literal =>
            Node := New_Node (K_Literal, Token_Location);
            Set_Value (Node,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));
         when T_String_Literal =>
            Node := New_Node (K_Literal, Token_Location);
            Set_Value (Node,
                       New_String_Value (String_Literal_Value));

         when T_Identifier =>
            Scan_Token;
            if Token = T_Tick
              or else Token = T_Interrogative
              or else Token = T_Dot
              or else Token = T_Left_Square_Bracket
            then
               Restore_Lexer (Loc);
               Node := P_Value_Variable (Container);
            elsif Token = T_Number_Sign
            then
               Restore_Lexer (Loc);
               Node := P_Property_Ref (Container);
            else
               Restore_Lexer (Loc);
               Node := P_Property_Constant (Container);
            end if;

         when T_Number_Sign =>
               Restore_Lexer (Loc);
               Node := P_Property_Ref (Container);

         when T_Self =>
               Restore_Lexer (Loc);
               Node := P_Property_Ref (Container);

         when T_Left_Parenthesis =>
            Node := P_Value_Expression (Container);
            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (PC_Value, Expected_Token => T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when others =>
            DPE (PC_Value,
                 Expected_Tokens => (T_False, T_True, T_Identifier,
                                     T_Real_Literal, T_Integer_Literal,
                                     T_String_Literal));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      if No (Node) then
         DPE (PC_Value, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Node;
      end if;

   end P_Value;

   ----------------
   -- P_Operator --
   ----------------

   function P_Operator (Container : Types.Node_Id) return Node_Id is
      Operator_Cat  : Operator_Kind;
      Operator_Node : Node_Id;
      Loc           : Location;

      --  fixme : todo add generic procedure for test operator kind
   begin
      Save_Lexer (Loc);

      case Token is
         --  logical operator
         when T_And =>
            Operator_Cat := OK_And;
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Then then
               Operator_Cat := OK_And_Then;
            else
               Restore_Lexer (Loc);
            end if;

         when T_Or =>
            Operator_Cat := OK_Or;
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Else then
               Operator_Cat := OK_Or_Else;
            else
               Restore_Lexer (Loc);
            end if;

         when T_Xor =>
            Operator_Cat := OK_Xor;

         --  relational operator
         when T_Equals_Sign =>
            Operator_Cat := OK_Equal;

         when T_Non_Equal =>
            Operator_Cat := OK_Non_Equal;

         when T_Less_Than_Sign =>
            Operator_Cat := OK_Less_Than;

         when T_Greater_Than_Sign =>
            Operator_Cat := OK_Greater_Than;

         when T_Greater_Or_Equal =>
            Operator_Cat := OK_Greater_Or_Equal;

         when T_Less_Or_Equal =>
            Operator_Cat := OK_Less_Or_Equal;

         --  unary and Binary adding operator
         when T_Plus =>
            Operator_Cat := OK_Plus;

         when T_Minus =>
            Operator_Cat := OK_Minus;

         --  multiplying operator
         when T_Multiply =>
            Operator_Cat := OK_Multiply;

         when T_Divide =>
            Operator_Cat := OK_Divide;

         when T_Mod =>
            Operator_Cat := Ok_Mod;

         when T_Rem =>
            Operator_Cat := Ok_Rem;

         --  highest precedence operator
         when T_Exponent =>
            Operator_Cat := Ok_Exponent;

         when T_Abs =>
            Operator_Cat := Ok_Abs;

         when T_Not =>
            Operator_Cat := Ok_Not;

         when others =>
            Restore_Lexer (Loc);
            DPE (PC_Operator, EMC_Operator_Unknown);
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Operator_Node := Add_New_Operator (Loc, Container, Operator_Cat);

      if No (Operator_Node) then
         DPE (PC_Operator, EMC_Failed);
         return No_Node;
      end if;

      return Operator_Node;

   end P_Operator;

   -------------------------
   -- P_Property_Constant --
   -------------------------

   --  property_constant ::=
   --    [ property_set_identifier :: ] property_constant_identifier

   function P_Property_Constant (Container : Types.Node_Id) return Node_Id is
      Start_Loc       : Location;
      Loc             : Location;

      Property_Cst_Node : Node_Id;
      Property_Cst_Id   : Node_Id;
      Property_Set_Id   : Node_Id := No_Node;
   begin
      Save_Lexer (Start_Loc);

      Property_Cst_Id := P_Identifier (No_Node);
      if No (Property_Cst_Id) then
         Scan_Token;
         DPE (PC_Property_Constant, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Colon_Colon then
         Property_Set_Id := Property_Cst_Id;
         Property_Cst_Id := P_Identifier (No_Node);
         if No (Property_Cst_Id) then
            Scan_Token;
            DPE (PC_Property_Constant, Expected_Token => T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Property_Cst_Node := Add_New_Property_Constant (Start_Loc, Container,
                                                      Property_Set_Id,
                                                      Property_Cst_Id);
      if No (Property_Cst_Node) then
         DPE (PC_Property_Constant, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Property_Cst_Node;
      end if;
   end P_Property_Constant;

   --------------------
   -- P_Property_Ref --
   --------------------

   --  property_reference ::=
   --  # [ property_set_identifier :: ] property_name
   --  | component_element_reference # property_name
   --  | unique_component_classifier_reference # property_name

   function P_Property_Ref (Container : Types.Node_Id) return Node_Id is
      Start_Loc       : Location;
      Loc             : Location;
      Loc1            : Location;
      Property_Ref : Node_Id;
      Ident   : Node_Id;
      Ident1  : Node_Id := No_Node;
      Property_Set_Id   : Node_Id := No_Node;
      Property_Name : Node_Id;

      --  fixme : todo parse unique_component_classifier_reference
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;
      if Token = T_Number_Sign then
         Save_Lexer (Loc1);
         Ident := P_Identifier (No_Node);
         --  if No (Ident) then
         --   DPE (PC_Property_Reference, Expected_Token => T_Identifier);
         --  Skip_Tokens (T_Semicolon);
         --   return No_Node;
         --  end if;
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Colon_Colon then
            Property_Set_Id := Ident;
            Property_Name := P_Property_Name (No_Node);
            if No (Property_Name) then
               DPE (PC_Property_Reference, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc1);
            Property_Name := P_Property_Name (No_Node);
            if No (Property_Name) then
               DPE (PC_Property_Reference, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         end if;
      else
         Restore_Lexer (Start_Loc);
         Ident1 := P_Component_Element_Ref (No_Node);
         if No (Ident1) then
            DPE (PC_Property_Reference, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
         Scan_Token;
         if Token /= T_Number_Sign then
            DPE (PC_Property_Reference,
              Expected_Token => T_Number_Sign);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
         Save_Lexer (Loc);
         Property_Name := P_Property_Name (No_Node);
         if No (Property_Name) then
            DPE (PC_Property_Reference, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      Property_Ref := Add_New_Property_Reference (Start_Loc, Container,
                                                      Property_Set_Id,
                                                      Ident1,
                                                      Property_Name);
      if No (Property_Ref) then
         DPE (PC_Property_Reference, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Property_Ref;
      end if;
   end P_Property_Ref;

   -----------------------------
   -- P_Component_Element_Ref --
   -----------------------------

   --  component_element_reference ::=
   --   subcomponent_identifier
   --  | local_variable_identifier
   --  | bound_prototype_identifier
   --  | feature_identifier
   --  | self

   function P_Component_Element_Ref (Container : Types.Node_Id) return Node_Id
   is
      Start_Loc    : Location;
      Ident        : Node_Id := No_Node;
      IS_Self_Bool : Boolean := False;
      Component_Element_Ref : Node_Id;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;
      if Token = T_Self then
         IS_Self_Bool := True;
      else
         Restore_Lexer (Start_Loc);
         Ident := P_Identifier (No_Node);
         if No (Ident) then
            DPE (PC_Component_Element_Ref, Expected_Token => T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;
      Component_Element_Ref := Add_New_Component_Element_Ref
                               (Start_Loc, Container,
                                Ident, IS_Self_Bool);
      if No (Component_Element_Ref) then
         DPE (PC_Component_Element_Ref, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Component_Element_Ref;
      end if;

   end P_Component_Element_Ref;

   ---------------------
   -- P_Property_Name --
   ---------------------

   --  property_name ::=
   --   property_identifier { property_field }*

   function P_Property_Name (Container : Types.Node_Id) return Node_Id is
      Start_Loc        : Location;
      Loc              : Location;
      Property_Name_Node        : Node_Id;
      Property_Idt           : Node_Id;
      Property_Field_List : List_Id  := No_List;
   begin
      Save_Lexer (Start_Loc);

      Property_Idt := P_Identifier (No_Node);
      if No (Property_Idt) then
         Scan_Token;
         DPE (PC_Property_Name, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Square_Bracket
        or else Token = T_Dot
      then
         Restore_Lexer (Loc);
         Property_Field_List := P_Items_List (P_Property_Field'Access,
                                              No_Node,
                                              PC_Property_Name);
         if Is_Empty (Property_Field_List) then
            --  DPE (PC_Property_Name, EMC_List_Is_Empty);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Property_Name_Node := Add_New_Property_Name (Start_Loc, Container,
                                                   Property_Idt,
                                                   Property_Field_List);
      if No (Property_Name_Node) then
         DPE (PC_Property_Name, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Property_Name_Node;
      end if;

   end P_Property_Name;

   ----------------------
   -- P_Property_Field --
   ----------------------

   --  property_field ::=
   --    [ integer_value ]
   --  | . field_identifier
   --  | . upper_bound
   --  | . lower_bound

   function P_Property_Field (Container : Types.Node_Id) return Node_Id is
      Loc              : Location;
      Loc1             : Location;
      Property_Field : Node_Id;
      Ident               : Node_Id := No_Node;
      Is_Upper_Bound : Boolean := False;
      Is_Lower_Bound : Boolean := False;
   begin
      Save_Lexer (Loc);

      Scan_Token;
      if Token = T_Left_Square_Bracket then
         Ident := P_Integer_Value (Container);
         if No (Ident) then
            Skip_Tokens (T_Semicolon);
            DPE (PC_Property_Field, EMC_Failed);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Right_Square_Bracket then
            DPE (PC_Property_Field, Expected_Token => T_Right_Square_Bracket);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      elsif Token = T_Dot then
         Save_Lexer (Loc1);
         Scan_Token;
         if Token = T_Upper_Bound then
            IS_Upper_Bound := True;
         elsif Token = T_Lower_Bound then
            IS_Lower_Bound := True;
         elsif Token = T_Identifier then
            Restore_Lexer (Loc1);
            Ident := P_Identifier (No_Node);
         else
            DPE (PC_Property_Field, Expected_Tokens => (T_Identifier,
                                                        T_Upper_Bound,
                                                        T_Lower_Bound));
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;
      Property_Field := Add_New_Property_Field (Loc, Container,
                                              Ident, Is_Upper_Bound,
                                              Is_Lower_Bound);
      if No (Property_Field) then
         DPE (PC_Property_Field, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Property_Field;
      end if;
   end P_Property_Field;

   ---------------------
   -- P_Integer_Range --
   ---------------------

   --  integer_range ::= integer_value .. integer_value

   function P_Integer_Range (Container : Types.Node_Id) return Node_Id is
      Start_Loc     : Location;
      Integer_Range : Node_Id;
      Lower_Bound   : Node_Id;
      Upper_Bound   : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Lower_Bound := P_Integer_Value (No_Node);
      if No (Lower_Bound) then
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Interval then
         DPE (PC_Integer_Range, Expected_Token => T_Interval);
         return No_Node;
      end if;

      Upper_Bound := P_Integer_Value (No_Node);

      if No (Upper_Bound) then
         return No_Node;
      end if;

      Integer_Range := Add_New_Integer_Range (Start_Loc, Container,
                                              Lower_Bound, Upper_Bound);
      if No (Integer_Range) then
         DPE (PC_Integer_Range, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Integer_Range;
      end if;

   end P_Integer_Range;

   ---------------------
   -- P_Integer_Value --
   ---------------------

   --  integer_value ::=
   --    integer_value_variable
   --  | integer_numerical_literal
   --  | integer_property_constant

   function P_Integer_Value (Container : Types.Node_Id) return Node_Id is
      Start_Loc   : Location;

      Integer_Cst  : Node_Id;
      Integer_Val  : Node_Id;

   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      case Token is
         when T_Identifier =>
            Scan_Token;
            if Token = T_Tick
              or else Token = T_Interrogative
              or else Token = T_Dot
              or else Token = T_Left_Square_Bracket
            then
               Restore_Lexer (Start_Loc);
               Integer_Cst := P_Value_Variable (Container);
            else
               Restore_Lexer (Start_Loc);
               Integer_Cst := P_Property_Constant (Container);
            end if;

         when T_Real_Literal =>
            Integer_Cst := New_Node (K_Literal, Token_Location);
            Set_Value (Integer_Cst,
                       New_Real_Value (Float_Literal_Value,
                                       False,
                                       Numeric_Literal_Base,
                                       Numeric_Literal_Exp));

         when T_Integer_Literal =>
            Integer_Cst := New_Node (K_Literal, Token_Location);
            Set_Value (Integer_Cst,
                       New_Integer_Value (Integer_Literal_Value,
                                          False,
                                          Numeric_Literal_Base,
                                          Numeric_Literal_Exp));

         when others =>
            DPE (PC_Integer_Value,
                 Expected_Tokens => (T_Identifier,
                                     T_Real_Literal,
                                     T_Integer_Literal));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Integer_Val := Add_New_Integer_Value (Start_Loc, Container, Integer_Cst);
      if No (Integer_Val) then
         DPE (PC_Integer_Value, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Integer_Val;
      end if;

   end P_Integer_Value;

   ---------------------
   -- P_Behavior_Time --
   ---------------------

   --  behavior_time ::= integer_value unit_identifier

   function P_Behavior_Time (Container : Types.Node_Id) return Node_Id is
      Start_Loc     : Location;
      Behavior_Time : Node_Id;
      Integer_Val   : Node_Id;
      Unit_Id       : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Integer_Val := P_Integer_Value (No_Node);
      if No (Integer_Val) then
         DPE (PC_Behavior_Time, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Unit_Id := P_Identifier (No_Node);
      if No (Unit_Id) then
         Scan_Token;
         DPE (PC_Behavior_Time, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Behavior_Time := Add_New_Behavior_Time (Start_Loc, Container,
                                              Integer_Val, Unit_Id);
      if No (Behavior_Time) then
         DPE (PC_Behavior_Time, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Behavior_Time;
      end if;
   end P_Behavior_Time;

end Ocarina.FE_AADL_BA.Parser.Expressions;
