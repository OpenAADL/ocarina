------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.FE_AADL.PARSER.PROPERTIES.VALUES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.AADL_Values;
with Ocarina.FE_AADL.Lexer;
with Ocarina.ME_AADL.Tokens;
with Ocarina.FE_AADL.Parser.Identifiers;
with Ocarina.FE_AADL.Parser.Components;
with Ocarina.FE_AADL.Parser.Annexes;
with Ocarina.FE_AADL.Parser.Components.Arrays;

with Ocarina.Builder.AADL.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities; use Ocarina.ME_AADL;
use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
use Ocarina.ME_AADL.AADL_Tree.Entities;

package body Ocarina.FE_AADL.Parser.Properties.Values is

   function P_Or_Boolean_Term return Node_Id;
   --  ATTENTION: This is the entry point for parsing boolean
   --  expression since OR is the operator with the lowest precedence.

   function P_Or_Boolean_Term_Aux (Bool_Term : Node_Id) return Node_Id;
   --  Bool_Term is the previous parsed And_Boolean_Term

   function P_And_Boolean_Term return Node_Id;

   function P_And_Boolean_Term_Aux (Bool_Term : Node_Id) return Node_Id;
   --  Bool_Term is the previous parsed Boolean_Term

   function P_Boolean_Term return Node_Id;
   --  ATTENTION: This is not the entry point to parse boolean
   --  expression. The entry point is P_Or_Boolean term, the lowest
   --  precedence operator expression.

   function P_Boolean_Or_Record_Term return Node_Id;
   --  Match if we parse Boolean_Term or Record_Term with '('

   function P_Classifier_Category (Container : Node_Id) return Node_Id;
   --  Current token must be component category

   function P_Classifier_Type return Node_Id;
   --  Current token must be 'classifier'

   function P_Component_Classifier_Term return Node_Id;
   --  Current token must be the first token of component_category

   function P_Constant_Property_Value (Container : Node_Id) return Node_Id;

   function P_Property_Type return Node_Id;

   function P_Enumeration_Type return Node_Id;
   --  Current token must be 'enumeration'

   function P_Number_Type return Node_Id;
   --  Current token must be 'aadlreal' or 'aadlinteger'

   function P_Minus_Numeric_Term (Code : Parsing_Code) return Node_Id;

   function P_Number_Range (Number_Cat : Number_Category) return Node_Id;
   --  If Number_Cat = NC_Real then parse Real_Range. If Number_Cat =
   --  NC_Integer then parse Integer_Range. If Number_Cat = NC_Unknown
   --  then parse Real_Range or Integer_Range.

   function P_Property_Type_Designator return Node_Id;

   function P_Signed_Number_Or_Range (First_Term : Node_Id) return Node_Id;
   --  Parse Signed_AADLInteger or Signed_AADLReal or
   --  Integer_Range_Term or Real_Range_Term. This function is used to
   --  factorize codes for parsing Property_Expression and Single_ /
   --  Multi_Valued_Property_Constant.

   function P_Computed_Term return Node_Id;
   --  Current token must be 'compute'

   function P_Record_Type return Node_Id;
   --  Parse Record_Type, current token must be 'record'

   function P_Record_Type_Element (Container : Node_Id) return Node_Id;
   --  Parse Element of Record_Type

   function P_Record_Term return Node_Id;
   --  Current token must be '('

   function P_Record_Term_Element (Container : Node_Id) return Node_Id;

   function P_Reference_Term return Node_Id;
   --  Current token must be 'component'

   function P_Range_Type return Node_Id;
   --  Current token must be 'range'

   function P_Referable_Element_Category (Container : Node_Id) return Node_Id;

   function P_Reference_Category (Container : Node_Id) return Node_Id;

   function P_Reference_Type return Node_Id;
   --  Current token must be 'reference'

   function P_Unit_Definition (Container : Node_Id) return Node_Id;

   function P_Units_Type return Node_Id;
   --  Curent token must be 'units'

   function P_Contained_Element (Container : Node_Id) return Node_Id;

   function P_Named_Element_Component return Named_Element;

   function P_Named_Element_Component_With_Access
     (Component_Cat : Component_Category) return Named_Element;

   function P_Named_Element_Feature return Named_Element;
   --  Current token must be 'feature'

   function P_Named_Element_Mode return Named_Element;
   --  Current token must be 'mode'

   function P_Named_Element_Flow return Named_Element;
   --  Current token must be 'flow'

   function P_Named_Element_End_To_End_Flow return Named_Element;
   --  Current token must be 'end'

   function P_Named_Element_Identifier return Named_Element;
   --  Current token must be 'identifier'

   function P_Named_Element_Port_Or_Access_Or_Event return Named_Element;
   --  Current token must be 'port', 'access' or 'event'

   ------------------------
   -- P_And_Boolean_Term --
   ------------------------

   --  boolean_term [ and boolean_term ]

   function P_And_Boolean_Term return Node_Id is
      Bool_Term : Node_Id;

   begin
      Bool_Term := P_Boolean_Term;

      if No (Bool_Term) then
         return No_Node;
      else
         return P_And_Boolean_Term_Aux (Bool_Term);
      end if;
   end P_And_Boolean_Term;

   ----------------------------
   -- P_And_Boolean_Term_Aux --
   ----------------------------

   --  [ and boolean_term ]

   function P_And_Boolean_Term_Aux (Bool_Term : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      And_Term         : Node_Id;
      Second_Bool_Term : Node_Id;
      Loc              : Location;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_And then
         Second_Bool_Term := P_Boolean_Term;

         if No (Second_Bool_Term) then
            return No_Node;
         else
            And_Term :=
              New_Node
                (K_And_Boolean_Term,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Bool_Term));
            Set_First_Term (And_Term, Bool_Term);
            Set_Second_Term (And_Term, Second_Bool_Term);

            return P_And_Boolean_Term_Aux (And_Term);
         end if;
      else
         Restore_Lexer (Loc);
         return Bool_Term;
      end if;
   end P_And_Boolean_Term_Aux;

   --------------------
   -- P_Boolean_Term --
   --------------------

   --  boolean_term ::=
   --     boolean_value
   --   | boolean_property_constant_term
   --   | not boolean_term
   --   | boolean_term and boolean_term
   --   | boolean_term or boolean_term
   --   | ( boolean_term )

   --  boolean_value ::= true | false

   function P_Boolean_Term return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.AADL_Values;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Bool_Term : Node_Id;     --  output
      Temp_Node : Node_Id;     --  used to save temporary result
      Loc       : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      case Token is
         when T_True =>
            Bool_Term := New_Node (K_Literal, Token_Location);
            Set_Value (Bool_Term, New_Boolean_Value (True));
            return Bool_Term;

         when T_False =>
            Bool_Term := New_Node (K_Literal, Token_Location);
            Set_Value (Bool_Term, New_Boolean_Value (False));
            return Bool_Term;

         when T_Value =>
            --  This token had been disabled in AADLv2

            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (PC_Boolean_Term, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Bool_Term :=
              P_Unique_Property_Identifier_Or_Term (PC_Boolean_Property_Term);

            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (PC_Boolean_Term, T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            if No (Bool_Term) then
               return No_Node;
            else
               return Bool_Term;
            end if;

         when T_Identifier =>

            if AADL_Version = AADL_V2 then
               Restore_Lexer (Loc);
               Bool_Term :=
                 P_Unique_Property_Identifier_Or_Term
                   (PC_Boolean_Property_Term);

               if No (Bool_Term) then
                  return No_Node;
               else
                  return Bool_Term;
               end if;
            else
               DPE (PC_Boolean_Term, EMC_Not_Allowed_In_AADL_V1, T_Identifier);
               return No_Node;
            end if;

         when T_Not =>
            Save_Lexer (Loc);
            Temp_Node := P_Boolean_Term;
            if No (Temp_Node) then
               return No_Node;
            else
               Bool_Term := New_Node (K_Not_Boolean_Term, Loc);
               Set_Boolean_Term (Bool_Term, Temp_Node);
               return Bool_Term;
            end if;

         when T_Left_Parenthesis =>
            Save_Lexer (Loc);
            Temp_Node := P_Or_Boolean_Term;
            if No (Temp_Node) then
               Skip_Tokens (T_Right_Parenthesis);
               return No_Node;
            else
               Scan_Token;
               if Token /= T_Right_Parenthesis then
                  DPE (PC_Boolean_Term, T_Right_Parenthesis);
                  return No_Node;
               else
                  Bool_Term := New_Node (K_Parenthesis_Boolean_Term, Loc);
                  Set_Boolean_Term (Bool_Term, Temp_Node);
                  return Bool_Term;
               end if;
            end if;

         when others =>
            DPE
              (PC_Boolean_Term,
               (T_True, T_False, T_Value, T_Not, T_Left_Parenthesis));
            return No_Node;
      end case;
   end P_Boolean_Term;

   ---------------------------
   -- P_Classifier_Category --
   ---------------------------

   --  AADL_V2
   --  classifier_category_reference ::= named_element_identifier

   function P_Classifier_Category (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Unreferenced (Container);

      Classifier_Cat : Node_Id;
   begin

      Classifier_Cat := P_Named_Element;

      if Classifier_Cat /= No_Node then
         Set_Kind (Classifier_Cat, K_Classifier_Category_Ref);
      else
         return No_Node;
      end if;

      return Classifier_Cat;
   end P_Classifier_Category;

   -----------------------
   -- P_Classifier_Type --
   -----------------------

   --  AADL_V1
   --  classifier_type ::=
   --     classifier [ ( component_category { , component_category }* ) ]

   --  AADL_V2
   --  classifier_type ::=
   --     classifier [ ( classifier_category_reference
   --                    { , classifier_category_reference }* ) ]

   function P_Classifier_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Components;

      Class_List : List_Id;
      Class_Type : Node_Id;
      Start_Loc  : Location;

   begin
      Save_Lexer (Start_Loc);
      Scan_Token;
      if Token /= T_Left_Parenthesis then
         --  classifier_type is empty

         Restore_Lexer (Start_Loc);
      else
         case AADL_Version is
            when AADL_V1 =>
               Class_List :=
                 P_Items_List
                   (P_Component_Category'Access,
                    No_Node,
                    T_Comma,
                    T_Right_Parenthesis,
                    PC_Classifier_Type);

            when AADL_V2 =>
               Class_List :=
                 P_Items_List
                   (P_Classifier_Category'Access,
                    No_Node,
                    T_Comma,
                    T_Right_Parenthesis,
                    PC_Classifier_Type);
         end case;

         if No (Class_List) then
            --  error when parsing classifier elements, quit

            return No_Node;
         end if;
      end if;

      Class_Type := New_Node (K_Classifier_Type, Start_Loc);
      Set_List_Items (Class_Type, Class_List);

      return Class_Type;
   end P_Classifier_Type;

   ---------------------------------
   -- P_Component_Classifier_Term --
   ---------------------------------

   --  AADL_V1
   --  component_classifier_term ::=
   --     component_category [ unique_component_type_identifier
   --                          [ . component_implementation_identifier ] ]

   --  AADL_V2
   --  component_classifier_term ::=
   --     ( classifier ( Unique_Component_Type_Reference
   --                  | Unique_Component_Implementation_Reference ) )

   function P_Component_Classifier_Term return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Components;

      Class_Term : Node_Id;
      Category   : Component_Category;
      Start_Loc  : Location;
      Loc        : Location;

   begin
      Save_Lexer (Start_Loc);
      case AADL_Version is
         when AADL_V1 =>
            Category := P_Component_Category;

         when AADL_V2 =>
            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (PC_Component_Classifier_Term, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
      end case;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Identifier then
         Restore_Lexer (Loc);
         Class_Term := P_Entity_Reference (PC_Component_Classifier_Term);

         if Class_Term = No_Node then
            return No_Node;
         else
            Set_Kind (Class_Term, K_Component_Classifier_Term);
         end if;
      else
         Class_Term := New_Node (K_Component_Classifier_Term, Start_Loc);
         Restore_Lexer (Loc);
      end if;

      if AADL_Version = AADL_V2 then
         Scan_Token;
         if Token /= T_Right_Parenthesis then
            DPE (PC_Component_Classifier_Term, T_Right_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      if AADL_Version = AADL_V1 then
         Set_Component_Cat (Class_Term, Component_Category'Pos (Category));
      end if;

      return Class_Term;
   end P_Component_Classifier_Term;

   -------------------------------
   -- P_Constant_Property_Value --
   -------------------------------

   function P_Constant_Property_Value (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.AADL_Values;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      pragma Unreferenced (Container);

      Const : Node_Id;     --  output
      Loc   : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_String_Literal | T_True | T_False =>
            Const := New_Node (K_Literal, Token_Location);

            if Token = T_String_Literal then
               Set_Value (Const, New_String_Value (String_Literal_Value));
            elsif Token = T_True then
               Set_Value (Const, New_Boolean_Value (True));
            else
               Set_Value (Const, New_Boolean_Value (False));
            end if;

            return Const;

         when T_Identifier =>
            Const := New_Node (K_Literal, Token_Location);
            Set_Value (Const, New_Enum_Value (Token_Display_Name));
            return Const;

         when T_Plus | T_Minus | T_Integer_Literal | T_Real_Literal =>
            Restore_Lexer (Loc);
            Const := P_Numeric_Term (PC_Constant_Property_Value);

            if Present (Const) then
               --  Signed_AADLNumber was parsed successfully,
               --  try to determine Number_Term or Range_Term
               return P_Signed_Number_Or_Range (Const);
            else
               return No_Node;
            end if;

         when others =>
            DPE (PC_Constant_Property_Value);
            Restore_Lexer (Loc);
            return No_Node;
      end case;
   end P_Constant_Property_Value;

   ------------------------
   -- P_Enumeration_Type --
   ------------------------

   --  enumeration_type ::=
   --     enumeration ( defining_enumeration_literal_identifier
   --                     { , defining_enumeration_literal_identifier }* )

   function P_Enumeration_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Enum  : Node_Id;
      Items : List_Id;
      Item  : Node_Id;
   begin
      Enum := New_Node (K_Enumeration_Type, Token_Location);
      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_Enumeration_Type, T_Left_Parenthesis);
         return No_Node;
      end if;

      Items := P_Items_List (P_Identifier'Access, No_Node, T_Comma);
      if No (Items) then
         DPE (PC_Enumeration_Type, T_Identifier);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Parenthesis then
         DPE (PC_Enumeration_Type, T_Right_Parenthesis);
         return No_Node;
      end if;

      Set_Identifiers (Enum, Items);

      --  add enumeration_literal_identifiers to current_context

      Item := First_Node (Items);
      while Present (Item) loop
         Set_Corresponding_Entity (Item, Enum);

         Item := Next_Node (Item);
      end loop;

      return Enum;
   end P_Enumeration_Type;

   -----------------------------
   -- P_Multi_Valued_Property --
   -----------------------------

   --  multi_valued_property ::= (list of)+ property_type_designator
   --     [ => ( [ default_property_expression
   --                 { , default_property_expression }* ] ) ]

   function P_Multi_Valued_Property return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Valued_Property          : Node_Id;  --  output;
      Property_Type_Designator : Node_Id;
      Property_Expressions     : List_Id;
      Loc                      : Location;
   begin
      Valued_Property := New_Node (K_Multi_Valued_Property, Token_Location);

      Scan_Token;
      if Token /= T_Of then
         DPE (PC_Multi_Valued_Property, T_Of);
         return No_Node;
      end if;

      Property_Type_Designator := P_Property_Type_Designator;
      if No (Property_Type_Designator) then
         --  error when parsing Property_Type_Designator, quit
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Association then
         Scan_Token;
         if Token /= T_Left_Parenthesis then
            DPE (PC_Multi_Valued_Property, T_Left_Parenthesis);
            return No_Node;
         end if;

         Save_Lexer (Loc);
         Scan_Token;
         if Token /= T_Right_Parenthesis then
            Restore_Lexer (Loc);
            Property_Expressions :=
              P_Items_List
                (Func      => P_Property_Expression'Access,
                 Container => No_Node,
                 Separator => T_Comma,
                 Delimiter => T_Right_Parenthesis,
                 Code      => PC_Multi_Valued_Property);
         else
            Property_Expressions := New_List (K_List_Id, Token_Location);
         end if;
      else
         Restore_Lexer (Loc);
         Property_Expressions := No_List;
      end if;

      Set_Property_Type_Designator (Valued_Property, Property_Type_Designator);
      Set_Property_Expressions (Valued_Property, Property_Expressions);
      return Valued_Property;
   end P_Multi_Valued_Property;

   --------------------
   -- P_Numeric_Term --
   --------------------

   --  signed_aadlreal_or_constant ::=
   --      ( signed_aadlreal | [ sign ] real_property_constant_term )

   --  signed_aadlinteger_or_constant ::=
   --      ( signed_aadlinteger | [ sign ] integer_property_constant_term )

   --  sign ::= + | -

   --  signed_aadlinteger ::=
   --      [ sign ] integer_literal  [ unit_identifier ]

   --  signed_aadlreal ::=
   --      [ sign ] real_literal [ unit_identifier ]

   function P_Numeric_Term (Code : Parsing_Code) return Node_Id is
      use Ocarina.FE_AADL.Lexer;
      use Ocarina.ME_AADL.Tokens;

      Start_Loc : Location;
      Loc       : Location;
      Term      : Node_Id;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      case Token is
         when T_Minus =>
            return P_Minus_Numeric_Term (Code);

         when T_Plus =>
            Save_Lexer (Loc);
            Scan_Token;

            case Token is
               when T_Real_Literal | T_Integer_Literal =>

                  Restore_Lexer (Start_Loc);
                  return P_Signed_AADLNumber (NC_Unknown, Code);

               when T_Value =>
                  --  This token had been disabled in AADLv2

                  Scan_Token;
                  if Token /= T_Left_Parenthesis then
                     DPE (Code, T_Left_Parenthesis);
                     Skip_Tokens (T_Semicolon);
                     return No_Node;
                  end if;

                  Term := P_Unique_Property_Identifier_Or_Term (Code);

                  Scan_Token;
                  if Token /= T_Right_Parenthesis then
                     DPE (Code, T_Right_Parenthesis);
                     Skip_Tokens (T_Semicolon);
                     return No_Node;
                  end if;

                  return Term;

               when T_Identifier =>
                  if AADL_Version = AADL_V2 then
                     Restore_Lexer (Loc);
                     Term := P_Unique_Property_Identifier_Or_Term (Code);

                     return Term;
                  else
                     DPE (Code, EMC_Not_Allowed_In_AADL_V1, T_Identifier);
                     return No_Node;
                  end if;

               when others =>
                  DPE (Code);
                  return No_Node;
            end case;

         when T_Real_Literal | T_Integer_Literal =>
            Restore_Lexer (Start_Loc);
            return P_Signed_AADLNumber (NC_Unknown, Code);

         when T_Value =>
            --  This token had been disabled in AADLv2

            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (Code, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Term := P_Unique_Property_Identifier_Or_Term (Code);

            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (Code, T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            return Term;

         when T_Identifier =>
            if AADL_Version = AADL_V2 then
               Restore_Lexer (Start_Loc);
               Term := P_Unique_Property_Identifier_Or_Term (Code);

               return Term;
            else
               DPE (Code, EMC_Not_Allowed_In_AADL_V1, T_Identifier);
               return No_Node;
            end if;

         when others =>
            DPE (Code);
            return No_Node;
      end case;

   end P_Numeric_Term;

   --------------------------
   -- P_Minus_Numeric_Term --
   --------------------------

   function P_Minus_Numeric_Term (Code : Parsing_Code) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.FE_AADL.Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc          : Location;
      Numeric_Term : constant Node_Id :=
        New_Node (K_Minus_Numeric_Term, No_Location);
      Subterm : Node_Id;
   begin
      Save_Lexer (Loc);
      Set_Loc (Numeric_Term, Loc);
      Scan_Token;

      case Token is
         when T_Real_Literal | T_Integer_Literal =>
            Restore_Lexer (Loc);
            Subterm :=
              P_Signed_AADLNumber (NC_Unknown, PC_Property_Expression);

         when T_Value =>
            --  This token had been disabled in AADLv2
            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (Code, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Subterm :=
              P_Unique_Property_Identifier_Or_Term
                (PC_Unique_Property_Constant_Identifier);

            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (Code, T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Identifier =>

            if AADL_Version = AADL_V2 then
               Restore_Lexer (Loc);
               Subterm := P_Unique_Property_Identifier_Or_Term (Code);
            else
               DPE (Code, EMC_Not_Allowed_In_AADL_V1, T_Identifier);
            end if;

         when others =>
            DPE (Code);
            Subterm := No_Node;
      end case;

      if Subterm = No_Node then
         return No_Node;
      else
         Set_Numeric_Term (Numeric_Term, Subterm);
      end if;

      return Numeric_Term;
   end P_Minus_Numeric_Term;

   --------------------
   -- P_Number_Range --
   --------------------

   --  real_range       ::= real_lower_bound .. real_upper_bound
   --  real_lower_bound ::= signed_aadlreal
   --  real_upper_bound ::= signed_aadlreal

   --  integer_range       ::= integer_lower_bound .. integer_upper_bound
   --  integer_lower_bound ::= signed_aadlinteger
   --  integer_upper_bound ::= signed_aadlinteger

   function P_Number_Range (Number_Cat : Number_Category) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Number_Range : Node_Id;
      Lower_Bound  : Node_Id;
      Upper_Bound  : Node_Id;
      Code         : Parsing_Code;

   begin
      case Number_Cat is
         when NC_Real =>
            Code := PC_Real_Range;
         when NC_Integer =>
            Code := PC_Integer_Range;
         when NC_Unknown =>
            Code := PC_Number_Range;
      end case;

      Lower_Bound := P_Numeric_Term (Code);

      if No (Lower_Bound) then
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Interval then
         DPE (Code, T_Interval);
         return No_Node;
      end if;

      Upper_Bound := P_Numeric_Term (Code);

      if No (Upper_Bound) then
         return No_Node;
      end if;

      Number_Range := New_Node (K_Number_Range, Loc (Lower_Bound));
      Set_Lower_Bound (Number_Range, Lower_Bound);
      Set_Upper_Bound (Number_Range, Upper_Bound);

      return Number_Range;
   end P_Number_Range;

   -------------------
   -- P_Number_Type --
   -------------------

   --  number_type ::=
   --       aadlreal [ real_range ] [ units unit_designator ]
   --     | aadlinteger [ integer_range ] [ units unit_designator ]

   --  unit_designator ::=   units_unique_property_type_identifier
   --                      | units_list

   --  unique_property_type_identifier ::=
   --     [ property_set_identifier :: ] property_type_identifier

   function P_Number_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Number_Type     : Node_Id;
      Type_Range      : Node_Id;
      Unit_Designator : Node_Id;
      Number_Cat      : Number_Category;
      Loc             : Location;

   begin
      if Token = T_AADLReal then
         Number_Type := New_Node (K_Real_Type, Token_Location);
         Number_Cat  := NC_Real;
      else
         Number_Type := New_Node (K_Integer_Type, Token_Location);
         Number_Cat  := NC_Integer;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Real_Literal
        or else Token = T_Integer_Literal
        or else Token = T_Minus
        or else Token = T_Plus
        or else Token = T_Value

      then
         Restore_Lexer (Loc);
         Type_Range := P_Number_Range (Number_Cat);

         if No (Type_Range) then
            return No_Node;
         end if;
      else
         Type_Range := No_Node;
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Units then
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Left_Parenthesis then
            Restore_Lexer (Loc);
            Unit_Designator := P_Units_Type;

            if No (Unit_Designator) then
               return No_Node;
            end if;
         elsif Token = T_Identifier then
            Restore_Lexer (Loc);
            Unit_Designator :=
              P_Unique_Property_Identifier_Or_Term
                (PC_Unique_Property_Type_Identifier);

            if No (Unit_Designator) then
               return No_Node;
            end if;
         end if;
      else
         Unit_Designator := No_Node;
         Restore_Lexer (Loc);
      end if;

      Set_Type_Range (Number_Type, Type_Range);
      Set_Unit_Designator (Number_Type, Unit_Designator);
      return Number_Type;
   end P_Number_Type;

   -----------------------
   -- P_Or_Boolean_Term --
   -----------------------

   --  boolean_term [ and boolean_term ] [ or boolean_term ]

   --  We add the [ and boolean_term ] since this is the entry point
   --  for parsing boolean expressions.

   function P_Or_Boolean_Term return Node_Id is
      Bool_Term : Node_Id;

   begin
      Bool_Term := P_And_Boolean_Term;
      if No (Bool_Term) then
         return No_Node;
      else
         return P_Or_Boolean_Term_Aux (Bool_Term);
      end if;
   end P_Or_Boolean_Term;

   ---------------------------
   -- P_Or_Boolean_Term_Aux --
   ---------------------------

   --  [ or boolean_term ]

   function P_Or_Boolean_Term_Aux (Bool_Term : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Or_Term          : Node_Id;
      Second_Bool_Term : Node_Id;
      Loc              : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Or then
         Second_Bool_Term := P_And_Boolean_Term;
         if No (Second_Bool_Term) then
            return No_Node;
         else
            Or_Term :=
              New_Node
                (K_Or_Boolean_Term,
                 Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Bool_Term));
            Set_First_Term (Or_Term, Bool_Term);
            Set_Second_Term (Or_Term, Second_Bool_Term);

            return P_Or_Boolean_Term_Aux (Or_Term);
         end if;
      else
         Restore_Lexer (Loc);
         return Bool_Term;
      end if;
   end P_Or_Boolean_Term_Aux;

   -------------------------
   -- P_Property_Constant --
   -------------------------

   --  AADL_V1
   --  property_constant ::=   single_valued_property_constant
   --                        | multi_valued_property_constant

   --  single_valued_property_constant ::=
   --     defining_property_constant_identifier : constant
   --        ( ( aadlinteger
   --            | aadlreal ) [ units_unique_property_type_identifier ]
   --          | aadlstring | aadlboolean
   --          | enumeration_unique_property_type_identifier
   --          | integer_range_unique_property_type_identifier
   --          | real_range_unique_property_type_identifier
   --          | integer_unique_property_type_identifer
   --          | real_unique_property_type_identifer )
   --     => constant_property_value ;

   --  multi_valued_property_constant ::=
   --     defining_property_constant_identifier : constant (list of)+
   --        ( ( aadlinteger
   --            | aadlreal ) [ units_unique_property_type_identifier ]
   --          | aadlstring | aadlboolean
   --          | enumeration_unique_property_type_identifier
   --          | integer_range_unique_property_type_identifier
   --          | real_range_unique_property_type_identifier
   --          | integer_unique_property_type_identifer
   --          | real_unique_property_type_identifer )
   --       => ( [ constant_property_value { , constant_property_value }* ] ) ;

   --  AADL_V2
   --  property_constant ::=   single_valued_property_constant
   --                        | multi_valued_property_constant

   --  single_valued_property_constant ::=
   --     defining_property_constant_identifier : constant
   --         property_type_designator => constant_property_expression;

   --  multi_valued_property_constant ::=
   --     defining_property_constant_identifier : constant list of
   --         property_type_designator =>
   --                        ( [ constant_property_expression
   --                               { , constant_property_expression }* ] ) ;

   function P_Property_Constant
     (Identifier   : Node_Id;
      Property_Set : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.Builder.AADL.Properties;

      Property      : Node_Id;       --  output
      Constant_Type : Node_Id;
      Unit_Ident    : Node_Id := No_Node;
      --  used only for AADLInteger and AADLReal

      Property_Value  : Node_Id := No_Node; --  only for single_valued_property
      Property_Values : List_Id := No_List; --  only for multi_valued_property
      Code            : Parsing_Code;
      Loc             : Location;
      Multiplicity    : Int := 0;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_List then
         Code := PC_Multi_Valued_Property_Constant;
         Multiplicity := 1;
         Scan_Token;
         if Token /= T_Of then
            DPE (Code, T_Of);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         Code := PC_Single_Valued_Property_Constant;
      end if;

      if AADL_Version = AADL_V2
        and then Code = PC_Multi_Valued_Property_Constant
      then
         loop
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_List then
               Scan_Token;
               if Token /= T_Of then
                  DPE (Code, T_Of);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
               Multiplicity := Multiplicity + 1;
            else
               Restore_Lexer (Loc);
               exit;
            end if;
         end loop;
      end if;

      case AADL_Version is
         when AADL_V1 =>
            Save_Lexer (Loc);
            Scan_Token;
            case Token is
               when T_AADLInteger | T_AADLReal =>
                  if Token = T_AADLInteger then
                     Constant_Type :=
                       New_Node (K_Integer_Type, Token_Location);
                  else
                     Constant_Type := New_Node (K_Real_Type, Token_Location);
                  end if;

                  Set_Type_Range (Constant_Type, No_Node);
                  Set_Unit_Designator (Constant_Type, No_Node);

                  --  try to parse unit_identifier

                  Save_Lexer (Loc);
                  Scan_Token;

                  if Token = T_Identifier then
                     Restore_Lexer (Loc);
                     Unit_Ident :=
                       P_Unique_Property_Identifier_Or_Term
                         (PC_Unique_Property_Type_Identifier);

                     if No (Unit_Ident) then
                        Skip_Tokens (T_Semicolon);
                        return No_Node;
                     end if;

                  else
                     Restore_Lexer (Loc);
                  end if;

               when T_AADLString =>
                  Constant_Type := New_Node (K_String_Type, Token_Location);

               when T_AADLBoolean =>
                  Constant_Type := New_Node (K_Boolean_Type, Token_Location);

               when T_Identifier =>
                  --  parse xxxxxx_unique_property_type_identifier

                  Restore_Lexer (Loc);
                  Constant_Type :=
                    P_Unique_Property_Identifier_Or_Term
                      (PC_Unique_Property_Type_Identifier);

                  if No (Constant_Type) then
                     Skip_Tokens (T_Semicolon);
                     return No_Node;
                  end if;

               when others =>
                  if Code = PC_Single_Valued_Property_Constant then
                     DPE
                       (PC_Property_Constant,
                        (T_List,
                         T_AADLInteger,
                         T_AADLReal,
                         T_AADLString,
                         T_AADLBoolean,
                         T_Identifier));
                  else
                     DPE
                       (Code,
                        (T_AADLInteger,
                         T_AADLReal,
                         T_AADLString,
                         T_AADLBoolean,
                         T_Identifier));
                  end if;
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
            end case;

            Scan_Token;
            if Token /= T_Association then
               DPE (Code, T_Association);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            if Code = PC_Single_Valued_Property_Constant then
               Property_Value := P_Constant_Property_Value (No_Node);
               if No (Property_Value) then
                  --  error when parsing Constant_Property_Value, quit

                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            else
               Scan_Token;
               if Token /= T_Left_Parenthesis then
                  DPE (Code, T_Left_Parenthesis);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Save_Lexer (Loc);
               Scan_Token;
               if Token = T_Right_Parenthesis then
                  --  Constant_Property_Value list is empty, they are optional

                  Property_Values := New_List (K_List_Id, Token_Location);
               else
                  Restore_Lexer (Loc);
                  Property_Values :=
                    P_Items_List
                      (P_Constant_Property_Value'Access,
                       No_Node,
                       T_Comma,
                       T_Right_Parenthesis,
                       Code);

                  if No (Property_Values) then
                     --  error when parsing Constant_Property_Value list, quit

                     Skip_Tokens (T_Semicolon);
                     return No_Node;
                  end if;
               end if;
            end if;

         when AADL_V2 =>
            Constant_Type := P_Property_Type_Designator;

            if No (Constant_Type) then
               DPE (Code);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Scan_Token;
            if Token /= T_Association then
               DPE (Code, T_Association);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            if Code = PC_Single_Valued_Property_Constant then
               Property_Value := P_Property_Expression (No_Node);
               if No (Property_Value) then
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

            else
               Scan_Token;
               if Token /= T_Left_Parenthesis then
                  DPE (Code, T_Left_Parenthesis);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;

               Save_Lexer (Loc);
               Scan_Token;
               if Token = T_Right_Parenthesis then
                  --  list is empty, they are optional

                  Property_Values := New_List (K_List_Id, Token_Location);
               else
                  Restore_Lexer (Loc);
                  Property_Values :=
                    P_Items_List
                      (P_Property_Expression'Access,
                       No_Node,
                       T_Comma,
                       T_Right_Parenthesis,
                       Code);

                  if No (Property_Values) then
                     --  error when parsing Property_Values list, quit

                     Skip_Tokens (T_Semicolon);
                     return No_Node;
                  end if;
               end if;
            end if;
      end case;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (Code, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Property :=
        Add_New_Property_Constant_Declaration
          (Loc             => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Name            => Identifier,
           Property_Set    => Property_Set,
           Single_Value    => Property_Value,
           Multiple_Values => Property_Values,
           Unit_Identifier => Unit_Ident,
           Constant_Type   => Constant_Type,
           Multiplicity    => Multiplicity);
      return Property;
   end P_Property_Constant;

   ---------------------------------
   -- P_Property_Type_Declaration --
   ---------------------------------

   --  AADL_V1
   --  property_type_declaration ::=
   --     defining_property_type_identifier : type property_type_designator ;

   --  AADL_V2
   --  property_type_declaration ::=
   --     defining_property_type_identifier : type property_type ;

   function P_Property_Type_Declaration
     (Identifier   : Node_Id;
      Property_Set : Node_Id) return Node_Id
   is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.Builder.AADL.Properties;

      Property   : Node_Id;
      Designator : Node_Id;
      Loc        : Location;

   begin
      case AADL_Version is
         when AADL_V1 =>
            Designator := P_Property_Type_Designator;
         when AADL_V2 =>
            Designator := P_Property_Type;
      end case;

      if No (Designator) then
         --  error when parsing property_type_designator, quit
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_Property_Type_Declaration, T_Semicolon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Property :=
        Add_New_Property_Type_Declaration
          (Loc             => Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Identifier),
           Property_Set    => Property_Set,
           Name            => Identifier,
           Type_Designator => Designator);

      return Property;
   end P_Property_Type_Declaration;

   --------------------------------
   -- P_Property_Type_Designator --
   --------------------------------

   --  AADL_V1
   --  property_type_designator ::=   property_type
   --                               | unique_property_type_identifier
   --
   --  property_type ::=   aadlboolean | aadlstring | enumeration_type
   --                    | units_type | number_type | range_type
   --                    | classifier_type | reference_type
   --
   --  unique_property_type_identifier ::=
   --     [ property_set_identifier :: ] property_type_identifier

   --  AADL_V2
   --  property_type_designator ::=   property_type
   --                               | unique_property_type_identifier

   --  property_type ::=  aadlboolean | aadlstring | enumeration_type
   --                    | units_type | number_type | range_type
   --                    | classifier_type | reference_type
   --                    | record_type

   --  unique_property_type_identifier ::=
   --     [ property_set_identifier :: ] property_type_identifier

   function P_Property_Type_Designator return Node_Id is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Prop_Id : Node_Id;
      Loc     : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_AADLBoolean |
           T_AADLString     |
           T_Enumeration    |
           T_Units          |
           T_AADLReal       |
           T_AADLInteger    |
           T_Range          |
           T_Classifier     |
           T_Reference      =>
            Restore_Lexer (Loc);
            return P_Property_Type;

         when T_Record =>
            if AADL_Version = AADL_V2 then
               Restore_Lexer (Loc);
               return P_Property_Type;
            else
               DPE
                 (PC_Property_Type_Designator,
                  EMC_Not_Allowed_In_AADL_V1,
                  T_Record);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Identifier =>
            Restore_Lexer (Loc);
            Prop_Id :=
              P_Unique_Property_Identifier_Or_Term
                (PC_Unique_Property_Type_Identifier);

            if No (Prop_Id) then
               DPE (PC_Property_Type_Designator, T_Identifier);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            return Prop_Id;

         when others =>
            DPE
              (PC_Property_Type_Designator,
               (T_AADLBoolean,
                T_AADLString,
                T_AADLInteger,
                T_AADLReal,
                T_Enumeration,
                T_Units,
                T_Range,
                T_Classifier,
                T_Reference,
                T_Identifier,
                T_Record));
            return No_Node;
      end case;
   end P_Property_Type_Designator;

   ---------------------
   -- P_Property_Type --
   ---------------------

   --  AADL_V1
   --
   --  property_type ::=   aadlboolean | aadlstring | enumeration_type
   --                    | units_type | number_type | range_type
   --                    | classifier_type | reference_type

   --  AADL_V2
   --  property_type ::=  aadlboolean | aadlstring | enumeration_type
   --                    | units_type | number_type | range_type
   --                    | classifier_type | reference_type
   --                    | record_type

   function P_Property_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_AADLBoolean =>
            return New_Node (K_Boolean_Type, Token_Location);

         when T_AADLString =>
            return New_Node (K_String_Type, Token_Location);

         when T_Enumeration =>
            return P_Enumeration_Type;

         when T_Units =>
            return P_Units_Type;

         when T_AADLReal | T_AADLInteger =>
            return P_Number_Type;

         when T_Range =>
            return P_Range_Type;

         when T_Classifier =>
            return P_Classifier_Type;

         when T_Reference =>
            return P_Reference_Type;

         when T_Record =>
            if AADL_Version = AADL_V2 then
               return P_Record_Type;
            else
               DPE (PC_Property_Type, EMC_Not_Allowed_In_AADL_V1, T_Record);
               return No_Node;
            end if;

         when others =>
            DPE
              (PC_Property_Type,
               (T_AADLBoolean,
                T_AADLString,
                T_AADLInteger,
                T_AADLReal,
                T_Enumeration,
                T_Units,
                T_Range,
                T_Classifier,
                T_Reference,
                T_Identifier,
                T_Record));
            return No_Node;
      end case;
   end P_Property_Type;

   ------------------
   -- P_Range_Type --
   ------------------

   --  range_type ::=   range of number_type
   --                 | range of number_unique_property_type_identifier

   function P_Range_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Range_Type  : Node_Id;      --  output
      Number_Type : Node_Id;
      Loc         : Location;

   begin
      Range_Type := New_Node (K_Range_Type, Token_Location);

      Scan_Token;
      if Token /= T_Of then
         DPE (PC_Range_Type, T_Of);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_AADLReal or else Token = T_AADLInteger then
         Number_Type := P_Number_Type;
         if No (Number_Type) then
            return No_Node;
         end if;

      elsif Token = T_Identifier then
         Restore_Lexer (Loc);
         Number_Type :=
           P_Unique_Property_Identifier_Or_Term
             (PC_Unique_Property_Type_Identifier);

         if No (Number_Type) then
            return No_Node;
         end if;

      else
         DPE (PC_Range_Type, (T_AADLInteger, T_AADLReal, T_Identifier));
         return No_Node;
      end if;

      Set_Number_Type (Range_Type, Number_Type);
      return Range_Type;
   end P_Range_Type;

   ----------------------------------
   -- P_Referable_Element_Category --
   ----------------------------------

   --  AADL_V1
   --    referable_element_category ::=   component_category
   --                                   | connections
   --                                   | server subprogram

   function P_Referable_Element_Category
     (Container : Node_Id) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Components;

      pragma Unreferenced (Container);

      Loc           : Location;
      Referable_Elt : Node_Id;
      Category      : Referable_Element_Category;
      Comp_Cat      : Component_Category;

   begin
      Save_Lexer (Loc);
      Scan_Token;
      Referable_Elt := New_Node (K_Referable_Element_Category, Token_Location);
      case Token is
         when T_Connections =>
            Comp_Cat := CC_Unknown;
            Category := REC_Connections;

         when T_Server =>
            Scan_Token;
            if Token /= T_Subprogram then
               DPE (PC_Referable_Element_Category, T_Subprogram);
               return No_Node;
            end if;
            Comp_Cat := CC_Unknown;
            Category := REC_Server_Subprogram;

         when T_Data    |
           T_Subprogram |
           T_Thread     |
           T_Process    |
           T_Memory     |
           T_Processor  |
           T_Bus        |
           T_Device     |
           T_System     |
           T_Virtual    |
           T_Abstract   =>
            Comp_Cat := P_Component_Category;
            Category := REC_Component_Category;

         when others =>
            DPE (PC_Referable_Element_Category);
            return No_Node;
      end case;

      Set_Component_Cat (Referable_Elt, Component_Category'Pos (Comp_Cat));
      Set_Category (Referable_Elt, Referable_Element_Category'Pos (Category));

      return Referable_Elt;
   end P_Referable_Element_Category;

   -------------------
   -- P_Record_Type --
   -------------------

   --  AADL_V2
   --
   --  record_type ::=
   --    record ( defining_field_identifier : property_type_designator ;
   --           ( defining_field_identifier : property_type_designator ; )* )

   function P_Record_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Start_Loc   : Location;
      Record_Type : Node_Id;
      Record_List : List_Id := No_List;

   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      if Token /= T_Left_Parenthesis then
         Restore_Lexer (Start_Loc);
         return No_Node;
      end if;

      Save_Lexer (Start_Loc);
      Scan_Token;
      if Token /= T_Right_Parenthesis then
         Record_List :=
           P_Items_List
             (P_Record_Type_Element'Access,
              No_Node,
              T_Semicolon,
              T_Right_Parenthesis,
              PC_Record_Type,
              True);
         if No (Record_List) then
            DPE (PC_Record_Type, EMC_List_Is_Empty);
            return No_Node;
         end if;

         Record_Type := New_Node (K_Record_Type, Start_Loc);
         Set_List_Items (Record_Type, Record_List);
      else
         DPE (PC_Record_Type, EMC_List_Is_Empty);
         return No_Node;
      end if;

      return Record_Type;
   end P_Record_Type;

   ----------------------------
   -- P_Record_Type_Element  --
   ----------------------------

   --  XXX To be renamed in P_Record_Field someday ..

   --  AADL_V2
   --
   --  defining_field_identifier : [list of ] property_type_designator

   function P_Record_Type_Element (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      pragma Unreferenced (Container);

      Loc                      : Location;
      Identifier               : Node_Id;
      Record_Element           : Node_Id;
      Property_Type_Designator : Node_Id;
      Is_List                  : Boolean := False;

   begin
      Save_Lexer (Loc);

      if Token /= T_Identifier then
         DPE (PC_Record_Type_Element, T_Identifier);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Record_Element := New_Node (K_Record_Type_Element, Token_Location);
      Identifier     := Make_Current_Identifier (Record_Element);
      Set_Identifier (Record_Element, Identifier);

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Colon then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_List then
            Scan_Token;
            if Token /= T_Of then
               DPE (PC_Record_Type_Element, T_Of);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
            Is_List := True;
         else
            Restore_Lexer (Loc);
         end if;

         Property_Type_Designator := P_Property_Type_Designator;
         if No (Property_Type_Designator) then
            DPE (PC_Record_Type_Element);
            Restore_Lexer (Loc);
            return No_Node;
         end if;
      else
         DPE (PC_Record_Type_Element, T_Colon);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Set_Property_Type_Designator (Record_Element, Property_Type_Designator);
      Set_Is_List (Record_Element, Is_List);
      return Record_Element;
   end P_Record_Type_Element;

   --------------------------
   -- P_Reference_Category --
   --------------------------

   --  AADL_V2
   --  reference_category ::= named_element_qualified_identifier

   function P_Reference_Category (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;

      pragma Unreferenced (Container);

      Reference_Cat : Node_Id;
   begin

      Reference_Cat := P_Named_Element;

      if Reference_Cat /= No_Node then
         Set_Kind (Reference_Cat, K_Reference_Category);
      else
         return No_Node;
      end if;

      return Reference_Cat;
   end P_Reference_Category;

   ----------------------
   -- P_Reference_Type --
   ----------------------

   --  AADL_V1
   --  reference_type ::= component [ ( referable_element_category
   --                                   { , referable_element_category }* ) ]

   --  AADL_V2
   --  reference_type ::=  reference  [ ( reference_category
   --                                     { , reference_category }* ) ]

   function P_Reference_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Ref_Type  : Node_Id;
      Ref_List  : List_Id;
      Start_Loc : Location;

   begin
      Save_Lexer (Start_Loc);
      Scan_Token;
      if Token /= T_Left_Parenthesis then
         --  reference_type is empty
         Restore_Lexer (Start_Loc);
         Ref_List := No_List;
      else
         case AADL_Version is
            when AADL_V1 =>
               Ref_List :=
                 P_Items_List
                   (P_Referable_Element_Category'Access,
                    No_Node,
                    T_Comma,
                    T_Right_Parenthesis,
                    PC_Reference_Type);

            when AADL_V2 =>
               Ref_List :=
                 P_Items_List
                   (P_Reference_Category'Access,
                    No_Node,
                    T_Comma,
                    T_Right_Parenthesis,
                    PC_Reference_Type);
         end case;

         if No (Ref_List) then
            --  error when parsing classifier elements, quit
            return No_Node;
         end if;
      end if;

      Ref_Type := New_Node (K_Reference_Type, Start_Loc);
      Set_List_Items (Ref_Type, Ref_List);

      return Ref_Type;
   end P_Reference_Type;

   -------------------------
   -- P_Signed_AADLNumber --
   -------------------------

   function P_Signed_AADLNumber
     (Number_Cat : Number_Category;
      Code       : Parsing_Code) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.AADL_Values;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Signed_Number : Node_Id;       --  output
      Number_Value  : Node_Id;
      Unit_Ident    : Node_Id;
      Unitable      : Boolean := False;
      Loc           : Location;
   begin
      Save_Lexer (Loc);
      Scan_Token;
      Signed_Number := New_Node (K_Signed_AADLNumber, Token_Location);

      if Token /= T_Minus and then Token /= T_Plus then
         Restore_Lexer (Loc);
      end if;

      --  Sign is actually ignored, since it should have been parsed
      --  before.

      Save_Lexer (Loc);
      Scan_Token;
      case Token is
         when T_Real_Literal =>
            if Number_Cat = NC_Integer then
               DPE (Code, T_Integer_Literal);
               return No_Node;
            end if;

            Number_Value := New_Node (K_Literal, Token_Location);
            Set_Value
              (Number_Value,
               New_Real_Value
                 (Float_Literal_Value,
                  False,
                  Numeric_Literal_Base,
                  Numeric_Literal_Exp));
            Unitable := True;

         when T_Integer_Literal =>
            Number_Value := New_Node (K_Literal, Token_Location);
            Set_Value
              (Number_Value,
               New_Integer_Value
                 (Integer_Literal_Value,
                  False,
                  Numeric_Literal_Base,
                  Numeric_Literal_Exp));
            Unitable := True;

         when T_Value =>
            --  This token had been disabled in AADLv2

            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (Code, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Number_Value :=
              P_Unique_Property_Identifier_Or_Term
                (PC_Unique_Property_Constant_Identifier);

            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (Code, T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            if No (Number_Value) then
               return No_Node;
            end if;

         when T_Identifier =>

            if AADL_Version = AADL_V2 then
               Restore_Lexer (Loc);
               Number_Value :=
                 P_Unique_Property_Identifier_Or_Term
                   (PC_Unique_Property_Constant_Identifier);

               if No (Number_Value) then
                  return No_Node;
               end if;
            else
               DPE (Code, EMC_Not_Allowed_In_AADL_V1, T_Identifier);
            end if;

         when others =>
            case Number_Cat is
               when NC_Real =>
                  DPE (Code, (T_Value, T_Real_Literal));
               when NC_Integer =>
                  DPE (Code, (T_Value, T_Integer_Literal));
               when NC_Unknown =>
                  DPE (Code, (T_Value, T_Real_Literal, T_Integer_Literal));
            end case;
            return No_Node;
      end case;

      if Unitable then
         --  try to parse unit_identifier

         Unit_Ident := P_Identifier (No_Node);
      else
         --  no unit for property_constant_term

         Unit_Ident := No_Node;
      end if;

      Set_Number_Value (Signed_Number, Number_Value);
      Set_Unit_Identifier (Signed_Number, Unit_Ident);

      return Signed_Number;
   end P_Signed_AADLNumber;

   ------------------------------
   -- P_Signed_Number_Or_Range --
   ------------------------------

   --  Number_Term [ .. Number_Term [ delta Number_Term ] ]

   function P_Signed_Number_Or_Range (First_Term : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Range_Term  : Node_Id;            --  output
      Second_Term : Node_Id;
      Delta_Term  : Node_Id;
      Loc         : Location;

   begin
      --  First_Term contains a Real_Term or an Integer_Term
      --  Continue parsing to distinguish Number_Term and Number_Range_Term

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Interval then
         Restore_Lexer (Loc);
         return First_Term;
      end if;

      --  Token '..' is found, parse Integer_Range_Term or Real_Range_Term

      Second_Term := P_Numeric_Term (PC_Number_Range_Term);
      if No (Second_Term) then
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Delta then

         Delta_Term := P_Numeric_Term (PC_Number_Range_Term);
         if No (Delta_Term) then
            return No_Node;
         end if;

      else
         Restore_Lexer (Loc);
         Delta_Term := No_Node;
      end if;

      Range_Term :=
        New_Node
          (K_Number_Range_Term,
           Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (First_Term));
      Set_Lower_Bound (Range_Term, First_Term);
      Set_Upper_Bound (Range_Term, Second_Term);
      Set_Delta_Term (Range_Term, Delta_Term);

      return Range_Term;
   end P_Signed_Number_Or_Range;

   ------------------------------
   -- P_Single_Valued_Property --
   ------------------------------

   --  single_valued_property ::=
   --     property_type_designator [ => default_property_expression ]

   function P_Single_Valued_Property return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Valued_Property          : Node_Id;
      Property_Type_Designator : Node_Id;
      Property_Expression      : Node_Id;
      Loc                      : Location;

   begin
      Property_Type_Designator := P_Property_Type_Designator;

      if No (Property_Type_Designator) then
         --  error when parsing Property_Type_Designator, quit

         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Association then
         Property_Expression := P_Property_Expression (No_Node);

         if No (Property_Expression) then
            --  error when parsing Property_Expression, quit

            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         Property_Expression := No_Node;
      end if;

      Valued_Property :=
        New_Node
          (K_Single_Valued_Property,
           Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Property_Type_Designator));
      Set_Property_Type_Designator (Valued_Property, Property_Type_Designator);
      Set_Property_Expression (Valued_Property, Property_Expression);

      return Valued_Property;
   end P_Single_Valued_Property;

   ---------------------------
   -- P_Property_Expression --
   ---------------------------

   --  AADL_V1
   --    property_expression ::=
   --       boolean_term
   --     | real_term
   --     | integer_term
   --     | string_term
   --     | enumeration_term
   --     | real_range_term
   --     | integer_range_term
   --     | property_term
   --     | component_classifier_term
   --     | reference_term

   --  AADL_V2
   --    property_expression ::=
   --       boolean_term
   --     | real_term
   --     | integer_term
   --     | string_term
   --     | enumeration_term
   --     | unit_term
   --     | real_range_term
   --     | integer_range_term
   --     | property_term
   --     | component_classifier_term
   --     | reference_term
   --     | record_term
   --     | computed_term

   function P_Property_Expression (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.AADL_Values;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      pragma Unreferenced (Container);

      Property       : Node_Id;     --  output
      First_Num_Term : Node_Id;
      Loc            : Location;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_True | T_False | T_Not =>
            Restore_Lexer (Loc);
            return P_Or_Boolean_Term;

         when T_Value =>
            --  This token had been disabled in AADLv2

            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (PC_Property_Expression, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Property :=
              P_Unique_Property_Identifier_Or_Term (PC_Property_Term);

            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (PC_Property_Expression, T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            if No (Property) then
               return No_Node;
            end if;

            --  Check whether the parsed Property is a boolean_property_term
            --  or an integer_term or an number_term

            Save_Lexer (Loc);
            Scan_Token;
            case Token is
               when T_And =>
                  Restore_Lexer (Loc);
                  Set_Kind (Property, K_Property_Term);
                  return P_And_Boolean_Term_Aux (Property);

               when T_Or =>
                  Restore_Lexer (Loc);
                  Set_Kind (Property, K_Property_Term);
                  return P_Or_Boolean_Term_Aux (Property);

               when T_Interval =>
                  --  restore lexer location, update First_Num_Term
                  Restore_Lexer (Loc);

                  First_Num_Term :=
                    New_Node
                      (K_Signed_AADLNumber,
                       Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Property));
                  Set_Number_Value (First_Num_Term, Property);
                  Set_Unit_Identifier (First_Num_Term, No_Node);

               when others =>
                  Restore_Lexer (Loc);
                  Set_Kind (Property, K_Property_Term);
                  return Property;
            end case;

         when T_Real_Literal | T_Integer_Literal | T_Plus | T_Minus =>
            Restore_Lexer (Loc);
            First_Num_Term := P_Numeric_Term (PC_Property_Expression);

            if No (First_Num_Term) then
               return No_Node;
            end if;

         when T_String_Literal =>
            Property := New_Node (K_Literal, Token_Location);
            Set_Value (Property, New_String_Value (String_Literal_Value));
            return Property;

         when T_Identifier =>
            case AADL_Version is
               when AADL_V1 =>
                  Property := New_Node (K_Literal, Token_Location);
                  Set_Value (Property, New_Enum_Value (Token_Name));
                  --  The value is not the raw string, but the string
                  --  normalized lower case. This way, the value are case
                  --  insensitive.

                  return Property;

               when AADL_V2 =>
                  Restore_Lexer (Loc);
                  Property :=
                    P_Unique_Property_Identifier_Or_Term (PC_Property_Term);

                  if No (Property) then
                     return No_Node;
                  end if;

                  --  Check whether the parsed Property is a
                  --  boolean_property_term or an integer_term
                  --  or an number_term

                  Save_Lexer (Loc);
                  Scan_Token;
                  case Token is
                     when T_And =>
                        Restore_Lexer (Loc);
                        Set_Kind (Property, K_Property_Term);
                        return P_And_Boolean_Term_Aux (Property);

                     when T_Or =>
                        Restore_Lexer (Loc);
                        Set_Kind (Property, K_Property_Term);
                        return P_Or_Boolean_Term_Aux (Property);

                     when T_Interval =>
                        --  restore lexer location, update First_Num_Term
                        Restore_Lexer (Loc);

                        First_Num_Term :=
                          New_Node
                            (K_Signed_AADLNumber,
                             Ocarina.ME_AADL.AADL_Tree.Nodes.Loc (Property));
                        Set_Number_Value (First_Num_Term, Property);
                        Set_Unit_Identifier (First_Num_Term, No_Node);

                     when others =>
                        Restore_Lexer (Loc);
                        Set_Kind (Property, K_Property_Term);
                        return Property;
                  end case;
                  return Property;
            end case;

         when T_Data    |
           T_Subprogram |
           T_Thread     |
           T_Process    |
           T_Memory     |
           T_Processor  |
           T_Bus        |
           T_Device     |
           T_System     |
           T_Virtual    |
           T_Abstract   =>
            if AADL_Version = AADL_V1 then
               return P_Component_Classifier_Term;
            else
               DPE (PC_Property_Expression, EMC_Not_Allowed_In_AADL_V2);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Classifier =>
            if AADL_Version = AADL_V2 then
               return P_Component_Classifier_Term;
            else
               DPE (PC_Property_Expression, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Reference =>
            return P_Reference_Term;

         when T_Left_Parenthesis =>
            return P_Boolean_Or_Record_Term;

         when T_Left_Square_Bracket =>
            if AADL_Version = AADL_V2 then
               return P_Record_Term;
            else
               DPE (PC_Property_Expression, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Compute =>
            if AADL_Version = AADL_V2 then
               return P_Computed_Term;
            else
               DPE (PC_Property_Expression, EMC_Not_Allowed_In_AADL_V1);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when others =>
            DPE (PC_Property_Expression);
            return No_Node;
      end case;

      return P_Signed_Number_Or_Range (First_Num_Term);
   end P_Property_Expression;

   ------------------------------
   -- P_Boolean_Or_Record_Term --
   ------------------------------

   function P_Boolean_Or_Record_Term return Node_Id is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc : Location;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Left_Parenthesis =>
            Restore_Lexer (Loc);
            return P_Boolean_Or_Record_Term;

         when T_True | T_False | T_Not =>
            Restore_Lexer (Loc);
            return P_Or_Boolean_Term;

         when T_Identifier =>
            if AADL_Version = AADL_V2 then
               Restore_Lexer (Loc);
               return P_Record_Term;
            else
               DPE (PC_Boolean_Or_Record_Term, EMC_Not_Allowed_In_AADL_V2);
               return No_Node;
            end if;

         when T_Left_Square_Bracket =>
            return P_Record_Term;

         when others =>
            DPE (PC_Boolean_Or_Record_Term);
            return No_Node;
      end case;
   end P_Boolean_Or_Record_Term;

   ----------------------
   -- P_Reference_Term --
   ----------------------

   --  AADL_V1
   --  reference_term ::= component
   --     ( subcomponent_identifier { . subcomponent_identifier }*
   --       | subcomponent_identifier { . connection_identifier }*
   --       | subcomponent_identifier { . server_subprogram_identifier }* )

   --  AADL_V2
   --  reference_term ::= reference ( contained_model_element_path )

   function P_Reference_Term return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Reference : Node_Id;
      Ref_Term  : constant Node_Id :=
        New_Node (K_Reference_Term, Token_Location);
   begin
      case AADL_Version is
         when AADL_V1 =>
            Reference := P_Entity_Reference (PC_Reference_Term);

         when AADL_V2 =>
            Scan_Token;
            if Token /= T_Left_Parenthesis then
               DPE (PC_Reference_Term, T_Left_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Reference := P_Contained_Element_Path (No_Node);

            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (PC_Reference_Term, T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
      end case;

      if No (Reference) then
         DPE (PC_Reference_Term, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Set_Reference_Term (Ref_Term, Reference);

      return Ref_Term;
   end P_Reference_Term;

   ---------------------
   -- P_Computed_Term --
   ---------------------

   --  AADL_V2
   --  computed_term ::= compute ( function_identifier )

   function P_Computed_Term return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Computed_Term : Node_Id;
      Identifier    : Node_Id;
      Start_Loc     : Location;

   begin
      Save_Lexer (Start_Loc);

      Computed_Term := New_Node (K_Computed_Term, Start_Loc);
      if No (Computed_Term) then
         --  error when add node Computed_term, quit
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_Computed_Term, T_Left_Parenthesis);
      else
         Scan_Token;
         if Token /= T_Identifier then
            DPE (PC_Computed_Term, T_Identifier);
            return No_Node;
         end if;

         Identifier := Make_Current_Identifier (Computed_Term);
         Set_Identifier (Computed_Term, Identifier);
      end if;

      Save_Lexer (Start_Loc);
      Scan_Token;
      if Token /= T_Right_Parenthesis then
         Restore_Lexer (Start_Loc);
         DPE (PC_Computed_Term, T_Right_Parenthesis);
         return No_Node;
      end if;

      return Computed_Term;
   end P_Computed_Term;

   -------------------
   -- P_Record_Term --
   -------------------

   --  AADL_V2
   --  record_term ::=
   --     [ record_field_identifier => property_expression ;
   --        ( record_field_identifier => property_expression ; )* ]

   function P_Record_Term return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Record_Term : Node_Id;
      Item        : Node_Id;
      Items       : List_Id;
      Start_Loc   : Location;

   begin
      Save_Lexer (Start_Loc);

      Record_Term := New_Node (K_Record_Term, Start_Loc);
      if No (Record_Term) then
         --  error when add node Rec_Term, quit
         return No_Node;
      end if;

      Save_Lexer (Start_Loc);
      Scan_Token;
      Items :=
        P_Items_List
          (P_Record_Term_Element'Access,
           No_Node,
           T_Semicolon,
           T_Right_Square_Bracket,
           PC_Record_Term,
           True);

      if No (Items) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Set_List_Items (Record_Term, Items);

      Item := First_Node (Items);
      while Present (Item) loop
         --         Set_Corresponding_Entity (Item, Record_Term);
         Item := Next_Node (Item);
      end loop;

      return Record_Term;
   end P_Record_Term;

   ---------------------------
   -- P_Record_Term_Element --
   ---------------------------

   --  AADL_V2
   --    record_field_identifier => property_expression

   function P_Record_Term_Element (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      pragma Unreferenced (Container);

      Record_Term_Element : Node_Id;
      Property_Expression : Node_Id;
      Identifier          : Node_Id;
      Loc                 : Location;

   begin
      Save_Lexer (Loc);

      if Token /= T_Identifier then
         DPE (PC_Record_Term_Element, T_Identifier);
         Restore_Lexer (Loc);
         return No_Node;
      end if;

      Record_Term_Element := New_Node (K_Record_Term_Element, Token_Location);
      Identifier          := Make_Current_Identifier (Record_Term_Element);
      Set_Identifier (Record_Term_Element, Identifier);

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Association then
         Property_Expression := P_Property_Expression (No_Node);

         if No (Property_Expression) then
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
         DPE (PC_Record_Term_Element, T_Association);
         return No_Node;
      end if;

      Set_Property_Expression (Record_Term_Element, Property_Expression);
      return Record_Term_Element;
   end P_Record_Term_Element;

   ------------------------------------------
   -- P_Unique_Property_Identifier_Or_Term --
   ------------------------------------------

   --  AADL_V1
   --  unique_property_constant_identifier ::=
   --     value ( [ property_set_identifier :: ] property_constant_identifier )
   --
   --  property_term ::=
   --     value ( [ property_set_identifier :: ] property_name_identifier )

   --  AADL_V1 AND AADL_V2
   --  unique_property_type_identifier ::=
   --     [ property_set_identifier :: ] property_type_identifier

   --  AADL_V2
   --  unique_property_constant_identifier ::=
   --     [ property_set_identifier :: ] property_constant_identifier
   --
   --  property_term ::=
   --     [ property_set_identifier :: ] property_name_identifier

   function P_Unique_Property_Identifier_Or_Term
     (Code : Parsing_Code) return Node_Id
   is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Start_Loc : Location;
      Loc       : Location;
      Property  : Node_Id;
      Ident1    : Node_Id;
      Ident2    : Node_Id := No_Node;

   begin
      Save_Lexer (Start_Loc);

      Ident1 := P_Identifier (No_Node);

      if No (Ident1) then
         DPE (Code, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Colon_Colon then
         Ident2 := P_Identifier (No_Node);

         if No (Ident2) then
            DPE (Code, T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      if Code = PC_Unique_Property_Constant_Identifier then
         Property := New_Node (K_Unique_Property_Const_Identifier, Start_Loc);
      elsif Code = PC_Unique_Property_Type_Identifier then
         Property := New_Node (K_Unique_Property_Type_Identifier, Start_Loc);
      else
         Property := New_Node (K_Property_Term, Start_Loc);
      end if;

      if No (Property) then
         DPE (Code, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      if Present (Ident2) then
         Set_Identifier (Property, Ident2);
         Set_Property_Set_Identifier (Property, Ident1);
      else
         Set_Identifier (Property, Ident1);
      end if;

      return Property;
   end P_Unique_Property_Identifier_Or_Term;

   -----------------------
   -- P_Unit_Definition --
   -----------------------

   --  defining_unit_identifier => unit_identifier * numeric_literal

   function P_Unit_Definition (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Ocarina.AADL_Values;
      use Ocarina.ME_AADL.Tokens;
      use Lexer;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Definition      : Node_Id;
      Identifier      : Node_Id;
      Unit_Identifier : Node_Id;
      Numeric_Literal : Node_Id;
      Literal_Value   : Value_Id;

   begin
      --  To be able to access to both the units type and the unit
      --  definition, we link the defining identifier of the unit
      --  definition to the unit definition node and the unit
      --  identifier of the unit definition to the units type.

      Scan_Token;
      if Token /= T_Identifier then
         DPE (PC_Unit_Definition, T_Identifier);
         return No_Node;
      end if;

      Definition := New_Node (K_Unit_Definition, Token_Location);
      Identifier := Make_Current_Identifier (Definition);
      Set_Identifier (Definition, Identifier);

      Scan_Token;
      if Token /= T_Association then
         DPE (PC_Unit_Definition, T_Association);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Identifier then
         DPE (PC_Unit_Definition, T_Identifier);
         return No_Node;
      end if;

      Unit_Identifier := Make_Current_Identifier (Container);

      Scan_Token;
      if Token /= T_Multiply then
         DPE (PC_Unit_Definition, T_Multiply);
         return No_Node;
      end if;

      Scan_Token;
      if Token = T_Integer_Literal then
         Literal_Value :=
           New_Integer_Value
             (Integer_Literal_Value,
              False,
              Numeric_Literal_Base,
              Numeric_Literal_Exp);
      elsif Token = T_Real_Literal then
         Literal_Value :=
           New_Real_Value
             (Float_Literal_Value,
              False,
              Numeric_Literal_Base,
              Numeric_Literal_Exp);
      else
         DPE (PC_Unit_Definition, (T_Integer_Literal, T_Real_Literal));
         return No_Node;
      end if;

      Numeric_Literal := New_Node (K_Literal, Token_Location);
      Ocarina.ME_AADL.AADL_Tree.Nodes.Set_Value
        (Numeric_Literal,
         Literal_Value);

      Set_Unit_Identifier (Definition, Unit_Identifier);
      Set_Numeric_Literal (Definition, Numeric_Literal);

      return Definition;
   end P_Unit_Definition;

   ------------------
   -- P_Units_Type --
   ------------------

   --  units_type ::=
   --       units units_list

   --  units_list ::=
   --  ( defining_unit_identifier
   --   { , defining_unit_identifier => unit_identifier * numeric_literal }* )

   function P_Units_Type return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;

      Units : Node_Id;
      Base  : Node_Id;
      Items : List_Id;
   begin
      Units := New_Node (K_Units_Type, Token_Location);

      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_Units_Type, T_Left_Parenthesis);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Identifier then
         DPE (PC_Units_Type, T_Identifier);
         return No_Node;
      end if;

      Base := Make_Current_Identifier (Units);
      Scan_Token;

      if Token = T_Comma then
         Items :=
           P_Items_List
             (P_Unit_Definition'Access,
              Units,
              T_Comma,
              T_Right_Parenthesis,
              PC_Units_Type);

         if No (Items) then
            --  error when parsing units elements, quit

            return No_Node;
         end if;
      elsif Token = T_Right_Parenthesis then
         Items := No_List;
      else
         DPE (PC_Units_Type, (T_Comma, T_Right_Parenthesis));
         return No_Node;
      end if;

      Set_Base_Identifier (Units, Base);
      Set_Unit_Definitions (Units, Items);
      return Units;
   end P_Units_Type;

   ---------------------
   -- P_Named_Element --
   ---------------------

   --  AADL_V2
   --  property_owner ::= named_element_qualified_identifier

   function P_Named_Element return Node_Id is

      use Ocarina.ME_AADL.AADL_Tree.Nodes;
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use FE_AADL.Parser.Identifiers;
      use FE_AADL.Parser.Components;

      Loc            : Location;
      Owner_Category : Node_Id;
      Category       : Named_Element;
      Comp_Cat       : Component_Category := CC_Unknown;
      Classifier_Ref : Node_Id            := No_Node;
      Identifier     : Node_Id            := No_Node;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      Owner_Category := New_Node (K_Named_Element, Token_Location);

      case Token is
         when T_Left_Curly_Bracket =>
            --  For now, we simply skip annex-dependent properties

            Save_Lexer (Loc);
            Skip_Tokens ((T_Comma, T_Right_Parenthesis), False);
            Category := PO_Alien_Meta_Model;
            Comp_Cat := CC_Unknown;

         when T_Abstract |
           T_System      |
           T_Processor   |
           T_Thread      |
           T_Process     |
           T_Virtual     |
           T_Device      =>
            Comp_Cat := P_Component_Category;
            Category := P_Named_Element_Component;

         when T_Data =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Port then
               Category := PO_Data_Port;
            else
               Comp_Cat := CC_Data;
               Restore_Lexer (Loc);
               Category := P_Named_Element_Component_With_Access (Comp_Cat);
            end if;

         when T_Bus | T_Memory =>
            Comp_Cat := P_Component_Category;
            Category := P_Named_Element_Component_With_Access (Comp_Cat);

         when T_Subprogram =>
            Scan_Token;

            if Token = T_Identifier and then Token_Owner = POT_Call then
               Save_Lexer (Loc);
               Scan_Token;
               if Token = T_Identifier and then Token_Owner = POT_Sequence then
                  Category := PO_Subprogram_Call_Sequence;
               else
                  Restore_Lexer (Loc);
                  DPE (PC_Named_Element);
                  Category := PO_Error;
               end if;
            else
               Restore_Lexer (Loc);
               Scan_Token;
               Comp_Cat := P_Component_Category;
               Category := P_Named_Element_Component_With_Access (Comp_Cat);
            end if;

         when T_Feature =>
            Category := P_Named_Element_Feature;

         when T_Port | T_Access | T_Event =>
            Category := P_Named_Element_Port_Or_Access_Or_Event;

         when T_Mode =>
            Category := P_Named_Element_Mode;

         when T_Flow =>
            Category := P_Named_Element_Flow;

         when T_End =>
            Category := P_Named_Element_End_To_End_Flow;

         when T_Package =>
            Category := PO_Package;

         when T_Classifier =>
            Category := PO_Classifier;

         when T_Parameter =>
            Category := PO_Parameter;

         when T_Identifier =>
            Comp_Cat   := CC_Unknown;
            Identifier := Make_Current_Identifier (No_Node);
            Category   := P_Named_Element_Identifier;

         when others =>
            return No_Node;

      end case;

      if Category = PO_Error then
         DPE (PC_Named_Element);
         return No_Node;
      end if;

      if Category = PO_Component_Category then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Identifier then
            Restore_Lexer (Loc);
            Classifier_Ref := P_Entity_Reference (PC_Named_Element_Category);

            if No (Classifier_Ref) then
               --  error when parsing Classifier_Reference, quit
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;
      end if;

      Set_Category (Owner_Category, Named_Element'Pos (Category));
      Set_Component_Cat (Owner_Category, Component_Category'Pos (Comp_Cat));
      Set_Classifier_Ref (Owner_Category, Classifier_Ref);
      Set_Identifier (Owner_Category, Identifier);

      return Owner_Category;
   end P_Named_Element;

   -------------------------------
   -- P_Named_Element_Component --
   -------------------------------

   function P_Named_Element_Component return Named_Element is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Category : Named_Element;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Implementation =>
            Category := PO_Component_Implementation;

         when T_Classifier =>
            Category := PO_Component_Classifier;

         when T_Type =>
            Category := PO_Component_Type;

         when T_Identifier =>
            if Token_Owner = POT_Instance then
               Category := PO_Component_Instance;
            elsif Token_Owner = POT_Subcomponent then
               Category := PO_Component_Subcomponent;
            else
               DPE (PC_Named_Element_Component);
               Category := PO_Error;
            end if;

         when others =>
            Restore_Lexer (Loc);
            Category := PO_Component_Category;
      end case;

      return Category;

   end P_Named_Element_Component;

   -------------------------------------------
   -- P_Named_Element_Component_With_Access --
   -------------------------------------------

   function P_Named_Element_Component_With_Access
     (Component_Cat : Component_Category) return Named_Element
   is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Category : Named_Element;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Access =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Identifier and then Token_Owner = POT_Connection then
               if Component_Cat /= CC_Memory then
                  Category := PO_Component_Access_Connection;
               else
                  DPE
                    (PC_Named_Element_Component_With_Access,
                     EMC_Memory_Access_Connection_Is_Not_Allowed);
                  Category := PO_Error;
               end if;
            else
               Restore_Lexer (Loc);
               Category := PO_Component_Access;
            end if;

         when T_Implementation =>
            Category := PO_Component_Implementation;

         when T_Classifier =>
            Category := PO_Component_Classifier;

         when T_Type =>
            Category := PO_Component_Type;

         when T_Identifier =>
            if Token_Owner = POT_Instance then
               Category := PO_Component_Instance;
            elsif Token_Owner = POT_Subcomponent then
               Category := PO_Component_Subcomponent;
            else
               DPE (PC_Named_Element_Component_With_Access);
               Category := PO_Error;
            end if;

         when others =>
            Restore_Lexer (Loc);
            Category := PO_Component_Category;
      end case;

      return Category;

   end P_Named_Element_Component_With_Access;

   -----------------------------
   -- P_Named_Element_Feature --
   -----------------------------

   function P_Named_Element_Feature return Named_Element is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Category : Named_Element;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Identifier =>
            if Token_Owner = POT_Instance then
               Category := PO_Feature_Instance;
            else
               DPE (PC_Named_Element_Feature, T_Identifier);
               Category := PO_Error;
            end if;

         when T_Group =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Type then
               Category := PO_Feature_Group_Type;

            elsif Token = T_Identifier
              and then Token_Owner = POT_Connection
            then
               Category := PO_Feature_Group_Connection;

            elsif Token = T_Identifier and then Token_Owner = POT_Instance then
               Category := PO_Feature_Group_Instance;

            else
               Restore_Lexer (Loc);
               Category := PO_Feature_Group;
            end if;

         when others =>
            Restore_Lexer (Loc);
            Category := PO_Feature;
      end case;

      return Category;
   end P_Named_Element_Feature;

   --------------------------
   -- P_Named_Element_Mode --
   --------------------------

   function P_Named_Element_Mode return Named_Element is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Loc2     : Location;
      Category : Named_Element;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Identifier then
         case Token_Owner is
            when POT_Transition =>
               Save_Lexer (Loc2);
               Scan_Token;

               if Token = T_Identifier and then Token_Owner = POT_Instance then
                  Category := PO_Mode_Transition_Instance;

               elsif Token = T_Identifier
                 and then Token_Owner = POT_Connection
               then
                  Save_Lexer (Loc2);
                  Scan_Token;
                  if Token_Owner = POT_Instance then
                     Category := PO_Mode_Transition_Connection_Instance;
                  else
                     Restore_Lexer (Loc2);
                     DPE (PC_Named_Element_Mode);
                     Category := PO_Error;
                  end if;

               else
                  Restore_Lexer (Loc2);
                  Category := PO_Mode_Transition;
               end if;

            when POT_Instance =>
               Category := PO_Mode_Instance;

            when others =>
               DPE (PC_Named_Element_Mode);
               Category := PO_Error;
         end case;

      else
         Restore_Lexer (Loc);
         Category := PO_Mode;
      end if;

      return Category;
   end P_Named_Element_Mode;

   --------------------------
   -- P_Named_Element_Flow --
   --------------------------

   function P_Named_Element_Flow return Named_Element is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Loc2     : Location;
      Category : Named_Element;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Identifier =>
            if Token_Owner = POT_Specification then
               Save_Lexer (Loc2);
               Scan_Token;
               if Token = T_Identifier and then Token_Owner = POT_Instance then
                  Category := PO_Flow_Specification_Instance;
               else
                  Restore_Lexer (Loc2);
                  Category := PO_Flow_Specification;
               end if;
            else
               DPE (PC_Named_Element_Flow, T_Identifier);
               Category := PO_Error;
            end if;

         when T_Source =>
            Save_Lexer (Loc2);
            Scan_Token;
            if Token = T_Identifier
              and then Token_Owner = POT_Specification
            then
               Category := PO_Flow_Source_Specification;
            else
               DPE (PC_Named_Element_Flow, T_Identifier);
               Category := PO_Error;
            end if;

         when T_Sink =>
            Save_Lexer (Loc2);
            Scan_Token;
            if Token = T_Identifier
              and then Token_Owner = POT_Specification
            then
               Category := PO_Flow_Sink_Specification;
            else
               DPE (PC_Named_Element_Flow, T_Identifier);
               Category := PO_Error;
            end if;

         when T_Path =>
            Save_Lexer (Loc2);
            Scan_Token;
            if Token = T_Identifier
              and then Token_Owner = POT_Specification
            then
               Category := PO_Flow_Path_Specification;
            else
               DPE (PC_Named_Element_Flow, T_Identifier);
               Category := PO_Error;
            end if;

         when others =>
            Restore_Lexer (Loc);
            Category := PO_Flow;
      end case;

      return Category;
   end P_Named_Element_Flow;

   -------------------------------------
   -- P_Named_Element_End_To_End_Flow --
   -------------------------------------

   function P_Named_Element_End_To_End_Flow return Named_Element is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Loc2     : Location;
      Success  : Boolean := False;
      Category : Named_Element;

   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token /= T_To then
         DPE (PC_Named_Element_End_To_End_Flow, T_To);
      else
         Scan_Token;
      end if;

      if Token /= T_End then
         DPE (PC_Named_Element_End_To_End_Flow, T_End);
      else
         Scan_Token;
      end if;

      if Token /= T_Flow then
         DPE (PC_Named_Element_End_To_End_Flow, T_Flow);
      else
         Save_Lexer (Loc2);
         Scan_Token;
         if Token = T_Identifier and then Token_Owner = POT_Instance then
            Success  := True;
            Category := PO_End_To_End_Flow_Instance;
         else
            Restore_Lexer (Loc2);
            Success  := True;
            Category := PO_End_To_End_Flow;
         end if;
      end if;

      if not Success then
         Restore_Lexer (Loc);
         DPE (PC_Named_Element_End_To_End_Flow);
         Category := PO_Error;
      end if;

      return Category;
   end P_Named_Element_End_To_End_Flow;

   --------------------------------
   -- P_Named_Element_Identifier --
   --------------------------------

   function P_Named_Element_Identifier return Named_Element is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Category : Named_Element;

   begin
      if Token /= T_Identifier then
         DPE (PC_Named_Element_Identifier, T_Identifier);
         Category := PO_Error;
      end if;

      case Token_Owner is
         when POT_Prototype =>
            Category := PO_Prototype;

         when POT_Named =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Identifier and then Token_Owner = POT_Element then
               Category := PO_Named_Element;
            else
               Restore_Lexer (Loc);
               DPE (PC_Named_Element);
            end if;

         when POT_Instance =>
            Category := PO_Instance;

         when POT_Connection =>
            Save_Lexer (Loc);
            Scan_Token;
            if Token = T_Identifier and then Token_Owner = POT_Instance then
               Category := PO_Connection_Instance;
            else
               Restore_Lexer (Loc);
               Category := PO_Connection;
            end if;

         when POT_Subcomponent =>
            Category := PO_Subcomponent;

         when POT_Component =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Classifier then
               Category := PO_Component_Classifier;
            elsif Token = T_Implementation then
               Category := PO_Component_Implementation;
            elsif Token = T_Type then
               Category := PO_Component_Type;
            else
               Restore_Lexer (Loc);
               DPE
                 (PC_Named_Element_Identifier,
                  (T_Classifier, T_Implementation, T_Type));
            end if;

         when others =>
            DPE (PC_Named_Element_Identifier);
            Category := PO_Error;
      end case;

      return Category;
   end P_Named_Element_Identifier;

   ---------------------------------------------
   -- P_Named_Element_Port_Or_Access_Or_Event --
   ---------------------------------------------

   function P_Named_Element_Port_Or_Access_Or_Event return Named_Element is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;

      Loc      : Location;
      Loc2     : Location;
      Category : Named_Element;

   begin

      case Token is
         when T_Port =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Identifier then
               case Token_Owner is
                  when POT_Instance =>
                     Category := PO_Port_Instance;

                  when POT_Connection =>
                     Save_Lexer (Loc2);
                     Scan_Token;

                     if Token = T_Identifier
                       and then Token_Owner = POT_Instance
                     then
                        Category := PO_Port_Connection_Instance;
                     else
                        Restore_Lexer (Loc2);
                        Category := PO_Port_Connection;
                     end if;

                  when others =>
                     DPE
                       (PC_Named_Element_Port_Or_Access_Or_Event,
                        T_Identifier);
                     Category := PO_Error;
               end case;
            else
               Restore_Lexer (Loc);
               Category := PO_Port;
            end if;

         when T_Event =>
            Save_Lexer (Loc);
            Scan_Token;

            case Token is
               when T_Port =>
                  Category := PO_Event_Port;

               when T_Data =>
                  Scan_Token;
                  if Token /= T_Port then
                     DPE (PC_Named_Element_Port_Or_Access_Or_Event, T_Port);
                     Category := PO_Error;
                  else
                     Category := PO_Event_Data_Port;
                  end if;

               when others =>
                  DPE
                    (PC_Named_Element_Port_Or_Access_Or_Event,
                     (T_Port, T_Data));
                  Category := PO_Error;
            end case;

         when T_Access =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Identifier and then Token_Owner = POT_Instance then
               Category := PO_Access_Instance;

            elsif Token = T_Identifier
              and then Token_Owner = POT_Connection
            then
               Save_Lexer (Loc2);
               Scan_Token;

               if Token = T_Identifier and then Token_Owner = POT_Instance then
                  Category := PO_Access_Connection_Instance;
               else
                  Restore_Lexer (Loc2);
                  Category := PO_Access_Connection;
               end if;

            else
               Restore_Lexer (Loc);
               Category := PO_Access;
            end if;

         when others =>
            DPE (PC_Named_Element_Port_Or_Access_Or_Event);
            Category := PO_Error;
      end case;

      return Category;
   end P_Named_Element_Port_Or_Access_Or_Event;

   ------------------------------
   -- P_Contained_Element_Path --
   ------------------------------

   --  AADL_V2

   --  contained_model_element_path ::=
   --     ( contained_model_element { . contained_model_element }*
   --       [ annex_path ] )
   --     | annex_path

   --  contained_model_element ::=
   --     named_element_identifier | named_element_array_selection_identifier

   function P_Contained_Element_Path (Container : Node_Id) return Node_Id is
      use Ocarina.ME_AADL.AADL_Tree.Nutils;
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Annexes;
      use Ocarina.Builder.AADL.Properties;

      Start_Loc     : Location;
      List_Items    : List_Id := No_List;
      Contained_Elt : Node_Id;
      Annex_Path    : Node_Id := No_Node;

   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      case Token is
         when T_Left_Curly_Bracket | T_Multiply =>
            Restore_Lexer (Start_Loc);
            Annex_Path := P_Annex_Path (Container);

         when T_Identifier =>
            Restore_Lexer (Start_Loc);

            List_Items :=
              P_Items_List (P_Contained_Element'Access, Container, T_Dot);
            if Is_Empty (List_Items) then
               DPE (PC_Contained_Element_Path, T_Identifier);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

            Save_Lexer (Start_Loc);
            Scan_Token;
            if Token = T_Left_Curly_Bracket or else Token = T_Multiply then
               Restore_Lexer (Start_Loc);
               Annex_Path := P_Annex_Path (Container);
            else
               Restore_Lexer (Start_Loc);
            end if;

         when others =>
            DPE
              (PC_Contained_Element_Path,
               (T_Identifier, T_Left_Curly_Bracket, T_Multiply));
            Skip_Tokens (T_Semicolon);
            return No_Node;

      end case;

      if Annex_Path = No_Node and then List_Items = No_List then
         DPE
           (PC_Contained_Element_Path,
            (T_Identifier, T_Left_Curly_Bracket, T_Multiply));
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         Contained_Elt :=
           Add_New_Contained_Element_Path
             (Start_Loc,
              Container,
              List_Items,
              Annex_Path);
      end if;

      if No (Contained_Elt) then
         DPE (PC_Contained_Element_Path);
         return No_Node;
      end if;

      return Contained_Elt;

   end P_Contained_Element_Path;

   -------------------------
   -- P_Contained_Element --
   -------------------------

   --  AADLv2
   --  contained_element ::=
   --     named_element_identifier | named_element_array_selection_identifier

   function P_Contained_Element (Container : Node_Id) return Node_Id is
      use Lexer;
      use Ocarina.ME_AADL.Tokens;
      use Ocarina.FE_AADL.Parser.Identifiers;
      use Ocarina.FE_AADL.Parser.Components.Arrays;

      Loc           : Location;
      Start_Loc     : Location;
      Contained_Elt : Node_Id;

   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      if Token /= T_Identifier then
         DPE (PC_Contained_Element);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         Contained_Elt := Make_Current_Identifier (No_Node);
      end if;

      if No (Contained_Elt) then
         DPE (PC_Contained_Element, T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Left_Square_Bracket then
         Restore_Lexer (Start_Loc);
         Contained_Elt := P_Array_Selection (Container);

         if No (Contained_Elt) then
            DPE (PC_Contained_Element);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      return Contained_Elt;

   end P_Contained_Element;

end Ocarina.FE_AADL.Parser.Properties.Values;
