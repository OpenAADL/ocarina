------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L _ B A . P A R S E R . A C T I O N S     --
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
with Ocarina.FE_AADL_BA.Parser.Expressions;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.Builder.Aadl_Ba.Actions;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;

package body Ocarina.FE_AADL_BA.Parser.Actions is

   use Locations;
   use Ocarina.FE_AADL_BA.Lexer;
   use Ocarina.FE_AADL_BA.Parser.Identifiers;
   use Ocarina.FE_AADL_BA.Parser.Expressions;

   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.Tokens;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   use Ocarina.Builder.Aadl_Ba.Actions;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   function P_Behavior_Actions (Container : Node_Id) return Node_Id;

   function P_Behavior_Action (Container : Node_Id) return Node_Id;

   function P_Conditional_Statement
     (Container : Node_Id;
      Code : Parsing_Code)
     return Node_Id;

   function P_If_Cond_Struct      (Start_Loc : Location) return Node_Id;
   function P_For_Cond_Struct     (Start_Loc : Location) return Node_Id;
   function P_While_Cond_Struct   (Start_Loc : Location) return Node_Id;
   function P_DoUntil_Cond_Struct (Start_Loc : Location) return Node_Id;
   function P_Forall_Cond_Struct  (Start_Loc : Location) return Node_Id;
   function P_Basic_Action        (Start_Loc : Location) return Node_Id;
   function P_Timed_Action        (Start_Loc : Location) return Node_Id;

   function P_Assignment_Or_Communication_Action
     (Start_Loc : Location)
     return Node_Id;

   function P_Element_Values
     (Container : Node_Id;
      Start_Loc : Location)
     return Node_Id;

   function P_Subprogram_Parameter_List
     (Container : Types.Node_Id)
     return List_Id;

   function P_Parameter_Label (Container : Types.Node_Id) return Node_Id;

   function P_Array_Index (Container : Types.Node_Id) return Node_Id;

   -----------------------------
   -- P_Behavior_Action_Block --
   -----------------------------

   --  behavior_action_block ::= { behavior_actions } [ timeout behavior_time ]

   function P_Behavior_Action_Block (Container : Node_Id) return Node_Id is
      Loc                   : Location;

      Behavior_Actions      : Node_Id := No_Node;
      Behavior_Time         : Node_Id := No_Node;
      Behavior_Action_Block : Node_Id;
   begin
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Left_Curly_Bracket then
         --  scanning behavior actions
         Behavior_Actions := P_Behavior_Actions (Container);

         if No (Behavior_Actions) then
            DPE (PC_Behavior_Action_Block, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;  --  consume token }
      else
         Restore_Lexer (Loc);
      end if;
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Timeout then
         Behavior_Time := P_Behavior_Time (No_Node);

         if No (Behavior_Time) then
            DPE (PC_Behavior_Action_Block, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;
      Behavior_Action_Block := Add_New_Behavior_Action_Block
                                         (Loc,
                                          Container,
                                          Behavior_Actions,
                                          Behavior_Time);

      if No (Behavior_Action_Block) then
         DPE (PC_Behavior_Action_Block, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Behavior_Action_Block;
      end if;

   end P_Behavior_Action_Block;

   ------------------------
   -- P_Behavior_Actions --
   ------------------------

   --  behavior_actions ::=
   --    behavior_action
   --    | behavior_action_sequence
   --    | behavior_action_set

   --  behavior_action_sequence ::=
   --    behavior_action { ; behavior_action }+

   --  behavior_action_set ::=
   --    behavior_action { & behavior_action }+

   function P_Behavior_Actions (Container : Node_Id) return Node_Id is
      Loc              : Location;
      Start_Loc        : Location;
      Behavior_Actions : Node_Id;
      Behavior_Act     : Node_Id := No_Node;
      Behavior_Act_Seq : List_Id := No_List;
      Behavior_Act_Set : List_Id := No_List;
   begin
      Save_Lexer (Start_Loc);
      Behavior_Act := P_Behavior_Action (No_Node);

      if No (Behavior_Act) then
         DPE (PC_Behavior_Actions, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;
      Save_Lexer (Loc);
      Scan_Token;
      case Token is
         when T_Semicolon =>
            Restore_Lexer (Start_Loc);
            Behavior_Act_Seq := P_Items_List (P_Behavior_Action'Access,
                                              No_Node,
                                              T_Semicolon);

            if Is_Empty (Behavior_Act_Seq) then
               DPE (PC_Behavior_Actions, EMC_List_Is_Empty);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         when T_Concat =>
            Restore_Lexer (Start_Loc);
            Behavior_Act_Set := P_Items_List (P_Behavior_Action'Access,
                                              No_Node,
                                              T_Concat);

            if Is_Empty (Behavior_Act_Set) then
               DPE (PC_Behavior_Actions, EMC_List_Is_Empty);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         when others =>
            if Token = T_Right_Curly_Bracket
              or else Token = T_Else
              or else Token = T_Elsif
              or else Token = T_End
              or else Token = T_Until
            then
               Restore_Lexer (Loc);
            else
               DPE (PC_Behavior_Actions,
               Expected_Tokens => (T_Semicolon, T_Concat,
                                   T_Right_Curly_Bracket));
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
      end case;

      Behavior_Actions := Add_New_Behavior_Actions (Start_Loc,
                                                    Container,
                                                    Behavior_Act,
                                                    Behavior_Act_Seq,
                                                    Behavior_Act_Set);

      if No (Behavior_Actions) then
         DPE (PC_Behavior_Actions, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Behavior_Actions;
      end if;
   end P_Behavior_Actions;

   -----------------------
   -- P_Behavior_Action --
   -----------------------

   --  behavior_action ::=
   --    basic_action
   --  | if ( logical_value_expression ) behavior_actions
   --    { elsif ( logical_value_expression ) behavior_actions }*
   --    [ else behavior_actions ]
   --    end if
   --  | for ( element_identifier : data_unique_component_classifier_reference
   --  in element_values ) { behavior_actions }
   --  | forall ( element_identifier :
   --  data_unique_component_classifier_referencein element_values )
   --  { behavior_actions }
   --  | while ( logical_value_expression ) { behavior_actions }
   --  | do behavior_actions until ( logical_value_expression )

   function P_Behavior_Action (Container : Node_Id) return Node_Id is
      Start_Loc            : Location;

      Action_Node          : Node_Id;
      Behavior_Action_Node : Node_Id := No_Node;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      case Token is
         when T_If =>
            Action_Node := P_If_Cond_Struct (Start_Loc);

         when T_For =>
            Action_Node := P_For_Cond_Struct (Start_Loc);

         when T_While =>
            Action_Node := P_While_Cond_Struct (Start_Loc);

         when T_Do =>
            Action_Node := P_DoUntil_Cond_Struct (Start_Loc);

         when T_Forall =>
            Action_Node := P_Forall_Cond_Struct (Start_Loc);

         when T_Identifier | T_Computation  =>
            Action_Node := P_Basic_Action (Start_Loc);

         when others =>
            Action_Node := No_Node;
            DPE (PC_Behavior_Action,
                 Expected_Tokens => (T_If, T_For, T_While, T_Forall, T_Do,
                                     T_Identifier, T_Computation));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      if Present (Action_Node) then
         Behavior_Action_Node := Add_New_Behavior_Action (Start_Loc, Container,
                                                       Action_Node);
      end if;
      if No (Behavior_Action_Node) then
         DPE (PC_Behavior_Action, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Behavior_Action_Node;
      end if;
   end P_Behavior_Action;

   ----------------------
   -- P_If_Cond_Struct --
   ----------------------

   function P_If_Cond_Struct (Start_Loc : Location) return Node_Id is
      Loc            : Location;
      If_Cond_Struct : Node_Id;
      If_Node        : Node_Id;
      Elsif_List     : List_Id := No_List;
      Elsif_Node     : Node_Id;
      Else_Node      : Node_Id := No_Node;
   begin
      If_Cond_Struct := Add_New_If_Cond_Struct (Start_Loc);

      if Token /= T_If then
         DPE (PC_If_Cond_Struct,
              Expected_Token => T_If);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      If_Node := P_Conditional_Statement (If_Cond_Struct,
                                          PC_If_Cond_Statement);
      if No (If_Node) then
         DPE (PC_If_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Elsif_List := New_List (K_List_Id, Token_Location);
      loop
         Save_Lexer (Loc);
         Scan_Token;
         if Token = T_Elsif then
            Elsif_Node := P_Conditional_Statement (If_Cond_Struct,
                                                   PC_Elsif_Cond_Statement);
            if Present (Elsif_Node) then
               Append_Node_To_List (Elsif_Node, Elsif_List);
            else
               DPE (PC_If_Cond_Struct, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
            exit;
         end if;
      end loop;

      if not Is_Empty (Elsif_List) then
         Set_Loc (Node_Id (Elsif_List),
                  Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                        (Elsif_List)));
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Else then
         Else_Node := P_Conditional_Statement (If_Cond_Struct,
                                               PC_Else_Cond_Statement);
         if No (Else_Node) then
            DPE (PC_If_Cond_Struct, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Scan_Token;
      if Token /= T_End then
         DPE (PC_If_Cond_Struct, Expected_Token => T_End);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_If then
         DPE (PC_If_Cond_Struct, Expected_Token => T_If);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_If_Cond_Struct (If_Cond_Struct, No_Node,
                              If_Node, Elsif_List, Else_Node);
      return If_Cond_Struct;
   end P_If_Cond_Struct;

   -----------------------------
   -- P_Conditional_Statement --
   -----------------------------

   function P_Conditional_Statement
     (Container : Node_Id;
      Code : Parsing_Code)
     return Node_Id
   is
      Start_Loc      : Location;
      Loc            : Location;
      Cond_Stat_Node : Node_Id;
      Expression     : Node_Id := No_Node;
      Actions        : Node_Id := No_Node;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      if Code /= PC_Else_Cond_Statement then
         if Token /= T_Left_Parenthesis then
            DPE (Code,
                 Expected_Token => T_Left_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Expression := P_Value_Expression (No_Node);
         if No (Expression) then
            DPE (Code, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Right_Parenthesis then
            DPE (Code,
                 Expected_Token => T_Right_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Start_Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      case Token is
         when T_If | T_For | T_While
           | T_Forall | T_Do | T_Identifier | T_Computation =>
            Restore_Lexer (Loc);
            Actions := P_Behavior_Actions (No_Node);
            if No (Actions) then
               DPE (Code, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when others =>
            DPE (Code,
                 Expected_Tokens => (T_If, T_For, T_While, T_Forall, T_Do,
                                     T_Identifier, T_Computation));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Cond_Stat_Node := Add_New_Conditional_Statement (Start_Loc,
                                                       Container,
                                                       Expression,
                                                       Actions);
      if No (Cond_Stat_Node) then
         DPE (Code, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Cond_Stat_Node;
      end if;
   end P_Conditional_Statement;

   -----------------------
   -- P_For_Cond_Struct --
   -----------------------

   function P_For_Cond_Struct (Start_Loc : Location) return Node_Id is
      Loc : Location;

      For_Cond_Struct          : Node_Id;
      Element_Idt              : Node_Id;
      Classifier_Ref           : Node_Id;
      Element_Values_Node      : Node_Id;
      Actions                  : Node_Id;
   begin
      For_Cond_Struct := Add_New_For_Cond_Struct (Start_Loc);
      if No (For_Cond_Struct) then
         DPE (PC_For_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Left_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Element_Idt := P_Identifier (For_Cond_Struct);
      if No (Element_Idt) then
         Scan_Token;
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Colon then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Colon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Classifier_Ref := P_Unique_Classifier_Reference (For_Cond_Struct);
      if No (Classifier_Ref) then
         DPE (PC_For_Cond_Struct, EMC_Unique_Classifier_Ref);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_In then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_In);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Element_Values_Node := P_Element_Values (No_Node, Loc);
      if No (Element_Values_Node) then
         DPE (PC_For_Cond_Struct, EMC_Invalid_Range);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Parenthesis then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Right_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Curly_Bracket then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Left_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Actions := P_Behavior_Actions (No_Node);
      if No (Actions) then
         DPE (PC_For_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Curly_Bracket then
         DPE (PC_For_Cond_Struct,
              Expected_Token => T_Right_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_For_Cond_Struct (For_Cond_Struct, No_Node,
                               Element_Idt, Classifier_Ref,
                               Element_Values_Node, Actions);

      return For_Cond_Struct;

   end P_For_Cond_Struct;

   -----------------------
   -- P_Forall_Cond_Struct --
   -----------------------

   function P_Forall_Cond_Struct (Start_Loc : Location) return Node_Id is
      Loc : Location;

      Forall_Cond_Struct       : Node_Id;
      Element_Idt              : Node_Id;
      Classifier_Ref           : Node_Id;
      Element_Values_Node      : Node_Id;
      Actions                  : Node_Id;
   begin
      Forall_Cond_Struct := Add_New_Forall_Cond_Struct (Start_Loc);
      if No (Forall_Cond_Struct) then
         DPE (PC_Forall_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_Forall_Cond_Struct,
              Expected_Token => T_Left_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Element_Idt := P_Identifier (Forall_Cond_Struct);
      if No (Element_Idt) then
         Scan_Token;
         DPE (PC_Forall_Cond_Struct,
              Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Colon then
         DPE (PC_Forall_Cond_Struct,
              Expected_Token => T_Colon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Classifier_Ref := P_Unique_Classifier_Reference (Forall_Cond_Struct);
      if No (Classifier_Ref) then
         DPE (PC_Forall_Cond_Struct, EMC_Unique_Classifier_Ref);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_In then
         DPE (PC_Forall_Cond_Struct,
              Expected_Token => T_In);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Element_Values_Node := P_Element_Values (No_Node, Loc);
      if No (Element_Values_Node) then
         DPE (PC_Forall_Cond_Struct, EMC_Invalid_Range);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Parenthesis then
         DPE (PC_Forall_Cond_Struct,
              Expected_Token => T_Right_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Curly_Bracket then
         DPE (PC_Forall_Cond_Struct,
              Expected_Token => T_Left_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Actions := P_Behavior_Actions (No_Node);
      if No (Actions) then
         DPE (PC_Forall_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Curly_Bracket then
         DPE (PC_Forall_Cond_Struct,
              Expected_Token => T_Right_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_Forall_Cond_Struct (Forall_Cond_Struct, No_Node,
                                  Element_Idt, Classifier_Ref,
                                  Element_Values_Node, Actions);

      return Forall_Cond_Struct;

   end P_Forall_Cond_Struct;

   -------------------------
   -- P_While_Cond_Struct --
   -------------------------

   function P_While_Cond_Struct (Start_Loc : Location) return Node_Id is
      While_Cond_Struct : Node_Id;
      Expression        : Node_Id := No_Node;
      Actions           : Node_Id := No_Node;
   begin
      While_Cond_Struct := Add_New_While_Cond_Struct (Start_Loc);
      if No (While_Cond_Struct) then
         DPE (PC_While_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_While_Cond_Struct,
              Expected_Token => T_Left_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Expression := P_Value_Expression (No_Node);
      if No (Expression) then
         DPE (PC_While_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Parenthesis then
         DPE (PC_While_Cond_Struct,
              Expected_Token => T_Right_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Curly_Bracket then
         DPE (PC_While_Cond_Struct,
              Expected_Token => T_Left_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Actions := P_Behavior_Actions (No_Node);
      if No (Actions) then
         DPE (PC_While_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Curly_Bracket then
         DPE (PC_While_Cond_Struct,
              Expected_Token => T_Right_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_While_Cond_Struct (While_Cond_Struct, No_Node,
                               Expression, Actions);

      return While_Cond_Struct;

   end P_While_Cond_Struct;

   -------------------------
   -- P_DoUntil_Cond_Struct --
   -------------------------

   function P_DoUntil_Cond_Struct (Start_Loc : Location) return Node_Id is
      DoUntil_Cond_Struct : Node_Id;
      Expression        : Node_Id := No_Node;
      Actions           : Node_Id := No_Node;
   begin
      DoUntil_Cond_Struct := Add_New_DoUntil_Cond_Struct (Start_Loc);
      if No (DoUntil_Cond_Struct) then
         DPE (PC_DoUntil_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Actions := P_Behavior_Actions (No_Node);
      if No (Actions) then
         DPE (PC_DoUntil_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Until then
         DPE (PC_DoUntil_Cond_Struct,
              Expected_Token => T_Right_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_DoUntil_Cond_Struct,
              Expected_Token => T_Left_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Expression := P_Value_Expression (No_Node);
      if No (Expression) then
         DPE (PC_DoUntil_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Parenthesis then
         DPE (PC_DoUntil_Cond_Struct,
              Expected_Token => T_Right_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_DoUntil_Cond_Struct (DoUntil_Cond_Struct, No_Node,
                               Expression, Actions);

      return DoUntil_Cond_Struct;

   end P_DoUntil_Cond_Struct;

   --------------------
   -- P_Basic_Action --
   --------------------

   --  basic_action ::=
   --    assignment_action
   --  | communication_action
   --  | timed_action

   function P_Basic_Action (Start_Loc : Location) return Node_Id is
   begin
      case Token is
         when T_Identifier =>
            return P_Assignment_Or_Communication_Action (Start_Loc);

         when T_Computation =>
            return P_Timed_Action (Start_Loc);

         when others =>
            DPE (PC_Basic_Action,
                 Expected_Tokens => (T_Identifier, T_Computation));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
   end P_Basic_Action;

   ------------------------------------------
   -- P_Assignment_Or_Communication_Action --
   ------------------------------------------

   --  assignment_action::=
   --    target := ( value_expression | any )

   --  target ::=
   --    outgoing_port_name
   --  | internal_port_name
   --  | outgoing_subprogram_parameter_identifier
   --  | data_component_reference
   --  | outgoing_port_prototype_name

   --  communication_action ::=
   --    subprogram_prototype_name ! [ ( subprogram_parameter_list ) ]
   --  | required_subprogram_access_name ! [ ( subprogram_parameter_list ) ]
   --  | subprogram_subcomponent_name ! [ ( subprogram_parameter_list ) ]
   --  | output_port_name ! [ ( value_expression ) ]
   --  | internal_port_name ! [ ( value_expression ) ]
   --  | input_port_name >>
   --  | input_port_name ? [ ( target ) ]
   --  | required_data_access_name !<
   --  | required_data_access_name !>
   --  | required_data_access_name . provided_subprogram_access_name !
   --    [ ( subprogram_parameter_list ) ]
   --  | data_subcomponent_name . provided_subprogram_access_name !
   --    [ ( subprogram_parameter_list ) ]
   --  | local_variable_name . provided_subprogram_access_name !
   --    [ ( subprogram_parameter_list ) ]

   function P_Assignment_Or_Communication_Action (Start_Loc : Location)
     return Node_Id
   is
      Loc               : Location;
      Loc2              : Location;
      Node              : Node_Id;
      Ident             : Node_Id;
      Value_Expr        : Node_Id            := No_Node;
      Target            : Node_Id            := No_Node;
      Sub_Parameters    : List_Id            := No_List;
      Com_Kind          : Communication_Kind := CK_No_Kind;
      IS_Any_Bool       : Boolean            := False;
   begin
      Restore_Lexer (Start_Loc);
      Ident := P_Name (No_Node);
      if No (Ident) then
         DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Dot then
         Restore_Lexer (Start_Loc);
         Ident := P_Data_Component_Reference (No_Node);
         if No (Ident) then
            DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Scan_Token;
      case Token is
         when T_Exclamation =>
            Com_Kind := CK_Exclamation;

         when T_Interrogative =>
            Com_Kind := CK_Interrogative;

         when T_Greater_Greater_Than =>
            Com_Kind := CK_Greater_Greater;

         when T_Exclamation_Greater =>
            Com_Kind := CK_Exclamation_Greater;

         when T_Exclamation_Lesser =>
            Com_Kind := CK_Exclamation_Lesser;

         when T_Assignment =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Any then
               IS_Any_Bool := True;
            else
               Restore_Lexer (Loc);
               Value_Expr := P_Value_Expression (No_Node);
               if No (Value_Expr) then
                  DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
                  Skip_Tokens (T_Semicolon);
                  return No_Node;
               end if;
            end if;

         when others =>
            DPE (PC_Assignment_Or_Communication_Action,
                 Expected_Tokens => (T_Exclamation,
                                     T_Interrogative,
                                     T_Greater_Greater_Than,
                                     T_Exclamation_Greater,
                                     T_Exclamation_Lesser,
                                     T_Assignment));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Parenthesis then
         if Com_Kind /= CK_Greater_Greater
           and then Com_Kind /= CK_Exclamation_Greater
           and then Com_Kind /= CK_Exclamation_Lesser
           and then Com_Kind /= CK_Interrogative
           and then Com_Kind /= CK_No_Kind
         then
            Sub_Parameters := P_Subprogram_Parameter_List (No_Node);

            if Is_Empty (Sub_Parameters) then
               DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         elsif Com_Kind = CK_Interrogative then
            Save_Lexer (Loc);
            Target := P_Name (No_Node);

            Save_Lexer (Loc2);
            Scan_Token;
            if Token = T_Dot then
               Restore_Lexer (Loc);
               Target := P_Data_Component_Reference (No_Node);
            else
               Restore_Lexer (Loc2);
            end if;

            if No (Target) then
               DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         elsif Com_Kind = CK_Greater_Greater
           and then Com_Kind = CK_Exclamation_Greater
           and then Com_Kind = CK_Exclamation_Lesser
         then
            DPE (PC_Assignment_Or_Communication_Action, EMC_Illegal_Syntax);
            Skip_Tokens (T_Semicolon);
            return No_Node;

         end if;

         Scan_Token;
         if Token /= T_Right_Parenthesis then
            DPE (PC_Assignment_Or_Communication_Action,
                 Expected_Token => T_Right_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

      else
         Restore_Lexer (Loc);
      end if;
      if Value_Expr /= No_Node or else Is_Any_Bool then
         Node := Add_New_Assignment_Action (Start_Loc, No_Node, Ident,
                                            Value_Expr, Is_Any_Bool);
      else
         Node := Add_New_Communication_Action (Start_Loc, No_Node, Ident,
                                               Target, Sub_Parameters,
                                               Com_Kind);
      end if;
      if No (Node) then
         DPE (PC_Assignment_Or_Communication_Action, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Node;
      end if;

   end P_Assignment_Or_Communication_Action;

   --------------------
   -- P_Timed_Action --
   --------------------

   --  timed_action ::=
   --    computation ( behavior_time [ .. behavior_time ] )
   --    [ in binding ( processor_unique_component_classier_reference
   --    { , processor_unique_component_classifier_reference }* ) ]

   function P_Timed_Action (Start_Loc : Location) return Node_Id is
      Loc               : Location;
      Timed_Action      : Node_Id;
      Fst_Behav_Time    : Node_Id;
      Scd_Behav_Time    : Node_Id           := No_Node;
      Is_InBinding      : Boolean           := False;
      Processor_Idt     : List_Id           := No_List;
   begin
      Restore_Lexer (Start_Loc);

      Scan_Token;
      if Token /= T_Computation then
         Restore_Lexer (Start_Loc);
         DPE (PC_Timed_Action,
              Expected_Token => T_Computation);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;
      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_Timed_Action, Expected_Token => T_Left_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Fst_Behav_Time := P_Behavior_Time (No_Node);
      if No (Fst_Behav_Time) then
         DPE (PC_Timed_Action, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;
      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Interval then
         Scd_Behav_Time := P_Behavior_Time (No_Node);
            if No (Scd_Behav_Time) then
               DPE (PC_Timed_Action, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
      else
         Restore_Lexer (Loc);
      end if;
      Scan_Token;
      if Token /= T_Right_Parenthesis then
         DPE (PC_Timed_Action, Expected_Token => T_Right_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_In then
         Scan_Token;
         if Token /= T_Binding then
            DPE (PC_Timed_Action, Expected_Token => T_Binding);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
         Scan_Token;
         if Token /= T_Left_Parenthesis then
            DPE (PC_Timed_Action, Expected_Token => T_Left_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
         Is_InBinding := True;
         Processor_Idt := P_Items_List (P_Identifier'Access,
                                        No_Node,
                                        T_Comma);

         if Is_Empty (Processor_Idt) then
            Scan_Token;
            DPE (PC_Timed_Action, Expected_Token => T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
         Scan_Token;
         if Token /= T_Right_Parenthesis then
            DPE (PC_Timed_Action, Expected_Token => T_Right_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Timed_Action := Add_New_Timed_Action (Start_Loc, No_Node,
                                         Fst_Behav_Time,
                                         Scd_Behav_Time,
                                         Processor_Idt,
                                         Is_InBinding);
      if No (Timed_Action) then
         DPE (PC_Timed_Action, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Timed_Action;
      end if;

   end P_Timed_Action;

   --------------------------------
   -- P_Data_Component_Reference --
   --------------------------------

   --  data_component_reference ::=
   --   data_subcomponent_name { . data_subcomponent_name }*
   --   | data_access_feature_name { . data_field }*
   --   | local_variable_name { . data_field }*
   --   | data_access_feature_prototype_name { . data_field }*

   --  data_field ::=
   --   data_subcomponent_name
   --   | data_access_feature_name
   --   | data_access_feature_prototype_name

   function P_Data_Component_Reference
     (Container : Node_Id)
     return Node_Id
   is
      Loc           : Location;
      Idents        : List_Id;
      Data_Comp_Ref : Node_Id;
   begin
      Save_Lexer (Loc);
      Idents := P_Items_List (P_Name'Access,
                              Container,
                              T_Dot);
      if Is_Empty (Idents) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Data_Comp_Ref := Add_New_Data_Component_Reference (Loc, Container,
                                                         Idents);
      if No (Data_Comp_Ref) then
         DPE (PC_Data_Component_Reference, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Data_Comp_Ref;
      end if;

   end P_Data_Component_Reference;

   ----------------------
   -- P_Element_Values --
   ----------------------

   --  element_values ::=
   --    integer_range
   --  | event_data
   --  | array_data_component_reference

   function P_Element_Values
     (Container : Node_Id;
      Start_Loc : Location)
     return Node_Id
   is

      Loc                 : Location;
      Node_Element_Values : Node_Id;
   begin
      Scan_Token;
      if Token = T_Identifier then
         Restore_Lexer (Start_Loc);
         Node_Element_Values := P_Data_Component_Reference (No_Node);

         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Interval then
            Restore_Lexer (Start_Loc);
            Node_Element_Values := P_Integer_Range (Container);
         else
            Restore_Lexer (Loc);
         end if;
      else
         Restore_Lexer (Start_Loc);
         Node_Element_Values := P_Integer_Range (Container);
      end if;

      if No (Node_Element_Values) then
            DPE (PC_Element_Values, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
      else
         return Node_Element_Values;
      end if;

   end P_Element_Values;

   ---------------------------------
   -- P_Subprogram_Paremeter_List --
   ---------------------------------

   --  subprogram_parameter_list ::= parameter_label { , parameter_label } *

   function P_Subprogram_Parameter_List
     (Container : Types.Node_Id)
     return List_Id
   is
   begin
      return P_Items_List (P_Parameter_Label'Access,
                           Container,
                           T_Comma);
   end P_Subprogram_Parameter_List;

   -----------------------
   -- P_Parameter_Label --
   -----------------------

   --  parameter_label ::= in_parameter_value_expression | out_parameter_target

   function P_Parameter_Label (Container : Types.Node_Id) return Node_Id
   is
      Start_Loc   : Location;
      --  Loc         : Location;
      Parameter   : Node_Id;
      Param_Label : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Parameter := P_Value_Expression (Container);
      if No (Parameter) then
         DPE (PC_Parameter_Label, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Param_Label := Add_New_Parameter_Label (Start_Loc, Container, Parameter);
      if No (Param_Label) then
         DPE (PC_Parameter_Label, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Param_Label;
      end if;
   end P_Parameter_Label;

   ------------
   -- P_Name --
   ------------

   --  name ::=
   --   { identifier . }*
   --   identifier { array_index }*

   --  array_index ::=
   --    [ integer_value ]

   function P_Name (Container : Types.Node_Id) return Node_Id is
      Start_Loc        : Location;
      Loc              : Location;
      Name_Node        : Node_Id;
      Idents           : List_Id;
      Array_Index_List : List_Id  := No_List;
   begin
      Save_Lexer (Start_Loc);

      Idents := P_Items_List (P_Identifier'Access, No_Node, T_Dot);
      if Is_Empty (Idents) then
         Scan_Token;
         DPE (PC_Name, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Square_Bracket then
         Array_Index_List := P_Items_List (P_Array_Index'Access, No_Node,
                                           T_Left_Square_Bracket);
         if Is_Empty (Array_Index_List) then
            DPE (PC_Name, EMC_List_Is_Empty);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Name_Node := Add_New_Name (Start_Loc, Container, Idents,
                                 Array_Index_List);
      if No (Name_Node) then
         DPE (PC_Name, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Name_Node;
      end if;

   end P_Name;

   function P_Array_Index (Container : Types.Node_Id) return Node_Id is
      Loc              : Location;
      Array_Index_Node : Node_Id := No_Node;
   begin
      Save_Lexer (Loc);

      --  Scan_Token; --  consume [
      Array_Index_Node := P_Integer_Value (Container);
      if No (Array_Index_Node) then
         DPE (PC_Array_Index, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
         if Token /= T_Right_Square_Bracket then
            DPE (PC_Array_Index, Expected_Token => T_Right_Square_Bracket);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      if No (Array_Index_Node) then
         DPE (PC_Array_Index, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Array_Index_Node;
      end if;
   end P_Array_Index;

end Ocarina.FE_AADL_BA.Parser.Actions;
