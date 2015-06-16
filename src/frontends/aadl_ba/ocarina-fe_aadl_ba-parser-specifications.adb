------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.FE_AADL_BA.PARSER.SPECIFICATIONS                  --
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

with Ada.Text_IO;

with Locations;

with Ocarina.FE_AADL_BA.Lexer;
with Ocarina.FE_AADL_BA.Parser.Identifiers;
with Ocarina.FE_AADL_BA.Parser.Thread_Dispatch;
with Ocarina.FE_AADL_BA.Parser.Actions;
with Ocarina.FE_AADL_BA.Parser.Expressions;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.ME_AADL.AADL_Tree.Nodes;

with Ocarina.AADL_Values;

with Ocarina.Builder.Aadl_Ba.Specifications;

package body Ocarina.FE_AADL_BA.Parser.Specifications is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package BATN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   use Locations;
   use Ocarina.FE_AADL_BA.Lexer;
   use Ocarina.FE_AADL_BA.Parser.Identifiers;
   use Ocarina.FE_AADL_BA.Parser.Thread_Dispatch;
   use Ocarina.FE_AADL_BA.Parser.Actions;
   use Ocarina.FE_AADL_BA.Parser.Expressions;

   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.Tokens;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   use Ocarina.AADL_Values;

   use Ocarina.Builder.Aadl_Ba.Specifications;

   function P_Behavior_Annex (Container : Node_Id) return Node_Id;
   --  parse an behavior annex node, current token is T_Begin_Annex

   function P_Behavior_Variable (Container : Types.Node_Id) return Node_Id;
   --  parse an behavior variable node, current token is T_Variables

   function P_Behavior_State (Container : Types.Node_Id) return Node_Id;
   --  parse an behavior state node, current token is T_States

   function P_Behavior_Transition (Container : Types.Node_Id) return Node_Id;
   --  parse an behavior transition node, current token is T_Transitions

   function P_Execute_Or_Mode_Behavior_Transition
     (Container : Types.Node_Id) return Node_Id;
   --  parse an execute behavior transition node or an mode behavior transition
   --  node, current token is T_Transitions

   function P_Behavior_Condition
     (Locate    : Location;
      Container : Node_Id) return Node_Id;
   --  parse an behavior condition node, current token is T_Right_Step_Bracket

   ------------------------------
   -- P_Behavior_Specification --
   ------------------------------

   --  Behavior_Specification, return an behavior annex node (K_Behavior_Annex)

   function P_Behavior_Specification
     (Annex_Subcl_Node : Types.Node_Id) return Node_Id
   is
      use ATN;

      Loc           : Location;
      Behavior_Spec : Node_Id := No_Node;
   begin

      if Present (Annex_Subcl_Node) then
         Restore_Lexer (ATN.Loc (Annex_Subcl_Node));

         Scan_Token;  -- Token T_Identifier
         Scan_Token;  -- Token T_Begin_Annex
      end if;

      Behavior_Spec := P_Behavior_Annex (Annex_Subcl_Node);
      if No (Behavior_Spec) then
         DPE (PC_Behavior_Specification, EMC_Failed, Fatal => True);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_End_Annex then
         DPE (PC_Behavior_Specification, Expected_Token => T_End_Annex);
         return No_Node;
      else
         Restore_Lexer (Loc);
      end if;

      return Behavior_Spec;

   end P_Behavior_Specification;

   ----------------------
   -- P_Behavior_Annex --
   ----------------------

   --  behavior_annex ::=
   --    [ variables { behavior_variable }+ ]
   --    [ states { behavior_state }+ ]
   --    [ transitions { behavior_transition }+ ]

   function P_Behavior_Annex (Container : Node_Id) return Node_Id is
      Start_Loc : Location;
      Loc       : Location;

      Behavior_Annex       : Node_Id;
      Behavior_Variables   : List_Id := No_List;
      Behavior_States      : List_Id := No_List;
      Behavior_Transitions : List_Id := No_List;
   begin
      Save_Lexer (Start_Loc);

      Behavior_Annex :=
        Add_New_Behavior_Annex
          (Start_Loc,
           Container,
           No_List,
           No_List,
           No_List);
      if No (Behavior_Annex) then
         DPE (PC_Behavior_Annex, EMC_Failed);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Variables then
         Behavior_Variables :=
           P_Elements_List
             (P_Behavior_Variable'Access,
              Behavior_Annex,
              (T_States, T_End_Annex),
              PC_Behavior_Variable);

         if Is_Empty (Behavior_Variables) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_States then
         Behavior_States :=
           P_Elements_List
             (P_Behavior_State'Access,
              Behavior_Annex,
              (T_Transitions, T_End_Annex),
              PC_Behavior_State);

         if Is_Empty (Behavior_States) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Transitions then
         Behavior_Transitions :=
           P_Elements_List
             (P_Behavior_Transition'Access,
              Behavior_Annex,
              (T_End_Annex, T_None),
              PC_Behavior_Transition);

         if Is_Empty (Behavior_Transitions) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

      Add_New_Behavior_Annex
        (Behavior_Annex,
         No_Node,             --  container
         Behavior_Variables,
         Behavior_States,
         Behavior_Transitions);

      return Behavior_Annex;

   end P_Behavior_Annex;

   -------------------------
   -- P_Behavior_Variable --
   -------------------------

   --  behavior_variable ::=
   --    local_variable_identifier { , local_variable_identifier }*
   --      [ : data_unique_component_classifier_reference ];

   --  unique_component_classifier_reference ::= <core AADL rule>

   function P_Behavior_Variable (Container : Types.Node_Id) return Node_Id is
      Start_Loc : Location;
      Loc       : Location;

      Identifiers       : List_Id := No_List;
      Classifier_Ref    : Node_Id := No_Node;
      Behavior_Variable : Node_Id := No_Node;
   begin
      Save_Lexer (Start_Loc);

      Behavior_Variable :=
        Add_New_Behavior_Variable (Start_Loc, Container, No_List, No_Node);
      if No (Behavior_Variable) then
         DPE (PC_Behavior_Variable, EMC_Failed);
         return No_Node;
      end if;

      Identifiers := P_Items_List (P_Identifier'Access, Container, T_Comma);
      if Is_Empty (Identifiers) then
         DPE (PC_Behavior_Variable, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Colon then
         Classifier_Ref := P_Unique_Classifier_Reference (Behavior_Variable);

         if No (Classifier_Ref) then
            DPE (PC_Behavior_Variable, EMC_Unique_Classifier_Ref);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_Behavior_Variable, Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_Behavior_Variable
        (Behavior_Variable,
         Container,
         Identifiers,
         Classifier_Ref);
      return Behavior_Variable;

   end P_Behavior_Variable;

   ----------------------
   -- P_Behavior_State --
   ----------------------

   --  behavior_state ::=
   --    behavior_state_identifier { , behavior_state_identifier }*
   --        : behavior_state_kind state;

   --  behavior_state_kind ::= [ initial ] [ complete ] [ final ]

   function P_Behavior_State (Container : Types.Node_Id) return Node_Id is
      Start_Loc : Location;
      Loc       : Location;

      Ident_List     : List_Id := No_List;
      Behavior_State : Node_Id;
      State_Kind     : Behavior_State_Kind;
   begin
      Save_Lexer (Start_Loc);

      Behavior_State :=
        Add_New_Behavior_State (Start_Loc, Container, No_List, BSK_Error);
      if No (Behavior_State) then
         DPE (PC_Behavior_State, EMC_Failed);
         return No_Node;
      end if;

      Ident_List := P_Items_List (P_Identifier'Access, Container, T_Comma);
      if Is_Empty (Ident_List) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Colon then
         DPE (PC_Behavior_State, Expected_Token => T_Colon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      --  parse Behavior_State_Kind
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Initial =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Complete then
               Save_Lexer (Loc);
               Scan_Token;

               if Token = T_Final then
                  State_Kind := BSK_Initial_Complete_Final;
               else
                  Restore_Lexer (Loc);
                  State_Kind := BSK_Initial_Complete;
               end if;
            elsif Token = T_Final then
               State_Kind := BSK_Initial_Final;
            else
               Restore_Lexer (Loc);
               State_Kind := BSK_Initial;
            end if;

         when T_Complete =>
            Save_Lexer (Loc);
            Scan_Token;

            if Token = T_Final then
               State_Kind := BSK_Complete_Final;
            else
               Restore_Lexer (Loc);
               State_Kind := BSK_Complete;
            end if;

         when T_Final =>
            State_Kind := BSK_Final;

         when others =>
            State_Kind := BSK_No_Kind;
            Restore_Lexer (Loc);

      end case;

      Scan_Token;
      if Token /= T_State then
         DPE (PC_Behavior_State, Expected_Token => T_State);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_Behavior_State, Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_Behavior_State
        (Behavior_State,
         Container,
         Ident_List,
         State_Kind);

      return Behavior_State;

   end P_Behavior_State;

   ---------------------------
   -- P_Behavior_Transition --
   ---------------------------

   --  behavior_transition ::=
   --    execution_behavior_transition | mode_transition

   function P_Behavior_Transition (Container : Types.Node_Id) return Node_Id is
      Start_Loc : Location;

      Transition_Node     : Node_Id;
      Behavior_Transition : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Transition_Node := P_Execute_Or_Mode_Behavior_Transition (No_Node);

      if No (Transition_Node) then
         return No_Node;
      end if;

      Behavior_Transition :=
        Add_New_Behavior_Transition (Start_Loc, Container, Transition_Node);

      if No (Behavior_Transition) then
         DPE (PC_Behavior_Transition, EMC_Failed);
         return No_Node;
      end if;

      return Behavior_Transition;
   end P_Behavior_Transition;

   -------------------------------------------
   -- P_Execute_Or_Mode_Behavior_Transition --
   -------------------------------------------

   --  execute_behavior_transition ::=
   --    [ transition_identifier [ [ behavior_transition_priority ] : ]
   --        source_state_identifier { , source_state_identifier }*
   --          -[ [ behavior_condition ] ]->
   --        destination_state_identifier [ { behavior_actions } ] ;

   --  mode_transition ::=
   --    mode_transition_identifier :
   --        source_mode_identifier
   --          –[ mode_transition_condition ]->
   --        destination_mode_identifier [ { behavior_actions } ] ;

   function P_Execute_Or_Mode_Behavior_Transition
     (Container : Types.Node_Id) return Node_Id
   is
      Start_Loc : Location;
      Loc       : Location;

      Ident               : Node_Id := No_Node;
      Sources             : List_Id := No_List;
      Destination         : Node_Id;
      Transition_Idt      : Node_Id := No_Node; --  optional
      Transition_Priority : Node_Id := No_Node; --  optional
      Behavior_Condition  : Node_Id := No_Node; --  optional
      Execute_Transition  : Node_Id;
      Behavior_Act_List   : List_Id := No_List; --  optional
   begin
      Save_Lexer (Start_Loc);

      Execute_Transition :=
        Add_New_Execute_Transition
          (Start_Loc,
           Container,
           No_Node,
           No_Node,
           No_List,
           No_Node,
           No_Node,
           No_List);
      if No (Execute_Transition) then
         DPE (PC_Execute_Behavior_Transition, EMC_Failed);
         return No_Node;
      end if;

      Ident := P_Identifier (Execute_Transition);
      if No (Ident) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token = T_Comma or else Token = T_Left_Step_Bracket then
         Restore_Lexer (Start_Loc);

      elsif Token = T_Left_Square_Bracket then
         Transition_Idt := Ident;

         Scan_Token;
         case Token is
            when T_Real_Literal =>
               Transition_Priority :=
                 New_Node (BATN.K_Literal, Token_Location);
               BATN.Set_Value
                 (Transition_Priority,
                  New_Real_Value
                    (Float_Literal_Value,
                     False,
                     Numeric_Literal_Base,
                     Numeric_Literal_Exp));

            when T_Integer_Literal =>
               Transition_Priority :=
                 New_Node (BATN.K_Literal, Token_Location);
               BATN.Set_Value
                 (Transition_Priority,
                  New_Integer_Value
                    (Integer_Literal_Value,
                     False,
                     Numeric_Literal_Base,
                     Numeric_Literal_Exp));
            when others =>
               DPE
                 (PC_Execute_Behavior_Transition,
                  Expected_Tokens => (T_Real_Literal, T_Integer_Literal));
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;

         Scan_Token;
         if Token /= T_Right_Square_Bracket then
            DPE
              (PC_Execute_Behavior_Transition,
               Expected_Token => T_Right_Square_Bracket);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Colon then
            DPE (PC_Execute_Behavior_Transition, Expected_Token => T_Colon);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

      elsif Token = T_Colon then
         Transition_Idt := Ident;
      end if;

      Sources := P_Items_List (P_Identifier'Access, Container, T_Comma);
      if Is_Empty (Sources) then
         DPE (PC_Execute_Behavior_Transition, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token; --  consume token -[
      if Token /= T_Left_Step_Bracket then
         DPE
           (PC_Execute_Behavior_Transition,
            Expected_Token => T_Left_Step_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Right_Step_Bracket then
         Restore_Lexer (Loc);
         Behavior_Condition := P_Behavior_Condition (Loc, Container);

         if No (Behavior_Condition) then
            DPE (PC_Behavior_Transition, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token /= T_Right_Step_Bracket then
         DPE (PC_Behavior_Transition, Expected_Token => T_Right_Step_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Destination := P_Identifier (Execute_Transition);

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Left_Curly_Bracket then
         Behavior_Act_List := P_Behavior_Actions (Container);

         if Is_Empty (Behavior_Act_List) then
            --  DPE (PC_Behavior_Transition, EMC_List_Is_Empty);
            raise Program_Error;
            --  Skip_Tokens (T_Semicolon);
            --  return No_Node;
         end if;

         Scan_Token;  --  consume token }
      else
         Restore_Lexer (Loc);
      end if;

      Scan_Token;

      if Token = T_Timeout then --  XXX
         Scan_Token;
         Ada.Text_IO.Put_Line (Token'Img);
         Scan_Token;
         Ada.Text_IO.Put_Line (Token'Img);
         Scan_Token;
         Ada.Text_IO.Put_Line (Token'Img);

      elsif Token /= T_Semicolon then
         DPE (PC_Behavior_Transition, Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_Execute_Transition
        (Execute_Transition,
         Container           => Container,
         Transition_Idt      => Transition_Idt,
         Transition_Priority => Transition_Priority,
         Sources             => Sources,
         Behavior_Condition  => Behavior_Condition,
         Destination         => Destination,
         Behavior_Act_List   => Behavior_Act_List);
      if No (Execute_Transition) then
         DPE (PC_Execute_Behavior_Transition, EMC_Failed);
         return No_Node;
      else
         return Execute_Transition;
      end if;
   end P_Execute_Or_Mode_Behavior_Transition;

   --------------------------
   -- P_Behavior_Condition --
   --------------------------

   --  behavior_condition ::= execute_condition | dispatch_condition

   function P_Behavior_Condition
     (Locate    : Location;
      Container : Node_Id) return Node_Id
   is
      Start_Loc : Location;

      Condition_Node     : Node_Id := No_Node;
      Behavior_Condition : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Scan_Token;
      if Token = T_On then
         Restore_Lexer (Start_Loc);
         Condition_Node := P_Dispatch_Condition (No_Node);

      else
         Restore_Lexer (Start_Loc);
         Condition_Node := P_Value_Expression (No_Node);
      end if;

      if No (Condition_Node) then
         DPE (PC_Behavior_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Behavior_Condition :=
        Add_New_Behavior_Condition (Locate, Container, Condition_Node);
      if No (Behavior_Condition) then
         DPE (PC_Behavior_Condition, EMC_Failed);
         return No_Node;
      else
         return Behavior_Condition;
      end if;
   end P_Behavior_Condition;

end Ocarina.FE_AADL_BA.Parser.Specifications;
