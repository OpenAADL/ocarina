------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.FE_AADL_BA.PARSER.SPECIFICATIONS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

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
     (Container : Types.Node_Id)
     return Node_Id;
   --  parse an execute behavior transition node or an mode behavior transition
   --  node, current token is T_Transitions

   function P_Behavior_Condition
     (Locate : Location;
      Container : Node_Id)
     return Node_Id;
   --  parse an behavior condition node, current token is T_Right_Step_Bracket

   function P_Execute_Condition
     (Container : Node_Id)
     return Node_Id;
   --  parse an execute condition node, current token is T_Right_Step_Bracket

   function P_Mode_Condition
     (Container : Node_Id)
     return Node_Id;
   --  parse a mode condition node, current token is T_On

   function P_Trigger_Logical_Expression
     (Container : Node_Id)
     return Node_Id;
   --  parse a trigger logical expression node

   function P_Event_Trigger
     (Container : Node_Id)
     return Node_Id;
   --  parse an event trigger node

   function P_Port_Component_Ref
     (Container : Node_Id)
     return Node_Id;
   --  parse a port component reference

   ------------------------------
   -- P_Behavior_Specification --
   ------------------------------

   --  Behavior_Specification, return an behavior annex node (K_Behavior_Annex)

   function P_Behavior_Specification
     (Annex_Subcl_Node : Types.Node_Id)
     return Node_Id
   is
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

      Behavior_Annex := Add_New_Behavior_Annex (Start_Loc, Container,
                                                No_List, No_List, No_List);
      if No (Behavior_Annex) then
         DPE (PC_Behavior_Annex, EMC_Failed);
         return No_Node;
      end if;
      --  scanning node 1
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Variables then
         Behavior_Variables := P_Elements_List (P_Behavior_Variable'Access,
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

      --  scanning node 2
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_States then
         Behavior_States := P_Elements_List (P_Behavior_State'Access,
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

      --  scaning node 3
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Transitions then
         Behavior_Transitions := P_Elements_List (P_Behavior_Transition'Access,
                                                  Behavior_Annex,
                                                  (T_End_Annex, T_None),
                                                  PC_Behavior_Transition);

         if Is_Empty (Behavior_Transitions) then
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Add_New_Behavior_Annex (Behavior_Annex,
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

   function P_Behavior_Variable (Container : Types.Node_Id) return Node_Id
   is
      Start_Loc         : Location;
      Loc               : Location;

      Identifiers       : List_Id := No_List;
      Classifier_Ref    : Node_Id := No_Node;
      Behavior_Variable : Node_Id := No_Node;
   begin
      Save_Lexer (Start_Loc);

      Behavior_Variable := Add_New_Behavior_Variable (Start_Loc, Container,
                                                      No_List, No_Node);
      if No (Behavior_Variable) then
         DPE (PC_Behavior_Variable, EMC_Failed);
         return No_Node;
      end if;

      Identifiers := P_Items_List (P_Identifier'Access,
                                   Container,
                                   T_Comma);
      if Is_Empty (Identifiers) then
         Scan_Token;
         DPE (PC_Behavior_Variable, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Colon then  -- :
         Classifier_Ref := P_Unique_Classifier_Reference (Behavior_Variable);

         if No (Classifier_Ref) then
            DPE (PC_Behavior_Variable, EMC_Unique_Classifier_Ref);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         DPE (PC_Behavior_Variable, Expected_Token => T_Colon);
         --  Restore_Lexer (Loc);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Semicolon then  -- ;
         DPE (PC_Behavior_Variable, Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_Behavior_Variable (Behavior_Variable,
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

   function P_Behavior_State (Container : Types.Node_Id) return Node_Id
   is
      Start_Loc      : Location;
      Loc            : Location;

      Ident_List     : List_Id             := No_List;
      Behavior_State : Node_Id;
      State_Kind     : Behavior_State_Kind;
   begin
      Save_Lexer (Start_Loc);

      Behavior_State := Add_New_Behavior_State (Start_Loc, Container,
                                                No_List, BSK_Error);
      if No (Behavior_State) then
         DPE (PC_Behavior_State, EMC_Failed);
         return No_Node;
      end if;

      --  scanning behavior state identifier
      Ident_List := P_Items_List (P_Identifier'Access,
                                   Container,
                                   T_Comma);
      if Is_Empty (Ident_List) then
         Scan_Token;
         DPE (PC_Behavior_State, Expected_Token => T_Identifier);
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

      Add_New_Behavior_State (Behavior_State,
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

   function P_Behavior_Transition
     (Container : Types.Node_Id)
     return Node_Id
   is
      Start_Loc           : Location;

      Transition_Node     : Node_Id;
      Behavior_Transition : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Transition_Node := P_Execute_Or_Mode_Behavior_Transition (No_Node);

      if No (Transition_Node) then
         return No_Node;
      end if;

      Behavior_Transition := Add_New_Behavior_Transition (Start_Loc,
                                                          Container,
                                                          Transition_Node);

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
     (Container : Types.Node_Id)
     return Node_Id
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
      Behavior_Act_Block  : Node_Id := No_Node; --  optional
   begin
      Save_Lexer (Start_Loc);

      Execute_Transition := Add_New_Execute_Transition (Start_Loc, Container,
                                                        No_Node, No_Node,
                                                        No_List, No_Node,
                                                        No_Node, No_Node);
      if No (Execute_Transition) then
         DPE (PC_Execute_Behavior_Transition, EMC_Failed);
         return No_Node;
      end if;

      --  scanning transition identifier
      Ident := P_Identifier (Execute_Transition);
      if No (Ident) then
         Scan_Token;
         DPE (PC_Execute_Behavior_Transition,
                    Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token = T_Comma
        or else Token = T_Left_Step_Bracket
      then
         Restore_Lexer (Start_Loc);

      elsif Token = T_Left_Square_Bracket then
         Transition_Idt := Ident;

         --  scanning Behavior Transition Priority
         Scan_Token;
         case Token is
            when T_Real_Literal =>
               Transition_Priority := New_Node (BATN.K_Literal,
                                                Token_Location);
               BATN.Set_Value (Transition_Priority,
                               New_Real_Value (Float_Literal_Value,
                                               False,
                                               Numeric_Literal_Base,
                                               Numeric_Literal_Exp));

            when T_Integer_Literal =>
               Transition_Priority := New_Node (BATN.K_Literal,
                                                Token_Location);
               BATN.Set_Value (Transition_Priority,
                               New_Integer_Value (Integer_Literal_Value,
                                                  False,
                                                  Numeric_Literal_Base,
                                                  Numeric_Literal_Exp));
            when others =>
               DPE (PC_Execute_Behavior_Transition,
                    Expected_Tokens => (T_Real_Literal, T_Integer_Literal));
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;

         Scan_Token;
         if Token /= T_Right_Square_Bracket then
            DPE (PC_Execute_Behavior_Transition,
                 Expected_Token => T_Right_Square_Bracket);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Colon then
            DPE (PC_Execute_Behavior_Transition,
                 Expected_Token => T_Colon);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

      elsif Token = T_Colon then
         Transition_Idt := Ident;
      else
         Restore_Lexer (Start_Loc);
      end if;

      --  scanning source state identifier
      --  behavior_transition must contain source state
      Sources := P_Items_List (P_Identifier'Access,
                               Container,
                               T_Comma);
      if Is_Empty (Sources) then
         Scan_Token;
         DPE (PC_Execute_Behavior_Transition, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      --  scanning behavior condition
      Scan_Token; --  consume token -[
      if Token /= T_Left_Step_Bracket then
         DPE (PC_Execute_Behavior_Transition,
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
         DPE (PC_Behavior_Transition,
              Expected_Token => T_Right_Step_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      --  scanning destination state identifier
      --  behavior_transition must contain destination state
      Destination := P_Identifier (Execute_Transition);
      if No (Destination) then
         Scan_Token;
         DPE (PC_Behavior_Transition, Expected_Token => T_Identifier);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      --  scanning behavior action block
      Behavior_Act_Block := P_Behavior_Action_Block (No_Node);

      if No (Behavior_Act_Block) then
         --  Restore_Lexer (Loc);
         DPE (PC_Behavior_Transition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_Behavior_Transition,
              Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_Execute_Transition (Execute_Transition,
                                  Container => Container,
                                  Transition_Idt => Transition_Idt,
                                  Transition_Priority => Transition_Priority,
                                  Sources => Sources,
                                  Behavior_Condition => Behavior_Condition,
                                  Destination => Destination,
                                  Behavior_Act_Block => Behavior_Act_Block);
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
   --    | mode_condition

   function P_Behavior_Condition
     (Locate    : Location;
      Container : Node_Id)
     return Node_Id
   is
      Start_Loc          : Location;

      Condition_Node     : Node_Id  := No_Node;
      Behavior_Condition : Node_Id;
   begin
      Save_Lexer (Start_Loc);

      Scan_Token;
      if Token = T_On then
         Scan_Token;
         if Token = T_Dispatch then
            Restore_Lexer (Start_Loc);
            Condition_Node := P_Dispatch_Condition (No_Node);
         else
            Restore_Lexer (Start_Loc);
            Condition_Node := P_Mode_Condition (No_Node);
         end if;
      else
         Restore_Lexer (Start_Loc);
         Condition_Node := P_Execute_Condition (No_Node);
      end if;

      if No (Condition_Node) then
         DPE (PC_Behavior_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Behavior_Condition := Add_New_Behavior_Condition (Locate,
                                                        Container,
                                                        Condition_Node);
      if No (Behavior_Condition) then
         DPE (PC_Behavior_Condition, EMC_Failed);
         return No_Node;
      else
         return Behavior_Condition;
      end if;

   end P_Behavior_Condition;

   --------------------------
   -- P_Execute_Condition --
   --------------------------

   --  execute_condition ::= [ logical_value_expression |
   --    behavior_action_block_timeout_catch | otherwise ]

   function P_Execute_Condition
     (Container : Node_Id)
     return Node_Id
   is
      Loc               : Location;

      Value_Expr        : Node_Id  := No_Node;
      Is_Otherwise_Bool : Boolean  := False;
      Execute_Condition : Node_Id;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Otherwise then
         Is_Otherwise_Bool := True;
         Save_Lexer (Loc);
         Scan_Token;
         if Token /= T_Right_Step_Bracket then
            DPE (PC_Execute_Condition,
            Expected_Token => T_Right_Step_Bracket);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         else
            Restore_Lexer (Loc);
         end if;
      else
         Restore_Lexer (Loc);
         Value_Expr := P_Value_Expression (Container);
         if No (Value_Expr) then
            DPE (PC_Execute_Condition, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;

         Execute_Condition := Add_New_Execute_Condition (Loc,
                                                         Container,
                                                         Value_Expr,
                                                         Is_Otherwise_Bool);
      if No (Execute_Condition) then
         DPE (PC_Execute_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Execute_Condition;
      end if;

   end P_Execute_Condition;

   --------------------------
   -- P_Mode_Condition --
   --------------------------

   --  mode_condition ::= on trigger_logical_expression

   function P_Mode_Condition
     (Container : Node_Id)
     return Node_Id
   is
      Start_Loc            : Location;

      Trigger_Logical_Expr : Node_Id  := No_Node;
      Mode_Condition       : Node_Id;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token; --  consume T_On

      Trigger_Logical_Expr := P_Trigger_Logical_Expression (No_Node);
      if No (Trigger_Logical_Expr) then
         DPE (PC_Mode_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Mode_Condition := Add_New_Mode_Condition (Start_Loc,
                                                Container,
                                                Trigger_Logical_Expr);
      if No (Mode_Condition) then
         DPE (PC_Mode_Condition, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Mode_Condition;
      end if;

   end P_Mode_Condition;

   ----------------------------------
   -- P_Trigger_Logical_Expression --
   ----------------------------------

   --  trigger_logical_expression ::=
   --    event_trigger { logical_operator event_Trigger }*

   function P_Trigger_Logical_Expression
      (Container : Types.Node_Id)
      return Node_Id
   is
      Start_Loc                 : Location;
      Loc                       : Location;
      Event_Trigger_Node        : Node_Id;
      Operator_Node             : Node_Id;
      Trigger_Logical_Expr      : Node_Id;
      Trigger_Logical_Expr_List : List_Id;
      Escape                    : Boolean   := False;
   begin
      Save_Lexer (Start_Loc);
      Trigger_Logical_Expr_List := New_List (K_List_Id, Token_Location);

      Event_Trigger_Node := P_Event_Trigger (No_Node);
      if Present (Event_Trigger_Node) then
         Append_Node_To_List (Event_Trigger_Node, Trigger_Logical_Expr_List);
      else
         DPE (PC_Trigger_Logical_Expression, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      loop
         Save_Lexer (Loc);
         Scan_Token;

         if Token in BA_Logical_Operator_Type then
            Operator_Node := P_Operator (No_Node);
            if Present (Operator_Node) then
               Append_Node_To_List (Operator_Node, Trigger_Logical_Expr_List);
            else
               DPE (PC_Trigger_Logical_Expression, EMC_Failed);
               Trigger_Logical_Expr_List := No_List;
               Escape := True;
            end if;

            Event_Trigger_Node := P_Event_Trigger (No_Node);
            if Present (Event_Trigger_Node) then
               Append_Node_To_List (Event_Trigger_Node,
                                                Trigger_Logical_Expr_List);
            else
               DPE (PC_Trigger_Logical_Expression, EMC_Failed);
               Trigger_Logical_Expr_List := No_List;
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

      if not Is_Empty (Trigger_Logical_Expr_List) then
         Set_Loc (Node_Id (Trigger_Logical_Expr_List),
               Ocarina.ME_AADL_BA.BA_Tree.Nodes.Loc (First_Node
                                                (Trigger_Logical_Expr_List)));
      else
         DPE (PC_Trigger_Logical_Expression, EMC_List_Is_Empty);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Trigger_Logical_Expr := Add_New_Trigger_Logical_Expr (Start_Loc,
                              Container, Trigger_Logical_Expr_List);

      if No (Trigger_Logical_Expr) then
         DPE (PC_Trigger_Logical_Expression, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Trigger_Logical_Expr;
      end if;

   end P_Trigger_Logical_Expression;

   ---------------------
   -- P_Event_Trigger --
   ---------------------

   --  event_trigger ::= in_event_port_component_reference
   --   | in_event_data_port_component_reference
   --   | ( trigger_logical_expression )

   function P_Event_Trigger
     (Container : Node_Id)
     return Node_Id
   is
      Loc                  : Location;

      Event_Trigger_Node   : Node_Id := No_Node;
      Port_Component_Ref   : Node_Id := No_Node;
      Trigger_Logical_Expr : Node_Id := No_Node;
   begin
      Save_Lexer (Loc);
      Scan_Token;

      case Token is
         when T_Identifier =>
            Restore_Lexer (Loc);
            Port_Component_Ref := P_Port_Component_Ref (Container);
            if No (Port_Component_Ref) then
               DPE (PC_Event_Trigger, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when T_Left_Parenthesis =>
            Trigger_Logical_Expr := P_Trigger_Logical_Expression (Container);
            if No (Trigger_Logical_Expr) then
               DPE (PC_Event_Trigger, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
            Scan_Token;
            if Token /= T_Right_Parenthesis then
               DPE (PC_Event_Trigger, Expected_Token => T_Right_Parenthesis);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;

         when others =>
            Event_Trigger_Node := No_Node;
            DPE (PC_Event_Trigger, Expected_Token => T_Identifier);
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
      Event_Trigger_Node := Add_New_Event_Trigger (Loc,
                                                   Container,
                                                   Port_Component_Ref,
                                                   Trigger_Logical_Expr);
      if No (Event_Trigger_Node) then
         DPE (PC_Event_Trigger, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Event_Trigger_Node;
      end if;

   end P_Event_Trigger;

   ----------------------------------
   -- P_Port_Component_Ref --
   ----------------------------------

   --  port_component_reference ::=
   --    subcomponent_name.port_identifier

   function P_Port_Component_Ref
     (Container : Node_Id)
     return Node_Id
   is
      Loc                : Location;

      Port_Idt           : Node_Id := No_Node;
      Item               : Node_Id := No_Node;
      Subcomponent_Name  : Node_Id := No_Node;
      Port_Component_Ref : Node_Id := No_Node;
   begin
      Item := P_Identifier (No_Node);

      if Present (Item) then
         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Dot then
            Subcomponent_Name := Item;

            Port_Idt := P_Identifier (No_Node);
            if No (Port_Idt) then
               Scan_Token;
               DPE (PC_Port_Component_Reference,
                    Expected_Token => T_Identifier);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         else
            DPE (PC_Port_Component_Reference,
                    Expected_Token => T_Dot);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      end if;
      Port_Component_Ref := Add_New_Port_Component_Reference (Loc,
                                                            Container,
                                                            Subcomponent_Name,
                                                            Port_Idt);
      if No (Port_Component_Ref) then
         DPE (PC_Port_Component_Reference, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Port_Component_Ref;
      end if;

   end P_Port_Component_Ref;

end Ocarina.FE_AADL_BA.Parser.Specifications;
