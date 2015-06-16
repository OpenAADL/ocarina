------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . F E _ A A D L _ B A . P A R S E R . A C T I O N S     --
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

with Ocarina.FE_AADL_BA.Lexer;
with Ocarina.FE_AADL_BA.Parser.Identifiers;
with Ocarina.FE_AADL_BA.Parser.Expressions;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.Tokens;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.Builder.Aadl_Ba.Actions;

package body Ocarina.FE_AADL_BA.Parser.Actions is

   use Locations;
   use Ocarina.FE_AADL_BA.Lexer;
   use Ocarina.FE_AADL_BA.Parser.Identifiers;
   use Ocarina.FE_AADL_BA.Parser.Expressions;

   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.Tokens;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   use Ocarina.Builder.Aadl_Ba.Actions;

   function P_Behavior_Action (Container : Node_Id) return Node_Id;

   function P_Conditional_Statement
     (Container : Node_Id;
      Code      : Parsing_Code) return Node_Id;

   function P_If_Cond_Struct (Start_Loc : Location) return Node_Id;
   function P_For_Cond_Struct (Start_Loc : Location) return Node_Id;
   function P_While_Cond_Struct (Start_Loc : Location) return Node_Id;
   function P_Basic_Action (Start_Loc : Location) return Node_Id;
   function P_Timed_Action (Start_Loc : Location) return Node_Id;

   function P_Assignment_Or_Communication_Action
     (Start_Loc : Location) return Node_Id;

   function P_Range (Container : Node_Id; Start_Loc : Location) return Node_Id;

   function P_Subprogram_Parameter_List
     (Container : Ocarina.Types.Node_Id) return List_Id;

   function P_Parameter_Label (Container : Ocarina.Types.Node_Id)
                              return Node_Id;

   ------------------------
   -- P_Behavior_Actions --
   ------------------------

   --  behavior_actions ::= { behavior_action ; }+

   function P_Behavior_Actions (Container : Node_Id) return List_Id is
   begin
      return P_Elements_List
          (P_Behavior_Action'Access,
           Container,
           (T_Right_Curly_Bracket, T_None),
           PC_Behavior_Actions);
   end P_Behavior_Actions;

   -----------------------
   -- P_Behavior_Action --
   -----------------------

   --  behavior_action ::=
   --    basic_action ;
   --  | if ( logical_expression ) { behavior_action }+
   --    { elsif ( logical_expression ) { behavior_action }+ }*
   --    [ else { behavior_action }+ ]
   --    end if ;
   --  | for ( loop_variable_identifier in range )
   --           { { behavior_action }+ };
   --  | while ( logical_expression ) { { behavior_action }+ };

   function P_Behavior_Action (Container : Node_Id) return Node_Id is
      Start_Loc : Location;

      Action_Node          : Node_Id;
      Behavior_Action_Node : Node_Id;
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

         when T_Identifier | T_Computation | T_Delay =>
            Action_Node := P_Basic_Action (Start_Loc);

         when others =>
            DPE
              (PC_Behavior_Action,
               Expected_Tokens =>
                 (T_If, T_For, T_While, T_Identifier, T_Computation, T_Delay));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Behavior_Action_Node :=
        Add_New_Behavior_Action (Start_Loc, Container, Action_Node);
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
      Elsif_Node     : Node_Id := No_Node;
      Else_Node      : Node_Id := No_Node;
   begin
      If_Cond_Struct := Add_New_If_Cond_Struct (Start_Loc);

      if Token /= T_If then
         DPE (PC_If_Cond_Struct, Expected_Token => T_If);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      If_Node :=
        P_Conditional_Statement (If_Cond_Struct, PC_If_Cond_Statement);
      if No (If_Node) then
         DPE (PC_If_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Elsif then
         Elsif_Node :=
           P_Conditional_Statement (If_Cond_Struct, PC_Elsif_Cond_Statement);
      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Else then
         Else_Node :=
           P_Conditional_Statement (If_Cond_Struct, PC_Else_Cond_Statement);
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

      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_If_Cond_Struct, Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_If_Cond_Struct
        (If_Cond_Struct,
         No_Node,
         If_Node,
         Elsif_Node,
         Else_Node);
      return If_Cond_Struct;
   end P_If_Cond_Struct;

   -----------------------------
   -- P_Conditional_Statement --
   -----------------------------

   function P_Conditional_Statement
     (Container : Node_Id;
      Code      : Parsing_Code) return Node_Id
   is
      Start_Loc      : Location;
      Loc            : Location;
      Cond_Stat_Node : Node_Id;
      Expression     : Node_Id := No_Node;
      Actions        : List_Id := No_List;
   begin
      Save_Lexer (Start_Loc);
      Scan_Token;

      if Code /= PC_Else_Cond_Statement then
         if Token /= T_Left_Parenthesis then
            DPE
              (PC_Conditional_Statement,
               Expected_Token => T_Left_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Expression := P_Value_Expression (No_Node);
         if No (Expression) then
            DPE (PC_Conditional_Statement, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Right_Parenthesis then
            DPE
              (PC_Conditional_Statement,
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
         when T_If       |
           T_For         |
           T_While       |
           T_Identifier  |
           T_Computation |
           T_Delay       =>
            Restore_Lexer (Loc);
            Actions := P_Behavior_Actions (No_Node);

         when others =>
            Restore_Lexer (Loc);
      end case;

      Cond_Stat_Node :=
        Add_New_Conditional_Statement
          (Start_Loc,
           Container,
           Expression,
           Actions);
      if No (Cond_Stat_Node) then
         DPE (PC_Conditional_Statement, EMC_Failed);
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

      For_Cond_Struct : Node_Id;
      Var_Identifier  : Node_Id;
      Range_Node      : Node_Id;
      Actions         : List_Id;
   begin
      For_Cond_Struct := Add_New_For_Cond_Struct (Start_Loc);
      if No (For_Cond_Struct) then
         DPE (PC_For_Cond_Struct, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Parenthesis then
         DPE (PC_For_Cond_Struct, Expected_Token => T_Left_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Var_Identifier := P_Identifier (For_Cond_Struct);
      if No (Var_Identifier) then
         DPE (PC_For_Cond_Struct, Expected_Token => T_Identifier);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_In then
         DPE (PC_For_Cond_Struct, Expected_Token => T_In);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Range_Node := P_Range (For_Cond_Struct, Loc);
      if No (Range_Node) then
         DPE (PC_For_Cond_Struct, EMC_Invalid_Range);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Parenthesis then
         DPE (PC_For_Cond_Struct, Expected_Token => T_Right_Parenthesis);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Left_Curly_Bracket then
         DPE (PC_For_Cond_Struct, Expected_Token => T_Left_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Actions := P_Behavior_Actions (For_Cond_Struct);
      if Is_Empty (Actions) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Scan_Token;
      if Token /= T_Right_Curly_Bracket then
         DPE (PC_For_Cond_Struct, Expected_Token => T_Right_Curly_Bracket);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Add_New_For_Cond_Struct
        (For_Cond_Struct,
         No_Node,
         Var_Identifier,
         Range_Node,
         Actions);

      return For_Cond_Struct;

   end P_For_Cond_Struct;

   -------------------------
   -- P_While_Cond_Struct --
   -------------------------

   function P_While_Cond_Struct (Start_Loc : Location) return Node_Id is

      pragma Assert (Token = T_While);

      While_Cond_Struct : constant Node_Id :=
        New_Node (K_While_Cond_Struct, Start_Loc);
      Cond_Stat_Node : Node_Id;
   begin
      if No (While_Cond_Struct) then
         DPE (PC_While_Cond_Statement, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Cond_Stat_Node :=
        P_Conditional_Statement (While_Cond_Struct, PC_While_Cond_Struct);

      Set_While_Statement (While_Cond_Struct, Cond_Stat_Node);

      return While_Cond_Struct;
   end P_While_Cond_Struct;

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

         when T_Delay | T_Computation =>
            return P_Timed_Action (Start_Loc);

         when others =>
            DPE
              (PC_Basic_Action,
               Expected_Tokens => (T_Identifier, T_Delay, T_Computation));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;
   end P_Basic_Action;

   ------------------------------------------
   -- P_Assignment_Or_Communication_Action --
   ------------------------------------------

   --  assignment_action::=
   --    target := value_expression | any

   --  target ::=
   --    local_variable_id
   --  | outgoing_port_id
   --  | outgoing_parameter_id
   --  | data_component_reference

   --  communication_action ::=
   --    required_subprogram_access_id ! [ ( subprogram_parameter_list ) ]
   --  | subprogram_classifier_id ! [ ( subprogram_parameter_list ) ]
   --  | output_port_id ! [ ( value_expression ) ]
   --  | input_port_id >>
   --  | input_port_id ? [ ( target ) ]

   function P_Assignment_Or_Communication_Action
     (Start_Loc : Location) return Node_Id
   is
      Loc            : Location;
      Loc2           : Location;
      Node           : Node_Id;
      Ident          : Node_Id;
      Value_Expr     : Node_Id            := No_Node;
      Target         : Node_Id            := No_Node;
      Sub_Parameters : List_Id            := No_List;
      Com_Kind       : Communication_Kind := CK_No_Kind;
      IS_Any_Bool    : Boolean            := False;
   begin
      Restore_Lexer (Start_Loc);
      Ident := P_Id (No_Node);

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Dot then
         Restore_Lexer (Start_Loc);
         Ident := P_Data_Component_Reference (No_Node);
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
            DPE
              (PC_Assignment_Or_Communication_Action,
               Expected_Tokens =>
                 (T_Exclamation,
                  T_Interrogative,
                  T_Greater_Greater_Than,
                  T_Assignment));
            Skip_Tokens (T_Semicolon);
            return No_Node;
      end case;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Left_Parenthesis then
         if Com_Kind /= CK_Greater_Greater
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
            Target := P_Id (No_Node);

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

         elsif Com_Kind = CK_Greater_Greater then
            DPE (PC_Assignment_Or_Communication_Action, EMC_Illegal_Syntax);
            Skip_Tokens (T_Semicolon);
            return No_Node;

         end if;

         Scan_Token;
         if Token /= T_Right_Parenthesis then
            DPE
              (PC_Assignment_Or_Communication_Action,
               Expected_Token => T_Right_Parenthesis);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

      elsif Token = T_Less_Than_Sign then
         --  Parsed "!<"
         Com_Kind := CK_Exclamation_Less_Than;

      elsif Token = T_Greater_Than_Sign then
         --  Parsed "!<"
         Com_Kind := CK_Exclamation_Greater_Than;

      else
         Restore_Lexer (Loc);
      end if;

      Save_Lexer (Loc);
      Scan_Token;

      if Token = T_Right_Curly_Bracket then
         Restore_Lexer (Loc);

      elsif Token /= T_Semicolon then
         DPE
           (PC_Assignment_Or_Communication_Action,
            Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      if No (Value_Expr) then
         Node :=
           Add_New_Communication_Action
             (Start_Loc,
              No_Node,
              Ident,
              Target,
              Sub_Parameters,
              Com_Kind);
      else
         Node :=
           Add_New_Assignment_Action
             (Start_Loc,
              No_Node,
              Ident,
              Value_Expr,
              IS_Any_Bool);
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
   --    computation ( behavior_time [ , behavior_time [ , distribution ] ] )
   --  | delay ( behavior_time [ , behavior_time [ , distribution ] ] )

   --  distribution ::= fixed | normal | poisson | random

   function P_Timed_Action (Start_Loc : Location) return Node_Id is
      Loc            : Location;
      Timed_Action   : Node_Id;
      Fst_Behav_Time : Node_Id;
      Scd_Behav_Time : Node_Id           := No_Node;
      Is_Comput      : Boolean           := False;
      Distribution   : Distribution_Kind := DK_No_Kind;
   begin
      Restore_Lexer (Start_Loc);

      Scan_Token;
      if Token = T_Computation then
         Is_Comput := True;
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

      Scan_Token;
      if Token = T_Comma then
         Save_Lexer (Loc);

         Scan_Token;
         if Token /= T_Fixed
           or else Token /= T_Normal
           or else Token /= T_Poisson
           or else Token /= T_Random
           or else Token /= T_Left_Parenthesis
         then
            Restore_Lexer (Loc);
            Scd_Behav_Time := P_Behavior_Time (No_Node);
            if No (Scd_Behav_Time) then
               DPE (PC_Timed_Action, EMC_Failed);
               Skip_Tokens (T_Semicolon);
               return No_Node;
            end if;
         else
            Restore_Lexer (Loc);
         end if;

         Save_Lexer (Loc);
         Scan_Token;
         case Token is
            when T_Fixed =>
               Distribution := DK_Fixed;

            when T_Normal =>
               Distribution := DK_Normal;

            when T_Poisson =>
               Distribution := DK_Poisson;

            when T_Random =>
               Distribution := DK_Random;

            when T_Left_Parenthesis =>
               Restore_Lexer (Loc);

            when others =>
               DPE
                 (PC_Timed_Action,
                  Expected_Tokens =>
                    (T_Fixed,
                     T_Normal,
                     T_Poisson,
                     T_Random,
                     T_Right_Parenthesis));
               Skip_Tokens (T_Semicolon);
               return No_Node;
         end case;
      end if;

      Scan_Token;
      if Token /= T_Semicolon then
         DPE (PC_Timed_Action, Expected_Token => T_Semicolon);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Timed_Action :=
        Add_New_Timed_Action
          (Start_Loc,
           No_Node,
           Fst_Behav_Time,
           Scd_Behav_Time,
           Distribution,
           Is_Comput);
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
   --    | data_subcomponent_identifier [ . data_subcomponent_identifier ]
   --    | data_access_feature_identifier [ . data_subcomponent_identifier ]

   function P_Data_Component_Reference (Container : Node_Id) return Node_Id is
      Loc           : Location;
      Idents        : List_Id;
      Data_Comp_Ref : Node_Id;
   begin
      Save_Lexer (Loc);
      Idents := P_Items_List (P_Identifier'Access, Container, T_Dot);
      if Is_Empty (Idents) then
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Data_Comp_Ref :=
        Add_New_Data_Component_Reference (Loc, Container, Idents);
      if No (Data_Comp_Ref) then
         DPE (PC_Data_Component_Reference, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Data_Comp_Ref;
      end if;

   end P_Data_Component_Reference;

   -------------
   -- P_Range --
   -------------

   --  range ::=
   --    integer_range
   --  | event_data_port_id
   --  | array_data_component_reference  --  fixme : todo

   function P_Range
     (Container : Node_Id;
      Start_Loc : Location) return Node_Id
   is

      Loc        : Location;
      Node_Range : Node_Id;
   begin
      Scan_Token;
      if Token = T_Identifier then
         Restore_Lexer (Start_Loc);
         Node_Range := P_Id (No_Node);

         Save_Lexer (Loc);
         Scan_Token;

         if Token = T_Interval then
            Restore_Lexer (Start_Loc);
            Node_Range := P_Integer_Range (Container);
         else
            Restore_Lexer (Loc);
         end if;
      else
         Restore_Lexer (Start_Loc);
         Node_Range := P_Integer_Range (Container);
      end if;

      if No (Node_Range) then
         DPE (PC_Range, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Node_Range;
      end if;

   end P_Range;

   ---------------------------------
   -- P_Subprogram_Paremeter_List --
   ---------------------------------

   --  subprogram_parameter_list ::= parameter_label { , parameter_label } *

   function P_Subprogram_Parameter_List
     (Container : Ocarina.Types.Node_Id) return List_Id
   is
   begin
      return P_Items_List (P_Parameter_Label'Access, Container, T_Comma);
   end P_Subprogram_Parameter_List;

   -----------------------
   -- P_Parameter_Label --
   -----------------------

   --  parameter_label ::= in_parameter_value_expression | out_parameter_target

   function P_Parameter_Label (Container : Ocarina.Types.Node_Id)
                              return Node_Id is
      Start_Loc   : Location;
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

   ----------
   -- P_Id --
   ----------

   --  id ::= identifier { [ integer_value_holder ] }*

   function P_Id (Container : Ocarina.Types.Node_Id) return Node_Id is
      Start_Loc    : Location;
      Loc          : Location;
      Id_Node      : Node_Id;
      Ident        : Node_Id;
      Value_Holder : Node_Id := No_Node;
   begin
      Save_Lexer (Start_Loc);

      Ident := P_Identifier (Container);
      if No (Ident) then
         DPE (PC_Id, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      end if;

      Save_Lexer (Loc);
      Scan_Token;
      if Token = T_Left_Square_Bracket then

         Value_Holder := P_Value_Holder (No_Node);
         if No (Value_Holder) then
            DPE (PC_Id, EMC_Failed);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;

         Scan_Token;
         if Token /= T_Right_Square_Bracket then
            DPE (PC_Id, Expected_Token => T_Right_Square_Bracket);
            Skip_Tokens (T_Semicolon);
            return No_Node;
         end if;
      else
         Restore_Lexer (Loc);
      end if;

      Id_Node := Add_New_Id (Start_Loc, Container, Ident, Value_Holder);
      if No (Id_Node) then
         DPE (PC_Id, EMC_Failed);
         Skip_Tokens (T_Semicolon);
         return No_Node;
      else
         return Id_Node;
      end if;

   end P_Id;

end Ocarina.FE_AADL_BA.Parser.Actions;
