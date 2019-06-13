------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . B A C K E N D S . C _ C O M M O N . B A          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2019 ESA & ISAE.                       --
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
with Locations;
with Utils;
with Ocarina.Backends.Utils;

with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.Properties;
with Ocarina.Backends.C_Common.Mapping;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;
with Ocarina.Backends.Helper;
with Ocarina.Analyzer.AADL_BA;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.C_Common.Types;

package body Ocarina.Backends.C_Common.BA is

   use Ocarina.Analyzer.AADL_BA;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Namet;
   use Locations;
   use Ocarina.Backends.Helper;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.C_Tree.Nodes;
   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.PO_HI_C.Runtime;

   package AAN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package BATN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   package BANu renames Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   package CV renames Ocarina.Backends.C_Values;

   --  function Get_Instances_Of_Component_Type
   --    (Root : Node_Id; E : Node_Id) return Node_Id;

   function Get_Behavior_Specification
     (S : Node_Id) return Node_Id;

   procedure Map_C_Behavior_Action_Block
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_Behav_Acts
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      WStatements  : List_Id);

   procedure Map_C_Behavior_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   function Map_C_Elsif_stat
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id;
      Else_st      : Node_Id) return List_Id;

   procedure Map_C_If_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_For_or_ForAll_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_While_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Map_C_Communication_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Make_Send_Output_Port
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id);

   procedure Make_Put_Value_On_port
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   procedure Make_Output_Port_Name
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id);

   procedure Map_C_Assignment_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);

   function Evaluate_BA_Value_Expression
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Relation
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Operator (Node : Node_Id) return Operator_Type;

      function Evaluate_BA_Simple_Expression
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Term
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Factor
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Value
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Integer_Value
     (Node         : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id) return Node_Id;

   function Evaluate_BA_Literal (Node : Node_Id) return Node_Id;

   function Evaluate_BA_Property_Constant
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

--     function Get_Port_Spec_Instance
--       (Node             : Node_Id;
--        Parent_Component : Node_Id) return Node_Id;

   procedure Make_Next_Value_of_Port
     (Node             : Node_Id;
      Subprogram_Root  : Node_Id;
      Statements       : List_Id);

   procedure Make_Request_Variable_Declaration
     (Declarations     : List_Id);

   function Make_Get_Value_of_Port
     (Node             : Node_Id;
      Subprogram_Root  : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Identifier
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id;

   function Evaluate_BA_Boolean_Literal (Node : Node_Id) return Node_Id;

   --     -------------------------------------
   --     -- Get_Instances_Of_Component_Type --
   --     -------------------------------------
   --
   --     function Get_Instances_Of_Component_Type
   --       (Root : Node_Id; E : Node_Id) return Node_Id
   --     is
   --        use type Aan.Node_Kind;
   --
   --        pragma Assert
   --          (AAN.Kind (E) = Aan.K_Component_Type
   --           or else AAN.Kind (E) = Aan.K_Component_Implementation
   --           or else AAN.Kind (E) = Aan.K_Feature_Group_Type);
   --
   --        Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
   --          := Features_Of (Root);
   --     begin
   --        for F of Fs loop
   --           if AIN.Corresponding_Declaration
   --             (AIN.Corresponding_Instance (F)) = E
   --           then
   --              return AIN.Corresponding_Instance (F);
   --           end if;
   --
   --        end loop;
   --
   --        --  raise Program_Error;
   --        return No_Node;
   --
   --     end Get_Instances_Of_Component_Type;

   --------------------------------
   -- Get_Behavior_Specification --
   --------------------------------

   function Get_Behavior_Specification
     (S : Node_Id) return Node_Id is
      D, BA  : Node_Id;
   begin
      D := AIN.First_Node (AIN.Annexes (S));
      BA := No_Node;
      while No (BA) loop
         if (Standard.Utils.To_Upper (AIN.Display_Name (AIN.Identifier (D))) =
               Standard.Utils.To_Upper (Get_String_Name
                                          ("behavior_specification")))
           and then Present (AIN.Corresponding_Annex (D))
         then
            BA := AIN.Corresponding_Annex (D);
            return BA;
         end if;
         D := AIN.Next_Node (D);
      end loop;

      raise Program_Error;

      return No_Node;
   end Get_Behavior_Specification;

   ------------------------------
   -- Map_C_Behavior_Variables --
   ------------------------------

   procedure Map_C_Behavior_Variables (S            : Node_Id;
                                       Declarations : List_Id)
   is
      BA, P, T      : Node_Id;
      Data_Instance : Node_Id;
   begin

      BA := Get_Behavior_Specification (S);
      if not BANu.Is_Empty (BATN.Variables (BA)) then
         P := BATN.First_Node (BATN.Variables (BA));
         loop
            T := BATN.First_Node (BATN.Identifiers (P));
            loop
               if Present (BATN.Corresponding_Declaration
                           (BATN.Classifier_Ref (P)))
                 and then Present (AAN.Default_Instance
                                   (BATN.Corresponding_Declaration
                                      (BATN.Classifier_Ref (P))))
               then

                  Data_Instance := AAN.Default_Instance
                    (BATN.Corresponding_Declaration
                       (BATN.Classifier_Ref (P)));

                  if No (AIN.Backend_Node
                         (AIN.Identifier (Data_Instance)))
                  then
                     Ocarina.Backends.C_Common.Types.Header_File.Visit
                       (Data_Instance);
                  end if;

                  CTU.Append_Node_To_List
                    (CTU.Make_Variable_Declaration
                       (Defining_Identifier => CTU.Make_Defining_Identifier
                            (BATN.Display_Name (T)),
                        Used_Type =>  Map_C_Data_Type_Designator
                          (Data_Instance)),
                     Declarations);

               else
                  if not BANu.Is_Empty (BATN.Package_Name
                                        (BATN.Classifier_Ref (P)))
                  then
                     Display_Error
                       (" The mapping of BA variable type ("
                        & Get_Name_String (BATN.Display_Name
                          (BATN.Full_Identifier (BATN.Classifier_Ref (P))))
                        & ") at " & Image (BATN.Loc (BATN.Classifier_Ref (P)))
                        & ", is not yet supported",
                        Fatal => True);
                  else
                     if Present (BATN.Component_Impl (BATN.Classifier_Ref (P)))
                     then
                        Display_Error
                          (" The mapping of BA variable type ("
                           & Get_Name_String
                             (Standard.Utils.Remove_Prefix_From_Name
                                  ("%ba%", BATN.Name (BATN.Component_Type
                                   (BATN.Classifier_Ref (P)))))
                           & "."
                           & Get_Name_String
                             (Standard.Utils.Remove_Prefix_From_Name
                                  ("%ba%", BATN.Name (BATN.Component_Impl
                                   (BATN.Classifier_Ref (P)))))
                           & ") at "
                           & Image (BATN.Loc (BATN.Classifier_Ref (P)))
                           & ", is not yet supported",
                           Fatal => True);
                     else
                        Display_Error
                          (" The mapping of BA variable type ("
                           & Get_Name_String
                             (Standard.Utils.Remove_Prefix_From_Name
                                  ("%ba%", BATN.Name (BATN.Component_Type
                                   (BATN.Classifier_Ref (P)))))
                           & ") at "
                           & Image (BATN.Loc (BATN.Classifier_Ref (P)))
                           & ", is not yet supported",
                           Fatal => True);
                     end if;
                  end if;
               end if;

               T := BATN.Next_Node (T);
               exit when No (T);
            end loop;
            P := BATN.Next_Node (P);
            exit when No (P);
         end loop;
      end if;

   end Map_C_Behavior_Variables;

   ----------------------------
   -- Map_C_Behavior_Actions --
   ----------------------------

   procedure Map_C_Behavior_Actions (S            : Node_Id;
                                     Declarations : List_Id;
                                     Statements   :  List_Id)
   is
      BA                 : Node_Id;
      behav_transition   : Node_Id;
      Transition_Node    : Node_Id;
   begin

      BA := Get_Behavior_Specification (S);

      --  For an AADL subprogram with BA, we have: a single state
      --  as initial final state; a single transition without
      --  condition with a Behavior_Action_Block.
      --  Thus, we need to map the Behavior_Action_Block
      --  To C-statements in the generated C-subprogram
      --
      if not BANu.Is_Empty (BATN.Transitions (BA)) then

         behav_transition := BATN.First_Node (BATN.Transitions (BA));
         Transition_Node := BATN.Transition (behav_transition);

         if Present (behav_transition) and then
           Present (BATN.Behavior_Action_Block (Transition_Node))
         then
            if BATN.Kind (Transition_Node) =
              BATN.K_Execution_Behavior_Transition
            then
               Map_C_Behavior_Action_Block
                 (BATN.Behavior_Action_Block (Transition_Node),
                  S, Declarations, Statements);
            else
               --  i.e. Kind (Transition_Node) = K_Mode_Transition
               --  We do not support Mode transition in a subprogram
               --
               Display_Error
                 ("Mode Transition is not supported for a subprogram",
                  Fatal => True);
            end if;

         end if;

      end if;

   end Map_C_Behavior_Actions;

   ---------------------------------
   -- Map_C_Behavior_Action_Block --
   ---------------------------------

   procedure Map_C_Behavior_Action_Block
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action_Block);
   begin

      Map_C_Behav_Acts (Node, S, Declarations, Statements);

      --  Behavior_Time (Node) is not yet supported
      --
      --  if Present (Behavior_Time (Node)) then
      --
      --  end if;

   end Map_C_Behavior_Action_Block;

   ----------------------
   -- Map_C_Behav_Acts --
   ----------------------

   procedure Map_C_Behav_Acts
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      WStatements  : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action_Block
                     or else BATN.Kind (Node) = K_Conditional_Statement
                     or else BATN.Kind (Node) = K_While_Cond_Structure
                     or else BATN.Kind (Node) = K_For_Cond_Structure
                     or else BATN.Kind (Node) = K_ForAll_Cond_Structure);

      pragma Assert (Present (BATN.Behav_Acts (Node)));

      Behav_actions   : Node_Id;
      Behav_action    : Node_Id;
   begin

      Behav_actions := BATN.Behav_Acts (Node);

      --  In the case of a sequence of behavior actions:
      --  It is mapped into a sequence of C-statement, i.e.
      --  each action is mapped to the corresponding C-statement.
      --  Statement sequence is writen in the same order as the
      --  action sequence.
      --
      if not BANu.Is_Empty (BATN.Behavior_Action_Sequence (Behav_actions))
      then

         Behav_action := BATN.First_Node
           (BATN.Behavior_Action_Sequence (Behav_actions));

         while Present (Behav_action) loop
            Map_C_Behavior_Action (Behav_action, S, Declarations, WStatements);
            Behav_action := BATN.Next_Node (Behav_action);
         end loop;
      end if;

      --  In the case of a set of behavior actions:
      --  we treat it as a sequence of behavior actions.
      --
      if not BANu.Is_Empty (BATN.Behavior_Action_Set (Behav_actions)) then
         Behav_action := BATN.First_Node
           (BATN.Behavior_Action_Set (Behav_actions));

         while Present (Behav_action) loop
            Map_C_Behavior_Action (Behav_action, S, Declarations, WStatements);
            Behav_action := BATN.Next_Node (Behav_action);
         end loop;
      end if;

      --  In the case of a single action: it is mapped
      --  into a C-statement.
      --
      if Present (BATN.Behavior_Action (Behav_actions)) and then
        BANu.Is_Empty (BATN.Behavior_Action_Sequence (Behav_actions))
        and then BANu.Is_Empty (BATN.Behavior_Action_Set (Behav_actions))
      then

         Map_C_Behavior_Action (BATN.Behavior_Action (Behav_actions), S,
                                Declarations, WStatements);
      end if;

   end Map_C_Behav_Acts;

   ---------------------------
   -- Map_C_Behavior_Action --
   ---------------------------

   procedure Map_C_Behavior_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action);
      pragma Assert (BATN.Kind (BATN.Action (Node)) = BATN.K_If_Cond_Struct
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_For_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_While_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_ForAll_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_DoUntil_Cond_Structure
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_Assignment_Action
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_Communication_Action
                     or else BATN.Kind (BATN.Action (Node)) =
                       BATN.K_Timed_Act);

      Action_Node : constant Node_Id := BATN.Action (Node);
      N           : Node_Id;
   begin
      case BATN.Kind (Action_Node) is

         when K_If_Cond_Struct         =>
            Map_C_If_Cond_Struct (Action_Node, S, Declarations, Statements);

         when K_For_Cond_Structure     =>
            Map_C_For_or_ForAll_Cond_Struct
              (Action_Node, S, Declarations, Statements);

         when K_While_Cond_Structure   =>
            Map_C_While_Cond_Struct (Action_Node, S, Declarations, Statements);

         when K_ForAll_Cond_Structure  =>
            Map_C_For_or_ForAll_Cond_Struct
              (Action_Node, S, Declarations, Statements);

            --  when K_DoUntil_Cond_Structure =>
            --    Map_C_DoUntil_Cond_Struct (Action_Node);

         when BATN.K_Assignment_Action      =>
            Map_C_Assignment_Action (Action_Node, S, Declarations, Statements);

         when K_Communication_Action   =>
            Map_C_Communication_Action (Action_Node, S,
                                        Declarations, Statements);

            --  when K_Timed_Act           =>
            --    Map_C_Timed_Action (Action_Node);

         when others                   =>
            N := Message_Comment ("Behavior Actions not yet mapped");
            CTU.Append_Node_To_List (N, Statements);
      end case;

   end Map_C_Behavior_Action;

   ----------------------
   -- Map_C_Elsif_stat --
   ----------------------

   function Map_C_Elsif_stat
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id;
      Else_st      : Node_Id) return List_Id
   is
      N                : Node_Id;
      Else_stats       : constant List_Id := New_List (CTN.K_Statement_List);
      elsif_statements : constant List_Id := New_List (CTN.K_Statement_List);
   begin
      N := BATN.Next_Node (Node);

      if Present (N) then
         Map_C_Behav_Acts
           (Node         => Node,
            S            => S,
            Declarations => Declarations,
            WStatements  => elsif_statements);

         CTU.Append_Node_To_List
           (CTU.Make_If_Statement
              (Condition       => Evaluate_BA_Value_Expression
                   (Node             => Logical_Expr (Node),
                    Subprogram_Root  => S,
                    Declarations     => Declarations,
                    Statements       => Statements),
               Statements      => elsif_statements,
               Else_Statements =>
                 Map_C_Elsif_stat
                   (Node         => N,
                    S            => S,
                    Declarations => Declarations,
                    Statements   => Statements,
                    Else_st      => Else_st)),
            Else_stats);
      else
         Map_C_Behav_Acts
           (Node         => Node,
            S            => S,
            Declarations => Declarations,
            WStatements  => elsif_statements);
         declare
            else_sts : constant List_Id := New_List (CTN.K_Statement_List);
         begin
            if Present (Else_st) then

               Map_C_Behav_Acts
                 (Node         => Else_st,
                  S            => S,
                  Declarations => Declarations,
                  WStatements  => else_sts);

               CTU.Append_Node_To_List
                 (CTU.Make_If_Statement
                    (Condition       => Evaluate_BA_Value_Expression
                         (Node             => Logical_Expr (Node),
                          Subprogram_Root  => S,
                          Declarations     => Declarations,
                          Statements       => Statements),
                     Statements      => elsif_statements,
                     Else_Statements => else_sts),
                  Else_stats);
            else
               CTU.Append_Node_To_List
                 (CTU.Make_If_Statement
                    (Condition       => Evaluate_BA_Value_Expression
                         (Node             => Logical_Expr (Node),
                          Subprogram_Root  => S,
                          Declarations     => Declarations,
                          Statements       => Statements),
                     Statements      => elsif_statements),
                  Else_stats);
            end if;
         end;
      end if;

      return Else_stats;

   end Map_C_Elsif_stat;

   --------------------------
   -- Map_C_If_Cond_Struct --
   --------------------------

   procedure Map_C_If_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is

      pragma Assert (BATN.Kind (Node) = K_If_Cond_Struct);

      pragma Assert (Present (Logical_Expr (If_Statement (Node))));
      pragma Assert (Present (Behav_Acts (If_Statement (Node))));

      Condition        : Node_Id;
      else_Statements  : List_Id;
      if_Statements    : constant List_Id := New_List (CTN.K_Statement_List);
      List_Node1       : Node_Id;
   begin

      Condition := Evaluate_BA_Value_Expression
        (Node             => Logical_Expr (If_Statement (Node)),
         Subprogram_Root  => S,
         Declarations     => Declarations,
         Statements       => Statements);

      Map_C_Behav_Acts
        (Node         => If_Statement (Node),
         S            => S,
         Declarations => Declarations,
         WStatements  => if_Statements);

      if not BANu.Is_Empty (Elsif_Statement (Node)) then

         List_Node1 := BATN.First_Node (Elsif_Statement (Node));

         else_statements := Map_C_Elsif_stat (List_Node1, S, Declarations,
                           Statements, Else_Statement (Node));
      else
         if Present (Else_Statement (Node)) then
            else_statements := New_List (CTN.K_Statement_List);
            Map_C_Behav_Acts
              (Node         => Else_Statement (Node),
               S            => S,
               Declarations => Declarations,
               WStatements  => else_statements);
         else
            else_statements := No_List;
         end if;
      end if;

      CTU.Append_Node_To_List
        (CTU.Make_If_Statement
           (Condition       => Condition,
            Statements      => if_Statements,
            Else_Statements => else_statements),
         Statements);

   end Map_C_If_Cond_Struct;

   -------------------------------------
   -- Map_C_For_or_ForAll_Cond_Struct --
   -------------------------------------

   procedure Map_C_For_or_ForAll_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is

      pragma Assert (BATN.Kind (Node) = K_For_Cond_Structure
                     or else BATN.Kind (Node) = K_ForAll_Cond_Structure);

      Pre_Cond       : Node_Id;
      Post_Cond      : Node_Id;
      Used_Type      : Node_Id;
      Data_Instance  : Node_Id;
      init_value     : Node_Id;
      Condition      : Node_Id;
      For_Statements : constant List_Id := New_List (CTN.K_Statement_List);
   begin

      if BATN.Kind (In_Element_Values (Node)) = BATN.K_Integer_Range then

         Data_Instance := AAN.Default_Instance
           (BATN.Corresponding_Declaration
              (BATN.Classifier_Ref (Node)));

         if No (AIN.Backend_Node
                (AIN.Identifier (Data_Instance)))
         then
            Ocarina.Backends.C_Common.Types.Header_File.Visit
              (Data_Instance);
         end if;

         Used_Type :=
           Ocarina.Backends.C_Common.Mapping.
             Map_C_Data_Type_Designator (Data_Instance);

         init_value := Evaluate_BA_Integer_Value
           (BATN.Lower_Int_Val (In_Element_Values (Node)),
            Declarations,
            Statements);

         Pre_Cond := Make_Variable_Declaration
           (Defining_Identifier => Make_Defining_Identifier
              (BATN.Display_Name (Element_Idt (Node))),
            Used_Type           => Used_Type,
            Value               => init_value);

         Condition := CTU.Make_Expression
           (Left_Expr  => Make_Defining_Identifier
              (BATN.Display_Name (Element_Idt (Node))),
            Operator   => CTU.Op_Less_Equal,
            Right_Expr => Evaluate_BA_Integer_Value
              (BATN.Upper_Int_Val (In_Element_Values (Node)),
               Declarations,
               Statements));

         Post_Cond := CTU.Make_Expression
              (Left_Expr  => Make_Defining_Identifier
                   (BATN.Display_Name (Element_Idt (Node))),
               Operator   => CTU.Op_Plus_Plus,
               Right_Expr => No_Node);

         Map_C_Behav_Acts
           (Node         => Node,
            S            => S,
            Declarations => Declarations,
            WStatements  => For_Statements);

         CTU.Append_Node_To_List
           (CTU.Make_For_Statement
              (Pre_Cond   => Pre_Cond,
               Condition  => Condition,
               Post_Cond  => Post_Cond,
               Statements => For_Statements),
            Statements);

      else
         Display_Error ("In For/ForAll construct, Kinds"
                        & " other than K_Integer_Range for In_Element_Values"
                        & " are not yet supported", Fatal => True);
      end if;

   end Map_C_For_or_ForAll_Cond_Struct;

   -----------------------------
   -- Map_C_While_Cond_Struct --
   -----------------------------

   procedure Map_C_While_Cond_Struct
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is

      pragma Assert (BATN.Kind (Node) = K_While_Cond_Structure);

      pragma Assert (Present (Logical_Expr (Node)));
      pragma Assert (Present (Behav_Acts (Node)));

      Condition        : Node_Id;
      while_statements : constant List_Id := New_List (CTN.K_Statement_List);
   begin

      Condition := Evaluate_BA_Value_Expression
        (Node             => Logical_Expr (Node),
         Subprogram_Root  => S,
         Declarations     => Declarations,
         Statements       => Statements);

      Map_C_Behav_Acts
        (Node         => Node,
         S            => S,
         Declarations => Declarations,
         WStatements  => while_statements);

      CTU.Append_Node_To_List
        (CTU.Make_While_Statement
           (Condition  => Condition,
            Statements => while_statements),
         Statements);

   end Map_C_While_Cond_Struct;

   --------------------------------
   -- Map_C_Communication_Action --
   --------------------------------

   procedure Map_C_Communication_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Communication_Action);

      Called_Spg               : Node_Id;
      Called_Spg_Instance      : Node_Id;
      Called_Spg_Spec          : Node_Id;
      Var_identifier           : Node_Id;
      Call_Parameters          : List_Id;
      N, k                     : Node_Id;
      Param_Node               : Node_Id;
      BA_Root                  : constant Node_Id
        := Get_Behavior_Specification (S);
      decl                     : Node_Id;
      Called_Spg_Spec_Exist    : Boolean := False;
   begin

      if Kind (BATN.Identifier (Node)) = BATN.K_Name then
         Var_identifier := CTU.Make_Defining_Identifier
           (Name => BATN.Display_Name
              (BATN.First_Node
                   (BATN.Idt (BATN.Identifier (Node)))),
            Pointer => False);

         --  else
         --  i.e.
         --     Kind (BATN.Identifier (Node))
         --       = BATN.K_Data_Component_Reference
         --  We must treat this case in the future
      end if;

      case Communication_Kind'Val (Comm_Kind (Node)) is

         when CK_Exclamation      =>

            if Is_Subprogram_Call (Node) then

               --  This means we have a call to a subprogram
               --

               --  If not yet added to the declarations of the
               --  current source file we must add the definition
               --  of the called spg

               --  First, we search if the called Spg is already declared
               --  in the current file, i.e. the called spg is not the
               --  first time is called
               --
               decl := CTN.First_Node (CTN.Declarations (Current_File));
               while Present (decl) loop

                  if Kind (decl) = CTN.K_Function_Specification
                    --  CTN.K_Extern_Entity_Declaration
                    and then
                      Get_Name_String
                        (Standard.Utils.To_Lower
                           (CTN.Name (CTN.Defining_Identifier
                            (decl))))
                    --   (CTN.Entity (decl)))))
                    = Get_Name_String
                    (Standard.Utils.To_Lower
                       (CTN.Name (Var_identifier)))

                  then
                     Called_Spg_Spec_Exist := True;
                  end if;
                  exit when Called_Spg_Spec_Exist;
                  decl := CTN.Next_Node (decl);
               end loop;

               if not Called_Spg_Spec_Exist then

                  Called_Spg :=
                    BATN.Corresponding_Entity
                      (BATN.First_Node
                         (BATN.Idt (BATN.Identifier (Node))));

                  Called_Spg_Instance := AAN.Default_Instance (Called_Spg);

                  declare
                     Proxy_Instance : Node_Id;
                  begin
                     if AINU.Is_Thread (S) then
                        Proxy_Instance := S;
                     elsif AINU.Is_Subprogram (S) then
                        Proxy_Instance := Get_Container_Thread (S);
                     end if;

                     if AIN.Subcomponents (Proxy_Instance) = No_List then
                        AIN.Set_Subcomponents
                          (Proxy_Instance,
                           AINU.New_List (AIN.K_List_Id,
                                          AIN.Loc (Proxy_Instance)));
                     end if;

                     declare
                        The_Sub : constant Node_Id :=
                          AINU.New_Node (AIN.K_Subcomponent_Instance,
                                         AIN.Loc (Proxy_Instance));
                     begin
                        AIN.Set_Parent_Component
                          (The_Sub, Called_Spg_Instance);
                        AIN.Set_Corresponding_Instance
                          (The_Sub, Called_Spg_Instance);

                        AIN.Set_Parent_Subcomponent
                          (Called_Spg_Instance,
                           (Proxy_Instance));
                        AINU.Append_Node_To_list
                          (The_Sub,
                           AIN.Subcomponents (Proxy_Instance));
                     end;
                  end;

                  Called_Spg_Spec := Map_C_Subprogram_Spec
                    (Called_Spg_Instance);

                  Set_Defining_Identifier
                    (Called_Spg_Spec,
                     Var_identifier);

                  Append_Node_To_List
                    (Called_Spg_Spec,
                     CTN.Declarations (Current_File));
               end if;

               --  Then, call the function provided by the user in our
               --  subprogram.

               if BANu.Is_Empty (Subprogram_Parameter_List (Node)) then

                  N := Make_Call_Profile (Var_identifier);

               else

                  Call_Parameters := New_List (CTN.K_Parameter_List);
                  N := BATN.First_Node (Subprogram_Parameter_List (Node));

                  while Present (N) loop
                     Param_Node := Parameter (N);

                     case BATN.Kind (Param_Node) is

                     when K_Value_Expression =>

                        K := Evaluate_BA_Value_Expression
                          (Node             => Param_Node,
                           Is_Out_Parameter => BATN.Is_Out (N),
                           BA_Root          => BA_Root,
                           Subprogram_Root  => S,
                           Declarations     => Declarations,
                           Statements       => Statements);

                        Append_Node_To_List
                          (K,
                           Call_Parameters);

                     when K_Name =>
                        Append_Node_To_List
                          (Evaluate_BA_Identifier
                             (Node             => BATN.First_Node
                                  (BATN.Idt (BATN.Identifier (Param_Node))),
                              Is_Out_Parameter => BATN.Is_Out (N),
                              BA_Root          => BA_Root,
                              Subprogram_Root  => S,
                              Declarations     => Declarations,
                              Statements       => Statements),
                           Call_Parameters);

                        --  when K_Data_Component_Reference =>
                        --    This case is not yet treated

                     when others =>
                        Display_Error ("Other param label Kinds are not"
                                       & " supported", Fatal => True);
                     end case;

                     N := BATN.Next_Node (N);

                  end loop;

                  N := Make_Call_Profile
                    (Defining_Identifier => Var_identifier,
                     Parameters          => Call_Parameters);
               end if;

               CTU.Append_Node_To_List (N, Statements);

            else
               --  It is a port sending action

               if not BANu.Is_Empty (Subprogram_Parameter_List (Node))
                 and then BANu.Length (Subprogram_Parameter_List (Node)) = 1
               then
                  --  Declare request variable if it is not yet declared
                  Make_Request_Variable_Declaration (Declarations);

                  Make_Output_Port_Name
                 (Node       => BATN.First_Node
                    (BATN.Idt (BATN.Identifier (Node))),
                  S          => S,
                  Statements => Statements);

                  Make_Put_Value_On_port (Node, S, Declarations, Statements);
               end if;

               --  Declare request variable if it is not yet declared
               Make_Request_Variable_Declaration (Declarations);

               Make_Send_Output_Port (Node, S, Statements);

            end if;

         when CK_Interrogative    =>
            if No (Target (Node)) then
               Make_Next_Value_of_Port
                 (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
                  S, Statements);
            else
               CTU.Append_Node_To_List
                 (Make_Assignment_Statement
                    (Variable_Identifier => Make_Defining_Identifier
                         (BATN.Display_Name
                              (BATN.First_Node
                                 (BATN.Idt (BATN.Target (Node))))),
                     Expression          => Make_Get_Value_of_Port
                       (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
                        S, Declarations, Statements)),
                  Statements);

               Make_Next_Value_of_Port
                 (BATN.First_Node (BATN.Idt (BATN.Identifier (Node))),
                  S, Statements);
            end if;

            --  when CK_Greater_Greater  =>
            --  when CK_Exclamation_Greater =>
            --  when CK_Exclamation_Lesser  =>

         when others              =>
            Display_Error ("Other Communication Action Kinds"
                           & " are not yet supported",
                           Fatal => True);
      end case;

      --  Target (Node) is not yet treated
      --
      --  if Present (Target (Node)) then
      --
      --  end if;
   end Map_C_Communication_Action;

   ---------------------------
   -- Make_Output_Port_Name --
   ---------------------------

   procedure Make_Output_Port_Name
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id)
   is
      N               : Node_Id;
   begin

      --  Generate the following code :
      --  request.port =
      --           REQUEST_PORT (thread_instance_name, port_name);

      N := Message_Comment (" The name of an output port is built"
                            & " from the thread_instance name"
                            & " and the port name using"
                            & " the REQUEST_PORT macro. ");
      Append_Node_To_List (N, Statements);

      CTU.Append_Node_To_List
        (Make_Assignment_Statement
           (Variable_Identifier => Make_Member_Designator
                (Defining_Identifier =>
                     Make_Defining_Identifier (MN (M_Port)),
                 Aggregate_Name      =>
                   Make_Defining_Identifier (VN (V_Request))),
            Expression          => Make_Call_Profile
              (RE (RE_REQUEST_PORT),
               Make_List_Id
                 (Make_Defining_Identifier
                      (Map_Thread_Port_Variable_Name (S)),
                  Make_Defining_Identifier
                    (BATN.Display_Name (Node))))),
         Statements);

   end Make_Output_Port_Name;

   ----------------------------
   -- Make_Put_Value_On_port --
   ----------------------------

   procedure Make_Put_Value_On_port
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      N : Node_Id;
   begin
      N := Message_Comment (" The name of the corresponding"
                            & " port variable is built from"
                            & " the port name,"
                            & " following similar pattern. ");
      Append_Node_To_List (N, Statements);

      N := Make_Call_Profile
        (RE (RE_PORT_VARIABLE),
         Make_List_Id
           (Make_Defining_Identifier
                (Map_Thread_Port_Variable_Name (S)),
            Make_Defining_Identifier
              (BATN.Display_Name (BATN.First_Node
               (BATN.Idt (BATN.Identifier (Node)))))));

      CTU.Append_Node_To_List
        (Make_Assignment_Statement
           (Variable_Identifier => Make_Member_Designator
                (Defining_Identifier => N,
                 Aggregate_Name      =>
                   Make_Defining_Identifier (VN (V_Request))),
            Expression          => Evaluate_BA_Value_Expression
              (Node             => Parameter
                   (BATN.First_Node
                        (Subprogram_Parameter_List (Node))),
               Subprogram_Root  => S,
               Declarations     => Declarations,
               Statements       => Statements)),
         Statements);

   end Make_Put_Value_On_port;

   ---------------------------
   -- Make_Send_Output_Port --
   ---------------------------

   procedure Make_Send_Output_Port
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id)
   is
      N               : Node_Id;
      N1              : Name_Id;
      Call_Parameters : List_Id;
   begin

      N := Message_Comment (" Send the request through the thread "
                            & " *local* port,"
                            & " built from the instance name"
                            & " and the port name using"
                            & " the LOCAL_PORT macro. ");
      Append_Node_To_List (N, Statements);

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Set_Str_To_Name_Buffer ("self");
      N1 := Name_Find;
      N := Make_Defining_Identifier (N1);
      Append_Node_To_List (N, Call_Parameters);

      Append_Node_To_List
        (Make_Call_Profile
           (RE (RE_Local_Port),
            Make_List_Id
              (Make_Defining_Identifier
                   (Map_Thread_Port_Variable_Name (S)),
               Make_Defining_Identifier
                 (BATN.Display_Name (BATN.First_Node
                  (BATN.Idt (BATN.Identifier (Node))))))),
         Call_Parameters);

      N :=
        Make_Variable_Address
          (Make_Defining_Identifier (VN (V_Request)));
      Append_Node_To_List (N, Call_Parameters);

      N :=
        CTU.Make_Call_Profile
          (RE (RE_Gqueue_Store_Out),
           Call_Parameters);
      Append_Node_To_List (N, Statements);

   end Make_Send_Output_Port;

   -----------------------------
   -- Map_C_Assignment_Action --
   -----------------------------

   procedure Map_C_Assignment_Action
     (Node         : Node_Id;
      S            : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id)
   is
      use AAN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Assignment_Action);

      Var_identifier       : Node_Id;
      N                    : Node_Id;
      Expr                 : Node_Id;
      Corresponding_Entity : Node_Id;
   begin

      if BATN.Kind (BATN.Target (Node)) = BATN.K_Name then

         --  Here we suppose that the target is a single
         --  identifier
         --  In the future, we must support the case of records
         --  position.x := ....
         --  position.y := ....
         --

         --  We verify if target is
         --  an outgoing_subprogram_parameter_identifier
         --  in this case the corresponding idendifier must
         --  be a pointer
         --
         Corresponding_Entity := BATN.Corresponding_Entity
           (BATN.First_Node (BATN.Idt (BATN.Target (Node))));

         if present (Corresponding_Entity) then

            if AAN.Kind (Corresponding_Entity) = AAN.K_Parameter
              and then AAN.Is_Out (Corresponding_Entity)
            then
               Var_identifier :=
                 CTU.Make_Defining_Identifier
                   (Name           => BATN.Display_Name
                      (BATN.First_Node (BATN.Idt (BATN.Target (Node)))),
                    Pointer        => True);
            elsif AAN.Kind (Corresponding_Entity) = AAN.K_Port_Spec
              and then AAN.Is_Out (Corresponding_Entity)
            then
               --  Declare request variable if it is not yet declared
               Make_Request_Variable_Declaration (Declarations);

               Make_Output_Port_Name
                 (Node       => BATN.First_Node
                    (BATN.Idt (BATN.Target (Node))),
                  S          => S,
                  Statements => Statements);

               N := Message_Comment (" The name of the corresponding"
                                     & " port variable is built from"
                                     & " the port name,"
                                     & " following similar pattern. ");
               Append_Node_To_List (N, Statements);

               N := Make_Call_Profile
                 (RE (RE_PORT_VARIABLE),
                  Make_List_Id
                    (Make_Defining_Identifier
                         (Map_Thread_Port_Variable_Name (S)),
                     Make_Defining_Identifier
                       (BATN.Display_Name (BATN.First_Node
                        (BATN.Idt (BATN.Target (Node)))))));

               Var_identifier := CTU.Make_Member_Designator
                 (Defining_Identifier => N,
                  Aggregate_Name      =>
                    Make_Defining_Identifier (VN (V_Request)));

            end if;

         else
            Var_identifier :=
              CTU.Make_Defining_Identifier
                (Name           => BATN.Display_Name
                   (BATN.First_Node (BATN.Idt (BATN.Target (Node)))),
                 Pointer        => False);
         end if;

      else
         --  i.e. the case of target with Data_Component_Reference
         --  kind, this case will be treated later
         --
         Display_Error
           ("Assignment action target with Data_Component_Reference kind"
            & " is not yet supported",
            Fatal => True);
      end if;

      Expr := Evaluate_BA_Value_Expression
        (Node             => BATN.Value_Expression (Node),
         Subprogram_Root  => S,
         Declarations     => Declarations,
         Statements       => Statements);

      CTU.Append_Node_To_List
        (CTU.Make_Assignment_Statement
           (Variable_Identifier => Var_identifier,
            Expression          => Expr),
         Statements);

      if BATN.Is_Any (Node) then
         --  i.e. the case of target with Data_Component_Reference
         --  kind, this case will be treated later
         Display_Error
           ("the mapping of any is not supported", Fatal => True);
      end if;

   end Map_C_Assignment_Action;

   ----------------------------------
   -- Evaluate_BA_Value_Expression --
   ----------------------------------

   function Evaluate_BA_Value_Expression
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Expression);
      pragma Assert (not BANu.Is_Empty (BATN.Relations (Node)));

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      N := BATN.First_Node (BATN.Relations (Node));

      Left_Expr := Evaluate_BA_Relation
        (N, Is_Out_Parameter, BA_Root, Subprogram_Root,
         Declarations, Statements);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else

         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Relation =>
                  Right_Expr := Evaluate_BA_Relation
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root,
                     Declarations     => Declarations,
                     Statements       => Statements);

               when BATN.K_Operator =>
                  Op := Evaluate_BA_Operator (N);
               when others     => Display_Error
                    ("Not valid Value_Expression", Fatal => True);
            end case;

            if Right_Expr /= No_Node and then
              Op /= Op_None
            then
               Expr := Make_Expression (Left_Expr, Op, Right_Expr);
               Left_Expr := Expr;
               Op := Op_None;
               Right_Expr := No_Node;
            end if;

            N := BATN.Next_Node (N);
         end loop;

         return Expr;
      end if;

   end Evaluate_BA_Value_Expression;

   --------------------------
   -- Evaluate_BA_Relation --
   --------------------------

   function Evaluate_BA_Relation
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Relation);

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      if not BANu.Is_Empty (BATN.Simple_Exprs (Node)) then

         N := BATN.First_Node (BATN.Simple_Exprs (Node));

         Left_Expr := Evaluate_BA_Simple_Expression
           (N, Is_Out_Parameter, BA_Root, Subprogram_Root,
            Declarations, Statements);

         N := BATN.Next_Node (N);

         if No (N) then
            return Left_Expr;
         else

            while Present (N) loop

               case BATN.Kind (N) is
                  when BATN.K_Simple_Expression =>
                     Right_Expr := Evaluate_BA_Simple_Expression
                       (Node             => N,
                        Subprogram_Root  => Subprogram_Root,
                        Declarations     => Declarations,
                        Statements       => Statements);
                  when BATN.K_Operator =>
                     Op := Evaluate_BA_Operator (N);
                  when others     => Display_Error
                       ("Not valid Relation", Fatal => True);
               end case;

               if Right_Expr /= No_Node and then
                 Op /= Op_None
               then
                  Expr := Make_Expression (Left_Expr, Op, Right_Expr);
                  Left_Expr := Expr;
                  Op := Op_None;
                  Right_Expr := No_Node;
               end if;

               N := BATN.Next_Node (N);
            end loop;

            return Expr;
         end if;

      end if;

      raise Program_Error;
      return No_Node;
   end Evaluate_BA_Relation;

   --------------------------
   -- Evaluate_BA_Operator --
   --------------------------

   function Evaluate_BA_Operator
     (Node             : Node_Id)
      return Operator_Type
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Operator);

   begin

      case Operator_Kind'Val (BATN.Operator_Category (Node)) is

         --  logical operator
         when OK_And              => return CTU.Op_And;
         when OK_Or               => return CTU.Op_Or;
         when OK_Xor              => Display_Error
              ("Not supported Operator", Fatal => True);
         when OK_Or_Else          => return CTU.Op_Or;
         when OK_And_Then         => return CTU.Op_And;

            --  relational_operator
         when OK_Equal            => return CTU.Op_Equal_Equal;
         when OK_Non_Equal        => return CTU.Op_Not_Equal;
         when OK_Less_Than        => return CTU.Op_Less;
         when OK_Less_Or_Equal    => return CTU.Op_Less_Equal;
         when OK_Greater_Than     => return CTU.Op_Greater;
         when OK_Greater_Or_Equal => return CTU.Op_Greater_Equal;

            --  unary_adding_opetor
            --  binary_adding_operator
         when OK_Plus             => return CTU.Op_Plus;
         when OK_Minus            => return CTU.Op_Minus;

            --  multiplying operator
         when OK_Multiply         => return CTU.Op_Asterisk;
         when OK_Divide           => return CTU.Op_Slash;
         when OK_Mod              => return CTU.Op_Modulo;
         when OK_Rem              => Display_Error
              ("Not supported Operator", Fatal => True);

            --  highest precedence operator
         when OK_Exponent         => Display_Error
              ("Not supported Operator", Fatal => True);
         when OK_Abs              => Display_Error
              ("Not supported Operator", Fatal => True);
         when OK_Not              => return CTU.Op_Not;

         when others              => Display_Error
              ("Not valid Operator", Fatal => True);
      end case;

      raise Program_Error;
      return CTU.Op_None;

   end Evaluate_BA_Operator;

   -----------------------------------
   -- Evaluate_BA_Simple_Expression --
   -----------------------------------

   function Evaluate_BA_Simple_Expression
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Simple_Expression);
      pragma Assert (not BANu.Is_Empty (BATN.Term_And_Operator (Node)));

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      N := BATN.First_Node (BATN.Term_And_Operator (Node));

      Left_Expr := Evaluate_BA_Term
        (N, Is_Out_Parameter, BA_Root, Subprogram_Root,
         Declarations, Statements);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else
         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Term =>
                  Right_Expr := Evaluate_BA_Term
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root,
                     Declarations     => Declarations,
                     Statements       => Statements);
               when BATN.K_Operator =>
                  Op := Evaluate_BA_Operator (N);
               when others     => Display_Error
                    ("Not valid BA_Term", Fatal => True);
            end case;

            if Right_Expr /= No_Node and then
              Op /= Op_None
            then
               Expr := Make_Expression (Left_Expr, Op, Right_Expr);
               Left_Expr := Expr;
               Op := Op_None;
               Right_Expr := No_Node;
            end if;

            N := BATN.Next_Node (N);
         end loop;

         return Expr;
      end if;

   end Evaluate_BA_Simple_Expression;

   ----------------------
   -- Evaluate_BA_Term --
   ----------------------

   function Evaluate_BA_Term
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Term);
      pragma Assert (not BANu.Is_Empty (BATN.Factors (Node)));

      N          : Node_Id;
      Left_Expr  : Node_Id;
      Right_Expr : Node_Id := No_Node;
      Op         : Operator_Type := Op_None;
      Expr       : Node_Id;
   begin

      N := BATN.First_Node (Factors (Node));

      Left_Expr := Evaluate_BA_Factor (N, Is_Out_Parameter,
                                       BA_Root, Subprogram_Root,
                                       Declarations, Statements);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else

         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Factor =>
                  Right_Expr := Evaluate_BA_Factor
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root,
                     Declarations     => Declarations,
                     Statements       => Statements);
               when BATN.K_Operator =>
                  Op := Evaluate_BA_Operator (N);
               when others     => Display_Error
                    ("Not valid BA_Term", Fatal => True);
            end case;

            if Right_Expr /= No_Node and then
              Op /= Op_None
            then
               Expr := Make_Expression (Left_Expr, Op, Right_Expr);
               Left_Expr := Expr;
               Op := Op_None;
               Right_Expr := No_Node;
            end if;

            N := BATN.Next_Node (N);
         end loop;

         return Expr;
      end if;

   end Evaluate_BA_Term;

   ------------------------
   -- Evaluate_BA_Factor --
   ------------------------

   function Evaluate_BA_Factor
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Factor);
   begin
      --        if BATN.Is_Abs (Node) then
      --           --  We must add the library #include <stdlib.h>
      --           --  in order to be able to use their functions
      --           --  abs (x), pow (x,y)
      --           --
      --           Display_Error ("Abs not treated yet", Fatal => True);
      --        elsif BATN.Is_Not (Node) then
      --           Op := CTU.Op_Not;
      --        end if;

      if BATN.Is_Not (Node) then
         return Make_Expression
           (Left_Expr  => Evaluate_BA_Value
              (BATN.Lower_Value (Node), Is_Out_Parameter,
               BA_Root, Subprogram_Root, Declarations, Statements),
            Operator   => CTU.Op_Not);
      else
         return Evaluate_BA_Value
           (BATN.Lower_Value (Node), Is_Out_Parameter,
            BA_Root, Subprogram_Root, Declarations, Statements);
      end if;

      --        if Present (BATN.Upper_Value (Node)) then
      --           Display_Error ("Exponent not treated yet", Fatal => True);
      --        end if;

   end Evaluate_BA_Factor;

   -----------------------
   -- Evaluate_BA_Value --
   -----------------------

   function Evaluate_BA_Value
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Value_Variable
                     or else Kind (Node) = BATN.K_Value_Expression
                     or else Kind (Node) = BATN.K_Literal
                     or else Kind (Node) = BATN.K_Boolean_Literal
                     or else Kind (Node) = BATN.K_Property_Constant
                     or else Kind (Node) = BATN.K_Property_Reference
                     or else Kind (Node) = BATN.K_Identifier);
   begin

      case BATN.Kind (Node) is

         --  when BATN.K_Value_Variable     =>
         --    Evaluate_BA_Value_Variable (Node);

         when BATN.K_Literal              =>
            return Evaluate_BA_Literal (Node);

         when BATN.K_Boolean_Literal    =>
            return Evaluate_BA_Boolean_Literal (Node);

         when BATN.K_Property_Constant  =>
            return Evaluate_BA_Property_Constant
              (Node, Is_Out_Parameter, BA_Root, Subprogram_Root,
               Declarations, Statements);

            --  when BATN.K_Property_Reference =>
            --    Evaluate_BA_Property_Reference (Node);

         when BATN.K_Value_Expression   =>
            return Evaluate_BA_Value_Expression
              (Node             => Node,
               Subprogram_Root  => Subprogram_Root,
               Declarations     => Declarations,
               Statements       => Statements);

         when BATN.K_Identifier           =>
            return Evaluate_BA_Identifier
              (Node             => Node,
               Subprogram_Root  => Subprogram_Root,
               Declarations     => Declarations,
               Statements       => Statements);

         when others                      =>
            Display_Error ("others cases are not yet treated",
                           Fatal => True);
      end case;

      raise Program_Error;
      return No_Node;

   end Evaluate_BA_Value;

   -------------------------------
   -- Evaluate_BA_Integer_Value --
   -------------------------------

   function Evaluate_BA_Integer_Value
     (Node         : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Integer_Value);
      pragma Assert (BATN.Kind (BATN.Entity (Node)) = K_Value_Variable
                       or else BATN.Kind (BATN.Entity (Node)) = K_Literal
                     or else BATN.Kind (BATN.Entity (Node))
                     = K_Property_Constant);

      Entity_Node : constant Node_Id := BATN.Entity (Node);
      result      : Node_Id;
   begin
      case BATN.Kind (Entity_Node) is
         --  when K_Value_Variable    =>
         when K_Literal           =>
            result := Evaluate_BA_Literal (Entity_Node);
         when K_Property_Constant =>
            result := Evaluate_BA_Property_Constant
              (Node             => Entity_Node,
               Declarations     => Declarations,
               Statements       => Statements);
         when others              =>
            Display_Error (" For Lower value in Interger range, Kinds"
                           & " other than K_Literal and K_Property_Constant"
                           & " are not yet supported", Fatal => True);
      end case;

      return result;

   end Evaluate_BA_Integer_Value;

   -------------------------
   -- Evaluate_BA_Literal --
   -------------------------

   function Evaluate_BA_Literal (Node : Node_Id) return Node_Id is
      pragma Assert (BATN.Kind (Node) = BATN.K_Literal);
   begin

      return CTU.Make_Literal (CV.To_C_Value (BATN.Value (Node)));

   end Evaluate_BA_Literal;

--     ----------------------------
--     -- Get_Port_Spec_Instance --
--     ----------------------------
--
--     function Get_Port_Spec_Instance
--       (Node             : Node_Id;
--        Parent_Component : Node_Id) return Node_Id
--     is
--        use type AIN.Node_Kind;
--        Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
--          := Features_Of (Parent_Component);
--        result : Node_Id;
--     begin
--        for F of Fs loop
--           if Standard.Utils.To_Upper
--             (AIN.Display_Name (AIN.Identifier (F)))
--             = Standard.Utils.To_Upper
--             (BATN.Display_Name (Node))
--             and then
--               AIN.Kind (F) = AIN.K_Port_Spec_Instance
--             and then AIN.Is_In (F)
--             and then AIN.Is_Data (F)
--           then
--              result := F;
--           end if;
--        end loop;
--        return result;
--     end Get_Port_Spec_Instance;

   -----------------------------
   -- Make_Next_Value_of_Port --
   -----------------------------

   procedure Make_Next_Value_of_Port
     (Node             : Node_Id;
      Subprogram_Root  : Node_Id;
      Statements       : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);

      N               : Node_Id;
      N1              : Name_Id;
      Call_Parameters : List_Id;
   begin

      --  Make the call to __po_hi_gqueue_next_value
      --  if it is an event port
      if AAN.Is_Event (BATN.Corresponding_Entity (Node)) then
         Call_Parameters := New_List (CTN.K_Parameter_List);

         Set_Str_To_Name_Buffer ("self");
         N1 := Name_Find;
         N := Make_Defining_Identifier (N1);
         Append_Node_To_List (N, Call_Parameters);

         Append_Node_To_List
           (Make_Call_Profile
              (RE (RE_Local_Port),
               Make_List_Id
                 (Make_Defining_Identifier
                      (Map_Thread_Port_Variable_Name (Subprogram_Root)),
                  Make_Defining_Identifier
                    (BATN.Display_Name (Node)))),
            Call_Parameters);

         N :=
           CTU.Make_Call_Profile
             (RE (RE_Gqueue_Next_Value),
              Call_Parameters);
         Append_Node_To_List (N, Statements);
      end if;

   end Make_Next_Value_of_Port;

   ---------------------------------------
   -- Make_Request_Variable_Declaration --
   ---------------------------------------

   procedure Make_Request_Variable_Declaration
     (Declarations     : List_Id)
   is
      decl                      : Node_Id;
      request_declaration_exist : Boolean := False;
   begin

      decl := CTN.First_Node (Declarations);
      while Present (decl) loop

         if Kind (decl) = CTN.K_Variable_Declaration then

            if Get_Name_String
              (Standard.Utils.To_Lower
                 (CTN.Name (CTN.Defining_Identifier (decl))))
                = Get_Name_String
              (Standard.Utils.To_Lower
                 (Get_String_Name ("request")))

            then
               request_declaration_exist := True;
            end if;
         end if;

         exit when request_declaration_exist;
         decl := CTN.Next_Node (decl);
      end loop;

      if not request_declaration_exist then
         CTU.Append_Node_To_List
           (Make_Variable_Declaration
              (Defining_Identifier =>
                   Make_Defining_Identifier (VN (V_Request)),
               Used_Type           => RE (RE_Request_T)),
            Declarations);
      end if;

   end Make_Request_Variable_Declaration;

   ----------------------------
   -- Make_Get_Value_of_Port --
   ----------------------------

   function Make_Get_Value_of_Port
     (Node             : Node_Id;
      Subprogram_Root  : Node_Id;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      N               : Node_Id;
      --  F           : Node_Id;
      N1              : Name_Id;
      result          : Node_Id;
      Call_Parameters : List_Id;
   begin

      --  F := Get_Port_Spec_Instance (Node, Subprogram_Root);

      --  Read from the in data port
      N := Message_Comment ("Read the data from the port "
                            & Get_Name_String
                              (BATN.Display_Name (Node)));

      Append_Node_To_List (N, Statements);

      Make_Request_Variable_Declaration (Declarations);

      --  Make the call to __po_hi_gqueue_get_value

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Set_Str_To_Name_Buffer ("self");
      N1 := Name_Find;
      N := Make_Defining_Identifier (N1);
      Append_Node_To_List (N, Call_Parameters);

      Append_Node_To_List
        (Make_Call_Profile
           (RE (RE_Local_Port),
            Make_List_Id
              (Make_Defining_Identifier
                   (Map_Thread_Port_Variable_Name (Subprogram_Root)),
               Make_Defining_Identifier (BATN.Display_Name (Node)))),
         Call_Parameters);

      N :=
        Make_Variable_Address
          (Make_Defining_Identifier (VN (V_Request)));
      Append_Node_To_List (N, Call_Parameters);

      N :=
        CTU.Make_Call_Profile
          (RE (RE_Gqueue_Get_Value),
           Call_Parameters);
      Append_Node_To_List (N, Statements);

      Call_Parameters := New_List (CTN.K_Parameter_List);

      Append_Node_To_List
        (Make_Defining_Identifier
           (Map_Thread_Port_Variable_Name (Subprogram_Root)),
         Call_Parameters);

      Append_Node_To_List
        (Make_Defining_Identifier (BATN.Display_Name (Node)),
         Call_Parameters);

      result := CTU.Make_Call_Profile
        (Make_Member_Designator
           (Defining_Identifier => RE (RE_PORT_VARIABLE),
            Aggregate_Name      =>
              Make_Defining_Identifier (VN (V_Request))),
         Call_Parameters);

      return result;

   end Make_Get_Value_of_Port;

   ----------------------------
   -- Evaluate_BA_Identifier --
   ----------------------------

   function Evaluate_BA_Identifier
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      use AAN;
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);
      Pointer                   : Boolean := False;
      Variable_Address          : Boolean := False;

      N                  : Node_Id;
      --  F                         : Node_Id;
--        N1                        : Name_Id;
      result                    : Node_Id;
--        request_declaration_exist : Boolean := False;
--        Call_Parameters           : List_Id;
   begin

      if Is_Out_Parameter then
         if Find_BA_Variable (Node, BA_Root) /= No_Node then
            --  The given parameter is a BA variable
            --  then it must be mapped to a variable
            --  address to enable its modification
            --  by the called subprogram
            --
            Variable_Address := True;
         else
            --  The given parameter is an OUT/INOUT parameter
            --  of the subprogram implementing the BA, it is
            --  already a pointer
            --
            Pointer := False;
         end if;
      else
         --  In the case of IN parameter_label, if it contain an
         --  OUT parameter (ex. p1) of the subprogram implementing
         --  the BA, then in the generated code C, in order
         --  to have the value of p1, it must be mapped to *p1
         --
         if Subprogram_Root /= No_Node then
            declare
               use type AIN.Node_Kind;
               Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
                 := Features_Of (Subprogram_Root);
            begin
               for F of Fs loop
                  if Standard.Utils.To_Upper
                    (AIN.Display_Name (AIN.Identifier (F)))
                    = Standard.Utils.To_Upper
                    (BATN.Display_Name (Node))
                    --  and then AIN.Is_Out (F)
                    and then
                      ((AIN.Kind (F) = AIN.K_Parameter_Instance
                        and then AIN.Is_Out (F))
                       or else
                         (AIN.Kind (F) = AIN.K_Subcomponent_Access_Instance
                          and then
                          Get_Required_Data_Access
                            (AIN.Corresponding_Instance (F))
                          /= Access_Read_Only))
                  then
                     Pointer := True;
                  end if;
               end loop;
            end;
         end if;
      end if;

      result := CTU.Make_Defining_Identifier
        (Name             => BATN.Display_Name (Node),
         Pointer          => Pointer,
         Variable_Address => Variable_Address);

      N := BATN.Corresponding_Entity (Node);
      if Present (N) then
         if AAN.kind (N) = AAN.K_Port_Spec
           and then AAN.Is_Data (N) and then AAN.Is_In (N)
         then
            result := Make_Get_Value_of_Port
              (Node, Subprogram_Root, Declarations, Statements);
         end if;
      end if;

      return result;

   end Evaluate_BA_Identifier;

   -----------------------------------
   -- Evaluate_BA_Property_Constant --
   -----------------------------------

   function Evaluate_BA_Property_Constant
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node;
      Declarations     : List_Id;
      Statements       : List_Id) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Property_Constant);

   begin
      --  We must treat later the case of Property_Set (Node)
      --
      --        if Present (BATN.Property_Set (Node)) then
      --
      --        end if;

      return Evaluate_BA_Identifier
        (BATN.Identifier (Node), Is_Out_Parameter, BA_Root,
         Subprogram_Root, Declarations, Statements);

   end Evaluate_BA_Property_Constant;

   ---------------------------
   -- Print_Boolean_Literal --
   ---------------------------

   function Evaluate_BA_Boolean_Literal (Node : Node_Id) return Node_Id  is
      pragma Assert (BATN.Kind (Node) = BATN.K_Boolean_Literal);
      N : Name_Id;
   begin
      if BATN.Is_True (Node) then
         Set_Str_To_Name_Buffer ("True");
         N := Name_Find;
         return Make_Defining_Identifier (N);
         --  Expr := PHR.RE (PHR.RE_True);
         --  Expr := Make_Literal (CV.New_Int_Value (1, 1, 10));
      else
         Set_Str_To_Name_Buffer ("False");
         N := Name_Find;
         return  Make_Defining_Identifier (N);
         --  Expr := PHR.RE (PHR.RE_False);
         --  Expr := Make_Literal (CV.New_Int_Value (0, 1, 10));
      end if;

   end Evaluate_BA_Boolean_Literal;

end Ocarina.Backends.C_Common.BA;
