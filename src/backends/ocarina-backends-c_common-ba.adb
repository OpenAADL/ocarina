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
with Utils;
pragma Warnings (Off);
with Ocarina.Backends.Utils;
pragma Warnings (On);

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

package body Ocarina.Backends.C_Common.BA is

   use Ocarina.Analyzer.AADL_BA;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Namet;
   use Ocarina.Backends.Helper;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.C_Tree.Nodes;
   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.Utils;

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

   function Map_C_Behavior_Action_Block
     (Node       : Node_Id;
      S          : Node_Id)
      return List_Id;

   function Map_C_Behav_Acts
     (Node       : Node_Id;
      S          : Node_Id)
      return List_Id;

   procedure Map_C_Behavior_Action
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id);

   procedure Map_C_If_Cond_Struct
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id);

   procedure Map_C_Communication_Action
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id);

   procedure Map_C_Assignment_Action
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id);

   function Evaluate_BA_Value_Expression
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

   function Evaluate_BA_Relation
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

   function Evaluate_BA_Operator (Node : Node_Id) return Operator_Type;

   function Evaluate_BA_Simple_Expression
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

   function Evaluate_BA_Term
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

   function Evaluate_BA_Factor
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

   function Evaluate_BA_Value
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

   function Evaluate_BA_Literal (Node : Node_Id) return Node_Id;

   function Evaluate_BA_Property_Constant
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

   function Evaluate_BA_Identifier
     (Node             : Node_Id;
      Is_Out_parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id;

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

   function Map_C_Behavior_Variables (S : Node_Id) return List_Id is
      BA, P, T     : Node_Id;
      Declarations : constant List_Id := New_List (CTN.K_Declaration_List);
      Used_Type    : Node_Id;
   begin

      BA := Get_Behavior_Specification (S);
      if not BANu.Is_Empty (BATN.Variables (BA)) then
         P := BATN.First_Node (BATN.Variables (BA));
         loop
            T := BATN.First_Node (BATN.Identifiers (P));
            loop

               Used_Type :=
                 Ocarina.Backends.C_Common.Mapping.
                   Map_C_Data_Type_Designator
                     (AAN.Default_Instance
                        (BATN.Corresponding_Declaration
                           (BATN.Classifier_Ref (P))));

               CTU.Append_Node_To_List
                 (CTU.Make_Variable_Declaration
                    (Defining_Identifier => CTU.Make_Defining_Identifier
                         (BATN.Display_Name (T)),
                     Used_Type =>  Used_Type),
                  Declarations);

               T := BATN.Next_Node (T);
               exit when No (T);
            end loop;
            P := BATN.Next_Node (P);
            exit when No (P);
         end loop;
      end if;

      return Declarations;

   end Map_C_Behavior_Variables;

   ----------------------------
   -- Map_C_Behavior_Actions --
   ----------------------------

   function Map_C_Behavior_Actions (S : Node_Id) return List_Id is

      BA                 : Node_Id;
      behav_transition   : Node_Id;
      Transition_Node    : Node_Id;
      Statements         : List_Id;
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
               Statements := Map_C_Behavior_Action_Block
                 (BATN.Behavior_Action_Block (Transition_Node),
                  S);
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

      return Statements;

   end Map_C_Behavior_Actions;

   ---------------------------------
   -- Map_C_Behavior_Action_Block --
   ---------------------------------

   function Map_C_Behavior_Action_Block
     (Node       : Node_Id;
      S          : Node_Id)
      return List_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action_Block);
      Statements         : List_Id;
   begin

      Statements := Map_C_Behav_Acts (Node, S);

      --  Behavior_Time (Node) is not yet supported
      --
      --  if Present (Behavior_Time (Node)) then
      --
      --  end if;

      return Statements;

   end Map_C_Behavior_Action_Block;

   ----------------------
   -- Map_C_Behav_Acts --
   ----------------------

   function Map_C_Behav_Acts
     (Node       : Node_Id;
      S          : Node_Id)
      return List_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Behavior_Action_Block
                     or else BATN.Kind (Node) = K_Conditional_Statement
                     or else BATN.Kind (Node) = K_For_Cond_Structure);

      pragma Assert (Present (BATN.Behav_Acts (Node)));

      Behav_actions   : Node_Id;
      Behav_action    : Node_Id;
      Statements      : List_Id;
   begin

      Statements := New_List (CTN.K_Statement_List);
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
            Map_C_Behavior_Action (Behav_action, S, Statements);
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
            Map_C_Behavior_Action (Behav_action, S, Statements);
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
                                Statements);
      end if;

      return Statements;

   end Map_C_Behav_Acts;

   ---------------------------
   -- Map_C_Behavior_Action --
   ---------------------------

   procedure Map_C_Behavior_Action
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id)
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
            Map_C_If_Cond_Struct (Action_Node, S, Statements);

            --  when K_For_Cond_Structure     =>
            --     Map_C_For_Cond_Struct (Action_Node, S, Statements);

            --  when K_While_Cond_Structure   =>
            --    Map_C_While_Cond_Struct (Action_Node);

            --  when K_ForAll_Cond_Structure  =>
            --    Map_C_Forall_Cond_Struct (Action_Node);

            --  when K_DoUntil_Cond_Structure =>
            --    Map_C_DoUntil_Cond_Struct (Action_Node);

         when BATN.K_Assignment_Action      =>
            Map_C_Assignment_Action (Action_Node, S, Statements);

         when K_Communication_Action   =>
            Map_C_Communication_Action (Action_Node, S, Statements);

            --  when K_Timed_Act           =>
            --    Map_C_Timed_Action (Action_Node);

         when others                   =>
            N := Message_Comment ("Behavior Actions not yet mapped");
            CTU.Append_Node_To_List (N, Statements);
      end case;

   end Map_C_Behavior_Action;

   --------------------------
   -- Map_C_If_Cond_Struct --
   --------------------------

   procedure Map_C_If_Cond_Struct
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id)
   is

      pragma Assert (BATN.Kind (Node) = K_If_Cond_Struct);

      pragma Assert (Present (Logical_Expr (If_Statement (Node))));
      pragma Assert (Present (Behav_Acts (If_Statement (Node))));

      Condition     : Node_Id;
      if_Statements : List_Id;
      else_statements : List_Id;
   begin

      Condition := Evaluate_BA_Value_Expression
        (Node             => Logical_Expr (If_Statement (Node)),
         Subprogram_Root  => S);

      if_Statements := Map_C_Behav_Acts
        (Node       => If_Statement (Node),
         S          => S);

      if Present (Elsif_Statement (Node)) then
         else_statements := New_List (CTN.K_Statement_List);
         if Present (Else_Statement (Node)) then
            CTU.Append_Node_To_List
              (CTU.Make_If_Statement
                 (Condition       => Evaluate_BA_Value_Expression
                      (Node             => Logical_Expr
                           (Elsif_Statement (Node)),
                       Subprogram_Root  => S),
                  Statements      => Map_C_Behav_Acts
                    (Node       => Elsif_Statement (Node),
                     S          => S),
                  Else_Statements => Map_C_Behav_Acts
                    (Node       => Else_Statement (Node),
                     S          => S)),
               else_statements);

         else

            CTU.Append_Node_To_List
              (CTU.Make_If_Statement
                 (Condition       => Evaluate_BA_Value_Expression
                      (Node             => Logical_Expr
                           (Elsif_Statement (Node)),
                       Subprogram_Root  => S),
                  Statements      => Map_C_Behav_Acts
                    (Node       => Elsif_Statement (Node),
                     S          => S)),
               else_statements);

         end if;

      else
         if Present (Else_Statement (Node)) then
            else_statements := New_List (CTN.K_Statement_List);
            else_statements := Map_C_Behav_Acts
              (Node       => Else_Statement (Node),
               S          => S);
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

   --------------------------------
   -- Map_C_Communication_Action --
   --------------------------------

   procedure Map_C_Communication_Action
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id)
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

                  if Kind (decl) = CTN.K_Extern_Entity_Declaration
                    --  CTN.K_Function_Specification
                    and then
                    Get_Name_String
                        (Standard.Utils.To_Lower
                           (CTN.Name (CTN.Defining_Identifier
                            (CTN.Entity (decl)))))
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

                  Called_Spg_Spec := Make_Extern_Entity_Declaration
                    (Called_Spg_Spec);

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
                           Subprogram_Root  => S);

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
                              Subprogram_Root  => S),
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

               --  else
               --  It is a port sending action
            end if;

            --  when CK_Interrogative    =>
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

   -----------------------------
   -- Map_C_Assignment_Action --
   -----------------------------

   procedure Map_C_Assignment_Action
     (Node       : Node_Id;
      S          : Node_Id;
      Statements : List_Id)
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Assignment_Action);

      Var_identifier                   : Node_Id;
      Is_outgoing_subprogram_param_idt : Boolean := False;

      Fs : constant Ocarina.ME_AADL.AADL_Instances.Nutils.Node_Array
        := Features_Of (S);

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
         for F of Fs loop
            if (Standard.Utils.To_Upper (AIN.Display_Name (AIN.Identifier (F)))
                = Standard.Utils.To_Upper
                  (BATN.Display_Name
                     (BATN.First_Node
                          (BATN.Idt (BATN.Target (Node))))))
              and then AIN.Is_Out (F)
            then

               Is_outgoing_subprogram_param_idt := True;
            end if;

         end loop;

         if not Is_outgoing_subprogram_param_idt then
            Var_identifier :=
              CTU.Make_Defining_Identifier
                (Name           => BATN.Display_Name
                   (BATN.First_Node (BATN.Idt (BATN.Target (Node)))),
                 Pointer        => False);
         else
            Var_identifier :=
              CTU.Make_Defining_Identifier
                (Name           => BATN.Display_Name
                   (BATN.First_Node (BATN.Idt (BATN.Target (Node)))),
                 Pointer        => True);
         end if;

      else
         --  i.e. the case of target with Data_Component_Reference
         --  kind, this case will be treated later
         --
         Display_Error
           ("target with Data_Component_Reference is not yet supported",
            Fatal => True);
      end if;

      if Present (BATN.Value_Expression (Node)) then

         CTU.Append_Node_To_List
           (Make_Assignment_Statement
              (Variable_Identifier => Var_identifier,
               Expression          => Evaluate_BA_Value_Expression
                 (Node             => BATN.Value_Expression (Node),
                  Subprogram_Root  => S)),
            Statements);

      end if;

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
      Subprogram_Root  : Node_Id := No_Node) return Node_Id
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
        (N, Is_Out_Parameter, BA_Root, Subprogram_Root);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else

         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Relation =>
                  Right_Expr := Evaluate_BA_Relation
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root);

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
      Subprogram_Root  : Node_Id := No_Node) return Node_Id
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
           (N, Is_Out_Parameter, BA_Root, Subprogram_Root);

         N := BATN.Next_Node (N);

         if No (N) then
            return Left_Expr;
         else

            while Present (N) loop

               case BATN.Kind (N) is
                  when BATN.K_Simple_Expression =>
                     Right_Expr := Evaluate_BA_Simple_Expression
                       (Node             => N,
                        Subprogram_Root  => Subprogram_Root);
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
      Subprogram_Root  : Node_Id := No_Node) return Node_Id
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
        (N, Is_Out_Parameter, BA_Root, Subprogram_Root);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else
         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Term =>
                  Right_Expr := Evaluate_BA_Term
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root);
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
      Subprogram_Root  : Node_Id := No_Node) return Node_Id
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
                                       BA_Root, Subprogram_Root);

      N := BATN.Next_Node (N);

      if No (N) then
         return Left_Expr;
      else

         while Present (N) loop

            case BATN.Kind (N) is
               when BATN.K_Factor =>
                  Right_Expr := Evaluate_BA_Factor
                    (Node             => N,
                     Subprogram_Root  => Subprogram_Root);
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
      Subprogram_Root  : Node_Id := No_Node) return Node_Id
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
               BA_Root, Subprogram_Root),
            Operator   => CTU.Op_Not);
      else
         return Evaluate_BA_Value
           (BATN.Lower_Value (Node), Is_Out_Parameter,
            BA_Root, Subprogram_Root);
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
      Subprogram_Root  : Node_Id := No_Node) return Node_Id
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
              (Node, Is_Out_Parameter, BA_Root, Subprogram_Root);

            --  when BATN.K_Property_Reference =>
            --    Evaluate_BA_Property_Reference (Node);

         when BATN.K_Value_Expression   =>
            return Evaluate_BA_Value_Expression (Node);

         when BATN.K_Identifier           =>
            return Evaluate_BA_Identifier (Node);

         when others                      =>
            Display_Error ("others cases are not yet treated",
                           Fatal => True);
      end case;

      raise Program_Error;
      return No_Node;

   end Evaluate_BA_Value;

   -------------------------
   -- Evaluate_BA_Literal --
   -------------------------

   function Evaluate_BA_Literal (Node : Node_Id) return Node_Id is
      pragma Assert (BATN.Kind (Node) = BATN.K_Literal);
   begin

      return CTU.Make_Literal (CV.To_C_Value (BATN.Value (Node)));

   end Evaluate_BA_Literal;

   ----------------------------
   -- Evaluate_BA_Identifier --
   ----------------------------

   function Evaluate_BA_Identifier
     (Node             : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node) return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Identifier);
      Pointer : Boolean := False;
      Variable_Address : Boolean := False;

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

      return CTU.Make_Defining_Identifier
        (Name             => BATN.Display_Name (Node),
         Pointer          => Pointer,
         Variable_Address => Variable_Address);

   end Evaluate_BA_Identifier;

   -----------------------------------
   -- Evaluate_BA_Property_Constant --
   -----------------------------------

   function Evaluate_BA_Property_Constant
     (Node : Node_Id;
      Is_Out_Parameter : Boolean := False;
      BA_Root          : Node_Id := No_Node;
      Subprogram_Root  : Node_Id := No_Node)
      return Node_Id
   is
      pragma Assert (BATN.Kind (Node) = BATN.K_Property_Constant);

   begin
      --  We must treat later the case of Property_Set (Node)
      --
      --        if Present (BATN.Property_Set (Node)) then
      --
      --        end if;

      return Evaluate_BA_Identifier
        (BATN.Identifier (Node), Is_Out_Parameter, BA_Root, Subprogram_Root);

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
