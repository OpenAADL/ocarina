------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L _ B A . A C T I O N S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2019 ESA & ISAE.                    --
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

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

package body Ocarina.Builder.AADL_BA.Actions is

   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   -----------------------------------
   -- Add_New_Behavior_Action_Block --
   -----------------------------------

   function Add_New_Behavior_Action_Block
     (Loc              : Location;
      Container        : Node_Id;
      Behavior_Actions : Node_Id;
      Behavior_Time    : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) =
                                      K_Execution_Behavior_Transition);

      Behavior_Action_Block : constant Node_Id := New_Node
                                              (K_Behavior_Action_Block, Loc);
   begin
      Set_BE_Container (Behavior_Action_Block, Container);
      if Present (Behavior_Actions) then
         Set_Behav_Acts (Behavior_Action_Block, Behavior_Actions);
      end if;
      if Present (Behavior_Time) then
         Set_Behavior_Time (Behavior_Action_Block, Behavior_Time);
      end if;

      if No (Behavior_Action_Block) then
         return No_Node;
      end if;
      return Behavior_Action_Block;
   end Add_New_Behavior_Action_Block;

   ------------------------------
   -- Add_New_Behavior_Actions --
   ------------------------------

   function Add_New_Behavior_Actions
     (Loc                      : Location;
      Container                : Node_Id;
      Behavior_Action          : Node_Id;
      Behavior_Action_Sequence : List_Id;
      Behavior_Action_Set      : List_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) =
                                      K_Behavior_Action_Block);

      Behavior_Actions : constant Node_Id := New_Node
                                              (K_Behavior_Actions, Loc);
   begin
      Set_BE_Container (Behavior_Actions, Container);
      if not Is_Empty (Behavior_Action_Sequence) then
         Set_Behavior_Action_Sequence (Behavior_Actions,
                                              Behavior_Action_Sequence);
      end if;
      if not Is_Empty (Behavior_Action_Set) then
         Set_Behavior_Action_Set (Behavior_Actions,
                                              Behavior_Action_Set);
      end if;
      if Present (Behavior_Action) and then
         Is_Empty (Behavior_Action_Sequence) and then
         Is_Empty (Behavior_Action_Set)
      then
         Set_Behavior_Action (Behavior_Actions, Behavior_Action);
      end if;

      if No (Behavior_Actions) then
         return No_Node;
      end if;
      return Behavior_Actions;
   end Add_New_Behavior_Actions;

   -----------------------------
   -- Add_New_Behavior_Action --
   -----------------------------

   function Add_New_Behavior_Action
     (Loc              : Location;
      Container        : Node_Id;
      Action_Node      : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) =
                                      K_Execution_Behavior_Transition
                       or else Kind (Container) = K_Mode_Transition
                       or else Kind (Container) = K_Behavior_Action);

      Behavior_Action : constant Node_Id := New_Node (K_Behavior_Action,
                                                      Loc);
   begin
      Set_BE_Container (Behavior_Action, Container);
      if Present (Action_Node) then
         Set_Action (Behavior_Action, Action_Node);
      end if;

      return Behavior_Action;

   end Add_New_Behavior_Action;

   ----------------------------
   -- Add_New_If_Cond_Struct --
   ----------------------------

   function Add_New_If_Cond_Struct (Loc : Location) return Node_Id is
      If_Cond_Struct : constant Node_Id := New_Node (K_If_Cond_Struct, Loc);
   begin
      return If_Cond_Struct;

   end Add_New_If_Cond_Struct;

   ----------------------------
   -- Add_New_If_Cond_Struct --
   ----------------------------

   procedure Add_New_If_Cond_Struct
     (If_Cond_Struct  : Node_Id;
      Container       : Node_Id  := No_Node;
      If_Stat         : Node_Id  := No_Node;
      Elsif_Stat      : List_Id  := No_List;
      Else_Stat       : Node_Id  := No_Node)
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (If_Cond_Struct) = K_If_Cond_Struct);
      pragma Assert (Kind (If_Stat) = K_Conditional_Statement);
      pragma Assert (No (Else_Stat)
                       or else Kind (Else_Stat) = K_Conditional_Statement);
   begin
      Set_BE_Container (If_Cond_Struct, Container);
      --  Container may be No_Node

      Set_If_Statement (If_Cond_Struct, If_Stat);
      if not Is_Empty (Elsif_Stat) then
         Set_Elsif_Statement (If_Cond_Struct, Elsif_Stat);
      end if;
      Set_Else_Statement (If_Cond_Struct, Else_Stat);

   end Add_New_If_Cond_Struct;

   -----------------------------------
   -- Add_New_Conditional_Statement --
   -----------------------------------

   function Add_New_Conditional_Statement
     (Loc        : Location;
      Container  : Node_Id;
      Expression : Node_Id;
      Actions    : Node_Id)
     return Node_Id
   is
      pragma Assert (Kind (Container) = K_If_Cond_Struct);

      Cond_Statement : constant Node_Id := New_Node (K_Conditional_Statement,
                                                     Loc);
   begin
      Set_BE_Container (Cond_Statement, Container);

      if Present (Expression) then
         Set_Logical_Expr (Cond_Statement, Expression);
      end if;
      if Present (Actions) then
         Set_Behav_Acts (Cond_Statement, Actions);
      end if;

      return Cond_Statement;

   end Add_New_Conditional_Statement;

   ----------------------------
   -- Add_New_For_Cond_Struct --
   ----------------------------

   function Add_New_For_Cond_Struct (Loc : Location) return Node_Id is
      For_Cond_Struct : constant Node_Id := New_Node
                                             (K_For_Cond_Structure, Loc);
   begin
      return For_Cond_Struct;

   end Add_New_For_Cond_Struct;

   ----------------------------
   -- Add_New_For_Cond_Struct --
   ----------------------------

   procedure Add_New_For_Cond_Struct
     (For_Cond_Struct          : Node_Id;
      Container                : Node_Id;
      Element_Idt              : Node_Id;
      Classifier_Ref           : Node_Id;
      Element_Values_Node      : Node_Id;
      Actions                  : Node_Id)
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (For_Cond_Struct) = K_For_Cond_Structure);
      pragma Assert (Kind (Element_Idt) = K_Identifier);
      pragma Assert (Kind (Classifier_Ref) = K_Component_Classifier_Ref);
      pragma Assert (Kind (Element_Values_Node) = K_Data_Component_Reference
                       or else Kind (Element_Values_Node) = K_Integer_Range);

   begin
      if Present (Container) then
         Set_BE_Container (For_Cond_Struct, Container);
      end if;

      Set_Element_Idt (For_Cond_Struct, Element_Idt);
      Set_Classifier_Ref (For_Cond_Struct, Classifier_Ref);
      Set_In_Element_Values (For_Cond_Struct, Element_Values_Node);
      Set_Behav_Acts (For_Cond_Struct, Actions);

   end Add_New_For_Cond_Struct;

   -------------------------------
   -- Add_New_While_Cond_Struct --
   -------------------------------

   function Add_New_While_Cond_Struct (Loc : Location) return Node_Id is
      While_Cond_Struct : constant Node_Id := New_Node
                                                 (K_While_Cond_Structure, Loc);
   begin
      return While_Cond_Struct;

   end Add_New_While_Cond_Struct;

   -------------------------------
   -- Add_New_While_Cond_Struct --
   -------------------------------

   procedure Add_New_While_Cond_Struct
     (While_Cond_Struct : Node_Id;
      Container       : Node_Id;
      Expression      : Node_Id;
      Actions         : Node_Id)
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (While_Cond_Struct) = K_While_Cond_Structure);
      pragma Assert (Kind (Expression) = K_Value_Expression);

   begin
      if Present (Container) then
         Set_BE_Container (While_Cond_Struct, Container);
      end if;

      if Present (Expression) then
         Set_Logical_Expr (While_Cond_Struct, Expression);
      end if;
      Set_Behav_Acts (While_Cond_Struct, Actions);

   end Add_New_While_Cond_Struct;

   --------------------------------
   -- Add_New_ForAll_Cond_Struct --
   --------------------------------

   function Add_New_ForAll_Cond_Struct (Loc : Location) return Node_Id is
      ForAll_Cond_Struct : constant Node_Id := New_Node
                                             (K_ForAll_Cond_Structure, Loc);
   begin
      return ForAll_Cond_Struct;

   end Add_New_ForAll_Cond_Struct;

   --------------------------------
   -- Add_New_ForAll_Cond_Struct --
   --------------------------------

   procedure Add_New_ForAll_Cond_Struct
     (ForAll_Cond_Struct          : Node_Id;
      Container                : Node_Id;
      Element_Idt              : Node_Id;
      Classifier_Ref           : Node_Id;
      Element_Values_Node      : Node_Id;
      Actions                  : Node_Id)
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (ForAll_Cond_Struct) = K_ForAll_Cond_Structure);
      pragma Assert (Kind (Element_Idt) = K_Identifier);
      pragma Assert (Kind (Classifier_Ref) = K_Component_Classifier_Ref);
      pragma Assert (Kind (Element_Values_Node) = K_Data_Component_Reference
                       or else Kind (Element_Values_Node) = K_Integer_Range);

   begin
      if Present (Container) then
         Set_BE_Container (ForAll_Cond_Struct, Container);
      end if;

      Set_Element_Idt (ForAll_Cond_Struct, Element_Idt);
      Set_Classifier_Ref (ForAll_Cond_Struct, Classifier_Ref);
      Set_In_Element_Values (ForAll_Cond_Struct, Element_Values_Node);
      Set_Behav_Acts (ForAll_Cond_Struct, Actions);

   end Add_New_ForAll_Cond_Struct;

   ---------------------------------
   -- Add_New_DoUntil_Cond_Struct --
   ---------------------------------

   function Add_New_DoUntil_Cond_Struct (Loc : Location) return Node_Id is
      DoUntil_Cond_Struct : constant Node_Id := New_Node
                                     (K_DoUntil_Cond_Structure, Loc);
   begin
      return DoUntil_Cond_Struct;

   end Add_New_DoUntil_Cond_Struct;

   ---------------------------------
   -- Add_New_DoUntil_Cond_Struct --
   ---------------------------------

   procedure Add_New_DoUntil_Cond_Struct
     (DoUntil_Cond_Struct : Node_Id;
      Container       : Node_Id;
      Expression      : Node_Id;
      Actions         : Node_Id)
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (DoUntil_Cond_Struct) = K_DoUntil_Cond_Structure);
      pragma Assert (Kind (Expression) = K_Value_Expression);

   begin
      if Present (Container) then
         Set_BE_Container (DoUntil_Cond_Struct, Container);
      end if;

      if Present (Expression) then
         Set_Logical_Expr (DoUntil_Cond_Struct, Expression);
      end if;
      Set_Behav_Acts (DoUntil_Cond_Struct, Actions);

   end Add_New_DoUntil_Cond_Struct;

   -------------------------------
   -- Add_New_Assignment_Action --
   -------------------------------

   function Add_New_Assignment_Action
     (Loc              : Location;
      Container        : Node_Id;
      Ident            : Node_Id;
      Value_Expr       : Node_Id;
      Is_Any_Bool      : Boolean)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (Ident) = K_Name
                       or else Kind (Ident) = K_Data_Component_Reference);
      --  fixme : Todo Set Assert for Value_Expression

      Assignment_Action : constant Node_Id := New_Node (K_Assignment_Action,
                                                        Loc);
   begin
      Set_Target (Assignment_Action, Ident);
      Set_BE_Container (Ident, Assignment_Action);

      if Value_Expr /= No_Node then
         Set_Value_Expression (Assignment_Action, Value_Expr);
         Set_BE_Container (Value_Expr, Assignment_Action);
      end if;

      Set_Is_Any (Assignment_Action, Is_Any_Bool);

      return Assignment_Action;

   end Add_New_Assignment_Action;

   ----------------------------------
   -- Add_New_Communication_Action --
   ----------------------------------

   function Add_New_Communication_Action
     (Loc            : Location;
      Container      : Node_Id;
      Ident          : Node_Id;
      Target_Node    : Node_Id;
      Sub_Parameters : List_Id;
      Com_Kind       : Communication_Kind)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (Ident) = K_Name
                       or else Kind (Ident) = K_Data_Component_Reference);
      pragma Assert (Com_Kind /= CK_Error);

      Comm_Action : constant Node_Id := New_Node (K_Communication_Action, Loc);
      List_Node   : Node_Id;
   begin
      Set_Identifier (Comm_Action, Ident);
      Set_BE_Container (Ident, Comm_Action);

      if Present (Target_Node) then
         Set_Target (Comm_Action, Target_Node);
         Set_BE_Container (Target_Node, Comm_Action);
      end if;

      Set_Comm_Kind (Comm_Action, Communication_Kind'Pos (Com_Kind));

      if not Is_Empty (Sub_Parameters) then
         Set_Subprogram_Parameter_List (Comm_Action, Sub_Parameters);

         List_Node := First_Node (Subprogram_Parameter_List (Comm_Action));
         while Present (List_Node) loop
            Set_BE_Container (List_Node, Comm_Action);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Comm_Action;

   end Add_New_Communication_Action;

   --------------------------
   -- Add_New_Timed_Action --
   --------------------------

   function Add_New_Timed_Action
     (Loc            : Location;
      Container      : Node_Id;
      Fst_Behav_Time : Node_Id;
      Scd_Behav_Time : Node_Id           := No_Node;
      Processor_Idt  : List_Id           := No_List;
      Is_InBinding   : Boolean           := False)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (Fst_Behav_Time) = K_Behavior_Time);

      Timed_Action : constant Node_Id := New_Node (K_Timed_Act, Loc);
   begin
      Set_BE_Container (Timed_Action, Container);

      Set_Fst_Behavior_Time (Timed_Action, Fst_Behav_Time);
      Set_BE_Container (Fst_Behav_Time, Timed_Action);

      if Scd_Behav_Time /= No_Node then
         Set_Scd_Behavior_Time (Timed_Action, Scd_Behav_Time);
      end if;
      if not Is_Empty (Processor_Idt) then
         Set_Processor_Idt (Timed_Action, Processor_Idt);
      end if;
      Set_Is_InBinding (Timed_Action, Is_InBinding);

      return Timed_Action;
   end Add_New_Timed_Action;

   --------------------------------------
   -- Add_New_Data_Component_Reference --
   --------------------------------------

   function Add_New_Data_Component_Reference
     (Loc            : Location;
      Container      : Node_Id;
      Idents         : List_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Assignment_Action
                       or else Kind (Container) = K_Communication_Action
                       or else Kind (Container) = K_Parameter_Label
                       or else Kind (Container) = K_Element_Values
                       or else Kind (Container) = K_Value_Variable);
      pragma Assert (not Is_Empty (Idents));

      Data_Component_Ref : constant Node_Id
        := New_Node (K_Data_Component_Reference, Loc);

      List_Node : Node_Id;
   begin
      Set_BE_Container (Data_Component_Ref, Container);

      if not Is_Empty (Idents) then
         Set_Identifiers (Data_Component_Ref, Idents);

         List_Node := First_Node (Identifiers (Data_Component_Ref));
         while Present (List_Node) loop
            Set_BE_Container (List_Node, Data_Component_Ref);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Data_Component_Ref;
   end Add_New_Data_Component_Reference;

   -----------------------------
   -- Add_New_Parameter_Label --
   -----------------------------

   function Add_New_Parameter_Label
     (Loc       : Location;
      Container : Node_Id;
      Param     : Node_Id)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Communication_Action);
      pragma Assert (Kind (Param) = K_Value_Expression);

      Param_Label : constant Node_Id := New_Node (K_Parameter_Label, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Param_Label, Container);
      end if;

      Set_Parameter (Param_Label, Param);
      Set_BE_Container (Param, Param_Label);

      return Param_Label;
   end Add_New_Parameter_Label;

   ------------------
   -- Add_New_Name --
   ------------------

   function Add_New_Name
     (Loc              : Location;
      Container        : Node_Id;
      Idents           : List_Id;
      Array_Index_List : List_Id := No_List)
     return Node_Id
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Assignment_Action
                       or else Kind (Container) = K_Communication_Action
                       or else Kind (Container) = K_Data_Component_Reference
                       or else Kind (Container) = K_Value_Variable);

      Name_Node : constant Node_Id := New_Node (K_Name, Loc);
   begin
      Set_BE_Container (Name_Node, Container);

      Set_Idt (Name_Node, Idents);

      if not Is_Empty (Array_Index_List) then
         Set_Array_Index (Name_Node, Array_Index_List);
      end if;

      return Name_Node;
   end Add_New_Name;

end Ocarina.Builder.AADL_BA.Actions;
