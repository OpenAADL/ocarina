------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B U I L D E R . A A D L _ B A . A C T I O N S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2015 ESA & ISAE.                    --
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

package body Ocarina.Builder.Aadl_Ba.Actions is

   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   -----------------------------
   -- Add_New_Behavior_Action --
   -----------------------------

   function Add_New_Behavior_Action
     (Loc         : Location;
      Container   : Node_Id;
      Action_Node : Node_Id) return Node_Id
   is
      pragma Assert
        (No (Container)
         or else Kind (Container) = K_Execution_Behavior_Transition
         or else Kind (Container) = K_Mode_Transition
         or else Kind (Container) = K_Behavior_Action);

      pragma Assert
        (Kind (Action_Node) = K_Assignment_Action
         or else Kind (Action_Node) = K_Communication_Action
         or else Kind (Action_Node) = K_Timed_Action
         or else Kind (Action_Node) = K_If_Cond_Struct
         or else Kind (Action_Node) = K_For_Cond_Struct
         or else Kind (Action_Node) = K_While_Cond_Struct);

      Behavior_Action : constant Node_Id := New_Node (K_Behavior_Action, Loc);
   begin
      Set_BE_Container (Behavior_Action, Container);
      Set_Action (Behavior_Action, Action_Node);

      Set_BE_Container (Action_Node, Behavior_Action);

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
     (If_Cond_Struct : Node_Id;
      Container      : Node_Id := No_Node;
      If_Stat        : Node_Id := No_Node;
      Elsif_Stat     : Node_Id := No_Node;
      Else_Stat      : Node_Id := No_Node)
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (If_Cond_Struct) = K_If_Cond_Struct);
      pragma Assert (Kind (If_Stat) = K_Conditional_Statement);
      pragma Assert
        (No (Elsif_Stat) or else Kind (Elsif_Stat) = K_Conditional_Statement);
      pragma Assert
        (No (Elsif_Stat) or else Kind (Else_Stat) = K_Conditional_Statement);
   begin
      Set_BE_Container (If_Cond_Struct, Container);
      --  Container may be No_Node

      Set_If_Statement (If_Cond_Struct, If_Stat);
      Set_Elsif_Statement (If_Cond_Struct, Elsif_Stat);
      Set_Else_Statement (If_Cond_Struct, Else_Stat);

   end Add_New_If_Cond_Struct;

   -----------------------------------
   -- Add_New_Conditional_Statement --
   -----------------------------------

   function Add_New_Conditional_Statement
     (Loc        : Location;
      Container  : Node_Id;
      Expression : Node_Id;
      Actions    : List_Id) return Node_Id
   is
      pragma Assert
        (Kind (Container) = K_If_Cond_Struct
         or else Kind (Container) = K_While_Cond_Struct);

      Cond_Statement : constant Node_Id :=
        New_Node (K_Conditional_Statement, Loc);
      List_Node : Node_Id;
   begin
      Set_BE_Container (Cond_Statement, Container);

      if Present (Expression) then
         Set_Logical_Expr (Cond_Statement, Expression);
      end if;

      if not Is_Empty (Actions) then
         Set_Behavior_Actions (Cond_Statement, Actions);

         List_Node := First_Node (Behavior_Actions (Cond_Statement));
         while Present (List_Node) loop
            Set_BE_Container (List_Node, Cond_Statement);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Cond_Statement;

   end Add_New_Conditional_Statement;

   ----------------------------
   -- Add_New_For_Cond_Struct --
   ----------------------------

   function Add_New_For_Cond_Struct (Loc : Location) return Node_Id is
      For_Cond_Struct : constant Node_Id := New_Node (K_For_Cond_Struct, Loc);
   begin
      return For_Cond_Struct;

   end Add_New_For_Cond_Struct;

   ----------------------------
   -- Add_New_For_Cond_Struct --
   ----------------------------

   procedure Add_New_For_Cond_Struct
     (For_Cond_Struct : Node_Id;
      Container       : Node_Id;
      Variable_Id     : Node_Id;
      Range_Node      : Node_Id;
      Actions         : List_Id)
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (For_Cond_Struct) = K_For_Cond_Struct);
      pragma Assert (Kind (Variable_Id) = K_Identifier);
      pragma Assert
        (Kind (Range_Node) = K_Id or else Kind (Range_Node) = K_Integer_Range);
      pragma Assert (not Is_Empty (Actions));

   begin
      if Present (Container) then
         Set_BE_Container (For_Cond_Struct, Container);
      end if;

      Set_Var_Identifier (For_Cond_Struct, Variable_Id);
      Set_In_Range (For_Cond_Struct, Range_Node);
      Set_Behavior_Actions (For_Cond_Struct, Actions);

   end Add_New_For_Cond_Struct;

   -------------------------------
   -- Add_New_Assignment_Action --
   -------------------------------

   function Add_New_Assignment_Action
     (Loc         : Location;
      Container   : Node_Id;
      Ident       : Node_Id;
      Value_Expr  : Node_Id;
      Is_Any_Bool : Boolean) return Node_Id
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Behavior_Action);
      pragma Assert
        (Kind (Ident) = K_Id
         or else Kind (Ident) = K_Data_Component_Reference);
      --  fixme : Todo Set Assert for Value_Expression

      Assignment_Action : constant Node_Id :=
        New_Node (K_Assignment_Action, Loc);
   begin
      Set_Target (Assignment_Action, Ident);
      Set_BE_Container (Ident, Assignment_Action);

      Set_Value_Expression (Assignment_Action, Value_Expr);
      Set_BE_Container (Value_Expr, Assignment_Action);

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
      Com_Kind       : Communication_Kind) return Node_Id
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (Ident) = K_Id);
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
      Distribution   : Distribution_Kind := DK_No_Kind;
      Is_Comput      : Boolean           := False) return Node_Id
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Behavior_Action);
      pragma Assert (Kind (Fst_Behav_Time) = K_Behavior_Time);
      pragma Assert
        (No (Scd_Behav_Time) or else Kind (Scd_Behav_Time) = K_Behavior_Time);
      pragma Assert (Distribution /= DK_Error);

      Timed_Action : constant Node_Id := New_Node (K_Timed_Action, Loc);
   begin
      Set_BE_Container (Timed_Action, Container);

      Set_Fst_Behavior_Time (Timed_Action, Fst_Behav_Time);
      Set_BE_Container (Fst_Behav_Time, Timed_Action);

      Set_Scd_Behavior_Time (Timed_Action, Scd_Behav_Time);
      if Present (Scd_Behav_Time) then
         Set_BE_Container (Scd_Behav_Time, Timed_Action);
      end if;

      Set_Distrib_Kind (Timed_Action, Distribution_Kind'Pos (Distribution));
      Set_Is_Computation (Timed_Action, Is_Comput);

      return Timed_Action;
   end Add_New_Timed_Action;

   --------------------------------------
   -- Add_New_Data_Component_Reference --
   --------------------------------------

   function Add_New_Data_Component_Reference
     (Loc       : Location;
      Container : Node_Id;
      Idents    : List_Id) return Node_Id
   is
      pragma Assert
        (No (Container)
         or else Kind (Container) = K_Assignment_Action
         or else Kind (Container) = K_Range
         or else Kind (Container) = K_Value_Holder);
      pragma Assert (not Is_Empty (Idents));

      Data_Component_Ref : constant Node_Id :=
        New_Node (K_Data_Component_Reference, Loc);

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
      Param     : Node_Id) return Node_Id
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Communication_Action);

      Param_Label : constant Node_Id := New_Node (K_Parameter_Label, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Param_Label, Container);
      end if;

      Set_Parameter (Param_Label, Param);
      Set_BE_Container (Param, Param_Label);

      return Param_Label;
   end Add_New_Parameter_Label;

   ----------------
   -- Add_New_Id --
   ----------------

   function Add_New_Id
     (Loc          : Location;
      Container    : Node_Id;
      Ident        : Node_Id;
      Value_Holder : Node_Id) return Node_Id
   is
      pragma Assert
        (No (Container)
         or else Kind (Container) = K_Range
         or else Kind (Container) = K_Assignment_Action
         or else Kind (Container) = K_Communication_Action
         or else Kind (Container) = K_Data_Component_Reference
         or else Kind (Container) = K_Value_Holder);

      Id_Node : constant Node_Id := New_Node (K_Id, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Id_Node, Container);
      end if;

      Set_Identifier (Id_Node, Ident);
      Set_BE_Container (Ident, Id_Node);

      if Present (Value_Holder) then
         Set_Int_Value_Holder (Id_Node, Value_Holder);
         Set_BE_Container (Value_Holder, Id_Node);
      end if;

      return Id_Node;

   end Add_New_Id;

end Ocarina.Builder.Aadl_Ba.Actions;
