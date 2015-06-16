------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL_BA.SPECIFICATIONS                   --
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

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils; use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

package body Ocarina.Builder.Aadl_Ba.Specifications is

   ----------------------------
   -- Add_New_Behavior_Annex --
   ----------------------------

   function Add_New_Behavior_Annex
     (Loc         : Location;
      Container   : Node_Id;
      Variables   : List_Id;
      States      : List_Id;
      Transitions : List_Id) return Node_Id
   is
      --  pragma Unreferenced (Container);
      --  fixme check Container

      Behavior_Annex : constant Node_Id := New_Node (K_Behavior_Annex, Loc);
   begin
      Add_New_Behavior_Annex
        (Behavior_Annex,
         Container,
         Variables,
         States,
         Transitions);

      if No (Behavior_Annex) then
         return No_Node;
      else
         return Behavior_Annex;
      end if;
   end Add_New_Behavior_Annex;

   ----------------------------
   -- Add_New_Behavior_Annex --
   ----------------------------

   procedure Add_New_Behavior_Annex
     (Behavior_Annex : Node_Id;
      Container      : Node_Id := No_Node;
      Variables      : List_Id := No_List;
      States         : List_Id := No_List;
      Transitions    : List_Id := No_List)
   is
      pragma Unreferenced (Container);
      pragma Assert (Kind (Behavior_Annex) = K_Behavior_Annex);
   begin
      if not Is_Empty (Variables) then
         Set_Variables (Behavior_Annex, Variables);
      end if;

      if not Is_Empty (States) then
         Set_States (Behavior_Annex, States);
      end if;

      if not Is_Empty (Transitions) then
         Set_Transitions (Behavior_Annex, Transitions);
      end if;
   end Add_New_Behavior_Annex;

   -------------------------------
   -- Add_New_Behavior_Variable --
   -------------------------------

   function Add_New_Behavior_Variable
     (Loc        : Location;
      Container  : Node_Id;
      Ident_List : List_Id;
      Class_Ref  : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Container) = K_Behavior_Annex);

      Behavior_Variable : constant Node_Id :=
        New_Node (K_Behavior_Variable, Loc);
   begin
      Add_New_Behavior_Variable
        (Behavior_Variable,
         Container,
         Ident_List,
         Class_Ref);

      if No (Behavior_Variable) then
         return No_Node;
      else
         return Behavior_Variable;
      end if;
   end Add_New_Behavior_Variable;

   -------------------------------
   -- Add_New_Behavior_Variable --
   -------------------------------

   procedure Add_New_Behavior_Variable
     (Behavior_Variable : Node_Id;
      Container         : Node_Id := No_Node;
      Ident_List        : List_Id;
      Class_Ref         : Node_Id := No_Node)
   is
      pragma Assert (Kind (Behavior_Variable) = K_Behavior_Variable);
   begin
      if Container /= No_Node then
         Set_BE_Container (Behavior_Variable, Container);
      end if;

      if not Is_Empty (Ident_List) then
         Set_Identifiers (Behavior_Variable, Ident_List);
      end if;

      if Class_Ref /= No_Node then
         Set_Classifier_Ref (Behavior_Variable, Class_Ref);
      end if;
   end Add_New_Behavior_Variable;

   ----------------------------
   -- Add_New_Behavior_State --
   ----------------------------

   function Add_New_Behavior_State
     (Loc        : Location;
      Container  : Node_Id;
      Ident_List : List_Id;
      State_Kind : Behavior_State_Kind) return Node_Id
   is
      pragma Assert (Kind (Container) = K_Behavior_Annex);

      Behavior_State : constant Node_Id := New_Node (K_Behavior_State, Loc);
   begin
      Add_New_Behavior_State
        (Behavior_State,
         Container,
         Ident_List,
         State_Kind);

      if No (Behavior_State) then
         return No_Node;
      else
         return Behavior_State;
      end if;
   end Add_New_Behavior_State;

   ----------------------------
   -- Add_New_Behavior_State --
   ----------------------------

   procedure Add_New_Behavior_State
     (Behavior_State : Node_Id;
      Container      : Node_Id             := No_Node;
      Ident_List     : List_Id             := No_List;
      State_Kind     : Behavior_State_Kind := BSK_Error)
   is
      pragma Assert (Kind (Behavior_State) = K_Behavior_State);
   begin
      if Container /= No_Node then
         Set_BE_Container (Behavior_State, Container);
      end if;

      if not Is_Empty (Ident_List) then
         Set_Identifiers (Behavior_State, Ident_List);
      end if;

      Set_State_Kind (Behavior_State, Behavior_State_Kind'Pos (State_Kind));

   end Add_New_Behavior_State;

   ---------------------------------
   -- Add_New_Behavior_Transition --
   ---------------------------------

   function Add_New_Behavior_Transition
     (Loc             : Location;
      Container       : Node_Id;
      Transition_Node : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Container) = K_Behavior_Annex);

      Behavior_Transition : constant Node_Id :=
        New_Node (K_Behavior_Transition, Loc);
   begin
      if Transition_Node /= No_Node then
         Set_Transition (Behavior_Transition, Transition_Node);
         Set_BE_Container (Transition_Node, Behavior_Transition);
      end if;

      if No (Behavior_Transition) then
         return No_Node;
      end if;

      return Behavior_Transition;
   end Add_New_Behavior_Transition;

   --------------------------------
   -- Add_New_Execute_Transition --
   --------------------------------

   function Add_New_Execute_Transition
     (Loc                 : Location;
      Container           : Node_Id;
      Transition_Idt      : Node_Id;
      Transition_Priority : Node_Id;
      Sources             : List_Id;
      Behavior_Condition  : Node_Id;
      Destination         : Node_Id;
      Behavior_Act_List   : List_Id) return Node_Id
   is
      --  pragma Assert (Kind (Container) = K_Behavior_Transition);

      Execute_Transition : constant Node_Id :=
        New_Node (K_Execution_Behavior_Transition, Loc);
   begin
      Add_New_Execute_Transition
        (Execute_Transition,
         Container,
         Transition_Idt,
         Transition_Priority,
         Sources,
         Behavior_Condition,
         Destination,
         Behavior_Act_List);

      if No (Execute_Transition) then
         return No_Node;
      else
         return Execute_Transition;
      end if;
   end Add_New_Execute_Transition;

   ---------------------------------
   -- Add_New_Execute_Transition --
   ---------------------------------

   procedure Add_New_Execute_Transition
     (Execute_Transition  : Node_Id;
      Container           : Node_Id := No_Node;
      Transition_Idt      : Node_Id := No_Node;
      Transition_Priority : Node_Id := No_Node;
      Sources             : List_Id := No_List;
      Behavior_Condition  : Node_Id := No_Node;
      Destination         : Node_Id := No_Node;
      Behavior_Act_List   : List_Id := No_List)
   is
      pragma Assert
        (Kind (Execute_Transition) = K_Execution_Behavior_Transition);
   begin
      if Container /= No_Node then
         Set_BE_Container (Execute_Transition, Container);
      end if;

      if Transition_Idt /= No_Node then
         Set_Behavior_Transition_Idt (Execute_Transition, Transition_Idt);
      end if;

      if Transition_Priority /= No_Node then
         Set_Behavior_Transition_Priority
           (Execute_Transition,
            Transition_Priority);
      end if;

      if not Is_Empty (Sources) then
         Set_Sources (Execute_Transition, Sources);
      end if;

      if Behavior_Condition /= No_Node then
         Set_Behavior_Condition (Execute_Transition, Behavior_Condition);
      end if;

      if Destination /= No_Node then
         Set_Destination (Execute_Transition, Destination);
      end if;

      if not Is_Empty (Behavior_Act_List) then
         Set_Behavior_Actions (Execute_Transition, Behavior_Act_List);
      end if;
   end Add_New_Execute_Transition;

   --------------------------------
   -- Add_New_Behavior_Condition --
   --------------------------------

   function Add_New_Behavior_Condition
     (Loc            : Location;
      Container      : Node_Id;
      Condition_Node : Node_Id) return Node_Id
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Behavior_Transition);
      pragma Assert
        (Kind (Condition_Node) = K_Dispatch_Condition
         or else Kind (Condition_Node) = K_Value_Expression);

      Behavior_Condition : constant Node_Id :=
        New_Node (K_Behavior_Condition, Loc);
   begin
      if Present (Container) then
         Set_BE_Container (Behavior_Condition, Container);
      end if;

      if Condition_Node /= No_Node then
         Set_Condition (Behavior_Condition, Condition_Node);
         Set_BE_Container (Condition_Node, Behavior_Condition);
      end if;

      if No (Behavior_Condition) then
         return No_Node;
      end if;

      return Behavior_Condition;
   end Add_New_Behavior_Condition;

end Ocarina.Builder.Aadl_Ba.Specifications;
