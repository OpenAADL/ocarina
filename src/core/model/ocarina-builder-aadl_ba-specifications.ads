------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL_BA.SPECIFICATIONS                   --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.Types;
with Locations;

with Ocarina.ME_AADL_BA;

package Ocarina.Builder.AADL_BA.Specifications is

   use Ocarina.Types;
   use Locations;
   use Ocarina.ME_AADL_BA;

   function Add_New_Behavior_Annex
     (Loc         : Location;
      Container   : Node_Id;
      Variables   : List_Id;
      States      : List_Id;
      Transitions : List_Id)
     return Node_Id;

   procedure Add_New_Behavior_Annex
     (Behavior_Annex : Node_Id;
      Container      : Node_Id := No_Node;
      Variables      : List_Id := No_List;
      States         : List_Id := No_List;
      Transitions    : List_Id := No_List);

   function Add_New_Behavior_Variable
     (Loc          : Location;
      Container    : Node_Id;
      Ident_List   : List_Id;
      Class_Ref    : Node_Id)
     return Node_Id;

   procedure Add_New_Behavior_Variable
     (Behavior_Variable : Node_Id;
      Container         : Node_Id := No_Node;
      Ident_List        : List_Id;
      Class_Ref         : Node_Id := No_Node);

   function Add_New_Behavior_State
     (Loc         : Location;
      Container   : Node_Id;
      Ident_List  : List_Id;
      State_Kind  : Behavior_State_Kind)
     return Node_Id;

   procedure Add_New_Behavior_State
     (Behavior_State : Node_Id;
      Container      : Node_Id             := No_Node;
      Ident_List     : List_Id             := No_List;
      State_Kind     : Behavior_State_Kind := BSK_Error);

   function Add_New_Behavior_Transition
     (Loc             : Location;
      Container       : Node_Id;
      Transition_Node : Node_Id)
     return Node_Id;

   function Add_New_Execute_Transition
     (Loc                 : Location;
      Container           : Node_Id;
      Transition_Idt      : Node_Id;
      Transition_Priority : Node_Id;
      Sources             : List_Id;
      Behavior_Condition  : Node_Id;
      Destination         : Node_Id;
      Behavior_Act_Block  : Node_Id)
     return Node_Id;

   procedure Add_New_Execute_Transition
     (Execute_Transition  : Node_Id;
      Container           : Node_Id := No_Node;
      Transition_Idt      : Node_Id := No_Node;
      Transition_Priority : Node_Id := No_Node;
      Sources             : List_Id := No_List;
      Behavior_Condition  : Node_Id := No_Node;
      Destination         : Node_Id := No_Node;
      Behavior_Act_Block  : Node_Id := No_Node);

   function Add_New_Behavior_Condition
     (Loc              : Location;
      Container        : Node_Id;
      Condition_Node   : Node_Id)
     return Node_Id;

   function Add_New_Execute_Condition
     (Loc               : Location;
      Container         : Node_Id;
      Value_Expr        : Node_Id;
      Is_Otherwise_Bool : Boolean)
     return Node_Id;

   function Add_New_Mode_Condition
     (Loc                  : Location;
      Container            : Node_Id;
      Trigger_Logical_Expr : Node_Id)
     return Node_Id;

   function Add_New_Trigger_Logical_Expr
     (Loc                       : Location;
      Container                 : Node_Id;
      Trigger_Logical_Expr_List : List_Id)
     return Node_Id;

   function Add_New_Event_Trigger
     (Loc                  : Location;
      Container            : Node_Id;
      Port_Component_Ref   : Node_Id;
      Trigger_Logical_Expr : Node_Id)
     return Node_Id;

   function Add_New_Port_Component_Reference
     (Loc               : Location;
      Container         : Node_Id;
      Subcomponent_Name : Node_Id;
      Port_Identifier   : Node_Id)
     return Node_Id;

end Ocarina.Builder.AADL_BA.Specifications;
