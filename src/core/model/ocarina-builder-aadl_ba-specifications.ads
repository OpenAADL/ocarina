------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL_BA.SPECIFICATIONS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Types;
with Locations;

with Ocarina.ME_AADL_BA;

package Ocarina.Builder.AADL_BA.Specifications is

   use Types;
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
      Behavior_Act_List   : List_Id)
     return Node_Id;

   procedure Add_New_Execute_Transition
     (Execute_Transition  : Node_Id;
      Container           : Node_Id := No_Node;
      Transition_Idt      : Node_Id := No_Node;
      Transition_Priority : Node_Id := No_Node;
      Sources             : List_Id := No_List;
      Behavior_Condition  : Node_Id := No_Node;
      Destination         : Node_Id := No_Node;
      Behavior_Act_List   : List_Id := No_List);

   function Add_New_Behavior_Condition
     (Loc              : Location;
      Container        : Node_Id;
      Condition_Node   : Node_Id)
     return Node_Id;

end Ocarina.Builder.AADL_BA.Specifications;
