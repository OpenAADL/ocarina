------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL_BA.THREAD_DISPATCH                  --
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

package Ocarina.Builder.AADL_BA.Thread_Dispatch is

   use Types;
   use Locations;
   use Ocarina.ME_AADL_BA;

   function Add_New_Dispatch_Condition
     (Loc              : Location;
      Container        : Node_Id;
      Expressions      : List_Id;
      Frozen_Port_List : List_Id)
     return Node_Id;

   procedure Add_New_Dispatch_Condition
     (Dispatch_Condition : Node_Id;
      Container          : Node_Id  := No_Node;
      Expressions        : List_Id  := No_List;
      Frozen_Port_List   : List_Id  := No_List);

   function Add_New_Dispatch_Trigger
     (Loc                 : Location;
      Container           : Node_Id;
      Trig_Kind           : Dispatch_Trigger_Kind;
      Trigger_Conjunction : Node_Id;
      Behavior_Time_Node  : Node_Id)
     return Node_Id;

   function Add_New_Dispatch_Trigger_Conjunction
     (Loc            : Location;
      Container      : Node_Id;
      Trigger_Event  : Node_Id;
      Trigger_Events : List_Id;
      Numeral        : Node_Id;
      Is_Ormore      : Boolean;
      Is_Orless      : Boolean)
     return Node_Id;

end Ocarina.Builder.AADL_BA.Thread_Dispatch;
