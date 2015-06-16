------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL_BA.THREAD_DISPATCH                  --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.Types;
with Locations;

with Ocarina.ME_AADL_BA;

package Ocarina.Builder.Aadl_Ba.Thread_Dispatch is

   use Ocarina.Types;
   use Locations;
   use Ocarina.ME_AADL_BA;

   function Add_New_Dispatch_Condition
     (Loc              : Location;
      Container        : Node_Id;
      Expressions      : List_Id;
      Frozen_Port_List : List_Id) return Node_Id;

   procedure Add_New_Dispatch_Condition
     (Dispatch_Condition : Node_Id;
      Container          : Node_Id := No_Node;
      Expressions        : List_Id := No_List;
      Frozen_Port_List   : List_Id := No_List);

   function Add_New_Dispatch_Trigger
     (Loc                 : Location;
      Container           : Node_Id;
      Trig_Kind           : Dispatch_Trigger_Kind;
      Trigger_Conjunction : Node_Id;
      Behavior_Time_Node  : Node_Id) return Node_Id;

   function Add_New_Dispatch_Trigger_Conjunction
     (Loc            : Location;
      Container      : Node_Id;
      Trigger_Event  : Node_Id;
      Trigger_Events : List_Id;
      Numeral        : Node_Id;
      Is_Ormore      : Boolean;
      Is_Orless      : Boolean) return Node_Id;

end Ocarina.Builder.Aadl_Ba.Thread_Dispatch;
