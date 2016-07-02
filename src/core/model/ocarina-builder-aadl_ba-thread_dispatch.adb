------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL_BA.THREAD_DISPATCH                  --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

package body Ocarina.Builder.AADL_BA.Thread_Dispatch is

   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   ---------------------------------------
   -- Add_New_Dispatch_Condition_Thread --
   ---------------------------------------

   function Add_New_Dispatch_Condition_Thread
     (Loc                        : Location;
      Container                  : Node_Id;
      Dispatch_Trigger_Condition : Node_Id;
      Frozen_Port_List           : List_Id)
     return Node_Id
   is
      pragma Assert (False
                       or else No (Container)
                       or else Kind (Container) = K_Behavior_Condition);

      Dispatch_Condition_Thread : constant Node_Id
        := New_Node (K_Dispatch_Condition_Thread, Loc);

   begin
      Add_New_Dispatch_Condition_Thread (Dispatch_Condition_Thread,
                                         Container,
                                         Dispatch_Trigger_Condition,
                                         Frozen_Port_List);

      if No (Dispatch_Condition_Thread) then
         return No_Node;
      end if;

      return Dispatch_Condition_Thread;
   end Add_New_Dispatch_Condition_Thread;

   procedure Add_New_Dispatch_Condition_Thread
     (Dispatch_Condition_Thread  : Node_Id;
      Container                  : Node_Id  := No_Node;
      Dispatch_Trigger_Condition : Node_Id  := No_Node;
      Frozen_Port_List           : List_Id  := No_List)
   is
      pragma Assert (No (Container)
                       or else Kind (Container) = K_Behavior_Condition);
      pragma Assert (Kind (Dispatch_Condition_Thread) =
                                           K_Dispatch_Condition_Thread);

   begin
      Set_BE_Container (Dispatch_Condition_Thread, Container);

      if Present (Dispatch_Trigger_Condition) then
         Set_Dispatch_Trigger_Condition (Dispatch_Condition_Thread,
                                            Dispatch_Trigger_Condition);
      end if;

      if not Is_Empty (Frozen_Port_List) then
         Set_Frozen_Ports (Dispatch_Condition_Thread, Frozen_Port_List);
      end if;
   end Add_New_Dispatch_Condition_Thread;

   ----------------------------------------
   -- Add_New_Dispatch_Trigger_Condition --
   ----------------------------------------

   function Add_New_Dispatch_Trigger_Condition
     (Loc                       : Location;
      Container                 : Node_Id;
      Trig_Kind                 : Dispatch_Trigger_Kind;
      Dispatch_Conjunction_List : List_Id;
      Behavior_Time             : Node_Id)
     return Node_Id
   is

      Dispatch_Trigger_Condition : constant Node_Id := New_Node
                                        (K_Dispatch_Trigger_Condition, Loc);
   begin
      Set_BE_Container (Dispatch_Trigger_Condition, Container);

      if not Is_Empty (Dispatch_Conjunction_List) then
         Set_Dispatch_Conjunction (Dispatch_Trigger_Condition,
                                           Dispatch_Conjunction_List);
      end if;

      if Trig_Kind /= TRI_Error then
         Set_Trigger_Kind (Dispatch_Trigger_Condition,
                           Dispatch_Trigger_Kind'Pos (Trig_Kind));
      end if;

      if Present (Behavior_Time) then
         Set_Behavior_Time (Dispatch_Trigger_Condition, Behavior_Time);
      end if;

      if No (Dispatch_Trigger_Condition) then
         return No_Node;
      end if;

      return Dispatch_Trigger_Condition;

   end Add_New_Dispatch_Trigger_Condition;

   ----------------------------------
   -- Add_New_Dispatch_Conjunction --
   ----------------------------------

   function Add_New_Dispatch_Conjunction
     (Loc                : Location;
      Container          : Node_Id;
      Dispatch_Trigger_List  : List_Id)
     return Node_Id
   is
      pragma Assert (No (Container));
      pragma Assert (not Is_Empty (Dispatch_Trigger_List));

      Dispatch_Conjunction : constant Node_Id := New_Node
                       (K_Dispatch_Conjunction, Loc);
      List_Node  : Node_Id;
   begin
      if Present (Container) then
         Set_BE_Container (Dispatch_Conjunction, Container);
      end if;

      Set_Dispatch_Triggers (Dispatch_Conjunction, Dispatch_Trigger_List);

      List_Node := First_Node (Dispatch_Triggers (Dispatch_Conjunction));
      while Present (List_Node) loop
         Set_BE_Container (List_Node, Dispatch_Conjunction);

         List_Node := Next_Node (List_Node);
      end loop;

      return Dispatch_Conjunction;

   end Add_New_Dispatch_Conjunction;

end Ocarina.Builder.AADL_BA.Thread_Dispatch;
