------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BUILDER.AADL_BA.THREAD_DISPATCH                  --
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
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

package body Ocarina.Builder.Aadl_Ba.Thread_Dispatch is

   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   --------------------------------
   -- Add_New_Dispatch_Condition --
   --------------------------------

   function Add_New_Dispatch_Condition
     (Loc              : Location;
      Container        : Node_Id;
      Expressions      : List_Id;
      Frozen_Port_List : List_Id) return Node_Id
   is
      pragma Assert
        (False
         or else No (Container)
         or else Kind (Container) = K_Behavior_Condition);

      Dispatch_Condition : constant Node_Id :=
        New_Node (K_Dispatch_Condition, Loc);

   begin
      Add_New_Dispatch_Condition
        (Dispatch_Condition,
         Container,
         Expressions,
         Frozen_Port_List);

      if No (Dispatch_Condition) then
         return No_Node;
      end if;

      return Dispatch_Condition;
   end Add_New_Dispatch_Condition;

   procedure Add_New_Dispatch_Condition
     (Dispatch_Condition : Node_Id;
      Container          : Node_Id := No_Node;
      Expressions        : List_Id := No_List;
      Frozen_Port_List   : List_Id := No_List)
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Behavior_Condition);
      pragma Assert (Kind (Dispatch_Condition) = K_Dispatch_Condition);

   begin
      Set_BE_Container (Dispatch_Condition, Container);

      if not Is_Empty (Expressions) then
         Set_Dispatch_Logical_Expressions (Dispatch_Condition, Expressions);
      end if;

      if not Is_Empty (Frozen_Port_List) then
         Set_Frozen_Ports (Dispatch_Condition, Frozen_Port_List);
      end if;
   end Add_New_Dispatch_Condition;

   ------------------------------
   -- Add_New_Dispatch_Trigger --
   ------------------------------

   function Add_New_Dispatch_Trigger
     (Loc                 : Location;
      Container           : Node_Id;
      Trig_Kind           : Dispatch_Trigger_Kind;
      Trigger_Conjunction : Node_Id;
      Behavior_Time_Node  : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Container) = K_Dispatch_Condition);

      Dispatch_Trigger : constant Node_Id :=
        New_Node (K_Dispatch_Trigger, Loc);
   begin
      Set_BE_Container (Dispatch_Trigger, Container);

      if Present (Trigger_Conjunction) then
         Set_Dispatch_Trigger_Conjunction
           (Dispatch_Trigger,
            Trigger_Conjunction);
         Set_BE_Container (Trigger_Conjunction, Dispatch_Trigger);
      end if;

      if Trig_Kind /= TRI_Error then
         Set_Trigger_Kind
           (Dispatch_Trigger,
            Dispatch_Trigger_Kind'Pos (Trig_Kind));
      end if;

      if Present (Behavior_Time_Node) then
         Set_Behavior_Time (Dispatch_Trigger, Behavior_Time_Node);
         Set_BE_Container (Behavior_Time_Node, Dispatch_Trigger);
      end if;

      if No (Dispatch_Trigger) then
         return No_Node;
      end if;

      return Dispatch_Trigger;

   end Add_New_Dispatch_Trigger;

   ------------------------------------------
   -- Add_New_Dispatch_Trigger_Conjunction --
   ------------------------------------------

   function Add_New_Dispatch_Trigger_Conjunction
     (Loc            : Location;
      Container      : Node_Id;
      Trigger_Event  : Node_Id;
      Trigger_Events : List_Id;
      Numeral        : Node_Id;
      Is_Ormore      : Boolean;
      Is_Orless      : Boolean) return Node_Id
   is
      pragma Assert
        (No (Container) or else Kind (Container) = K_Dispatch_Trigger);
      pragma Assert
        (No (Trigger_Event)
         or else Kind (Trigger_Event) = K_Identifier
         or else Kind (Trigger_Event) = K_Identifier_With_Value);

      Trigger_Conjunction : constant Node_Id :=
        New_Node (K_Dispatch_Trigger_Conjunction, Loc);
   begin
      Set_BE_Container (Trigger_Conjunction, Container);
      Set_Numeral (Trigger_Conjunction, Numeral);
      Set_Is_Ormore (Trigger_Conjunction, Is_Ormore);
      Set_Is_Orless (Trigger_Conjunction, Is_Orless);

      if Present (Trigger_Event) then
         Set_Dispatch_Trigger_Event (Trigger_Conjunction, Trigger_Event);
      elsif not Is_Empty (Trigger_Events) then
         Set_Dispatch_Trigger_Events (Trigger_Conjunction, Trigger_Events);
      end if;

      if No (Trigger_Conjunction) then
         return No_Node;
      end if;

      return Trigger_Conjunction;

   end Add_New_Dispatch_Trigger_Conjunction;

end Ocarina.Builder.Aadl_Ba.Thread_Dispatch;
