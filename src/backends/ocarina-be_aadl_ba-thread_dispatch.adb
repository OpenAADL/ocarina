------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B E _ A A D L _ B A . T H R E A D _ D I S P A T C H    --
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

with Ocarina.Output;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.BE_AADL_BA.Identifiers;
with Ocarina.BE_AADL_BA.Expressions;

package body Ocarina.BE_AADL_BA.Thread_Dispatch is

   use Ocarina.Output;

   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.BE_AADL_BA.Identifiers;
   use Ocarina.BE_AADL_BA.Expressions;

   procedure Print_Dispatch_Logical_Expressions (List : List_Id);
   procedure Print_Dispatch_Trigger_Kind        (Trigger_Kind : Byte);
   procedure Print_Dispatch_Trigger_Condition   (Node : Node_Id);
   procedure Print_Dispatch_Conjunction         (Node : Node_Id);
   procedure Print_Frozen_Ports                 (List : List_Id);

   ------------------------------
   -- Print_Dispatch_Condition --
   ------------------------------

   procedure Print_Dispatch_Condition (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Dispatch_Condition_Thread);

   begin
      Print_Tokens ((T_On, T_Dispatch));

      Print_Dispatch_Trigger_Condition (Dispatch_Trigger_Condition (Node));

      if not Is_Empty (Frozen_Ports (Node)) then
         Write_Space;
         Print_Token (T_Frozen);
         Print_Frozen_Ports (Frozen_Ports (Node));
      end if;
   end Print_Dispatch_Condition;

   --------------------------------------
   -- Print_Dispatch_Trigger_Condition --
   --------------------------------------

   procedure Print_Dispatch_Trigger_Condition (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Dispatch_Trigger_Condition);

   begin
      if not Is_Empty (Dispatch_Conjunction (Node)) then
         Write_Space;
         Print_Dispatch_Logical_Expressions (Dispatch_Conjunction (Node));
      end if;
      if Dispatch_Trigger_Kind'Val (Trigger_Kind (Node)) /= TRI_No_Kind then
         Write_Space;
         Print_Dispatch_Trigger_Kind (Trigger_Kind (Node));
      end if;

      if Present (Behavior_Time (Node)) then
         Write_Space;
         Print_Behavior_Time (Behavior_Time (Node));
      end if;
   end Print_Dispatch_Trigger_Condition;

   ----------------------------------------
   -- Print_Dispatch_Logical_Expressions --
   ----------------------------------------

   procedure Print_Dispatch_Logical_Expressions (List : List_Id) is
      pragma Assert (not Is_Empty (List));

      List_Node : Node_Id;
   begin
      List_Node := First_Node (List);

      while Present (List_Node) loop
         Print_Dispatch_Conjunction (List_Node);

         List_Node := Next_Node (List_Node);
         if Present (List_Node) then
            Write_Space;
            Print_Token (T_Or);
            Write_Space;
         end if;
      end loop;
   end Print_Dispatch_Logical_Expressions;

   --------------------------------
   -- Print_Dispatch_Conjunction --
   --------------------------------

   procedure Print_Dispatch_Conjunction (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Dispatch_Conjunction);

      List_Node : Node_Id;
   begin
      List_Node := First_Node (Dispatch_Triggers (Node));

      while Present (List_Node) loop
         Print_Identifier (List_Node);

         List_Node := Next_Node (List_Node);
         if Present (List_Node) then
            Write_Space;
            Print_Token (T_And);
            Write_Space;
         end if;
      end loop;
   end Print_Dispatch_Conjunction;

   ---------------------------------
   -- Print_Dispatch_Trigger_Kind --
   ---------------------------------

   procedure Print_Dispatch_Trigger_Kind (Trigger_Kind : Byte) is
   begin
      case Dispatch_Trigger_Kind'Val (Trigger_Kind) is
         when TRI_Stop    => Print_Token (T_Stop);
         when TRI_Timeout => Print_Token (T_Timeout);
         when others      => Write_Line  (Bug_Str);
      end case;
   end Print_Dispatch_Trigger_Kind;

   ------------------------
   -- Print_Frozen_Ports --
   ------------------------

   procedure Print_Frozen_Ports (List : List_Id) is
      pragma Assert (not Is_Empty (List));

      List_Node : Node_Id;
   begin
      List_Node := First_Node (List);
      Write_Space;
      Print_Identifier (List_Node);

      List_Node := Next_Node (List_Node);
      while Present (List_Node) loop
         Print_Token (T_Comma);
         Write_Space;
         Print_Identifier (List_Node);

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Frozen_Ports;

end Ocarina.BE_AADL_BA.Thread_Dispatch;
