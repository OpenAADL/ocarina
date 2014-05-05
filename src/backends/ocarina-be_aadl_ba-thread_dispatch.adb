------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B E _ A A D L _ B A . T H R E A D _ D I S P A T C H    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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
   procedure Print_Dispatch_Trigger             (Node : Node_Id);
   procedure Print_Dispatch_Trigger_Kind        (Trigger_Kind : Byte);
   procedure Print_Dispatch_Trigger_Conjunction (Node : Node_Id);
   procedure Print_Frozen_Ports                 (List : List_Id);

   ------------------------------
   -- Print_Dispatch_Condition --
   ------------------------------

   procedure Print_Dispatch_Condition (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Dispatch_Condition);

   begin
      Print_Tokens ((T_On, T_Dispatch));

      Print_Dispatch_Logical_Expressions (Dispatch_Logical_Expressions (Node));

      if not Is_Empty (Frozen_Ports (Node)) then
         Print_Frozen_Ports (Frozen_Ports (Node));
      end if;
   end Print_Dispatch_Condition;

   ----------------------------------------
   -- Print_Dispatch_Logical_Expressions --
   ----------------------------------------

   procedure Print_Dispatch_Logical_Expressions (List : List_Id) is
      List_Node : Node_Id;

   begin
      if not Is_Empty (List) then
         List_Node := First_Node (List);

         while Present (List_Node) loop
            Print_Dispatch_Trigger (List_Node);

            List_Node := Next_Node (List_Node);
            if Present (List_Node) then
               Write_Space;
               Print_Token (T_Or);
               Write_Space;
            end if;
         end loop;
      end if;
   end Print_Dispatch_Logical_Expressions;

   ----------------------------
   -- Print_Dispatch_Trigger --
   ----------------------------

   procedure Print_Dispatch_Trigger (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Dispatch_Trigger);

   begin
      if Present (Dispatch_Trigger_Conjunction (Node)) then
         Print_Dispatch_Trigger_Conjunction
           (Dispatch_Trigger_Conjunction (Node));
      end if;

      if Dispatch_Trigger_Kind'Val (Trigger_Kind (Node)) /= TRI_No_Kind then
         Write_Space;
         Print_Dispatch_Trigger_Kind (Trigger_Kind (Node));
      end if;

      if Present (Behavior_Time (Node)) then
         Write_Space;
         Print_Behavior_Time (Behavior_Time (Node));
      end if;
   end Print_Dispatch_Trigger;

   ---------------------------------
   -- Print_Dispatch_Trigger_Kind --
   ---------------------------------

   procedure Print_Dispatch_Trigger_Kind (Trigger_Kind : Byte) is
   begin
      case Dispatch_Trigger_Kind'Val (Trigger_Kind) is
         when TRI_Abort   => Print_Token (T_Abort);
         when TRI_Stop    => Print_Token (T_Stop);
         when TRI_Timeout => Print_Token (T_Timeout);
         when others      => Write_Line  (Bug_Str);
      end case;
   end Print_Dispatch_Trigger_Kind;

   ----------------------------------------
   -- Print_Dispatch_Trigger_Conjunction --
   ----------------------------------------

   procedure Print_Dispatch_Trigger_Conjunction (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Dispatch_Trigger_Conjunction);

      Is_Numeral : Boolean := False;
      List_Node  : Node_Id;
   begin
      if Present (Numeral (Node)) then
         Is_Numeral := True;

         Print_Integer_Value (Numeral (Node));

         if Is_Ormore (Node) then
            Print_Token (T_Ormore);
         elsif Is_Orless (Node) then
            Print_Token (T_Orless);
         end if;
      end if;

      if Present (Dispatch_Trigger_Event (Node)) then
         Write_Space;
         Print_Identifier_With_Value (Dispatch_Trigger_Event (Node));

      elsif not Is_Empty (Dispatch_Trigger_Events (Node)) then
         Write_Space;
         Print_Token (T_Left_Parenthesis);

         List_Node := First_Node (Dispatch_Trigger_Events (Node));
         Print_Identifier_With_Value (List_Node);

         List_Node := Next_Node (List_Node);
         while Present (List_Node) loop
            --  fixme : todo print constant_value or others BA 2.9

            if Is_Numeral then
               Print_Token (T_Comma);
            else
               Print_Token (T_And);
            end if;

            Write_Space;
            Print_Identifier (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;

         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_Dispatch_Trigger_Conjunction;

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
