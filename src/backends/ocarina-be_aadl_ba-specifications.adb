------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B E _ A A D L _ B A . S P E C I F I C A T I O N S     --
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

with Ocarina.Output;

with Ocarina.ME_AADL_BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Ocarina.BE_AADL_BA.Identifiers;
with Ocarina.BE_AADL_BA.Thread_Dispatch;
with Ocarina.BE_AADL_BA.Actions;
with Ocarina.BE_AADL_BA.Expressions;

package body Ocarina.BE_AADL_BA.Specifications is

   use Ocarina.Output;
   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.BE_AADL_BA.Identifiers;
   use Ocarina.BE_AADL_BA.Thread_Dispatch;
   use Ocarina.BE_AADL_BA.Actions;
   use Ocarina.BE_AADL_BA.Expressions;

   package BAN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   procedure Print_Behavior_Variable (Node : Node_Id);
   procedure Print_Behavior_State (Node : Node_Id);
   procedure Print_Behavior_State_Kind (State_Kind : Byte);
   procedure Print_Behavior_Transition (Node : Node_Id);
   procedure Print_Execution_Behavior_Transition (Node : Node_Id);
   procedure Print_Mode_Transition (Node : Node_Id);
   procedure Print_Behavior_Condition (Node : Node_Id);

   --------------------------
   -- Print_Behavior_Annex --
   --------------------------

   procedure Print_Behavior_Annex (Node : Node_Id) is

      List_Node : Node_Id;
   begin
      Write_Eol;

      if not Is_Empty (Variables (Node)) then
         Write_Indentation;
         Print_Token (T_Variables);
         Write_Eol;

         List_Node := First_Node (Variables (Node));

         while Present (List_Node) loop
            Write_Indentation (+2);
            Print_Behavior_Variable (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Write_Indentation;
      if not Is_Empty (States (Node)) then
         Print_Token (T_States);
         Write_Eol;

         List_Node := First_Node (States (Node));

         while Present (List_Node) loop
            Write_Indentation (+2);
            Print_Behavior_State (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Write_Indentation;
      if not Is_Empty (Transitions (Node)) then
         Print_Token (T_Transitions);

         List_Node := First_Node (Transitions (Node));

         while Present (List_Node) loop
            Write_Indentation (+2);
            Print_Behavior_Transition (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Write_Eol;
   end Print_Behavior_Annex;

   -----------------------------
   -- Print_Behavior_Variable --
   -----------------------------

   procedure Print_Behavior_Variable (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_Variable);

      List_Node : Node_Id;
   begin
      if not Is_Empty (BAN.Identifiers (Node)) then
         List_Node := First_Node (BAN.Identifiers (Node));

         while Present (List_Node) loop
            Print_Identifier (List_Node);

            if Present (Next_Node (List_Node)) then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if Present (Classifier_Ref (Node)) then
         Write_Space;
         Print_Token (T_Colon);
         Write_Space;
         Print_Component_Classifier_Ref (Classifier_Ref (Node));
      end if;

      Print_Token (T_Semicolon);

      Write_Eol;
   end Print_Behavior_Variable;

   --------------------------
   -- Print_Behavior_State --
   --------------------------

   procedure Print_Behavior_State (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_State);

      List_Node : Node_Id;
   begin
      if not Is_Empty (BAN.Identifiers (Node)) then
         List_Node := First_Node (BAN.Identifiers (Node));

         while Present (List_Node) loop
            Print_Identifier (List_Node);

            if Present (Next_Node (List_Node)) then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Write_Space;
      Print_Token (T_Colon);

      if Behavior_State_Kind'Val (State_Kind (Node)) /= BSK_No_Kind then
         Write_Space;
         Print_Behavior_State_Kind (State_Kind (Node));
      end if;

      Write_Space;
      Print_Token (T_State);
      Print_Token (T_Semicolon);

      Write_Eol;
   end Print_Behavior_State;

   -------------------------------
   -- Print_Behavior_State_Kind --
   -------------------------------

   procedure Print_Behavior_State_Kind (State_Kind : Byte) is
   begin
      case Behavior_State_Kind'Val (State_Kind) is
         when BSK_Initial =>
            Print_Token (T_Initial);
         when BSK_Initial_Complete =>
            Print_Tokens ((T_Initial, T_Complete));
         when BSK_Initial_Complete_Final =>
            Print_Tokens ((T_Initial, T_Complete, T_Final));
         when BSK_Initial_Final =>
            Print_Tokens ((T_Initial, T_Final));
         when BSK_Complete =>
            Print_Token (T_Complete);
         when BSK_Complete_Final =>
            Print_Tokens ((T_Complete, T_Final));
         when BSK_Final =>
            Print_Token (T_Final);

         when others =>
            Write_Line (Bug_Str);
      end case;
   end Print_Behavior_State_Kind;

   -------------------------------
   -- Print_Behavior_Transition --
   -------------------------------

   procedure Print_Behavior_Transition (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_Transition);

      Transition_Node : constant Node_Id := Transition (Node);
   begin
      if Kind (Transition_Node) = K_Execution_Behavior_Transition then
         Print_Execution_Behavior_Transition (Transition_Node);
      elsif Kind (Transition_Node) = K_Mode_Transition then
         Print_Mode_Transition (Transition_Node);
      end if;
   end Print_Behavior_Transition;

   -----------------------------------------
   -- Print_Execution_Behavior_Transition --
   -----------------------------------------

   procedure Print_Execution_Behavior_Transition (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Execution_Behavior_Transition);

      List_Node : Node_Id;
   begin
      Write_Eol;
      Write_Indentation (+2);

      if Present (Behavior_Transition_Idt (Node)) then
         Print_Identifier (Behavior_Transition_Idt (Node));
         Write_Space;

         if Present (Behavior_Transition_Priority (Node)) then
            Print_Token (T_Left_Square_Bracket);
            Print_Literal (Behavior_Transition_Priority (Node));
            Print_Token (T_Right_Square_Bracket);
            Write_Space;
         end if;

         Print_Token (T_Colon);
         Write_Space;
      end if;

      if not Is_Empty (Sources (Node)) then
         List_Node := First_Node (Sources (Node));

         while Present (List_Node) loop
            Print_Identifier (List_Node);

            if Present (Next_Node (List_Node)) then
               Print_Token (T_Comma);
               Write_Space;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      Write_Space;
      Print_Token (T_Left_Step_Bracket);

      if Present (Behavior_Condition (Node)) then
         Print_Behavior_Condition (Behavior_Condition (Node));
      end if;

      Print_Token (T_Right_Step_Bracket);
      Write_Space;
      Print_Identifier (Destination (Node));

      if not Is_Empty (Behavior_Actions (Node)) then
         Write_Space;
         Print_Token (T_Left_Curly_Bracket);
         Print_Behavior_Actions (Behavior_Actions (Node));
         Write_Eol;
         Write_Indentation (+4);
         Print_Token (T_Right_Curly_Bracket);
      end if;

      Print_Token (T_Semicolon);
   end Print_Execution_Behavior_Transition;

   ---------------------------
   -- Print_Mode_Transition --
   ---------------------------

   procedure Print_Mode_Transition (Node : Node_Id) is
      pragma Unreferenced (Node);

   begin
      Write_Eol;
      Print_Token (T_None);
      Write_Eol;
   end Print_Mode_Transition;

   ------------------------------
   -- Print_Behavior_Condition --
   ------------------------------

   procedure Print_Behavior_Condition (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_Condition);

      Cond_Node : constant Node_Id := Condition (Node);
   --  Execution_condition node is logical_value_expression node
   begin
      case Kind (Cond_Node) is
         when K_Value_Expression =>
            Print_Value_Expression (Cond_Node);

         when K_Dispatch_Condition =>
            Print_Dispatch_Condition (Cond_Node);

         when others =>
            Write_Line (Bug_Str);
      end case;
   end Print_Behavior_Condition;

end Ocarina.BE_AADL_BA.Specifications;
