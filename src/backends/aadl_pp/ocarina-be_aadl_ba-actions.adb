------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B E _ A A D L _ B A . A C T I O N S            --
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

package body Ocarina.BE_AADL_BA.Actions is

   use Ocarina.Output;
   use Ocarina.ME_AADL_BA;
   use Ocarina.ME_AADL_BA.BA_Tree.Nutils;
   use Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   use Ocarina.BE_AADL_BA.Identifiers;
   use Ocarina.BE_AADL_BA.Expressions;

   package BAN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;

   procedure Print_Behavior_Actions          (Node         : Node_Id);
   procedure Print_Behavior_Action           (Node         : Node_Id);
   procedure Print_Conditional_Statement     (Node         : Node_Id);
   procedure Print_If_Cond_Struct            (Node         : Node_Id);
   procedure Print_For_Cond_Struct           (Node         : Node_Id);
   procedure Print_While_Cond_Struct         (Node         : Node_Id);
   procedure Print_Forall_Cond_Struct        (Node         : Node_Id);
   procedure Print_DoUntil_Cond_Struct       (Node         : Node_Id);
   procedure Print_Element_Values            (Node         : Node_Id);
   procedure Print_Assignment_Action         (Node         : Node_Id);
   procedure Print_Communication_Action      (Node         : Node_Id);
   procedure Print_Communication_Kind        (Comm_Kind    : Byte);
   procedure Print_Timed_Action              (Node         : Node_Id);
   procedure Print_Subprogram_Parameter_List (List         : List_Id);
   procedure Print_Parameter_Label           (Node         : Node_Id);

   ---------------------------------
   -- Print_Behavior_Action_Block --
   ---------------------------------

   procedure Print_Behavior_Action_Block (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_Action_Block);

   begin
      if Present (Behav_Acts (Node)) then
         Write_Space;
         Print_Token (T_Left_Curly_Bracket);
         Print_Behavior_Actions (Behav_Acts (Node));
         Write_Eol;
         Write_Indentation (+4);
         Print_Token (T_Right_Curly_Bracket);
      end if;

      if Present (Behavior_Time (Node)) then
         Write_Space;
         Print_Token (T_Timeout);
         Write_Space;
         Print_Behavior_Time (Behavior_Time (Node));
      end if;
   end Print_Behavior_Action_Block;

   ----------------------------
   -- Print_Behavior_Actions --
   ----------------------------

   procedure Print_Behavior_Actions (Node : Node_Id) is

      List_Node1 : Node_Id;
      List_Node2 : Node_Id;
   begin
      Write_Eol;
      Write_Indentation (+4);

      if not Is_Empty (Behavior_Action_Sequence (Node)) then
         List_Node1 := First_Node (Behavior_Action_Sequence (Node));
         Print_Behavior_Action (List_Node1);

         List_Node1 := Next_Node (List_Node1);
         while Present (List_Node1) loop
            Print_Token (T_Semicolon);
            Write_Eol;
            Write_Indentation (+4);

            Print_Behavior_Action (List_Node1);

            List_Node1 := Next_Node (List_Node1);
         end loop;
      end if;

      if not Is_Empty (Behavior_Action_Set (Node)) then
         List_Node2 := First_Node (Behavior_Action_Set (Node));
         Print_Behavior_Action (List_Node2);

         List_Node2 := Next_Node (List_Node2);
         while Present (List_Node2) loop
            Print_Token (T_Concat);
            Write_Eol;
            Write_Indentation (+4);

            Print_Behavior_Action (List_Node2);

            List_Node2 := Next_Node (List_Node2);
         end loop;
      end if;

      if Present (Behavior_Action (Node)) and then
         Is_Empty (Behavior_Action_Sequence (Node)) and then
         Is_Empty (Behavior_Action_Set (Node))
      then
         Print_Behavior_Action (Behavior_Action (Node));
      end if;
   end Print_Behavior_Actions;

   ---------------------------
   -- Print_Behavior_Action --
   ---------------------------

   procedure Print_Behavior_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Behavior_Action);
      pragma Assert (Kind (Action (Node)) = K_If_Cond_Struct
                       or else Kind (Action (Node)) = K_For_Cond_Structure
                       or else Kind (Action (Node)) = K_While_Cond_Structure
                       or else Kind (Action (Node)) = K_ForAll_Cond_Structure
                       or else Kind (Action (Node)) = K_DoUntil_Cond_Structure
                       or else Kind (Action (Node)) = K_Assignment_Action
                       or else Kind (Action (Node)) = K_Communication_Action
                       or else Kind (Action (Node)) = K_Timed_Act);

      Action_Node : constant Node_Id := Action (Node);
   begin
      case Kind (Action_Node) is
         when K_If_Cond_Struct         => Print_If_Cond_Struct   (Action_Node);
         when K_For_Cond_Structure     => Print_For_Cond_Struct  (Action_Node);
         when K_While_Cond_Structure   => Print_While_Cond_Struct
                                                                 (Action_Node);
         when K_ForAll_Cond_Structure  => Print_Forall_Cond_Struct
                                                                 (Action_Node);
         when K_DoUntil_Cond_Structure => Print_DoUntil_Cond_Struct
                                                                 (Action_Node);
         when K_Assignment_Action      => Print_Assignment_Action
                                                                 (Action_Node);
         when K_Communication_Action   => Print_Communication_Action
                                                                 (Action_Node);
         when K_Timed_Act           => Print_Timed_Action     (Action_Node);

         when others                   => Write_Line (Bug_Str);
      end case;
   end Print_Behavior_Action;

   ---------------------------------
   -- Print_Conditional_Statement --
   ---------------------------------

   procedure Print_Conditional_Statement
     (Node     : Node_Id)
   is
      pragma Assert (Kind (Node) = K_Conditional_Statement);

   begin
      if Present (Logical_Expr (Node)) then
         Print_Token (T_Left_Parenthesis);
         Print_Value_Expression (Logical_Expr (Node));
         Print_Token (T_Right_Parenthesis);
      end if;

      Print_Behavior_Actions (Behav_Acts (Node));
   end Print_Conditional_Statement;

   --------------------------
   -- Print_If_Cond_Struct --
   --------------------------

   procedure Print_If_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_If_Cond_Struct);
      List_Node : Node_Id;
   begin
      Print_Token (T_If);
      Write_Space;
      Print_Conditional_Statement (If_Statement (Node));

      if not Is_Empty (Elsif_Statement (Node)) then
         List_Node := First_Node (Elsif_Statement (Node));
         while Present (List_Node) loop
            Write_Eol;
            Write_Indentation (+4);
            Print_Token (T_Elsif);
            Write_Space;
            Print_Conditional_Statement (List_Node);
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      if Present (Else_Statement (Node)) then
         Write_Eol;
         Write_Indentation (+4);
         Print_Token (T_Else);
         Write_Eol;
         Print_Conditional_Statement (Else_Statement (Node));
      end if;
      Write_Eol;
      Write_Indentation (+4);
      Print_Tokens ((T_End, T_if));
   end Print_If_Cond_Struct;

   --------------------------
   -- Print_For_Cond_Struct --
   --------------------------

   procedure Print_For_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_For_Cond_Structure);

   begin
      Write_Space;
      Print_Token (T_For);
      Write_Space;

      Print_Token (T_Left_Parenthesis);
      Print_Identifier (Element_Idt (Node));
      Write_Space;
      Print_Token (T_Colon);
      Write_Space;
      Print_Component_Classifier_Ref (Classifier_Ref (Node));
      Write_Space;
      Print_Token (T_In);
      Write_Space;
      Print_Element_Values (In_Element_Values (Node));
      Print_Token (T_Right_Parenthesis);

      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Left_Curly_Bracket);
      Print_Behavior_Actions (Behav_Acts (Node));
      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Right_Curly_Bracket);
   end Print_For_Cond_Struct;

   ------------------------------
   -- Print_Forall_Cond_Struct --
   ------------------------------

   procedure Print_Forall_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_ForAll_Cond_Structure);

   begin
      Write_Space;
      Print_Token (T_Forall);
      Write_Space;

      Print_Token (T_Left_Parenthesis);
      Print_Identifier (Element_Idt (Node));
      Write_Space;
      Print_Token (T_Colon);
      Write_Space;
      Print_Component_Classifier_Ref (Classifier_Ref (Node));
      Write_Space;
      Print_Token (T_In);
      Write_Space;
      Print_Element_Values (In_Element_Values (Node));
      Print_Token (T_Right_Parenthesis);

      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Left_Curly_Bracket);
      Print_Behavior_Actions (Behav_Acts (Node));
      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Right_Curly_Bracket);
   end Print_Forall_Cond_Struct;

   -----------------------------
   -- Print_While_Cond_Struct --
   -----------------------------

   procedure Print_While_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_While_Cond_Structure);

   begin
      Print_Token (T_While);
      Write_Space;
      Print_Token (T_Left_Parenthesis);
      Print_Value_Expression (Logical_Expr (Node));
      Print_Token (T_Right_Parenthesis);

      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Left_Curly_Bracket);
      Print_Behavior_Actions (Behav_Acts (Node));
      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Right_Curly_Bracket);
   end Print_While_Cond_Struct;

   -------------------------------
   -- Print_DoUntil_Cond_Struct --
   -------------------------------

   procedure Print_DoUntil_Cond_Struct (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_DoUntil_Cond_Structure);

   begin
      Print_Token (T_Do);

      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Left_Curly_Bracket);
      Print_Behavior_Actions (Behav_Acts (Node));
      Write_Eol;
      Write_Indentation (+4);
      Print_Token (T_Right_Curly_Bracket);
      Write_Space;
      Print_Token (T_Until);
      Write_Space;
      Print_Token (T_Left_Parenthesis);
      Print_Value_Expression (Logical_Expr (Node));
      Print_Token (T_Right_Parenthesis);
   end Print_DoUntil_Cond_Struct;

   --------------------------
   -- Print_Element_Values --
   --------------------------

   procedure Print_Element_Values (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Integer_Range
                       or else Kind (Node) = K_Data_Component_Reference);
   begin
      case Kind (Node) is
         when K_Integer_Range =>
            Print_Integer_Range (Node);

         when K_Data_Component_Reference =>
            Print_Data_Component_Reference (Node);

         when others =>
            Write_Line (Bug_Str);
      end case;
   end Print_Element_Values;

   -----------------------------
   -- Print_Assignment_Action --
   -----------------------------

   procedure Print_Assignment_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Assignment_Action);

   begin
      if Kind (Target (Node)) = K_Name then
         Print_Name (Target (Node));
      else
         Print_Data_Component_Reference (Target (Node));
      end if;
      Write_Space;
      Print_Token (T_Assignment);
      Write_Space;

      if Present (Value_Expression (Node)) then
         Print_Value_Expression (Value_Expression (Node));
      end if;

      if Is_Any (Node) then
         Print_Token (T_Any);
      end if;
   end Print_Assignment_Action;

   --------------------------------
   -- Print_Communication_Action --
   --------------------------------

   procedure Print_Communication_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Communication_Action);

   begin
      Write_Space;
      if Kind (BAN.Identifier (Node)) = K_Name then
         Print_Name (BAN.Identifier (Node));
      else
         Print_Data_Component_Reference (BAN.Identifier (Node));
      end if;

      Print_Communication_Kind (Comm_Kind (Node));

      if not Is_Empty (Subprogram_Parameter_List (Node)) then
         Print_Token (T_Left_Parenthesis);
         Print_Subprogram_Parameter_List (Subprogram_Parameter_List (Node));
         Print_Token (T_Right_Parenthesis);
      end if;

      if Present (Target (Node)) then
         Write_Space;
         Print_Token (T_Left_Parenthesis);
         if Kind (Target (Node)) = K_Name then
            Print_Name (Target (Node));
         else
            Print_Data_Component_Reference (Target (Node));
         end if;
         Print_Token (T_Right_Parenthesis);
      end if;
   end Print_Communication_Action;

   ------------------------------
   -- Print_Communication_Kind --
   ------------------------------

   procedure Print_Communication_Kind (Comm_Kind : Byte) is
   begin
      case Communication_Kind'Val (Comm_Kind) is
         when CK_Exclamation      => Print_Token (T_Exclamation);
         when CK_Interrogative    => Print_Token (T_Interrogative);
         when CK_Greater_Greater  => Print_Token (T_Greater_Greater_Than);
         when CK_Exclamation_Greater  => Print_Token (T_Exclamation_Greater);
         when CK_Exclamation_Lesser  => Print_Token (T_Exclamation_Lesser);
         when others              => Write_Line  (Bug_Str);
      end case;
   end Print_Communication_Kind;

   ------------------------
   -- Print_Timed_Action --
   ------------------------

   procedure Print_Timed_Action (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Timed_Act);
      List_Node : Node_Id;
   begin
      Write_Space;
      Print_Token (T_Computation);

      Write_Space;
      Print_Token (T_Left_Parenthesis);

      Print_Behavior_Time (Fst_Behavior_Time (Node));

      if Present (Scd_Behavior_Time (Node)) then
         Write_Space;
         Print_Token (T_Interval);
         Write_Space;
         Print_Behavior_Time (Scd_Behavior_Time (Node));
      end if;
      Write_Space;
      Print_Token (T_Right_Parenthesis);
      Write_Space;

      if not Is_Empty (Processor_Idt (Node)) then
         Print_Tokens ((T_In, T_Binding));
         Write_Space;
         Print_Token (T_Left_Parenthesis);
         List_Node := First_Node (Processor_Idt (Node));
         Print_Identifier (List_Node);

         List_Node := Next_Node (List_Node);
         while Present (List_Node) loop
            Print_Token (T_Comma);
            Write_Space;
            Print_Identifier (List_Node);

            List_Node := Next_Node (List_Node);
         end loop;
         Write_Space;
         Print_Token (T_Right_Parenthesis);
      end if;

   end Print_Timed_Action;

   ------------------------------------
   -- Print_Suprogram_Parameter_List --
   ------------------------------------

   procedure Print_Subprogram_Parameter_List (List : List_Id) is
      pragma Assert (not Is_Empty (List));

      List_Node : Node_Id;
   begin
      List_Node := First_Node (List);
      Print_Parameter_Label (List_Node);

      List_Node := Next_Node (List_Node);
      while Present (List_Node) loop
         Print_Token (T_Comma);
         Write_Space;
         Print_Parameter_Label (List_Node);

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Subprogram_Parameter_List;

   ---------------------------
   -- Print_Parameter_Label --
   ---------------------------

   procedure Print_Parameter_Label (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Parameter_Label);

      Param_Node : constant Node_Id := Parameter (Node);
   begin
      case Kind (Param_Node) is
         when K_Value_Expression       => Print_Value_Expression (Param_Node);
         when K_Name                     => Print_Name (Param_Node);
         when K_Data_Component_Reference => Print_Data_Component_Reference
                                                           (Param_Node);
         when others                     => Write_Line  (Bug_Str);
      end case;
   end Print_Parameter_Label;

   ------------------------------------
   -- Print_Data_Component_Reference --
   ------------------------------------

   procedure Print_Data_Component_Reference (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Data_Component_Reference);
      pragma Assert (not Is_Empty (BAN.Identifiers (Node)));

      List_Node : Node_Id;
   begin
      List_Node := First_Node (BAN.Identifiers (Node));
      Print_Name (List_Node);

      List_Node := Next_Node (List_Node);
      while Present (List_Node) loop
         Print_Token (T_Dot);
         Print_Name (List_Node);

         List_Node := Next_Node (List_Node);
      end loop;
   end Print_Data_Component_Reference;

   ----------------
   -- Print_Name --
   ----------------

   procedure Print_Name (Node : Node_Id) is
      pragma Assert (Kind (Node) = K_Name);
      List_Node1 : Node_Id;
      List_Node2 : Node_Id;
   begin
      List_Node1 := First_Node (Idt (Node));
      Print_Identifier (List_Node1);

      List_Node1 := Next_Node (List_Node1);
      while Present (List_Node1) loop
         Print_Token (T_Dot);
         Write_Space;
         Print_Identifier (List_Node1);

         List_Node1 := Next_Node (List_Node1);
      end loop;
      Write_Space;
      if not Is_Empty (Array_Index (Node)) then
         List_Node2 := First_Node (Array_Index (Node));
         Print_Token (T_Left_Square_Bracket);
         Write_Space;
         Print_Integer_Value (List_Node2);
         Write_Space;
         Print_Token (T_Right_Square_Bracket);

         List_Node2 := Next_Node (List_Node2);
         while Present (List_Node2) loop
            Print_Token (T_Left_Square_Bracket);
            Write_Space;
            Print_Integer_Value (List_Node2);
            Write_Space;
            Print_Token (T_Right_Square_Bracket);

            List_Node2 := Next_Node (List_Node2);
         end loop;
      end if;

   end Print_Name;

end Ocarina.BE_AADL_BA.Actions;
