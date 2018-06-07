------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BACKENDS.LNT.TREE_GENERATOR_THREAD                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2016-2018 ESA & ISAE.                    --
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

with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.Backends;
with Ocarina.Backends.LNT.Nutils;
with Ocarina.Backends.LNT.Nodes;
with Ocarina.Backends.LNT.Components;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;
--  with Ocarina.ME_AADL_BA.BA_Tree.Debug;
with Ocarina.Analyzer.AADL_BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;

with Utils; use Utils;

use Ocarina.Backends.LNT.Components;

use Ocarina.ME_AADL;
use Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.ME_AADL.AADL_Instances.Debug;
use Ocarina.ME_AADL.AADL_Instances.Debug;
with Ada.Text_IO; use Ada.Text_IO;

package body Ocarina.Backends.LNT.Tree_Generator_Thread is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINu renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   use AIN;

   package BLN renames Ocarina.Backends.LNT.Nodes;
   package BLNu renames Ocarina.Backends.LNT.Nutils;
   use BLN;
   use BLNu;

   package BATN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   package BANu renames Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   procedure Visit (E : Node_Id);
   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);
   procedure Visit_Device_Instance (E : Node_Id);
   Module_Node : Node_Id := No_Node;
   Definitions_List : List_Id := No_List;
   Modules_List : List_Id := No_List;
   Predefined_Functions_List : List_Id := No_List;

   function Generate_LNT_Thread (AADL_Tree : Node_Id)
     return Node_Id is
   begin
      Put_Line ("Begin Thread");
      Visit (AADL_Tree);
      return Module_Node;
   end Generate_LNT_Thread;

   -----------
   -- Visit --
   -----------
   procedure Visit (E : Node_Id) is
   begin
      case AIN.Kind (E) is

      when K_Architecture_Instance =>
         Visit_Architecture_Instance (E);

      when K_Component_Instance =>
         Visit_Component_Instance (E);

      when others =>
         null;
      end case;
   end Visit;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------
   procedure Visit_Architecture_Instance (E : Node_Id) is
      N : constant Node_Id := Root_System (E);
   begin
      Module_Node := Make_Module_Definition
       (New_Identifier (Get_String_Name ("_Threads"),
                        Get_Name_String (System_Name)));
      Definitions_List := New_List;
      Modules_List := New_List (Make_Identifier ("Types"));
      Predefined_Functions_List := New_List;
      Visit (N);
      Set_Definitions (Module_Node, Definitions_List);
      Set_Modules (Module_Node, Modules_List);
      Set_Predefined_Functions (Module_Node, Predefined_Functions_List);
   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------
   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category
        := Get_Category_Of_Component (E);
   begin
      case Category is
            when CC_System =>
               Visit_System_Instance (E);
            when CC_Process =>
               Visit_Process_Instance (E);
            when CC_Thread =>
               Visit_Thread_Instance (E);
            when CC_Device =>
               Visit_Device_Instance (E);
            when others =>
               null;
      end case;
   end Visit_Component_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------
   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
      Cs : Node_Id;
   begin
      --  Visit all the subcomponents of the system
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));
         while Present (S) loop
            Cs := Corresponding_Instance (S);
            Visit (Cs);
            S := AIN.Next_Node (S);
         end loop;
      end if;

   end Visit_System_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------
   procedure Visit_Process_Instance (E : Node_Id) is
      S : Node_Id;
      Cs : Node_Id;
      Ns : Name_Id;
   begin
      --  Visit all the subcomponents of the process
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));
         while Present (S) loop

            Cs := Corresponding_Instance (S);
            Ns := AIN.Name (AIN.Identifier (Cs));
            if No (Node_Id ((Get_Name_Table_Info (Ns)))) then
               Set_Name_Table_Info (Ns, Int (Cs));
               Visit (Cs);
            end if;
            S := AIN.Next_Node (S);
         end loop;
      end if;
   end Visit_Process_Instance;

   ----------------------------
   -- Visit_Thread_Instance --
   ----------------------------
   procedure Visit_Thread_Instance (E : Node_Id) is
      S : Node_Id;
      BA : Node_Id;
      N : Node_Id;
      N_Activation : Node_Id;
      N_Port : Node_Id;
      N_Variable_Port : Node_Id;
      Aux_N_Variable_Port : Node_Id;
      Gate : Node_Id;
      Communication : Node_Id;
      Aux_Communication : Node_Id;
      L_Out_Port : List_Id;
      L_In_Port : List_Id;
      L_End : List_Id;
      L_Begin : List_Id;
      L_All : List_Id;
      L_Gates : List_Id;
      L_Statements : List_Id;
      --  When BA
      Has_BA : Boolean := false;
      Si : Node_Id;
      N_Si : Node_Id;
      Vi : Node_Id;
      N_Vi : Node_Id;
      Var_Dec : List_Id;
      Out_Loop : List_Id;
      In_Select : List_Id;
      Variable : Node_Id;
      Ti : Node_Id;
      Source_Transition_List : List_Id;
      Destination_Transition : Node_Id;

      Thread_Identifier : constant Name_Id
        := AIN.Display_Name (AIN.Identifier (E));
   begin
      N_Activation := Make_Identifier ("ACTIVATION");
      LNT_States_List := New_List;
      Var_Dec := New_List;
      Out_Loop := New_List;
      L_Out_Port := New_List;
      L_In_Port := New_List;
      L_Begin := New_List (Make_Communication_Statement
           (N_Activation, New_List (Make_Identifier
            ("T_Dispatch_Preemption"))));
      L_All := New_List (Make_Communication_Statement
           (N_Activation, New_List (Make_Identifier
            ("T_Dispatch_Completion"))));
      L_End := New_List (Make_Communication_Statement
           (N_Activation, New_List (Make_Identifier
            ("T_Preemption_Completion"))));
      L_Gates := New_List (Make_Gate_Declaration
       (Make_Identifier ("LNT_Channel_Dispatch"), N_Activation));

      --  Visit all the subcomponents of the thread
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));
         while Present (S) loop
            Visit (Corresponding_Instance (S));
            S := AIN.Next_Node (S);
         end loop;
      end if;
      --  BA mapping
      if not AINU.Is_Empty (AIN.Annexes (E)) then
         S := AIN.First_Node (AIN.Annexes (E));
         loop
            if (To_Upper (AIN.Display_Name (AIN.Identifier (S))) =
                To_Upper (Get_String_Name ("behavior_specification")))
               and then Present (AIN.Corresponding_Annex (S))
            then
               Has_BA := true;
               BA := AIN.Corresponding_Annex (S);
               if not BANu.Is_Empty (BATN.States (BA)) then
                  Si := BATN.First_Node (BATN.States (BA));
                  loop
                     N_Si := BATN.First_Node (BATN.Identifiers (Si));
                     loop
                        if Analyzer.AADL_BA.Is_Initial (BA, N_Si) then
                           Variable := Make_Var_Declaration (
                            Make_Identifier ("STATE"),
                            Make_Identifier ("LNT_Type_States"));
                           BLNu.Append_Node_To_List (Variable, Var_Dec);
                           BLNu.Append_Node_To_List (Make_Assignment_Statement
                             (Make_Identifier ("STATE"), Make_Identifier (
                             To_Upper (BATN.Display_Name (N_Si)))),
                               Out_Loop);
                        end if;
                        BLNu.Append_Node_To_List (Make_Identifier (
                          To_Upper (BATN.Display_Name (N_Si))),
                         LNT_States_List);
                        N_Si := BATN.Next_Node (N_Si);
                        exit when No (N_Si);
                     end loop;
                     Si := BATN.Next_Node (Si);
                     exit when No (Si);
                  end loop;
               end if;
               if not BANu.Is_Empty (BATN.Variables (BA)) then
                  Vi := BATN.First_Node (BATN.Variables (BA));
                  loop
                     N_Vi := BATN.First_Node (BATN.Identifiers (Vi));
                     loop
                        BLNu.Append_Node_To_List (Make_Identifier (
                          To_Upper (BATN.Display_Name (N_Vi))),
                         LNT_States_List);
                        N_Vi := BATN.Next_Node (N_Vi);
                        exit when No (N_Vi);
                     end loop;
                     Vi := BATN.Next_Node (Vi);
                     exit when No (Vi);
                  end loop;
               end if;
               if not BANu.Is_Empty (BATN.Transitions (BA)) then
                  Ti := BATN.First_Node (BATN.Transitions (BA));
                  loop
                     Ti := BATN.Transition (Ti);
                     Source_Transition_List := BATN.Sources (Ti);
                     Destination_Transition := BATN.Destination (Ti);
                     --  if (STATE == S1) and (not (INPUT))
                     --  then STATE := S1; ... end if;
                     Put_Line (Image (BATN.Display_Name
                     (Destination_Transition)));
                     Put_Line (Image (BATN.Display_Name
                     (BATN.First_Node (Source_Transition_List))));
                     --  --- ----- --- --- ---
                     Ti := BATN.Next_Node (Ti);
                     exit when No (Ti);
                  end loop;
               end if;
            end if;
            S := AIN.Next_Node (S);
            exit when No (S);
         end loop;
      end if;
      --  end BA mapping

      if not AINU.Is_Empty (Features (E)) then
         S := AIN.First_Node (Features (E));
         loop
            if (AIN.Kind (S) = K_Port_Spec_Instance) then
               --  gate identifier
               N_Port := New_Identifier (To_Upper (AIN.Name
                  (AIN.Identifier (S))), "PORT_");
               --  variable identifier
               N_Variable_Port := Make_Identifier (To_Upper (AIN.Name
                      (AIN.Identifier (S))));
               Aux_N_Variable_Port := BLNu.Make_Node_Container
                (N_Variable_Port);
               --  port variable
               Variable := Make_Var_Declaration (N_Variable_Port,
                    Make_Identifier ("LNT_Type_Data"));
               BLNu.Append_Node_To_List (Variable, Var_Dec);

               --  gate declaration
               Gate := Make_Gate_Declaration (
                  Make_Identifier ("LNT_Channel_Port"), N_Port);
               BLNu.Append_Node_To_List (Gate, L_Gates);
               --  gate communication
               if AIN.Is_Out (S) then
                  --  port variable initialization
                  if Has_BA then
                     BLNu.Append_Node_To_List (Make_Assignment_Statement
                     (Aux_N_Variable_Port, Make_Identifier ("NONE")),
                      Out_Loop);
                  else
                     BLNu.Append_Node_To_List (Make_Assignment_Statement
                     (Aux_N_Variable_Port, Make_Identifier ("AADLDATA")),
                      Out_Loop);
                  end if;
                  Communication := Make_Communication_Statement
                   (N_Port, New_List (Make_Offer_Statement
                       (No_Node, N_Variable_Port, false)));
                  Aux_Communication := BLNu.Make_Node_Container
                   (Communication);

                  BLNu.Append_Node_To_List (Communication, L_Out_Port);
                  BLNu.Append_Node_To_List (Aux_Communication, L_End);
               elsif AIN.Is_In (S) then
                  --  port variable initialization
                  if Has_BA then
                     BLNu.Append_Node_To_List (Make_Assignment_Statement
                     (Aux_N_Variable_Port, Make_Identifier ("NONE")),
                      Out_Loop);
                  else
                     BLNu.Append_Node_To_List (Make_Assignment_Statement
                     (Aux_N_Variable_Port, Make_Identifier ("EMPTY")),
                      Out_Loop);
                  end if;
                  Communication := Make_Communication_Statement
                   (N_Port, New_List (Make_Offer_Statement
                       (No_Node, N_Variable_Port, true)));
                  Aux_Communication := BLNu.Make_Node_Container
                   (Communication);

                  BLNu.Append_Node_To_List (Communication, L_In_Port);
                  BLNu.Append_Node_To_List (Aux_Communication, L_Begin);
               end if;
            end if;
            S := AIN.Next_Node (S);
            exit when No (S);
         end loop;
      end if;

      BLNu.Append_List_To_List (L_Out_Port, L_In_Port);

      BLNu.Append_List_To_List (L_In_Port, L_All);

      if (not BLNu.Is_Empty (L_Begin) or else
         not BLNu.Is_Empty (L_End) or else
         not BLNu.Is_Empty (L_All))
      then
         In_Select := New_List (
           Make_Select_Statement_Alternative (New_List (
           Make_Select_Statement (
           New_List (
            Make_Select_Statement_Alternative (L_Begin),
            Make_Select_Statement_Alternative (L_End),
            Make_Select_Statement_Alternative (L_All),
            Make_Select_Statement_Alternative (New_List (
             Make_Communication_Statement
              (N_Activation, New_List (Make_Identifier ("T_Preemption"))))))),
            Make_Communication_Statement
              (N_Activation, New_List (Make_Identifier ("T_Complete"))))),

            Make_Select_Statement_Alternative (New_List (
             Make_Communication_Statement
              (N_Activation, New_List (Make_Identifier ("T_Error"))))),
            Make_Select_Statement_Alternative (New_List (
             Make_Communication_Statement
              (N_Activation, New_List (Make_Identifier ("T_Stop"))))));
         L_Statements := BLNu.New_List (
             Make_Var_Loop_Select (
                 Var_Dec,
                 Out_Loop,
                 In_Select));
      else
         L_Statements := BLNu.New_List (Make_Null_Statement);
      end if;
      N := Make_Process_Definition
       (E, New_Identifier (Thread_Identifier, "Thread_"),
        No_List,
        L_Gates,
        No_List,
        No_List,
        L_Statements
        );
      BLNu.Append_Node_To_List (N, Definitions_List);

   end Visit_Thread_Instance;
   ----------------------------
   -- Visit_Device_Instance --
   ----------------------------
   procedure Visit_Device_Instance (E : Node_Id) is
      S : Node_Id;
      N : Node_Id;

      N_Port : Node_Id;
      Gate : Node_Id;
      Communication : Node_Id;
      L_Communications : List_Id;
      L_Gates : List_Id;
      L_Statements : List_Id;
      N_Loop : Node_Id;
      Device_Identifier : constant Name_Id
        := AIN.Display_Name (AIN.Identifier (E));
   begin

      --  Visit all the subcomponents of the thread
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));
         while Present (S) loop
            Visit (Corresponding_Instance (S));
            S := AIN.Next_Node (S);
         end loop;
      end if;
      L_Gates := New_List;
      L_Communications := New_List;
      if not AINU.Is_Empty (Features (E)) then
         S := AIN.First_Node (Features (E));
         loop
            if (AIN.Kind (S) = K_Port_Spec_Instance) then
               N_Port := New_Identifier (To_Upper (AIN.Name
                  (AIN.Identifier (S))), "PORT_");

               Gate := Make_Gate_Declaration (
                  Make_Identifier ("LNT_Channel_Port"), N_Port);

               Communication := Make_Communication_Statement
                (N_Port, New_List (Make_Identifier ("AADLDATA")));

               BLNu.Append_Node_To_List (Make_Select_Statement_Alternative
                  (New_List (Communication)), L_Communications);
               BLNu.Append_Node_To_List (Gate, L_Gates);

            end if;
            --  Visit (Corresponding_Instance (S));
            S := AIN.Next_Node (S);
            exit when No (S);
         end loop;
      end if;
      if not BLNu.Is_Empty (L_Communications) then
         if (BLNu.Length (L_Communications) = 1) then
            BLNu.Append_Node_To_List (Make_Select_Statement_Alternative
                  (New_List (Make_Null_Statement)), L_Communications);
         end if;
         N_Loop := Make_Loop_Statement (
           New_List (Make_Select_Statement (L_Communications)));
         L_Statements := BLNu.New_List (N_Loop);
      else
         L_Statements := BLNu.New_List (Make_Null_Statement);
      end if;

      N := Make_Process_Definition
       (E, New_Identifier (Device_Identifier, "Device_"),
        No_List,
        L_Gates,
        No_List,
        No_List,
        L_Statements
        );
      BLNu.Append_Node_To_List (N, Definitions_List);

   end Visit_Device_Instance;
end Ocarina.Backends.LNT.Tree_Generator_Thread;
