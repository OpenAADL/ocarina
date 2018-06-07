------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.BACKENDS.LNT.TREE_GENERATOR_PROCESSOR               --
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
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Utils; use Utils;

use Ocarina.Backends.LNT.Components;
use Ocarina.Backends.Properties;
use Ocarina.Backends.Messages;

use Ocarina.ME_AADL;
use Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Debug;
use Ocarina.ME_AADL.AADL_Instances.Debug;

with Ada.Text_IO; use Ada.Text_IO;

package body Ocarina.Backends.LNT.Tree_Generator_Processor is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINu renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   use AIN;

   package BLN renames Ocarina.Backends.LNT.Nodes;
   package BLNu renames Ocarina.Backends.LNT.Nutils;
   use BLN;
   use BLNu;

   procedure Visit (E : Node_Id;
                  T : in out Thread_Array; P : in out Period_Array);
   procedure Visit_Architecture_Instance (E : Node_Id;
                  T : in out Thread_Array; P : in out Period_Array);
   procedure Visit_Component_Instance (E : Node_Id;
                  T : in out Thread_Array; P : in out Period_Array);
   procedure Visit_System_Instance (E : Node_Id;
                  T : in out Thread_Array; P : in out Period_Array);
   procedure Visit_Process_Instance (E : Node_Id;
                  T : in out Thread_Array; P : in out Period_Array);
   procedure Visit_Thread_Instance (E : Node_Id;
                  T : in out Thread_Array; P : in out Period_Array);

   procedure Make_LNT_RM_Processor (T : Thread_Array);
   procedure Make_LNT_RM_Processor_Functions;
   procedure Quick_Sort_Threads (Threads : in out Thread_Array);
   function Connections_Number (Port : Node_Id) return Natural;
   function Threads_Hyperperiod (T : Period_Array)
    return Natural;
   function Make_Time_Const (A : Natural) return Node_Id;
   procedure Make_Notif (Event_Port_Number : Natural;
     I : Natural; Notif_Function : String;
      L_Incoming_Event : List_Id);
   function Make_Time_Const (A : Natural) return Node_Id is
   begin
      return Make_Constructed_Pattern
            (Make_Identifier ("LNT_Type_Time_Constraint"),
             New_List (Make_Nat (A)));
   end Make_Time_Const;

   Module_Node : Node_Id := No_Node;
   Definitions_List : List_Id := No_List;
   Modules_List : List_Id := No_List;
   Predefined_Functions_List : List_Id := No_List;
   Counter : Natural := 1;

   ----------------------------
   -- Generate_LNT_Processor --
   ----------------------------
   function Generate_LNT_Processor (AADL_Tree : Node_Id)
     return Node_Id
   is

      Threads : Thread_Array (1 .. Thread_Number);
      Periods : Period_Array (1 .. Thread_Number);
   begin
      Put_Line ("Begin Processor");
      Visit (AADL_Tree, Threads, Periods);
      return Module_Node;
   end Generate_LNT_Processor;

   -----------
   -- Visit --
   -----------
   procedure Visit (E : Node_Id;
                    T : in out Thread_Array;
                    P : in out Period_Array) is
   begin
      case AIN.Kind (E) is

      when K_Architecture_Instance =>
         Visit_Architecture_Instance (E, T, P);

      when K_Component_Instance =>
         Visit_Component_Instance (E, T, P);

      when others =>
         null;
      end case;
   end Visit;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------
   procedure Visit_Architecture_Instance (E : Node_Id;
                                          T : in out Thread_Array;
                                          P : in out Period_Array) is
      N : constant Node_Id := Root_System (E);
   begin
      Module_Node := Make_Module_Definition
       (New_Identifier (Get_String_Name ("_Processor"),
                        Get_Name_String (System_Name)));
      Definitions_List := New_List;
      Make_LNT_RM_Processor_Functions;
      Modules_List := New_List (Make_Identifier ("Types"));
      Predefined_Functions_List := New_List;
      Visit (N, T, P);
      --  Hyperperiod calculation
      Hyperperiod := Threads_Hyperperiod (P);
      Make_LNT_RM_Processor (T);
      Set_Definitions (Module_Node, Definitions_List);
      Set_Modules (Module_Node, Modules_List);
      Set_Predefined_Functions (Module_Node, Predefined_Functions_List);
   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------
   procedure Visit_Component_Instance (E : Node_Id;
                                       T : in out Thread_Array;
                                       P : in out Period_Array) is
      Category : constant Component_Category
        := Get_Category_Of_Component (E);
   begin
      case Category is
            when CC_System =>
               Visit_System_Instance (E, T, P);
            when CC_Process =>
               Visit_Process_Instance (E, T, P);
            when CC_Thread =>
               Visit_Thread_Instance (E, T, P);
            when others =>
               null;
      end case;
   end Visit_Component_Instance;
   ---------------------------
   -- Visit_System_Instance --
   ---------------------------
   procedure Visit_System_Instance (E : Node_Id;
                                    T : in out Thread_Array;
                                    P : in out Period_Array) is
      S : Node_Id;
   begin
      --  Visit all the subcomponents of the system
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));
         while Present (S) loop
            Visit (Corresponding_Instance (S), T, P);
            S := AIN.Next_Node (S);
         end loop;
      end if;
   end Visit_System_Instance;
   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------
   procedure Visit_Process_Instance (E : Node_Id;
                                     T : in out Thread_Array;
                                     P : in out Period_Array) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));
         while Present (S) loop
            Visit (Corresponding_Instance (S), T, P);
            S := AIN.Next_Node (S);
         end loop;
      end if;
   end Visit_Process_Instance;

   ----------------------------
   -- Visit_Thread_Instance --
   ----------------------------
   procedure Visit_Thread_Instance (E : Node_Id;
                                    T : in out Thread_Array;
                                    P : in out Period_Array)
   is
      S : Node_Id;
      Th : Thread;
      Period : Natural;
      Event_Port_Number : Natural := 0;
      Thread_Identifier : constant Name_Id
        := AIN.Display_Name (AIN.Identifier (E));
      Dispatch : constant Supported_Thread_Dispatch_Protocol :=
        Get_Thread_Dispatch_Protocol (E);
   begin
      if not AINU.Is_Empty (Features (E)) then
         S := AIN.First_Node (Features (E));
         loop
            if (AIN.Kind (S) = K_Port_Spec_Instance) then
               if Connections_Number (S) > 1 then
                  Display_Located_Error (AIN.Loc (S),
                   "LNT generation requires 1 to 1 connections",
                Fatal => True);
               end if;
               if (AIN.Is_In (S))  and then
                  (AIN.Is_Event (S))
               then
                  Event_Port_Number := Event_Port_Number + 1;
               end if;
            end if;
            S := AIN.Next_Node (S);
            exit when No (S);
         end loop;
      end if;
      Th.Identifier := New_Identifier (Thread_Identifier, "Thread_");
      if (((Dispatch = Thread_Periodic) or else
           (Dispatch = Thread_Sporadic) or else
           (Dispatch = Thread_Timed) or else
           (Dispatch = Thread_Hybrid)) and then
          (Get_Execution_Time (E) /= Empty_Time_Array))
      then
         Period := Natural (Get_Thread_Period (E).T);
         Th.Period := Period;
         Th.Capacity := Natural (Get_Execution_Time (E)(1).T);
         Th.Dispatch_Protocol := Dispatch;
         Th.Event_Port_Number := Event_Port_Number;
         T (Counter) := Th;
         P (Counter) := Period;
         Counter := Counter + 1;
      else
         Display_Located_Error (AIN.Loc (E),
                "LNT generation requires the definition " &
                "of Dispatch_Protocol (Periodic, Sporadic," &
                " Timed or Hybrid), " &
                "Execution_Time and Period properties.",
                Fatal => True);
      end if;
   end Visit_Thread_Instance;

   ----------------------------
   --  Make_LNT_RM_Processor --
   ----------------------------
   procedure Make_LNT_RM_Processor (T : Thread_Array) is
      N : Node_Id;
      N_Sort : Node_Id;
      L_While : List_Id;
      L_Else_Running_State : List_Id;
      N_Act : Node_Id;
      Aux_Act_1 : Node_Id;
      Aux_Act_2 : Node_Id;

      Is_Not_Periodic : Boolean := false;
      Threads : Thread_Array := T;
      L_Processor_Gates   : List_Id;
      L_Thread_Activation : List_Id;
      L_Threads_Array     : List_Id;
      L_Stop_Orders       : List_Id;
      L_Incoming_Event    : List_Id;
      N_Ready_State      : Node_Id;
      Thread_Activation : Node_Id;
      Aux_Thread_Activation : Node_Id;
      L_In_Then           : List_Id;
      function Make_Var_declaration_List return List_Id;
      function Make_Var_declaration_List
        return List_Id is
         N_Var_Declarations : List_Id;
      begin
         N_Var_Declarations := New_List;

         BLNu.Append_Node_To_List (Make_Var_Declaration
         (Make_Identifier ("Threads"),
          Make_Identifier ("LNT_Type_Thread_Array")), N_Var_Declarations);

         BLNu.Append_Node_To_List (Make_Var_Declaration
         (Make_Identifier ("Counter"),
          Make_Identifier ("LNT_Type_Time_Constraint")),
          N_Var_Declarations);

         BLNu.Append_Node_To_List (Make_Var_Declaration
         (Make_Identifier ("I"),
          Make_Identifier ("Nat")), N_Var_Declarations);

         BLNu.Append_Node_To_List (Make_Var_Declaration
         (Make_Identifier ("k"),
          Make_Identifier ("Nat")), N_Var_Declarations);

         if (Not_Periodic_Thread_Number > 0) then
            BLNu.Append_Node_To_List (Make_Var_Declaration
             (Make_Identifier ("Is_Activated"),
              Make_Identifier ("bool")), N_Var_Declarations);
         end if;

         BLNu.Append_Node_To_List (Make_Var_Declaration
         (Make_Identifier ("Preempt"),
          Make_Identifier ("bool")), N_Var_Declarations);

         BLNu.Append_Node_To_List (Make_Var_Declaration
         (Make_Identifier ("TODO"),
          Make_Identifier ("LNT_Type_Time_Constraint")),
          N_Var_Declarations);

         BLNu.Append_Node_To_List (Make_Var_Declaration
         (Make_Identifier ("Nb_Active_Thread"),
          Make_Identifier ("Nat")), N_Var_Declarations);

         return N_Var_Declarations;
      end Make_Var_declaration_List;

   begin

      Quick_Sort_Threads (Threads);
      L_Processor_Gates := New_List;
      L_Thread_Activation := New_List;
      L_Stop_Orders  := New_List;
      L_Incoming_Event  := New_List;

      L_Threads_Array := New_List (
          Make_Assignment_Statement (
            Make_Identifier ("Counter"),
            Make_Time_Const (0)),

          Make_Assignment_Statement (
            Make_Identifier ("Threads"),
            Make_Constructed_Pattern (
              Make_Identifier ("LNT_Type_Thread_Array"),
              New_List (
                Make_Constructed_Pattern (
                  Make_Identifier ("LNT_Type_Thread"),
                  New_List (Make_Time_Const (0)))))));

      for I in Threads'Range loop
         --  ACTIVATION_I identifier
         N_Act := New_Identifier (
           Remove_Prefix_From_Name (
           " ", Get_String_Name (Integer'Image (I))),
           "ACTIVATION_");

         Aux_Act_1 := BLNu.Make_Node_Container (N_Act);
         Aux_Act_2 := BLNu.Make_Node_Container (N_Act);

         BLNu.Append_Node_To_List (
             Make_Gate_Declaration (
               Make_Identifier ("LNT_Channel_Dispatch"),
               N_Act),
             L_Processor_Gates);

         BLNu.Append_Node_To_List (Aux_Act_2,
             L_Thread_Activation);

         BLNu.Append_Node_To_List (
           Make_Communication_Statement
             (Aux_Act_1, New_List (Make_Identifier ("T_Stop")),
             false, No_Node),
             L_Stop_Orders);

--  ---------------------------------------------
--  Thread ([0][1][2][3][4][5][6][7][8][9][10][11])
--  Thread ([C][P][0][1][0][P][0][0][0][1][1 ][0 ]) P
--  Thread ([C][P][0][0][0][P][0][0][0][1][0 ][1 ]) S
--  Thread ([C][P][0][0][0][P][0][0][0][1][1 ][2 ]) T
--  Thread ([C][P][0][0][0][P][0][0][0][1][1 ][3 ]) H
--  ---------------------------------------------

         case Threads (I).Dispatch_Protocol is
            when Thread_Periodic =>
               Is_Not_Periodic := false;
               BLNu.Append_Node_To_List (
                 Make_Array_Element_Assignment_Statement
                 (Make_Identifier ("Threads"),
                  Make_Identifier (Integer'Image (I)),
                  Make_Constructed_Pattern
                   (Make_Identifier ("LNT_Type_Thread"),
                    New_List (
                           Make_Time_Const (Threads (I).Capacity),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (1),
                           Make_Time_Const (0),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (1),
                           Make_Time_Const (1),
                           Make_Time_Const (0)))),
                 L_Threads_Array);
            when Thread_Sporadic =>
               Is_Not_Periodic := true;
               BLNu.Append_Node_To_List (
                Make_Array_Element_Assignment_Statement
                 (Make_Identifier ("Threads"),
                  Make_Identifier (Integer'Image (I)),
                  Make_Constructed_Pattern
                   (Make_Identifier ("LNT_Type_Thread"),
                    New_List (
                           Make_Time_Const (Threads (I).Capacity),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (1),
                           Make_Time_Const (0),
                           Make_Time_Const (1)))),
                L_Threads_Array);

               Make_Notif (Threads (I).Event_Port_Number,
                 I, "Sporadic_Notif", L_Incoming_Event);
            when Thread_Timed =>
               Is_Not_Periodic := true;
               BLNu.Append_Node_To_List (
                Make_Array_Element_Assignment_Statement
                 (Make_Identifier ("Threads"),
                  Make_Identifier (Integer'Image (I)),
                  Make_Constructed_Pattern
                   (Make_Identifier ("LNT_Type_Thread"),
                    New_List (
                           Make_Time_Const (Threads (I).Capacity),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (1),
                           Make_Time_Const (1),
                           Make_Time_Const (2)))),
                L_Threads_Array);
               Make_Notif (Threads (I).Event_Port_Number,
                 I, "Timed_Hybrid_Notif", L_Incoming_Event);
            when Thread_Hybrid =>
               Is_Not_Periodic := true;
               BLNu.Append_Node_To_List (
                Make_Array_Element_Assignment_Statement
                 (Make_Identifier ("Threads"),
                  Make_Identifier (Integer'Image (I)),
                  Make_Constructed_Pattern
                   (Make_Identifier ("LNT_Type_Thread"),
                    New_List (
                           Make_Time_Const (Threads (I).Capacity),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (1),
                           Make_Time_Const (0),
                           Make_Time_Const (Threads (I).Period),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (0),
                           Make_Time_Const (1),
                           Make_Time_Const (1),
                           Make_Time_Const (3)))),
                L_Threads_Array);
               Make_Notif (Threads (I).Event_Port_Number,
                 I, "Timed_Hybrid_Notif", L_Incoming_Event);
            when others =>
               null;
         end case;

         --  instances list for Main generation
         BLNu.Append_Node_To_List (
            Make_Process_Instantiation_Statement (
                Make_Identifier (Threads (I).Identifier),
                No_List, No_List, Is_Not_Periodic),
                LNT_Thread_Instance_List);

      end loop;
      Thread_Activation := Make_Process_Instantiation_Statement
             (Make_Identifier ("Activate_k"),
              L_Thread_Activation,
              New_List (
               Make_Actual_Parameter
                (Make_Identifier ("Threads"), false, true),
               Make_Actual_Parameter
                (Make_Identifier ("k"))));
      Aux_Thread_Activation := BLNu.Make_Node_Container (Thread_Activation);

      L_Else_Running_State :=  New_List (
          Make_Assignment_Statement (
            Make_Identifier ("Preempt"),
            Make_Identifier ("true")),
----------------------------  assi TODO := Threads [K][0] - Threads [K][2];
          Make_Assignment_Statement (
            Make_Identifier ("TODO"),
            Make_Infix_Function_Call_Expression (
              Make_Identifier ("-"),
              Make_Array_Elt_Access_Expression (
                Make_Identifier ("0"),
                Make_Array_Elt_Access_Expression (
                  Make_Identifier ("K"),
                  Make_Identifier ("Threads"))),
              Make_Array_Elt_Access_Expression (
                Make_Nat (2),
                Make_Array_Elt_Access_Expression (
                  Make_Identifier ("K"),
                  Make_Identifier ("Threads"))))),
---------------------------  assi I := 1;
          Make_Assignment_Statement (
            Make_Identifier ("I"),
            Make_Nat (1)),
----------------------------  while
          Make_While_Statement (
            Make_Parenthesized_Expression (
            Make_Infix_Function_Call_Expression (
              Make_Predefined_Function (K_And),
              Make_Parenthesized_Expression (
              Make_Infix_Function_Call_Expression (
                Make_Predefined_Function (K_Less_Than_Or_Equal_To, false),
                Make_Identifier ("I"),
                Make_Parenthesized_Expression (
                  Make_Infix_Function_Call_Expression (
                    Make_Identifier ("-"),
                    Make_Identifier ("K"),
                    Make_Nat (1))))),
              Make_Identifier ("Preempt"))),

            New_List (
              Make_If_Statement (
               Make_Infix_Function_Call_Expression (
                Make_Predefined_Function (K_And),
                Make_Parenthesized_Expression (
                Make_Infix_Function_Call_Expression (
                  Make_Predefined_Function (K_Equality, false),
                    Make_Array_Elt_Access_Expression (
                      Make_Nat (10),
                      Make_Array_Elt_Access_Expression (
                      Make_Identifier ("I"),
                      Make_Identifier ("Threads"))),
                    Make_Time_Const (1))),

                Make_Infix_Function_Call_Expression (
                 Make_Predefined_Function (K_And),
                  Make_Parenthesized_Expression (
                  Make_Infix_Function_Call_Expression (
                    Make_Predefined_Function (K_Equality, false),
                      Make_Array_Elt_Access_Expression (
                        Make_Nat (9),
                        Make_Array_Elt_Access_Expression (
                        Make_Identifier ("I"),
                        Make_Identifier ("Threads"))),
                       Make_Time_Const (1))),
                 Make_Parenthesized_Expression (
                 Make_Infix_Function_Call_Expression (
                  Make_Predefined_Function (K_Greater_Than, false),
                  Make_Infix_Function_Call_Expression (
                    Make_Identifier ("+"),
                    Make_Identifier ("Counter"),
                    Make_Identifier ("TODO")),
                  Make_Array_Elt_Access_Expression (
                      Make_Nat (4),
                      Make_Array_Elt_Access_Expression (
                        Make_Identifier ("I"),
                        Make_Identifier ("Threads")))))
                     )),
                New_List (
                  Make_Assignment_Statement (
                    Make_Identifier ("TODO"),
                    Make_Infix_Function_Call_Expression (
                      Make_Identifier ("-"),
                      Make_Array_Elt_Access_Expression (
                        Make_Nat (4),
                        Make_Array_Elt_Access_Expression (
                          Make_Identifier ("I"),
                          Make_Identifier ("Threads"))),
                        Make_Identifier ("Counter"))),
                  Make_Assignment_Statement (
                    Make_Identifier ("Preempt"),
                    Make_Identifier ("false"))),
                No_List,
                New_List (Make_Assignment_Statement (
                  Make_Identifier ("I"),
                    Make_Infix_Function_Call_Expression (
                      Make_Identifier ("+"),
                      Make_Identifier ("I"),
                      Make_Nat (1))))
                 ))),
-----------------------------  assi Counter := Counter + TODO;
          Make_Assignment_Statement (
            Make_Identifier ("Counter"),
            Make_Infix_Function_Call_Expression (
              Make_Identifier ("+"),
              Make_Identifier ("Counter"),
              Make_Identifier ("TODO"))),
----------------------------  assi Nb_Active_Thread := Nb_Active_Thread + 1;
          Make_Assignment_Statement (
            Make_Identifier ("Nb_Active_Thread"),
            Make_Infix_Function_Call_Expression (
              Make_Identifier ("+"),
              Make_Identifier ("Nb_Active_Thread"),
              Make_Nat (1))),
----------------------------
          Make_Assignment_Statement (
               Make_Identifier ("Threads"),
               Make_Function_Call_Expression (
                Make_Identifier ("Update_Thread"),
                New_List (
                  Make_Identifier ("Threads"),
                  Make_Identifier ("K"),
                  Make_Identifier ("TODO")))));

      BLNu.Append_Node_To_List (Aux_Thread_Activation, L_Else_Running_State);
      BLNu.Append_Node_To_List (Make_If_Statement
                    (Make_Identifier ("Preempt"),
                     New_List (Make_Assignment_Statement
                    (Make_Identifier ("k"),
                     Make_Identifier ("I"))),
                     No_List,
                     New_List (Make_Assignment_Statement
                    (Make_Identifier ("k"),
                      Make_Infix_Function_Call_Expression
                          (Make_Identifier ("+"),
                           Make_Identifier ("k"),
                           Make_Nat (1))))),
            L_Else_Running_State);

      N_Ready_State := Make_If_Statement
              (Make_Infix_Function_Call_Expression (
                    Make_Predefined_Function
                     (K_Greater_Than_Or_Equal_To, false),
                    Make_Identifier ("Counter"),
                    Make_Array_Elt_Access_Expression (
                      Make_Nat (5),
                      Make_Array_Elt_Access_Expression (
                      Make_Identifier ("K"),
                      Make_Identifier ("Threads")))),
            --  then Missed_Deadline state
            New_List (
                Make_Assignment_Statement (
                   Make_Identifier ("Threads"),
                   Make_Function_Call_Expression
                     (Make_Identifier ("Assign"),
                       New_List (
                        Make_Identifier ("Threads"),
                        Make_Identifier ("K"),
                        Make_Nat (9),
                        Make_Time_Const (0)))),
                Thread_Activation,
                Make_Assignment_Statement
                    (Make_Identifier ("k"),
                      Make_Infix_Function_Call_Expression
                          (Make_Identifier ("+"),
                           Make_Identifier ("k"),
                           Make_Nat (1)))),
            No_List,
            --  else Running state
            L_Else_Running_State);

      L_While := New_List (
           Make_If_Statement (
               Make_Infix_Function_Call_Expression
               (Make_Predefined_Function (K_And),
                Make_Parenthesized_Expression (
                Make_Infix_Function_Call_Expression (
                  Make_Predefined_Function (K_Equality, false),
                    Make_Array_Elt_Access_Expression (
                      Make_Nat (10),
                      Make_Array_Elt_Access_Expression (
                      Make_Identifier ("K"),
                      Make_Identifier ("Threads"))),
                    Make_Time_Const (1))),

               Make_Infix_Function_Call_Expression
                (Make_Predefined_Function (K_And),
                  Make_Parenthesized_Expression (
                  Make_Infix_Function_Call_Expression (
                    Make_Predefined_Function (K_Equality, false),
                      Make_Array_Elt_Access_Expression (
                        Make_Nat (9),
                        Make_Array_Elt_Access_Expression (
                        Make_Identifier ("K"),
                        Make_Identifier ("Threads"))),
                       Make_Time_Const (1))),
                   Make_Parenthesized_Expression (
                   Make_Infix_Function_Call_Expression (
                    Make_Predefined_Function
                     (K_Greater_Than_Or_Equal_To, false),
                    Make_Identifier ("Counter"),
                    Make_Array_Elt_Access_Expression (
                      Make_Nat (4),
                      Make_Array_Elt_Access_Expression (
                      Make_Identifier ("K"),
                      Make_Identifier ("Threads")))))
                     )),
        --  test Missed_Deadline state
        New_List (N_Ready_State),
        No_List,
        New_List (Make_Assignment_Statement
                    (Make_Identifier ("k"),
                      Make_Infix_Function_Call_Expression
                          (Make_Identifier ("+"),
                           Make_Identifier ("k"),
                           Make_Nat (1))))));
      if (Not_Periodic_Thread_Number > 0) then
         BLNu.Append_Node_To_List (Make_Assignment_Statement
                    (Make_Identifier ("Is_Activated"),
                     Make_Identifier ("false")), L_While);
         BLNu.Append_List_To_List (L_Incoming_Event, L_While);
         BLNu.Append_Node_To_List (Make_If_Statement
                    (Make_Identifier ("Is_Activated"),
                     New_List (Make_Assignment_Statement
                    (Make_Identifier ("k"),
                     Make_Nat (1))),
                     No_List,
                     No_List), L_While);

      end if;

      L_In_Then := New_List (
                   Make_Assignment_Statement
                    (Make_Identifier ("Nb_Active_Thread"),
                     Make_Identifier ("0")),
                   Make_Assignment_Statement
                    (Make_Identifier ("K"),
                     Make_Nat (1)),

                   Make_While_Statement (
                    Make_Parenthesized_Expression (
                     Make_Infix_Function_Call_Expression
                      (Make_Predefined_Function
                       (K_Less_Than_Or_Equal_To, false),
                        Make_Identifier ("K"),
                        Make_Identifier ("Thread_Number"))),
                     L_While),

                   Make_If_Statement
                    (Make_Infix_Function_Call_Expression
                      (Make_Predefined_Function
                        (K_Equality, false),
                       Make_Identifier ("Nb_Active_Thread"),
                       Make_Identifier ("0")),
                     New_List (
                       Make_Assignment_Statement
                        (Make_Identifier ("Counter"),
                         Make_Infix_Function_Call_Expression
                          (Make_Identifier ("+"),
                           Make_Identifier ("Counter"),
                           Make_Time_Const (1)))),
                     No_List,
                     No_List));

      N_Sort := Make_If_Statement
                (Make_Infix_Function_Call_Expression
                  (Make_Predefined_Function
                     (K_Less_Than, false),
                   Make_Identifier ("Counter"),
                   Make_Identifier ("PPCM_THREAD")),
                 L_In_Then,
                 No_List,
                 L_Stop_Orders);

      N := Make_Process_Definition
      (No_Node,
       Make_Identifier ("Processor"),
       No_List,
       No_List, -- to be added in main generation
       No_List,
       No_List,
       New_List (
         Make_Var_Loop_Select (
           Make_Var_declaration_List,
           L_Threads_Array,
           New_List (N_Sort), false)));
      The_Processor := BLNu.Make_Node_Container (N);
      BLNu.Append_Node_To_List (N, Definitions_List);
   end Make_LNT_RM_Processor;

   --------------------------
   --  Threads_Hyperperiod --
   --------------------------
   function Threads_Hyperperiod (T : Period_Array)
    return Natural is
      function Array_Min (Ar : Period_Array) return Natural;
      function Array_Min (Ar : Period_Array) return Natural is
         A : Natural := Ar (1);
      begin
         for i in 2 .. Ar'Length loop
            if Ar (i) < A then
               A := Ar (i);
            end if;
         end loop;
         return A;
      end Array_Min;
      Aux : Period_Array := T;
      Min : Natural;
      Loop_End : boolean;
   begin
      loop
         Loop_End := true;
         Min := Array_Min (Aux);
         for i in T'Range loop
            if Aux (i) > Min then
               Loop_End := false;
               exit;
            end if;
         end loop;
         exit when Loop_End;
         for i in T'Range loop
            if Aux (i) = Min then
               Aux (i) := Aux (i) + T (i);
            end if;
         end loop;
      end loop;
      return Min;

   end Threads_HyperPeriod;

   -------------------------------------
   -- Make_LNT_RM_Processor_Functions --
   -------------------------------------
   procedure Make_LNT_RM_Processor_Functions is
      N : Node_Id;
      L_Sts_S : List_Id;
      L_Sts_A : List_Id;
      N_Case_Update : Node_Id;
      Activate_K_Gates_List : List_Id;
      Activate_K_Case_Sts_List : List_Id;
      Activate_K_Case_St : Node_Id;
   begin

      N := Make_Function_Definition
        (Make_Identifier ("Assign"),
         New_List (
          Make_Parameter_Specification
             (Make_Identifier ("Threads"),
              Make_Identifier ("LNT_Type_Thread_Array"),
              Mode_Out),
          Make_Parameter_Specification
             (Make_Identifier ("I"),
              Make_Identifier ("Nat"),
              Mode_In),
          Make_Parameter_Specification
             (Make_Identifier ("K"),
              Make_Identifier ("Nat"),
              Mode_In),
          Make_Parameter_Specification
             (Make_Identifier ("Val"),
              Make_Identifier ("LNT_Type_Time_Constraint"),
              Mode_In)),
         Make_Identifier ("LNT_Type_Thread_Array"),
         No_List,
         No_List,
         New_List (
           Make_Var_Statement
            (New_List (Make_Var_Declaration
              (Make_Identifier ("P"),
               Make_Identifier ("LNT_Type_Thread"))),
             New_List (
               Make_Assignment_Statement
               (Make_Identifier ("P"),
                Make_Array_Elt_Access_Expression
                 (Make_Identifier ("I"),
                  Make_Identifier ("Threads"))),
               Make_Array_Element_Assignment_Statement
                   (Make_Identifier ("P"),
                    Make_Identifier ("K"),
                    Make_Identifier ("Val")),
               Make_Array_Element_Assignment_Statement
                   (Make_Identifier ("Threads"),
                    Make_Identifier ("I"),
                    Make_Identifier ("P")))),
           Make_Return_Statement
           (Make_Identifier ("Threads"), true))
         );
      BLNu.Append_Node_To_List (N, Definitions_List);

      N_Case_Update := Make_Case_Statement (
         Make_Pattern (
            Make_Array_Elt_Access_Expression (
                 Make_Nat (11),
                 Make_Identifier ("P")),
            Make_Identifier ("LNT_Type_Time_Constraint"),
            false,
            true),
          No_List,
          New_List (
           --  Sporadic
           Make_Case_Statement_Alternative (
              New_List (Make_Time_Const (1)),
              New_List (Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (4),
                      Make_Array_Elt_Access_Expression
                         (Make_Nat (5),
                          Make_Identifier ("P"))),
                     Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (5),
                      Make_Identifier ("PPCM_THREAD")),
                     Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (10),
                      Make_Time_Const (0)))),
           --  Hybrid and Periodic
           Make_Case_Statement_Alternative (
              New_List (Make_Time_Const (0), Make_Time_Const (3)),
              New_List (Make_If_Statement (
                   Make_Infix_Function_Call_Expression (
                   Make_Predefined_Function (K_And),
                   Make_Parenthesized_Expression (
                     Make_Infix_Function_Call_Expression
                      (Make_Predefined_Function
                       (K_Less_Than_Or_Equal_To, false),
                       Make_Infix_Function_Call_Expression
                      (Make_Identifier ("*"),
                       Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (3),
                          Make_Identifier ("P")),
                       Make_Time_Const (1)),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P"))),
                       Make_Identifier ("PPCM_THREAD"))),
                   Make_Parenthesized_Expression (
                     Make_Infix_Function_Call_Expression
                      (Make_Predefined_Function
                       (K_Less_Than_Or_Equal_To, false),
                       Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (4),
                          Make_Identifier ("P")),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P"))),
                          Make_Identifier ("PPCM_THREAD")))),
                 New_List (
                   Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (4),
                      Make_Infix_Function_Call_Expression
                      (Make_Identifier ("*"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (3),
                          Make_Identifier ("P")),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P")))),
                   Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (3),
                      Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (3),
                          Make_Identifier ("P")),
                       Make_Time_Const (1))),
                   Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (5),
                      Make_Infix_Function_Call_Expression
                      (Make_Identifier ("*"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (3),
                          Make_Identifier ("P")),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P"))))
                 ),  --  then
                 No_List,
                 New_List (Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (10),
                      Make_Time_Const (0)))))),
           --  Timed
           Make_Case_Statement_Alternative (
              New_List (Make_Time_Const (2)),
              New_List (Make_If_Statement (
                   Make_Infix_Function_Call_Expression (
                   Make_Predefined_Function (K_And),
                   Make_Parenthesized_Expression (
                     Make_Infix_Function_Call_Expression
                      (Make_Predefined_Function
                       (K_Less_Than_Or_Equal_To, false),
                       Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (4),
                          Make_Identifier ("P")),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P"))),
                          Make_Identifier ("PPCM_THREAD"))),
                   Make_Parenthesized_Expression (
                     Make_Infix_Function_Call_Expression
                      (Make_Predefined_Function
                       (K_Less_Than_Or_Equal_To, false),
                       Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (5),
                          Make_Identifier ("P")),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P"))),
                          Make_Identifier ("PPCM_THREAD")))),
                 New_List (
                   Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (4),
                      Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (4),
                          Make_Identifier ("P")),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P")))),
                   Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (5),
                      Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (5),
                          Make_Identifier ("P")),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (1),
                          Make_Identifier ("P"))))
                 ),  --  then
                 No_List,
                 New_List (Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (10),
                      Make_Time_Const (0)))))),
           --  Any
           Make_Case_Statement_Alternative (
              No_List,
              New_List (Make_Null_Statement))));

      L_Sts_S := New_List (
               Make_Assignment_Statement
               (Make_Identifier ("P"),
                Make_Array_Elt_Access_Expression
                 (Make_Identifier ("I"),
                  Make_Identifier ("Aux_Threads"))),

               Make_If_Statement
                (Make_Infix_Function_Call_Expression
                  (Make_Predefined_Function (K_Equality, false),
                   Make_Array_Elt_Access_Expression
                         (Make_Nat (2),
                          Make_Identifier ("P")),
                   Make_Time_Const (0)),
                 New_List (Make_Array_Element_Assignment_Statement
                   (Make_Identifier ("P"),
                    Make_Nat (6),
                    Make_Time_Const (1))),
                 No_List,
                 No_List),

               Make_Array_Element_Assignment_Statement
                   (Make_Identifier ("P"),
                    Make_Nat (2),
                    Make_Infix_Function_Call_Expression
                      (Make_Identifier ("+"),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (2),
                          Make_Identifier ("P")),
                          Make_Identifier ("TODO"))),

               Make_Array_Element_Assignment_Statement
                   (Make_Identifier ("P"),
                    Make_Nat (8),
                    Make_Time_Const (1)),
               --  if case statement for new period
               Make_If_Statement (
                  Make_Infix_Function_Call_Expression
                      (Make_Predefined_Function (K_Equality, false),
                       Make_Array_Elt_Access_Expression
                         (Make_Nat (2),
                          Make_Identifier ("P")),
                          Make_Array_Elt_Access_Expression
                         (Make_Nat (0),
                          Make_Identifier ("P"))),
                  New_List (
                     Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (7),
                      Make_Time_Const (1)),
                     Make_Array_Element_Assignment_Statement
                     (Make_Identifier ("P"),
                      Make_Nat (2),
                      Make_Time_Const (0)), N_Case_Update)),

               Make_Array_Element_Assignment_Statement
                   (Make_Identifier ("Aux_Threads"),
                    Make_Identifier ("I"),
                    Make_Identifier ("P")));

      N := Make_Function_Definition
        (Make_Identifier ("Update_Thread"),
         New_List (
          Make_Parameter_Specification
             (Make_Identifier ("Aux_Threads"),
              Make_Identifier ("LNT_Type_Thread_Array"),
              Mode_Out),
          Make_Parameter_Specification
             (Make_Identifier ("I"),
              Make_Identifier ("Nat"),
              Mode_In),
          Make_Parameter_Specification
             (Make_Identifier ("TODO"),
              Make_Identifier ("LNT_Type_Time_Constraint"),
              Mode_In)),
         Make_Identifier ("LNT_Type_Thread_Array"),
         No_List,
         No_List,
         New_List (
           Make_Var_Statement
            (New_List (Make_Var_Declaration
              (Make_Identifier ("P"),
               Make_Identifier ("LNT_Type_Thread"))),
               L_Sts_S),
           Make_Return_Statement
           (Make_Identifier ("Aux_Threads"), true))
         );
      BLNu.Append_Node_To_List (N, Definitions_List);

      --  Sporadic_Notif
      N := Make_Process_Definition
        (No_Node,
         Make_Identifier ("Sporadic_Notif"),
         New_List (
          Make_Parameter_Specification
             (Make_Identifier ("Threads"),
              Make_Identifier ("LNT_Type_Thread_Array"),
              Mode_Inout),
          Make_Parameter_Specification
             (Make_Identifier ("k"),
              Make_Identifier ("Nat"),
              Mode_In),
          Make_Parameter_Specification
             (Make_Identifier ("Counter"),
              Make_Identifier ("LNT_Type_Time_Constraint"),
              Mode_In),
          Make_Parameter_Specification
             (Make_Identifier ("Is_Activated"),
              Make_Identifier ("bool"),
              Mode_Inout)),
         New_List (Make_Gate_Declaration
            (Make_Identifier ("LNT_Channel_Event"),
             Make_Identifier ("INCOMING_EVENT_GATE"))),
         No_List,
         No_List,
         New_List (
----------  select
          Make_Select_Statement (New_List (
            Make_Select_Statement_Alternative
               (New_List (
                 Make_Communication_Statement
                  (Make_Identifier ("INCOMING_EVENT_GATE"),
                   New_List (Make_Identifier ("Incoming_Event"))),
                 Make_Assignment_Statement
                   (Make_Identifier ("Threads"),
                     Make_Function_Call_Expression
                      (Make_Identifier ("Assign"),
                     New_List (
                       Make_Identifier ("Threads"),
                       Make_Identifier ("K"),
                       Make_Nat (3),
                       Make_Parenthesized_Expression (
                       Make_Infix_Function_Call_Expression (
                         Make_Identifier ("+"),
                         Make_Array_Elt_Access_Expression (
                            Make_Nat (3),
                            Make_Array_Elt_Access_Expression (
                              Make_Identifier ("k"),
                              Make_Identifier ("Threads"))),
                         Make_Time_Const (1)))))))),

          Make_Select_Statement_Alternative
               (New_List (Make_Communication_Statement
                  (Make_Identifier ("INCOMING_EVENT_GATE"),
                   New_List (Make_Identifier ("No_Event"))))))),
-------------  if
          Make_If_Statement (
          Make_Infix_Function_Call_Expression (
           Make_Predefined_Function (K_And),
           Make_Parenthesized_Expression (
           Make_Infix_Function_Call_Expression (
             Make_Predefined_Function (
               K_Greater_Than_Or_Equal_To, false),
             Make_Identifier ("Counter"),
             Make_Array_Elt_Access_Expression (
               Make_Nat (4),
                 Make_Array_Elt_Access_Expression (
                   Make_Identifier ("k"),
                   Make_Identifier ("Threads"))))),

           Make_Parenthesized_Expression (
           Make_Infix_Function_Call_Expression (
             Make_Predefined_Function (
               K_Greater_Than, false),
             Make_Array_Elt_Access_Expression (
               Make_Nat (3),
               Make_Array_Elt_Access_Expression (
                   Make_Identifier ("k"),
                   Make_Identifier ("Threads"))),
             Make_Time_Const (0)))),

           New_List (
           Make_Assignment_Statement
            (Make_Identifier ("Threads"),
             Make_Function_Call_Expression
               (Make_Identifier ("Assign"),
                New_List (Make_Identifier ("Threads"),
                  Make_Identifier ("k"),
                  Make_Nat (4),
                  Make_Identifier ("Counter")))),
           Make_Assignment_Statement
            (Make_Identifier ("Threads"),
             Make_Function_Call_Expression
               (Make_Identifier ("Assign"),
                New_List (Make_Identifier ("Threads"),
                  Make_Identifier ("k"),
                  Make_Nat (5),
                  Make_Parenthesized_Expression (
                  Make_Infix_Function_Call_Expression (
                    Make_Identifier ("+"),
                    Make_Identifier ("Counter"),
                    Make_Array_Elt_Access_Expression (
                      Make_Nat (1),
                      Make_Array_Elt_Access_Expression (
                        Make_Identifier ("k"),
                        Make_Identifier ("Threads")))))))),
           Make_Assignment_Statement
            (Make_Identifier ("Threads"),
             Make_Function_Call_Expression
               (Make_Identifier ("Assign"),
                New_List (Make_Identifier ("Threads"),
                  Make_Identifier ("k"),
                  Make_Nat (10),
                  Make_Time_Const (1)))),
           Make_Assignment_Statement
                   (Make_Identifier ("Threads"),
                     Make_Function_Call_Expression
                      (Make_Identifier ("Assign"),
                     New_List (
                       Make_Identifier ("Threads"),
                       Make_Identifier ("K"),
                       Make_Nat (3),
                       Make_Parenthesized_Expression (
                       Make_Infix_Function_Call_Expression (
                         Make_Identifier ("-"),
                         Make_Array_Elt_Access_Expression (
                            Make_Nat (3),
                            Make_Array_Elt_Access_Expression (
                              Make_Identifier ("k"),
                              Make_Identifier ("Threads"))),
                         Make_Time_Const (1)))))),
           Make_Assignment_Statement
                    (Make_Identifier ("Is_Activated"),
                     Make_Identifier ("true"))),
         No_List,
         No_List)));

      BLNu.Append_Node_To_List (N, Definitions_List);

      --  Timed or Hybrid notif
      N := Make_Process_Definition
        (No_Node,
         Make_Identifier ("Timed_Hybrid_Notif"),
         New_List (
          Make_Parameter_Specification
             (Make_Identifier ("Threads"),
              Make_Identifier ("LNT_Type_Thread_Array"),
              Mode_Inout),
          Make_Parameter_Specification
             (Make_Identifier ("k"),
              Make_Identifier ("Nat"),
              Mode_In),
          Make_Parameter_Specification
             (Make_Identifier ("Counter"),
              Make_Identifier ("LNT_Type_Time_Constraint"),
              Mode_In),
          Make_Parameter_Specification
             (Make_Identifier ("Is_Activated"),
              Make_Identifier ("bool"),
              Mode_Inout)),
         New_List (Make_Gate_Declaration
            (Make_Identifier ("LNT_Channel_Event"),
             Make_Identifier ("INCOMING_EVENT_GATE"))),
         No_List,
         No_List,
         New_List (
         --  select
          Make_Select_Statement (New_List (
            Make_Select_Statement_Alternative
               (New_List (
                 Make_Communication_Statement
                  (Make_Identifier ("INCOMING_EVENT_GATE"),
                   New_List (Make_Identifier ("Incoming_Event"))),
                 Make_Assignment_Statement (
                   Make_Identifier ("Threads"),
                   Make_Function_Call_Expression
                    (Make_Identifier ("Assign"),
                     New_List (Make_Identifier ("Threads"),
                       Make_Identifier ("k"),
                       Make_Nat (4),
                       Make_Identifier ("Counter")))),
                 Make_Assignment_Statement
                  (Make_Identifier ("Threads"),
                   Make_Function_Call_Expression
                   (Make_Identifier ("Assign"),
                    New_List (Make_Identifier ("Threads"),
                     Make_Identifier ("k"),
                     Make_Nat (5),
                     Make_Parenthesized_Expression (
                     Make_Infix_Function_Call_Expression (
                      Make_Identifier ("+"),
                      Make_Identifier ("Counter"),
                      Make_Array_Elt_Access_Expression (
                        Make_Nat (1),
                        Make_Array_Elt_Access_Expression (
                          Make_Identifier ("k"),
                          Make_Identifier ("Threads")))))))),
                 Make_Assignment_Statement
                    (Make_Identifier ("Is_Activated"),
                     Make_Identifier ("true")))),
          Make_Select_Statement_Alternative
               (New_List (Make_Communication_Statement
                  (Make_Identifier ("INCOMING_EVENT_GATE"),
                   New_List (Make_Identifier ("No_Event")))))))));

      BLNu.Append_Node_To_List (N, Definitions_List);

      L_Sts_A := New_List (
        Make_If_Statement
         (Make_Infix_Function_Call_Expression (
            Make_Predefined_Function (K_Equality, false),
            Make_Array_Elt_Access_Expression (
              Make_Nat (8),
              Make_Array_Elt_Access_Expression (
                Make_Identifier ("K"),
                Make_Identifier ("Threads"))),
            Make_Time_Const (1)),
          New_List (
            Make_If_Statement
             (Make_Infix_Function_Call_Expression
               (Make_Predefined_Function (K_And),
                Make_Parenthesized_Expression (
                Make_Infix_Function_Call_Expression (
                  Make_Predefined_Function (K_Equality, false),
                    Make_Array_Elt_Access_Expression (
                      Make_Nat (6),
                      Make_Array_Elt_Access_Expression (
                      Make_Identifier ("K"),
                      Make_Identifier ("Threads"))),
                    Make_Time_Const (1))),
                Make_Parenthesized_Expression (
                Make_Infix_Function_Call_Expression (
                  Make_Predefined_Function (K_Equality, false),
                    Make_Array_Elt_Access_Expression (
                    Make_Nat (7),
                  Make_Array_Elt_Access_Expression (
                    Make_Identifier ("K"),
                    Make_Identifier ("Threads"))),
                  Make_Time_Const (1)))),

              New_List (
                Make_Communication_Statement
                  (Make_Identifier ("ACTIVATION"),
                   New_List (Make_Identifier ("T_Dispatch_Completion")),
                   false, No_Node),
                Make_Assignment_Statement (
                  Make_Identifier ("Threads"),
                  Make_Function_Call_Expression
                   (Make_Identifier ("Assign"),
                    New_List (Make_Identifier ("Threads"),
                       Make_Identifier ("K"),
                       Make_Nat (6),
                       Make_Time_Const (0)))),
                Make_Assignment_Statement (
                  Make_Identifier ("Threads"),
                  Make_Function_Call_Expression
                   (Make_Identifier ("Assign"),
                    New_List (Make_Identifier ("Threads"),
                       Make_Identifier ("K"),
                       Make_Nat (7),
                       Make_Time_Const (0))))),
              New_List (
                Make_Elsif_Statement
                 (Make_Infix_Function_Call_Expression (
                    Make_Predefined_Function (K_Equality, false),
                     Make_Array_Elt_Access_Expression (
                      Make_Nat (7),
                      Make_Array_Elt_Access_Expression (
                      Make_Identifier ("K"),
                      Make_Identifier ("Threads"))),
                    Make_Time_Const (1)),
                  New_List (
                  Make_Communication_Statement
                  (Make_Identifier ("ACTIVATION"),
                   New_List (Make_Identifier ("T_Preemption_Completion")),
                   false, No_Node),
                Make_Assignment_Statement (
                  Make_Identifier ("Threads"),
                  Make_Function_Call_Expression
                   (Make_Identifier ("Assign"),
                    New_List (Make_Identifier ("Threads"),
                       Make_Identifier ("K"),
                       Make_Nat (7),
                       Make_Time_Const (0)))))),
                Make_Elsif_Statement
                   (Make_Infix_Function_Call_Expression (
                     Make_Predefined_Function (K_Equality, false),
                      Make_Array_Elt_Access_Expression (
                        Make_Nat (6),
                        Make_Array_Elt_Access_Expression (
                        Make_Identifier ("K"),
                        Make_Identifier ("Threads"))),
                     Make_Time_Const (1)),
                New_List (
                Make_Communication_Statement
                  (Make_Identifier ("ACTIVATION"),
                   New_List (Make_Identifier ("T_Dispatch_Preemption")),
                   false, No_Node),
                Make_Assignment_Statement (
                  Make_Identifier ("Threads"),
                  Make_Function_Call_Expression
                   (Make_Identifier ("Assign"),
                    New_List (Make_Identifier ("Threads"),
                       Make_Identifier ("K"),
                       Make_Nat (6),
                       Make_Time_Const (0))))))),

              New_List (Make_Communication_Statement
                  (Make_Identifier ("ACTIVATION"),
                   New_List (Make_Identifier ("T_Preemption")),
                   false, No_Node))),
            Make_Assignment_Statement (
             Make_Identifier ("Threads"),
             Make_Function_Call_Expression
             (Make_Identifier ("Assign"),
              New_List (Make_Identifier ("Threads"),
                       Make_Identifier ("K"),
                       Make_Nat (8),
                       Make_Time_Const (0)))),
            Make_Communication_Statement
              (Make_Identifier ("ACTIVATION"),
               New_List (Make_Identifier ("T_Complete")))),
          New_List (
            Make_Elsif_Statement
                 (Make_Infix_Function_Call_Expression (
                   Make_Predefined_Function (K_Equality, false),
                     Make_Array_Elt_Access_Expression (
                      Make_Nat (9),
                      Make_Array_Elt_Access_Expression (
                      Make_Identifier ("K"),
                      Make_Identifier ("Threads"))),
                    Make_Time_Const (0)),
                  New_List (
                Make_Communication_Statement
                  (Make_Identifier ("ACTIVATION"),
                   New_List (Make_Identifier ("T_ERROR")),
                   false, No_Node)))),
          No_List));

      Activate_K_Gates_List := New_List;
      Activate_K_Case_Sts_List := New_List;

      for I in 1 .. Thread_Number loop
         BLNu.Append_Node_To_List (
          Make_Gate_Declaration
            (Make_Identifier ("LNT_Channel_Dispatch"),
             New_Identifier (
           Remove_Prefix_From_Name (
           " ", Get_String_Name (Integer'Image (I))),
           "ACTIVATION_")),
          Activate_K_Gates_List);

         BLNu.Append_Node_To_List (
            Make_Case_Statement_Alternative (
            New_List (Make_Identifier (Integer'Image (I))),
            New_List (Make_Process_Instantiation_Statement (
                Make_Identifier ("Thread_Activation"),
                New_List (New_Identifier (
           Remove_Prefix_From_Name (
           " ", Get_String_Name (Integer'Image (I))),
           "ACTIVATION_")),
                New_List (
               Make_Actual_Parameter
                (Make_Identifier ("Threads"), false, true),
               Make_Actual_Parameter
                (Make_Identifier (Integer'Image (I))))))),
          Activate_K_Case_Sts_List);
      end loop;
      BLNu.Append_Node_To_List (
            Make_Case_Statement_Alternative (
            No_List,
            New_List (Make_Null_Statement)),
          Activate_K_Case_Sts_List);

      Activate_k_Case_St := Make_Case_Statement
         (Make_Identifier ("k"),
          No_List,
          Activate_K_Case_Sts_List);

      N := Make_Process_Definition
        (No_Node,
         Make_Identifier ("Activate_K"),
         New_List (
          Make_Parameter_Specification
             (Make_Identifier ("Threads"),
              Make_Identifier ("LNT_Type_Thread_Array"),
              Mode_Inout),
          Make_Parameter_Specification
             (Make_Identifier ("K"),
              Make_Identifier ("Nat"),
              Mode_In)),
         Activate_K_Gates_List,
         No_List,
         No_List,
         New_List (Activate_k_Case_St));

      BLNu.Append_Node_To_List (N, Definitions_List);

      N := Make_Process_Definition
        (No_Node,
         Make_Identifier ("Thread_Activation"),
         New_List (
          Make_Parameter_Specification
             (Make_Identifier ("Threads"),
              Make_Identifier ("LNT_Type_Thread_Array"),
              Mode_Inout),
          Make_Parameter_Specification
             (Make_Identifier ("K"),
              Make_Identifier ("Nat"),
              Mode_In)),
         New_List (Make_Gate_Declaration
            (Make_Identifier ("LNT_Channel_Dispatch"),
             Make_Identifier ("ACTIVATION"))),
         No_List,
         No_List,
         L_Sts_A);

      BLNu.Append_Node_To_List (N, Definitions_List);
   end Make_LNT_RM_Processor_Functions;

   procedure Quick_Sort_Threads (Threads : in out Thread_Array)
   is
      Left, Right : Natural range Threads'First - 1 .. Threads'Last + 1;
      Pivot       : Natural;
      procedure Swap
        (I       : in     Natural;
         J       : in     Natural;
         Threads : in out Thread_Array)
      is
         Tmp : constant Thread := Threads (I);
      begin
         Threads (I) := Threads (J);
         Threads (J) := Tmp;
      end Swap;
   begin
      Left  := Threads'First;
      Right := Threads'Last;
      Pivot := Threads ((Left + Right) / 2).Period;

      while Left <= Right loop
         while Threads (Left).Period < Pivot loop
            Left := Left + 1;
         end loop;
         while Threads (Right).Period > Pivot loop
            Right := Right - 1;
         end loop;
         if Left <= Right then
            if Left < Right then
               Swap (Left, Right, Threads);
            end if;
            Left := Left + 1;
            Right := Right - 1;
         end if;
      end loop;

      if Threads'First < Right then
         Quick_Sort_Threads (Threads (Threads'First .. Right));
      end if;
      if Threads'Last > Left then
         Quick_Sort_Threads (Threads (Left .. Threads'Last));
      end if;
   end Quick_Sort_Threads;

   procedure Make_Notif (
      Event_Port_Number : Natural;
      I : Natural;
      Notif_Function : String;
      L_Incoming_Event : List_Id)
   is
      N_Event_Name : Name_Id;
      N_Event : Node_Id;
   begin
      N_Event_Name := New_Identifier (
        Remove_Prefix_From_Name (
        " ", Get_String_Name (Integer'Image (I))),
        "INCOMING_EVENT_");

      for P in 1 .. Event_Port_Number loop
         if (P = 1) then
            N_Event := Make_Identifier (N_Event_Name);
         else
            N_Event := New_Identifier (
              Remove_Prefix_From_Name (
              " ", Get_String_Name (
              Integer'Image (P - 1))),
              Get_Name_String (N_Event_Name));
         end if;
         BLNu.Append_Node_To_List (
           Make_Process_Instantiation_Statement (
             Make_Identifier (Notif_Function),
               New_List (N_Event),
               New_List (
               Make_Actual_Parameter (
                 Make_Identifier ("Threads"), false, true),
                 Make_Actual_Parameter (
                   Make_Identifier (Integer'Image (I))),
                   Make_Actual_Parameter (
                     Make_Identifier ("Counter")),
                     Make_Actual_Parameter (
                   Make_Identifier ("Is_Activated"), false, true))),
           L_Incoming_Event);
      end loop;
   end Make_Notif;

   function Connections_Number (Port : Node_Id) return Natural is
      N, Parent : Node_Id;
      Val_Left      : Natural          := 0;
   begin
            if AIN.Kind (Port) = K_Port_Spec_Instance then
               if AIN.Is_In (Port) then
                  N := AIN.First_Node (Sources (Port));
                  while Present (N) loop
                     Parent := AIN.Item (N);
                     if AIN.Kind (Parent) = K_Port_Spec_Instance then
                        Parent := AIN.Parent_Component (Parent);
                        if Get_Category_Of_Component (Parent) =
                          CC_Thread
                        then
                           Val_Left := Val_Left + 1;
                        end if;
                     end if;
                     N := AIN.Next_Node (N);
                  end loop;
               end if;
               if AIN.Is_Out (Port) then
                  N := AIN.First_Node (Destinations (Port));
                  while Present (N) loop
                     Parent := AIN.Item (N);
                     if AIN.Kind (Parent) = K_Port_Spec_Instance then
                        Parent := AIN.Parent_Component (Parent);
                        if Get_Category_Of_Component (Parent) =
                          CC_Thread
                        then
                           Val_Left := Val_Left + 1;
                        end if;
                     end if;
                     N := AIN.Next_Node (N);
                  end loop;
               end if;
            end if;
      return Val_Left;
   end Connections_Number;
end Ocarina.Backends.LNT.Tree_Generator_Processor;
