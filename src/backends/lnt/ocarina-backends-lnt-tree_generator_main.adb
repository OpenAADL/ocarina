------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.LNT.TREE_GENERATOR_MAIN                  --
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
with Utils;         use Utils;

with Ocarina.Backends.LNT.Nutils;
with Ocarina.Backends.LNT.Nodes;
with Ocarina.Backends.LNT.Components;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

use Ocarina.Backends.LNT.Components;

use Ocarina.ME_AADL;
use Ocarina.ME_AADL.AADL_Instances.Entities;
use Ocarina.Backends.Properties;
use Ocarina.Backends.Messages;

with Ocarina.ME_AADL.AADL_Instances.Debug;
use Ocarina.ME_AADL.AADL_Instances.Debug;
with Ada.Text_IO; use Ada.Text_IO;

package body Ocarina.Backends.LNT.Tree_Generator_Main is

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINu renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   use AIN;

   package BLN renames Ocarina.Backends.LNT.Nodes;
   package BLNu renames Ocarina.Backends.LNT.Nutils;
   use BLN;
   use BLNu;

   procedure Visit (E : Node_Id);
   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);
   procedure Visit_Device_Instance (E : Node_Id);

   procedure Find_Instance_By_Name (Key : Name_Id; Target : List_Id;
                       Instance : out Node_Id; Index : out Natural;
                       Is_Not_Periodic : out boolean);

   procedure Make_Process_Main;
   function Make_Gate_Identifier (S : Node_Id) return Name_Id;

   function Make_Gate_Identifier (S : Node_Id) return Name_Id is
      N_Connection : Node_Id;
      L : List_Id;
      Connection : Name_Id;
   begin
      if (AIN.Is_In (S)) then
         --  port_in thread
         L := Sources (S);
         --  port_out thread, first connection 1-1
         if Present (AIN.First_Node (L)) then
            N_Connection := AIN.Item (AIN.First_Node (L));
            while (AIN.Is_In (N_Connection)) loop
               L := Sources (N_Connection);
               if Present (AIN.First_Node (L)) then
                  N_Connection := AIN.Item (AIN.First_Node (L));
               end if;
            end loop;
            N_Connection := AIN.Extra_Item (AIN.First_Node (L));
            --  connection identifier at system level
            Connection := Add_Prefix_To_Name
             ("RECEIVE_", AIN.Display_Name (
             AIN.Identifier (N_Connection)));
         end if;
      elsif (AIN.Is_Out (S)) then
         --  port_in thread
         L := Destinations (S);
         --  port_out thread, first connection 1-1
         if Present (AIN.First_Node (L)) then
            N_Connection := AIN.Item (AIN.First_Node (L));

            while (AIN.Is_Out (N_Connection)) loop
            --  port_out process
               L := Destinations (N_Connection);
               if Present (AIN.First_Node (L)) then
                  N_Connection := AIN.Item (AIN.First_Node (L));
               end if;
            end loop;
            --  connection instance system/process
            N_Connection := AIN.Extra_Item (AIN.First_Node (L));
            Connection := Add_Prefix_To_Name
             ("SEND_", AIN.Display_Name (
              AIN.Identifier (N_Connection)));
         end if;
      end if;
      return Connection;
   end Make_Gate_Identifier;

   Module_Node : Node_Id := No_Node;
   Definitions_List : List_Id := No_List;
   Modules_List : List_Id := No_List;
   Module_Pragmas_List : List_Id := No_List;
   Predefined_Functions_List : List_Id := No_List;

   --  Main_Hide_Declaration_List : List_Id := No_List;
   Main_Gate_Declaration_List : List_Id := No_List;
   Processor_Gates_List : List_Id := No_List;
   Processor_Event_Gates_List : List_Id := No_List;
   Processor_Event_Gate_Declaration_List : List_Id := No_List;
   Main_Interface_Synchronisation_List : List_Id := No_List;

   function Generate_LNT_Main (AADL_Tree : Node_Id)
     return Node_Id is
   begin
      Put_Line ("Begin Main");
      Processor_Gates_List := New_List;
      Main_Gate_Declaration_List := New_List;
      Main_Interface_Synchronisation_List := New_List;
      Processor_Event_Gate_Declaration_List := New_List;
      Processor_Event_Gates_List := New_List;
      LNT_Processor_Gate_Declaration_List := New_List;
      Visit (AADL_Tree);
      return Module_Node;
   end Generate_LNT_Main;

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
       (New_Identifier (Get_String_Name ("_Main"),
                        Get_Name_String (System_Name)));
      if ((Hyperperiod * 2) > 255) then
         Module_Pragmas_List := New_List (
           Make_Module_Pragma
            (K_Nat_Bits, 16));
      end if;
      Definitions_List := New_List;
      Modules_List := New_List (
                        Make_Identifier ("Types"),
                        New_Identifier (
                          Get_String_Name ("_Processor"),
                          Get_Name_String (System_Name)),
                        New_Identifier (
                          Get_String_Name ("_Threads"),
                          Get_Name_String (System_Name)));
      if BLNu.Is_Empty (LNT_States_List) then
         BLNu.Append_Node_To_List (
          Make_Identifier (
            "LNT_Generic_Process_For_Port_Connections"),
          Modules_List);
      else
         BLNu.Append_Node_To_List (
          Make_Identifier (
           "LNT_Generic_Process_For_Port_Connections_BA"),
          Modules_List);
      end if;

      Predefined_Functions_List := New_List;
      Visit (N);
      Make_Process_Main;
      Set_Definitions (Module_Node, Definitions_List);
      Set_Modules (Module_Node, Modules_List);
      Set_Module_Pragmas (Module_Node, Module_Pragmas_List);
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
   begin
      --  Visit all the subcomponents of the process
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));
         while Present (S) loop
            Cs := Corresponding_Instance (S);
            Visit (Cs);
            S := AIN.Next_Node (S);
         end loop;
      end if;
   end Visit_Process_Instance;

   ----------------------------
   -- Visit_Device_Instance --
   ----------------------------

   procedure Visit_Device_Instance (E : Node_Id) is
      Interface_Connection_Gates_List : List_Id;
      Device_Gates_List : List_Id;
      Port_Gates_List : List_Id;
      Device_Instance : Node_Id;
      --  Aux_Device_Instance : Node_Id;
      L_Gates_Declaration : List_Id;
      --  needed to add device gates declaration in main process
      L_Ports : List_Id;
      --  needed to gather port node
      L_Stat : List_Id;
      S : Node_Id;
      Ni : Node_Id;
      P : Node_Id;

      N_Port : Node_Id;
      Aux_N_Port_1 : Node_Id;
      Aux_N_Port_2 : Node_Id;
      Aux_N_Port_3 : Node_Id;
      N_Send : Name_Id;
      Connection : Name_Id;

      Device_Identifier : constant Name_Id
        := New_Identifier (AIN.Display_Name
        (AIN.Identifier (E)), "Device_");
   begin
      Device_Gates_List := New_List;
      Interface_Connection_Gates_List := New_List;
      L_Ports := New_List;
      L_Gates_Declaration := New_List;

      if not AINU.Is_Empty (Features (E)) then
         S := AIN.First_Node (Features (E));
         loop
            if (AIN.Kind (S) = K_Port_Spec_Instance) then
               Connection := Make_Gate_Identifier (S);
               --  Connection Identifier
               N_Port := Make_Identifier (Connection);
               Aux_N_Port_1 := BLNu.Make_Node_Container (N_Port);
               Aux_N_Port_2 := BLNu.Make_Node_Container (N_Port);
               Aux_N_Port_3 := BLNu.Make_Node_Container (N_Port);

               BLNu.Append_Node_To_List (Make_Gate_Declaration
                (Make_Identifier ("LNT_Channel_Port"),
                 N_Port), L_Gates_Declaration);

               BLNu.Append_Node_To_List (Aux_N_Port_1, Device_Gates_List);

               if AIN.Is_In (S) then

                  N_SEND := Add_Prefix_To_Name ("SEND_",
                    Remove_Prefix_From_Name
                    ("RECEIVE_", Connection));

                  --  port
                  Port_Gates_List := New_List (
                          Make_Identifier (N_SEND),  -- SEND_
                          Aux_N_Port_2);  --  RECEIVE_

                  N_Port := Make_Process_Instantiation_Statement
                           (Make_Identifier ("Data_Port"),
                            Port_Gates_List,
                            No_List);

                  P := Make_Interface_Synchronisation (
                       Port_Gates_List,
                       New_List (N_Port)
                  );
                  BLNu.Append_Node_To_List (P, L_Ports);
                  BLNu.Append_Node_To_List (Make_Identifier (N_SEND),
                    Interface_Connection_Gates_List);

               elsif AIN.Is_Out (S) then
                  BLNu.Append_Node_To_List (Aux_N_Port_3,
                   Interface_Connection_Gates_List);
               end if;

            end if;
            S := AIN.Next_Node (S);
            exit when No (S);
         end loop;
      end if;
      Device_Instance :=
        Make_Process_Instantiation_Statement
        (Make_Identifier (Device_Identifier),
         Device_Gates_List,
         No_List);

      if not BLNu.Is_Empty (L_Ports) then
         BLNu.Append_Node_To_List (
           Make_Interface_Synchronisation (
             Device_Gates_List,
             New_List (Device_Instance)),
         L_Ports);

         L_Stat := New_List (
           Make_Parallel_Composition_Statement (
             No_List,
             L_Ports));
      else
         L_stat := New_List (Device_Instance);
      end if;

      BLNu.Append_List_To_List (L_Gates_Declaration,
        Main_Gate_Declaration_List);

      Ni := Make_Interface_Synchronisation (
        Interface_Connection_Gates_List, L_stat);
      BLNu.Append_Node_To_List (Ni, Main_Interface_Synchronisation_List);
   end Visit_Device_Instance;

   ----------------------------
   -- Visit_Thread_Instance --
   ----------------------------
   --  Add a Interface_Synchronisation node
   --  to Main_Interface_Synchronisation_List
   --  --  -- With port
   --  Interface_Connection_Gates_List ->
   --  par
   --     Thread_Gates_List ->
   --     Thread_* [Thread_Gates_List]
   --  ||
   --     Port_Gates_List ->
   --     Port_* [Port_Gates_List]
   --  end par
   --  --  -- Without port
   --  Interface_Connection_Gates_List ->
   --  Thread_* [Thread_Gates_List]

   procedure Visit_Thread_Instance (E : Node_Id) is
      Interface_Connection_Gates_List : List_Id;
      Thread_Gates_List : List_Id;
      Port_Gates_List : List_Id;
      Thread_Instance : Node_Id;
      Aux_Thread_Instance : Node_Id;
      L_Gates_Declaration : List_Id;
      --  needed to add thread gates declaration in main process
      L_Ports : List_Id;
      --  needed to gather port node
      L_Stat : List_Id;
      S : Node_Id;
      Ni : Node_Id;
      P : Node_Id;
      Event_Number : Natural := 0;
      Index : Natural;
      Queue_Size : Natural := 3;
      --  Overflow : Name_Id;
      N_Activation : Node_Id;
      Aux_N_Activation_1 : Node_Id;
      Aux_N_Activation_2 : Node_Id;
      N_Event_Declaration : Node_Id;
      Aux_N_Event_Declaration : Node_Id;
      N_Event : Node_Id;
      N_Event_Name : Name_Id;
      Aux_N_Event : Node_Id;
      Aux_N_Event_1 : Node_Id;
      Aux_N_Event_2 : Node_Id;
      N_Port : Node_Id;
      Aux_N_Port_1 : Node_Id;
      Aux_N_Port_2 : Node_Id;
      Aux_N_Port_3 : Node_Id;
      Aux_N_Port_4 : Node_Id;
      N_Send : Name_Id;
      Connection : Name_Id;
      Is_Not_Periodic : boolean := false;
      Thread_Identifier : constant Name_Id
        := New_Identifier (AIN.Display_Name
        (AIN.Identifier (E)), "Thread_");
   begin
      --  Thread_Instance : this thread instance
      --  Index : thread priority
      Find_Instance_By_Name (Thread_Identifier,
                             LNT_Thread_Instance_List,
                             Thread_Instance,
                             Index,
                             Is_Not_Periodic);

      N_Activation := New_Identifier (
           Remove_Prefix_From_Name (
           " ", Get_String_Name (Integer'Image (Index))),
           "ACTIVATION_");
      Aux_N_Activation_1 := BLNu.Make_Node_Container (N_Activation);
      Aux_N_Activation_2 := BLNu.Make_Node_Container (N_Activation);

      Thread_Gates_List := New_List (N_Activation);
      Interface_Connection_Gates_List := New_List (Aux_N_Activation_1);
      L_Ports := New_List;
      L_Gates_Declaration := New_List (
        Make_Gate_Declaration (
          Make_Identifier ("LNT_Channel_Dispatch"),
          Aux_N_Activation_2));
      if Is_Not_Periodic then
         N_Event_Name := New_Identifier (
              Remove_Prefix_From_Name (
              " ", Get_String_Name (Integer'Image (Index))),
              "INCOMING_EVENT_");
      end if;
      if not AINU.Is_Empty (Features (E)) then
         S := AIN.First_Node (Features (E));
         loop
            if (AIN.Kind (S) = K_Port_Spec_Instance) then
               Connection := Make_Gate_Identifier (S);
               --  Connection Identifier
               N_Port := Make_Identifier (Connection);
               Aux_N_Port_1 := BLNu.Make_Node_Container (N_Port);
               Aux_N_Port_2 := BLNu.Make_Node_Container (N_Port);
               Aux_N_Port_3 := BLNu.Make_Node_Container (N_Port);
               Aux_N_Port_4 := BLNu.Make_Node_Container (N_Port);

               BLNu.Append_Node_To_List (Make_Gate_Declaration
                (Make_Identifier ("LNT_Channel_Port"),
                 N_Port), L_Gates_Declaration);

               BLNu.Append_Node_To_List (Aux_N_Port_1, Thread_Gates_List);

               if AIN.Is_In (S) then
                  --  port properties
                  if (AIN.Is_Event (S)) and then
                     (Get_Queue_Size (S) /= -1)
                  then
                     Queue_Size := Natural (Get_Queue_Size (S));
                  else
                     Display_Located_Error (AIN.Loc (S),
                       "LNT generation requires the definition " &
                       "of Queue_Size property. " &
                       "For this generation, the default  value " &
                       "(Queue_Size = 3) is used.",
                     Fatal => false, Warning => true);
                  end if;
                  --  case Get_Overflow_Handling_Protocol (S) is
                  --   when Overflow_Handling_Protocol_DropOldest |
                  --      Overflow_Handling_Protocol_None =>
                  --      Overflow := Get_String_Name ("DropOldest");
                  --   when Overflow_Handling_Protocol_DropNewest =>
                  --      Overflow := Get_String_Name ("DropNewest");
                  --   when Overflow_Handling_Protocol_Error =>
                  --      Overflow := Get_String_Name ("Error");
                  --  end case;

                  N_SEND := Add_Prefix_To_Name ("SEND_",
                    Remove_Prefix_From_Name
                    ("RECEIVE_", Connection));
                  if ((AIN.Is_Data (S)) and then (not AIN.Is_Event (S))) then
                     --  data port
                     Port_Gates_List := New_List (
                          Make_Identifier (N_SEND), -- SEND_
                          Aux_N_Port_2 --  RECEIVE_
                          );

                     N_Port := Make_Process_Instantiation_Statement
                           (Make_Identifier ("Data_Port"),
                            Port_Gates_List,
                            No_List);
                  elsif AIN.Is_Event (S) then
                     --  event port
                     if Is_Not_Periodic then
                        if (Event_Number > 0) then
                           N_Event := New_Identifier (
                             Remove_Prefix_From_Name (
                             " ", Get_String_Name (
                             Integer'Image (Event_Number))),
                             Get_Name_String (N_Event_Name));
                        else
                           N_Event := Make_Identifier (N_Event_Name);
                        end if;
                        Aux_N_Event := BLNu.Make_Node_Container (N_Event);
                        Aux_N_Event_1 := BLNu.Make_Node_Container (N_Event);
                        Aux_N_Event_2 := BLNu.Make_Node_Container (N_Event);
                        --  add to the Processor event gate list
                        BLNu.Append_Node_To_List (N_Event,
                          Processor_Event_Gates_List);
                        --  for processor and main generation
                        N_Event_Declaration := Make_Gate_Declaration (
                           Make_Identifier ("LNT_Channel_Event"),
                           Aux_N_Event);
                        Aux_N_Event_Declaration :=
                          BLNu.Make_Node_Container (N_Event_Declaration);
                        BLNu.Append_Node_To_List (N_Event_Declaration,
                           Processor_Event_Gate_Declaration_List);
                        Event_Number := Event_Number + 1;
                        BLNu.Append_Node_To_List (Aux_N_Event_Declaration,
                         L_Gates_Declaration);

                        Port_Gates_List := New_List (
                          Make_Identifier (N_SEND), -- SEND_
                          Aux_N_Port_3,  --  RECEIVE_
                          Aux_N_Event_2);  --  INCOMING_EVENT_
                        N_Port := Make_Process_Instantiation_Statement
                           (Make_Identifier ("Event_Port"),
                            Port_Gates_List,
                            New_List (Make_Identifier (Integer'Image
                              (Queue_Size))));

                        BLNu.Append_Node_To_List (Aux_N_Event_1,
                          Interface_Connection_Gates_List);
                     else
                        Port_Gates_List := New_List (
                          Make_Identifier (N_SEND), -- SEND_
                          Aux_N_Port_3  --  RECEIVE_
                        );
                        N_Port := Make_Process_Instantiation_Statement
                           (Make_Identifier ("Event_Port_For_Periodic"),
                            Port_Gates_List,
                            New_List (Make_Identifier (Integer'Image
                              (Queue_Size))));

                     end if;
                  end if;
                  P := Make_Interface_Synchronisation (
                       Port_Gates_List,
                       New_List (N_Port)
                  );
                  BLNu.Append_Node_To_List (P, L_Ports);
                  BLNu.Append_Node_To_List (Make_Identifier (N_SEND),
                    Interface_Connection_Gates_List);

               elsif AIN.Is_Out (S) then
                  BLNu.Append_Node_To_List (Aux_N_Port_4,
                   Interface_Connection_Gates_List);
               end if;
            end if;
            S := AIN.Next_Node (S);
            exit when No (S);
         end loop;
      end if;
      Set_Actual_Gates (Thread_Instance, Thread_Gates_List);
      Aux_Thread_Instance := BLNu.Make_Node_Container (Thread_Instance);
      if not BLNu.Is_Empty (L_Ports) then
         BLNu.Append_Node_To_List (
           Make_Interface_Synchronisation (
             Thread_Gates_List,
             New_List (Aux_Thread_Instance)),
         L_Ports);

         L_Stat := New_List (
           Make_Parallel_Composition_Statement (
             No_List,
             L_Ports));
      else
         L_stat := New_List (Aux_Thread_Instance);
      end if;

      BLNu.Append_List_To_List (L_Gates_Declaration,
        Main_Gate_Declaration_List);

      Ni := Make_Interface_Synchronisation (
        Interface_Connection_Gates_List, L_stat);
      BLNu.Append_Node_To_List (Ni, Main_Interface_Synchronisation_List);
   end Visit_Thread_Instance;
   -------------------------
   --  Make_Process_Main  --
   -------------------------
   procedure Make_Process_Main is
      N : Node_Id;
      N_Activation : Node_Id;
      Aux_N_Activation : Node_Id;

   begin
      --  add processor
      for I in 1 .. Thread_Number loop
         N_Activation := New_Identifier (
           Remove_Prefix_From_Name (
           " ", Get_String_Name (Integer'Image (I))),
           "ACTIVATION_");
         Aux_N_Activation := BLNu.Make_Node_Container (N_Activation);
         BLNu.Append_Node_To_List (N_Activation, Processor_Gates_List);
         BLNu.Append_Node_To_List (
                           Make_Gate_Declaration (
                            Make_Identifier ("LNT_Channel_Dispatch"),
                            Aux_N_Activation),
                           LNT_Processor_Gate_Declaration_List);
      end loop;
      --  add  the set of Incoming_Event
      BLNu.Append_List_To_List (Processor_Event_Gates_List,
         Processor_Gates_List);
      --  for processor generation
      BLNu.Append_List_To_List (Processor_Event_Gate_Declaration_List,
         LNT_Processor_Gate_Declaration_List);
      BLNu.Append_Node_To_List (
           Make_Interface_Synchronisation (
             Processor_Gates_List,
             New_List (Make_Process_Instantiation_Statement (
                            Make_Identifier ("Processor"),
                            Processor_Gates_List, No_List))),
           Main_Interface_Synchronisation_List);
      Set_Process_Gate_Declarations (BLN.Item (The_Processor),
        LNT_Processor_Gate_Declaration_List);
      --  add threads

      N := Make_Process_Definition
       (No_Node, Make_Identifier (Get_String_Name ("Main")),
        No_List,
        Main_Gate_Declaration_List,
        No_List,
        No_List,
        New_List (
         Make_Parallel_Composition_Statement (No_List,
           Main_Interface_Synchronisation_List))
        );
      BLNu.Append_Node_To_List (N, Definitions_List);
   end Make_Process_Main;

   procedure Find_Instance_By_Name (Key : Name_Id; Target : List_Id;
                       Instance : out Node_Id;
                       Index : out Natural;
                       Is_Not_Periodic : out boolean)
   is
      N  : Node_Id;
      Counter : Natural := 1;
   begin
      if BLNu.Is_Empty (Target) then
         Instance := No_Node;
      else
         N := BLN.First_Node (Target);

         while Present (N) loop
            if ((BLN.Name (BLN.Identifier (N)) = Key and then
               BLNu.Is_Empty (Actual_Gates (N))))
            then
               Is_Not_Periodic := BLN.Is_Not_Periodic (N);
               Instance := N;
               Index := Counter;
               exit;
            end if;
            N := BLN.Next_Node (N);
            Counter := Counter + 1;
         end loop;
      end if;
   end Find_Instance_By_Name;

end Ocarina.Backends.LNT.Tree_Generator_Main;
