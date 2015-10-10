------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . M A S T . M A I N            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2015 ESA & ISAE.                    --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.MAST_Tree.Nodes;
with Ocarina.Backends.MAST_Tree.Nutils;
with Ocarina.Backends.MAST_Values;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.MAST.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.MAST_Tree.Nutils;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.MAST_Values;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package MTN renames Ocarina.Backends.MAST_Tree.Nodes;
   package MTU renames Ocarina.Backends.MAST_Tree.Nutils;

   procedure Visit_Component (E : Node_Id);
   procedure Visit_System (E : Node_Id);
   procedure Visit_Processor (E : Node_Id);
   procedure Visit_Process (E : Node_Id);
   procedure Visit_Thread (E : Node_Id);
   procedure Visit_Subprogram (E : Node_Id);
   procedure Visit_Bus (E : Node_Id);
   procedure Visit_Data (E : Node_Id);
   procedure Visit_Device (E : Node_Id);
   procedure Visit_Virtual_Processor (E : Node_Id);

   function Map_Port_Operation_Name
     (The_Thread : Node_Id;
      The_Port   : Node_Id) return Name_Id;
   function Map_Driver_Receive_Operation_Name
     (The_Device : Node_Id) return Name_Id;
   function Map_Driver_Send_Operation_Name
     (The_Device : Node_Id) return Name_Id;
   function Map_Driver_Scheduling_Server_Name
     (The_Device : Node_Id) return Name_Id;
   function Map_Scheduler_Name (The_Processor : Node_Id) return Name_Id;
   function Map_Port_Shared_Resource_Name (The_Port : Node_Id) return Name_Id;
   function Map_Port_Shared_Resource_Operation_Name
     (The_Port : Node_Id) return Name_Id;

   function Make_Driver_Wrapper (The_Device : Node_Id) return Node_Id;
   --  Return No_Node if the corresponding device has no impact on
   --  scheduling, i.e. no attached driver or other properties.

   function Map_Operation_Message_Transmission_Name
     (The_Data : Node_Id) return Name_Id;

   Root_System_Node : Node_Id := No_Node;

   ------------------------
   -- Map_Scheduler_Name --
   ------------------------

   function Map_Scheduler_Name (The_Processor : Node_Id) return Name_Id is
      N : Name_Id;
   begin
      Get_Name_String
        (Normalize_Name
           (Name (Identifier (Parent_Subcomponent (The_Processor)))));
      Add_Str_To_Name_Buffer ("_scheduler");
      N := Name_Find;
      return N;
   end Map_Scheduler_Name;

   ---------------------------------------------
   -- Map_Operation_Message_Transmission_Name --
   ---------------------------------------------

   function Map_Operation_Message_Transmission_Name
     (The_Data : Node_Id) return Name_Id
   is
      N : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Normalize_Name (Name (Identifier (The_Data))));
      Add_Str_To_Name_Buffer ("_transmission_operation");
      N := Name_Find;
      return N;
   end Map_Operation_Message_Transmission_Name;

   -----------------------------------
   -- Map_Port_Shared_Resource_Name --
   -----------------------------------

   function Map_Port_Shared_Resource_Name
     (The_Port : Node_Id) return Name_Id
   is
      Component_Instance_Name : Name_Id;
      Port_Name               : Name_Id;
      N                       : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Component_Instance_Name :=
        Fully_Qualified_Instance_Name (AIN.Parent_Component (The_Port));

      Set_Str_To_Name_Buffer ("");
      Port_Name := (Normalize_Name (Name (Identifier (The_Port))));
      Set_Str_To_Name_Buffer ("");

      Set_Str_To_Name_Buffer
        (Get_Name_String (Component_Instance_Name) &
         "_" &
         Get_Name_String (Port_Name) &
         "_shared_resource");

      N := Name_Find;
      return N;
   end Map_Port_Shared_Resource_Name;

   ---------------------------------------------
   -- Map_Port_Shared_Resource_Operation_Name --
   ---------------------------------------------

   function Map_Port_Shared_Resource_Operation_Name
     (The_Port : Node_Id) return Name_Id
   is
      Component_Instance_Name : Name_Id;
      Port_Name               : Name_Id;
      N                       : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Component_Instance_Name :=
        Fully_Qualified_Instance_Name (AIN.Parent_Component (The_Port));

      Set_Str_To_Name_Buffer ("");
      Port_Name := (Normalize_Name (Name (Identifier (The_Port))));
      Set_Str_To_Name_Buffer ("");

      Set_Str_To_Name_Buffer
        (Get_Name_String (Component_Instance_Name) &
         "_" &
         Get_Name_String (Port_Name) &
         "_shared_resource_operation");

      N := Name_Find;
      return N;
   end Map_Port_Shared_Resource_Operation_Name;

   -----------------------------
   -- Map_Port_Operation_Name --
   -----------------------------

   function Map_Port_Operation_Name
     (The_Thread : Node_Id;
      The_Port   : Node_Id) return Name_Id
   is
      Thread_Name : Name_Id;
      Port_Name   : Name_Id;
      N           : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Thread_Name :=
        (Normalize_Name
           (Name (Identifier (Parent_Subcomponent (The_Thread)))));
      Port_Name := (Normalize_Name (Name (Identifier (The_Port))));
      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Port_Name);
      Add_Str_To_Name_Buffer ("_port_");
      Get_Name_String_And_Append (Thread_Name);
      N := Name_Find;
      return N;
   end Map_Port_Operation_Name;

   ------------------------------------
   -- Map_Driver_Send_Operation_Name --
   ------------------------------------

   function Map_Driver_Send_Operation_Name
     (The_Device : Node_Id) return Name_Id
   is
      N : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String
        (Normalize_Name
           (Name (Identifier (Parent_Subcomponent (The_Device)))));
      Add_Str_To_Name_Buffer ("_operation_send");
      N := Name_Find;
      return N;
   end Map_Driver_Send_Operation_Name;

   ---------------------------------------
   -- Map_Driver_Receive_Operation_Name --
   ---------------------------------------

   function Map_Driver_Receive_Operation_Name
     (The_Device : Node_Id) return Name_Id
   is
      N : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String
        (Normalize_Name
           (Name (Identifier (Parent_Subcomponent (The_Device)))));
      Add_Str_To_Name_Buffer ("_operation_receive");
      N := Name_Find;
      return N;
   end Map_Driver_Receive_Operation_Name;

   ---------------------------------------
   -- Map_Driver_Scheduling_Server_Name --
   ---------------------------------------

   function Map_Driver_Scheduling_Server_Name
     (The_Device : Node_Id) return Name_Id
   is
      N : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String
        (Normalize_Name
           (Name (Identifier (Parent_Subcomponent (The_Device)))));
      Add_Str_To_Name_Buffer ("_scheduling_server");
      N := Name_Find;
      return N;
   end Map_Driver_Scheduling_Server_Name;

   -------------------------
   -- Make_Driver_Wrapper --
   -------------------------

   function Make_Driver_Wrapper (The_Device : Node_Id) return Node_Id is
      M       : Node_Id;
      New_Drv : Node_Id;
   begin
      M :=
        Make_Scheduling_Server
          (Map_Driver_Scheduling_Server_Name (The_Device),
           No_Name);
      if Present (Get_Bound_Processor (The_Device)) then
         MTN.Set_Associated_Scheduler
           (M,
            Map_Scheduler_Name (Get_Bound_Processor (The_Device)));
      else
         --  This device is not bound to a processor, cannot impact
         --  scheduling.
         MTN.Set_Associated_Scheduler (M, No_Name);
         return No_Node;
      end if;

      MTN.Set_Parameters
        (M,
         Make_Scheduling_Server_Parameters (Fixed_Priority, 1));

      MTU.Append_Node_To_List (M, MTN.Declarations (MAST_File));

      M :=
        Make_Operation
          (Map_Driver_Send_Operation_Name (The_Device),
           Simple,
           No_List);
      MTU.Append_Node_To_List (M, MTN.Declarations (MAST_File));

      M :=
        Make_Operation
          (Map_Driver_Receive_Operation_Name (The_Device),
           Simple,
           No_List);
      MTU.Append_Node_To_List (M, MTN.Declarations (MAST_File));

      New_Drv :=
        MTU.Make_Driver
          (Normalize_Name
             (Name (Identifier (Parent_Subcomponent (The_Device)))),
           Packet,
           Map_Driver_Scheduling_Server_Name (The_Device),
           Map_Driver_Send_Operation_Name (The_Device),
           Map_Driver_Receive_Operation_Name (The_Device),
           False,
           Coupled);
      return New_Drv;
   end Make_Driver_Wrapper;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Root_System_Node := Root_System (E);
            Visit (Root_System_Node);

         when K_Component_Instance =>
            Visit_Component (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------
   -- Visit_Component --
   ---------------------

   procedure Visit_Component (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System (E);

         when CC_Processor =>
            Visit_Processor (E);

         when CC_Subprogram =>
            Visit_Subprogram (E);

         when CC_Process =>
            Visit_Process (E);

         when CC_Data =>
            Visit_Data (E);

         when CC_Device =>
            Visit_Device (E);

         when CC_Thread =>
            Visit_Thread (E);

         when CC_Bus =>
            Visit_Bus (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor (E);

         when others =>
            null;
      end case;
   end Visit_Component;

   ---------------
   -- Visit_Bus --
   ---------------

   procedure Visit_Bus (E : Node_Id) is
      N                : Node_Id;
      C                : Node_Id;
      C_Src            : Node_Id;
      C_Dst            : Node_Id;
      Src_Component    : Node_Id;
      Dst_Component    : Node_Id;
      Driver_Component : Node_Id;

   begin
      N :=
        MTU.Make_Processing_Resource
          (Normalize_Name (Name (Identifier (Parent_Subcomponent (E)))),
           PR_Packet_Based_Network);

      if not AINU.Is_Empty (Connections (Root_System_Node)) then
         C := First_Node (Connections (Root_System_Node));
         while Present (C) loop
            C_Src := Source (C);
            if Kind (C_Src) = K_Entity_Reference_Instance then
               Src_Component := Get_Referenced_Entity (C_Src);
            end if;
            C_Dst := Get_Referenced_Entity (Destination (C));

            if C_Src /= No_Node and then C_Dst /= No_Node then
               Dst_Component := Parent_Subcomponent (Parent_Component (C_Dst));

               if Src_Component = Parent_Subcomponent (E)
                 and then Get_Category_Of_Component (Dst_Component) = CC_Device
               then
                  Driver_Component :=
                    Make_Driver_Wrapper
                      (Corresponding_Instance (Dst_Component));
                  if Present (Driver_Component) then
                     Append_Node_To_List
                       (Driver_Component,
                        MTN.List_Of_Drivers (N));
                  end if;
               end if;
            end if;

            C := Next_Node (C);
         end loop;
      end if;

      MTN.Set_Is_Full_Duplex (N, True);
      MTU.Append_Node_To_List (N, MTN.Declarations (MAST_File));
   end Visit_Bus;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Virtual_Processor;

   ------------------
   -- Visit_Device --
   ------------------

   procedure Visit_Device (E : Node_Id) is
      S : Node_Id;
      N : Node_Id;
   begin
      N := Make_Driver_Wrapper (E);
      if Present (N) then
         --  Here, we consider (for scheduling) only devices that lead
         --  to generated MAST stuff
         MTU.Append_Node_To_List (N, MTN.Declarations (MAST_File));
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Device;

   ---------------------
   -- Visit_Processor --
   ---------------------

   procedure Visit_Processor (E : Node_Id) is
      S : Node_Id;
      N : Node_Id;
   begin
      N :=
        MTU.Make_Processing_Resource
          (Normalize_Name (Name (Identifier (Parent_Subcomponent (E)))),
           PR_Regular_Processor);

      MTU.Append_Node_To_List (N, MTN.Declarations (MAST_File));

      N :=
        MTU.Make_Scheduler
          (Map_Scheduler_Name (E),
           Normalize_Name (Name (Identifier (Parent_Subcomponent (E)))));
      MTN.Set_Is_Primary_Scheduler (N, True);
      MTN.Set_Use_Fixed_Priority (N, True);
      MTN.Set_Min_Priority (N, Make_Literal (New_Numeric_Value (1, 1, 10)));
      MTN.Set_Max_Priority (N, Make_Literal (New_Numeric_Value (256, 1, 10)));

      MTU.Append_Node_To_List (N, MTN.Declarations (MAST_File));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Processor;

   -------------------
   -- Visit_Process --
   -------------------

   procedure Visit_Process (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Process;

   ----------------
   -- Visit_Data --
   -------------------

   procedure Visit_Data (E : Node_Id) is
      S  : Node_Id;
      N  : Node_Id;
      CP : constant Supported_Concurrency_Control_Protocol :=
        Get_Concurrency_Protocol (E);
   begin
      N :=
        Make_Operation
          (Map_Operation_Message_Transmission_Name (E),
           Message_Transmission,
           No_List);
      if Get_Data_Size (E) /= Null_Size then
         MTN.Set_Max_Message_Size
           (N,
            Make_Literal
              (New_Numeric_Value (To_Bytes (Get_Data_Size (E)), 1, 10)));

         MTN.Set_Avg_Message_Size
           (N,
            Make_Literal
              (New_Numeric_Value (To_Bytes (Get_Data_Size (E)), 1, 10)));

         MTN.Set_Min_Message_Size
           (N,
            Make_Literal
              (New_Numeric_Value (To_Bytes (Get_Data_Size (E)), 1, 10)));
      end if;
      Append_Node_To_List (N, MTN.Declarations (MAST_File));

      if CP = Priority_Ceiling
        or else Is_Protected_Data (E)
        or else Get_Data_Representation (E) = Data_With_Accessors
      then
         N :=
           Make_Shared_Resource
             (Immediate_Ceiling,
              Normalize_Name (Name (Identifier (E))));
         Append_Node_To_List (N, MTN.Declarations (MAST_File));
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Data;

   ------------------
   -- Visit_Thread --
   ------------------

   procedure Visit_Thread (E : Node_Id) is
      S                            : Node_Id;
      N                            : Node_Id;
      Call                         : Node_Id;
      Spg_Call                     : Node_Id;
      Spg                          : Node_Id;
      Activation_Event             : Node_Id;
      Activation_Kind              : Event_Kind          := Regular;
      Activation_Event_Name        : Name_Id;
      Output_Event                 : Node_Id;
      Output_Event_Name            : Name_Id;
      Event_Handler                : Node_Id;
      Server_Parameters            : Node_Id;
      Server_Sched_Name            : Name_Id;
      Operation_Name               : Name_Id;
      Operation                    : Node_Id;
      Operations_List : constant List_Id    := MTU.New_List (MTN.K_List_Id);
      Output_Event_Req             : Node_Id             := No_Node;
      Prio                         : Unsigned_Long_Long;
      Exec_Time : constant Time_Array := Get_Execution_Time (E);
      The_Feature                  : Node_Id;
      Port_Operation               : Node_Id;
      Port_Shared_Resource         : Node_Id;
      Port_Shared_Resource_Op      : Node_Id;
      Port_Shared_Resource_Op_List : List_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Fully_Qualified_Instance_Name (E));
      Add_Str_To_Name_Buffer ("_sched_server");
      Server_Sched_Name := Name_Find;

      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Fully_Qualified_Instance_Name (E));
      Add_Str_To_Name_Buffer ("_operations");
      Operation_Name := Name_Find;

      Prio := Get_Thread_Priority (E);
      if Prio = 0 then
         Prio := 1;
      end if;
      Server_Parameters :=
        Make_Scheduling_Server_Parameters (Fixed_Priority, Prio);

      N := Make_Scheduling_Server (Server_Sched_Name, No_Name);
      MTN.Set_Associated_Scheduler
        (N,
         Map_Scheduler_Name
           (Get_Bound_Processor (Parent_Component (Parent_Subcomponent (E)))));

      MTN.Set_Parameters (N, Server_Parameters);

      Append_Node_To_List (N, MTN.Declarations (MAST_File));

      N := Make_Transaction (Fully_Qualified_Instance_Name (E), Regular);
      Append_Node_To_List (N, MTN.Declarations (MAST_File));

      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Fully_Qualified_Instance_Name (E));
      Add_Str_To_Name_Buffer ("_activation_event");
      Activation_Event_Name := Name_Find;

      if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic then
         Activation_Kind := Periodic;

         Output_Event_Req :=
           Make_Event_Timing_Requirement
             (Hard_Deadline,
              To_Milliseconds (Get_Thread_Deadline (E)),
              Activation_Event_Name);
      elsif Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic then
         Activation_Kind := Sporadic;

         Output_Event_Req :=
           Make_Event_Timing_Requirement
             (Hard_Deadline,
              To_Milliseconds (Get_Thread_Deadline (E)),
              Activation_Event_Name);

      else
         Activation_Kind := Regular;
      end if;

      Activation_Event := Make_Event (Activation_Event_Name, Activation_Kind);

      if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic then
         MTN.Set_Period
           (Activation_Event,
            Make_Literal
              (New_Numeric_Value
                 (To_Milliseconds (Get_Thread_Period (E)),
                  1,
                  10)));
      elsif Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic then
         MTN.Set_Min_Interarrival
           (Activation_Event,
            Make_Literal
              (New_Numeric_Value
                 (To_Milliseconds (Get_Thread_Period (E)),
                  1,
                  10)));
      else
         MTN.Set_Period (Activation_Event, No_Node);
      end if;

      Append_Node_To_List (Activation_Event, MTN.External_Events (N));

      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Fully_Qualified_Instance_Name (E));
      Add_Str_To_Name_Buffer ("_output_event");
      Output_Event_Name := Name_Find;

      Output_Event := Make_Event (Output_Event_Name, Regular);

      MTN.Set_Timing_Requirements (Output_Event, Output_Event_Req);

      Append_Node_To_List (Output_Event, MTN.Internal_Events (N));

      Event_Handler :=
        Make_Event_Handler
          (Activity,
           Activation_Event_Name,
           Output_Event_Name,
           Operation_Name,
           Server_Sched_Name);

      Append_Node_To_List (Event_Handler, MTN.Event_Handlers (N));

      Operation := Make_Operation (Operation_Name, Enclosing);
      MTN.Set_Operations (Operation, Operations_List);

      if Exec_Time /= Empty_Time_Array then
         MTN.Set_Best_Case_Execution_Time
           (Operation,
            Make_Literal
              (New_Numeric_Value (To_Milliseconds (Exec_Time (0)), 1, 10)));
         MTN.Set_Worst_Case_Execution_Time
           (Operation,
            Make_Literal
              (New_Numeric_Value (To_Milliseconds (Exec_Time (1)), 1, 10)));
      else
         MTN.Set_Best_Case_Execution_Time
           (Operation,
            Make_Literal (New_Numeric_Value (0, 1, 10)));
         MTN.Set_Worst_Case_Execution_Time
           (Operation,
            Make_Literal (New_Numeric_Value (1, 1, 10)));
      end if;

      Append_Node_To_List (Operation, MTN.Declarations (MAST_File));

      if Has_Ports (E) then
         The_Feature := First_Node (Features (E));

         while Present (The_Feature) loop
            if Kind (The_Feature) = K_Port_Spec_Instance
              and then Is_In (The_Feature)
            then
               Append_Node_To_List
                 (Make_Defining_Identifier
                    (Map_Port_Shared_Resource_Operation_Name (The_Feature)),
                  Operations_List);

               Port_Shared_Resource :=
                 Make_Shared_Resource
                   (Immediate_Ceiling,
                    Map_Port_Shared_Resource_Name (The_Feature));
               Append_Node_To_List
                 (Port_Shared_Resource,
                  MTN.Declarations (MAST_File));

               Port_Shared_Resource_Op_List := New_List (MTN.K_List_Id);

               Append_Node_To_List
                 (Make_Defining_Identifier
                    (Map_Port_Shared_Resource_Name (The_Feature)),
                  Port_Shared_Resource_Op_List);

               Port_Shared_Resource_Op :=
                 Make_Operation
                   (Map_Port_Shared_Resource_Operation_Name (The_Feature),
                    Simple,
                    No_List);
               MTN.Set_Shared_Resources_List
                 (Port_Shared_Resource_Op,
                  Port_Shared_Resource_Op_List);

               MTN.Set_Best_Case_Execution_Time
                 (Port_Shared_Resource_Op,
                  Make_Literal (New_Numeric_Value (1, 1, 10)));
               MTN.Set_Worst_Case_Execution_Time
                 (Port_Shared_Resource_Op,
                  Make_Literal (New_Numeric_Value (10, 1, 10)));

               Append_Node_To_List
                 (Port_Shared_Resource_Op,
                  MTN.Declarations (MAST_File));

            end if;
            The_Feature := Next_Node (The_Feature);
         end loop;
      end if;

      if not AINU.Is_Empty (Calls (E)) then
         Call := AIN.First_Node (Calls (E));
         while Present (Call) loop

            if not AINU.Is_Empty (Subprogram_Calls (Call)) then
               Spg_Call := AIN.First_Node (AIN.Subprogram_Calls (Call));
               while Present (Spg_Call) loop

                  Spg := AIN.Corresponding_Instance (Spg_Call);
                  Append_Node_To_List
                    (Make_Defining_Identifier (Name (Identifier (Spg))),
                     Operations_List);
                  Visit (Spg);
                  Spg_Call := AIN.Next_Node (Spg_Call);
               end loop;
            end if;
            Call := AIN.Next_Node (Call);
         end loop;
      end if;

      if Has_Ports (E) then
         The_Feature := First_Node (Features (E));

         while Present (The_Feature) loop
            if Kind (The_Feature) = K_Port_Spec_Instance
              and then Is_Out (The_Feature)
            then
               declare
                  Dest_Ports : constant List_Id :=
                    Get_Destination_Ports (The_Feature);
                  Dest_Port : Node_Id;
               begin
                  if not AINU.Is_Empty (Dest_Ports) then
                     Dest_Port := AIN.First_Node (Dest_Ports);
                     while Present (Dest_Port) loop
                        if Get_Category_Of_Component
                            (Corresponding_Instance
                               (Parent_Subcomponent
                                  (Parent_Component (Item (Dest_Port))))) /=
                          CC_Device
                        then
                           --  XXX should also consider device driver

                           Append_Node_To_List
                             (Make_Defining_Identifier
                                (Map_Port_Shared_Resource_Operation_Name
                                   (Item (Dest_Port))),
                              Operations_List);
                        end if;
                        Dest_Port := AIN.Next_Node (Dest_Port);
                     end loop;
                  end if;
               end;
            end if;
            The_Feature := Next_Node (The_Feature);
         end loop;
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      if Has_Ports (E) then
         The_Feature := First_Node (Features (E));

         while Present (The_Feature) loop
            if Kind (The_Feature) = K_Port_Spec_Instance then
               Port_Operation :=
                 Make_Operation
                   (Map_Port_Operation_Name (E, The_Feature),
                    Simple);
               MTN.Set_Operations (Port_Operation, New_List (MTN.K_List_Id));

               MTN.Set_Best_Case_Execution_Time
                 (Port_Operation,
                  Make_Literal (New_Numeric_Value (1, 1, 10)));
               MTN.Set_Worst_Case_Execution_Time
                 (Port_Operation,
                  Make_Literal (New_Numeric_Value (2, 1, 10)));

               Append_Node_To_List
                 (Port_Operation,
                  MTN.Declarations (MAST_File));
            end if;
            The_Feature := Next_Node (The_Feature);
         end loop;
      end if;

   end Visit_Thread;

   ----------------------
   -- Visit_Subprogram --
   ----------------------

   procedure Visit_Subprogram (E : Node_Id) is
      Operation       : Node_Id;
      Operation_Name  : Name_Id;
      Operations_List : constant List_Id := MTU.New_List (MTN.K_List_Id);
   begin
      Operation_Name := Name (Identifier (E));
      Operation      := Make_Operation (Operation_Name, Simple);
      MTN.Set_Operations (Operation, Operations_List);

      Append_Node_To_List (Operation, MTN.Declarations (MAST_File));
   end Visit_Subprogram;

   ------------------
   -- Visit_System --
   ------------------

   procedure Visit_System (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if Get_Category_Of_Component (Corresponding_Instance (S)) /=
              CC_Device
            then
               Visit (Corresponding_Instance (S));
            end if;

            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System;
end Ocarina.Backends.MAST.Main;
