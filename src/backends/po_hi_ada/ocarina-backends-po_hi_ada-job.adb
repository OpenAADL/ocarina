------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . P O _ H I _ A D A . J O B        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2019 ESA & ISAE.                       --
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

with Ocarina.Namet;
with Locations;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Values;
with Ocarina.Backends.PO_HI_Ada.Mapping;
with Ocarina.Backends.PO_HI_Ada.Runtime;

package body Ocarina.Backends.PO_HI_Ada.Job is

   use Ocarina.Namet;
   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Ada_Values;
   use Ocarina.Backends.PO_HI_Ada.Mapping;
   use Ocarina.Backends.PO_HI_Ada.Runtime;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   function Get_Fully_Qualified_Subprogram (S : Name_Id) return Node_Id;
   --  Return an identifier to S whose parent unit name is the
   --  instantiated package correspodning to the interface of E.

   ------------------------------------
   -- Get_Fully_Qualified_Subprogram --
   ------------------------------------

   function Get_Fully_Qualified_Subprogram
     (S : Name_Id) return Node_Id
   is
      P : constant Node_Id :=
        RU (RU_PolyORB_HI_Generated_Activity);
      N : constant Node_Id := Make_Defining_Identifier (S);
   begin
      Set_Homogeneous_Parent_Unit_Name (N, P);

      return N;
   end Get_Fully_Qualified_Subprogram;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      procedure Cyclic_Task_Instantiation_Formals
        (E      : Node_Id;
         P_List : List_Id);
      --  Appends the formal generic parameter association which are
      --  common between periodic and sporadic tasks.

      function Periodic_Task_Instantiation (E : Node_Id) return Node_Id;
      --  Build a package instantiation for a periodic task

      function Sporadic_Task_Instantiation (E : Node_Id) return Node_Id;
      --  Build a package instantiation for a sporadic task

      function Aperiodic_Task_Instantiation (E : Node_Id) return Node_Id;
      --  Build a package instantiation for an aperiodic task

      function Hybrid_Task_Instantiation (E : Node_Id) return Node_Id;
      --  Build a package instantiation for a hybrid task

      function Background_Task_Instantiation (E : Node_Id) return Node_Id;
      --  Build a package instantiation for a background task

      function Null_Task_Instantiation (E : Node_Id) return Node_Id;
      --  Build a package instantiation for a null task

      function ISR_Task_Instantiation (E : Node_Id) return Node_Id;
      --  Build a package instantiation for a background task

      function Task_Job_Spec (E : Node_Id) return Node_Id;
      --  Creates the parameterless subprogram specification that does
      --  the thread's job.

      function Make_Mode_Updater_Spec (E : Node_Id) return Node_Id;
      --  Create the procedure which will update the current mode

      function Make_Modes_Enumeration (E : Node_Id) return Node_Id;
      --  Create the mode enumeration

      ---------------------------------------
      -- Cyclic_Task_Instantiation_Formals --
      ---------------------------------------

      procedure Cyclic_Task_Instantiation_Formals
        (E      : Node_Id;
         P_List : List_Id)
      is
         N : Node_Id;
         I : Unsigned_Long_Long;
         T : Time_Type;

      begin
         --  The entity name

         N :=
           Make_Parameter_Association
             (Selector_Name    => Make_Defining_Identifier (PN (P_Entity)),
              Actual_Parameter => Extract_Enumerator (E));
         Append_Node_To_List (N, P_List);

         if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic
           or else Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic
           or else Get_Thread_Dispatch_Protocol (E) = Thread_Hybrid
           or else Get_Thread_Dispatch_Protocol (E) = Thread_ISR
         then
            --  The task period of minimal interarrival time

            N :=
              Make_Parameter_Association
                (Selector_Name =>
                   Make_Defining_Identifier (PN (P_Task_Period)),
                 Actual_Parameter => Map_Ada_Time (Get_Thread_Period (E)));
            Append_Node_To_List (N, P_List);

            --  The task deadline

            N := Map_Ada_Time (Get_Thread_Deadline (E));

            N :=
              Make_Parameter_Association
                (Selector_Name =>
                   Make_Defining_Identifier (PN (P_Task_Deadline)),
                 Actual_Parameter => N);
            Append_Node_To_List (N, P_List);
         end if;

         if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic then
            --  The dispatch offset
            T := Get_Dispatch_Offset (E);

            if T /= Null_Time then
               N :=
                 Make_Parameter_Association
                   (Selector_Name =>
                      Make_Defining_Identifier (PN (P_Dispatch_Offset)),
                    Actual_Parameter => Map_Ada_Time (T));
               Append_Node_To_List (N, P_List);
            end if;
         end if;

         --  The task priority, if the thread has no priority, we
         --  assign a default one.

         I := Get_Thread_Priority (E);

         if I = 0 then
            N := RE (RE_Default_Priority);
         else
            N := Map_Ada_Priority (I);
         end if;

         N :=
           Make_Parameter_Association
             (Selector_Name => Make_Defining_Identifier (PN (P_Task_Priority)),
              Actual_Parameter => N);
         Append_Node_To_List (N, P_List);

         --  The task stack size, if the thread has no stack size, we
         --  assign a default one.

         I := To_Bytes (Get_Thread_Stack_Size (E));

         if I = 0 then
            --  The default stack size is 100 Kb

            N := Make_Literal (New_Integer_Value (100_000, 1, 10));
         else
            N := Make_Literal (New_Integer_Value (I, 1, 10));
         end if;

         N :=
           Make_Parameter_Association
             (Selector_Name =>
                Make_Defining_Identifier (PN (P_Task_Stack_Size)),
              Actual_Parameter => N);
         Append_Node_To_List (N, P_List);

         --  The task job

         N :=
           Make_Parameter_Association
             (Selector_Name    => Make_Defining_Identifier (PN (P_Job)),
              Actual_Parameter => Map_Task_Job_Identifier (E));
         Append_Node_To_List (N, P_List);

         --  If an activate entrypoint has been specified for the
         --  thread, add an additional parameter association

         declare
            Activate_Entrypoint : constant Name_Id :=
              Get_Thread_Activate_Entrypoint (E);
         begin
            if Activate_Entrypoint /= No_Name then
               --  We cannot use direcly the activate entrypoint
               --  because the Activity spec must not depend on user
               --  package specs (to avoid introducing elaboration
               --  cycles). We use subprogram renaming as workaround.

               N :=
                 Make_Subprogram_Specification
                   (Defining_Identifier => Map_Task_Init_Identifier (E),
                    Parameter_Profile   => No_List,
                    Return_Type         => No_Node);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               N :=
                 Make_Parameter_Association
                   (Selector_Name =>
                      Make_Defining_Identifier (PN (P_Activate_Entrypoint)),
                    Actual_Parameter => Map_Task_Init_Identifier (E));
               Append_Node_To_List (N, P_List);
            end if;
         end;

         --  If a recover entrypoint has been specified for the
         --  thread, add an additional parameter association

         declare
            Rec_Entrypoint : constant Name_Id :=
              Get_Thread_Recover_Entrypoint (E);
         begin
            if Rec_Entrypoint /= No_Name then
               --  We cannot use direcly the recover entrypoint
               --  because the Activity spec must not depend on user
               --  package specs (to avoid introducing elaboration
               --  cycles). We use subprogram renaminng as workaround.

               N :=
                 Make_Subprogram_Specification
                   (Defining_Identifier => Map_Task_Recover_Identifier (E),
                    Parameter_Profile   => No_List,
                    Return_Type         => No_Node);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               N :=
                 Make_Parameter_Association
                   (Selector_Name =>
                      Make_Defining_Identifier (PN (P_Recover_Entrypoint)),
                    Actual_Parameter => Map_Task_Recover_Identifier (E));
               Append_Node_To_List (N, P_List);
            end if;
         end;
      end Cyclic_Task_Instantiation_Formals;

      ---------------------------------
      -- Periodic_Task_Instantiation --
      ---------------------------------

      function Periodic_Task_Instantiation (E : Node_Id) return Node_Id is
         N              : Node_Id;
         Parameter_List : constant List_Id :=
           New_List (ADN.K_Parameter_Profile);
      begin
         --  Build the common parameters

         Cyclic_Task_Instantiation_Formals (E, Parameter_List);

         --  Build the package instantiation

         N :=
           Make_Package_Instantiation
             (Defining_Identifier => Map_Task_Identifier (E),
              Generic_Package     =>
                RU (RU_PolyORB_HI_Periodic_Task, Elaborated => True),
              Parameter_List => Parameter_List);
         return N;
      end Periodic_Task_Instantiation;

      ---------------------------------
      -- Sporadic_Task_Instantiation --
      ---------------------------------

      function Sporadic_Task_Instantiation (E : Node_Id) return Node_Id is
         N              : Node_Id;
         Parameter_List : constant List_Id := New_List (ADN.K_List_Id);
      begin
         --  Port_Type

         N :=
           Make_Parameter_Association
             (Selector_Name    => Make_Defining_Identifier (TN (T_Port_Type)),
              Actual_Parameter =>
                Make_Defining_Identifier (Map_Port_Enumeration_Name (E)));
         Append_Node_To_List (N, Parameter_List);

         --  Raise an error if the thread does not have IN ports

         if not Has_In_Ports (E) then
            Display_Located_Error
              (Loc (E),
               "This sporadic thread does not have IN ports",
               Fatal => True);
         end if;

         --  Raise an error if the thread does not have 'in event'
         --  ports.

         if not Has_In_Event_Ports (E) then
            Display_Located_Error
              (Loc (E),
               "None of the IN ports of this sporadic thread is an event port",
               Fatal => True);
         end if;

         --  Append the common parameters

         Cyclic_Task_Instantiation_Formals (E, Parameter_List);

         --  The blocking routine

         N :=
           Make_Parameter_Association
             (Selector_Name =>
                Make_Defining_Identifier (SN (S_Wait_For_Incoming_Events)),
              Actual_Parameter =>
                Get_Fully_Qualified_Subprogram
                  (SN (S_Wait_For_Incoming_Events)));
         Append_Node_To_List (N, Parameter_List);

         --  Build the package instantiation

         N :=
           Make_Package_Instantiation
             (Defining_Identifier => Map_Task_Identifier (E),
              Generic_Package     =>
                RU (RU_PolyORB_HI_Sporadic_Task, Elaborated => True),
              Parameter_List => Parameter_List);
         return N;
      end Sporadic_Task_Instantiation;

      ----------------------------------
      -- Aperiodic_Task_Instantiation --
      ----------------------------------

      function Aperiodic_Task_Instantiation (E : Node_Id) return Node_Id is
         N              : Node_Id;
         Parameter_List : constant List_Id := New_List (ADN.K_List_Id);
      begin
         --  Port_Type

         N :=
           Make_Parameter_Association
             (Selector_Name    => Make_Defining_Identifier (TN (T_Port_Type)),
              Actual_Parameter =>
                Make_Defining_Identifier (Map_Port_Enumeration_Name (E)));
         Append_Node_To_List (N, Parameter_List);

         --  Raise an error if the thread does not have IN ports

         if not Has_In_Ports (E) then
            Display_Located_Error
              (Loc (E),
               "This sporadic thread does not have IN ports",
               Fatal => True);
         end if;

         --  Raise an error if the thread has no 'in event' ports

         if not Has_In_Event_Ports (E) then
            Display_Located_Error
              (Loc (E),
               "None of the IN ports of this thread is an event port",
               Fatal => True);
         end if;

         --  Append the common parameters

         Cyclic_Task_Instantiation_Formals (E, Parameter_List);

         --  The blocking routine

         N :=
           Make_Parameter_Association
             (Selector_Name =>
                Make_Defining_Identifier (SN (S_Wait_For_Incoming_Events)),
              Actual_Parameter =>
                Make_Defining_Identifier (SN (S_Wait_For_Incoming_Events)));
         Append_Node_To_List (N, Parameter_List);

         --  Build the package instantiation

         N :=
           Make_Package_Instantiation
             (Defining_Identifier => Map_Task_Identifier (E),
              Generic_Package     =>
                RU (RU_PolyORB_HI_Aperiodic_Task, Elaborated => True),
              Parameter_List => Parameter_List);
         return N;
      end Aperiodic_Task_Instantiation;

      -----------------------------------
      -- Background_Task_Instantiation --
      -----------------------------------

      function Background_Task_Instantiation (E : Node_Id) return Node_Id is
         N              : Node_Id;
         Parameter_List : constant List_Id := New_List (ADN.K_List_Id);
      begin
         --  Append the common parameters

         Cyclic_Task_Instantiation_Formals (E, Parameter_List);

         --  Build the package instantiation

         N :=
           Make_Package_Instantiation
             (Defining_Identifier => Map_Task_Identifier (E),
              Generic_Package     =>
                RU (RU_PolyORB_HI_Background_Task, Elaborated => True),
              Parameter_List => Parameter_List);
         return N;
      end Background_Task_Instantiation;

      -----------------------------
      -- Null_Task_Instantiation --
      -----------------------------

      function Null_Task_Instantiation (E : Node_Id) return Node_Id is
         N              : Node_Id;
         Parameter_List : constant List_Id := New_List (ADN.K_List_Id);
      begin
         --  Append the common parameters

         Cyclic_Task_Instantiation_Formals (E, Parameter_List);

         --  Build the package instantiation

         N :=
           Make_Package_Instantiation
             (Defining_Identifier => Map_Task_Identifier (E),
              Generic_Package     => RU (RU_PolyORB_HI_Null_Task),
              Parameter_List      => Parameter_List);
         return N;
      end Null_Task_Instantiation;

      ----------------------------
      -- ISR_Task_Instantiation --
      ----------------------------

      function ISR_Task_Instantiation (E : Node_Id) return Node_Id is
         N              : Node_Id;
         Parameter_List : constant List_Id := New_List (ADN.K_List_Id);
         Configuration  : Name_Id;
      begin
         Configuration := Get_Configuration (E);
         if Configuration = No_Name then
            Display_Located_Error
              (Loc (E),
               "No interrupt configured",
               Fatal => True);
         end if;

         Add_With_Package (RU (RU_Ada_Interrupts_Names));

         N :=
           Make_Parameter_Association
             (Selector_Name =>
                Make_Defining_Identifier (PN (P_Interrupt_Identifier)),
              Actual_Parameter =>
                Make_Defining_Identifier (Get_Configuration (E)));
         Append_Node_To_List (N, Parameter_List);

         --  Append the common parameters

         Cyclic_Task_Instantiation_Formals (E, Parameter_List);

         --  Build the package instantiation

         N :=
           Make_Package_Instantiation
             (Defining_Identifier => Map_Task_Identifier (E),
              Generic_Package     => RU (RU_PolyORB_HI_ISR_Task),
              Parameter_List      => Parameter_List);
         return N;
      end ISR_Task_Instantiation;

      -------------------------------
      -- Hybrid_Task_Instantiation --
      -------------------------------

      function Hybrid_Task_Instantiation (E : Node_Id) return Node_Id is
         N              : Node_Id;
         Parameter_List : constant List_Id := New_List (ADN.K_List_Id);
      begin
         --  Port_Type

         N :=
           Make_Parameter_Association
             (Selector_Name    => Make_Defining_Identifier (TN (T_Port_Type)),
              Actual_Parameter =>
                Make_Defining_Identifier (Map_Port_Enumeration_Name (E)));
         Append_Node_To_List (N, Parameter_List);

         --  Raise an error if the thread does not have IN ports

         if not Has_In_Ports (E) then
            Display_Located_Error
              (Loc (E),
               "This hybrid thread does not have IN ports",
               Fatal => True);
         end if;

         --  Raise an error if the thread does not have 'in event'
         --  ports.

         if not Has_In_Event_Ports (E) then
            Display_Located_Error
              (Loc (E),
               "None of the IN ports of this hybrid thread is an event port",
               Fatal => True);
         end if;

         --  Append the common parameters

         Cyclic_Task_Instantiation_Formals (E, Parameter_List);

         --  The blocking routine

         N :=
           Make_Parameter_Association
             (Selector_Name =>
                Make_Defining_Identifier (SN (S_Wait_For_Incoming_Events)),
              Actual_Parameter =>
                Get_Fully_Qualified_Subprogram
                  (SN (S_Wait_For_Incoming_Events)));
         Append_Node_To_List (N, Parameter_List);

         --  Build the package instantiation

         N :=
           Make_Package_Instantiation
             (Defining_Identifier => Map_Task_Identifier (E),
              Generic_Package     => RU (RU_PolyORB_HI_Hybrid_Task),
              Parameter_List      => Parameter_List);
         return N;
      end Hybrid_Task_Instantiation;

      -------------------
      -- Task_Job_Spec --
      -------------------

      function Task_Job_Spec (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Param_List : List_Id                                     := No_List;
         P          : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
      begin
         if P = Thread_Sporadic
           or else P = Thread_Hybrid
           or else P = Thread_Aperiodic
         then
            Param_List := Make_List_Id
              (Make_Parameter_Specification
                 (Defining_Identifier =>
                    Make_Defining_Identifier (PN (P_Port)),
                  Subtype_Mark        =>
                    Make_Defining_Identifier (Map_Port_Enumeration_Name (E))),
               Make_Parameter_Specification
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Result)),
                  Subtype_Mark        => RE (RE_Error_Kind),
                  Parameter_Mode => Mode_Out));

         elsif P = Thread_Periodic
           or else P = Thread_Background
         then
            Param_List := Make_List_Id
              (Make_Parameter_Specification
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Result)),
                  Subtype_Mark        => RE (RE_Error_Kind),
                  Parameter_Mode => Mode_Out));
         end if;

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier => Map_Task_Job_Identifier (E),
              Parameter_Profile   => Param_List);
         return N;
      end Task_Job_Spec;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
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
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P                   : constant Node_Id              := ADN.Entity (U);
         S                   : Node_Id;
         Scheduling_Protocol : Supported_Scheduling_Protocol :=
           Get_Scheduling_Protocol (Get_Bound_Processor (E));
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));

      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Job_Spec;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         if Scheduling_Protocol = Unknown_Scheduler then
            Display_Located_Error
              (Loc (Get_Bound_Processor (E)),
               "Undefined scheduling protocol, " &
               "will use FIFO_WITHIN_PRIORITIES",
               Fatal   => False,
               Warning => True);

            Scheduling_Protocol := POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL;
         elsif Scheduling_Protocol /=
           POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL
           and then Scheduling_Protocol /= ROUND_ROBIN_PROTOCOL
         then
            Display_Located_Error
              (Loc (Parent_Subcomponent (E)),
               "Incompatible scheduling protocol, " &
               "PolyORB-HI/Ada runtime requires " &
               "POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL or" &
               " ROUND_ROBIN_PROTOCOL",
               Fatal => True);

            --  XXX In case of Round robin, we should also check that
            --  the scheduler is set to non-preemptive mode.
         end if;

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            S := First_Node (Subcomponents (The_System));
            while Present (S) loop
               if AAU.Is_Device (Corresponding_Instance (S))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (S)) =
                   Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : constant Node_Id := Get_Implementation (E);
         S              : Node_Id;
      begin
         if Implementation /= No_Node then
            if not AAU.Is_Empty (AAN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  Visit_Component_Instance (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end if;
      end Visit_Device_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
      begin
         Push_Entity (Ada_Root);

         --  Visit all the subcomponents of the system

         Visit_Subcomponents_Of (E);

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         P : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
         O : Node_Id;

         Scheduling_Protocol : Supported_Scheduling_Protocol :=
           Unknown_Scheduler;
         Process_Node : Node_Id;

      begin
         --  Determine the scheduler that controls the current thread instance

         Process_Node := Get_Container_Process (Parent_Subcomponent (E));

         if Present (Process_Node) then
            Scheduling_Protocol :=
              Get_Scheduling_Protocol
                (Get_Bound_Processor (Corresponding_Instance (Process_Node)));
         end if;

         case P is
            when Thread_Periodic =>
               N :=
                 Message_Comment
                   ("Periodic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            when Thread_Sporadic =>
               N :=
                 Message_Comment
                   ("Sporadic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            when Thread_Hybrid =>
               N :=
                 Message_Comment
                   ("Hybrid task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            when Thread_Aperiodic =>
               N :=
                 Message_Comment
                   ("Aperiodic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            when Thread_Background =>
               N :=
                 Message_Comment
                   ("Background task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            when Thread_ISR =>
               N :=
                 Message_Comment
                   ("ISR task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            when others =>
               Display_Located_Error
                 (AIN.Loc (E),
                  "Unsupported dispatch protocol",
                  Fatal => True);
         end case;

         --  Create the spec of the parameterless subprogram that
         --  executes the thread job.

         N := Task_Job_Spec (E);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         Bind_AADL_To_Job (Identifier (S), N);

         --  For each AADL thread, we instantiate a task.

         if Scheduling_Protocol = ROUND_ROBIN_PROTOCOL then
            N := Null_Task_Instantiation (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         else
            --  Default case : FIFO_WITHIN_PRIORITIES
            case P is
               when Thread_Periodic =>
                  --  Instantiate the periodic task

                  N := Periodic_Task_Instantiation (E);
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Sporadic =>
                  --  Instantiate the sporadic task

                  N := Sporadic_Task_Instantiation (E);
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Hybrid =>
                  --  Instantiate the hybrid task

                  N := Hybrid_Task_Instantiation (E);
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Aperiodic =>
                  --  Instantiate the aperiodic task

                  N := Aperiodic_Task_Instantiation (E);
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Background =>
                  --  Instantiate the background task

                  N := Background_Task_Instantiation (E);
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_ISR =>
                  --  Instantiate the ISR task

                  N := ISR_Task_Instantiation (E);
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when others =>
                  raise Program_Error;
            end case;
         end if;

         if Has_Modes (E) then
            --  If the thread has operational modes, then generate the
            --  enumeration type corresponding to the thread mode list
            --  and the procedure allowing to update the current mode.

            N := Make_Modes_Enumeration (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            if Is_Fusioned (E) then
               N := Make_Mode_Updater_Spec (E);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;
         end if;

         --  Visit thread local objects

         if not AINU.Is_Empty (Subcomponents (E)) then
            O := First_Node (Subcomponents (E));

            while Present (O) loop
               if AINU.Is_Data (Corresponding_Instance (O)) then
                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier => Map_Ada_Defining_Identifier (O),
                       Object_Definition   =>
                         Map_Ada_Data_Type_Designator
                           (Corresponding_Instance (O)));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

                  --  Link the variable and the object

                  Bind_AADL_To_Object (Identifier (O), N);
               end if;

               O := Next_Node (O);
            end loop;
         end if;
      end Visit_Thread_Instance;

      ----------------------------
      -- Make_Modes_Enumeration --
      ----------------------------

      function Make_Modes_Enumeration (E : Node_Id) return Node_Id is
         Enum_List : constant List_Id := New_List (ADN.K_Enumeration_Literals);
         M         : Node_Id;
         N         : Node_Id;
      begin
         M := First_Node (Modes (E));

         while Present (M) loop
            N := Map_Ada_Defining_Identifier (M);
            Append_Node_To_List (N, Enum_List);

            M := Next_Node (M);
         end loop;

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (Map_Modes_Enumeration_Name (E)),
              Type_Definition => Make_Enumeration_Type_Definition (Enum_List));

         return N;
      end Make_Modes_Enumeration;

      ----------------------------
      -- Make_Mode_Updater_Spec --
      ----------------------------

      function Make_Mode_Updater_Spec (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (SN (S_Change_Mode)),
              Parameter_Profile =>
                Make_List_Id
                  (Make_Parameter_Specification
                     (Defining_Identifier =>
                        Make_Defining_Identifier (PN (P_Mode)),
                      Subtype_Mark =>
                        Make_Defining_Identifier
                          (Map_Modes_Enumeration_Name (E)),
                      Parameter_Mode => Mode_In)),
              Return_Type => No_Node);
         Set_Backend_Node (Identifier (First_Node (Modes (E))), N);

         return N;
      end Make_Mode_Updater_Spec;

   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      function Task_Job_Body (E : Node_Id) return Node_Id;
      --  Creates the parameterless subprogram body that does the
      --  thread's job.

      function Make_Current_Mode_Declaration (E : Node_Id) return Node_Id;
      --  Create, if necessary, the current mode variable declaration for
      --  thread E.

      function Make_Mode_Updater_body (E : Node_Id) return Node_Id;
      --  Create the procedure which will update the current mode

      Has_Hybrid_Threads       : Boolean            := False;
      Hybrid_Thread_Elements   : List_Id            := No_List;
      Last_Hybrid_Thread_Index : Unsigned_Long_Long := 0;

      Current_Mode_Identifier : Node_Id;

      --  The runtime routines are generated per thread component and
      --  not per thread instance. For each thread instance, we must
      --  complete the case alternative specific to it in each one of
      --  the routines. To perform this, we attache to each thread
      --  component a set of List_Id's which represent the case
      --  statement of the corresponding routines. The entities below
      --  allow to Get/Set these list for each thread component.

      Interrogation_Routine_List : List_Id;
      --  This list will hold all the bodies declarations of the
      --  interrogation routines. We do this to ensure all the bodies
      --  are appended after all entities generated for threads since
      --  they need visibility on these entities.

      -------------------
      -- Task_Job_Body --
      -------------------

      function Task_Job_Body (E : Node_Id) return Node_Id is
         S    : constant Node_Id := Parent_Subcomponent (E);
         Spec : constant Node_Id :=
           ADN.Job_Node (Backend_Node (Identifier (S)));
         Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         P            : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         Impl_Kind : constant Supported_Thread_Implementation :=
           Get_Thread_Implementation_Kind (E);
         Need_Error_Initialization : Boolean := True;

         function Make_Get_Valid_Value (F : Node_Id) return Node_Id;
         --  This function generated an If statement that tests
         --  whether the port ever received a value. In this case, it
         --  returns tha last received value. Otherwithe, it return
         --  the default value for the port data type.

         --------------------------------------------------------------
         -- All routines below do NOT perfom any verification on the --
         -- thread and rely completely on the good faith of their    --
         -- caller.                                                  --
         --------------------------------------------------------------

         procedure Make_Mode_Update;
         --  Generate a case statement that updates the thread mode
         --  depending on the received event port. The event port that
         --  causes the mode switch is dequeued.

         procedure Make_Fetch_In_Ports;
         --  Generate the routines to fetch the values of the thread
         --  IN ports in a non-blocking way.

         procedure Make_Fetch_In_Ports
           (Statements   : List_Id;
            Declarations : List_Id);
         --  Generate the routines to fetch the values of the thread
         --  IN ports in a non-blocking way, puting the result in
         --  the parameters.

         procedure Make_Dequeue_In_Ports;
         --  Generate the routines to dequeue the oldest values of the
         --  thread IN ports in a non-blocking way.

         procedure Make_Dequeue_In_Ports
           (Statements   : List_Id;
            Declarations : List_Id);
         --  Generate the routines to dequeue the oldest values of the
         --  thread IN ports in a non-blocking way, puting the results
         --  in the parameters.

         procedure Make_Call_Sequence (CS : Node_Id := No_Node);
         --  Generate code relying on the thread call sequence

         procedure Make_Thread_Compute_Entrypoint;
         --  Generate code relying on the thread's own compute
         --  entrypoint.

         procedure Make_Ports_Compute_Entrypoint;
         --  Generate code relying on the compute entrypoints of the
         --  thread ports.

         procedure Make_Set_Call_Sequence_Out_Ports
           (CS         : Node_Id;
            Statements : List_Id);
         --  Generate the routines to set the values of the thread OUT
         --  ports used by the call sequence CS.

         procedure Make_Send_Call_Sequence_Out_Ports
           (CS         : Node_Id;
            Statements : List_Id);
         --  Generate the routines to send the values of the thread
         --  OUT ports from teh call sequence CS.

         procedure Make_Set_Out_Ports;
         --  Generate the routines to set the values of the thread
         --  OUT ports.

         procedure Make_Send_Out_Ports;
         --  Generate the routines to send the values of the thread
         --  OUT ports.

         procedure Create_Call_Sequence
           (Stats : List_Id;
            Decl  : List_Id;
            CS    : Node_Id := No_Node;
            Port  : Node_Id := No_Node);

         --------------------------
         -- Make_Get_Valid_Value --
         --------------------------

         function Make_Get_Valid_Value (F : Node_Id) return Node_Id is
            Then_Statements : constant List_Id :=
              New_List (ADN.K_Statement_List);
            Else_Statements : constant List_Id :=
              New_List (ADN.K_Statement_List);
            Condition : Node_Id;
            N         : Node_Id;
         begin
            --  The condition of validity is that the return value of
            --  Get_Count is different from -1.

            Condition :=
              Make_Expression
                (Map_Ada_Defining_Identifier (F, "C"),
                 Op_Not_Equal,
                 Make_Literal (New_Integer_Value (1, -1, 10)));

            --  Then

            N := Make_Subprogram_Call
              (Get_Fully_Qualified_Subprogram (SN (S_Get_Value)),
               Make_List_Id
               (RE (RE_Get_Task_Id),
               Map_Ada_Defining_Identifier (F),
               Map_Ada_Defining_Identifier (F, "I")));

            Append_Node_To_List (N, Then_Statements);

            N := Make_Selected_Component
              (Map_Ada_Defining_Identifier (F, "I"),
               Make_Defining_Identifier (Map_Ada_Component_Name (F)));

            N :=
              Make_Assignment_Statement
                (Map_Ada_Defining_Identifier (F, "V"),
                 N);
            Append_Node_To_List (N, Then_Statements);

            --  Else

            N :=
              Extract_Designator
                (ADN.Default_Value_Node
                   (Backend_Node (Identifier (Corresponding_Instance (F)))));

            N :=
              Make_Assignment_Statement
                (Map_Ada_Defining_Identifier (F, "V"),
                 N);
            Append_Node_To_List (N, Else_Statements);

            N :=
              Make_If_Statement
                (Condition       => Condition,
                 Then_Statements => Then_Statements,
                 Else_Statements => Else_Statements);
            return N;
         end Make_Get_Valid_Value;

         ----------------------
         -- Make_Mode_Update --
         ----------------------

         procedure Make_Mode_Update is
            Alternatives       : constant List_Id := New_List (ADN.K_List_Id);
            Inner_Alternatives : List_Id;
            Choices            : List_Id;
            Inner_Statements   : List_Id;
            F                  : Node_Id;
            N                  : Node_Id;
            M                  : Node_Id;
            Src                : Node_Id;

            function Belongs (F : Node_Id; L : List_Id) return Boolean;
            --  Return True IFF F is referenced by one of the entity
            --  reference instances of list L.

            -------------
            -- Belongs --
            -------------

            function Belongs (F : Node_Id; L : List_Id) return Boolean is
               Ref : Node_Id;
            begin
               Ref := First_Node (L);

               while Present (Ref) loop
                  if F = Item (Last_Node (Path (Ref))) then
                     return True;
                  end if;

                  Ref := Next_Node (Ref);
               end loop;

               return False;
            end Belongs;

         begin
            --  If no mode transition description is given, we do not
            --  have to generate anything

            if AINU.Is_Empty (Mode_transitions (E)) then
               return;
            end if;

            --  FIXME: Taking account of port urgency should NOT be
            --  implemented here but in the event delivery routine
            --  (thread interrogators).

            --  If the thread is sporadic, we already got the value of
            --  the port that triggered the thread. If the thread is
            --  sporadic, we read the value of the oldest triggered
            --  event port.

            if P = Thread_Periodic then
               --  Declare the Port and Valid variables

               N :=
                 Make_Object_Declaration
                   (Defining_Identifier =>
                      Make_Defining_Identifier (PN (P_Port)),
                    Object_Definition =>
                      Make_Defining_Identifier
                        (Map_Port_Enumeration_Name (E)));
               Append_Node_To_List (N, Declarations);

               N :=
                 Make_Object_Declaration
                   (Defining_Identifier =>
                      Make_Defining_Identifier (PN (P_Valid)),
                    Object_Definition => RE (RE_Boolean));
               Append_Node_To_List (N, Declarations);

               --  Call Get_Next_Event

               N := Make_Defining_Identifier (SN (S_Get_Next_Event));
               Set_Homogeneous_Parent_Unit_Name
                 (N,
                  Make_Defining_Identifier (Map_Interrogators_Name (E)));

               N :=
                 Make_Subprogram_Call
                   (N,
                    Make_List_Id
                      (Make_Defining_Identifier (PN (P_Port)),
                       Make_Defining_Identifier (PN (P_Valid))));
               Append_Node_To_List (N, Statements);
            end if;

            --  We generate a global case statement basing on the
            --  received (or read) port. Each alternative of the
            --  statement contains a nested case statement based on
            --  the current mode value to perform the switch.

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then not AIN.Is_Data (F)
               then
                  M                  := First_Node (Mode_transitions (E));
                  Inner_Alternatives := New_List (ADN.K_Statement_List);

                  while Present (M) loop
                     --  If F belongs to the port list of the mode
                     --  transition M, generate necessary case
                     --  alternative for the mode change. We are sure
                     --  this works using case statements without
                     --  having the risk of to case alternative with
                     --  the same labels means the mode switch state
                     --  machine is not deterministic as stated by the
                     --  AADL standard.

                     if Belongs (F, Triggers (M)) then
                        --  For each one of the source ports of M
                        --  generate an inner case alternatice that
                        --  effects the mode switch.

                        Src              := First_Node (Source_Modes (M));
                        Choices          := New_List (ADN.K_List_Id);
                        Inner_Statements := New_List (ADN.K_Statement_List);

                        while Present (Src) loop
                           N := Map_Ada_Defining_Identifier (Item (Src));
                           Append_Node_To_List (N, Choices);

                           Src := Next_Node (Src);
                        end loop;

                        --  Perform the mode change

                        N :=
                          Make_Assignment_Statement
                            (Make_Defining_Identifier
                               (Map_Current_Mode_Name (E)),
                             Map_Ada_Defining_Identifier
                               (Item (Destination_Mode (M))));
                        Append_Node_To_List (N, Inner_Statements);

                        --  Dequeue the event port

                        --  Create a qualified value of the port enumerator
                        --  to avoid name clashing between ports.

                        N :=
                          Make_Record_Aggregate
                            (Make_List_Id
                               (Make_Defining_Identifier (PN (P_Port))));

                        N :=
                          Make_Qualified_Expression
                            (Make_Defining_Identifier
                               (Map_Port_Enumeration_Name (E)),
                             N);

                        --  Call Next_Value

                        N :=
                          Make_Subprogram_Call
                            (Get_Fully_Qualified_Subprogram
                               (SN (S_Next_Value)),
                             Make_List_Id (N));
                        Append_Node_To_List (N, Inner_Statements);

                        N :=
                          Make_Case_Statement_Alternative
                            (Choices,
                             Inner_Statements);
                        Append_Node_To_List (N, Inner_Alternatives);
                     end if;

                     M := Next_Node (M);
                  end loop;

                  --  If the port triggers at least one mode switch,
                  --  add a case alternative

                  if not Is_Empty (Inner_Alternatives) then
                     --  Default case alternative (when others => null;)

                     N := Make_Case_Statement_Alternative (No_List, No_List);
                     Append_Node_To_List (N, Inner_Alternatives);

                     N :=
                       Make_Case_Statement
                         (Make_Defining_Identifier (Map_Current_Mode_Name (E)),
                          Inner_Alternatives);

                     --  External case alternative

                     N :=
                       Make_Case_Statement_Alternative
                         (Make_List_Id (Map_Ada_Defining_Identifier (F)),
                          Make_List_Id (N));
                     Append_Node_To_List (N, Alternatives);
                  end if;
               end if;

               F := Next_Node (F);
            end loop;

            --  Default case alternative (when others => null;)

            N := Make_Case_Statement_Alternative (No_List, No_List);
            Append_Node_To_List (N, Alternatives);

            --  Make the case statement

            N :=
              Make_Case_Statement
                (Make_Defining_Identifier (PN (P_Port)),
                 Alternatives);

            --  If the thread is sporadic, the case statement is added
            --  directly to the thread job statements. If the thread
            --  is periodic, the case statement is executed only if
            --  'Valid' is True.

            if P = Thread_Periodic then
               N :=
                 Make_If_Statement
                   (Condition       => Make_Defining_Identifier (PN (P_Valid)),
                    Then_Statements => Make_List_Id (N));
            end if;

            Append_Node_To_List (N, Statements);
         end Make_Mode_Update;

         -------------------------
         -- Make_Fetch_In_Ports --
         -------------------------

         procedure Make_Fetch_In_Ports is
         begin
            Make_Fetch_In_Ports
              (Statements   => Statements,
               Declarations => Declarations);
         end Make_Fetch_In_Ports;

         -------------------------
         -- Make_Fetch_In_Ports --
         -------------------------

         procedure Make_Fetch_In_Ports
           (Statements   : List_Id;
            Declarations : List_Id)
         is
            N : Node_Id;
            F : Node_Id;
         begin
            N := Message_Comment ("Get the IN port values");
            Append_Node_To_List (N, Statements);
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then AIN.Is_Data (F)
               then
                  --  Declare local variable

                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier =>
                         Map_Ada_Defining_Identifier (F, "V"),
                       Object_Definition =>
                         Map_Ada_Data_Type_Designator
                           (Corresponding_Instance (F)));
                  Append_Node_To_List (N, Declarations);

                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier =>
                         Map_Ada_Defining_Identifier (F, "I"),
                       Object_Definition =>
                       Make_Defining_Identifier (Map_Port_Interface_Name (E)));
                  Append_Node_To_List (N, Declarations);

                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier =>
                         Map_Ada_Defining_Identifier (F, "C"),
                       Constant_Present  => True,
                       Object_Definition => RE (RE_Integer),
                       Expression        =>
                         Make_Subprogram_Call
                           (Get_Fully_Qualified_Subprogram (SN (S_Get_Count)),
                            Make_List_Id
                              (RE (RE_Get_Task_Id),
                               Make_Qualified_Expression
                                 (Make_Defining_Identifier
                                    (Map_Port_Enumeration_Name (E)),
                                  Make_Record_Aggregate
                                    (Make_List_Id
                                       (Map_Ada_Defining_Identifier (F)))))));

                  Append_Node_To_List (N, Declarations);

                  --  Assign the port value

                  N := Make_Get_Valid_Value (F);
                  Append_Node_To_List (N, Statements);

                  --  If the in port has not any destination inside
                  --  the thread and in the call sequence (if specified),
                  --  display a warning

                  if AINU.Is_Empty (Destinations (F)) then
                     Display_Located_Error
                       (AIN.Loc (F),
                        "This IN port has no destination inside the thread." &
                        " This could be an inconsistency in the AADL model",
                        Fatal   => False,
                        Warning => True);
                  end if;
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Fetch_In_Ports;

         ---------------------------
         -- Make_Dequeue_In_Ports --
         ---------------------------

         procedure Make_Dequeue_In_Ports is
         begin
            Make_Dequeue_In_Ports
              (Statements   => Statements,
               Declarations => Declarations);
         end Make_Dequeue_In_Ports;

         ---------------------------
         -- Make_Dequeue_In_Ports --
         ---------------------------

         procedure Make_Dequeue_In_Ports
           (Statements   : List_Id;
            Declarations : List_Id)
         is
            pragma Unreferenced (Declarations);
            N                   : Node_Id;
            F                   : Node_Id;
            First_Port_Dequeued : Boolean := True;
         begin
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then Is_Event (F)
               then
                  if First_Port_Dequeued then
                     N := Message_Comment ("Dequeue the IN port values");
                     Append_Node_To_List (N, Statements);
                     First_Port_Dequeued := False;
                  end if;

                  --  Create a qualified value of the port enumerator
                  --  to avoid name clashing between ports.

                  N :=
                    Make_Record_Aggregate
                      (Make_List_Id (Map_Ada_Defining_Identifier (F)));

                  N :=
                    Make_Qualified_Expression
                      (Make_Defining_Identifier
                         (Map_Port_Enumeration_Name (E)),
                       N);

                  --  Call Next_Value

                  N :=
                    Make_Subprogram_Call
                      (Get_Fully_Qualified_Subprogram (SN (S_Next_Value)),
                       Make_List_Id (RE (RE_Get_Task_Id), N));

                  Append_Node_To_List (N, Statements);
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Dequeue_In_Ports;

         ------------------------
         -- Make_Call_Sequence --
         ------------------------

         procedure Make_Call_Sequence (CS : Node_Id := No_Node) is
         begin
            Create_Call_Sequence
              (Stats => Statements,
               Decl  => Declarations,
               CS    => CS,
               Port  => No_Node);
         end Make_Call_Sequence;

         --------------------------
         -- Create_Call_Sequence --
         --------------------------

         procedure Create_Call_Sequence
           (Stats : List_Id;
            Decl  : List_Id;
            CS    : Node_Id := No_Node;
            Port  : Node_Id := No_Node)
         is
            pragma Assert
              (No (CS) or else Kind (CS) = K_Call_Sequence_Instance);

            function In_Modes_To_Choices (L : List_Id) return List_Id;
            --  Converts an In_Modes (modes only) list into a case
            --  statement alternative choice list.

            -------------------------
            -- In_Modes_To_Choices --
            -------------------------

            function In_Modes_To_Choices (L : List_Id) return List_Id is
               Choices : constant List_Id := New_List (ADN.K_List_Id);
               M       : Node_Id;
            begin
               M := First_Node (L);

               while Present (M) loop
                  Append_Node_To_List
                    (Map_Ada_Defining_Identifier (Item (M)),
                     Choices);

                  M := Next_Node (M);
               end loop;

               return Choices;
            end In_Modes_To_Choices;

            Call_Seq : Node_Id;
            N        : Node_Id;
         begin
            if No (CS) or else Has_Modes (E) then
               Call_Seq := First_Node (Calls (E));
            else
               Call_Seq := CS;
            end if;

            if not Has_Modes (E) or else AINU.Length (Calls (E)) = 1 then
               --  If the thread has no modes, then it should have one
               --  unique call sequence, handle it.

               Handle_Call_Sequence
                 (E,
                  Extract_Enumerator (E),
                  Call_Seq,
                  Decl,
                  Stats);

               Make_Set_Call_Sequence_Out_Ports (Call_Seq, Stats);
               Make_Send_Call_Sequence_Out_Ports (Call_Seq, Stats);

               --  If the thread is sporadic and there is no
               --  ambiguity in the call sequence to be called,
               --  then the 'Port' parameter is not used

               if not AINU.Is_Empty (AIN.Calls (E))
                 and then AINU.Length (AIN.Calls (E)) <= 1
                 and then
                 ((Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic)
                  or else
                  (Get_Thread_Dispatch_Protocol (E) = Thread_Aperiodic))
               then
                  N :=
                    Make_Pragma_Statement
                      (Pragma_Unreferenced,
                       Make_List_Id (Make_Defining_Identifier (PN (P_Port))));
                  Append_Node_To_List (N, Decl);
               end if;
            else
               declare
                  Alternatives  : constant List_Id := New_List (ADN.K_List_Id);
                  Alt_Sts       : List_Id;
                  Share_In_Port : Boolean;
                  CS_Cnt        : Natural          := 0;
               begin
                  while Present (Call_Seq) loop
                     Share_In_Port := No (Port);
                     if Present (CS) and then Present (Port) then
                        --  If there is a specified call sequence,
                        --  we want to create call sequences from the
                        --  same ports

                        declare
                           Mode : Name_Id;
                           G    : Node_Id;
                           CE   : Node_Id;
                        begin
                           G := AIN.First_Node (AIN.Modes (E));
                           while Present (G) loop
                              Mode := AIN.Name (AIN.Identifier (G));
                              CE   := Get_Port_Compute_Entrypoint (Port, Mode);
                              if Present (CE) then
                                 declare
                                    Value : constant Node_Id :=
                                      ATN.Expanded_Single_Value
                                        (AIN.Property_Association_Value (CE));
                                 begin
                                    if ATN.Entity
                                        (ATN.Reference_Term (Value)) =
                                      Call_Seq
                                    then
                                       Share_In_Port := True;
                                       exit;
                                    end if;
                                 end;
                              end if;

                              G := AIN.Next_Node (G);
                           end loop;
                        end;
                     end if;

                     if Share_In_Port then
                        --  Handle the call sequence inside the case
                        --  alternative statements.

                        Alt_Sts := New_List (ADN.K_Statement_List);
                        Handle_Call_Sequence
                          (E,
                           Extract_Enumerator (E),
                           Call_Seq,
                           Decl,
                           Alt_Sts);

                        Make_Set_Call_Sequence_Out_Ports (Call_Seq, Alt_Sts);
                        Make_Send_Call_Sequence_Out_Ports (Call_Seq, Alt_Sts);

                        if Present (AIN.In_Modes (Call_Seq))
                          and then not AINU.Is_Empty
                            (ATN.Modes (AIN.In_Modes (Call_Seq)))
                        then
                           --  Generate a case statement alternative that
                           --  handles this sequence.

                           N :=
                             Make_Case_Statement_Alternative
                               (In_Modes_To_Choices
                                  (ATN.Modes (In_Modes (Call_Seq))),
                                Alt_Sts);
                           Append_Node_To_List (N, Alternatives);
                           CS_Cnt := CS_Cnt + 1;
                        end if;

                     end if;
                     Call_Seq := Next_Node (Call_Seq);
                  end loop;

                  if CS_Cnt = 0 then
                     --  We are sure this is the unique call
                     --  sequence without in_modes statement. As
                     --  stated by the standard it should be used
                     --  when none of the other call sequences
                     --  match.

                     N := Make_Case_Statement_Alternative (No_List, Alt_Sts);
                     Append_Node_To_List (N, Alternatives);
                  else
                     --  Default case alternative (when others => null;)

                     N := Make_Case_Statement_Alternative (No_List, No_List);
                     Append_Node_To_List (N, Alternatives);
                  end if;

                  N :=
                    Make_Case_Statement
                      (Make_Defining_Identifier (Map_Current_Mode_Name (E)),
                       Alternatives);

                  if Is_Fusioned (E) and then CS_Cnt > 1 then
                     declare
                        N2 : Node_Id;
                     begin
                        N2 :=
                          Make_Used_Package
                            (Make_Designator
                               (Map_Scheduler_Instance_Object_Name (E),
                                Map_Scheduler_Instance_Name (E)));
                        Append_Node_To_List (N2, Declarations);

                        N2 :=
                          Make_Assignment_Statement
                            (Make_Defining_Identifier (SN (S_R_Continue)),
                             Make_Defining_Identifier (SN (S_True)));
                        Append_Node_To_List (N2, Stats);

                        N2 :=
                          Make_Exit_When_Statement
                            (Make_Expression
                               (Make_Defining_Identifier (SN (S_R_Continue)),
                                Op_Not));
                        N := Make_Loop_Statement (Make_List_Id (N, N2));
                        Append_Node_To_List (N, Stats);
                     end;
                  else
                     Append_Node_To_List (N, Stats);
                  end if;
               end;
            end if;
         end Create_Call_Sequence;

         ------------------------------------
         -- Make_Thread_Compute_Entrypoint --
         ------------------------------------

         procedure Make_Thread_Compute_Entrypoint is
            N            : Node_Id;
            Call_Profile : List_Id;
         begin
            N := Message_Comment ("Call the thread compute entrypoint");
            Append_Node_To_List (N, Statements);

            --  If the entrypoint is set by Activate_Entrypoint_Source_Text,
            --  then the parameters are implicit. If it is declared with
            --  Activate_Entrypoint_Call_Sequence, the parameters are the
            --  same as the call sequence single subprogram's.

            --  If the thread is periodic, then the compute
            --  entrypoint's unique parameter is the enumerator
            --  corresponding to the thread because the thread is
            --  triggered with a time event. If the thread is sporadic
            --  or hybrid then the compute entrypoint takes another
            --  parameter which is the port that triggered the thread.

            case P is
               when Thread_Periodic | Thread_Background =>
                  Call_Profile := Make_List_Id (Extract_Enumerator (E));

               when Thread_Sporadic | Thread_Hybrid =>
                  Call_Profile :=
                    Make_List_Id
                      (Extract_Enumerator (E),
                       Make_Defining_Identifier (PN (P_Port)));

               when others =>
                  raise Program_Error;
            end case;

            --  If the thread has no modes, we just call the compute
            --  entrypoint.

            if not Has_Modes (E) then
               N :=
                 Make_Subprogram_Call
                   (Map_Ada_Subprogram_Identifier (E),
                    Call_Profile);
               Append_Node_To_List (N, Statements);
            else
               declare
                  Alternatives : constant List_Id := New_List (ADN.K_List_Id);
                  Alt_Sts      : List_Id;
                  CEP_Name     : Name_Id;
                  Default      : Boolean          := False;
                  M            : Node_Id := AIN.First_Node (AIN.Modes (E));
                  Mode_Name    : Name_Id;

               begin
                  while Present (M) loop
                     Alt_Sts := New_List (ADN.K_Statement_List);

                     Mode_Name := AIN.Name (AIN.Identifier (M));
                     CEP_Name  :=
                       Get_Thread_Compute_Entrypoint
                         (T       => E,
                          In_Mode => Mode_Name);

                     if CEP_Name = No_Name then
                        --  If there is no compute entry point
                        --  associated to M, then this mode will be
                        --  handled in the 'default' switch case.

                        Default := True;
                     else
                        N :=
                          Make_Subprogram_Call
                            (Map_Ada_Subprogram_Identifier (CEP_Name),
                             Call_Profile);

                        Append_Node_To_List (N, Alt_Sts);

                        N :=
                          Make_Case_Statement_Alternative
                            (Make_List_Id (Map_Ada_Defining_Identifier (M)),
                             Alt_Sts);
                        Append_Node_To_List (N, Alternatives);
                     end if;

                     M := AIN.Next_Node (M);
                  end loop;

                  if Default then
                     --  Default case alternative (when others =>
                     --  <call default entrypoint> or <null>;)

                     CEP_Name :=
                       Get_Thread_Compute_Entrypoint
                         (T       => E,
                          In_Mode => No_Name);

                     if CEP_Name = No_Name then
                        N :=
                          Make_Case_Statement_Alternative (No_List, No_List);
                     else
                        N :=
                          Make_Case_Statement_Alternative
                            (No_List,
                             Make_List_Id
                               (Map_Ada_Subprogram_Identifier (CEP_Name)));
                     end if;
                     Append_Node_To_List (N, Alternatives);
                  end if;

                  N :=
                    Make_Case_Statement
                      (Make_Defining_Identifier (Map_Current_Mode_Name (E)),
                       Alternatives);
                  Append_Node_To_List (N, Statements);
               end;
            end if;
         end Make_Thread_Compute_Entrypoint;

         -----------------------------------
         -- Make_Ports_Compute_Entrypoint --
         -----------------------------------

         procedure Make_Ports_Compute_Entrypoint is
            N            : Node_Id;
            F            : Node_Id;
            Alternatives : constant List_Id := New_List (ADN.K_List_Id);
            Is_Reference : Boolean          := False;
            Is_String    : Boolean          := False;
            Caller_Nb    : Natural          := 0;
            St           : List_Id          := No_List;
         begin
            N :=
              Message_Comment
                ("Depending on the triggered port, call" &
                 " the corresponding compute " &
                 "entrypoint.");
            Append_Node_To_List (N, Statements);

            F := First_Node (Features (E));
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then Is_Event (F)
               then
                  declare
                     use ATN;

                     Value : constant Node_Id :=
                       ATN.Expanded_Single_Value
                         (AIN.Property_Association_Value
                            (Get_Thread_Compute_Entrypoint (F)));
                  begin
                     Caller_Nb := Caller_Nb + 1;
                     St        := New_List (ADN.K_Statement_List);

                     if ATN.Kind (Value) = ATN.K_Reference_Term then
                        if Is_String then
                           Display_Located_Error
                             (AIN.Loc (E),
                              "Cannot use both compute_entrypoint and " &
                              "compute_entrypoint_call_sequence in the " &
                              "same thread.",
                              Fatal => True);
                        end if;

                        if not Is_Reference then
                           Make_Fetch_In_Ports (Statements, Declarations);
                           Make_Dequeue_In_Ports (Statements, Declarations);
                           Is_Reference := True;
                        end if;

                        declare
                           Call_Seq : constant Node_Id :=
                             ATN.Entity (ATN.Reference_Term (Value));
                        begin
                           --  Handle the thread call sequences

                           Create_Call_Sequence
                             (St,
                              Declarations,
                              Call_Seq,
                              F);
                        end;
                     else
                        if Is_Reference then
                           Display_Located_Error
                             (AIN.Loc (E),
                              "Cannot use both compute_entrypoint and " &
                              "compute_entrypoint_call_sequence in the " &
                              "same thread.",
                              Fatal => True);
                        end if;
                        Is_String := True;

                        if AIN.Loc (F) = No_Location then
                           N :=
                             Message_Comment
                               ("Received a period event from the hybrid" &
                                " tasks driver");
                           Append_Node_To_List (N, St);
                        end if;

                        --  We only fetch and dequeue the usefull port
                        --  (string entrypoint cannot access ports
                        --  values others than their trigger)

                        --  Declare and read the variable current value

                        if AIN.Is_Data (F) then
                           N :=
                             Make_Object_Declaration
                               (Defining_Identifier =>
                                  Map_Ada_Defining_Identifier (F, "V"),
                                Object_Definition =>
                                  Map_Ada_Data_Type_Designator
                                    (Corresponding_Instance (F)));
                           Append_Node_To_List (N, Declarations);
                           N :=
                             Make_Object_Declaration
                               (Defining_Identifier =>
                                  Map_Ada_Defining_Identifier (F, "C"),
                                Constant_Present  => True,
                                Object_Definition => RE (RE_Integer),
                                Expression        =>
                                  Make_Subprogram_Call
                                    (Get_Fully_Qualified_Subprogram
                                       (SN (S_Get_Count)),
                                     Make_List_Id
                                       (RE (RE_Get_Task_Id),
                                        Make_Qualified_Expression
                                          (Make_Defining_Identifier
                                             (Map_Port_Enumeration_Name (E)),
                                           Make_Record_Aggregate
                                             (Make_List_Id
                                                (Map_Ada_Defining_Identifier
                                                   (F)))))));

                           Append_Node_To_List (N, Declarations);

                           N :=
                             Make_Object_Declaration
                               (Defining_Identifier =>
                                  Map_Ada_Defining_Identifier (F, "I"),
                                Object_Definition =>
                                  Make_Defining_Identifier
                                    (Map_Port_Interface_Name (E)));
                           Append_Node_To_List (N, Declarations);

                           --  Assign the port value

                           N := Make_Get_Valid_Value (F);
                           Append_Node_To_List (N, St);
                        end if;

                        --  Create a qualified value of the port
                        --  enumerator to avoid name clashing
                        --  between ports.

                        N := Message_Comment ("Dequeue the IN port values");
                        Append_Node_To_List (N, St);

                        N :=
                          Make_Record_Aggregate
                            (Make_List_Id (Map_Ada_Defining_Identifier (F)));
                        N :=
                          Make_Qualified_Expression
                            (Make_Defining_Identifier
                               (Map_Port_Enumeration_Name (E)),
                             N);

                        --  Call Next_Value

                        N :=
                          Make_Subprogram_Call
                            (Get_Fully_Qualified_Subprogram
                               (SN (S_Next_Value)),
                             Make_List_Id (RE (RE_Get_Task_Id), N));
                        Append_Node_To_List (N, St);

                        --  Call the port compute entrypoint with the
                        --  received value (if any).

                        declare
                           Profile : constant List_Id :=
                             Make_List_Id (Extract_Enumerator (E));
                        begin
                           if AIN.Is_Data (F) then
                              Append_Node_To_List
                                (Map_Ada_Defining_Identifier (F, "V"),
                                 Profile);
                           end if;

                           N :=
                             Make_Subprogram_Call
                               (Map_Ada_Subprogram_Identifier (F),
                                Profile);
                           Append_Node_To_List (N, St);
                        end;
                     end if;

                     --  Make the case statement alternative

                     N :=
                       Make_Case_Statement_Alternative
                         (Make_List_Id (Map_Ada_Defining_Identifier (F)),
                          St);
                     Append_Node_To_List (N, Alternatives);
                  end;
               end if;

               F := Next_Node (F);
            end loop;

            if Caller_Nb > 1 then
               N :=
                 Make_Case_Statement_Alternative
                   (No_List,
                    Make_List_Id
                      (Make_Raise_Statement
                         (Make_Designator (EN (E_Program_Error)))));
               Append_Node_To_List (N, Alternatives);

               --  Make the case statement

               N :=
                 Make_Case_Statement
                   (Make_Defining_Identifier (PN (P_Port)),
                    Alternatives);
               Append_Node_To_List (N, Statements);

            elsif Caller_Nb = 1 then
               ADN.Set_Next_Node
                 (ADN.Last_Node (Statements),
                  ADN.First_Node (St));
               ADN.Set_Last_Node (Statements, ADN.Last_Node (St));

               if Is_String
                 and then Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic
               then

                  --  If the thread is sporadic, then the 'Port'
                  --  parameter is not used

                  N :=
                    Make_Pragma_Statement
                      (Pragma_Unreferenced,
                       Make_List_Id (Make_Defining_Identifier (PN (P_Port))));
                  Append_Node_To_List (N, Declarations);
               end if;
            end if;
         end Make_Ports_Compute_Entrypoint;

         --------------------------------------
         -- Make_Set_Call_Sequence_Out_Ports --
         --------------------------------------

         procedure Make_Set_Call_Sequence_Out_Ports
           (CS         : Node_Id;
            Statements : List_Id)
         is
            Wrapper : constant Node_Id :=
              Corresponding_Instance (First_Node (Subprogram_Calls (CS)));
            N        : Node_Id;
            F        : Node_Id;
            Find_One : Boolean := False;
         begin
            if AINU.Is_Empty (Features (E)) then
               F := No_Node;
            else
               F := First_Node (Features (E));
            end if;
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
                  --  We do not set the ports that are connected to
                  --  the call sequence wrapper out ports, this should be
                  --  done during the subprogram call sequence handling.
                  --  We also do not set OUT port that have not any sources.

                  declare
                     D    : Node_Id := First_Node (Sources (F));
                     Set  : Boolean := True;
                     Used : Boolean := False;
                  begin
                     --  If the OUT port has not any sources, we
                     --  display a warning.

                     if No (D) then
                        Set := False;

                        Display_Located_Error
                          (AIN.Loc (F),
                           "This OUT port has no source from inside the" &
                           " thread. This could be an inconsistency in the" &
                           " AADL model",
                           Fatal   => False,
                           Warning => True);
                     end if;

                     while Present (D) loop
                        Set := Kind (Item (D)) /= K_Port_Spec_Instance;
                        exit when not Set;

                        if not Used then
                           if Parent_Component (Item (D)) = Wrapper then
                              Used := True;
                           end if;
                        end if;

                        D := Next_Node (D);
                     end loop;

                     if Used then
                        if not Find_One then
                           Find_One := True;
                           N        :=
                             Message_Comment
                               ("Set the call sequence OUT port values");
                           Append_Node_To_List (N, Statements);
                        end if;

                        N :=
                          Make_Record_Aggregate
                            (Make_List_Id
                               (Make_Component_Association
                                  (Make_Defining_Identifier (CN (C_Port)),
                                   Map_Ada_Defining_Identifier (F)),
                                Make_Component_Association
                                  (Make_Defining_Identifier
                                     (Map_Ada_Component_Name (F)),
                                   Map_Ada_Defining_Identifier (F, "V"))));

                        N :=
                          Make_Qualified_Expression
                            (Make_Defining_Identifier
                               (Map_Port_Interface_Name (E)),
                             N);

                        N :=
                          Make_Subprogram_Call
                            (Get_Fully_Qualified_Subprogram (SN (S_Put_Value)),
                             Make_List_Id (RE (RE_Get_Task_Id), N));

                        Append_Node_To_List (N, Statements);
                     end if;
                  end;
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Set_Call_Sequence_Out_Ports;

         ---------------------------------------
         -- Make_Send_Call_Sequence_Out_Ports --
         ---------------------------------------

         procedure Make_Send_Call_Sequence_Out_Ports
           (CS         : Node_Id;
            Statements : List_Id)
         is
            Wrapper : constant Node_Id :=
              Corresponding_Instance (First_Node (Subprogram_Calls (CS)));
            N        : Node_Id;
            F        : Node_Id;
            Find_One : Boolean := False;
         begin
            if AINU.Is_Empty (Features (E)) then
               F := No_Node;
            else
               F := First_Node (Features (E));
            end if;
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
                  declare
                     D    : Node_Id := First_Node (Sources (F));
                     Used : Boolean := False;
                  begin
                     while Present (D) loop
                        if not Used then
                           if Parent_Component (Item (D)) = Wrapper then
                              Used := True;
                           end if;
                        end if;

                        exit when Kind (Item (D)) = K_Port_Spec_Instance;

                        D := Next_Node (D);
                     end loop;

                     if Used then
                        if not Find_One then
                           Find_One := True;
                           N        :=
                             Message_Comment
                               ("Send the call sequence OUT port values");
                           Append_Node_To_List (N, Statements);
                        end if;
                        N :=
                          Make_Subprogram_Call
                            (Get_Fully_Qualified_Subprogram
                               (SN (S_Send_Output)),
                             Make_List_Id (RE (RE_Get_Task_Id),
                                           Make_Qualified_Expression
                                          (Make_Defining_Identifier
                                             (Map_Port_Enumeration_Name (E)),
                                           Make_Record_Aggregate
                                             (Make_List_Id
                                                (Map_Ada_Defining_Identifier
                                                   (F))))));

                        N :=
                          Make_Assignment_Statement
                            (Variable_Identifier =>
                               Make_Defining_Identifier (VN (V_Error)),
                             Expression => N);
                        Append_Node_To_List (N, Statements);
                        Need_Error_Initialization := False;
                     end if;
                  end;
               end if;
               F := Next_Node (F);
            end loop;
         end Make_Send_Call_Sequence_Out_Ports;

         ------------------------
         -- Make_Set_Out_Ports --
         ------------------------

         procedure Make_Set_Out_Ports is
            N : Node_Id;
            F : Node_Id;
         begin
            N := Message_Comment ("Set the OUT port values");
            Append_Node_To_List (N, Statements);

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
                  --  We do not set the ports that are connected to
                  --  subprogram out ports, this should be done during
                  --  the subprogram call sequence handling. We also
                  --  do not set OUT port that have not any sources.

                  declare
                     D   : Node_Id := First_Node (Sources (F));
                     Set : Boolean := True;
                  begin
                     --  If the OUT port has not any sources, we
                     --  display a warning.

                     if No (D) then
                        Set := False;

                        Display_Located_Error
                          (AIN.Loc (F),
                           "This OUT port has no source from inside the" &
                           " thread. This could be an inconsistency in the" &
                           " AADL model",
                           Fatal   => False,
                           Warning => True);
                     end if;

                     while Present (D) loop
                        Set := Kind (Item (D)) /= K_Port_Spec_Instance;
                        exit when not Set;

                        D := Next_Node (D);
                     end loop;

                     if Set then
                        N :=
                          Make_Record_Aggregate
                            (Make_List_Id
                               (Make_Component_Association
                                  (Make_Defining_Identifier (CN (C_Port)),
                                   Map_Ada_Defining_Identifier (F)),
                                Make_Component_Association
                                  (Make_Defining_Identifier
                                     (Map_Ada_Component_Name (F)),
                                   Map_Ada_Defining_Identifier (F, "V"))));

                        N :=
                          Make_Qualified_Expression
                            (Make_Defining_Identifier
                               (Map_Port_Interface_Name (E)),
                             N);

                        N :=
                          Make_Subprogram_Call
                            (Get_Fully_Qualified_Subprogram (SN (S_Put_Value)),
                             Make_List_Id (N));

                        Append_Node_To_List (N, Statements);
                     end if;
                  end;
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Set_Out_Ports;

         -------------------------
         -- Make_Send_Out_Ports --
         -------------------------

         procedure Make_Send_Out_Ports is
            N : Node_Id;
            F : Node_Id;
         begin
            N := Message_Comment ("Send the OUT ports");
            Append_Node_To_List (N, Statements);

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
                  N :=
                    Make_Subprogram_Call
                      (Get_Fully_Qualified_Subprogram (SN (S_Send_Output)),
                       Make_List_Id (RE (RE_Get_Task_Id),
                                     Make_Qualified_Expression
                                       (Make_Defining_Identifier
                                          (Map_Port_Enumeration_Name (E)),
                                        Make_Record_Aggregate
                                          (Make_List_Id
                                             (Map_Ada_Defining_Identifier
                                                (F))))));

                  N :=
                    Make_Assignment_Statement
                      (Variable_Identifier =>
                         Make_Defining_Identifier (VN (V_Error)),
                       Expression => N);
                  Append_Node_To_List (N, Statements);
                  Need_Error_Initialization := False;
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Send_Out_Ports;

         N : Node_Id;
      begin
         Check_Thread_Consistency (E);

         if Has_Ports (E) then
            Add_With_Package
              (E    => RU (Ru_PolyORB_HI_Generated_Activity),
               Used => True);
         end if;

         --  If the thread contains operational modes. we update the
         --  value of the current mode depending on the received
         --  events.

         if Has_Modes (E) then
            Make_Mode_Update;
         end if;

         --  Depending on the implementation kind, call the proper
         --  implementation routines.

         case Impl_Kind is
            when Thread_With_Call_Sequence =>
               --  This kind of implementation is the simplest
               --  one. The user has only to implementation the
               --  behaviour of subprograms and does not have to worry
               --  about sending and receiving ports.

               --  Get IN ports values and dequeue them

               if Has_In_Ports (E) then
                  Make_Fetch_In_Ports;
                  Make_Dequeue_In_Ports;
               end if;

               --  Handle the thread call sequences

               if not AINU.Is_Empty (Calls (E)) then
                  Make_Call_Sequence;
               end if;

            when Thread_With_Compute_Entrypoint =>
               declare
                  use ATN;
                  Property : constant Node_Id :=
                    Get_Thread_Compute_Entrypoint (E);
                  Value : Node_Id;
               begin
                  if AIN.Kind (Property) = K_Component_Instance then
                     --  Call the compute entrypoint. The code of the
                     --  compute entry point will include the setting
                     --  of the thread OUT ports.

                     Make_Thread_Compute_Entrypoint;

                     --  Send OUT ports.

                     --  XXX: Depending on an AADL property, the
                     --  code of the thread entrypoint may include the
                     --  sending of OUT ports. Which AADL property?

                     if Has_Out_Ports (E) then
                        Make_Set_Out_Ports;
                        Make_Send_Out_Ports;
                     end if;

                  else
                     Value :=
                       Expanded_Single_Value
                         (AIN.Property_Association_Value (Property));

                     if ATN.Kind (Value) = ATN.K_Reference_Term then
                        --  Get IN ports values and dequeue them

                        if Has_In_Ports (E) then
                           Make_Fetch_In_Ports;
                           Make_Dequeue_In_Ports;
                        end if;

                        --  Handle the thread call sequences

                        Make_Call_Sequence
                          (ATN.Entity (ATN.Reference_Term (Value)));
                     else
                        --  Call the compute entrypoint. The code of the
                        --  compute entry point will include the setting
                        --  of the thread OUT ports.

                        Make_Thread_Compute_Entrypoint;

                        --  Send OUT ports.

                        --  XXX: Depending on an AADL property, the
                        --  code of the thread entrypoint may include the
                        --  sending of OUT ports. Which AADL property?

                        if Has_Out_Ports (E) then
                           Make_Set_Out_Ports;
                           Make_Send_Out_Ports;
                        end if;
                     end if;
                  end if;
               end;

            when Thread_With_Port_Compute_Entrypoint =>
               --  Call the compute entrypoints of the triggeing
               --  port. The code of the compute entry point will
               --  include the sentting of the thread OUT ports.

               Make_Ports_Compute_Entrypoint;
               if Has_Out_Ports (E) then
                  Make_Set_Out_Ports;
                  Make_Send_Out_Ports;
               end if;

            when others =>
               raise Program_Error with "Unconsistency in Task_Job_Body";
         end case;

         --  Define Error variable

         if Need_Error_Initialization then
            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (VN (V_Error)),
                 Constant_Present  => True,
                 Object_Definition => RE (RE_Error_Kind),
                 Expression        => RE (RE_Error_None));
            Append_Node_To_List (N, Declarations);
         else
            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (VN (V_Error)),
                 Object_Definition => RE (RE_Error_Kind));
            Append_Node_To_List (N, Declarations);
         end if;

         N := Make_Used_Type (RE (RE_Error_Kind));
         Append_Node_To_List (N, Declarations);

         --  Return default error code: at this point, everything
         --  has been properly handled.

         N := Message_Comment ("Return error code");
         Append_Node_To_List (N, Statements);

         N :=
           Make_Assignment_Statement
             (Variable_Identifier =>
                Make_Defining_Identifier (PN (P_Result)),
              Expression => Make_Defining_Identifier (VN (V_Error)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Declarations, Statements);
         return N;
      end Task_Job_Body;

      -----------------------------------
      -- Make_Current_Mode_Declaration --
      -----------------------------------

      function Make_Current_Mode_Declaration (E : Node_Id) return Node_Id is
         M : Node_Id;
         N : Node_Id;
      begin
         --  The value of the global variable is the enumeratioin
         --  literal corresponding to the initial mode of the thread.

         M := First_Node (Modes (E));
         N := No_Node;

         while Present (M) loop
            if Is_Initial (M) then
               N := Map_Ada_Defining_Identifier (M);
               exit;
            end if;

            M := Next_Node (M);
         end loop;

         --  If no initial mode has been found, there is definitely an
         --  error in the analyzer.

         if No (N) then
            raise Program_Error with "No initial mode in mode list";
         end if;

         --  Declare the variable

         Current_Mode_Identifier :=
           Make_Defining_Identifier (Map_Current_Mode_Name (E));

         N :=
           Make_Object_Declaration
             (Defining_Identifier => Current_Mode_Identifier,
              Object_Definition   =>
                Make_Defining_Identifier (Map_Modes_Enumeration_Name (E)),
              Expression => N);

         return N;
      end Make_Current_Mode_Declaration;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
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
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : constant Node_Id := Get_Implementation (E);

      begin
         if Implementation /= No_Node then

            --  A device may be "implemented" using an abstract
            --  component, representing its driver. We iterate on its
            --  subcomponents to attach specific threads associated.

            Visit_Subcomponents_Of (Implementation);
         end if;
      end Visit_Device_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P          : constant Node_Id := ADN.Entity (U);
         S          : Node_Id;
         N          : Node_Id;
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));

      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Job_Body;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         --  Reset hybrid thread related global variables

         Has_Hybrid_Threads       := False;
         Hybrid_Thread_Elements   := No_List;
         Last_Hybrid_Thread_Index := 0;

         --  Initialize the runtime routine list

         Interrogation_Routine_List := New_List (ADN.K_Statement_List);

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         --  Append the runtime routines

         Append_Node_To_List
           (ADN.First_Node (Interrogation_Routine_List),
            ADN.Statements (Current_Package));

         if Has_Hybrid_Threads then
            declare
               Profile : constant List_Id := New_List (ADN.K_List_Id);
            begin
               pragma Assert (not Is_Empty (Hybrid_Thread_Elements));

               N :=
                 Message_Comment
                   ("In order for them to work correctly," &
                    " hybrid task need the presence of" &
                    " a driver task to trigger them at" &
                    " their period");
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Declare the hybrid task set

               N :=
                 Make_Object_Declaration
                   (Defining_Identifier =>
                      Make_Defining_Identifier (PN (P_Hybrid_Task_Set)),
                    Object_Definition => RE (RE_Hybrid_Task_Info_Array),
                    Expression        =>
                      Make_Array_Aggregate (Hybrid_Thread_Elements));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Instantiate the hybrid task driver

               N := Make_Defining_Identifier (PN (P_Hybrid_Task_Set));
               Append_Node_To_List (N, Profile);

               N := Make_Attribute_Designator (RE (RE_Priority), A_Last);
               Append_Node_To_List (N, Profile);

               N := Make_Literal (New_Integer_Value (128_000, 1, 10));
               Append_Node_To_List (N, Profile);

               N := RE (RE_Deliver);
               Append_Node_To_List (N, Profile);

               N :=
                 Make_Package_Instantiation
                   (Make_Defining_Identifier (PN (P_Hybrid_Task_Driver)),
                    RU (RU_PolyORB_HI_Hybrid_Task_Driver_Driver),
                    Profile);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               N :=
                 Make_Pragma_Statement
                   (Pragma_Unreferenced,
                    Make_List_Id
                      (Make_Defining_Identifier (PN (P_Hybrid_Task_Driver))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));
            end;
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            S := First_Node (Subcomponents (The_System));
            while Present (S) loop
               if AAU.Is_Device (Corresponding_Instance (S))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (S)) =
                   Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
      begin
         Push_Entity (Ada_Root);

         --  Visit all the subcomponents of the system

         Visit_Subcomponents_Of (E);

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         P : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
      begin
         case P is
            when Thread_Periodic =>
               N :=
                 Message_Comment
                   ("Periodic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

            when Thread_Sporadic =>
               N :=
                 Message_Comment
                   ("Sporadic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

            when Thread_Aperiodic =>
               N :=
                 Message_Comment
                   ("Aperiodic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

            when Thread_Background =>
               N :=
                 Message_Comment
                   ("Background task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

            when Thread_ISR =>
               N :=
                 Message_Comment
                   ("ISR task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

            when Thread_Hybrid =>
               N :=
                 Message_Comment
                   ("Hybrid task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Hybrid threads requires an extra driver thread to be
               --  created.

               declare
                  Aggr : constant List_Id := New_List (ADN.K_Component_List);
               begin
                  Has_Hybrid_Threads := True;

                  if Hybrid_Thread_Elements = No_List then
                     Hybrid_Thread_Elements := New_List (ADN.K_Element_List);
                  end if;

                  --  Append the element association corresponding to
                  --  E to the hybrid task set.

                  N := Extract_Enumerator (E);
                  Append_Node_To_List (N, Aggr);

                  --  We know that the last node added to the feature
                  --  list of E is the one appended at exapnsion time
                  --  and corresponding to the fake event part that
                  --  will receive the dispatch messages from the
                  --  driver.

                  N := Extract_Enumerator (Last_Node (Features (E)));
                  Append_Node_To_List (N, Aggr);

                  N := Map_Ada_Time (Get_Thread_Period (E));
                  Append_Node_To_List (N, Aggr);

                  N := RE (RE_System_Startup_Time);
                  Append_Node_To_List (N, Aggr);

                  N := RE (RE_True);
                  Append_Node_To_List (N, Aggr);

                  N :=
                    Make_Qualified_Expression
                      (RE (RE_Hybrid_Task_Info),
                       Make_Record_Aggregate (Aggr));

                  Last_Hybrid_Thread_Index := Last_Hybrid_Thread_Index + 1;

                  N :=
                    Make_Element_Association
                      (Make_Literal
                         (New_Integer_Value (Last_Hybrid_Thread_Index, 1, 10)),
                       N);
                  Append_Node_To_List (N, Hybrid_Thread_Elements);
               end;

            when others =>
               raise Program_Error;
         end case;

         declare
            Activate_Entrypoint : constant Name_Id :=
              Get_Thread_Activate_Entrypoint (E);
         begin
            --  If the thread has been assigned an initialize
            --  entrypoint, we complete the subprogram renaming
            --  initiated in the spec.

            if Activate_Entrypoint /= No_Name then
               N :=
                 Make_Subprogram_Specification
                   (Defining_Identifier => Map_Task_Init_Identifier (E),
                    Parameter_Profile   => No_List,
                    Return_Type         => No_Node,
                    Renamed_Subprogram  =>
                      Map_Ada_Subprogram_Identifier (Activate_Entrypoint));
               Append_Node_To_List (N, ADN.Statements (Current_Package));
            end if;
         end;

         declare
            Rec_Entrypoint : constant Name_Id :=
              Get_Thread_Recover_Entrypoint (E);
         begin
            --  If the thread has been assigned a recover
            --  entrypoint, we complete the subprogram renaming
            --  initiated in the spec.

            if Rec_Entrypoint /= No_Name then
               N :=
                 Make_Subprogram_Specification
                   (Defining_Identifier => Map_Task_Recover_Identifier (E),
                    Parameter_Profile   => No_List,
                    Return_Type         => No_Node,
                    Renamed_Subprogram  =>
                      Map_Ada_Subprogram_Identifier (Rec_Entrypoint));
               Append_Node_To_List (N, ADN.Statements (Current_Package));
            end if;
         end;

         if Has_Modes (E) then
            --  If the thread has operational modes, then generate the
            --  body of the mode updater procedure and the global
            --  variable designating the current mode. there is no
            --  harm using a global variable because
            --  it is accessed exclusively by the thread.
            --  We also with a package instance of teh corresponding
            --  scheduler

            N := Make_Current_Mode_Declaration (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            if Is_Fusioned (E) then

               N := Make_Mode_Updater_body (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               N :=
                 Make_Withed_Package
                   (Make_Defining_Identifier
                      (Map_Scheduler_Instance_Name (E)));
               Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));
            end if;
         end if;

         --  Create the body of the parameterless subprogram that
         --  executes the thread job.
         --
         --  Note it is added in the Interrogation_Routine_List to
         --  avoid issues with Ada 2012 freezing rules.

         N := Task_Job_Body (E);
         Append_Node_To_List (N, Interrogation_Routine_List);
      end Visit_Thread_Instance;

      ----------------------------
      -- Make_Mode_Updater_Body --
      ----------------------------

      function Make_Mode_Updater_body (E : Node_Id) return Node_Id is
         N    : Node_Id;
         Spec : constant Node_Id :=
           Backend_Node (Identifier (First_Node (Modes (E))));
         Stats : constant List_Id := New_List (ADN.K_List_Id);
      begin
         N :=
           Make_Assignment_Statement
             (Variable_Identifier => Current_Mode_Identifier,
              Expression          => Make_Defining_Identifier (PN (P_Mode)));
         Append_Node_To_List (N, Stats);
         N := Make_Subprogram_Implementation (Spec, No_List, Stats);
         return N;
      end Make_Mode_Updater_body;

   end Package_Body;

end Ocarina.Backends.PO_HI_Ada.Job;
