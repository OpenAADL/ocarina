------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . P R O P E R T I E S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Locations;
with Ocarina.Namet;
with Utils; use Utils;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.AADL_Values;
with Ocarina.Instances.Queries;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;

with Ocarina.Backends.Properties.Utils;
use Ocarina.Backends.Properties.Utils;

package body Ocarina.Backends.Properties is

   use Locations;
   use Ocarina.Namet;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
   use Ocarina.Instances.Queries;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIEP renames Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
   package ATEP renames Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

   ----------------------------------
   -- Several component properties --
   ----------------------------------

   AADL_Priority : Name_Id;
   --  Thread and Data

   Compute_Entrypoint_Source_Text_Name : Name_Id;
   Compute_Entrypoint_Name             : Name_Id;
   Compute_Entrypoint_Call_Sequence    : Name_Id;
   --  Thread and IN [event] [data] ports

   Source_Language  : Name_Id;
   Source_Name      : Name_Id;
   T_Source_Name    : Name_Id;
   Type_Source_Name : Name_Id;
   Source_Text      : Name_Id;
   Fusion_Occurred  : Name_Id;
   Priority_Shifter : Name_Id;
   Scheduler_Name   : Name_Id;
   Original_Name    : Name_Id;
   --  Subprogram, thread, data, port...

   Implemented_As     : Name_Id;
   Device_Driver_Name : Name_Id;

   -------------------------------
   -- Data component properties --
   -------------------------------

   Base_Type                 : Name_Id;
   Base_Address              : Name_Id;
   Memory_Size               : Name_Id;
   Code_Set                  : Name_Id;
   Data_Digits               : Name_Id;
   Data_Scale                : Name_Id;
   Data_Representation       : Name_Id;
   Dimension                 : Name_Id;
   Element_Names             : Name_Id;
   Enumerators               : Name_Id;
   IEEE754_Precision         : Name_Id;
   Initial_Value             : Name_Id;
   Integer_Range             : Name_Id;
   Measurement_Unit          : Name_Id;
   Number_Representation     : Name_Id;
   Real_Range                : Name_Id;
   Data_Required_Access      : Name_Id;
   Data_Provided_Access      : Name_Id;
   Source_Data_Size          : Name_Id;
   Data_Size                 : Name_Id;
   Code_Size                 : Name_Id;
   Data_Concurrency_Protocol : Name_Id;

   ---------------------------------
   -- Thread component properties --
   ---------------------------------

   Dispatch_Offset                       : Name_Id;
   Thread_Period                         : Name_Id;
   Thread_Dispatch_Absolute_Time         : Name_Id;
   Thread_Cheddar_Dispatch_Absolute_Time : Name_Id;
   Thread_Deadline                       : Name_Id;
   Thread_Dispatch_Protocol              : Name_Id;
   Thread_Cheddar_Priority               : Name_Id;
   Thread_Stack_Size                     : Name_Id;
   Activate_Entrypoint                   : Name_Id;
   Activate_Entrypoint_Source_Text       : Name_Id;
   Initialize_Entrypoint                 : Name_Id;
   Initialize_Entrypoint_Source_Text     : Name_Id;
   Recover_Entrypoint                    : Name_Id;
   Recover_Entrypoint_Source_Text        : Name_Id;

   POSIX_Scheduling_Policy         : Name_Id;
   Cheddar_POSIX_Scheduling_Policy : Name_Id;
   SCHED_FIFO_Name                 : Name_Id;
   SCHED_RR_Name                   : Name_Id;
   SCHED_Others_Name               : Name_Id;

   ----------------------------------
   -- Process component properties --
   ----------------------------------

   Port_Number             : Name_Id;
   Processor_Binding       : Name_Id;
   Function_Binding        : Name_Id;
   Memory_Binding          : Name_Id;

   Scheduling_Protocol                                   : Name_Id;
   PARAMETRIC_PROTOCOL_Name                              : Name_Id;
   EDF_Name                                              : Name_Id;
   EARLIEST_DEADLINE_FIRST_PROTOCOL_Name                 : Name_Id;
   LEAST_LAXITY_FIRST_PROTOCOL_Name                      : Name_Id;
   RMS_Name                                              : Name_Id;
   RATE_MONOTONIC_PROTOCOL_Name                          : Name_Id;
   DEADLINE_MONOTONIC_PROTOCOL_Name                      : Name_Id;
   ROUND_ROBIN_PROTOCOL_Name                             : Name_Id;
   TIME_SHARING_BASED_ON_WAIT_TIME_PROTOCOL_Name         : Name_Id;
   POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL_Name       : Name_Id;
   D_OVER_PROTOCOL_Name                                  : Name_Id;
   MAXIMUM_URGENCY_FIRST_BASED_ON_LAXITY_PROTOCOL_Name   : Name_Id;
   MAXIMUM_URGENCY_FIRST_BASED_ON_DEADLINE_PROTOCOL_Name : Name_Id;
   TIME_SHARING_BASED_ON_CPU_USAGE_PROTOCOL_Name         : Name_Id;
   NO_SCHEDULING_PROTOCOL_Name                           : Name_Id;
   HIERARCHICAL_CYCLIC_PROTOCOL_Name                     : Name_Id;
   HIERARCHICAL_ROUND_ROBIN_PROTOCOL_Name                : Name_Id;
   HIERARCHICAL_FIXED_PRIORITY_PROTOCOL_Name             : Name_Id;
   HIERARCHICAL_PARAMETRIC_PROTOCOL_Name                 : Name_Id;

   ---------------------------------
   -- Memory component properties --
   ---------------------------------

   Byte_Count : Name_Id;
   Word_Size  : Name_Id;

   ------------------------------------
   -- Processor component properties --
   ------------------------------------

   Location                  : Name_Id;
   Execution_Platform        : Name_Id;
   Scheduler_Quantum         : Name_Id;
   Cheddar_Scheduler_Quantum : Name_Id;

   ---------------------------------
   -- AADL Connections properties --
   ---------------------------------

   Connection_Binding : Name_Id;

   -------------------------------
   -- Bus components properties --
   -------------------------------

   Transport_API : Name_Id;

   ---------------------
   -- Port properties --
   ---------------------

   Queue_Size                                 : Name_Id;
   Overflow_Handling_Protocol                 : Name_Id;
   Overflow_Handling_Protocol_DropOldest_Name : Name_Id;
   Overflow_Handling_Protocol_DropNewest_Name : Name_Id;
   Overflow_Handling_Protocol_Error_Name      : Name_Id;
   Port_Urgency                               : Name_Id;
   Port_Timing                                : Name_Id;
   Port_Timing_Sampled_Name                   : Name_Id;
   Port_Timing_Immediate_Name                 : Name_Id;
   Port_Timing_Delayed_Name                   : Name_Id;

   ---------------------------------
   -- System component properties --
   ---------------------------------

   Protocol : Name_Id;

   --  All the Name_Id's below MUST be initialized in the Init
   --  procedure.

   Data_Array_Name     : Name_Id;
   Data_Boolean_Name   : Name_Id;
   Data_Character_Name : Name_Id;
   Data_Enum_Name      : Name_Id;
   Data_Float_Name     : Name_Id;
   Data_Fixed_Name     : Name_Id;
   Data_Integer_Name   : Name_Id;
   Data_String_Name    : Name_Id;
   Data_Struct_Name    : Name_Id;
   Data_Union_Name     : Name_Id;

   Access_Read_Only_Name  : Name_Id;
   Access_Write_Only_Name : Name_Id;
   Access_Read_Write_Name : Name_Id;
   Access_By_Method_Name  : Name_Id;

   Language_Ada_Name             : Name_Id;
   Language_Ada_95_Name          : Name_Id;
   Language_Ada_05_Name          : Name_Id;
   Language_ASN1_Name            : Name_Id;
   Language_C_Name               : Name_Id;
   Language_CPP_Name             : Name_Id;
   Language_SDL_OpenGEODE_Name   : Name_Id;
   Language_Device_Name          : Name_Id;
   Language_Esterel_Name         : Name_Id;
   Language_GUI_Name             : Name_Id;
   Language_LUA_Name             : Name_Id;
   Language_Lustre_Name          : Name_Id;
   Language_Lustre5_Name         : Name_Id;
   Language_Lustre6_Name         : Name_Id;
   Language_Rhapsody_Name        : Name_Id;
   Language_RTDS_Name            : Name_Id;
   Language_RTSJ_Name            : Name_Id;
   Language_Scade_Name           : Name_Id;
   Language_SCADE6_Name          : Name_Id;
   Language_SDL_Name             : Name_Id;
   Language_SDL_ObjectGeode_Name : Name_Id;
   Language_SDL_RTDS_Name        : Name_Id;
   Language_Simulink_Name        : Name_Id;
   Language_System_C_Name        : Name_Id;
   Language_VHDL_Name            : Name_Id;

   Thread_Periodic_Name   : Name_Id;
   Thread_Aperiodic_Name  : Name_Id;
   Thread_Sporadic_Name   : Name_Id;
   Thread_Hybrid_Name     : Name_Id;
   Thread_Timed_Name      : Name_Id;
   Thread_Interrupt_Name  : Name_Id;
   Thread_Background_Name : Name_Id;

   Time_Ps_Name  : Name_Id;
   Time_Ns_Name  : Name_Id;
   Time_Us_Name  : Name_Id;
   Time_Ms_Name  : Name_Id;
   Time_Sec_Name : Name_Id;
   Time_Min_Name : Name_Id;
   Time_Hr_Name  : Name_Id;

   Size_Bit_Name       : Name_Id;
   Size_Byte_Name      : Name_Id;
   Size_Kilo_Byte_Name : Name_Id;
   Size_Mega_Byte_Name : Name_Id;
   Size_Giga_Byte_Name : Name_Id;
   Size_Tera_Byte_Name : Name_Id;

   Platform_Native_Name                 : Name_Id;
   Platform_Bench_Name                  : Name_Id;
   Platform_Native_Compcert_Name        : Name_Id;
   Platform_LINUX32_Name                : Name_Id;
   Platform_Win32_Name                  : Name_Id;
   Platform_LINUX32_Xenomai_Native_Name : Name_Id;
   Platform_LINUX32_Xenomai_Posix_Name  : Name_Id;
   Platform_LINUX64_Name                : Name_Id;
   Platform_NDS_RTEMS_Name              : Name_Id;
   Platform_NDS_RTEMS_POSIX_Name        : Name_Id;
   Platform_Gumstix_RTEMS_Name          : Name_Id;
   Platform_Gumstix_RTEMS_POSIX_Name    : Name_Id;
   Platform_LEON_RTEMS_Name             : Name_Id;
   Platform_LEON_RTEMS_POSIX_Name       : Name_Id;
   Platform_X86_LINUXTASTE_Name         : Name_Id;
   Platform_X86_RTEMS_Name              : Name_Id;
   Platform_X86_RTEMS_POSIX_Name        : Name_Id;
   Platform_LEON_GNAT_Name              : Name_Id;
   Platform_LEON_ORK_Name               : Name_Id;
   Platform_LEON3_SCOC3_Name            : Name_Id;
   Platform_LEON3_XM3_Name              : Name_Id;
   Platform_LEON3_Xtratum_Name          : Name_Id;
   Platform_ERC32_ORK_Name              : Name_Id;
   Platform_ARM_DSLINUX_Name            : Name_Id;
   Platform_ARM_N770_Name               : Name_Id;
   Platform_MARTE_OS_Name               : Name_Id;
   Platform_Vxworks_Name                : Name_Id;

   Transport_BSD_Sockets_Name : Name_Id;
   Transport_SpaceWire_Name   : Name_Id;

   Protocol_IIOP_Name : Name_Id;
   Protocol_DIOP_Name : Name_Id;

   Provided_Virtual_Bus_Class       : Name_Id;
   Allowed_Connection_Binding_Class : Name_Id;
   Compute_Execution_Time           : Name_Id;
   Execution_Time                   : Name_Id;
   Compute_Deadline                 : Name_Id;

   Core_Id : Name_Id;

   --------------------
   -- SEI properties --
   --------------------

   SEI_Wait_For_All_Events : Name_Id;
   SEI_Stream_Miss_Rate    : Name_Id;

   ----------------------
   -- Scade properties --
   ----------------------

   Scade_Signal_Name : Name_Id;

   --------------------
   -- POK properties --
   --------------------

   POK_Arch_x86_Name         : Name_Id;
   POK_BSP_x86_qemu_Name     : Name_Id;
   POK_BSP_x86_qemu_vmm_Name : Name_Id;
   POK_Arch_Sparc_Name       : Name_Id;
   POK_BSP_Leon_Name         : Name_Id;
   POK_Arch_ppc_Name         : Name_Id;
   POK_BSP_prep_Name         : Name_Id;
   POK_Needed_Memory_Size    : Name_Id;
   POK_BSP                   : Name_Id;
   POK_Arch                  : Name_Id;
   POK_Source_Location       : Name_Id;
   POK_Timeslice             : Name_Id;
   POK_Security_Level        : Name_Id;
   POK_Mils_Verified         : Name_Id;
   POK_Refresh_Time          : Name_Id;
   POK_Scheduler             : Name_Id;
   POK_Scheduler_RMS_Name    : Name_Id;
   POK_Scheduler_EDF_Name    : Name_Id;
   POK_Scheduler_RR_Name     : Name_Id;
   POK_Scheduler_LLF_Name    : Name_Id;
   POK_Scheduler_Static_Name : Name_Id;
   POK_Slots                 : Name_Id;
   POK_Slots_Allocation      : Name_Id;
   POK_Major_Frame           : Name_Id;

   POK_Recovery_Errors                             : Name_Id;
   POK_Recovery_Error_Deadline                     : Name_Id;
   POK_Recovery_Error_Application                  : Name_Id;
   POK_Recovery_Error_Numeric                      : Name_Id;
   POK_Recovery_Error_Request                      : Name_Id;
   POK_Recovery_Error_Stack                        : Name_Id;
   POK_Recovery_Error_Memory                       : Name_Id;
   POK_Recovery_Error_Hardware                     : Name_Id;
   POK_Recovery_Error_Power                        : Name_Id;
   POK_Recovery_Error_Partition_Conf               : Name_Id;
   POK_Recovery_Error_Partition_Init               : Name_Id;
   POK_Recovery_Error_Partition_Sched              : Name_Id;
   POK_Recovery_Error_Partition_Proc               : Name_Id;
   POK_Recovery_Error_Kernel_Init                  : Name_Id;
   POK_Recovery_Error_Kernel_Sched                 : Name_Id;
   POK_Recovery_Actions                            : Name_Id;
   POK_Recovery_Action_Ignore                      : Name_Id;
   POK_Recovery_Action_Confirm                     : Name_Id;
   POK_Recovery_Action_Thread_Restart              : Name_Id;
   POK_Recovery_Action_Thread_Stop_And_Start_Other : Name_Id;
   POK_Recovery_Action_Thread_Stop                 : Name_Id;
   POK_Recovery_Action_Partition_Restart           : Name_Id;
   POK_Recovery_Action_Partition_Stop              : Name_Id;
   POK_Recovery_Action_Kernel_Stop                 : Name_Id;
   POK_Recovery_Action_Kernel_Restart              : Name_Id;
   POK_Recovery_Action_Nothing                     : Name_Id;

   -------------------------
   -- ARINC653 properties --
   -------------------------

   ARINC653_Module_Major_Frame               : Name_Id;
   ARINC653_Partition_Slots                  : Name_Id;
   ARINC653_Slots_Allocation                 : Name_Id;
   ARINC653_Sampling_Refresh_Period          : Name_Id;
   ARINC653_Timeout                          : Name_Id;
   ARINC653_Queuing_Discipline_Name          : Name_Id;
   ARINC653_Queuing_Discipline_Fifo_Name     : Name_Id;
   ARINC653_Queuing_Discipline_Priority_Name : Name_Id;
   ARINC653_Access_Type_Name                 : Name_Id;
   ARINC653_Access_Type_Read_Name            : Name_Id;
   ARINC653_Access_Type_Write_Name           : Name_Id;
   ARINC653_Access_Type_Read_Write_Name      : Name_Id;
   ARINC653_Memory_Kind_Name                 : Name_Id;
   ARINC653_Memory_Kind_Code_Name            : Name_Id;
   ARINC653_Memory_Kind_Data_Name            : Name_Id;

   ARINC653_Actions_Name                             : Name_Id;
   ARINC653_Module_Recovery_Actions_Name             : Name_Id;
   ARINC653_Partition_Recovery_Actions_Name          : Name_Id;
   ARINC653_Process_Recovery_Actions_Name            : Name_Id;
   ARINC653_Action_Ignore_Name                       : Name_Id;
   ARINC653_Action_Confirm_Name                      : Name_Id;
   ARINC653_Action_Partition_Stop_Name               : Name_Id;
   ARINC653_Action_Module_Stop_Name                  : Name_Id;
   ARINC653_Action_Process_Stop_Name                 : Name_Id;
   ARINC653_Action_Process_Stop_And_Start_Other_Name : Name_Id;
   ARINC653_Action_Process_Restart_Name              : Name_Id;
   ARINC653_Action_Partition_Restart_Name            : Name_Id;
   ARINC653_Action_Cold_Restart_Name                 : Name_Id;
   ARINC653_Action_Warm_Restart_Name                 : Name_Id;
   ARINC653_Action_Module_Restart_Name               : Name_Id;
   ARINC653_Action_Nothing_Name                      : Name_Id;
   ARINC653_Action_Reset_Name                        : Name_Id;
   ARINC653_Action_Stop_Name                         : Name_Id;

   ARINC653_Errors_Name                     : Name_Id;
   ARINC653_Error_Module_Init_Name          : Name_Id;
   ARINC653_Error_Module_Config_Name        : Name_Id;
   ARINC653_Error_Module_Scheduling_Name    : Name_Id;
   ARINC653_Error_Partition_Config_Name     : Name_Id;
   ARINC653_Error_Partition_Init_Name       : Name_Id;
   ARINC653_Error_Partition_Scheduling_Name : Name_Id;
   ARINC653_Error_Partition_Handler_Name    : Name_Id;
   ARINC653_Error_Deadline_Miss_Name        : Name_Id;
   ARINC653_Error_Application_Name          : Name_Id;
   ARINC653_Error_Numeric_Name              : Name_Id;
   ARINC653_Error_Invalid_Request_Name      : Name_Id;
   ARINC653_Error_Stack_Overflow_Name       : Name_Id;
   ARINC653_Error_Memory_Violation_Name     : Name_Id;
   ARINC653_Error_Hardware_Fault_Name       : Name_Id;
   ARINC653_Error_Power_Fail_Name           : Name_Id;

   function Get_Size_Property_Value
     (C             : Node_Id;
      Property_Name : Name_Id) return Size_Type;
   --  Code factorization between thread and data interrogators

   function Get_Compute_Entrypoint
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   function Get_Compute_Entrypoint
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Code factorization between thread and port interrogators
   --  Note that in case of call sequence reference,
   --  it will return the call sequence's single (by construction)
   --  subprogram call

   function Is_Defined_Entrypoint_Property
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Boolean;

   function Get_Time_Property_Value
     (E             : Node_Id;
      Property_Name : Name_Id) return Time_Type;
   --  Return the value of the property "Property_Name" whose AADL
   --  type is Time.

   function Get_Priority (E : Node_Id) return Unsigned_Long_Long;
   --  Code factorization between thread and data components

   -----------------------------
   -- Get_Size_Property_Value --
   -----------------------------

   function Get_Size_Property_Value
     (C             : Node_Id;
      Property_Name : Name_Id) return Size_Type
   is
      V      : Node_Id;
      U      : Node_Id;
      Result : Size_Type;
      N      : Name_Id;
   begin
      if Is_Defined_Integer_Property (C, Property_Name) then
         V := Get_Value_Of_Property_Association (C, Property_Name);

         if Present (V) and then Present (Unit_Identifier (V)) then
            U := Unit_Identifier (V);

            --  Get the size

            Result.S := Get_Integer_Property (C, Property_Name);

            --  Convert the value to its unit

            N := ATN.Name (U);

            if N = Size_Bit_Name then
               Result.U := Bit;
            elsif N = Size_Byte_Name then
               Result.U := Byte;
            elsif N = Size_Kilo_Byte_Name then
               Result.U := Kilo_Byte;
            elsif N = Size_Mega_Byte_Name then
               Result.U := Mega_Byte;
            elsif N = Size_Giga_Byte_Name then
               Result.U := Giga_Byte;
            elsif N = Size_Tera_Byte_Name then
               Result.U := Tera_Byte;
            else
               Display_Located_Error
                 (AIN.Loc (U),
                  "Wrong unit",
                  Fatal => True);
               return ((0, Bit));
            end if;

            return Result;
         else
            Display_Located_Error
              (AIN.Loc (U),
               "A size without unit",
               Fatal => True);
            return ((0, Bit));
         end if;
      else
         return Null_Size;
      end if;
   end Get_Size_Property_Value;

   ----------------------------
   -- Get_Compute_Entrypoint --
   ----------------------------

   function Get_Compute_Entrypoint
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id
   is
      Spg_Classifier : Node_Id;
   begin
      if Is_Defined_String_Property
          (E,
           Compute_Entrypoint_Source_Text_Name,
           In_Mode)
      then
         return Get_String_Property
             (E,
              Compute_Entrypoint_Source_Text_Name,
              In_Mode);

      elsif Is_Defined_Property (E, Compute_Entrypoint_Name, In_Mode) then
         Spg_Classifier :=
           Get_Classifier_Property (E, Compute_Entrypoint_Name, In_Mode);
         return Get_Source_Name (Spg_Classifier);
      end if;

      return No_Name;
   end Get_Compute_Entrypoint;

   function Get_Compute_Entrypoint
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
   begin
      if Kind (E) = K_Port_Spec_Instance and then not AIN.Is_In (E) then
         Display_Located_Error
           (AIN.Loc (E),
            "Compute entrypoint cannot be specified for OUT-only ports",
            Fatal => True);
      end if;

      if Is_Defined_String_Property
          (E,
           Compute_Entrypoint_Source_Text_Name,
           In_Mode)
      then
         return Get_Property_Association
             (E,
              Compute_Entrypoint_Source_Text_Name,
              In_Mode);

      elsif Is_Defined_Reference_Property
          (E,
           Compute_Entrypoint_Call_Sequence,
           In_Mode)
      then
         return Get_Property_Association
             (E,
              Compute_Entrypoint_Call_Sequence,
              In_Mode);

      elsif Is_Defined_Property (E, Compute_Entrypoint_Name, In_Mode) then
         return Get_Classifier_Property (E, Compute_Entrypoint_Name, In_Mode);

      else
         return No_Node;

      end if;
   end Get_Compute_Entrypoint;

   ------------------------------------
   -- Is_Defined_Entrypoint_Property --
   ------------------------------------

   function Is_Defined_Entrypoint_Property
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
   begin
      return Is_Defined_String_Property
          (E,
           Compute_Entrypoint_Source_Text_Name,
           In_Mode)
        or else Is_Defined_Reference_Property
          (E,
           Compute_Entrypoint_Call_Sequence,
           In_Mode)
        or else Is_Defined_Property (E, Compute_Entrypoint_Name, In_Mode);
   end Is_Defined_Entrypoint_Property;

   -------------------
   -- Get_Base_Type --
   -------------------

   function Get_Base_Type (D : Node_Id) return List_Id is
      pragma Assert (AINU.Is_Data (D));
   begin
      return Check_And_Get_Property (D, Base_Type);
   end Get_Base_Type;

   ------------------
   -- Get_Code_Set --
   ------------------

   function Get_Code_Set (D : Node_Id) return Unsigned_Long_Long is
      pragma Assert (AINU.Is_Data (D));
   begin
      return Check_And_Get_Property (D, Code_Set);
   end Get_Code_Set;

   ---------------------
   -- Get_Data_Digits --
   ---------------------

   function Get_Data_Digits (D : Node_Id) return Unsigned_Long_Long is
      pragma Assert (Get_Data_Representation (D) = Data_Fixed);
   begin
      return Check_And_Get_Property (D, Data_Digits);
   end Get_Data_Digits;

   --------------------
   -- Get_Data_Scale --
   --------------------

   function Get_Data_Scale (D : Node_Id) return Unsigned_Long_Long is
      pragma Assert (Get_Data_Representation (D) = Data_Fixed);
   begin
      return Check_And_Get_Property (D, Data_Scale);
   end Get_Data_Scale;

   -----------------------------
   -- Get_Data_Representation --
   -----------------------------

   function Get_Data_Representation
     (D : Node_Id) return Supported_Data_Representation
   is
      F      : Node_Id;
      T_Name : Name_Id;
   begin
      pragma Assert (AINU.Is_Data (D));

      if Is_Defined_Enumeration_Property (D, Data_Representation) then
         T_Name := Get_Enumeration_Property (D, Data_Representation);

         --  Although Name_Id is a scalar type, we cannot use a switch
         --  case since the <..>_Name global variables are not (and
         --  cannot be) static.

         if T_Name = Data_Array_Name then
            if not Is_Defined_List_Property (D, Dimension) then
               Display_Located_Error
                 (ATN.Loc (D),
                  "Array types must have one or more dimensions",
                  Fatal => True);
            end if;

            if ATNU.Length (Get_Base_Type (D)) /= 1 then
               Display_Located_Error
                 (ATN.Loc (D),
                  "Array types must have an element type",
                  Fatal => True);
            end if;

            return Data_Array;

         elsif T_Name = Data_Boolean_Name then
            return Data_Boolean;

         elsif T_Name = Data_Character_Name then
            if Get_Code_Set (D) = 16#0# or else Get_Code_Set (D) = 16#1# then
               --  PCS: Portable Character Set
               return Data_Character;
            else
               return Data_Wide_Character;
            end if;

         elsif T_Name = Data_Enum_Name then
            if Get_Enumerators (D)'Length = 0 then
               Display_Located_Error
                 (ATN.Loc (D),
                  "Enumeration type must be assigned an enumerator list",
                  Fatal => True);
            end if;

            return Data_Enum;

         elsif T_Name = Data_Float_Name then
            return Data_Float;

         elsif T_Name = Data_Fixed_Name then
            return Data_Fixed;

         elsif T_Name = Data_Integer_Name then
            return Data_Integer;

         elsif T_Name = Data_String_Name then
            if not Is_Defined_List_Property (D, Dimension) then
               Display_Located_Error
                 (ATN.Loc (D),
                  "String types must have a maximum length",
                  Fatal => True);
            end if;

            if Get_Code_Set (D) = 16#0# or else Get_Code_Set (D) = 16#1# then
               --  PCS: Portable Character Set
               return Data_String;
            else
               return Data_Wide_String;
            end if;

         elsif T_Name = Data_Struct_Name then
            declare
               Fields : constant Name_Array := Get_Element_Names (D);
               Types  : constant List_Id    := Get_Base_Type (D);
            begin
               --  Some trivial case elimination and coherency checking

               if Fields'Length /= ATNU.Length (Types) then
                  Display_Located_Error
                    (AIN.Loc (D),
                     "The number of elements and their corresponding" &
                     " types ar fifferent in this data structure",
                     Fatal => True);
               elsif Fields'Length = 0
                 and then Is_Empty (AIN.Subcomponents (D))
               then
                  Display_Located_Error
                    (AIN.Loc (D),
                     "Data structures must be defined using the" &
                     " subcomponents or the Base_Type/Element_Names" &
                     " properties.",
                     Fatal => True);
               end if;

               if Is_Empty (AIN.Features (D)) then
                  return Data_Struct;
               else
                  --  Check whether it is a protected type

                  F := AIN.First_Node (AIN.Features (D));

                  while Present (F) loop
                     if Kind (F) /= K_Subprogram_Spec_Instance
                       and then Kind (F) /= K_Subcomponent_Access_Instance
                     then
                        Display_Located_Error
                          (AIN.Loc (F),
                           "Unsupported feature kind for data type",
                           Fatal => True);
                     end if;

                     F := AIN.Next_Node (F);
                  end loop;

                  return Data_With_Accessors;
               end if;
            end;

         elsif T_Name = Data_Union_Name then
            return Data_Union;
         else
            Display_Located_Error
              (AIN.Loc (D),
               "Unknown data type",
               Fatal => True);
            return Data_None;
         end if;
      else
         return Data_None;
      end if;
   end Get_Data_Representation;

   -------------------
   -- Get_Dimension --
   -------------------

   function Get_Dimension (D : Node_Id) return ULL_Array is
      pragma Assert (AINU.Is_Data (D));
   begin
      return Check_And_Get_Property (D, Dimension);
   end Get_Dimension;

   -----------------------
   -- Get_Element_Names --
   -----------------------

   function Get_Element_Names (D : Node_Id) return Name_Array is
      pragma Assert (AINU.Is_Data (D));

   begin
      return Check_And_Get_Property (D, Element_Names);
   end Get_Element_Names;

   ---------------------
   -- Get_Enumerators --
   ---------------------

   function Get_Enumerators (D : Node_Id) return Name_Array is
      pragma Assert (AINU.Is_Data (D));
   begin
      return Check_And_Get_Property (D, Enumerators);
   end Get_Enumerators;

   ---------------------------
   -- Get_IEEE754_Precision --
   ---------------------------

   function Get_IEEE754_Precision
     (D : Node_Id) return Supported_IEEE754_Precision
   is
      pragma Assert (AINU.Is_Data (D));

      function Get_IEEE754_Precision_Enumerator is new
        Check_And_Get_Property_Enumerator (T => Supported_IEEE754_Precision);
   begin
      return Get_IEEE754_Precision_Enumerator (D, IEEE754_Precision);
   end Get_IEEE754_Precision;

   -----------------------
   -- Get_Initial_Value --
   -----------------------

   function Get_Initial_Value (D : Node_Id) return Name_Array is
      pragma Assert (AINU.Is_Data (D));

   begin
      return Check_And_Get_Property (D, Initial_Value);
   end Get_Initial_Value;

   -----------------------
   -- Get_Integer_Range --
   -----------------------

   function Get_Integer_Range (D : Node_Id) return LL_Array is
      pragma Assert (AINU.Is_Data (D));
   begin
      return Check_And_Get_Property (D, Integer_Range);
   end Get_Integer_Range;

   -------------------------
   -- Get_Mesurement_Unit --
   -------------------------

   function Get_Mesurement_Unit (D : Node_Id) return Name_Id is
      pragma Assert (AINU.Is_Data (D));
   begin
      return Check_And_Get_Property (D, Measurement_Unit);
   end Get_Mesurement_Unit;

   -------------------------------
   -- Get_Number_Representation --
   -------------------------------

   function Get_Number_Representation
     (D : Node_Id) return Supported_Number_Representation
   is
      pragma Assert (AINU.Is_Data (D));

      function Get_Number_Representation_Enumerator is new
        Check_And_Get_Property_Enumerator_With_Default
        (T => Supported_Number_Representation,
        Default_Value => None);

   begin
      return Get_Number_Representation_Enumerator (D, Number_Representation);
   end Get_Number_Representation;

   --------------------
   -- Get_Real_Range --
   --------------------

   function Get_Real_Range (D : Node_Id) return LD_Array is
      pragma Assert (AINU.Is_Data (D));
   begin
      return Check_And_Get_Property (D, Real_Range);
   end Get_Real_Range;

   -------------------
   -- Get_Data_Size --
   -------------------

   function Get_Data_Size (D : Node_Id) return Size_Type is
      Ret : Size_Type;
   begin
      pragma Assert (AINU.Is_Data (D) or else AINU.Is_Process (D));

      Ret := Get_Size_Property_Value (D, Data_Size);

      if Ret = Null_Size then
         Ret := Get_Size_Property_Value (D, Source_Data_Size);
      end if;

      return Ret;
   end Get_Data_Size;

   -------------------
   -- Get_Code_Size --
   -------------------

   function Get_Code_Size (E : Node_Id) return Size_Type is
   begin
      return Get_Size_Property_Value (E, Code_Size);
   end Get_Code_Size;

   ------------------------------------
   -- Get_Overflow_Handling_Protocol --
   ------------------------------------

   function Get_Overflow_Handling_Protocol
     (P : Node_Id) return Supported_Overflow_Handling_Protocol
   is
      OHP_Name : Name_Id;
   begin
      if Is_Defined_Enumeration_Property (P, Overflow_Handling_Protocol) then
         OHP_Name := Get_Enumeration_Property (P, Overflow_Handling_Protocol);

         if OHP_Name = Overflow_Handling_Protocol_DropOldest_Name then
            return Overflow_Handling_Protocol_DropOldest;
         elsif OHP_Name = Overflow_Handling_Protocol_DropNewest_Name then
            return Overflow_Handling_Protocol_DropNewest;
         elsif OHP_Name = Overflow_Handling_Protocol_Error_Name then
            return Overflow_Handling_Protocol_Error;
         else
            Display_Located_Error
              (AIN.Loc (P),
               "Unknown port overflow handling protocol name",
               Fatal => True);
            return Overflow_Handling_Protocol_DropOldest;
         end if;
      else
         return Overflow_Handling_Protocol_DropOldest;
      end if;
   end Get_Overflow_Handling_Protocol;

   ----------------------------------------
   -- Get_Priority_Celing_Of_Data_Access --
   ----------------------------------------

   function Get_Priority_Celing_Of_Data_Access
     (D : Node_Id) return Unsigned_Long_Long
   is
      Prio : Unsigned_Long_Long;
   begin
      pragma Assert (AINU.Is_Data (D));

      Prio := Get_Priority (D);

      if Prio /= 0
        and then Get_Concurrency_Protocol (D) /= Priority_Ceiling
      then
         Display_Located_Error
           (AIN.Loc (D),
            "Inconsistent definition of data types: should define " &
            "Concurrency_Control_Protocol to " &
            "Concurrency_Priority_Ceiling",
            Fatal => True);
      end if;

      return Prio;
   end Get_Priority_Celing_Of_Data_Access;

   ------------------------------
   -- Get_Provided_Data_Access --
   ------------------------------

   function Get_Provided_Data_Access
     (D : Node_Id) return Supported_Data_Access
   is
      T_Name : Name_Id;
   begin
      pragma Assert (AINU.Is_Data (D));

      if Is_Defined_Enumeration_Property (D, Data_Provided_Access) then
         T_Name := Get_Enumeration_Property (D, Data_Provided_Access);

         if T_Name = Access_Read_Only_Name then
            return Access_Read_Only;
         elsif T_Name = Access_Write_Only_Name then
            return Access_Write_Only;
         elsif T_Name = Access_Read_Write_Name then
            return Access_Read_Write;
         elsif T_Name = Access_By_Method_Name then
            return Access_By_Method;
         else
            Display_Located_Error
              (AIN.Loc (D),
               "Unknown access type",
               Fatal => True);
            return Access_None;
         end if;
      else
         return Access_None;
      end if;
   end Get_Provided_Data_Access;

   ------------------------------
   -- Get_Required_Data_Access --
   ------------------------------

   function Get_Required_Data_Access
     (D : Node_Id) return Supported_Data_Access
   is
      pragma Assert (AINU.Is_Data (D));

      T_Name : Name_Id;
   begin
      if Is_Defined_Enumeration_Property (D, Data_Required_Access) then
         T_Name := Get_Enumeration_Property (D, Data_Required_Access);

         if T_Name = Access_Read_Only_Name then
            return Access_Read_Only;
         elsif T_Name = Access_Write_Only_Name then
            return Access_Write_Only;
         elsif T_Name = Access_Read_Write_Name then
            return Access_Read_Write;
         elsif T_Name = Access_By_Method_Name then
            return Access_By_Method;
         else
            Display_Located_Error
              (AIN.Loc (D),
               "Unknown access type",
               Fatal => True);
            return Access_None;
         end if;
      else
         return Access_None;
      end if;
   end Get_Required_Data_Access;

   ------------------------------
   -- Get_Concurrency_Protocol --
   ------------------------------

   function Get_Concurrency_Protocol
     (D : Node_Id) return Supported_Concurrency_Control_Protocol
   is
      pragma Assert (AINU.Is_Data (D));

      function Get_Concurrency_Protocol_Enumerator is new
        Check_And_Get_Property_Enumerator_With_Default
        (T => Supported_Concurrency_Control_Protocol,
         Default_Value => None_Specified);

   begin
      return Get_Concurrency_Protocol_Enumerator
        (D, Data_Concurrency_Protocol);
   end Get_Concurrency_Protocol;

   -------------------------
   -- Get_Source_Language --
   -------------------------

   function Get_Source_Language
     (E : Node_Id) return Supported_Source_Language
   is
      Source_L : Name_Id;
   begin
      if AADL_Version = AADL_V1
        and then Is_Defined_Enumeration_Property (E, Source_Language)
      then
         Source_L := Get_Enumeration_Property (E, Source_Language);

      elsif AADL_Version = AADL_V2
        and then Is_Defined_List_Property (E, Source_Language)
      then
         declare
            Source_Language_List : constant List_Id :=
              Get_List_Property (E, Source_Language);
         begin
            if ATNU.Length (Source_Language_List) > 1 then
               Display_Located_Error
                 (AIN.Loc (E),
                  "Cannot use more than one language",
                  Fatal => True);
            end if;
            Source_L :=
              ATN.Name
                (ATN.Identifier (ATN.First_Node (Source_Language_List)));
         end;
      else
         return Language_None;
      end if;

      if Source_L = Language_Ada_95_Name
        or else Source_L = Language_Ada_Name
        or else Source_L = Language_Ada_05_Name
      then
         --  All instances of Ada are aliased to Ada_95
         return Language_Ada_95;

      elsif Source_L = Language_ASN1_Name then
         return Language_ASN1;

      elsif Source_L = Language_Device_Name then
         return Language_Device;

      elsif Source_L = Language_Lustre_Name
        or else Source_L = Language_Lustre5_Name
        or else Source_L = Language_Lustre6_Name
        or else Source_L = Language_SCADE6_Name
      then
         return Language_Lustre;

      elsif Source_L = Language_Esterel_Name then
         return Language_Esterel;

      elsif Source_L = Language_SDL_Name
        or else Source_L = Language_SDL_ObjectGeode_Name
      then
         return Language_SDL;

      elsif Source_L = Language_RTDS_Name
        or else Source_L = Language_SDL_RTDS_Name
      then
         return Language_SDL_RTDS;

      elsif Source_L = Language_C_Name then
         return Language_C;

      elsif Source_L = Language_CPP_Name then
         return Language_CPP;

      elsif Source_L = Language_SDL_OpenGEODE_Name then
         return Language_SDL_OpenGEODE;

      elsif Source_L = Language_RTSJ_Name then
         return Language_RTSJ;

      elsif Source_L = Language_Simulink_Name then
         return Language_Simulink;

      elsif Source_L = Language_Scade_Name then
         return Language_Scade;

      elsif Source_L = Language_Rhapsody_Name then
         return Language_Rhapsody;

      elsif Source_L = Language_System_C_Name then
         return Language_System_C;

      elsif Source_L = Language_VHDL_Name then
         return Language_VHDL;

      elsif Source_L = Language_GUI_Name then
         return Language_Gui;

      elsif Source_L = Language_LUA_Name then
         return Language_Lua;

      else
         Display_Located_Error
           (AIN.Loc (E),
            "Unknown source language",
            Fatal => True);
         return Language_None;
      end if;
   end Get_Source_Language;

   -------------------------
   -- Get_Subprogram_Kind --
   -------------------------

   function Get_Subprogram_Kind
     (S : Node_Id) return Supported_Subprogram_Kind
   is
      Language : constant Supported_Source_Language := Get_Source_Language (S);
      Src_Name   : constant Name_Id                   := Get_Source_Name (S);
      T_Src_Name : constant Name_Id := Get_Transfo_Source_Name (S);
      Src_Files  : constant Name_Array                := Get_Source_Text (S);
   begin
      case Language is
         when Language_Ada_95 =>
            if Src_Name /= No_Name or else Src_Files'Length > 0 then
               if not Is_Empty (AIN.Calls (S))
                 and then not Is_Empty
                   (AIN.Subprogram_Calls (AIN.First_Node (AIN.Calls (S))))
               then
                  --  A subprogram having Ada 95 as implementation
                  --  language, an implementation name and a *non
                  --  null* call sequence list is a hybrid Ada 95
                  --  subprogram.

                  return Subprogram_Hybrid_Ada_95;
               else
                  --  A subprogram having Ada 95 as implementation
                  --  language, an implementation name and a *null*
                  --  call sequence list is an opaque Ada 95
                  --  subprogram.

                  return Subprogram_Opaque_Ada_95;
               end if;
            elsif T_Src_Name /= No_Name then

               return Subprogram_Opaque_Ada_95_Transfo;
            else
               --  A subprogram having Ada 95 as implementation
               --  language and a null source name and source text is
               --  a wrong built subprogram.

               return Subprogram_Unknown;
            end if;

         when Language_Lustre =>
            if not Is_Empty (AIN.Calls (S))
              and then not Is_Empty
                (AIN.Subprogram_Calls (AIN.First_Node (AIN.Calls (S))))
            then
               --  A subprogram having Lustre as implementation
               --  language, an implementation name and a *non
               --  null* call sequence list is not supported yet.

               return Subprogram_Unknown;
            else
               --  A subprogram having Lustre as implementation
               --  language, an implementation name and a *null*
               --  call sequence list is a Lustre Subprogram.
               --  Lustre subprogram are implemented in a
               --  different way in the backends. In PolyORB-HI-Ada
               --  it was used with ASN1 in the IST-ASSERT
               --  process. In POK, we generate glue code
               --  to call Lustre application code.

               return Subprogram_Lustre;
            end if;

         when Language_Esterel =>
            return Subprogram_Esterel;

         when Language_Lua =>
            return Subprogram_Lua;

         when Language_ASN1 =>
            --  A subprogram having ASN1 as implementation
            --  language is not supported.

            return Subprogram_Unknown;

         when Language_Device =>
            --  A subprogram having Device as implementation
            --  language is not supported.

            return Subprogram_Unknown;

         when Language_SDL        |
           Language_SDL_RTDS      |
           Language_System_C      |
           Language_SDL_OpenGEODE |
           Language_VHDL          =>
            --  A subprogram having this language as implementation
            --  language is not supported.

            return Subprogram_Unknown;

         when Language_C =>
            if Src_Name /= No_Name or else Src_Files'Length > 0 then
               if not Is_Empty (AIN.Calls (S))
                 and then not Is_Empty
                   (AIN.Subprogram_Calls (AIN.First_Node (AIN.Calls (S))))
               then
                  --  A subprogram having C as implementation
                  --  language, an implementation name and a *non
                  --  null* call sequence list is not supported yet.

                  return Subprogram_Unknown;
               else
                  --  A subprogram having C as implementation
                  --  language, an implementation name and a *null*
                  --  call sequence list is an opaque C subprogram.

                  return Subprogram_Opaque_C;
               end if;
            else
               --  A subprogram having C as implementation language
               --  and a null source name and a null source text is a
               --  wrong built subprogram.

               return Subprogram_Opaque_C;
            end if;

         when Language_CPP =>
            if Src_Name /= No_Name or else Src_Files'Length > 0 then
               if not Is_Empty (AIN.Calls (S))
                 and then not Is_Empty
                   (AIN.Subprogram_Calls (AIN.First_Node (AIN.Calls (S))))
               then
                  --  A subprogram having CPP as implementation
                  --  language, an implementation name and a *non
                  --  null* call sequence list is not supported yet.

                  return Subprogram_Unknown;
               else
                  --  A subprogram having CPP as implementation
                  --  language, an implementation name and a *null*
                  --  call sequence list is an opaque C subprogram.

                  return Subprogram_Opaque_CPP;
               end if;
            else
               --  A subprogram having CPP as implementation language
               --  and a null source name and a null source text is a
               --  wrong built subprogram.

               return Subprogram_Opaque_CPP;
            end if;

         when Language_RTSJ =>
            if Src_Name /= No_Name or else Src_Files'Length > 0 then
               if not Is_Empty (AIN.Calls (S))
                 and then not Is_Empty
                   (AIN.Subprogram_Calls (AIN.First_Node (AIN.Calls (S))))
               then
                  --  A subprogram having RTSJ as implementation
                  --  language, an implementation name and a *non
                  --  null* call sequence list is not supported yet.

                  return Subprogram_Unknown;
               else
                  --  A subprogram having RTSJ as implementation
                  --  language, an implementation name and a *null*
                  --  call sequence list is an opaque RTSJ subprogram

                  return Subprogram_Opaque_RTSJ;
               end if;
            else
               --  A subprogram having RTSJ as implementation language
               --  and a null source name and a null source text is a
               --  wrong built subprogram.

               return Subprogram_Unknown;
            end if;

         when Language_Simulink =>
            return Subprogram_Simulink;

         when Language_Scade =>
            return Subprogram_Scade;

         when Language_None =>
            if Src_Name /= No_Name or else Src_Files'Length > 0 then
               --  A subprogram having no implementation source
               --  language but a non null source name or an non null
               --  source text is assumed to use the same source
               --  language as the back-end.

               return Subprogram_Default;
            else
               if not Is_Empty (AIN.Calls (S))
                 and then not Is_Empty
                   (AIN.Subprogram_Calls (AIN.First_Node (AIN.Calls (S))))
               then
                  --  A subprogram having no implementation language,
                  --  no implementation name and a pure call sequence
                  --  subprogram. However the length of its call
                  --  sequence has to be exactly 1.

                  if Length (AIN.Calls (S)) = 1 then
                     return Subprogram_Pure_Call_Sequence;
                  else
                     return Subprogram_Unknown;
                  end if;
               else
                  --  A subprogram having no implementation language
                  --  and a *null* call sequence list is an empty
                  --  subprogram.

                  return Subprogram_Empty;
               end if;
            end if;

         when Language_Gui | Language_Rhapsody =>
            return Subprogram_Unknown;
      end case;
   end Get_Subprogram_Kind;

   -----------------
   -- Is_Fusioned --
   -----------------

   function Is_Fusioned (E : Node_Id) return Boolean is
   begin
      return Check_And_Get_Property (E, Fusion_Occurred);
   end Is_Fusioned;

   --------------------------
   -- Get_Thread_Scheduler --
   --------------------------

   function Get_Thread_Scheduler (E : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property (E, Scheduler_Name);
   end Get_Thread_Scheduler;

   -------------------------------
   -- Get_Thread_Reference_Name --
   -------------------------------

   function Get_Thread_Reference_Name (E : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property (E, Original_Name);
   end Get_Thread_Reference_Name;

   -------------------------
   -- Is_Priority_Shifter --
   -------------------------

   function Is_Priority_Shifter (E : Node_Id) return Boolean is
   begin
      return Check_And_Get_Property (E, Priority_Shifter);
   end Is_Priority_Shifter;

   ---------------------
   -- Get_Source_Name --
   ---------------------

   function Get_Source_Name (E : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property (E, Source_Name);
   end Get_Source_Name;

   -----------------------------
   -- Get_Transfo_Source_Name --
   -----------------------------

   function Get_Transfo_Source_Name (E : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property (E, T_Source_Name);
   end Get_Transfo_Source_Name;

   --------------------------
   -- Get_Type_Source_Name --
   --------------------------

   function Get_Type_Source_Name (E : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property (E, Type_Source_Name);
   end Get_Type_Source_Name;

   ---------------------
   -- Get_Source_Text --
   ---------------------

   function Get_Source_Text (E : Node_Id) return Name_Array is
   begin
      return Check_And_Get_Property (E, Source_Text);
   end Get_Source_Text;

   ----------------------------------
   -- Get_Thread_Dispatch_Protocol --
   ----------------------------------

   function Get_Thread_Dispatch_Protocol
     (T : Node_Id) return Supported_Thread_Dispatch_Protocol
   is
      P_Name : Name_Id;
   begin
      pragma Assert (Is_Thread (T));

      if Is_Defined_Enumeration_Property (T, Thread_Dispatch_Protocol) then
         P_Name := Get_Enumeration_Property (T, Thread_Dispatch_Protocol);

         if P_Name = Thread_Periodic_Name then
            if not Is_Defined_Integer_Property (T, Thread_Period) then
               Display_Located_Error
                 (AIN.Loc (T),
                  "Periodic threads must have a period",
                  Fatal => True);
            end if;

            return Thread_Periodic;

         elsif P_Name = Thread_Aperiodic_Name then
            return Thread_Aperiodic;

         elsif P_Name = Thread_Interrupt_Name then
            return Thread_ISR;

         elsif P_Name = Thread_Sporadic_Name then
            if not Is_Defined_Integer_Property (T, Thread_Period) then
               Display_Located_Error
                 (AIN.Loc (T),
                  "Sporadic threads must have a period",
                  Fatal => True);
            end if;

            return Thread_Sporadic;

         elsif P_Name = Thread_Hybrid_Name then
            if not Is_Defined_Integer_Property (T, Thread_Period) then
               Display_Located_Error
                 (AIN.Loc (T),
                  "Hybrid threads must have a period",
                  Fatal => True);
            end if;

            return Thread_Hybrid;
         elsif P_Name = Thread_Timed_Name then
            return Thread_Timed;

         elsif P_Name = Thread_Background_Name then
            return Thread_Background;

         else
            Display_Located_Error
              (AIN.Loc (T),
               "Unknown thread dispatch protocol",
               Fatal => True);
            return Thread_None;
         end if;
      else
         return Thread_None;
      end if;
   end Get_Thread_Dispatch_Protocol;

   ----------------------------------------
   -- Get_Thread_POSIX_Scheduling_Policy --
   ----------------------------------------

   function Get_Thread_POSIX_Scheduling_Policy
     (T : Node_Id) return Supported_POSIX_Scheduling_Policy
   is
      P_Name : Name_Id := No_Name;
   begin
      pragma Assert (Is_Thread (T));

      if Is_Defined_Enumeration_Property (T, POSIX_Scheduling_Policy) then
         P_Name := Get_Enumeration_Property (T, POSIX_Scheduling_Policy);
      elsif Is_Defined_Enumeration_Property
          (T,
           Cheddar_POSIX_Scheduling_Policy)
      then
         P_Name :=
           Get_Enumeration_Property (T, Cheddar_POSIX_Scheduling_Policy);
      end if;

      if P_Name /= No_Name then
         if P_Name = SCHED_FIFO_Name then
            return SCHED_FIFO;

         elsif P_Name = SCHED_RR_Name then
            return SCHED_RR;

         elsif P_Name = SCHED_Others_Name then
            return SCHED_OTHERS;
         end if;
      end if;

      return None;
   end Get_Thread_POSIX_Scheduling_Policy;

   --------------------------------
   -- Convert_Value_To_Time_Type --
   --------------------------------

   function Convert_Value_To_Time_Type (V : Node_Id) return Time_Type is
      N      : Name_Id;
      Result : Time_Type;
      U      : Node_Id;
   begin
      U := Unit_Identifier (V);

      --  Get the value

      Result.T := ATEP.Get_Integer_Of_Property_Value (V);

      --  Convert the value to its unit

      N := ATN.Name (U);

      if N = Time_Ps_Name then
         Result.U := Picosecond;
      elsif N = Time_Ns_Name then
         Result.U := Nanosecond;
      elsif N = Time_Us_Name then
         Result.U := Microsecond;
      elsif N = Time_Ms_Name then
         Result.U := Millisecond;
      elsif N = Time_Sec_Name then
         Result.U := Second;
      elsif N = Time_Min_Name then
         Result.U := Minute;
      elsif N = Time_Hr_Name then
         Result.U := Hour;
      else
         Display_Located_Error
           (AIN.Loc (V),
            "Unsupported unit",
            Fatal => True);
      end if;

      return Result;
   end Convert_Value_To_Time_Type;

   -----------------------------
   -- Get_Time_Property_Value --
   -----------------------------

   function Get_Time_Property_Value
     (E             : Node_Id;
      Property_Name : Name_Id) return Time_Type
   is
      Result : Time_Type := Null_Time;
      V      : Node_Id;
   begin
      V := Get_Value_Of_Property_Association (E, Property_Name);

      if Present (V) then
         if Present (Unit_Identifier (V)) then
            Result := Convert_Value_To_Time_Type (V);
         else
            Display_Located_Error
              (AIN.Loc (E),
               "Time without unit",
               Fatal => True);
         end if;
      end if;

      return Result;
   end Get_Time_Property_Value;

   -------------------------
   -- Get_Dispatch_Offset --
   -------------------------

   function Get_Dispatch_Offset (T : Node_Id) return Time_Type is
   begin
      pragma Assert (Is_Thread (T));

      case Get_Thread_Dispatch_Protocol (T) is
         when Thread_Periodic =>
            return Get_Time_Property_Value (T, Dispatch_Offset);

         when others =>
            Display_Located_Error
              (AIN.Loc (T),
               "This kind of thread does not have a dispatch offset",
               Fatal => True);
            return Null_Time;
      end case;
   end Get_Dispatch_Offset;

   -----------------------
   -- Get_Thread_Period --
   -----------------------

   function Get_Thread_Period (T : Node_Id) return Time_Type is
      The_Period : Time_Type;
   begin
      pragma Assert (Is_Thread (T));

      case Get_Thread_Dispatch_Protocol (T) is
         when Thread_Periodic | Thread_Sporadic | Thread_Hybrid | Thread_ISR =>
            --  We are sure the thread has a period

            The_Period := Get_Time_Property_Value (T, Thread_Period);

            if The_Period.T = 0 then
               Display_Located_Error
                 (AIN.Loc (T),
                  "Period cannot be null",
                  Fatal => True);
            end if;
            return The_Period;

         when others =>
            Display_Located_Error
              (AIN.Loc (T),
               "This kind of thread does not have a period",
               Fatal => True);
            return Null_Time;
      end case;
   end Get_Thread_Period;

   ----------------
   -- Get_Period --
   ----------------

   function Get_Period (T : Node_Id) return Time_Type is
   begin
      return Get_Time_Property_Value (T, Thread_Period);
   end Get_Period;

   ------------------------
   -- Get_Execution_Time --
   ------------------------

   function Get_Execution_Time (T : Node_Id) return Time_Type is
   begin
      return Get_Time_Property_Value (T, Execution_Time);
   end Get_Execution_Time;

   ------------------------------------
   -- Get_Thread_First_Dispatch_Time --
   ------------------------------------

   function Get_Thread_First_Dispatch_Time (T : Node_Id) return Time_Type is
      pragma Assert (Is_Thread (T));

   begin
      if Is_Defined_Property (T, Thread_Dispatch_Absolute_Time) then
         return Get_Time_Property_Value (T, Thread_Dispatch_Absolute_Time);
      --  XXX For now, use the old name, we should update the
      --  timing_properties property set and use new name:
      --  first_dispatch_time

      elsif Is_Defined_Property (T, Thread_Cheddar_Dispatch_Absolute_Time) then
         return Get_Time_Property_Value
             (T,
              Thread_Cheddar_Dispatch_Absolute_Time);
      else
         return Null_Time;
      end if;
   end Get_Thread_First_Dispatch_Time;

   -------------------------
   -- Get_Thread_Deadline --
   -------------------------

   function Get_Thread_Deadline (T : Node_Id) return Time_Type is
   begin
      pragma Assert (Is_Thread (T));

      case Get_Thread_Dispatch_Protocol (T) is
         when Thread_Periodic | Thread_Sporadic | Thread_Hybrid | Thread_ISR =>
            if Is_Defined_Property (T, Thread_Deadline) then
               return Get_Time_Property_Value (T, Thread_Deadline);
            else
               --  We are sure the thread has a period

               return Get_Thread_Period (T);
            end if;

         when others =>
            Display_Located_Error
              (AIN.Loc (T),
               "This kind of thread does not have a deadline",
               Fatal => True);
            return Null_Time;
      end case;
   end Get_Thread_Deadline;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (E : Node_Id) return Unsigned_Long_Long is
      use Ocarina.AADL_Values;

      Min_Priority : Unsigned_Long_Long := 0;
      Max_Priority : Unsigned_Long_Long := 0;

      procedure Update_Priority_Bounds (P : Name_Id);
      --  Check that the priority type corresponding to E has explicit
      --  bounds and set Max_Priority and Min_Priority
      --  accordingly.

      ----------------------------
      -- Update_Priority_Bounds --
      ----------------------------

      procedure Update_Priority_Bounds (P : Name_Id) is
         use Ocarina.ME_AADL;

         Property : constant Node_Id :=
           Find_Property_Association_From_Name (AIN.Properties (E), P);
         Property_Type       : Node_Id := No_Node;
         Property_Type_Range : Node_Id := No_Node;
         Process_Inst        : Node_Id;
         Processor_Inst      : Node_Id := No_Node;
      begin
         case AADL_Version is
            when AADL_V1 =>
               Property_Type :=
                 Expanded_Type_Designator
                   (Property_Name_Type
                      (Entity (AIN.Property_Name (Property))));
               Property_Type_Range := Type_Range (Property_Type);

            when AADL_V2 =>
               if Present (Parent_Subcomponent (E)) then
                  Process_Inst :=
                    Parent_Subcomponent
                      (Parent_Component (Parent_Subcomponent (E)));

                  if Present (Process_Inst) then
                     if Get_Category_Of_Component (Process_Inst) =
                       CC_Thread
                     then
                        Process_Inst :=
                          Parent_Subcomponent
                            (Parent_Component (Process_Inst));
                     end if;
                     Processor_Inst :=
                       Get_Bound_Processor
                         (Corresponding_Instance (Process_Inst));

                     if Is_Defined_Range_Property
                         (Processor_Inst,
                          Get_String_Name ("priority_range"))
                     then
                        Property_Type :=
                          (AIN.Property_Association_Value
                             (AIEP.Find_Property_Association_From_Name
                                (Property_List =>
                                   AIN.Properties (Processor_Inst),
                                 Property_Name =>
                                   Get_String_Name ("priority_range"))));

                        Property_Type_Range := Single_Value (Property_Type);
                     else
                        Display_Located_Error
                          (AIN.Loc (Processor_Inst),
                           "Priority_Range property is not defined",
                           Fatal   => False,
                           Warning => True);
                     end if;
                  end if;
               end if;
         end case;

         if No (Property_Type_Range) then
            --  In this case, we define a fake interval

            Min_Priority := 1;
            Max_Priority := 0;
         else
            Min_Priority :=
              Value (Value (Number_Value (Lower_Bound (Property_Type_Range))))
                .IVal;

            Max_Priority :=
              Value (Value (Number_Value (Upper_Bound (Property_Type_Range))))
                .IVal;
         end if;
      end Update_Priority_Bounds;

      Priority : Unsigned_Long_Long;

   begin
      if Is_Defined_Integer_Property (E, AADL_Priority) then
         Update_Priority_Bounds (AADL_Priority);

         Priority := Get_Integer_Property (E, AADL_Priority);

      elsif Is_Defined_Integer_Property (E, Thread_Cheddar_Priority) then
         Update_Priority_Bounds (Thread_Cheddar_Priority);

         Priority := Get_Integer_Property (E, Thread_Cheddar_Priority);

      else
         Priority := 0;
      end if;

      if Max_Priority /= 0
        and then (Priority > Max_Priority or else Priority < Min_Priority)
      then
         Display_Located_Error
           (AIN.Loc (E),
            "Priority not in priority interval set by Priority_Range by" &
            " hosting processor ([" &
            Min_Priority'Img &
            " .. " &
            Max_Priority'Img &
            " ])",
            Fatal => True);
      end if;

      return Priority;
   end Get_Priority;

   -------------------------
   -- Get_Thread_Priority --
   -------------------------

   function Get_Thread_Priority (T : Node_Id) return Unsigned_Long_Long is
   begin
      pragma Assert (Is_Thread (T));

      return Get_Priority (T);
   end Get_Thread_Priority;

   ---------------------------
   -- Get_Thread_Stack_Size --
   ---------------------------

   function Get_Thread_Stack_Size (T : Node_Id) return Size_Type is
   begin
      pragma Assert (Is_Thread (T));

      return (Get_Size_Property_Value (T, Thread_Stack_Size));
   end Get_Thread_Stack_Size;

   ------------------------------------
   -- Get_Thread_Implementation_Kind --
   ------------------------------------

   function Get_Thread_Implementation_Kind
     (T : Node_Id) return Supported_Thread_Implementation
   is
      pragma Assert (Is_Thread (T));

      Result : Supported_Thread_Implementation := Thread_Unknown;
      CS_Cnt : Natural                         := 0;
   begin
      --  See whether the thread IN ports have their own compute
      --  entrypoints.
      --  Can be call sequences

      if not Is_Empty (AIN.Features (T)) and then Has_In_Ports (T) then
         declare
            F         : Node_Id := AIN.First_Node (AIN.Features (T));
            All_Match : Boolean := True;
            One_Match : Boolean := False;
         begin
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then AIN.Is_In (F)
                 and then AIN.Is_Event (F)
                 and then AIN.Loc (F) /= No_Location
               then
                  One_Match :=
                    One_Match or else Is_Defined_Entrypoint_Property (F);

                  All_Match :=
                    All_Match and then Is_Defined_Entrypoint_Property (F);

               elsif Kind (F) = K_Port_Spec_Instance
                 and then AIN.Is_Out (F)
                 and then Is_Defined_Entrypoint_Property (F)
               then
                  Display_Located_Error
                    (AIN.Loc (F),
                     "You cannot specify Compute_Entrypoint property" &
                     " for a NON-in port",
                     Fatal => True);
               elsif Kind (F) = K_Port_Spec_Instance
                 and then AIN.Is_Data (F)
                 and then not AIN.Is_Event (F)
                 and then Is_Defined_Entrypoint_Property (F)
               then
                  Display_Located_Error
                    (AIN.Loc (F),
                     "You cannot specify Compute_Entrypoint property" &
                     " for a NON-event port",
                     Fatal => True);
               end if;

               F := AIN.Next_Node (F);
            end loop;

            if One_Match and then All_Match then
               Result := Thread_With_Port_Compute_Entrypoint;

            elsif One_Match then
               Display_Located_Error
                 (AIN.Loc (T),
                  "You should specify a compute entrypoint for all IN " &
                  "EVENT ports or else for NONE of them",
                  Fatal => True);
            end if;
         end;
      end if;

      if not Is_Empty (AIN.Calls (T)) then

         --  A thread must have either
         --  a single call sequence
         --  multiple call sequences and an entrypoint

         declare
            Call_Seq : Node_Id := AIN.First_Node (AIN.Calls (T));
         begin
            while Present (Call_Seq) loop
               if Length (AIN.Subprogram_Calls (Call_Seq)) > 1
                 and then Get_Current_Backend_Kind /= PolyORB_Kernel_C
               then
                  Display_Located_Error
                    (AIN.Loc (Call_Seq),
                     "Multiple subprogram calls in the thread call sequence" &
                     " are not supported. You should encapsulate them in" &
                     " a wrapper subprogram.",
                     Fatal => True);
               end if;

               Call_Seq := AIN.Next_Node (Call_Seq);
            end loop;

            CS_Cnt := CS_Cnt + 1;
         end;
      end if;

      if Result /= Thread_With_Port_Compute_Entrypoint then
         --  Thread_With_Port_Compute_Entryopoint takes precedence

         if Is_Defined_Entrypoint_Property (T) then
            --  See whether the thread itself has a compute entrypoint

            Result := Thread_With_Compute_Entrypoint;
         elsif CS_Cnt >= 1 then
            Result := Thread_With_Call_Sequence;
         end if;
      end if;

      return Result;
   end Get_Thread_Implementation_Kind;

   -----------------------------------
   -- Get_Thread_Compute_Entrypoint --
   -----------------------------------

   function Get_Thread_Compute_Entrypoint
     (T       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id renames
     Get_Compute_Entrypoint;

   function Get_Thread_Compute_Entrypoint
     (T       : Node_Id;
      In_Mode : Name_Id := No_Name) return Node_Id renames
     Get_Compute_Entrypoint;

   ------------------------
   -- Get_Implementation --
   ------------------------

   function Get_Implementation (E : Node_Id) return Node_Id is
   begin
      if Is_Defined_Property (E, Implemented_As) then
         return Get_Classifier_Property (E, Implemented_As);

      elsif Is_Defined_Property (E, Device_Driver_Name) then
         return Get_Classifier_Property (E, Device_Driver_Name);

      else
         return No_Node;
      end if;
   end Get_Implementation;

   --------------------------------------
   -- Get_Thread_Initialize_Entrypoint --
   --------------------------------------

   function Get_Thread_Initialize_Entrypoint (T : Node_Id) return Name_Id is
   begin
      if Is_Defined_String_Property (T, Initialize_Entrypoint) then
         return Get_String_Property (T, Initialize_Entrypoint);

      elsif Is_Defined_String_Property
          (T,
           Initialize_Entrypoint_Source_Text)
      then
         return Get_String_Property (T, Initialize_Entrypoint_Source_Text);
      else
         return No_Name;
      end if;
   end Get_Thread_Initialize_Entrypoint;

   function Get_Thread_Initialize_Entrypoint (T : Node_Id) return Node_Id is
   begin
      return Check_And_Get_Property (T, Initialize_Entrypoint);
   end Get_Thread_Initialize_Entrypoint;

   ------------------------------------
   -- Get_Thread_Activate_Entrypoint --
   ------------------------------------

   function Get_Thread_Activate_Entrypoint (T : Node_Id) return Name_Id is
   begin
      if Is_Defined_String_Property (T, Activate_Entrypoint) then
         return Get_String_Property (T, Activate_Entrypoint);

      elsif Is_Defined_String_Property
          (T,
           Activate_Entrypoint_Source_Text)
      then
         return Get_String_Property (T, Activate_Entrypoint_Source_Text);
      else
         return No_Name;
      end if;
   end Get_Thread_Activate_Entrypoint;

   ------------------------------------
   -- Get_Thread_Activate_Entrypoint --
   ------------------------------------

   function Get_Thread_Activate_Entrypoint (T : Node_Id) return Node_Id is
   begin
      return Check_And_Get_Property (T, Activate_Entrypoint);
   end Get_Thread_Activate_Entrypoint;

   -----------------------------------
   -- Get_Thread_Recover_Entrypoint --
   -----------------------------------

   function Get_Thread_Recover_Entrypoint (T : Node_Id) return Name_Id is
   begin
      if Is_Defined_String_Property (T, Recover_Entrypoint) then
         return Get_String_Property (T, Recover_Entrypoint);
      elsif Is_Defined_String_Property (T, Recover_Entrypoint_Source_Text) then
         return Get_String_Property (T, Recover_Entrypoint_Source_Text);
      else
         return No_Name;
      end if;
   end Get_Thread_Recover_Entrypoint;

   -----------------------------------
   -- Get_Thread_Recover_Entrypoint --
   -----------------------------------

   function Get_Thread_Recover_Entrypoint (T : Node_Id) return Node_Id is
   begin
      return Check_And_Get_Property (T, Recover_Entrypoint);
   end Get_Thread_Recover_Entrypoint;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (P : Node_Id) return Name_Id is
   begin
      if AINU.Is_Processor (P) or else AINU.Is_Device (P) then
         if not Is_Defined_String_Property (P, Location) then
            return No_Name;
         else
            return Get_String_Property (P, Location);
         end if;
      elsif AINU.Is_Virtual_Processor (P) then
         return Get_Location (Parent_Component (Parent_Subcomponent (P)));
      else
         raise Program_Error;
      end if;
   end Get_Location;

   ---------------------------
   -- Get_Scheduler_Quantum --
   ---------------------------

   function Get_Scheduler_Quantum (P : Node_Id) return Time_Type is
      pragma Assert (AINU.Is_Processor (P));

   begin
      if Is_Defined_Property (P, Scheduler_Quantum) then
         return Get_Time_Property_Value (P, Scheduler_Quantum);

      elsif Is_Defined_Property (P, Cheddar_Scheduler_Quantum) then
         return Get_Time_Property_Value (P, Cheddar_Scheduler_Quantum);

      else
         return Null_Time;
      end if;
   end Get_Scheduler_Quantum;

   ---------------------
   -- Get_Port_Number --
   ---------------------

   function Get_Port_Number (P : Node_Id) return Value_Id is
      use Ocarina.AADL_Values;
   begin
      pragma Assert (Is_Process (P) or else Is_Device (P));

      if not Is_Defined_Integer_Property (P, Port_Number) then
         return No_Value;
      end if;

      return New_Integer_Value (Get_Integer_Property (P, Port_Number));
   end Get_Port_Number;

   -------------------------
   -- Get_Bound_Processor --
   -------------------------

   function Get_Bound_Processor (P : Node_Id) return Node_Id is
      pragma Assert (Is_Process_Or_Device (P));
   begin
      if not Is_Defined_Reference_Property (P, Processor_Binding)
        and then Is_Process (P)
      then
         if Get_Current_Backend_Kind = Petri_Nets then
            --  optional for this backend
            return No_Node;
         else
            Display_Located_Error
              (AIN.Loc (Parent_Subcomponent (P)),
               "This process has to be bound to a processor",
               Fatal => True);
         end if;
      end if;

      return Get_Reference_Property (P, Processor_Binding);
   end Get_Bound_Processor;

   ----------------------
   -- Get_Bound_Memory --
   ----------------------

   function Get_Bound_Memory (P : Node_Id) return Node_Id is
   begin
      pragma Assert (Is_Process_Or_Device (P));

      if Is_Defined_Reference_Property (P, Memory_Binding) then
         return Get_Reference_Property (P, Memory_Binding);
      else
         return No_Node;
      end if;
   end Get_Bound_Memory;

   ------------------------
   -- Get_Bound_Function --
   ------------------------

   function Get_Bound_Function (P : Node_Id) return Node_Id is
   begin
      if not Is_Defined_Reference_Property (P, Function_Binding) then
         return No_Node;
      end if;

      return Get_Reference_Property (P, Function_Binding);
   end Get_Bound_Function;

   -------------------
   -- Get_Bound_Bus --
   -------------------

   function Get_Bound_Bus
     (C     : Node_Id;
      Check : Boolean := True) return Node_Id
   is
      Result_List : List_Id;
      Result      : Node_Id;
   begin
      pragma Assert (Kind (C) = K_Connection_Instance);

      --  Checks that the connection C can be bound to a bus

      if not Is_System (Parent_Component (C)) then
         return No_Node;
      elsif Is_System (Parent_Component (C))
        and then Is_Process
          (Parent_Component (Get_Referenced_Entity (AIN.Source (C))))
        and then Is_Process
          (Parent_Component (Get_Referenced_Entity (AIN.Destination (C))))
        and then
          Get_Execution_Platform
            (Get_Bound_Processor
               (Parent_Component (Get_Referenced_Entity (AIN.Source (C))))) =
          Platform_LEON3_XM3
        and then
          Get_Execution_Platform
            (Get_Bound_Processor
               (Parent_Component
                  (Get_Referenced_Entity (AIN.Destination (C))))) =
          Platform_LEON3_XM3
        and then
          Parent_Component
            (Parent_Subcomponent
               (Parent_Component
                  (Get_Referenced_Entity (AIN.Destination (C))))) =
          Parent_Component
            (Parent_Subcomponent
               (Parent_Component (Get_Referenced_Entity (AIN.Source (C)))))
      then
         return No_Node;
      elsif not Is_Defined_List_Property (C, Connection_Binding) then
         if Check then
            Display_Located_Error
              (AIN.Loc (C),
               "This connection has to be bound to a bus",
               Fatal => True);
         else
            --  We do not enforce connection binding for the remaining
            --  generators.

            return No_Node;
         end if;
      end if;

      Result_List := Get_List_Property (C, Connection_Binding);

      if ATNU.Is_Empty (Result_List) then
         Display_Located_Error
           (AIN.Loc (C),
            "This connection is not bound to any bus",
            Fatal => True);
      end if;

      if ATNU.Length (Result_List) > 1 then
         Display_Located_Error
           (AIN.Loc (C),
            "This connection is bound to more than one bus",
            Fatal => True);
      end if;

      Result := ATE.Get_Referenced_Entity (ATN.First_Node (Result_List));

      pragma Assert (Is_Bus (Result) or else Is_Virtual_Bus (Result));

      return Result;
   end Get_Bound_Bus;

   ----------------------------
   -- Get_Execution_Platform --
   ----------------------------

   function Get_Execution_Platform (P : Node_Id) return Name_Id is
      pragma Assert
        (AINU.Is_Processor (P) or else AINU.Is_Virtual_Processor (P));

   begin
      if Is_Defined_Enumeration_Property (P, Execution_Platform) then
         return Get_Enumeration_Property (P, Execution_Platform);
      else
         return No_Name;
      end if;
   end Get_Execution_Platform;

   function Get_Execution_Platform
     (P : Node_Id) return Supported_Execution_Platform
   is
      P_Name : Name_Id;
   begin
      pragma Assert
        (AINU.Is_Processor (P) or else AINU.Is_Virtual_Processor (P));

      if Is_Defined_Enumeration_Property (P, Execution_Platform) then
         P_Name := Get_Enumeration_Property (P, Execution_Platform);

         if P_Name = Platform_Native_Name then
            return Platform_Native;
         elsif P_Name = Platform_Bench_Name then
            return Platform_Bench;
         elsif P_Name = Platform_Native_Compcert_Name then
            return Platform_Native_Compcert;
         elsif P_Name = Platform_Gumstix_RTEMS_POSIX_Name then
            return Platform_Gumstix_RTEMS_POSIX;
         elsif P_Name = Platform_Gumstix_RTEMS_Name then
            return Platform_Gumstix_RTEMS;
         elsif P_Name = Platform_NDS_RTEMS_POSIX_Name then
            return Platform_NDS_RTEMS_POSIX;
         elsif P_Name = Platform_NDS_RTEMS_Name then
            return Platform_NDS_RTEMS;
         elsif P_Name = Platform_LEON_RTEMS_POSIX_Name then
            return Platform_LEON_RTEMS_POSIX;
         elsif P_Name = Platform_LEON_RTEMS_Name then
            return Platform_LEON_RTEMS;
         elsif P_Name = Platform_X86_LINUXTASTE_Name then
            return Platform_X86_LINUXTASTE;
         elsif P_Name = Platform_LINUX32_Name then
            return Platform_LINUX32;
         elsif P_Name = Platform_Win32_Name then
            return Platform_WIN32;
         elsif P_Name = Platform_LINUX32_Xenomai_Native_Name then
            return Platform_LINUX32_XENOMAI_NATIVE;
         elsif P_Name = Platform_LINUX32_Xenomai_Posix_Name then
            return Platform_LINUX32_XENOMAI_POSIX;
         elsif P_Name = Platform_LINUX64_Name then
            return Platform_LINUX64;
         elsif P_Name = Platform_X86_RTEMS_POSIX_Name then
            return Platform_X86_RTEMS_POSIX;
         elsif P_Name = Platform_X86_RTEMS_Name then
            return Platform_X86_RTEMS;
         elsif P_Name = Platform_LEON_GNAT_Name then
            return Platform_LEON_GNAT;
         elsif P_Name = Platform_LEON_ORK_Name then
            return Platform_LEON_ORK;
         elsif P_Name = Platform_LEON3_XM3_Name then
            return Platform_LEON3_XM3;
         elsif P_Name = Platform_LEON3_SCOC3_Name then
            return Platform_LEON3_SCOC3;
         elsif P_Name = Platform_LEON3_Xtratum_Name then
            return Platform_LEON3_XTRATUM;
         elsif P_Name = Platform_ERC32_ORK_Name then
            return Platform_ERC32_ORK;
         elsif P_Name = Platform_ARM_DSLINUX_Name then
            return Platform_ARM_DSLINUX;
         elsif P_Name = Platform_ARM_N770_Name then
            return Platform_ARM_N770;
         elsif P_Name = Platform_MARTE_OS_Name then
            return Platform_MARTE_OS;
         elsif P_Name = Platform_Vxworks_Name then
            return Platform_VxWorks;
         else
            return Platform_None;
         end if;
      else
         return Platform_None;
      end if;
   end Get_Execution_Platform;

   -----------------------
   -- Get_Transport_API --
   -----------------------

   function Get_Transport_API
     (B : Node_Id;
      E : Node_Id := No_Node) return Supported_Transport_APIs
   is
      T_Name   : Name_Id;
      Root_Sys : Node_Id := No_Node;
      S        : Node_Id;
   begin
      if No (B) then
         return Transport_None;
      end if;

      pragma Assert (Is_Bus (B) or else Is_Virtual_Bus (B));

      if Present (E) then
         Root_Sys := Parent_Component (Parent_Subcomponent (E));
      end if;

      if Is_Defined_Enumeration_Property (B, Transport_API) then

         --  Check whether the user specified a default transport API
         --  for the bus.

         T_Name := Get_Enumeration_Property (B, Transport_API);

         if T_Name = Transport_BSD_Sockets_Name then
            return Transport_BSD_Sockets;
         elsif T_Name = Transport_SpaceWire_Name then
            return Transport_SpaceWire;
         else
            Display_Located_Error
              (AIN.Loc (B),
               "Unknown transport layer",
               Fatal => True);
            return Transport_None;
         end if;

      elsif Present (Root_Sys)
        and then not AINU.Is_Empty (AIN.Subcomponents (Root_Sys))
      then
         --  Check whether this is a user-defined transport.
         --
         --  In this case, the user has to define a device
         --  connected to this bus.
         --
         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         S := AIN.First_Node (AIN.Subcomponents (Root_Sys));
         while Present (S) loop
            if AINU.Is_Device (Corresponding_Instance (S))
              and then
                Get_Bound_Processor (Corresponding_Instance (S)) =
                Get_Bound_Processor (E)
            then
               --  XXX incomplete
               return Transport_User;
            end if;
            S := AIN.Next_Node (S);
         end loop;

         Display_Located_Error
           (AIN.Loc (B),
            "Unknown transport layer",
            Fatal => True);
         return Transport_None;

      else
         return Transport_None;
      end if;
   end Get_Transport_API;

   --------------------
   -- Get_Queue_Size --
   --------------------

   function Get_Queue_Size (P : Node_Id) return Long_Long is
   begin
      pragma Assert
        (Kind (P) = K_Port_Spec_Instance and then AIN.Is_Event (P));

      if Is_Defined_Integer_Property (P, Queue_Size) then
         return Long_Long (Get_Integer_Property (P, Queue_Size));
      else
         return -1;
      end if;
   end Get_Queue_Size;

   ---------------------
   -- Get_Port_Timing --
   ---------------------

   function Get_Port_Timing (P : Node_Id) return Supported_Port_Timing is
      T_Name : Name_Id;
   begin
      if Is_Defined_Enumeration_Property (P, Port_Timing) then
         T_Name := Get_Enumeration_Property (P, Port_Timing);

         if T_Name = Port_Timing_Sampled_Name then
            return Port_Timing_Sampled;
         elsif T_Name = Port_Timing_Immediate_Name then
            return Port_Timing_Immediate;
         elsif T_Name = Port_Timing_Delayed_Name then
            return Port_Timing_Delayed;
         else
            Display_Located_Error
              (AIN.Loc (P),
               "Unknown port timing name",
               Fatal => True);
            return Port_Timing_None;
         end if;
      else
         return Port_Timing_None;
      end if;
   end Get_Port_Timing;

   ----------------------
   -- Get_Port_Urgency --
   ----------------------

   function Get_Port_Urgency (P : Node_Id) return Unsigned_Long_Long is
   begin
      return Check_And_Get_Property (P, Port_Urgency);
   end Get_Port_Urgency;

   ---------------------------------
   -- Get_Port_Compute_Entrypoint --
   ---------------------------------

   function Get_Port_Compute_Entrypoint
     (P       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id
   is
   begin
      if not AIN.Is_In (P) then
         Display_Located_Error
           (AIN.Loc (P),
            "Compute entrypoint cannot be specified for OUT-only ports",
            Fatal => True);
      end if;

      return Get_Compute_Entrypoint (P, In_Mode);
   end Get_Port_Compute_Entrypoint;

   ---------------------------------
   -- Get_Port_Compute_Entrypoint --
   ---------------------------------

   function Get_Port_Compute_Entrypoint
     (P       : Node_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
   begin
      if not AIN.Is_In (P) then
         Display_Located_Error
           (AIN.Loc (P),
            "Compute entrypoint cannot be specified for OUT-only ports",
            Fatal => True);
      end if;

      return Get_Compute_Entrypoint (P, In_Mode);
   end Get_Port_Compute_Entrypoint;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol (S : Node_Id) return Protocol_Type is
      P_Name : Name_Id;
   begin
      pragma Assert (Is_System (S));

      if Is_Defined_Enumeration_Property (S, Protocol) then
         P_Name := Get_Enumeration_Property (S, Protocol);

         if P_Name = Protocol_IIOP_Name then
            return Protocol_IIOP;
         elsif P_Name = Protocol_DIOP_Name then
            return Protocol_DIOP;
         else
            Display_Located_Error
              (AIN.Loc (S),
               "Unknown protocol name",
               Fatal => True);
            return Protocol_None;
         end if;
      else
         return Protocol_None;
      end if;
   end Get_Protocol;

   --------------------
   -- Get_Byte_Count --
   --------------------

   function Get_Byte_Count (S : Node_Id) return Unsigned_Long_Long is
      pragma Assert (Is_Memory (S));
   begin
      return Check_And_Get_Property (S, Byte_Count);
   end Get_Byte_Count;

   -------------------
   -- Get_Word_Size --
   -------------------

   function Get_Word_Size (S : Node_Id) return Size_Type is
   begin
      return (Get_Size_Property_Value (S, Word_Size));
   end Get_Word_Size;

   -----------------------------
   -- Get_Scheduling_Protocol --
   -----------------------------

   function Get_Scheduling_Protocol
     (P : Node_Id) return Supported_Scheduling_Protocol
   is
      pragma Assert (AINU.Is_Processor (P));
      use Ocarina.AADL_Values;
      Scheduling_L : List_Id;
      Scheduling_N : Name_Id;

   begin
      if AADL_Version = AADL_V1
        and then Is_Defined_Enumeration_Property (P, Scheduling_Protocol)
      then
         Scheduling_L := Get_List_Property (P, Scheduling_Protocol);
         Scheduling_N := Value (Value (ATN.First_Node (Scheduling_L))).EVal;

      elsif AADL_Version = AADL_V2
        and then Is_Defined_List_Property (P, Scheduling_Protocol)
      then
         Scheduling_L := Get_List_Property (P, Scheduling_Protocol);
         if ATNU.Length (Scheduling_L) = 1 then
            Scheduling_N :=
              To_Lower
                (ATN.Name (ATN.Identifier (ATN.First_Node (Scheduling_L))));
         else
            raise Program_Error;
         end if;

      else
         return Unknown_Scheduler;
      end if;

      if Scheduling_N = PARAMETRIC_PROTOCOL_Name then
         return PARAMETRIC_PROTOCOL;

      elsif Scheduling_N = EARLIEST_DEADLINE_FIRST_PROTOCOL_Name
        or else Scheduling_N = EDF_Name
      then
         return EARLIEST_DEADLINE_FIRST_PROTOCOL;

      elsif Scheduling_N = LEAST_LAXITY_FIRST_PROTOCOL_Name then
         return LEAST_LAXITY_FIRST_PROTOCOL;

      elsif Scheduling_N = RATE_MONOTONIC_PROTOCOL_Name
        or else Scheduling_N = RMS_Name
      then
         return RATE_MONOTONIC_PROTOCOL;

      elsif Scheduling_N = DEADLINE_MONOTONIC_PROTOCOL_Name then
         return DEADLINE_MONOTONIC_PROTOCOL;

      elsif Scheduling_N = ROUND_ROBIN_PROTOCOL_Name then
         return ROUND_ROBIN_PROTOCOL;

      elsif Scheduling_N = TIME_SHARING_BASED_ON_WAIT_TIME_PROTOCOL_Name then
         return TIME_SHARING_BASED_ON_WAIT_TIME_PROTOCOL;

      elsif Scheduling_N = POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL_Name then
         return POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL;

      elsif Scheduling_N = D_OVER_PROTOCOL_Name then
         return D_OVER_PROTOCOL;

      elsif Scheduling_N =
        MAXIMUM_URGENCY_FIRST_BASED_ON_LAXITY_PROTOCOL_Name
      then
         return MAXIMUM_URGENCY_FIRST_BASED_ON_LAXITY_PROTOCOL;

      elsif Scheduling_N =
        MAXIMUM_URGENCY_FIRST_BASED_ON_DEADLINE_PROTOCOL_Name
      then
         return MAXIMUM_URGENCY_FIRST_BASED_ON_DEADLINE_PROTOCOL;

      elsif Scheduling_N = TIME_SHARING_BASED_ON_CPU_USAGE_PROTOCOL_Name then
         return TIME_SHARING_BASED_ON_CPU_USAGE_PROTOCOL;

      elsif Scheduling_N = NO_SCHEDULING_PROTOCOL_Name then
         return NO_SCHEDULING_PROTOCOL;

      elsif Scheduling_N = HIERARCHICAL_CYCLIC_PROTOCOL_Name then
         return HIERARCHICAL_CYCLIC_PROTOCOL;

      elsif Scheduling_N = HIERARCHICAL_ROUND_ROBIN_PROTOCOL_Name then
         return HIERARCHICAL_ROUND_ROBIN_PROTOCOL;

      elsif Scheduling_N = HIERARCHICAL_FIXED_PRIORITY_PROTOCOL_Name then
         return HIERARCHICAL_FIXED_PRIORITY_PROTOCOL;

      elsif Scheduling_N = HIERARCHICAL_PARAMETRIC_PROTOCOL_Name then
         return HIERARCHICAL_PARAMETRIC_PROTOCOL;

      else
         Display_Located_Error
           (AIN.Loc (P),
            "Unknown scheduling protocol",
            Fatal => True);
         return Unknown_Scheduler;
      end if;
   end Get_Scheduling_Protocol;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Core_Id := Get_String_Name ("processor_properties::core_id");
      Source_Language  := Get_String_Name ("source_language");
      Source_Name      := Get_String_Name ("source_name");
      T_Source_Name    := Get_String_Name ("transformations::source_name");
      Type_Source_Name := Get_String_Name ("type_source_name");
      Source_Text      := Get_String_Name ("source_text");
      Fusion_Occurred  := Get_String_Name ("transformations::fusion_occurred");
      Priority_Shifter :=
        Get_String_Name ("transformations::priority_shifter");
      Scheduler_Name := Get_String_Name ("transformations::scheduler_name");
      Original_Name      := Get_String_Name ("transformations::original_name");
      Implemented_As     := Get_String_Name ("implemented_as");
      Device_Driver_Name := Get_String_Name ("device_driver");

      Base_Type            := Get_String_Name ("data_model::base_type");
      Code_Set             := Get_String_Name ("data_model::code_set");
      Data_Digits          := Get_String_Name ("data_model::data_digits");
      Data_Scale           := Get_String_Name ("data_model::data_scale");
      Dimension            := Get_String_Name ("data_model::dimension");
      Element_Names        := Get_String_Name ("data_model::element_names");
      Enumerators          := Get_String_Name ("data_model::enumerators");
      IEEE754_Precision := Get_String_Name ("data_model::ieee754_precision");
      Initial_Value        := Get_String_Name ("data_model::initial_value");
      Integer_Range        := Get_String_Name ("data_model::integer_range");
      Measurement_Unit     := Get_String_Name ("data_model::measurement_unit");
      Real_Range           := Get_String_Name ("data_model::real_range");
      Data_Required_Access := Get_String_Name ("required_access");
      Data_Provided_Access := Get_String_Name ("provided_access");
      Data_Size            := Get_String_Name ("data_size");
      Source_Data_Size     := Get_String_Name ("source_data_size");
      Code_Size            := Get_String_Name ("source_code_size");
      Data_Representation  :=
        Get_String_Name ("data_model::data_representation");
      Number_Representation :=
        Get_String_Name ("data_model::number_representation");
      Data_Concurrency_Protocol :=
        Get_String_Name ("concurrency_control_protocol");

      Protocol := Get_String_Name ("deployment::protocol");

      Dispatch_Offset               := Get_String_Name ("dispatch_offset");
      Thread_Period                 := Get_String_Name ("period");
      Thread_Deadline               := Get_String_Name ("deadline");
      Thread_Dispatch_Absolute_Time :=
        Get_String_Name ("dispatch_absolute_time");
      Thread_Cheddar_Dispatch_Absolute_Time :=
        Get_String_Name ("cheddar_properties::dispatch_absolute_time");

      Thread_Dispatch_Protocol := Get_String_Name ("dispatch_protocol");
      Thread_Cheddar_Priority  :=
        Get_String_Name ("cheddar_properties::fixed_priority");
      Thread_Stack_Size := Get_String_Name ("source_stack_size");

      Port_Timing                := Get_String_Name ("timing");
      Port_Timing_Sampled_Name   := Get_String_Name ("sampled");
      Port_Timing_Immediate_Name := Get_String_Name ("immediate");
      Port_Timing_Delayed_Name   := Get_String_Name ("delayed");

      Port_Urgency := Get_String_Name ("urgency");

      Overflow_Handling_Protocol :=
        Get_String_Name ("overflow_handling_protocol");
      Overflow_Handling_Protocol_DropOldest_Name :=
        Get_String_Name ("dropoldest");
      Overflow_Handling_Protocol_DropNewest_Name :=
        Get_String_Name ("dropnewest");
      Overflow_Handling_Protocol_Error_Name := Get_String_Name ("error");

      Port_Number       := Get_String_Name ("deployment::port_number");
      Processor_Binding := Get_String_Name ("actual_processor_binding");
      Function_Binding  :=
        Get_String_Name ("aram_properties::actual_function_binding");
      Memory_Binding          := Get_String_Name ("actual_memory_binding");
      Byte_Count              := Get_String_Name ("byte_count");
      Word_Size               := Get_String_Name ("word_size");

      Location                  := Get_String_Name ("deployment::location");
      Execution_Platform := Get_String_Name ("deployment::execution_platform");
      Scheduler_Quantum         := Get_String_Name ("scheduler_quantum");
      Cheddar_Scheduler_Quantum :=
        Get_String_Name ("cheddar_properties::scheduler_quantum");

      Connection_Binding := Get_String_Name ("actual_connection_binding");

      Transport_API := Get_String_Name ("deployment::transport_api");

      Queue_Size := Get_String_Name ("queue_size");

      Base_Address        := Get_String_Name ("base_address");
      Memory_Size         := Get_String_Name ("memory_size");
      Data_Array_Name     := Get_String_Name ("array");
      Data_Boolean_Name   := Get_String_Name ("boolean");
      Data_Character_Name := Get_String_Name ("character");
      Data_Enum_Name      := Get_String_Name ("enum");
      Data_Float_Name     := Get_String_Name ("float");
      Data_Fixed_Name     := Get_String_Name ("fixed");
      Data_Integer_Name   := Get_String_Name ("integer");
      Data_String_Name    := Get_String_Name ("string");
      Data_Struct_Name    := Get_String_Name ("struct");
      Data_Union_Name     := Get_String_Name ("union");

      Access_Read_Only_Name  := Get_String_Name ("read_only");
      Access_Write_Only_Name := Get_String_Name ("write_only");
      Access_Read_Write_Name := Get_String_Name ("read_write");
      Access_By_Method_Name  := Get_String_Name ("by_method");

      Language_Ada_95_Name          := Get_String_Name ("ada95");
      Language_Ada_Name             := Get_String_Name ("ada");
      Language_Ada_05_Name          := Get_String_Name ("ada05");
      Language_C_Name               := Get_String_Name ("c");
      Language_CPP_Name             := Get_String_Name ("cpp");
      Language_Device_Name          := Get_String_Name ("blackbox_device");
      Language_RTSJ_Name            := Get_String_Name ("rtsj");
      Language_Simulink_Name        := Get_String_Name ("simulink");
      Language_Scade_Name           := Get_String_Name ("scade");
      Language_ASN1_Name            := Get_String_Name ("asn1");
      Language_Esterel_Name         := Get_String_Name ("esterel");
      Language_Lustre_Name          := Get_String_Name ("lustre");
      Language_Lustre_Name          := Get_String_Name ("lustre");
      Language_Lustre5_Name         := Get_String_Name ("lustre5");
      Language_Lustre6_Name         := Get_String_Name ("lustre6");
      Language_GUI_Name             := Get_String_Name ("gui");
      Language_LUA_Name             := Get_String_Name ("lua");
      Language_Rhapsody_Name        := Get_String_Name ("rhapsody");
      Language_SCADE6_Name          := Get_String_Name ("scade6");
      Language_SDL_Name             := Get_String_Name ("sdl");
      Language_SDL_OpenGEODE_Name   := Get_String_Name ("sdl_opengeode");
      Language_SDL_ObjectGeode_Name := Get_String_Name ("sdl_objectgeode");
      Language_RTDS_Name            := Get_String_Name ("rtds");
      Language_SDL_RTDS_Name        := Get_String_Name ("sdl_rtds");
      Language_VHDL_Name            := Get_String_Name ("vhdl");
      Language_System_C_Name        := Get_String_Name ("system_c");

      Thread_Periodic_Name   := Get_String_Name ("periodic");
      Thread_Aperiodic_Name  := Get_String_Name ("aperiodic");
      Thread_Sporadic_Name   := Get_String_Name ("sporadic");
      Thread_Hybrid_Name     := Get_String_Name ("hybrid");
      Thread_Timed_Name      := Get_String_Name ("timed");
      Thread_Interrupt_Name  := Get_String_Name ("interrupt");
      Thread_Background_Name := Get_String_Name ("background");

      Time_Ps_Name  := Get_String_Name ("ps");
      Time_Ns_Name  := Get_String_Name ("ns");
      Time_Us_Name  := Get_String_Name ("us");
      Time_Ms_Name  := Get_String_Name ("ms");
      Time_Sec_Name := Get_String_Name ("sec");
      Time_Min_Name := Get_String_Name ("min");
      Time_Hr_Name  := Get_String_Name ("hr");

      Platform_Native_Name              := Get_String_Name ("native");
      Platform_Bench_Name               := Get_String_Name ("bench");
      Platform_Native_Compcert_Name     := Get_String_Name ("native_compcert");
      Platform_Gumstix_RTEMS_Name       := Get_String_Name ("gumstix_rtems");
      Platform_Gumstix_RTEMS_POSIX_Name :=
        Get_String_Name ("gumstix_rtems_posix");

      Platform_NDS_RTEMS_Name       := Get_String_Name ("nds_rtems");
      Platform_NDS_RTEMS_POSIX_Name := Get_String_Name ("nds_rtems_posix");

      Platform_X86_RTEMS_Name              := Get_String_Name ("x86_rtems");
      Platform_X86_RTEMS_POSIX_Name := Get_String_Name ("x86_rtems_posix");
      Platform_LINUX32_Name                := Get_String_Name ("linux32");
      Platform_Win32_Name                  := Get_String_Name ("win32");
      Platform_LINUX32_Xenomai_Native_Name :=
        Get_String_Name ("linux32_xenomai_native");
      Platform_LINUX32_Xenomai_Posix_Name :=
        Get_String_Name ("linux32_xenomai_posix");
      Platform_LINUX64_Name          := Get_String_Name ("linux64");
      Platform_X86_LINUXTASTE_Name   := Get_String_Name ("x86_linuxtaste");
      Platform_LEON_RTEMS_Name       := Get_String_Name ("leon_rtems");
      Platform_LEON_RTEMS_POSIX_Name := Get_String_Name ("leon_rtems_posix");
      Platform_LEON_GNAT_Name        := Get_String_Name ("leon_gnat");
      Platform_LEON_ORK_Name         := Get_String_Name ("leon_ork");
      Platform_LEON3_SCOC3_Name      := Get_String_Name ("leon3_scoc3");
      Platform_LEON3_XM3_Name        := Get_String_Name ("leon3_xm3");
      Platform_LEON3_Xtratum_Name    := Get_String_Name ("leon3_xtratum");
      Platform_ERC32_ORK_Name        := Get_String_Name ("erc32_ork");
      Platform_ARM_DSLINUX_Name      := Get_String_Name ("arm_dslinux");
      Platform_ARM_N770_Name         := Get_String_Name ("arm_n770");
      Platform_MARTE_OS_Name         := Get_String_Name ("marte_os");
      Platform_Vxworks_Name          := Get_String_Name ("vxworks");

      Transport_BSD_Sockets_Name := Get_String_Name ("bsd_sockets");
      Transport_SpaceWire_Name   := Get_String_Name ("spacewire");

      Protocol_IIOP_Name := Get_String_Name ("iiop");
      Protocol_DIOP_Name := Get_String_Name ("diop");

      Scheduling_Protocol := Get_String_Name ("scheduling_protocol");
      PARAMETRIC_PROTOCOL_Name := Get_String_Name ("parametric_protocol");
      EDF_Name                              := Get_String_Name ("edf");
      EARLIEST_DEADLINE_FIRST_PROTOCOL_Name :=
        Get_String_Name ("earliest_deadline_first_protocol");
      LEAST_LAXITY_FIRST_PROTOCOL_Name :=
        Get_String_Name ("least_laxity_first_protocol");
      RMS_Name                     := Get_String_Name ("rms");
      RATE_MONOTONIC_PROTOCOL_Name :=
        Get_String_Name ("rate_monotonic_protocol");
      DEADLINE_MONOTONIC_PROTOCOL_Name :=
        Get_String_Name ("deadline_monotonic_protocol");
      ROUND_ROBIN_PROTOCOL_Name := Get_String_Name ("round_robin_protocol");
      TIME_SHARING_BASED_ON_WAIT_TIME_PROTOCOL_Name :=
        Get_String_Name ("time_sharing_based_on_wait_time_protocol");
      POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL_Name :=
        Get_String_Name ("posix_1003_highest_priority_first_protocol");
      D_OVER_PROTOCOL_Name := Get_String_Name ("d_over_protocol");
      MAXIMUM_URGENCY_FIRST_BASED_ON_LAXITY_PROTOCOL_Name :=
        Get_String_Name ("maximum_urgency_first_based_on_laxity_protocol");
      MAXIMUM_URGENCY_FIRST_BASED_ON_DEADLINE_PROTOCOL_Name :=
        Get_String_Name ("maximum_urgency_first_based_on_deadline_protocol");
      TIME_SHARING_BASED_ON_CPU_USAGE_PROTOCOL_Name :=
        Get_String_Name ("time_sharing_based_on_cpu_usage_protocol");
      NO_SCHEDULING_PROTOCOL_Name :=
        Get_String_Name ("no_scheduling_protocol");
      HIERARCHICAL_CYCLIC_PROTOCOL_Name :=
        Get_String_Name ("hierarchical_cyclic_protocol");
      HIERARCHICAL_ROUND_ROBIN_PROTOCOL_Name :=
        Get_String_Name ("hierarchical_round_robin_protocol");
      HIERARCHICAL_FIXED_PRIORITY_PROTOCOL_Name :=
        Get_String_Name ("hierarchical_fixed_priority_protocol");
      HIERARCHICAL_PARAMETRIC_PROTOCOL_Name :=
        Get_String_Name ("hierarchical_parametric_protocol");

      -------------------------
      -- ARINC653 properties --
      -------------------------

      ARINC653_Partition_Slots :=
        Get_String_Name ("arinc653::partition_slots");
      ARINC653_Slots_Allocation :=
        Get_String_Name ("arinc653::slots_allocation");
      ARINC653_Module_Major_Frame :=
        Get_String_Name ("arinc653::module_major_frame");
      ARINC653_Sampling_Refresh_Period :=
        Get_String_Name ("arinc653::sampling_refresh_period");
      ARINC653_Timeout := Get_String_Name ("arinc653::timeout");
      ARINC653_Queuing_Discipline_Name :=
        Get_String_Name ("arinc653::queueing_discipline");
      ARINC653_Queuing_Discipline_Fifo_Name     := Get_String_Name ("fifo");
      ARINC653_Queuing_Discipline_Priority_Name :=
        Get_String_Name ("priority_based");
      ARINC653_Access_Type_Name := Get_String_Name ("arinc653::access_type");
      ARINC653_Access_Type_Read_Name       := Get_String_Name ("read");
      ARINC653_Access_Type_Write_Name      := Get_String_Name ("write");
      ARINC653_Access_Type_Read_Write_Name := Get_String_Name ("read_write");
      ARINC653_Memory_Kind_Name := Get_String_Name ("arinc653::memory_kind");
      ARINC653_Memory_Kind_Code_Name       := Get_String_Name ("memory_code");
      ARINC653_Memory_Kind_Data_Name       := Get_String_Name ("memory_data");

      ARINC653_Actions_Name := Get_String_Name ("arinc653::hm_actions");
      ARINC653_Module_Recovery_Actions_Name :=
        Get_String_Name ("arinc653::hm_module_recovery_actions");
      ARINC653_Partition_Recovery_Actions_Name :=
        Get_String_Name ("arinc653::hm_partition_recovery_actions");
      ARINC653_Process_Recovery_Actions_Name :=
        Get_String_Name ("arinc653::hm_process_recovery_actions");
      ARINC653_Action_Ignore_Name         := Get_String_Name ("ignore");
      ARINC653_Action_Confirm_Name        := Get_String_Name ("confirm");
      ARINC653_Action_Partition_Stop_Name :=
        Get_String_Name ("partition_stop");
      ARINC653_Action_Module_Stop_Name := Get_String_Name ("module_stop");
      ARINC653_Action_Process_Stop_Name := Get_String_Name ("process_stop");
      ARINC653_Action_Process_Stop_And_Start_Other_Name :=
        Get_String_Name ("proces_stop_and_start_another");
      ARINC653_Action_Process_Restart_Name :=
        Get_String_Name ("process_restart");
      ARINC653_Action_Warm_Restart_Name := Get_String_Name ("warm_restart");
      ARINC653_Action_Cold_Restart_Name := Get_String_Name ("cold_restart");
      ARINC653_Action_Partition_Restart_Name :=
        Get_String_Name ("partition_restart");
      ARINC653_Action_Module_Restart_Name :=
        Get_String_Name ("module_restart");
      ARINC653_Action_Reset_Name   := Get_String_Name ("reset");
      ARINC653_Action_Stop_Name    := Get_String_Name ("stop");
      ARINC653_Action_Nothing_Name := Get_String_Name ("nothing");

      ARINC653_Errors_Name := Get_String_Name ("arinc653::hm_errors");
      ARINC653_Error_Module_Init_Name       := Get_String_Name ("module_init");
      ARINC653_Error_Module_Config_Name := Get_String_Name ("module_config");
      ARINC653_Error_Module_Scheduling_Name :=
        Get_String_Name ("module_scheduling");
      ARINC653_Error_Partition_Config_Name :=
        Get_String_Name ("partition_config");
      ARINC653_Error_Partition_Init_Name := Get_String_Name ("partition_init");
      ARINC653_Error_Partition_Scheduling_Name :=
        Get_String_Name ("partition_scheduling");
      ARINC653_Error_Partition_Handler_Name :=
        Get_String_Name ("partition_handler");
      ARINC653_Error_Deadline_Miss_Name   := Get_String_Name ("deadline_miss");
      ARINC653_Error_Application_Name := Get_String_Name ("application_error");
      ARINC653_Error_Numeric_Name         := Get_String_Name ("numeric_error");
      ARINC653_Error_Invalid_Request_Name :=
        Get_String_Name ("illegal_request");
      ARINC653_Error_Stack_Overflow_Name := Get_String_Name ("stack_overflow");
      ARINC653_Error_Memory_Violation_Name :=
        Get_String_Name ("memory_violation");
      ARINC653_Error_Hardware_Fault_Name := Get_String_Name ("hardware_fault");
      ARINC653_Error_Power_Fail_Name     := Get_String_Name ("power_fail");

      --------------------
      -- POK properties --
      --------------------

      POK_Needed_Memory_Size := Get_String_Name ("pok::needed_memory_size");
      POK_BSP                := Get_String_Name ("pok::bsp");
      POK_Arch               := Get_String_Name ("pok::architecture");
      POK_Source_Location    := Get_String_Name ("pok::source_location");
      POK_Timeslice          := Get_String_Name ("pok::timeslice");
      POK_Security_Level     := Get_String_Name ("pok::security_level");
      POK_Mils_Verified      := Get_String_Name ("pok::mils_verified");
      POK_Refresh_Time       := Get_String_Name ("pok::refresh_time");
      POK_Scheduler          := Get_String_Name ("pok::scheduler");
      POK_Major_Frame        := Get_String_Name ("pok::major_frame");
      POK_Slots              := Get_String_Name ("pok::slots");
      POK_Slots_Allocation   := Get_String_Name ("pok::slots_allocation");

      POK_Arch_x86_Name         := Get_String_Name ("x86");
      POK_BSP_x86_qemu_Name     := Get_String_Name ("x86_qemu");
      POK_BSP_x86_qemu_vmm_Name := Get_String_Name ("x86_qemu_vmm");
      POK_Arch_Sparc_Name       := Get_String_Name ("sparc");
      POK_BSP_Leon_Name         := Get_String_Name ("leon3");
      POK_Arch_ppc_Name         := Get_String_Name ("ppc");
      POK_BSP_prep_Name         := Get_String_Name ("prep");
      POK_Scheduler_EDF_Name    := Get_String_Name ("edf");
      POK_Scheduler_LLF_Name    := Get_String_Name ("llf");
      POK_Scheduler_RMS_Name    := Get_String_Name ("rms");
      POK_Scheduler_RR_Name     := Get_String_Name ("rr");
      POK_Scheduler_Static_Name := Get_String_Name ("static");

      POK_Recovery_Errors := Get_String_Name ("pok::recovery_errors");
      POK_Recovery_Error_Deadline       := Get_String_Name ("deadline_missed");
      POK_Recovery_Error_Application := Get_String_Name ("application_error");
      POK_Recovery_Error_Numeric        := Get_String_Name ("numeric_error");
      POK_Recovery_Error_Request        := Get_String_Name ("illegal_request");
      POK_Recovery_Error_Stack          := Get_String_Name ("stack_overflow");
      POK_Recovery_Error_Memory := Get_String_Name ("memory_violation");
      POK_Recovery_Error_Hardware       := Get_String_Name ("hardware_fault");
      POK_Recovery_Error_Power          := Get_String_Name ("power_fail");
      POK_Recovery_Error_Partition_Conf :=
        Get_String_Name ("partition_configuration");
      POK_Recovery_Error_Partition_Init  := Get_String_Name ("partition_init");
      POK_Recovery_Error_Partition_Sched :=
        Get_String_Name ("partition_scheduling");
      POK_Recovery_Error_Partition_Proc :=
        Get_String_Name ("partition_process");
      POK_Recovery_Error_Kernel_Init := Get_String_Name ("kernel_init");
      POK_Recovery_Error_Kernel_Sched := Get_String_Name ("kernel_scheduling");
      POK_Recovery_Actions := Get_String_Name ("pok::recovery_actions");
      POK_Recovery_Action_Ignore := Get_String_Name ("ignore");
      POK_Recovery_Action_Confirm := Get_String_Name ("confirm");
      POK_Recovery_Action_Thread_Restart := Get_String_Name ("thread_restart");
      POK_Recovery_Action_Thread_Stop_And_Start_Other :=
        Get_String_Name ("thread_stop_and_start_another");
      POK_Recovery_Action_Thread_Stop       := Get_String_Name ("thread_stop");
      POK_Recovery_Action_Partition_Restart :=
        Get_String_Name ("partition_restart");
      POK_Recovery_Action_Partition_Stop := Get_String_Name ("partition_stop");
      POK_Recovery_Action_Kernel_Stop    := Get_String_Name ("kernel_stop");
      POK_Recovery_Action_Kernel_Restart := Get_String_Name ("kernel_restart");
      POK_Recovery_Action_Nothing        := Get_String_Name ("nothing");

      Provided_Virtual_Bus_Class :=
        Get_String_Name ("provided_virtual_bus_class");

      Allowed_Connection_Binding_Class :=
        Get_String_Name ("allowed_connection_binding_class");

      Compute_Execution_Time := Get_String_Name ("compute_execution_time");
      Execution_Time         := Get_String_Name ("execution_time");

      Compute_Deadline := Get_String_Name ("compute_deadline");

      SEI_Wait_For_All_Events := Get_String_Name ("sei::waitallevents");
      SEI_Stream_Miss_Rate    := Get_String_Name ("sei::streammissrate");

      Scade_Signal_Name := Get_String_Name ("scade::signal");

      Initialize_Entrypoint := Get_String_Name ("initialize_entrypoint");
      Initialize_Entrypoint_Source_Text :=
        Get_String_Name ("initialize_entrypoint_source_text");

      Activate_Entrypoint := Get_String_Name ("activate_entrypoint");
      Activate_Entrypoint_Source_Text :=
        Get_String_Name ("activate_entrypoint_source_text");
      Recover_Entrypoint             := Get_String_Name ("recover_entrypoint");
      Recover_Entrypoint_Source_Text :=
        Get_String_Name ("recover_entrypoint_source_text");

      POSIX_Scheduling_Policy := Get_String_Name ("posix_scheduling_policy");
      Cheddar_POSIX_Scheduling_Policy :=
        Get_String_Name ("cheddar_properties::posix_scheduling_policy");
      SCHED_FIFO_Name   := Get_String_Name ("sched_fifo");
      SCHED_RR_Name     := Get_String_Name ("sched_rr");
      SCHED_Others_Name := Get_String_Name ("sched_others");

      case AADL_Version is
         when AADL_V1 =>
            Size_Bit_Name       := Get_String_Name ("bits");
            Size_Byte_Name      := Get_String_Name ("b");
            Size_Kilo_Byte_Name := Get_String_Name ("kb");
            Size_Mega_Byte_Name := Get_String_Name ("mb");
            Size_Giga_Byte_Name := Get_String_Name ("gb");

            AADL_Priority := Get_String_Name ("deployment::priority");
            Compute_Entrypoint_Source_Text_Name :=
              Get_String_Name ("compute_entrypoint");

         when AADL_V2 =>
            Size_Bit_Name       := Get_String_Name ("bits");
            Size_Byte_Name      := Get_String_Name ("bytes");
            Size_Kilo_Byte_Name := Get_String_Name ("kbyte");
            Size_Mega_Byte_Name := Get_String_Name ("mbyte");
            Size_Giga_Byte_Name := Get_String_Name ("gbyte");
            Size_Tera_Byte_Name := Get_String_Name ("tbyte");

            AADL_Priority := Get_String_Name ("priority");

            Compute_Entrypoint_Source_Text_Name :=
              Get_String_Name ("compute_entrypoint_source_text");
            Compute_Entrypoint_Call_Sequence :=
              Get_String_Name ("compute_entrypoint_call_sequence");
      end case;

      Compute_Entrypoint_Name := Get_String_Name ("compute_entrypoint");
   end Init;

   -----------------
   -- Get_POK_BSP --
   -----------------

   function Get_POK_BSP (P : Node_Id) return Supported_POK_BSP is
      P_Name : Name_Id;
   begin
      pragma Assert (AINU.Is_Processor (P));

      if Is_Defined_Enumeration_Property (P, POK_BSP) then
         P_Name := Get_Enumeration_Property (P, POK_BSP);

         if P_Name = POK_BSP_x86_qemu_Name then
            return POK_BSP_x86_qemu;
         elsif P_Name = POK_BSP_x86_qemu_vmm_Name then
            return POK_BSP_x86_qemu_vmm;
         elsif P_Name = POK_BSP_prep_Name then
            return POK_BSP_prep;
         elsif P_Name = POK_BSP_Leon_Name then
            return POK_BSP_Leon;
         else
            Display_Located_Error (AIN.Loc (P), "Unknown BSP", Fatal => True);
            return Invalid_BSP;
         end if;
      else
         return Invalid_BSP;
      end if;
   end Get_POK_BSP;

   -----------------------------
   -- Get_POK_Recovery_Errors --
   -----------------------------

   function Get_POK_Recovery_Errors (E : Node_Id) return POK_Handled_Errors is
      L : List_Id;
      P : Node_Id;
   begin
      if Is_Defined_Property (E, POK_Recovery_Errors) then
         --  L := Get_List_Property (E, POK_Recovery_Errors);
         P :=
           AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => POK_Recovery_Errors,
              In_Mode       => No_Name);

         L := Multi_Value (AIN.Property_Association_Value (P));

         if ATNU.Is_Empty (L) then
            return POK_Empty_Handled_Errors;
         else
            declare
               Handled_Errors : POK_Handled_Errors
               (1 .. Int (ATNU.Length (L)));
               S : Node_Id;
               I : Int := 1;
            begin
               S := ATN.First_Node (L);
               while Present (S) loop
                  if ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Deadline
                  then
                     Handled_Errors (I) := POK_Error_Deadline_Missed;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Application
                  then
                     Handled_Errors (I) := POK_Error_Application;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Numeric
                  then
                     Handled_Errors (I) := POK_Error_Numeric;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Request
                  then
                     Handled_Errors (I) := POK_Error_Illegal_Request;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Stack
                  then
                     Handled_Errors (I) := POK_Error_Stack_Overflow;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Memory
                  then
                     Handled_Errors (I) := POK_Error_Memory_Violation;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Hardware
                  then
                     Handled_Errors (I) := POK_Error_Hardware_Fault;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Power
                  then
                     Handled_Errors (I) := POK_Error_Power_Fail;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Partition_Conf
                  then
                     Handled_Errors (I) := POK_Error_Partition_Configuration;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Partition_Init
                  then
                     Handled_Errors (I) := POK_Error_Partition_Init;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Partition_Sched
                  then
                     Handled_Errors (I) := POK_Error_Partition_Scheduling;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Partition_Proc
                  then
                     Handled_Errors (I) := POK_Error_Kernel_Scheduling;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Kernel_Init
                  then
                     Handled_Errors (I) := POK_Error_Kernel_Init;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Error_Kernel_Sched
                  then
                     Handled_Errors (I) := POK_Error_Kernel_Scheduling;
                  else
                     Handled_Errors (I) := POK_Error_Invalid;
                  end if;
                  S := ATN.Next_Node (S);
                  I := I + 1;
               end loop;
               return Handled_Errors;
            end;
         end if;
      end if;

      return POK_Empty_Handled_Errors;
   end Get_POK_Recovery_Errors;

   ------------------------------
   -- Get_POK_Recovery_Actions --
   ------------------------------

   function Get_POK_Recovery_Actions
     (E : Node_Id) return POK_Handled_Actions
   is
      L : List_Id;
      P : Node_Id;
   begin
      if Is_Defined_Property (E, POK_Recovery_Actions) then
         P :=
           AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => POK_Recovery_Actions,
              In_Mode       => No_Name);

         L := Multi_Value (AIN.Property_Association_Value (P));

         if ATNU.Is_Empty (L) then
            return POK_Empty_Handled_Actions;
         else
            declare
               Handled_Actions : POK_Handled_Actions
               (1 .. Int (ATNU.Length (L)));
               S : Node_Id;
               I : Int := 1;
            begin
               S := ATN.First_Node (L);
               while Present (S) loop
                  if ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Ignore
                  then
                     Handled_Actions (I) := POK_Action_Ignore;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Confirm
                  then
                     Handled_Actions (I) := POK_Action_Confirm;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Thread_Restart
                  then
                     Handled_Actions (I) := POK_Action_Thread_Restart;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Thread_Stop_And_Start_Other
                  then
                     Handled_Actions (I) :=
                       POK_Action_Thread_Stop_And_Start_Another;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Thread_Stop
                  then
                     Handled_Actions (I) := POK_Action_Thread_Stop;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Partition_Restart
                  then
                     Handled_Actions (I) := POK_Action_Partition_Restart;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Partition_Stop
                  then
                     Handled_Actions (I) := POK_Action_Partition_stop;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Kernel_Stop
                  then
                     Handled_Actions (I) := POK_Action_Kernel_Stop;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Kernel_Restart
                  then
                     Handled_Actions (I) := POK_Action_Kernel_Restart;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    POK_Recovery_Action_Nothing
                  then
                     Handled_Actions (I) := POK_Action_Nothing;
                  end if;
                  S := ATN.Next_Node (S);
                  I := I + 1;
               end loop;
               return Handled_Actions;
            end;
         end if;
      end if;

      return POK_Empty_Handled_Actions;
   end Get_POK_Recovery_Actions;

   --------------------------
   -- Get_POK_Architecture --
   --------------------------

   function Get_POK_Architecture
     (P : Node_Id) return Supported_POK_Architectures
   is
      P_Name : Name_Id;
   begin
      pragma Assert (AINU.Is_Processor (P));

      if Is_Defined_Enumeration_Property (P, POK_Arch) then
         P_Name := Get_Enumeration_Property (P, POK_Arch);

         if P_Name = POK_Arch_x86_Name then
            return POK_Arch_x86;
         elsif P_Name = POK_Arch_ppc_Name then
            return POK_Arch_Ppc;
         elsif P_Name = POK_Arch_Sparc_Name then
            return POK_Arch_Sparc;
         else
            Display_Located_Error
              (AIN.Loc (P),
               "Unknown Architecture",
               Fatal => True);
            return Invalid_Architecture;
         end if;
      else
         return Invalid_Architecture;
      end if;
   end Get_POK_Architecture;

   -------------------
   -- Get_Timeslice --
   -------------------

   function Get_Timeslice (T : Node_Id) return Time_Type is
   begin
      return Get_Time_Property_Value (T, POK_Timeslice);
   end Get_Timeslice;

   -------------------------
   -- Get_Source_Location --
   -------------------------

   function Get_Source_Location (E : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property (E, POK_Source_Location);
   end Get_Source_Location;

   ----------------------------
   -- Get_Needed_Memory_Size --
   ----------------------------

   function Get_Needed_Memory_Size (P : Node_Id) return Unsigned_Long_Long is
   begin
      pragma Assert (Is_Process_Or_Device (P));

      if not Is_Defined_Integer_Property (P, POK_Needed_Memory_Size) then
         return 0;
      end if;

      return To_Bytes (Get_Size_Property_Value (P, POK_Needed_Memory_Size));
   end Get_Needed_Memory_Size;

   ------------------------------------
   -- Get_Provided_Virtual_Bus_Class --
   ------------------------------------

   function Get_Provided_Virtual_Bus_Class (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := Get_Classifier_Property (E, Provided_Virtual_Bus_Class);

      if Present (N) then
         return N;
      else
         N := Get_Classifier_Property (E, Allowed_Connection_Binding_Class);

         if Present (N) then
            return N;
         end if;
      end if;

      return No_Node;
   end Get_Provided_Virtual_Bus_Class;

   -------------------------------------
   -- Get_Bounded_Virtual_Bus_Classes --
   -------------------------------------

   function Get_Bounded_Virtual_Bus_Classes (E : Node_Id) return List_Id is
      L : List_Id;
   begin
      if Is_Defined_Property (E, Provided_Virtual_Bus_Class) then
         L := Get_List_Property (E, Provided_Virtual_Bus_Class);
         if L /= No_List then
            return L;
         end if;
      end if;

      if Is_Defined_Property (E, Allowed_Connection_Binding_Class) then
         L := Get_List_Property (E, Allowed_Connection_Binding_Class);
         if L /= No_List then
            return L;
         end if;
      end if;

      return No_List;
   end Get_Bounded_Virtual_Bus_Classes;

   ------------------------
   -- Get_Security_Level --
   ------------------------

   function Get_Security_Level (E : Node_Id) return Unsigned_Long_Long is
   begin
      return Check_And_Get_Property (E, POK_Security_Level);
   end Get_Security_Level;

   --------------------------------------------
   -- Get_Security_Level_Through_Virtual_Bus --
   --------------------------------------------

   function Get_Security_Level_Through_Virtual_Bus
     (E : Node_Id) return Unsigned_Long_Long
   is
      N : Node_Id;
   begin
      N := Get_Provided_Virtual_Bus_Class (E);

      if Present (N) then
         return Get_Security_Level (N);
      end if;

      return 0;
   end Get_Security_Level_Through_Virtual_Bus;

   ------------------------
   -- Get_Execution_Time --
   ------------------------

   function Get_Execution_Time (E : Node_Id) return Time_Array is
      Res            : Time_Array (0 .. 1);
      Property_Value : Node_Id;
      Range_Node     : Node_Id;
      use Ocarina.AADL_Values;
   begin
      if Is_Defined_Property (E, Compute_Execution_Time) then
         --  CRAP !!
         --  FIXME
         --  Must change that, it's too crappy, but it works.
         --  Must rely on ocarina.backends.properties package

         Property_Value :=
           Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => Compute_Execution_Time,
              In_Mode       => No_Name);

         Range_Node :=
           Single_Value (AIN.Property_Association_Value (Property_Value));

         Res (0) := Convert_Value_To_Time_Type (Lower_Bound (Range_Node));
         Res (1) := Convert_Value_To_Time_Type (Upper_Bound (Range_Node));

         return Res;
      else
         return Empty_Time_Array;
      end if;
   end Get_Execution_Time;

   --------------------------
   -- Get_Compute_Deadline --
   --------------------------

   function Get_Compute_Deadline (E : Node_Id) return Time_Type is
   begin
      if Is_Defined_Property (E, Compute_Deadline) then
         return Convert_Value_To_Time_Type
             (Get_Value_Of_Property_Association (E, Compute_Deadline));
      else
         return Null_Time;
      end if;
   end Get_Compute_Deadline;

   --------------------------
   -- Get_ARINC653_Timeout --
   --------------------------

   function Get_ARINC653_Timeout (E : Node_Id) return Time_Type is
   begin
      if Is_Defined_Property (E, ARINC653_Timeout) then
         return Convert_Value_To_Time_Type
             (Get_Value_Of_Property_Association (E, ARINC653_Timeout));
      else
         return Null_Time;
      end if;
   end Get_ARINC653_Timeout;

   --------------------------
   -- Get_POK_Refresh_Time --
   --------------------------

   function Get_POK_Refresh_Time (E : Node_Id) return Time_Type is
   begin
      if Is_Defined_Property (E, POK_Refresh_Time) then
         return Convert_Value_To_Time_Type
             (Get_Value_Of_Property_Association (E, POK_Refresh_Time));
      elsif Is_Defined_Property (E, ARINC653_Sampling_Refresh_Period) then
         return Convert_Value_To_Time_Type
             (Get_Value_Of_Property_Association
                (E,
                 ARINC653_Sampling_Refresh_Period));
      else
         return Null_Time;
      end if;
   end Get_POK_Refresh_Time;

   ---------------------------
   -- Get_POK_Mils_Verified --
   ---------------------------

   function Get_POK_Mils_Verified (E : Node_Id) return Boolean is
   begin
      return Check_And_Get_Property (E, POK_Mils_Verified);
   end Get_POK_Mils_Verified;

   -------------------
   -- Get_Miss_Rate --
   -------------------

   function Get_Miss_Rate (E : Node_Id) return Unsigned_Long_Long is
      Value : Long_Long_Float;
   begin
      if Is_Defined_Property (E, SEI_Stream_Miss_Rate) then
         Value := Get_Float_Property (E, SEI_Stream_Miss_Rate);
         Value := Value * Long_Long_Float (100);
         return Unsigned_Long_Long (Value);
      end if;
      return 0;
   end Get_Miss_Rate;

   -----------------------------
   -- Get_Wait_For_All_Events --
   -----------------------------

   function Get_Wait_For_All_Events (E : Node_Id) return Boolean is
   begin
      return Check_And_Get_Property (E, SEI_Wait_For_All_Events);
   end Get_Wait_For_All_Events;

   -----------------------
   -- Get_POK_Scheduler --
   -----------------------

   function Get_POK_Scheduler (P : Node_Id) return Supported_POK_Scheduler is
      P_Name : Name_Id;
   begin
      if Is_Defined_Enumeration_Property (P, POK_Scheduler) then
         P_Name := Get_Enumeration_Property (P, POK_Scheduler);

         if P_Name = POK_Scheduler_RMS_Name then
            return RMS;
         elsif P_Name = POK_Scheduler_EDF_Name then
            return EDF;
         elsif P_Name = POK_Scheduler_LLF_Name then
            return LLF;
         elsif P_Name = POK_Scheduler_RR_Name then
            return RR;
         elsif P_Name = POK_Scheduler_Static_Name then
            return Static;
         else
            return Invalid_Scheduler;
         end if;
      else
         return Invalid_Scheduler;
      end if;
   end Get_POK_Scheduler;

   -------------------------
   -- Get_POK_Major_Frame --
   -------------------------

   function Get_POK_Major_Frame (E : Node_Id) return Time_Type is
   begin
      if Is_Defined_Property (E, POK_Major_Frame) then
         return Get_Time_Property_Value (E, POK_Major_Frame);
      elsif Is_Defined_Property (E, ARINC653_Module_Major_Frame) then
         return Get_Time_Property_Value (E, ARINC653_Module_Major_Frame);
      end if;
      return Null_Time;
   end Get_POK_Major_Frame;

   ------------------------------
   -- Get_POK_Slots_Allocation --
   ------------------------------

   function Get_POK_Slots_Allocation (E : Node_Id) return List_Id is
      L : List_Id;
   begin
      pragma Assert (AINU.Is_Processor (E));
      if Is_Defined_Property (E, POK_Slots_Allocation) then
         L := Get_List_Property (E, POK_Slots_Allocation);
         if L /= No_List then
            return L;
         end if;
      elsif Is_Defined_Property (E, ARINC653_Slots_Allocation) then
         L := Get_List_Property (E, ARINC653_Slots_Allocation);
         if L /= No_List then
            return L;
         end if;
      end if;

      return No_List;
   end Get_POK_Slots_Allocation;

   -------------------
   -- Get_POK_Slots --
   -------------------

   function Get_POK_Slots (E : Node_Id) return Time_Array is
      L : List_Id := No_List;
      N : Node_Id;
   begin
      if Is_Defined_Property (E, POK_Slots) then
         L := Get_List_Property (E, POK_Slots);
      elsif Is_Defined_Property (E, ARINC653_Partition_Slots) then
         L := Get_List_Property (E, ARINC653_Partition_Slots);
      else
         return Empty_Time_Array;
      end if;

      if L = No_List then
         return Empty_Time_Array;
      end if;

      declare
         T : Time_Array (1 .. Int (ATNU.Length (L)));
      begin
         N := ATN.First_Node (L);
         for I in 1 .. ATNU.Length (L) loop
            T (Int (I)) := Convert_Value_To_Time_Type (N);
            N           := ATN.Next_Node (N);
         end loop;
         return T;
      end;
   end Get_POK_Slots;

   ----------------------
   -- Get_Scade_Signal --
   ----------------------

   function Get_Scade_Signal
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id
   is
      pragma Unreferenced (In_Mode);
   begin
      return Check_And_Get_Property (E, Scade_Signal_Name);
   end Get_Scade_Signal;

   -------------------------------------
   -- Get_ARINC653_Queuing_Discipline --
   -------------------------------------

   function Get_ARINC653_Queuing_Discipline
     (Port : Node_Id) return ARINC653_Queuing_Discipline
   is
      Property_Value : Name_Id;
   begin
      if Is_Defined_Property (Port, ARINC653_Queuing_Discipline_Name) then
         Property_Value :=
           Get_Enumeration_Property (Port, ARINC653_Queuing_Discipline_Name);

         if Property_Value = ARINC653_Queuing_Discipline_Fifo_Name then
            return Fifo;
         elsif Property_Value = ARINC653_Queuing_Discipline_Priority_Name then
            return Priority_Based;
         else
            return Invalid;
         end if;
      else
         return Invalid;
      end if;
   end Get_ARINC653_Queuing_Discipline;

   ------------------------------
   -- Get_ARING653_Access_Type --
   ------------------------------

   function Get_ARINC653_Access_Type
     (Memory : Node_Id) return ARINC653_Access_Type
   is
      Property_Value : Name_Id;
   begin
      if Is_Defined_Property (Memory, ARINC653_Access_Type_Name) then
         Property_Value :=
           Get_Enumeration_Property (Memory, ARINC653_Access_Type_Name);

         if Property_Value = ARINC653_Access_Type_Read_Name then
            return Read;
         elsif Property_Value = ARINC653_Access_Type_Write_Name then
            return Write;
         elsif Property_Value = ARINC653_Access_Type_Read_Write_Name then
            return Read_Write;
         else
            return Invalid;
         end if;
      else
         return Invalid;
      end if;
   end Get_ARINC653_Access_Type;

   ------------------------------
   -- Get_ARING653_Memory_Kind --
   ------------------------------

   function Get_ARINC653_Memory_Kind
     (Memory : Node_Id) return ARINC653_Memory_Kind
   is
      Property_Value : Name_Id;
   begin
      if Is_Defined_Property (Memory, ARINC653_Memory_Kind_Name) then
         Property_Value :=
           Get_Enumeration_Property (Memory, ARINC653_Memory_Kind_Name);

         if Property_Value = ARINC653_Memory_Kind_Code_Name then
            return Code;
         elsif Property_Value = ARINC653_Memory_Kind_Data_Name then
            return Data;
         else
            return Invalid;
         end if;
      else
         return Invalid;
      end if;
   end Get_ARINC653_Memory_Kind;

   ----------------------------
   -- Get_ARINC653_HM_Errors --
   ----------------------------

   function Get_ARINC653_HM_Errors (E : Node_Id) return ARINC653_Errors is
      L : List_Id;
      P : Node_Id;
   begin
      if Is_Defined_Property (E, ARINC653_Errors_Name) then
         P :=
           AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => ARINC653_Errors_Name,
              In_Mode       => No_Name);

         L := Multi_Value (AIN.Property_Association_Value (P));

         if ATNU.Is_Empty (L) then
            return ARINC653_Empty_Errors;
         else
            declare
               Handled_Errors : ARINC653_Errors (1 .. Int (ATNU.Length (L)));
               S              : Node_Id;
               I              : Int := 1;
            begin
               S := ATN.First_Node (L);
               while Present (S) loop
                  if ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Deadline_Miss_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Deadline_Miss;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Application_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Application;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Numeric_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Numeric;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Invalid_Request_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Illegal_Request;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Stack_Overflow_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Stack_Overflow;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Memory_Violation_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Memory_Violation;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Hardware_Fault_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Hardware_Fault;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Power_Fail_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Power_Fail;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Partition_Config_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Partition_Config;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Partition_Init_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Partition_Init;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Partition_Scheduling_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Partition_Scheduling;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Partition_Handler_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Partition_Handler;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Module_Init_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Module_Init;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Module_Scheduling_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Module_Scheduling;
                  elsif ATN.Name (ATN.Identifier (S)) =
                    ARINC653_Error_Module_Config_Name
                  then
                     Handled_Errors (I) := ARINC653_Error_Module_Config;
                  else
                     Handled_Errors (I) := ARINC653_Error_Invalid;
                  end if;
                  S := ATN.Next_Node (S);
                  I := I + 1;
               end loop;
               return Handled_Errors;
            end;
         end if;
      end if;

      return ARINC653_Empty_Errors;
   end Get_ARINC653_HM_Errors;

   -----------------------------
   -- Get_ARINC653_HM_Actions --
   -----------------------------

   function Get_ARINC653_HM_Actions (E : Node_Id) return ARINC653_Actions is
      L : List_Id := No_List;
      P : Node_Id;
   begin
      if Is_Defined_Property (E, ARINC653_Actions_Name) then
         P :=
           AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => ARINC653_Actions_Name,
              In_Mode       => No_Name);

         L := Multi_Value (AIN.Property_Association_Value (P));
      elsif Is_Defined_Property (E, ARINC653_Module_Recovery_Actions_Name) then
         P :=
           AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => ARINC653_Module_Recovery_Actions_Name,
              In_Mode       => No_Name);

         L := Multi_Value (AIN.Property_Association_Value (P));
      elsif Is_Defined_Property
          (E,
           ARINC653_Partition_Recovery_Actions_Name)
      then
         P :=
           AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => ARINC653_Partition_Recovery_Actions_Name,
              In_Mode       => No_Name);

         L := Multi_Value (AIN.Property_Association_Value (P));
      elsif Is_Defined_Property
          (E,
           ARINC653_Process_Recovery_Actions_Name)
      then
         P :=
           AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (E),
              Property_Name => ARINC653_Process_Recovery_Actions_Name,
              In_Mode       => No_Name);

         L := Multi_Value (AIN.Property_Association_Value (P));
      else
         L := No_List;
      end if;

      if ATNU.Is_Empty (L) then
         return ARINC653_Empty_Actions;
      else
         declare
            Handled_Actions : ARINC653_Actions (1 .. Int (ATNU.Length (L)));
            S               : Node_Id;
            I               : Int := 1;
         begin
            S := ATN.First_Node (L);
            while Present (S) loop
               if ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Ignore_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Ignore;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Confirm_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Confirm;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Process_Restart_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Process_Restart;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Process_Stop_And_Start_Other_Name
               then
                  Handled_Actions (I) :=
                    ARINC653_Action_Process_Stop_And_Start_Another;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Process_Stop_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Process_Stop;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Partition_Restart_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Partition_Restart;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Warm_Restart_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Partition_Restart;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Cold_Restart_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Partition_Restart;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Partition_Stop_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Partition_Stop;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Module_Stop_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Module_Stop;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Stop_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Module_Stop;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Module_Restart_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Module_Restart;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Reset_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Module_Restart;
               elsif ATN.Name (ATN.Identifier (S)) =
                 ARINC653_Action_Nothing_Name
               then
                  Handled_Actions (I) := ARINC653_Action_Nothing;
               end if;
               S := ATN.Next_Node (S);
               I := I + 1;
            end loop;
            return Handled_Actions;
         end;
      end if;
   end Get_ARINC653_HM_Actions;

   ---------------------
   -- Get_Driver_Name --
   ---------------------

   function Get_Driver_Name (Device : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property
        (Device, Get_String_Name ("deployment::driver_name"));
   end Get_Driver_Name;

   -----------------------
   -- Get_Configuration --
   -----------------------

   function Get_Configuration (Device : Node_Id) return Name_Id is
   begin
      return Check_And_Get_Property
        (Device, Get_String_Name ("deployment::configuration"));
   end Get_Configuration;

   ----------------------------
   -- Get_Send_Function_Name --
   ----------------------------

   function Get_Send_Function_Name (Device : Node_Id) return Name_Id is
      Impl : Node_Id;
      S    : Node_Id;
      I    : Node_Id;
   begin
      if Device = No_Node then
         return No_Name;
      end if;

      Impl := Get_Implementation (Device);

      if Present (Impl) then
         if not AINU.Is_Empty (AIN.Subcomponents (Impl)) then
            S := AIN.First_Node (AIN.Subcomponents (Impl));
            while Present (S) loop
               I := Corresponding_Instance (S);
               if Is_Subprogram (I)
                 and then
                   AIN.Name (AIN.Identifier (S)) =
                   Get_String_Name ("sender")
                 and then Get_Source_Name (I) /= No_Name
               then
                  return Get_Source_Name (I);
               end if;
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end if;

      return No_Name;
   end Get_Send_Function_Name;

   ----------------------
   -- Get_Base_Address --
   ----------------------

   function Get_Base_Address (D : Node_Id) return Unsigned_Long_Long is
   begin
      return Check_And_Get_Property (D, Base_Address);
   end Get_Base_Address;

   ---------------------
   -- Get_Memory_Size --
   ---------------------

   function Get_Memory_Size (D : Node_Id) return Size_Type is
   begin
      return Get_Size_Property_Value (D, Memory_Size);
   end Get_Memory_Size;

   -----------------
   -- Get_Core_Id --
   -----------------

   function Get_Core_Id (D : Node_Id) return Unsigned_Long_Long is
   begin
      return Check_And_Get_Property (D, Core_Id);
   end Get_Core_Id;

end Ocarina.Backends.Properties;
