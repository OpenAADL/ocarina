------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . P O K _ C . R U N T I M E        --
--                                                                          --
--                                 S p e c                                  --
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

package Ocarina.Backends.POK_C.Runtime is

   --  Headers file that can be included

   type RH_Id is
     (RH_Null,                       --  Workaround to denote a null RH
      RH_Activity,                   --  Activity.h from generated code
      RH_Apex,                       --  apex.h from deos
      RH_ApexType,                   --  apexType.h from vxworks653
      RH_ApexProcess,                --  apexProcess.h from vxworks653
      RH_ApexPartition,              --  apexPartition.h from vxworks653
      RH_ApexBuffer,                 --  apexBuffer.h from vxworks653
      RH_ApexBlackboard,             --  apexBlackboard.h from vxworks653
      RH_ApexEvent,                  --  apexEvent.h from vxworks653
      RH_ApexError,                  --  apexError.h from vxworks653
      RH_ApexQueuing,                --  apexQueuing.h from vxworks653
      RH_ApexSampling,               --  apexSampling.h from vxworks653
      RH_ApexSemaphore,              --  apexSemaphore.h from vxworks653
      RH_ApexTime,                   --  apexTime.h from vxworks653
      RH_Assert,                     --  Assert.h from POK
      RH_Blackboard,                 --  Middleware/blackboard.h from POK
      RH_Buffer,                     --  Middleware/buffer.h from POK
      RH_Error,                      --  Error.h from POK
      RH_Errno,                      --  Errno.h from POK
      RH_Event,                      --  Event.h from POK
      RH_Gtypes,                     --  Gtypes.h from generated code
      RH_Kernel,                     --  Kernel.h from POK
      RH_Partition,                  --  Partition.h from POK
      RH_Port,                       --  Middleware/port.h from POK
      RH_Process,                    --  Process.h from ARINC653
      RH_Queueing,                   --  Queueing.h from ARINC653
      RH_Sampling,                   --  Sampling.h from ARINC653
      RH_Sched,                      --  Sched.h from POK
      RH_Protocols,                  --  Schedvalues.h from POK/libpok
      RH_Schedvalues,                --  Schedvalues.h from POK
      RH_Semaphore,                  --  Schedvalues.h from POK/ARINC653
      RH_Subprograms,                --  Subprograms.h from generated code
      RH_Thread,                     --  Thread.h from POK
      RH_Time,                       --  Time.h from POK
      RH_Types,                      --  Types.h from POK/ARINC653
      RH_Deployment                  --  Deployment.h from generated code
      );

   type RHS_Id is
     (RHS_Null,
      RHS_Generated,
      RHS_ARINC653,
      RHS_Apex,
      RHS_Middleware,
      RHS_Protocols,
      RHS_Core);

   --  Runtime Entities

   type RE_Id is
     (RE_Create_Blackboard,               --  ARINC653 CREATE_BLACKBOARD
      RE_Display_Blackboard,              --  ARINC653 DISPLAY_BLACKBOARD
      RE_Read_Blackboard,                 --  ARINC653 READ_BLACKBOARD
      RE_Clear_Blackboard,                --  ARINC653 CLEAR_BLACKBOARD
      RE_Create_Buffer,                   --  ARINC653 CREATE_BUFFER
      RE_Send_Buffer,                     --  ARINC653 SEND_BUFFER
      RE_Receive_Buffer,                  --  ARINC653 RECEIVE_BUFFER
      RE_Create_Process,                  --  ARINC653 CREATE_PROCESS
      RE_Suspend,                         --  ARINC653 SUSPEND
      RE_Suspend_Self,                    --  ARINC653 SUSPEND_SELF
      RE_Set_Priority,                    --  ARINC653 SET_PRIORITY
      RE_Resume,                          --  ARINC653 RESUME
      RE_Stop,                            --  ARINC653 STOP
      RE_Stop_Self,                       --  ARINC653 STOP_SELF
      RE_Start,                           --  ARINC653 START
      RE_Lock_Preemption,                 --  ARINC653 LOCK_PREEMPTION
      RE_Unlock_Preemption,               --  ARINC653 UNLOCK_PREEMPTION
      RE_Create_Semaphore,                --  ARINC653 CREATE_SEMAPHORE
      RE_Wait_Semaphore,                  --  ARINC653 WAIT_SEMAPHORE
      RE_Signal_Semaphore,                --  ARINC653 SIGNAL_SEMAPHORE
      RE_Get_Semaphore_Id,                --  ARINC653 GET_SEMAPHORE_ID
      RE_Timed_Wait,                      --  ARINC653 TIMED_WAIT
      RE_Periodic_Wait,                   --  ARINC653 PERIODIC_WAIT
      RE_Get_Time,                        --  ARINC653 GET_TIME
      RE_Replenish,                       --  ARINC653 REPLENISH
      RE_Create_Event,                    --  ARINC653 CREATE_EVENT
      RE_Set_Event,                       --  ARINC653 SET_EVENT
      RE_Reset_Event,                     --  ARINC653 RESET_EVENT
      RE_Wait_Event,                      --  ARINC653 WAIT_EVENT
      RE_Get_Event_Id,                    --  ARINC653 GET_EVENT_ID
      RE_Create_Sampling_Port,            --  ARINC653 CREATE_SAMPLING_PORT
      RE_Write_Sampling_Message,          --  ARINC653 WRITE_SAMPLING_MESSAGE
      RE_Read_Sampling_Message,           --  ARINC653 READ_SAMPLING_MESSAGE
      RE_Get_Sampling_Port_Id,            --  ARINC653 GET_SAMPLING_PORT_ID
      RE_Create_Queuing_Port,             --  ARINC653 CREATE_QUEUING_PORT
      RE_Send_Queuing_Message,            --  ARINC653 SEND_QUEUING_MESSAGE
      RE_Receive_Queuing_Message,         --  ARINC653 RECEIVE_QUEUING_MESSAGE
      RE_Get_Queuing_Port_Id,             --  ARINC653 GET_QUEUING_PORT_ID
      RE_Create_Error_Handler,            --  ARINC653 CREATE_ERROR_HANDLER
      RE_Get_Error_Status,                --  ARINC653 GET_ERROR_STATUS
      RE_Set_Partition_Mode,              --  ARINC653 SET_PARTITION_MODE
      RE_Raise_Application_Error,         --  ARINC653 RAISE_APPLICATION_ERROR

      RE_Pok_Thread_Sleep,               -- pok_thread_sleep
      RE_Pok_Thread_Sleep_Until,         -- pok_thread_sleep_until
      RE_Pok_Thread_Create,              -- pok_thread_create
      RE_Pok_Thread_Attr_Init,           -- pok_thread_attr_init
      RE_Pok_Thread_Period,              -- pok_thread_period
      RE_Pok_Thread_Restart,             -- pok_thread_restart
      RE_Pok_Thread_Stop,                -- pok_thread_stop
      RE_Pok_Thread_Stop_Self,           -- pok_thread_stop_self
      RE_Pok_Thread_Suspend,             -- pok_thread_suspend
      RE_Pok_Thread_Wait_Infinite,       -- pok_thread_wait_infinite
      RE_Pok_Partition_Error,            -- pok_partition_error
      RE_Pok_Kernel_Error,               -- pok_kernel_error
      RE_Pok_Kernel_Restart,             -- pok_kernel_restart
      RE_Pok_Kernel_Stop,                -- pok_kernel_stop
      RE_Pok_Partition_Set_Mode,         -- pok_partition_set_mode
      RE_Pok_Port_Queueing_Create,       -- pok_port_queueing_create
      RE_Pok_Port_Virtual_Create,        -- pok_port_virtual_create
      RE_Pok_Port_Queueing_Send,         -- pok_port_queueing_send
      RE_Pok_Port_Queueing_Receive,      -- pok_port_queueing_receive
      RE_Pok_Port_Sampling_Create,       -- pok_port_sampling_create
      RE_Pok_Port_Sampling_Write,        -- pok_port_sampling_write
      RE_Pok_Port_Sampling_Read,         -- pok_port_sampling_read
      RE_Pok_Event_Create,               -- pok_port_event_create
      RE_Pok_Event_Signal,               -- pok_port_event_signal
      RE_Pok_Event_Wait,                 -- pok_port_event_wait
      RE_Pok_Event_Broadcast,            -- pok_port_event_broadcast
      RE_Pok_Buffer_Create,              -- pok_port_buffer_create
      RE_Pok_Buffer_Send,                -- pok_port_buffer_send
      RE_Pok_Buffer_Receive,             -- pok_port_buffer_receive
      RE_Pok_Blackboard_Create,          -- pok_port_blackboard_create
      RE_Pok_Blackboard_Read,            -- pok_port_blackboard_read
      RE_Pok_Blackboard_Display,         -- pok_port_blackboard_display
      RE_Pok_Sem_Create,                 -- pok_sem_create
      RE_Pok_Sem_Wait,                   -- pok_sem_wait
      RE_Pok_Sem_Signal,                 -- pok_sem_signal
      RE_Pok_Time_Compute_Deadline,      -- pok_time_compute_deadline
      RE_Pok_Time_Seconds,               -- pok_port_time_seconds
      RE_Pok_Time_Milliseconds,          -- pok_time_milliseconds
      RE_Pok_Time_Minutes,               -- pok_time_minutes
      RE_Pok_Time_Hours,                 -- pok_time_hours
      RE_Pok_Time_Days,                  -- pok_time_days
      RE_Pok_Error_Handler_Create,       -- pok_error_handler_create
      RE_Pok_Error_Kernel_Callback,      -- pok_error_kernel_callback
      RE_Pok_Error_Partition_Callback,   -- pok_error_partition_callback
      RE_Pok_Error_Ignore,               -- pok_error_ignore
      RE_Pok_Error_Confirm,              -- pok_error_confirm
      RE_Pok_Error_Get,                  -- pok_error_get
      RE_Pok_Error_Handler_Worker,       -- pok_error_handler_worker
      RE_Wait_For_Tasks,                 -- FIXME, to delete in the future

      RE_Default_Priority,               --  FIXME, to delete in the future
      RE_Assert_Ret,                     --  ASSERT_RET
      RE_Source,                         --  ARINC653 source value
      RE_No_Error,                       --  ARINC653 NO_ERROR return code
      RE_Fifo,                           --  ARINC653 fifo value
      RE_Priority,                       --  ARINC653 priority value
      RE_Destination,                    --  ARINC653 destination value
      RE_Normal,                         --  ARINC653 normal mode value
      RE_Assert_Ret_With_Exception,      --  ASSERT_RET_WITH_EXCEPTION
      RE_Pok_Generated_Code,             --  POK_GENERATED_CODE
      RE_Pok_Needs_Time,                 --  POK_NEEDS_TIME
      RE_Ocarina_Runtime_Deos,           --  OCARINA_RUNTIME_DEOS
      RE_Ocarina_Runtime_Pok,            --  OCARINA_RUNTIME_DEOS
      RE_Ocarina_Runtime_Vxworks653,     --  OCARINA_RUNTIME_DEOS
      RE_Pok_Needs_Events,               --  POK_NEEDS_EVENTS
      RE_Pok_Needs_Threads,              --  POK_NEEDS_THREADS
      RE_Pok_Needs_Debug,                --  POK_NEEDS_DEBUG
      RE_Pok_Needs_Sched_Rms,            --  POK_NEEDS_SCHED_RMS
      RE_Pok_Needs_Sched_Edf,            --  POK_NEEDS_SCHED_EDF
      RE_Pok_Needs_Sched_Llf,            --  POK_NEEDS_SCHED_LLF
      RE_Pok_Needs_Sched_Rr,             --  POK_NEEDS_SCHED_RR
      RE_Pok_Needs_Console,              --  POK_NEEDS_CONSOLE
      RE_Pok_Needs_Partitions,           --  POK_NEEDS_PARTITIONS
      RE_Pok_Needs_Lockobjects,          --  POK_NEEDS_LOCKOBJECTS
      RE_Pok_Needs_Libmath,              --  POK_NEEDS_LIBMATH
      RE_Pok_Needs_Stdio,                --  POK_NEEDS_STDIO
      RE_Pok_Needs_String,               --  POK_NEEDS_STRING
      RE_Pok_Needs_Libc,                 --  POK_NEEDS_LIBC
      RE_Pok_Needs_Io,                   --  POK_NEEDS_IO
      RE_Pok_Needs_Pci,                  --  POK_NEEDS_PCI
      RE_Pok_Needs_Stdlib,               --  POK_NEEDS_STDLIB
      RE_Pok_Needs_Protocols,            --  POK_NEEDS_PROTOCOLS
      RE_Pok_Needs_Protocols_Des,        --  POK_NEEDS_PROTOCOLS_DES
      RE_Pok_Needs_Protocols_Ceasar,     --  POK_NEEDS_PROTOCOLS_CEASAR
      RE_Pok_Needs_Protocols_Blowfish,   --  POK_NEEDS_PROTOCOLS_BLOWFISH
      RE_Pok_Needs_Libc_String,          --  POK_NEEDS_LIBC_STRING
      RE_Pok_Needs_Ports_Sampling,       --  POK_NEEDS_PORTS_SAMPLING
      RE_Pok_Needs_Ports_Virtual,        --  POK_NEEDS_PORTS_VIRTUAL
      RE_Pok_Needs_Gettick,              --  POK_NEEDS_GETTICK
      RE_Pok_Needs_Ports_Queueing,       --  POK_NEEDS_PORTS_QUEUEING
      RE_Pok_Needs_X86_Vmm,              --  POK_NEEDS_X86_VMM
      RE_Pok_Errno_Empty,                --  POK_ERRNO_EMPTY
      RE_Pok_Errno_Ok,                   --  POK_ERRNO_OK
      RE_Pok_Hw_Addr,                    --  POK_HW_ADDR
      RE_Pok_Pci_Vendor_Id,              --  POK_PCI_VENDOR_ID
      RE_Pok_Pci_Device_Id,              --  POK_PCI_DEVICE_ID
      RE_Pok_Protocols_Des_Init,         --  POK_PROTOCOLS_DES_INIT
      RE_Pok_Protocols_Des_Key,          --  POK_PROTOCOLS_DES_KEY
      RE_Pok_Protocols_Blowfish_Init,    --  POK_PROTOCOLS_BLOWFISH_INIT
      RE_Pok_Protocols_Blowfish_Key,     --  POK_PROTOCOLS_BLOWFISH_KEY
      RE_Pok_Port_Direction_Out,         --  POK_PORT_DIRECTION_OUT
      RE_Pok_Port_Direction_In,          --  POK_PORT_DIRECTION_IN
      RE_Pok_Port_Kind_Queueing,         --  POK_PORT_KIND_QUEUEING
      RE_Pok_Port_Kind_Virtual,          --  POK_PORT_KIND_VIRTUAL
      RE_Pok_Port_Kind_Sampling,         --  POK_PORT_KIND_SAMPLING
      RE_Pok_Semaphore_Discipline_Fifo,
      RE_Pok_Port_Queueing_Discipline_Fifo,
      RE_Pok_Partition_Mode_Init_Cold,   --  POK_PARTITION_MODE_INIT_COLD
      RE_Pok_Partition_Mode_Init_Warm,   --  POK_PARTITION_MODE_INIT_WARM
      RE_Pok_Partition_Mode_Normal,      --  POK_PARTITION_MODE_NORMAL
      RE_Pok_Partition_Mode_Idle,        --  POK_PARTITION_MODE_IDLE
      RE_Pok_Partition_Mode_Stopped,     --  POK_PARTITION_MODE_STOPPED
      RE_Pok_Buffer_Discipline_Fifo,     --  POK_BUFFER_DISCIPLINE_FIFO
      RE_Pok_Needs_ARINC653_Process,
      RE_Pok_Needs_ARINC653_Partition,
      RE_Pok_Needs_ARINC653_Time,
      RE_Pok_Needs_ARINC653_Error,
      RE_Pok_Needs_ARINC653_Queueing,
      RE_Pok_Needs_ARINC653_Sampling,
      RE_Pok_Needs_ARINC653_Buffer,
      RE_Pok_Needs_ARINC653_Blackboard,
      RE_Pok_Needs_ARINC653_Event,
      RE_Pok_Needs_ARINC653_Semaphore,
      RE_Pok_Needs_Buffers,              --  POK_NEEDS_BUFFERS
      RE_Pok_Needs_Semaphores,           --  POK_NEEDS_SEMAPHORES
      RE_Pok_Needs_Blackboards,          --  POK_NEEDS_BLACKBOARDS
      RE_Pok_Needs_Middleware,           --  POK_NEEDS_MIDDLEWARE
      RE_Pok_Needs_Error_Handling,       --  POK_NEEDS_ERROR_HANDLING
      RE_Pok_Needs_Sched,                --  POK_NEEDS_SCHED
      RE_Pok_Use_Generated_Error_Handler, --  POK_USE_GENERATED_ERROR_HANDLER
      RE_Pok_Use_Generated_Partition_Error_Handler,
      RE_Pok_Use_Generated_Partition_Error_Callback,
      RE_Pok_Use_Generated_Kernel_Error_Handler,
      RE_Pok_Use_Generated_Kernel_Error_Callback,
      RE_Pok_Config_Needs_Debug,         --  POK_CONFIG_NEEDS_DEBUG
      RE_Pok_Config_Nb_Threads,          --  POK_CONFIG_NB_THREADS
      RE_Pok_Config_Nb_Buses,            --  POK_CONFIG_NB_BUSES
      RE_Pok_Config_ARINC653_Nb_Semaphores,
      RE_Pok_Config_ARINC653_Nb_Events,
      RE_Pok_Config_Nb_Partitions,       --  POK_CONFIG_NB_PARTITIONS
      RE_Pok_Config_Nb_Blackboards,      --  POK_CONFIG_NB_BLACKBOARDS
      RE_Pok_Config_Nb_Buffers,          --  POK_CONFIG_NB_BUFFERS
      RE_Pok_Config_Nb_Events,           --  POK_CONFIG_NB_EVENTS
      RE_Pok_Config_Nb_Lockobjects,      --  POK_CONFIG_NB_LOCKOBJECTS
      RE_Pok_Config_Nb_Ports,            --  POK_CONFIG_NB_PORTS
      RE_Pok_Config_Nb_Nodes,            --  POK_CONFIG_NB_NODES
      RE_Pok_Config_Nb_Global_Ports,     --  POK_CONFIG_NB_GLOBAL_PORTS
      RE_Pok_Config_Partitions_Size,     --  POK_CONFIG_PARTITIONS_SIZE
      RE_Pok_Config_Partitions_Ports,     --  POK_CONFIG_PARTITIONS_PORTS
      RE_Pok_Config_Partitions_Timeslice, --  POK_CONFIG_PARTITIONS_TIMESLICE
      RE_Pok_Config_Partitions_Scheduler, --  POK_CONFIG_PARTITIONS_SCHEDULER
      RE_Pok_Config_Scheduling_Slots,     --  POK_CONFIG_SCHEDULING_SLOTS
      RE_Pok_Config_Scheduling_Nbslots,   --  POK_CONFIG_SCHEDULING_NB_SLOTS
      RE_Pok_Sched_Fifo,                  --  POK_SCHED_FIFO
      RE_Pok_Sched_Rr,                    --  POK_SCHED_RR
      RE_Pok_Config_Local_Node,           --  POK_CONFIG_LOCAL_NODE
      RE_Pok_Sched_Rms,                   --  POK_SCHED_RMS
      RE_Pok_Sched_Edf,                   --  POK_SCHED_EDF
      RE_Pok_Sched_Llf,                   --  POK_SCHED_LLF
      RE_Pok_Sched_Global_Timeslice,      --  POK_SCHED_GLOBAL_TIMESLICE
      RE_Pok_Error_Kind_Stack_Overflow,   --  POK_ERROR_KIND_STACK_OVERFLOW
      RE_Pok_Error_Kind_Deadline_Missed,  --  POK_ERROR_KIND_DEADLINE_MISSED
      RE_Pok_Error_Kind_Application_Error, --  POK_ERROR_KIND_APPLICATION_ERROR
      RE_Pok_Error_Kind_Numeric_Error,      --  POK_ERROR_KIND_NUMERIC_ERROR
      RE_Pok_Error_Kind_Illegal_Request,    --  POK_ERROR_KIND_ILLEGAL_REQUEST
      RE_Pok_Error_Kind_Memory_Violation,   --  POK_ERROR_KIND_MEMORY_VIOLATION
      RE_Pok_Error_Kind_Hardware_Fault,     --  POK_ERROR_KIND_HARDWARE_FAULT
      RE_Pok_Error_Kind_Power_Fail,         --  POK_ERROR_KIND_POWER_FAIL
      RE_Pok_Error_Kind_Partition_Configuration, --  POK_ERROR_KIND_PARTITION
      RE_Pok_Error_Kind_Partition_Init,       --  POK_ERROR_KIND_PARTITION_INIT
      RE_Pok_Error_Kind_Partition_Scheduling, --  POK_ERROR_KIND_PARTITION
      RE_Pok_Error_Kind_Partition_Handler,    --  POK_ERROR_KIND_PARTITION_PROC
      RE_Pok_Error_Kind_Kernel_Init,          --  POK_ERROR_KIND_KERNEL_INIT
      RE_Pok_Error_Kind_Kernel_Config,        --  POK_ERROR_KIND_KERNEL_CONFIG
      RE_Pok_Error_Kind_Kernel_Scheduling,    --  POK_ERROR_KIND_KERNEL_SCHED
      RE_Pok_Config_Scheduling_Slots_Allocation,
      RE_Pok_Config_Stacks_Size,
      RE_Pok_Config_Scheduling_Major_Frame,
      RE_Null,                            --  NULL
      RE_Pok_Config_Partitions_Nlockobjects, --  POK_CONFIG_PARTITIONS_NLOCKOBJ
      RE_Pok_Config_Partitions_Nthreads, --  POK_CONFIG_PARTITIONS_NTHREADS

      RE_Nb_Tasks,                       --  FIXME :to be deleted in the future

      RE_Task_Id,                        --  FIXME :to be deleted in the future
      RE_Apex_Char,                      --  ARIND653 APEX_CHAR
      RE_Error_Status_Type,              --  ARINC653 Error Status Type
      RE_Process_Attribute_Type,         --  ARINC653 Process Attribute Type
      RE_Process_Id_Type,                --  ARIND653 Process_Id_Type
      RE_Infinite_Time_Value,            --  ARIND653 INFINITE_TIME_VALUE
      RE_Message_Size_Type,              --  ARINC653 Message_size_type
      RE_Validity_Type,                  --  ARINC653 Validity_type
      RE_Buffer_Id_Type,                 --  ARINC653 Buffer_Id_Type
      RE_Event_Id_Type,                  --  ARINC653 Event_Id_Type
      RE_Blackboard_Id_Type,             --  ARINC653 Blackboard_Id_Type
      RE_Sampling_Port_Id_Type,          --  ARINC653 Sampling_Port_Id
      RE_Semaphore_Id_Type,              --  ARINC653 Semaphore_Port_Id
      RE_Queuing_Port_Id_Type,           --  ARINC653 Queueing_Port_Id
      RE_Apex_Integer,                   --  APEX_INTEGER
      RE_Apex_Natural,                   --  APEX_NATURAL
      RE_Return_Code_Type,               --  RETURN_CODE_TYPE

      RE_Pok_Ret_T,                      --  pok_ret_t
      RE_Bool_T,                         --  bool_t
      RE_Char,                           --  Char
      RE_Int,                            --  int
      RE_Size_T,                         --  size_t
      RE_Int8_T,                         --  int8_t
      RE_Int16_T,                        --  int16_t
      RE_Int32_T,                        --  int32_t
      RE_Int64_T,                        --  int64_t
      RE_Uint8_T,                        --  uint8_t
      RE_Uint16_T,                       --  uint16_t
      RE_Uint32_T,                       --  uint32_t
      RE_Uint64_T,                       --  uint64_t
      RE_Pok_Error_Status_T,             --  pok_error_status_t from POK
      RE_Pok_Thread_Attr_T,              --  pok_thread_attr_t from POK
      RE_Pok_Port_Kind_T,                --  pok_port_kind_t from POK
      RE_Pok_Port_Size_T,                --  pok_port_size_t from POK
      RE_Pok_Node_Identifier_T,          --  pok_node_identifier_t
      RE_Pok_Bus_Identifier_T,           --  pok_bus_identifier_t
      RE_Pok_Port_Identifier_T,          --  pok_port_identifier_t
      RE_Pok_Event_Id_T,                 --  pok_event_id_t
      RE_Pok_Port_Local_Identifier_T,    --  pok_port_local_identifier_t
      RE_Main,                           --  main
      RE_Pok_Time_T,                     --  pok_time_t
      RE_Pok_Sem_Id_T,                   --  pok_sem_id_t
      RE_Node_T,                         --  FIXME :to be deleted in the future

      RE_Pok_Buffers_Names,              --  pok_buffers_names
      RE_Pok_ARINC653_Semaphores_Names,  --  pok_arinc653_semaphores_names
      RE_Pok_ARINC653_Events_Names,  --  pok_arinc653_events_names
      RE_Arinc_Threads,                  --  arinc_threads
      RE_Pok_Threads,                    --  pok_threads
      RE_Pok_Ports_Nb_Ports_By_Partition, --  pok_ports_nb_ports_by_partition
      RE_Pok_Ports_By_Partition,         --  pok_ports_by_partition
      RE_Pok_Local_Ports_To_Global_Ports,
      RE_Pok_Global_Ports_To_Local_Ports,
      RE_Pok_Buses_Partitions,
      RE_Pok_Global_Ports_To_Bus,
      RE_Pok_Ports_Names,                --  pok_ports_names
      RE_Pok_Ports_Nodes,                --  pok_ports_nodes
      RE_Pok_Ports_Identifiers,          --  pok_ports_identifiers
      RE_Pok_Ports_Nb_Destinations,      --  pok_ports_nb_destinations
      RE_Pok_Ports_Destinations,         --  pok_ports_destinations
      RE_Pok_Ports_Kind,                 --  pok_ports_kind
      RE_Pok_Blackboards_Names,          --  pok_blackboards_names

      RE_Entry_Point,
      RE_Name,
      RE_Entry,
      RE_Deadline,
      RE_Time_Capacity,
      RE_Base_Priority,
      RE_Stack_Size,
      RE_Failed_Process_Id,
      RE_Error_Code,
      RE_Deadline_Missed,
      RE_Application_Error,
      RE_Numeric_Error,
      RE_Illegal_Request,
      RE_Stack_Overflow,
      RE_Memory_Violation,
      RE_Hardware_Fault,
      RE_Power_Fail,
      RE_Period);

   --  Runtime types

   subtype MR_Id is RE_Id range RE_Entry_Point .. RE_Period;

   --  The MR type are the potential member runtime. These identifier
   --  are used in structures, unions, etc.

   subtype ARF_Id is
     RE_Id range RE_Create_Blackboard .. RE_Raise_Application_Error;
   --  The ARF_Id type represents ARINC653 functions.

   subtype PRF_Id is RE_Id range RE_Pok_Thread_Sleep .. RE_Wait_For_Tasks;
   --  The PRF_Id type represents POK functions.

   subtype ART_Id is RE_Id range RE_Apex_Char .. RE_Return_Code_Type;
   --  The ART_Id type represents ARINC653 types.

   subtype PRT_Id is RE_Id range RE_Pok_Ret_T .. RE_Node_T;
   --  The PRT_Id type represents POK types.

   subtype RT_Id is RE_Id range RE_Task_Id .. RE_Node_T;
   --  All types (ARINC653, POK, ...)

   subtype RC_Id is RE_Id range RE_Default_Priority .. RE_Nb_Tasks;
   --  All constants (mostly macros)

   subtype RF_Id is RE_Id range RE_Create_Blackboard .. RE_Wait_For_Tasks;
   --  All functions (ARINC653, POK, ...).

   subtype RV_Id is
     RE_Id range RE_Pok_Buffers_Names .. RE_Pok_Blackboards_Names;
   --  Global variables defined or used in the generated code (mostly
   --  variables defined in deployment.c). These variables are mostly
   --  defined for configuration purposes (POK-dependent). By the way,
   --  if we want to integrate generated code in other OS, we should
   --  adapt this mechanism.

   RF_Define_Table : constant array (RF_Id) of RE_Id :=
     (RE_Create_Blackboard            => RE_Pok_Needs_ARINC653_Blackboard,
      RE_Display_Blackboard           => RE_Pok_Needs_ARINC653_Blackboard,
      RE_Read_Blackboard              => RE_Pok_Needs_ARINC653_Blackboard,
      RE_Clear_Blackboard             => RE_Pok_Needs_ARINC653_Blackboard,
      RE_Create_Buffer                => RE_Pok_Needs_ARINC653_Buffer,
      RE_Send_Buffer                  => RE_Pok_Needs_ARINC653_Buffer,
      RE_Receive_Buffer               => RE_Pok_Needs_ARINC653_Buffer,
      RE_Create_Process               => RE_Pok_Needs_ARINC653_Process,
      RE_Suspend                      => RE_Pok_Needs_ARINC653_Process,
      RE_Suspend_Self                 => RE_Pok_Needs_ARINC653_Process,
      RE_Set_Priority                 => RE_Pok_Needs_ARINC653_Process,
      RE_Resume                       => RE_Pok_Needs_ARINC653_Process,
      RE_Stop                         => RE_Pok_Needs_ARINC653_Process,
      RE_Stop_Self                    => RE_Pok_Needs_ARINC653_Process,
      RE_Start                        => RE_Pok_Needs_ARINC653_Process,
      RE_Lock_Preemption              => RE_Pok_Needs_ARINC653_Process,
      RE_Unlock_Preemption            => RE_Pok_Needs_ARINC653_Process,
      RE_Create_Semaphore             => RE_Pok_Needs_ARINC653_Semaphore,
      RE_Wait_Semaphore               => RE_Pok_Needs_ARINC653_Semaphore,
      RE_Signal_Semaphore             => RE_Pok_Needs_ARINC653_Semaphore,
      RE_Get_Semaphore_Id             => RE_Pok_Needs_ARINC653_Semaphore,
      RE_Timed_Wait                   => RE_Pok_Needs_ARINC653_Time,
      RE_Periodic_Wait                => RE_Pok_Needs_ARINC653_Time,
      RE_Get_Time                     => RE_Pok_Needs_ARINC653_Time,
      RE_Replenish                    => RE_Pok_Needs_ARINC653_Time,
      RE_Create_Event                 => RE_Pok_Needs_ARINC653_Event,
      RE_Set_Event                    => RE_Pok_Needs_ARINC653_Event,
      RE_Reset_Event                  => RE_Pok_Needs_ARINC653_Event,
      RE_Wait_Event                   => RE_Pok_Needs_ARINC653_Event,
      RE_Get_Event_Id                 => RE_Pok_Needs_ARINC653_Event,
      RE_Create_Sampling_Port         => RE_Pok_Needs_ARINC653_Sampling,
      RE_Write_Sampling_Message       => RE_Pok_Needs_ARINC653_Sampling,
      RE_Read_Sampling_Message        => RE_Pok_Needs_ARINC653_Sampling,
      RE_Get_Sampling_Port_Id         => RE_Pok_Needs_ARINC653_Sampling,
      RE_Create_Queuing_Port          => RE_Pok_Needs_ARINC653_Queueing,
      RE_Send_Queuing_Message         => RE_Pok_Needs_ARINC653_Queueing,
      RE_Receive_Queuing_Message      => RE_Pok_Needs_ARINC653_Queueing,
      RE_Get_Queuing_Port_Id          => RE_Pok_Needs_ARINC653_Queueing,
      RE_Create_Error_Handler         => RE_Pok_Needs_ARINC653_Error,
      RE_Set_Partition_Mode           => RE_Pok_Needs_ARINC653_Partition,
      RE_Get_Error_Status             => RE_Pok_Needs_ARINC653_Error,
      RE_Raise_Application_Error      => RE_Pok_Needs_ARINC653_Error,
      RE_Pok_Thread_Sleep             => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Sleep_Until       => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Restart           => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Period            => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Suspend           => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Stop              => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Stop_Self         => RE_Pok_Needs_Threads,
      RE_Pok_Port_Sampling_Create     => RE_Pok_Needs_Ports_Sampling,
      RE_Pok_Port_Sampling_Write      => RE_Pok_Needs_Ports_Sampling,
      RE_Pok_Port_Sampling_Read       => RE_Pok_Needs_Ports_Sampling,
      RE_Pok_Port_Queueing_Create     => RE_Pok_Needs_Ports_Queueing,
      RE_Pok_Port_Virtual_Create      => RE_Pok_Needs_Ports_Virtual,
      RE_Pok_Port_Queueing_Send       => RE_Pok_Needs_Ports_Queueing,
      RE_Pok_Port_Queueing_Receive    => RE_Pok_Needs_Ports_Queueing,
      RE_Pok_Blackboard_Create        => RE_Pok_Needs_Blackboards,
      RE_Pok_Blackboard_Read          => RE_Pok_Needs_Blackboards,
      RE_Pok_Blackboard_Display       => RE_Pok_Needs_Blackboards,
      RE_Pok_Sem_Create               => RE_Pok_Needs_Semaphores,
      RE_Pok_Sem_Wait                 => RE_Pok_Needs_Semaphores,
      RE_Pok_Sem_Signal               => RE_Pok_Needs_Semaphores,
      RE_Pok_Event_Create             => RE_Pok_Needs_Events,
      RE_Pok_Event_Wait               => RE_Pok_Needs_Events,
      RE_Pok_Event_Signal             => RE_Pok_Needs_Events,
      RE_Pok_Event_Broadcast          => RE_Pok_Needs_Events,
      RE_Pok_Buffer_Create            => RE_Pok_Needs_Buffers,
      RE_Pok_Buffer_Receive           => RE_Pok_Needs_Buffers,
      RE_Pok_Buffer_Send              => RE_Pok_Needs_Buffers,
      RE_Pok_Thread_Attr_Init         => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Wait_Infinite     => RE_Pok_Needs_Threads,
      RE_Pok_Thread_Create            => RE_Pok_Needs_Threads,
      RE_Wait_For_Tasks               => RE_Pok_Needs_Threads,
      RE_Pok_Error_Handler_Create     => RE_Pok_Needs_Error_Handling,
      RE_Pok_Error_Kernel_Callback    => RE_Pok_Needs_Error_Handling,
      RE_Pok_Error_Partition_Callback => RE_Pok_Needs_Error_Handling,
      RE_Pok_Error_Ignore             => RE_Pok_Needs_Error_Handling,
      RE_Pok_Error_Confirm            => RE_Pok_Needs_Error_Handling,
      RE_Pok_Kernel_Error             => RE_Pok_Needs_Error_Handling,
      RE_Pok_Kernel_Stop              => RE_Pok_Needs_Error_Handling,
      RE_Pok_Kernel_Restart           => RE_Pok_Needs_Error_Handling,
      RE_Pok_Partition_Error          => RE_Pok_Needs_Error_Handling,
      RE_Pok_Error_Get                => RE_Pok_Needs_Error_Handling,
      RE_Pok_Error_Handler_Worker     => RE_Pok_Needs_Error_Handling,
      RE_Pok_Partition_Set_Mode       => RE_Pok_Needs_Partitions,
      RE_Pok_Time_Compute_Deadline    => RE_Pok_Needs_Time,
      RE_Pok_Time_Milliseconds        => RE_Pok_Needs_Time,
      RE_Pok_Time_Seconds             => RE_Pok_Needs_Time,
      RE_Pok_Time_Minutes             => RE_Pok_Needs_Time,
      RE_Pok_Time_Hours               => RE_Pok_Needs_Time,
      RE_Pok_Time_Days                => RE_Pok_Needs_Time);

   RH_Service_Table : array (RH_Id) of RHS_Id :=
     (RH_Null           => RHS_Null,
      RH_Activity       => RHS_Generated,
      RH_Assert         => RHS_Null,
      RH_Apex           => RHS_Null,
      RH_Blackboard     => RHS_Middleware,
      RH_Buffer         => RHS_Middleware,
      RH_Event          => RHS_Core,
      RH_Gtypes         => RHS_Generated,
      RH_Thread         => RHS_Core,
      RH_Time           => RHS_Core,
      RH_Errno          => RHS_Null,
      RH_ApexType       => RHS_Apex,
      RH_ApexProcess    => RHS_Apex,
      RH_ApexPartition  => RHS_Apex,
      RH_ApexBuffer     => RHS_Apex,
      RH_ApexBlackboard => RHS_Apex,
      RH_ApexEvent      => RHS_Apex,
      RH_ApexError      => RHS_Apex,
      RH_ApexQueuing    => RHS_Apex,
      RH_ApexSampling   => RHS_Apex,
      RH_ApexSemaphore  => RHS_Apex,
      RH_ApexTime       => RHS_Apex,
      RH_Kernel         => RHS_Core,
      RH_Error          => RHS_Core,
      RH_Partition      => RHS_Core,
      RH_Port           => RHS_Middleware,
      RH_Process        => RHS_ARINC653,
      RH_Queueing       => RHS_Null,
      RH_Sampling       => RHS_Null,
      RH_Sched          => RHS_Core,
      RH_Schedvalues    => RHS_Core,
      RH_Protocols      => RHS_Protocols,
      RH_Semaphore      => RHS_Core,
      RH_Types          => RHS_Null,
      RH_Subprograms    => RHS_Generated,
      RH_Deployment     => RHS_Generated);

   RE_Header_Table : array (RE_Id) of RH_Id :=
     (
   --  Runtime functions associations
   RE_Pok_Thread_Sleep                => RH_Thread,
      RE_Pok_Thread_Sleep_Until       => RH_Thread,
      RE_Pok_Thread_Wait_Infinite     => RH_Thread,
      RE_Pok_Thread_Create            => RH_Thread,
      RE_Pok_Thread_Attr_Init         => RH_Thread,
      RE_Pok_Thread_Suspend           => RH_Thread,
      RE_Pok_Thread_Restart           => RH_Thread,
      RE_Pok_Thread_Period            => RH_Thread,
      RE_Pok_Thread_Stop              => RH_Thread,
      RE_Pok_Thread_Stop_Self         => RH_Thread,
      RE_Pok_Error_Handler_Create     => RH_Error,
      RE_Pok_Error_Kernel_Callback    => RH_Error,
      RE_Pok_Error_Partition_Callback => RH_Error,
      RE_Pok_Error_Ignore             => RH_Error,
      RE_Pok_Error_Confirm            => RH_Error,
      RE_Pok_Error_Get                => RH_Error,
      RE_Pok_Error_Handler_Worker     => RH_Error,
      RE_Pok_Partition_Set_Mode       => RH_Partition,
      RE_Pok_Port_Sampling_Create     => RH_Port,
      RE_Pok_Port_Sampling_Write      => RH_Port,
      RE_Pok_Port_Sampling_Read       => RH_Port,
      RE_Pok_Port_Queueing_Create     => RH_Port,
      RE_Pok_Port_Virtual_Create      => RH_Port,
      RE_Pok_Port_Queueing_Send       => RH_Port,
      RE_Pok_Port_Queueing_Receive    => RH_Port,
      RE_Pok_Event_Create             => RH_Event,
      RE_Pok_Event_Signal             => RH_Event,
      RE_Pok_Event_Broadcast          => RH_Event,
      RE_Pok_Event_Wait               => RH_Event,
      RE_Pok_Buffer_Create            => RH_Buffer,
      RE_Pok_Kernel_Error             => RH_Null,
      RE_Pok_Kernel_Stop              => RH_Kernel,
      RE_Pok_Kernel_Restart           => RH_Kernel,
      RE_Pok_Partition_Error          => RH_Null,
      RE_Pok_Buffer_Send              => RH_Buffer,
      RE_Pok_Buffer_Receive           => RH_Buffer,
      RE_Pok_Blackboard_Create        => RH_Blackboard,
      RE_Pok_Blackboard_Display       => RH_Blackboard,
      RE_Pok_Blackboard_Read          => RH_Blackboard,
      RE_Pok_Sem_Create               => RH_Semaphore,
      RE_Pok_Sem_Wait                 => RH_Semaphore,
      RE_Pok_Sem_Signal               => RH_Semaphore,
      RE_Pok_Time_Compute_Deadline    => RH_Time,
      RE_Pok_Time_Milliseconds        => RH_Time,
      RE_Pok_Time_Seconds             => RH_Time,
      RE_Pok_Time_Minutes             => RH_Time,
      RE_Pok_Time_Hours               => RH_Time,
      RE_Pok_Time_Days                => RH_Time,
      RE_Wait_For_Tasks               => RH_Deployment,
      RE_Create_Blackboard            => RH_Blackboard,
      RE_Display_Blackboard           => RH_Blackboard,
      RE_Read_Blackboard              => RH_Blackboard,
      RE_Clear_Blackboard             => RH_Blackboard,
      RE_Create_Buffer                => RH_Buffer,
      RE_Send_Buffer                  => RH_Buffer,
      RE_Receive_Buffer               => RH_Buffer,
      RE_Set_Partition_Mode           => RH_Partition,
      RE_Create_Process               => RH_Process,
      RE_Suspend                      => RH_Process,
      RE_Suspend_Self                 => RH_Process,
      RE_Set_Priority                 => RH_Process,
      RE_Resume                       => RH_Process,
      RE_Stop                         => RH_Process,
      RE_Stop_Self                    => RH_Process,
      RE_Start                        => RH_Process,
      RE_Lock_Preemption              => RH_Process,
      RE_Unlock_Preemption            => RH_Process,
      RE_Create_Semaphore             => RH_Semaphore,
      RE_Wait_Semaphore               => RH_Semaphore,
      RE_Signal_Semaphore             => RH_Semaphore,
      RE_Get_Semaphore_Id             => RH_Semaphore,
      RE_Timed_Wait                   => RH_Time,
      RE_Periodic_Wait                => RH_Time,
      RE_Get_Time                     => RH_Time,
      RE_Replenish                    => RH_Time,
      RE_Create_Event                 => RH_Event,
      RE_Set_Event                    => RH_Event,
      RE_Reset_Event                  => RH_Event,
      RE_Wait_Event                   => RH_Event,
      RE_Get_Event_Id                 => RH_Event,
      RE_Create_Sampling_Port         => RH_Sampling,
      RE_Write_Sampling_Message       => RH_Sampling,
      RE_Read_Sampling_Message        => RH_Sampling,
      RE_Get_Sampling_Port_Id         => RH_Sampling,
      RE_Create_Queuing_Port          => RH_Queueing,
      RE_Send_Queuing_Message         => RH_Queueing,
      RE_Receive_Queuing_Message      => RH_Queueing,
      RE_Get_Queuing_Port_Id          => RH_Queueing,
      RE_Create_Error_Handler         => RH_Error,
      RE_Get_Error_Status             => RH_Error,
      RE_Raise_Application_Error      => RH_Error,

   --  Runtime types associations

      RE_Task_Id => RH_Deployment,

      RE_Error_Status_Type      => RH_Error,
      RE_Apex_Char              => RH_Types,
      RE_Process_Attribute_Type => RH_Types,
      RE_Process_Id_Type        => RH_Types,
      RE_Infinite_Time_Value    => RH_Types,
      RE_Sampling_Port_Id_Type  => RH_Types,
      RE_Semaphore_Id_Type      => RH_Semaphore,
      RE_Validity_Type          => RH_Types,
      RE_Buffer_Id_Type         => RH_Buffer,
      RE_Event_Id_Type          => RH_Event,
      RE_Blackboard_Id_Type     => RH_Blackboard,
      RE_Message_Size_Type      => RH_Types,
      RE_Queuing_Port_Id_Type   => RH_Queueing,
      RE_Apex_Integer           => RH_Types,
      RE_Apex_Natural           => RH_Types,
      RE_Return_Code_Type       => RH_Types,

      RE_Pok_Ret_T                   => RH_Types,
      RE_Size_T                      => RH_Types,
      RE_Uint8_T                     => RH_Types,
      RE_Uint16_T                    => RH_Types,
      RE_Uint32_T                    => RH_Types,
      RE_Uint64_T                    => RH_Types,
      RE_Int8_T                      => RH_Types,
      RE_Bool_T                      => RH_Types,
      RE_Int16_T                     => RH_Types,
      RE_Int32_T                     => RH_Types,
      RE_Int64_T                     => RH_Types,
      RE_Int                         => RH_Null,
      RE_Char                        => RH_Null,
      RE_Main                        => RH_Null,
      RE_Pok_Error_Status_T          => RH_Error,
      RE_Pok_Thread_Attr_T           => RH_Thread,
      RE_Pok_Port_Kind_T             => RH_Types,
      RE_Pok_Event_Id_T              => RH_Types,
      RE_Pok_Port_Size_T             => RH_Types,
      RE_Pok_Node_Identifier_T       => RH_Deployment,
      RE_Pok_Bus_Identifier_T        => RH_Deployment,
      RE_Pok_Port_Identifier_T       => RH_Deployment,
      RE_Pok_Port_Local_Identifier_T => RH_Deployment,
      RE_Pok_Time_T                  => RH_Types,
      RE_Pok_Sem_Id_T                => RH_Types,
      RE_Node_T                      => RH_Deployment,

   --  Runtime Constants associations

      RE_Default_Priority                           => RH_Deployment,
      RE_Pok_Port_Direction_Out                     => RH_Port,
      RE_Pok_Port_Direction_In                      => RH_Port,
      RE_Pok_Port_Queueing_Discipline_Fifo          => RH_Port,
      RE_Pok_Semaphore_Discipline_Fifo              => RH_Semaphore,
      RE_Pok_Port_Kind_Queueing                     => RH_Port,
      RE_Pok_Port_Kind_Virtual                      => RH_Port,
      RE_Pok_Port_Kind_Sampling                     => RH_Port,
      RE_Pok_Partition_Mode_Init_Cold               => RH_Partition,
      RE_Pok_Partition_Mode_Init_Warm               => RH_Partition,
      RE_Pok_Partition_Mode_Normal                  => RH_Partition,
      RE_Pok_Partition_Mode_Idle                    => RH_Partition,
      RE_Pok_Partition_Mode_Stopped                 => RH_Partition,
      RE_Pok_Buffer_Discipline_Fifo                 => RH_Buffer,
      RE_Pok_Needs_Error_Handling                   => RH_Deployment,
      RE_Assert_Ret                                 => RH_Assert,
      RE_Assert_Ret_With_Exception                  => RH_Assert,
      RE_Source                                     => RH_Types,
      RE_No_Error                                   => RH_Types,
      RE_Fifo                                       => RH_Types,
      RE_Priority                                   => RH_Types,
      RE_Destination                                => RH_Types,
      RE_Normal                                     => RH_Types,
      RE_Pok_Needs_Time                             => RH_Deployment,
      RE_Pok_Generated_Code                         => RH_Deployment,
      RE_Pok_Needs_Events                           => RH_Deployment,
      RE_Ocarina_Runtime_Deos                       => RH_Deployment,
      RE_Ocarina_Runtime_Pok                        => RH_Deployment,
      RE_Ocarina_Runtime_Vxworks653                 => RH_Deployment,
      RE_Pok_Needs_Ports_Sampling                   => RH_Deployment,
      RE_Pok_Needs_Ports_Virtual                    => RH_Deployment,
      RE_Pok_Needs_Gettick                          => RH_Null,
      RE_Pok_Needs_Ports_Queueing                   => RH_Deployment,
      RE_Pok_Needs_X86_Vmm                          => RH_Deployment,
      RE_Pok_Errno_Empty                            => RH_Errno,
      RE_Pok_Errno_Ok                               => RH_Errno,
      RE_Pok_Hw_Addr                                => RH_Deployment,
      RE_Pok_Pci_Device_Id                          => RH_Deployment,
      RE_Pok_Pci_Vendor_Id                          => RH_Deployment,
      RE_Pok_Protocols_Des_Init                     => RH_Deployment,
      RE_Pok_Protocols_Des_Key                      => RH_Deployment,
      RE_Pok_Protocols_Blowfish_Init                => RH_Deployment,
      RE_Pok_Protocols_Blowfish_Key                 => RH_Deployment,
      RE_Pok_Needs_Lockobjects                      => RH_Deployment,
      RE_Pok_Needs_Libmath                          => RH_Deployment,
      RE_Pok_Needs_Stdio                            => RH_Deployment,
      RE_Pok_Needs_String                           => RH_Deployment,
      RE_Pok_Needs_Stdlib                           => RH_Deployment,
      RE_Pok_Needs_Io                               => RH_Deployment,
      RE_Pok_Needs_Pci                              => RH_Deployment,
      RE_Pok_Needs_Sched_Rms                        => RH_Deployment,
      RE_Pok_Needs_Sched_Edf                        => RH_Deployment,
      RE_Pok_Needs_Sched_Llf                        => RH_Deployment,
      RE_Pok_Needs_Sched_Rr                         => RH_Deployment,
      RE_Pok_Needs_Buffers                          => RH_Deployment,
      RE_Pok_Needs_Semaphores                       => RH_Deployment,
      RE_Pok_Needs_Blackboards                      => RH_Deployment,
      RE_Pok_Needs_Threads                          => RH_Deployment,
      RE_Pok_Needs_Partitions                       => RH_Deployment,
      RE_Pok_Needs_ARINC653_Process                 => RH_Deployment,
      RE_Pok_Needs_ARINC653_Partition               => RH_Deployment,
      RE_Pok_Needs_ARINC653_Time                    => RH_Deployment,
      RE_Pok_Needs_ARINC653_Error                   => RH_Deployment,
      RE_Pok_Needs_ARINC653_Queueing                => RH_Deployment,
      RE_Pok_Needs_ARINC653_Sampling                => RH_Deployment,
      RE_Pok_Needs_ARINC653_Buffer                  => RH_Deployment,
      RE_Pok_Needs_ARINC653_Blackboard              => RH_Deployment,
      RE_Pok_Needs_ARINC653_Event                   => RH_Deployment,
      RE_Pok_Needs_ARINC653_Semaphore               => RH_Deployment,
      RE_Pok_Config_Scheduling_Slots                => RH_Deployment,
      RE_Pok_Config_Scheduling_Nbslots              => RH_Deployment,
      RE_Pok_Config_Stacks_Size                     => RH_Deployment,
      RE_Pok_Config_Scheduling_Slots_Allocation     => RH_Deployment,
      RE_Pok_Config_Scheduling_Major_Frame          => RH_Deployment,
      RE_Pok_Needs_Debug                            => RH_Deployment,
      RE_Pok_Needs_Console                          => RH_Deployment,
      RE_Pok_Needs_Sched                            => RH_Deployment,
      RE_Pok_Needs_Protocols                        => RH_Deployment,
      RE_Pok_Needs_Protocols_Des                    => RH_Deployment,
      RE_Pok_Needs_Protocols_Blowfish               => RH_Deployment,
      RE_Pok_Needs_Protocols_Ceasar                 => RH_Deployment,
      RE_Pok_Needs_Libc_String                      => RH_Deployment,
      RE_Pok_Needs_Libc                             => RH_Deployment,
      RE_Pok_Use_Generated_Error_Handler            => RH_Null,
      RE_Pok_Use_Generated_Kernel_Error_Handler     => RH_Null,
      RE_Pok_Use_Generated_Kernel_Error_Callback    => RH_Null,
      RE_Pok_Use_Generated_Partition_Error_Callback => RH_Null,
      RE_Pok_Use_Generated_Partition_Error_Handler  => RH_Null,
      RE_Pok_Needs_Middleware                       => RH_Deployment,
      RE_Pok_Config_Needs_Debug                     => RH_Deployment,
      RE_Pok_Config_Nb_Threads                      => RH_Deployment,
      RE_Pok_Config_Nb_Buses                        => RH_Deployment,
      RE_Pok_Config_Nb_Lockobjects                  => RH_Deployment,
      RE_Pok_Config_ARINC653_Nb_Semaphores          => RH_Deployment,
      RE_Pok_Config_ARINC653_Nb_Events              => RH_Deployment,
      RE_Pok_Config_Nb_Partitions                   => RH_Deployment,
      RE_Pok_Config_Nb_Blackboards                  => RH_Deployment,
      RE_Pok_Config_Nb_Buffers                      => RH_Deployment,
      RE_Pok_Config_Nb_Events                       => RH_Deployment,
      RE_Pok_Config_Nb_Ports                        => RH_Deployment,
      RE_Pok_Config_Nb_Nodes                        => RH_Deployment,
      RE_Pok_Config_Nb_Global_Ports                 => RH_Deployment,
      RE_Pok_Config_Partitions_Size                 => RH_Deployment,
      RE_Pok_Config_Partitions_Ports                => RH_Deployment,
      RE_Pok_Config_Partitions_Timeslice            => RH_Deployment,
      RE_Pok_Config_Partitions_Scheduler            => RH_Deployment,
      RE_Pok_Config_Partitions_Nthreads             => RH_Deployment,
      RE_Pok_Config_Partitions_Nlockobjects         => RH_Deployment,
      RE_Pok_Error_Kind_Deadline_Missed             => RH_Error,
      RE_Pok_Error_Kind_Application_Error           => RH_Error,
      RE_Pok_Error_Kind_Numeric_Error               => RH_Error,
      RE_Pok_Error_Kind_Illegal_Request             => RH_Error,
      RE_Pok_Error_Kind_Stack_Overflow              => RH_Error,
      RE_Pok_Config_Local_Node                      => RH_Deployment,
      RE_Pok_Sched_Fifo                             => RH_Schedvalues,
      RE_Pok_Sched_Rr                               => RH_Schedvalues,
      RE_Pok_Sched_Rms                              => RH_Schedvalues,
      RE_Pok_Sched_Edf                              => RH_Schedvalues,
      RE_Pok_Sched_Llf                              => RH_Schedvalues,
      RE_Pok_Sched_Global_Timeslice                 => RH_Schedvalues,
      RE_Pok_Error_Kind_Memory_Violation            => RH_Error,
      RE_Pok_Error_Kind_Hardware_Fault              => RH_Error,
      RE_Pok_Error_Kind_Power_Fail                  => RH_Error,
      RE_Pok_Error_Kind_Partition_Configuration     => RH_Error,
      RE_Pok_Error_Kind_Partition_Init              => RH_Error,
      RE_Pok_Error_Kind_Partition_Scheduling        => RH_Error,
      RE_Pok_Error_Kind_Partition_Handler           => RH_Error,
      RE_Pok_Error_Kind_Kernel_Init                 => RH_Error,
      RE_Pok_Error_Kind_Kernel_Config               => RH_Error,
      RE_Pok_Error_Kind_Kernel_Scheduling           => RH_Error,
      RE_Null                                       => RH_Types,
      RE_Nb_Tasks                                   => RH_Deployment,

   --  Variables associations

      RE_Pok_Buffers_Names               => RH_Deployment,
      RE_Arinc_Threads                   => RH_Null,
      RE_Pok_Threads                     => RH_Null,
      RE_Pok_Ports_Nb_Ports_By_Partition => RH_Deployment,
      RE_Pok_Ports_By_Partition          => RH_Deployment,
      RE_Pok_Ports_Names                 => RH_Deployment,
      RE_Pok_ARINC653_Semaphores_Names   => RH_Deployment,
      RE_Pok_ARINC653_Events_Names       => RH_Deployment,
      RE_Pok_Ports_Nodes                 => RH_Deployment,
      RE_Pok_Local_Ports_To_Global_Ports => RH_Deployment,
      RE_Pok_Buses_Partitions            => RH_Deployment,
      RE_Pok_Global_Ports_To_Local_Ports => RH_Deployment,
      RE_Pok_Global_Ports_To_Bus         => RH_Deployment,
      RE_Pok_Ports_Identifiers           => RH_Deployment,
      RE_Pok_Ports_Nb_Destinations       => RH_Deployment,
      RE_Pok_Ports_Destinations          => RH_Deployment,
      RE_Pok_Ports_Kind                  => RH_Deployment,
      RE_Pok_Blackboards_Names           => RH_Deployment,

   --  Runtime member elements
      RE_Entry_Point       => RH_Null,
      RE_Name              => RH_Null,
      RE_Entry             => RH_Null,
      RE_Stack_Size        => RH_Null,
      RE_Deadline          => RH_Null,
      RE_Time_Capacity     => RH_Null,
      RE_Base_Priority     => RH_Null,
      RE_Failed_Process_Id => RH_Null,
      RE_Error_Code        => RH_Null,
      RE_Deadline_Missed   => RH_Null,
      RE_Application_Error => RH_Null,
      RE_Numeric_Error     => RH_Null,
      RE_Illegal_Request   => RH_Null,
      RE_Stack_Overflow    => RH_Null,
      RE_Memory_Violation  => RH_Null,
      RE_Hardware_Fault    => RH_Null,
      RE_Power_Fail        => RH_Null,
      RE_Period            => RH_Null);
   procedure Initialize;
   procedure Reset;

   function RE (Id : RE_Id) return Node_Id;
   --  Return a designator for entity Id (a general-purpose
   --  runtime entity).

   function RH (Id : RH_Id) return Node_Id;
   --  Return a node for entity RH_Id (a header name)

   function RF (Id : RF_Id) return Node_Id;
   --  Return a node for a RF_Id (a function).

   procedure User_Mode;
   --  Change runtime entities to fit with the userland
   --  sources tree.

   procedure Kernel_Mode;
   --  Change runtime entities to fit with the kernel
   --  sources tree.

   procedure Register_Kernel_Unit (Unit : Node_Id);
   --  Register the C node that contains the kernel unit.
   --  It is done to add configuration code in the
   --  kernel according to needed services in partitions.

   function Get_Errcode_OK return Node_Id;
   --  Return the errcode value that indicate there is no
   --  error.

   procedure Normal_Mode;

   procedure POK_Mode;

   procedure ARINC653_Mode;

end Ocarina.Backends.POK_C.Runtime;
