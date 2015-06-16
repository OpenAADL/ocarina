------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . P O _ H I _ C . R U N T I M E      --
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

package Ocarina.Backends.PO_HI_C.Runtime is

   --  Headers file that can be included

   type RH_Id is
     (RH_Null,                       --  Workaround to denote a null RH
      RH_PO_HI_Task,                 --  po_hi_task header
      RH_PO_HI_Time,                 --  po_hi_time header
      RH_PO_HI_Common,               --  po_hi_common header
      RH_PO_HI_Lua,                  --  po_hi_lua
      RH_PO_HI_Main,                 --  po_hi_main header
      RH_PO_HI_Messages,             --  po_hi_messages header
      RH_PO_HI_Protected,            --  po_hi_protected header
      RH_PO_HI_Marshallers,          --  po_hi_marshallers header
      RH_PO_HI_Returns,              --  po_hi_returns header
      RH_PO_HI_Transport,            --  po_hi_marshallers header
      RH_PO_HI_Types,                --  po_hi_types header
      RH_PO_HI_Gqueue,               --  po_hi_gqueue header
      RH_PO_HI_Simulink,             --  po_hi_simulink header
      RH_PO_HI_Utils,                --  po_hi_utils header
      RH_Activity,                   --  the activity header
      RH_Request,                    --  the request header
      RH_Types,                      --  the types header
      RH_Marshallers,                --  the marshallers header
      RH_Subprograms,                --  the subprograms
      RH_Deployment);

   --  Runtime Entities

   type RE_Id is
     (RE_Null,                           --  Workaround to denote a null RE
      RE_Microseconds,                   --  __po_hi_microseconds
      RE_Milliseconds,                   --  __po_hi_milliseconds
      RE_Seconds,                        --  __po_hi_seconds
      RE_Minutes,                        --  __po_hi_minutes
      RE_Create_Periodic_Task,           -- __po_hi_create_periodic_task
      RE_Create_Sporadic_Task,           -- __po_hi_create_sporadic_task
      RE_Task_Wait_Offset,               -- __po_hi_task_wait_offset
      RE_Wait_For_Next_Period,           -- __po_hi_task_wait_for_next_period
      RE_Initialize,                     -- __po_hi_initialize
      RE_Initialize_Early,               -- __po_hi_initialize_early
      RE_LUA_Load,                       -- __po_hi_lua_load
      RE_LUA_Init_Function_Call,         -- __po_hi_lua_init_function_call
      RE_LUA_Perform_Function_Call,      -- __po_hi_lua_perform_function_call
      RE_LUA_Push_Boolean,               -- __po_hi_lua_push_boolean
      RE_LUA_Push_Number,                -- __po_hi_lua_push_number
      RE_LUA_Push_String,                -- __po_hi_lua_push_string
      RE_LUA_Get_Boolean,                -- __po_hi_lua_get_boolean
      RE_LUA_Get_Number,                 -- __po_hi_lua_get_number
      RE_LUA_Get_String,                 -- __po_hi_lua_get_string
      RE_LUA_Load_File,                  -- __po_hi_lua_load_file
      RE_LUA_Load_File_With_Function,    -- __po_hi_lua_load_file_with_function
      RE_Wait_Initialization,            -- __po_hi_wait_initialization
      RE_Marshall_Bool,                  -- __po_hi_marshall_bool
      RE_Unmarshall_Bool,                -- __po_hi_unmarshall_bool
      RE_Marshall_Char,                  -- __po_hi_marshall_char
      RE_Unmarshall_Char,                -- __po_hi_unmarshall_char
      RE_Marshall_Float,                 -- __po_hi_marshall_float
      RE_Unmarshall_Float,               -- __po_hi_unmarshall_float
      RE_Marshall_Float32,               -- __po_hi_marshall_float32
      RE_Unmarshall_Float32,             -- __po_hi_unmarshall_float32
      RE_Marshall_Float64,               -- __po_hi_marshall_float64
      RE_Unmarshall_Float64,             -- __po_hi_unmarshall_float64
      RE_Marshall_Request,               -- __po_hi_marshall_request
      RE_Unmarshall_Request,             -- __po_hi_unmarshall_request
      RE_Marshall_Asn1_Request,          -- __po_hi_marshall_asn1_request
      RE_Unmarshall_Asn1_Request,        -- __po_hi_unmarshall_asn1_request
      RE_Marshall_Int,                   -- __po_hi_marshall_int
      RE_Unmarshall_Int,                 -- __po_hi_unmarshall_int
      RE_Marshall_Int8,                  -- __po_hi_marshall_int8
      RE_Unmarshall_Int8,                -- __po_hi_unmarshall_int8
      RE_Marshall_Int16,                 -- __po_hi_marshall_int16
      RE_Unmarshall_Int16,               -- __po_hi_unmarshall_int16
      RE_Marshall_Int32,                 -- __po_hi_marshall_int32
      RE_Unmarshall_Int32,               -- __po_hi_unmarshall_int32
      RE_Marshall_Int64,                 -- __po_hi_marshall_int64
      RE_Unmarshall_Int64,               -- __po_hi_unmarshall_int64
      RE_Marshall_Uint8,                 -- __po_hi_marshall_uint8
      RE_Unmarshall_Uint8,               -- __po_hi_unmarshall_uint8
      RE_Marshall_Uint16,                -- __po_hi_marshall_uint16
      RE_Unmarshall_Uint16,              -- __po_hi_unmarshall_uint16
      RE_Marshall_Uint32,                -- __po_hi_marshall_uint32
      RE_Unmarshall_Uint32,              -- __po_hi_unmarshall_uint32
      RE_Marshall_Uint64,                -- __po_hi_marshall_uint64
      RE_Unmarshall_Uint64,              -- __po_hi_unmarshall_uint64
      RE_Marshall_Port,                  -- __po_hi_marshall_port
      RE_Unmarshall_Port,                -- __po_hi_unmarshall_port
      RE_Marshall_Array,                 -- __po_hi_marshall_array
      RE_Unmarshall_Array,               -- __po_hi_unmarshall_array
      RE_Send_Output,                    -- __po_hi_send_output
      RE_Get_Entity_From_Global_Port,    -- __po_hi_get_entity_from_global_port
      RE_Gqueue_Init,                    -- __po_hi_gqueue_init
      RE_Gqueue_Store_In,                -- __po_hi_gqueue_store_in
      RE_Gqueue_Store_Out,               -- __po_hi_gqueue_store_out
      RE_Gqueue_Get_Count,               -- __po_hi_gqueue_get_count
      RE_Gqueue_Send_Output,             -- __po_hi_gqueue_send_output
      RE_Gqueue_Get_Value,               -- __po_hi_gqueue_get_value
      RE_Gqueue_Next_Value,              -- __po_hi_gqueue_next_value
      RE_Gqueue_Wait_For_Incoming_Event, -- __po_hi_gqueue_wait_for_incoming
      RE_Compute_Next_Period,            -- __po_hi_compute_next_period
      RE_Sporadic_Wait,                  -- __po_hi_sporadic_wait
      RE_Compute_Miss,                   -- __po_hi_compute_miss
      RE_Deployment_Endiannesses,        -- __po_hi_deployment_endiannesses
      RE_Sporadic_Wait_Release,          -- __po_hi_sporadic_wait_release
      RE_Transport_Send_Default,         -- __po_hi_transport_send_default
      RE_Main_Deliver,                   -- __po_hi_main_deliver
      RE_Msg_Reallocate,                 -- __po_hi_msg_reallocate
      RE_Copy_Array,                     -- __po_hi_copy_array
      RE_Protected_Lock,                 -- __po_hi_protected_lock
      RE_Protected_Unlock,               -- __po_hi_protected_unlock
      RE_Simulink_Init,                  -- __po_hi_simulink_init
      RE_Simulink_Find_Signal,           -- __po_hi_simulink_find_signal
      RE_Simulink_Find_Var,              -- __po_hi_simulink_find_var
      RE_Simulink_Find_Parameter,        -- __po_hi_simulink_find_parameter
      RE_Simulink_Update,                -- __po_hi_simulink_update
      RE_Wait_End_Of_Instrumentation,    -- __po_hi_wait_end_of_instrumentation
      RE_Wait_For_Tasks,                 -- __po_hi_wait_for_tasks

      RE_Default_Priority,               --  __PO_HI_DEFAULT_PRIORITY
      RE_Marshallers_Asn1_Copy_Member,   --  __PO_HI_MARSHALLERS_ASN1_COPY_MEMB
      RE_Main_Type,                      --  __PO_HI_MAIN_TYPE
      RE_Main_Args,                      --  __PO_HI_MAIN_ARGS
      RE_Main_Name,                      --  __PO_HI_MAIN_NAME
      RE_Main_Return,                    --  __PO_HI_MAIN_RETURN
      RE_Nb_Servers,                     --  __PO_HI_NB_SERVERS
      RE_Nb_Ports,                       --  __PO_HI_NB_PORTS
      RE_Noport,                         --  __PO_HI_NOPORT
      RE_Bigendian,                      --  __PO_HI_BIGENDIAN
      RE_Littleendian,                   --  __PO_HI_LITTLEENDIAN
      RE_Noaddr,                         --  __PO_HI_NOADDR
      RE_No_Device,                      --  __PO_HI_NO_DEVICE
      RE_Unused_Node,                    --  __PO_HI_UNUSED_NODE
      RE_Nb_Protected,                   --  __PO_HI_NB_PROTECTED
      RE_Protected_Regular,              --  __PO_HI_PROTECTED_REGULAR
      RE_Protected_PIP,                  --  __PO_HI_PROTECTED_PIP
      RE_Protected_PCP,                  --  __PO_HI_PROTECTED_PCP
      RE_Protected_IPCP,                 --  __PO_HI_PROTECTED_IPCP
      RE_Nb_Nodes,                       --  __PO_HI_NB_NODES
      RE_Nb_Devices,                     --  __PO_HI_NB_DEVICES
      RE_Nb_Buses,                       --  __PO_HI_NB_BUSES
      RE_Nb_Protocols,                   --  __PO_HI_NB_PROTOCOLS
      RE_My_Node,                        --  __PO_HI_MY_NODE
      RE_Port_Type_Content,              --  __PO_HI_PORT_TYPE_CONTENT
      RE_Gqueue_Fifo_Indata,             --  __PO_HI_GQUEUE_FIFO_DATA
      RE_Gqueue_Fifo_Out,                --  __PO_HI_GQUEUE_FIFO_OUT
      RE_Nb_Operations,                  --  __PO_HI_NB_OPERATIONS
      RE_Nb_Entities,                    --  __PO_HI_NB_ENTITIES
      RE_Simulink_Node,                  --  __PO_HI_SIMULINK_NODE
      RE_Simulink_Init_Func,             --  __PO_HI_SIMULINK_INIT
      RE_Simulink_Model_Type,            --  __PO_HI_SIMULINK_MODEL
      RE_Nb_Tasks,                       --  __PO_HI_NB_TASKS
      RE_In_Data_Inter_Process,
      RE_In_Data_Intra_Process,
      RE_Out_Data_Inter_Process,
      RE_Out_Data_Intra_Process,
      RE_In_Event_Inter_Process,
      RE_In_Event_Intra_Process,
      RE_Out_Event_Inter_Process,
      RE_Out_Event_Intra_Process,
      RE_In_Event_Data_Inter_Process,
      RE_In_Event_Data_Intra_Process,
      RE_Out_Event_Data_Inter_Process,
      RE_Out_Event_Data_Intra_Process,
      RE_Need_Driver_Sockets,            --  __PO_HI_NEED_DRIVER_SOCKETS
      RE_SUCCESS,                        --  __PO_HI_SUCCESS

      RE_Task_Id,                        --  __po_hi_task_id
      RE_Device_Id,                      --  __po_hi_device_id
      RE_Port_Kind_T,                    --  __po_hi_port_kind_t
      RE_Bus_Id,                         --  __po_hi_bus_id
      RE_Entity_T,                       --  __po_hi_entity_t
      RE_Inetport_T,                     --  __po_hi_inetport_t
      RE_Inetaddr_T,                     --  __po_hi_inetaddr_t
      RE_Protected_T,                    --  __po_hi_protected_t
      RE_Entity_Server_T,                --  __po_hi_entity_server_t
      RE_Operation_T,                    --  __po_hi_operation_t
      RE_Request_T,                      --  __po_hi_request_t
      RE_Protected_Protocol_T,           --  __po_hi_protected_protocol_t
      RE_Bool_T,                         --  __po_hi_bool_t
      RE_Lua_Context_T,                  --  __po_hi_lua_context_t
      RE_Asn1_Pkt_T,                     --  __po_hi_asn1_pkt_t
      RE_Asn1_Buffer_T,                  --  __po_hi_asn1_buffer_t
      RE_Time_T,                         --  __po_hi_time_t
      RE_Byte_T,                         --  __po_hi_byte_t
      RE_Uint64_T,                       --  __po_hi_uint64_t
      RE_Uint32_T,                       --  __po_hi_uint32_t
      RE_Uint16_T,                       --  __po_hi_uint16_t
      RE_Uint8_T,                        --  __po_hi_uint8_t
      RE_Int8_T,                         --  __po_hi_int8_t
      RE_Int16_T,                        --  __po_hi_int16_t
      RE_Int32_T,                        --  __po_hi_int32_t
      RE_Int64_T,                        --  __po_hi_int64_t
      RE_Float32_T,                      --  __po_hi_float32_t
      RE_Float64_T,                      --  __po_hi_float64_t
      RE_Msg_T,                          --  __po_hi_msg_t
      RE_Port_T,                         --  __po_hi_port_t
      RE_Protocol_Conf_T,                --  __po_hi_protocol_conf_t
      RE_Protocol_T,                     --  __po_hi_protocol_t
      RE_Local_Port_T,                   --  __po_hi_local_port_t
      RE_Node_T,                         --  __po_hi_node_t

      RE_Operation_Names,                --  __po_hi_operation_names
      RE_Ports_Names,                    --  __po_hi_port_names
      RE_Devices_Naming,                 --  __po_hi_devices_naming
      RE_Protocols_Configuration,        --  __po_hi_protocols_configuration
      RE_Devices_Configuration_Values,   --  __po_hi_devices_configuration_...
      RE_Devices_Nb_Accessed_Buses,      --  __po_hi_devices_nb_accessed_buses
      RE_Devices_Accessed_Buses,         --  __po_hi_devices_accessed_buses
      RE_Protected_Configuration,        --  __po_hi_protected_configuration
      RE_Protected_Priorities,           --  __po_hi_protected_priorities
      RE_Port_To_Device,                 --  __po_hi_port_to_device
      RE_Devices_To_Nodes,               --  __po_hi_devices_to_nodes
      RE_Port_Global_To_Entity,          --  __po_hi_port_global_to_entity
      RE_Port_Global_To_Device,          --  __po_hi_port_global_to_device
      RE_Port_Global_Names,              --  __po_hi_port_global_names
      RE_Port_Global_Kind,               --  __po_hi_port_global_kind
      RE_Port_Global_Queue_Size,         --  __po_hi_port_global_queue_size
      RE_Ports_Protocols,                --  __po_hi_ports_protocols
      RE_Port_Global_Data_Size,          --  __po_hi_port_global_data_size
      RE_Mynode,                         --  __po_hi_mynode
      RE_Node_Port,                      --  __po_hi_node_port
      RE_Node_Addr,                      --  __po_hi_node_addr
      RE_Entity_Table,                   --  __po_hi_entity_table
      RE_Port_Global_Model_Names,        --  __po_hi_port_global_model_names
      RE_Port_Global_To_Local            --  __po_hi_port_global_to_local
      );

   --  Runtime types

   subtype RT_Id is RE_Id range RE_Task_Id .. RE_Node_T;
   subtype RC_Id is RE_Id range RE_Default_Priority .. RE_SUCCESS;
   subtype RF_Id is RE_Id range RE_Null .. RE_Wait_For_Tasks;
   subtype RV_Id is RE_Id range RE_Operation_Names .. RE_Port_Global_To_Local;

   RE_Header_Table : constant array (RE_Id) of RH_Id :=
     (RE_Null => RH_Null,
   --  Runtime functions associations
      RE_Microseconds                   => RH_PO_HI_Time,
      RE_Milliseconds                   => RH_PO_HI_Time,
      RE_Seconds                        => RH_PO_HI_Time,
      RE_Minutes                        => RH_PO_HI_Time,
      RE_Transport_Send_Default         => RH_PO_HI_Transport,
      RE_Unused_Node                    => RH_PO_HI_Types,
      RE_Msg_Reallocate                 => RH_PO_HI_Messages,
      RE_Marshall_Array                 => RH_PO_HI_Marshallers,
      RE_Unmarshall_Array               => RH_PO_HI_Marshallers,
      RE_Marshall_Bool                  => RH_PO_HI_Marshallers,
      RE_Unmarshall_Bool                => RH_PO_HI_Marshallers,
      RE_Marshall_Char                  => RH_PO_HI_Marshallers,
      RE_Unmarshall_Char                => RH_PO_HI_Marshallers,
      RE_Marshall_Int                   => RH_PO_HI_Marshallers,
      RE_Unmarshall_Int                 => RH_PO_HI_Marshallers,
      RE_Marshall_Int8                  => RH_PO_HI_Marshallers,
      RE_Unmarshall_Int8                => RH_PO_HI_Marshallers,
      RE_Marshall_Int16                 => RH_PO_HI_Marshallers,
      RE_Unmarshall_Int16               => RH_PO_HI_Marshallers,
      RE_Marshall_Int32                 => RH_PO_HI_Marshallers,
      RE_Unmarshall_Int32               => RH_PO_HI_Marshallers,
      RE_Marshall_Int64                 => RH_PO_HI_Marshallers,
      RE_Unmarshall_Int64               => RH_PO_HI_Marshallers,
      RE_Marshall_Uint8                 => RH_PO_HI_Marshallers,
      RE_Unmarshall_Uint8               => RH_PO_HI_Marshallers,
      RE_Marshall_Uint16                => RH_PO_HI_Marshallers,
      RE_Unmarshall_Uint16              => RH_PO_HI_Marshallers,
      RE_Marshall_Uint32                => RH_PO_HI_Marshallers,
      RE_Unmarshall_Uint32              => RH_PO_HI_Marshallers,
      RE_Marshall_Uint64                => RH_PO_HI_Marshallers,
      RE_Unmarshall_Uint64              => RH_PO_HI_Marshallers,
      RE_Marshall_Float                 => RH_PO_HI_Marshallers,
      RE_Unmarshall_Float               => RH_PO_HI_Marshallers,
      RE_Marshall_Float32               => RH_PO_HI_Marshallers,
      RE_Unmarshall_Float32             => RH_PO_HI_Marshallers,
      RE_Marshall_Float64               => RH_PO_HI_Marshallers,
      RE_Unmarshall_Float64             => RH_PO_HI_Marshallers,
      RE_Marshall_Port                  => RH_PO_HI_Marshallers,
      RE_Unmarshall_Port                => RH_PO_HI_Marshallers,
      RE_LUA_Load                       => RH_PO_HI_Lua,
      RE_LUA_Load_File                  => RH_PO_HI_Lua,
      RE_LUA_Load_File_With_Function    => RH_PO_HI_Lua,
      RE_LUA_Init_Function_Call         => RH_PO_HI_Lua,
      RE_LUA_Perform_Function_Call      => RH_PO_HI_Lua,
      RE_LUA_Push_Boolean               => RH_PO_HI_Lua,
      RE_LUA_Push_Number                => RH_PO_HI_Lua,
      RE_LUA_Push_String                => RH_PO_HI_Lua,
      RE_LUA_Get_Boolean                => RH_PO_HI_Lua,
      RE_LUA_Get_Number                 => RH_PO_HI_Lua,
      RE_LUA_Get_String                 => RH_PO_HI_Lua,
      RE_Marshall_Request               => RH_Marshallers,
      RE_Unmarshall_Request             => RH_Marshallers,
      RE_Marshall_Asn1_Request          => RH_Marshallers,
      RE_Unmarshall_Asn1_Request        => RH_Marshallers,
      RE_Create_Periodic_Task           => RH_PO_HI_Task,
      RE_Main_Deliver                   => RH_Activity,
      RE_Create_Sporadic_Task           => RH_PO_HI_Task,
      RE_Compute_Miss                   => RH_PO_HI_Utils,
      RE_Deployment_Endiannesses        => RH_PO_HI_Transport,
      RE_Task_Wait_Offset               => RH_PO_HI_Task,
      RE_Wait_For_Next_Period           => RH_PO_HI_Task,
      RE_Compute_Next_Period            => RH_PO_HI_Task,
      RE_Initialize                     => RH_PO_HI_Main,
      RE_Initialize_Early               => RH_PO_HI_Main,
      RE_Wait_Initialization            => RH_PO_HI_Main,
      RE_Protected_Lock                 => RH_PO_HI_Protected,
      RE_Protected_Unlock               => RH_PO_HI_Protected,
      RE_Copy_Array                     => RH_PO_HI_Types,
      RE_Send_Output                    => RH_Activity,
      RE_Get_Entity_From_Global_Port    => RH_PO_HI_Transport,
      RE_Gqueue_Store_In                => RH_PO_HI_Gqueue,
      RE_Gqueue_Store_Out               => RH_PO_HI_Gqueue,
      RE_Gqueue_Send_Output             => RH_PO_HI_Gqueue,
      RE_Gqueue_Init                    => RH_PO_HI_Gqueue,
      RE_Gqueue_Get_Count               => RH_PO_HI_Gqueue,
      RE_Gqueue_Get_Value               => RH_PO_HI_Gqueue,
      RE_Gqueue_Next_Value              => RH_PO_HI_Gqueue,
      RE_Gqueue_Wait_For_Incoming_Event => RH_PO_HI_Gqueue,
      RE_Sporadic_Wait                  => RH_PO_HI_Task,
      RE_Sporadic_Wait_Release          => RH_PO_HI_Task,
      RE_Simulink_Find_Var              => RH_PO_HI_Simulink,
      RE_Simulink_Find_Signal           => RH_PO_HI_Simulink,
      RE_Simulink_Find_Parameter        => RH_PO_HI_Simulink,
      RE_Simulink_Update                => RH_PO_HI_Simulink,
      RE_Wait_End_Of_Instrumentation    => RH_PO_HI_Main,
      RE_Wait_For_Tasks                 => RH_PO_HI_Task,

   --  Runtime types associations

      RE_Msg_T                => RH_PO_HI_Messages,
      RE_Task_Id              => RH_Deployment,
      RE_Device_Id            => RH_Deployment,
      RE_Bus_Id               => RH_Deployment,
      RE_Entity_T             => RH_Deployment,
      RE_Local_Port_T         => RH_Deployment,
      RE_Port_T               => RH_Deployment,
      RE_Protocol_Conf_T      => RH_PO_HI_Transport,
      RE_Protocol_T           => RH_Deployment,
      RE_Entity_Server_T      => RH_Deployment,
      RE_Protected_T          => RH_PO_HI_Protected,
      RE_Inetport_T           => RH_PO_HI_Transport,
      RE_Inetaddr_T           => RH_PO_HI_Transport,
      RE_Time_T               => RH_PO_HI_Time,
      RE_Byte_T               => RH_PO_HI_Types,
      RE_Lua_Context_T        => RH_PO_HI_Lua,
      RE_Asn1_Pkt_T           => RH_PO_HI_Marshallers,
      RE_Asn1_Buffer_T        => RH_PO_HI_Marshallers,
      RE_Bool_T               => RH_PO_HI_Types,
      RE_Port_Kind_T          => RH_PO_HI_Types,
      RE_Uint8_T              => RH_PO_HI_Types,
      RE_Int8_T               => RH_PO_HI_Types,
      RE_Uint16_T             => RH_PO_HI_Types,
      RE_Int16_T              => RH_PO_HI_Types,
      RE_Uint32_T             => RH_PO_HI_Types,
      RE_Int32_T              => RH_PO_HI_Types,
      RE_Uint64_T             => RH_PO_HI_Types,
      RE_Int64_T              => RH_PO_HI_Types,
      RE_Float32_T            => RH_PO_HI_Types,
      RE_Float64_T            => RH_PO_HI_Types,
      RE_Node_T               => RH_Deployment,
      RE_Operation_T          => RH_Request,
      RE_Protected_Protocol_T => RH_PO_HI_Protected,
      RE_Request_T            => RH_Request,

   --  Runtime Constants associations

      RE_Default_Priority             => RH_PO_HI_Task,
      RE_Marshallers_Asn1_Copy_Member => RH_PO_HI_Marshallers,
      RE_Main_Type                    => RH_PO_HI_Task,
      RE_Main_Args                    => RH_PO_HI_Task,
      RE_Main_Name                    => RH_PO_HI_Task,
      RE_Main_Return                  => RH_PO_HI_Task,
      RE_Nb_Nodes                     => RH_Deployment,
      RE_Nb_Devices                   => RH_Deployment,
      RE_Nb_Buses                     => RH_Deployment,
      RE_Nb_Protocols                 => RH_Deployment,
      RE_Port_Type_Content            => RH_Deployment,
      RE_My_Node                      => RH_Deployment,
      RE_Nb_Ports                     => RH_Deployment,
      RE_Nb_Servers                   => RH_Deployment,
      RE_Nb_Protected                 => RH_Deployment,
      RE_Nb_Entities                  => RH_Deployment,
      RE_Simulink_Init                => RH_PO_HI_Simulink,
      RE_Simulink_Node                => RH_Null,
      RE_Simulink_Init_Func           => RH_Null,
      RE_Simulink_Model_Type          => RH_Null,
      RE_Gqueue_Fifo_Indata           => RH_PO_HI_Gqueue,
      RE_Gqueue_Fifo_Out              => RH_PO_HI_Gqueue,
      RE_Nb_Operations                => RH_Request,
      RE_Protected_Configuration      => RH_PO_HI_Protected,
      RE_Protected_Priorities         => RH_PO_HI_Protected,
      RE_Protected_Regular            => RH_PO_HI_Protected,
      RE_Protected_PIP                => RH_PO_HI_Protected,
      RE_Protected_PCP                => RH_PO_HI_Protected,
      RE_Protected_IPCP               => RH_PO_HI_Protected,
      RE_Noaddr                       => RH_PO_HI_Transport,
      RE_No_Device                    => RH_Deployment,
      RE_Noport                       => RH_PO_HI_Transport,
      RE_Bigendian                    => RH_PO_HI_Transport,
      RE_Littleendian                 => RH_PO_HI_Transport,
      RE_Nb_Tasks                     => RH_Deployment,
      RE_In_Data_Inter_Process        => RH_PO_HI_Types,
      RE_In_Data_Intra_Process        => RH_PO_HI_Types,
      RE_Out_Data_Inter_Process       => RH_PO_HI_Types,
      RE_Out_Data_Intra_Process       => RH_PO_HI_Types,
      RE_In_Event_Inter_Process       => RH_PO_HI_Types,
      RE_In_Event_Intra_Process       => RH_PO_HI_Types,
      RE_Out_Event_Inter_Process      => RH_PO_HI_Types,
      RE_Out_Event_Intra_Process      => RH_PO_HI_Types,
      RE_In_Event_Data_Inter_Process  => RH_PO_HI_Types,
      RE_In_Event_Data_Intra_Process  => RH_PO_HI_Types,
      RE_Out_Event_Data_Inter_Process => RH_PO_HI_Types,
      RE_Out_Event_Data_Intra_Process => RH_PO_HI_Types,
      RE_Need_Driver_Sockets          => RH_Deployment,
      RE_SUCCESS                      => RH_PO_HI_Returns,

   --  Variables associations

      RE_Ports_Names                  => RH_PO_HI_Types,
      RE_Devices_Naming               => RH_Deployment,
      RE_Protocols_Configuration      => RH_Deployment,
      RE_Devices_Configuration_Values => RH_Deployment,
      RE_Devices_Nb_Accessed_Buses    => RH_Deployment,
      RE_Devices_Accessed_Buses       => RH_Deployment,
      RE_Port_To_Device               => RH_Deployment,
      RE_Devices_To_Nodes             => RH_Deployment,
      RE_Operation_Names              => RH_PO_HI_Types,
      RE_Port_Global_To_Entity        => RH_Deployment,
      RE_Port_Global_To_Device        => RH_Deployment,
      RE_Port_Global_Names            => RH_Deployment,
      RE_Port_Global_Queue_Size       => RH_Deployment,
      RE_Ports_Protocols              => RH_Deployment,
      RE_Port_Global_Data_Size        => RH_Deployment,
      RE_Port_Global_Kind             => RH_Deployment,
      RE_Mynode                       => RH_Deployment,
      RE_Node_Port                    => RH_Deployment,
      RE_Node_Addr                    => RH_Deployment,
      RE_Entity_Table                 => RH_Deployment,
      RE_Port_Global_Model_Names      => RH_Deployment,
      RE_Port_Global_To_Local         => RH_Deployment);
   procedure Initialize;
   procedure Reset;

   function RE (Id : RE_Id) return Node_Id;
   --  Return a designator for entity Id

   function RH (Id : RH_Id) return Node_Id;
   --  Return a node for entity RH_Id

end Ocarina.Backends.PO_HI_C.Runtime;
