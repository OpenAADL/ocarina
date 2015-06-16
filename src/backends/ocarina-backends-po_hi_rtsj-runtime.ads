------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ H I _ R T S J . R U N T I M E   --
--                                                                          --
--                                 S p e c                                  --
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

package Ocarina.Backends.PO_HI_RTSJ.Runtime is

   --  Headers file that can be included
   type RH_Id is
     (RH_Null,
   --
      RH_Suspenders,                     --  Runtime imports
      RH_Utils,
      RH_Context,
      RH_Deployment,
      RH_Periodic_Task,
      RH_Sporadic_Task,
      RH_Hybrid_Task,
      RH_Program_Exception,
      RH_Task_Handler,
      RH_Event_Handler,
      RH_TimeUnit,
      RH_Generated_Type,
      RH_Message,
      RH_Marshallers,
      RH_Transport_High_Level,
      RH_Transport_Low_Level_Sockets,
      RH_In_Port,
      RH_Out_Port,
      RH_Ports_Router,
   --
      RH_Immortal_Memory,                --  javax.realtime imports
      RH_No_Heap_Realtime_Thread);

   --  Runtime Entities
   type RE_Id is
     (RE_Null,
   --
      RE_Job,
      RE_Initialize_Entrypoint,
      RE_Recover_Entrypoint,
      RE_Handle_Event,
      RE_Wait_For_Events,
      RE_Periodic_Task,
      RE_Sporadic_Task,
      RE_Hybrid_Task,
      RE_Suspend_Forever,
      RE_Unblock_All_Tasks,
      RE_Wait_Initialization,
      RE_Compute_System_Priority,
      RE_Store,
      RE_Set,
      RE_Copy,
      RE_Put_Int,
      RE_Get_Int,
      RE_Rewind,
      RE_Deliver,
      RE_Send,
      RE_Unmarshall_Port,
      RE_Set_Destination_Entity,
      RE_Set_Sender_Entity,
      RE_Get_Destination_Entity,
      RE_Wait_For_Incoming_Events,
   --
      RE_Entities_Offset,
      RE_Entities_Table,
      RE_Max_Payload_Size,
      RE_My_Node,
      RE_Nb_Local_Entities,
      RE_Nb_Nodes,
      RE_Nodes_Inet_Address,
      RE_Nodes_Inet_Port,
      RE_Ports_Table,
      RE_Transport,
      RE_Port_Number,
   --
      RE_Public,
      RE_Protected,
      RE_Private,
      RE_Abstract,
      RE_Static,
      RE_Final,
      RE_This,
      RE_Synchronized,
      RE_Break,
      RE_Default,
   --
      RE_Pico_Second,
      RE_Nano_Second,
      RE_Micro_Second,
      RE_Milli_Second,
      RE_Second,
      RE_Minute,
      RE_Hour,
   --
      RE_Clock,
      RE_No_Inet_Address,
      RE_No_Port,
      RE_Default_Priority,
      RE_Min_Priority,
      RE_Max_Priority,
      RE_Max_Priority_Parameters,
      RE_In_Event_Port,
      RE_In_Data_Port,
      RE_In_Event_Data_Port,
      RE_Out_Event_Port,
      RE_Out_Data_Port,
      RE_Out_Event_Data_Port);

   --  Constants
   subtype RC_Id is RE_Id range RE_Clock .. RE_Out_Event_Data_Port;

   --  Variables and methods
   subtype RV_Id is RE_Id range RE_Job .. RE_Default;

   --  TimeUnit
   subtype RT_Id is RE_Id range RE_Pico_Second .. RE_Hour;

   --  Imports
   subtype RM_Id is RH_Id range RH_Suspenders .. RH_Ports_Router;
   subtype RN_Id is
     RH_Id range RH_Immortal_Memory .. RH_No_Heap_Realtime_Thread;

   RE_Header_Table : constant array (RE_Id) of RH_Id :=
     (RE_Null => RH_Null,
   --  Runtime functions associations
      RE_Job                      => RH_Task_Handler,
      RE_Initialize_Entrypoint    => RH_Task_Handler,
      RE_Recover_Entrypoint       => RH_Task_Handler,
      RE_Handle_Event             => RH_Event_Handler,
      RE_Wait_For_Events          => RH_Event_Handler,
      RE_Periodic_Task            => RH_Periodic_Task,
      RE_Sporadic_Task            => RH_Sporadic_Task,
      RE_Hybrid_Task              => RH_Hybrid_Task,
      RE_Suspend_Forever          => RH_Suspenders,
      RE_Unblock_All_Tasks        => RH_Suspenders,
      RE_Wait_Initialization      => RH_Suspenders,
      RE_Compute_System_Priority  => RH_Utils,
      RE_Store                    => RH_Generated_Type,
      RE_Set                      => RH_Generated_Type,
      RE_Copy                     => RH_Generated_Type,
      RE_Put_Int                  => RH_Message,
      RE_Get_Int                  => RH_Message,
      RE_Rewind                   => RH_Message,
      RE_Set_Destination_Entity   => RH_Message,
      RE_Set_Sender_Entity        => RH_Message,
      RE_Deliver                  => RH_Transport_High_Level,
      RE_Send                     => RH_Transport_High_Level,
      RE_Unmarshall_Port          => RH_Marshallers,
      RE_Get_Destination_Entity   => RH_Transport_Low_Level_Sockets,
      RE_Wait_For_Incoming_Events => RH_Ports_Router,

   --  Constants associations
      RE_Clock                   => RH_Utils,
      RE_No_Inet_Address         => RH_Utils,
      RE_No_Port                 => RH_Utils,
      RE_Default_Priority        => RH_Utils,
      RE_Min_Priority            => RH_Utils,
      RE_Max_Priority            => RH_Utils,
      RE_Max_Priority_Parameters => RH_Utils,
      RE_In_Event_Port           => RH_In_Port,
      RE_In_Data_Port            => RH_In_Port,
      RE_In_Event_Data_Port      => RH_In_Port,
      RE_Out_Event_Port          => RH_Out_Port,
      RE_Out_Data_Port           => RH_Out_Port,
      RE_Out_Event_Data_Port     => RH_Out_Port,

   --  Variables associations
      RE_Entities_Offset    => RH_Context,
      RE_Entities_Table     => RH_Context,
      RE_Max_Payload_Size   => RH_Context,
      RE_My_Node            => RH_Context,
      RE_Nb_Local_Entities  => RH_Context,
      RE_Nb_Nodes           => RH_Context,
      RE_Nodes_Inet_Address => RH_Context,
      RE_Nodes_Inet_Port    => RH_Context,
      RE_Ports_Table        => RH_Context,
      RE_Transport          => RH_Context,
      RE_Pico_Second        => RH_TimeUnit,
      RE_Nano_Second        => RH_TimeUnit,
      RE_Micro_Second       => RH_TimeUnit,
      RE_Milli_Second       => RH_TimeUnit,
      RE_Second             => RH_TimeUnit,
      RE_Minute             => RH_TimeUnit,
      RE_Hour               => RH_TimeUnit,
      RE_Port_Number        => RH_In_Port,
   --
      RE_Public .. RE_Default => RH_Null);

   procedure Initialize;
   procedure Reset;

   --  Return a designator for entity RE_Id
   function RE (Id : RE_Id) return Node_Id;

   --  Return a node for entity RH_Id
   function RH (Id : RH_Id) return Node_Id;

end Ocarina.Backends.PO_HI_RTSJ.Runtime;
