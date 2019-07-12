------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B A C K E N D S . P O _ H I _ A D A . R U N T I M E    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2019 ESA & ISAE.      --
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

package Ocarina.Backends.PO_HI_Ada.Runtime is

   --  Runtime Units

   type RU_Id is
     (RU_Null,
      RU_Ada,
      RU_Ada_Exceptions,
      RU_Ada_Interrupts,
      RU_Ada_Interrupts_Names,
      RU_Ada_Real_Time,
      RU_Ada_Strings,
      RU_Ada_Strings_Bounded,
      RU_Ada_Strings_Bounded_Generic_Bounded_Length,
      RU_Ada_Strings_Wide_Bounded,
      RU_Ada_Strings_Wide_Bounded_Generic_Bounded_Length,
      RU_Ada_Synchronous_Task_Control,
      RU_ASN1_Types,
      RU_PolyORB_HI_Generated,
      RU_PolyORB_HI_Generated_Activity,
      RU_PolyORB_HI_Generated_Deployment,
      RU_PolyORB_HI_Generated_Job,
      RU_PolyORB_HI_Generated_Marshallers,
      RU_PolyORB_HI_Generated_Naming,
      RU_PolyORB_HI_Generated_Subprograms,
      RU_PolyORB_HI_Generated_Transport,
      RU_PolyORB_HI_Generated_Types,
      RU_PolyORB_HI,
      RU_PolyORB_HI_Errors,
      RU_PolyORB_HI_Hybrid_Task_Driver,
      RU_PolyORB_HI_Hybrid_Task_Driver_Driver,
      RU_PolyORB_HI_Aperiodic_Task,
      RU_PolyORB_HI_Background_Task,
      RU_PolyORB_HI_Null_Task,
      RU_PolyORB_HI_Periodic_Task,
      RU_PolyORB_HI_Sporadic_Task,
      RU_PolyORB_HI_Hybrid_Task,
      RU_PolyORB_HI_ISR_Task,
      RU_PolyORB_HI_Port_Kinds,
      RU_PolyORB_HI_Marshallers_G,
      RU_PolyORB_HI_Port_Type_Marshallers,
      RU_PolyORB_HI_Time_Marshallers,
      RU_PolyORB_HI_Messages,
      RU_PolyORB_HI_Output_Low_Level,
      RU_PolyORB_HI_Output,
      RU_PolyORB_HI_Port_Types,
      RU_PolyORB_HI_Protocols,
      RU_PolyORB_HI_Streams,
      RU_PolyORB_HI_Suspenders,
      RU_PolyORB_HI_Thread_Interrogators,
      RU_PolyORB_HI_Transport_Low_Level,
      RU_PolyORB_HI_Utils,
      RU_Interfaces,
      RU_Interfaces_C,
      RU_Standard,
      RU_System);

   --  Runtime Entities

   type RE_Id is
     (RE_Null,                       --  Workaround to denote a null RE
      RE_Off,                        --  Off
      RE_On,                         --  On
      RE_True,                       --  True
      RE_False,                      --  False
      RE_Get_Count,                  --  PO_HI_GEN.Activity.Get_Count
      RE_Get_Sender,                 --  PO_HI_GEN.Activity.Get_Sender
      RE_Get_Value,                  --  PO_HI_GEN.Activity.Get_Value
      RE_Next_Value,                 --  PO_HI_GEN.Activity.Next_Value
      RE_Put_Value,                  --  PO_HI_GEN.Activity.Put_Value
      RE_Receive_Input,              --  PO_HI_GEN.Activity.Receive_Input
      RE_Store_Incoming_Message,     --  PO_HI_GEN.Activity.Store_Incom...
      RE_Wait_For_Incoming_Events,   --  PO_HI_GEN.Activity.Wait_For_In...
      RE_Node_Type,                  --  PO_HI_GEN.Deployment.Node_Type
      RE_Entity_Type,                --  PO_HI_GEN.Deployment.Entity_Type
      RE_Entity_Table,               --  PO_HI_GEN.Deployment.Entity_Table
      RE_Port_Type_1,                --  PO_HI_GEN.Deployment.Port_Type
      RE_Port_Image,                 --  PO_HI_GEN.Deployment.Port_Image
      RE_Max_Port_Image_Size,        --  PO_HI_GEN.Deployment.Max_Port_Image...
      RE_Deliver,                    --  PO_HI_GEN.Transport.Deliver
      RE_Send_2,                     --  PO_HI_GEN.Transport.Send
      RE_Clock,                      --  Ada.Real_Time.Clock
      RE_Nanoseconds,                --  Ada.Real_Time.Nanoseconds
      RE_Microseconds,               --  Ada.Real_Time.Microseconds
      RE_Milliseconds,               --  Ada.Real_Time.Milliseconds
      RE_Seconds,                    --  Ada.Real_Time.Seconds
      RE_Minutes,                    --  Ada.Real_Time.Minutes
      RE_Time,                       --  Ada.Real_Time.Time
      RE_Time_Span,                  --  Ada.Real_Time.Time_Span
      RE_Set_True,                   --  Ada.Synchronous_Task_Control.Set_True
      RE_Integer_8,                  --  Interfaces.Integer_8
      RE_Integer_16,                 --  Interfaces.Integer_16
      RE_Integer_32,                 --  Interfaces.Integer_32
      RE_Integer_64,                 --  Interfaces.Integer_64
      RE_Unsigned_8,                 --  Interfaces.Unsigned_8
      RE_Unsigned_16,                --  Interfaces.Unsigned_16
      RE_Unsigned_32,                --  Interfaces.Unsigned_32
      RE_Unsigned_64,                --  Interfaces.Unsigned_64
      RE_Char_Array,                 --  Interfaces.C.char_array
      RE_Nul,                        --  Interfaces.C.nul
      RE_Error_Kind,                 --  Po..HI.Error_Kind
      RE_Error_None,                 --  Po..HI.Error_Kind
      RE_Error_Transport,            --  Po..HI.Error_Kind
      RE_Marshall_1,                 --  Po..HI.Port_Type_Marshallers.Marshall
      RE_Unmarshall_1,               --  Po..HI.Port_.._Marshallers.Unmarshall
      RE_Marshall_2,                 --  Po..HI.Time_Marshallers.Marshall
      RE_Unmarshall_2,               --  Po..HI.Time_Marshallers.Unmarshall
      RE_Port_Kind,                  --  Po..HI.Port_Kinds.Port_Kind

   --  IMPORTANT: The order of the 9 enumerators below and the fact
   --  they are consecutive must NOT be modified unless you know
   --  exactly what you are doing!

      RE_In_Event_Port,              --  Po..HI.Port_Kinds.In_Event_Port
      RE_In_Event_Data_Port,         --  Po..HI.Port_Kinds.In_Event_Data_Port
      RE_In_Data_Port,               --  Po..HI.Port_Kinds.In_Data_Port
      RE_In_Out_Event_Port,          --  Po..HI.Port_Kinds.In_Out_Event_Port
      RE_In_Out_Event_Data_Port,     --  Po..HI.Po..nds.In_Out_Event_Data_Port
      RE_In_Out_Data_Port,           --  Po..HI.Port_Kinds.In_Out_Data_Port
      RE_Out_Event_Port,             --  Po..HI.Port_Kinds.Out_Event_Port
      RE_Out_Event_Data_Port,        --  Po..HI.Port_Kinds.Out_Event_Data_Port
      RE_Out_Data_Port,              --  PolyORB_HI.Port_Kinds.Out_Data_Port
      RE_Overflow_Handling_Protocol, --  Po..HI.Port_Kinds.Over..ing_Protocol
      RE_DropOldest,                 --  Po..HI.Port_Kinds.DropOldest
      RE_DropNewest,                 --  Po..HI.Port_Kinds.DropNewest
      RE_Error,                      --  Po..HI.Port_Kinds.Error
      RE_Stream_Element_Array,       --  Po..HI.Streams.Stream_Element_Array
      RE_Stream_Element_Offset,      --  Po..HI.Stream_Element_Offset
      RE_Write,                      --  PolyORB_HI.Messages.Write
      RE_Header_Size,                --  PolyORB_HI.Messages.Header_Size
      RE_Message_Type,               --  PolyORB_HI.Messages.Message_Type
      RE_Reallocate,                 --  PolyORB_HI.Messages.Reallocate
      RE_Sender,                     --  PolyORB_HI.Messages.Sender
      RE_Encapsulate,                --  PolyORB_HI.Messages.Encapsulate
      RE_Not_Empty,                  --  PolyORB_HI.Messages.Not_Empty
      RE_Valid,                      --  PolyORB_HI.Messages.Valid
      RE_Size,                       --  PolyORB_HI.Messages.Size
      RE_Naming_Entry,               --  PolyORB_HI.Transport.Naming_Entry
      RE_Get_Task_Id,                --  PolyORB_HI.Utils.Get_Task_Id
      RE_To_HI_String,               --  PolyORB_HI.Utils.To_HI_String
      RE_Naming_Table_Type,          --  PolyORB_HI.Utils.Naming_Table_Type
      RE_Corresponding_Port,         --  Po..HI.Port_Types.Corresponding_Port
      RE_Non_Blocking_Receive,       --  Po..HI.Protocols.Non_Blocking_Receive
      RE_Send_1,                     --  Po..HI.Protocols.Send
      RE_Initialize,                 --  Po..HI.Transport_Low_Level.Initialize
      RE_Send_3,                     --  Po..HI.Transport_Low_Level.Send
      RE_Hybrid_Task_Info,           --  Po..HI.Hybrid_...pes.Hybrid_Task_Info
      RE_Hybrid_Task_Info_Array,     --  PO..HI.Hybrid..es.Hybrid_.._Info_Array
      RE_Driver_Suspender,           --  PO..HI.Hybrid..es.Driver_Suspender
      RE_Boolean,                    --  Standard.Boolean
      RE_Integer,                    --  Standard.Integer
      RE_Natural,                    --  Standard.Natural
      RE_Positive,                   --  Standard.Positive
      RE_Float,                      --  Standard.Float
      RE_Long_Float,                 --  Standard.Long_Float
      RE_String,                     --  Standard.String
      RE_Character,                  --  Standard.Character
      RE_Wide_Character,             --  Standard.Wide_Character
      RE_Suspend_Forever,            --  Suspenders.Suspend_Forever
      RE_Unblock_All_Tasks,          --  Suspenders.Unblock_All_Tasks
      RE_System_Startup_Time,        --  Suspenders.System_Startup_Time
      RE_Address,                    --  System.Address
      RE_Null_Address,               --  System.Null_Address
      RE_Default_Priority,           --  System.Default_Priority
      RE_Priority);                  --  System.Priority

   RE_Unit_Table : constant array (RE_Id) of RU_Id :=
     (RE_Null                       => RU_Null,
      RE_Off                        => RU_Null,
      RE_On                         => RU_Null,
      RE_True                       => RU_Null,
      RE_False                      => RU_Null,
      RE_Get_Count                  => RU_PolyORB_HI_Generated_Activity,
      RE_Get_Sender                 => RU_PolyORB_HI_Generated_Activity,
      RE_Get_Value                  => RU_PolyORB_HI_Generated_Activity,
      RE_Next_Value                 => RU_PolyORB_HI_Generated_Activity,
      RE_Put_Value                  => RU_PolyORB_HI_Generated_Activity,
      RE_Receive_Input              => RU_PolyORB_HI_Generated_Activity,
      RE_Store_Incoming_Message     => RU_PolyORB_HI_Generated_Activity,
      RE_Wait_For_Incoming_Events   => RU_PolyORB_HI_Generated_Activity,
      RE_Node_Type                  => RU_PolyORB_HI_Generated_Deployment,
      RE_Entity_Type                => RU_PolyORB_HI_Generated_Deployment,
      RE_Entity_Table               => RU_PolyORB_HI_Generated_Deployment,
      RE_Port_Type_1                => RU_PolyORB_HI_Generated_Deployment,
      RE_Port_Image                 => RU_PolyORB_HI_Generated_Deployment,
      RE_Max_Port_Image_Size        => RU_PolyORB_HI_Generated_Deployment,
      RE_Deliver                    => RU_PolyORB_HI_Generated_Transport,
      RE_Send_2                     => RU_PolyORB_HI_Generated_Transport,
      RE_Clock                      => RU_Ada_Real_Time,
      RE_Nanoseconds                => RU_Ada_Real_Time,
      RE_Microseconds               => RU_Ada_Real_Time,
      RE_Milliseconds               => RU_Ada_Real_Time,
      RE_Seconds                    => RU_Ada_Real_Time,
      RE_Minutes                    => RU_Ada_Real_Time,
      RE_Time                       => RU_Ada_Real_Time,
      RE_Time_Span                  => RU_Ada_Real_Time,
      RE_Set_True                   => RU_Ada_Synchronous_Task_Control,
      RE_Integer_8                  => RU_Interfaces,
      RE_Integer_16                 => RU_Interfaces,
      RE_Integer_32                 => RU_Interfaces,
      RE_Integer_64                 => RU_Interfaces,
      RE_Unsigned_8                 => RU_Interfaces,
      RE_Unsigned_16                => RU_Interfaces,
      RE_Unsigned_32                => RU_Interfaces,
      RE_Unsigned_64                => RU_Interfaces,
      RE_Char_Array                 => RU_Interfaces_C,
      RE_Nul                        => RU_Interfaces_C,
      RE_Error_Kind                 => RU_PolyORB_HI_Errors,
      RE_Error_None                 => RU_PolyORB_HI_Errors,
      RE_Error_Transport            => RU_PolyORB_HI_Errors,
      RE_Marshall_1                 => RU_PolyORB_HI_Port_Type_Marshallers,
      RE_Unmarshall_1               => RU_PolyORB_HI_Port_Type_Marshallers,
      RE_Marshall_2                 => RU_PolyORB_HI_Time_Marshallers,
      RE_Unmarshall_2               => RU_PolyORB_HI_Time_Marshallers,
      RE_Port_Kind                  => RU_PolyORB_HI_Port_Kinds,
      RE_In_Event_Port              => RU_PolyORB_HI_Port_Kinds,
      RE_In_Event_Data_Port         => RU_PolyORB_HI_Port_Kinds,
      RE_In_Data_Port               => RU_PolyORB_HI_Port_Kinds,
      RE_In_Out_Event_Port          => RU_PolyORB_HI_Port_Kinds,
      RE_In_Out_Event_Data_Port     => RU_PolyORB_HI_Port_Kinds,
      RE_In_Out_Data_Port           => RU_PolyORB_HI_Port_Kinds,
      RE_Out_Event_Port             => RU_PolyORB_HI_Port_Kinds,
      RE_Out_Event_Data_Port        => RU_PolyORB_HI_Port_Kinds,
      RE_Out_Data_Port              => RU_PolyORB_HI_Port_Kinds,
      RE_Overflow_Handling_Protocol => RU_PolyORB_HI_Port_Kinds,
      RE_DropOldest                 => RU_PolyORB_HI_Port_Kinds,
      RE_DropNewest                 => RU_PolyORB_HI_Port_Kinds,
      RE_Error                      => RU_PolyORB_HI_Port_Kinds,
      RE_Write                      => RU_PolyORB_HI_Messages,
      RE_Header_Size                => RU_PolyORB_HI_Messages,
      RE_Message_Type               => RU_PolyORB_HI_Messages,
      RE_Reallocate                 => RU_PolyORB_HI_Messages,
      RE_Sender                     => RU_PolyORB_HI_Messages,
      RE_Encapsulate                => RU_PolyORB_HI_Messages,
      RE_Not_Empty                  => RU_PolyORB_HI_Messages,
      RE_Valid                      => RU_PolyORB_HI_Messages,
      RE_Size                       => RU_PolyORB_HI_Messages,
      RE_Naming_Entry               => RU_PolyORB_HI_Utils,
      Re_Get_Task_Id                => RU_PolyORB_HI_Utils,
      RE_To_HI_String               => RU_PolyORB_HI_Utils,
      RE_Naming_Table_Type          => RU_PolyORB_HI_Utils,
      RE_Corresponding_Port         => RU_PolyORB_HI_Port_Types,
      RE_Non_Blocking_Receive       => RU_PolyORB_HI_Protocols,
      RE_Send_1                     => RU_PolyORB_HI_Protocols,
      RE_Stream_Element_Array       => RU_PolyORB_HI_Streams,
      RE_Stream_Element_Offset      => RU_PolyORB_HI_Streams,
      RE_Initialize                 => RU_PolyORB_HI_Transport_Low_Level,
      RE_Send_3                     => RU_PolyORB_HI_Transport_Low_Level,
      RE_Hybrid_Task_Info           => RU_PolyORB_HI_Hybrid_Task_Driver,
      RE_Hybrid_Task_Info_Array     => RU_PolyORB_HI_Hybrid_Task_Driver,
      RE_Driver_Suspender           => RU_PolyORB_HI_Hybrid_Task_Driver,
      RE_Boolean                    => RU_Standard,
      RE_Integer                    => RU_Standard,
      RE_Natural                    => RU_Standard,
      RE_Positive                   => RU_Standard,
      RE_Float                      => RU_Standard,
      RE_Long_Float                 => RU_Standard,
      RE_String                     => RU_Standard,
      RE_Character                  => RU_Standard,
      RE_Wide_Character             => RU_Standard,
      RE_Suspend_Forever            => RU_PolyORB_HI_Suspenders,
      RE_Unblock_All_Tasks          => RU_PolyORB_HI_Suspenders,
      RE_System_Startup_Time        => RU_PolyORB_HI_Suspenders,
      RE_Address                    => RU_System,
      RE_Null_Address               => RU_System,
      RE_Default_Priority           => RU_System,
      RE_Priority                   => RU_System);

   procedure Initialize;
   procedure Reset;

   function RE (Id : RE_Id; Withed : Boolean := True) return Node_Id;
   --  Return a designator for entity Id

   function RU
     (Id         : RU_Id;
      Withed     : Boolean := True;
      Elaborated : Boolean := False) return Node_Id;
   --  Return a node for Unit Id

end Ocarina.Backends.PO_HI_Ada.Runtime;
