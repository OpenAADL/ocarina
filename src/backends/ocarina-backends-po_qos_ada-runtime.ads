------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A . R U N T I M E   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2006-2008, GET-Telecom Paris.                --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

package Ocarina.Backends.PO_QoS_Ada.Runtime is

   --  Runtime Units

   type RU_Id is
     (RU_Null,
      RU_ARAO,
      RU_ARAO_Obj_Adapters,
      RU_ARAO_Object_Adapter_G,
      RU_ARAO_Object_Adapter,
      RU_ARAO_RT_Obj_Adapters,
      RU_ARAO_Periodic_Threads,
      RU_ARAO_Requests,
      RU_ARAO_Utils,
      RU_ARAO_Cyclic_Array,
      RU_ARAO_Setup,
      RU_ARAO_Threads,
      RU_ARAO_Setup_Base,
      RU_ARAO_Setup_Ocarina_OA,
      RU_ARAO_Setup_Application,
      RU_ARAO_Setup_Tasking,
      RU_ARAO_Setup_Tasking_Full_Tasking,
      RU_ARAO_Setup_Tasking_No_Tasking,
      RU_ARAO_Setup_OA,
      RU_ARAO_Setup_OA_Monothreaded,
      RU_ARAO_Setup_OA_Multithreaded,
      RU_ARAO_Setup_OA_Multithreaded_Prio,
      RU_Ada,
      RU_Ada_Exceptions,
      RU_Ada_Real_Time,
      RU_Ada_Strings,
      RU_Ada_Strings_Bounded,
      RU_Ada_Strings_Bounded_Generic_Bounded_Length,
      RU_Ada_Strings_Wide_Bounded,
      RU_Ada_Strings_Wide_Bounded_Generic_Bounded_Length,
      RU_Helpers,
      RU_Servants,
      RU_PolyORB,
      RU_PolyORB_Any,
      RU_PolyORB_Any_Initialization,
      RU_PolyORB_Any_ExceptionList,
      RU_PolyORB_Any_NVList,
      RU_PolyORB_Any_NVList_Internals,
      RU_PolyORB_Any_NVList_Internals_NV_Lists,
      RU_PolyORB_Any_TypeCode,
      RU_PolyORB_Components,
      RU_PolyORB_Errors,
      RU_PolyORB_Initialization,
      RU_PolyORB_ORB_Controller,
      RU_PolyORB_ORB_Controller_Workers,
      RU_PolyORB_ORB,
      RU_PolyORB_References,
      RU_PolyORB_Requests,
      RU_PolyORB_Servants,
      RU_PolyORB_Servants_Iface,
      RU_PolyORB_Setup,
      RU_PolyORB_Setup_Base,
      RU_PolyORB_Setup_IIOP,
      RU_PolyORB_Setup_DIOP,
      RU_PolyORB_Setup_OA,
      RU_PolyORB_Setup_OA_Simple_OA,
      RU_PolyORB_Setup_OA_Basic_RT_POA,
      RU_PolyORB_Setup_Access_Points,
      RU_PolyORB_Setup_Access_Points_IIOP,
      RU_PolyORB_Setup_Access_Points_DIOP,
      RU_PolyORB_Setup_Access_Points_SOAP,
      RU_PolyORB_Setup_Tasking,
      RU_PolyORB_Setup_Tasking_Full_Tasking,
      RU_PolyORB_Setup_Tasking_No_Tasking,
      RU_PolyORB_Tasking,
      RU_PolyORB_Tasking_Mutexes,
      RU_PolyORB_Parameters,
      RU_PolyORB_Parameters_Partition,
      RU_PolyORB_Types,
      RU_PolyORB_Utils,
      RU_PolyORB_Utils_Strings,
      RU_PolyORB_Utils_Strings_Lists,
      RU_Standard,
      RU_System);

   --  Runtime Entities

   type RE_Id is
     (RE_Null,                      --  Workaround to denote a null RE
      RE_Ref_1,                     --  Ref
      RE_From_Any_1,                --  From_Any
      RE_To_Any_1,                  --  To_Any
      RE_False,                     --  False
      RE_True,                      --  True
      RE_On,                        --  On
      RE_Off,                       --  Off
      RE_Get,                       --  Get
      RE_Append,                    --  Append
      RE_Push_Back,                 --  Push_Back
      RE_Link_To_Obj_Adapter,       --  ARAO.Obj_Adapters.Link_To_Obj_Adapter
      RE_Link_To_Obj_Adapter_2,     --  ARAO.RT_Object_Adapters
                                    --       Link_To_Obj_Adapter
      RE_Create_Periodic_Thread,    --  ARAO.Periodic_Threads.
                                    --       Create_Periodic_Thread
      RE_Emit_Msg,                  --  ARAO.Requests.Emit_Msg
      RE_Get_Ref,                   --  ARAO.Utils.Get_Ref
      RE_Get_GIOP_Ref,              --  ARAO.Utils.Get_GIOP_Ref
      RE_Clock,                     --  Ada.Real_Time.Clock
      RE_Nanoseconds,               --  Ada.Real_Time.Nanoseconds
      RE_Microseconds,              --  Ada.Real_Time.Microseconds
      RE_Milliseconds,              --  Ada.Real_Time.Milliseconds
      RE_Seconds,                   --  Ada.Real_Time.Seconds
      RE_Minutes,                   --  Ada.Real_Time.Minutes
      RE_Time,                      --  Ada.Real_Time.Time
      RE_Time_Span,                 --  Ada.Real_Time.Time_Span
      RE_Add_Aggregate_Element,     --  PolyORB.Any.Add_Aggregate_Element,
      RE_Any,                       --  PolyORB.Any.Any
      RE_ARG_IN,                    --  PolyORB.Any.ARG_IN
      RE_ARG_INOUT,                 --  PolyORB.Any.ARG_INOUT
      RE_ARG_OUT,                   --  PolyORB.Any.ARG_OUT
      RE_NamedValue,                --  PolyORB.Any.NamedValue
      RE_From_Any_2,                --  PolyORB.Any.From_Any
      RE_Get_Aggregate_Element,     --  PolyORB.Any.Get_Aggregate_Element
      RE_Get_Empty_Any,             --  PolyORB.Any.Get_Empty_Any
      RE_Get_Empty_Any_Aggregate,   --  PolyORB.Any.Get_Empty_Any_Aggregate
      RE_Set_Type,                  --  PolyORB.Any.Set_Type
      RE_TC_Boolean,                --  PolyORB.Any.TC_Boolean
      RE_TC_Float,                  --  PolyORB.Any.TC_Float
      RE_TC_Long,                   --  PolyORB.Any.TC_Long
      RE_TC_Character,              --  PolyORB.Any.TC_Character
      RE_TC_Wide_Character,         --  PolyORB.Any.TC_Wide_Character
      RE_TC_String,                 --  PolyORB.Any.TC_String
      RE_TC_Wide_String,            --  PolyORB.Any.TC_Wide_String
      RE_To_Any_2,                  --  PolyORB.Any.To_Any
      RE_Add_Item,                  --  PolyORB.Any.NVList.Add_Item
      RE_Create,                    --  PolyORB.Any.NVList.Create
      RE_Ref_2,                     --  PolyORB.Any.NVList.Ref
      RE_Add_Parameter,             --  PolyORB.Any.TypeCode.Add_Parameter
      RE_Local_Ref,                 --  PolyORB.Any.TypeCode.Local_Ref
      RE_Object_Ptr,                --  PolyORB.Any.TypeCode.Object_Ptr
      RE_Object_Of,                 --  PolyORB.Any.TypeCode.Object_Of
      RE_To_Ref,                    --  PolyORB.Any.TypeCode.To_Ref
      RE_Disable_Reference_Counting, -- PolyORB.Any.TypeCode
      --                                  .Disable_Reference_Counting
      RE_TC_Alias,                  --  PolyORB.Any.TypeCode.TC_Alias
      RE_TC_Struct,                 --  PolyORB.Any.TypeCode.TC_Struct
      RE_Message,                   --  PolyORB.Components.Message
      RE_Error_Container,           --  PolyORB.Errors.Error_Container
      RE_Catch,                     --  PolyORB.Errors.Catch
      RE_Found,                     --  PolyORB.Errors.Found
      RE_Initialize_World,          --  PolyORB.Initialization.Initialize_World
      RE_Module_Info,               --  PolyORB.Initialization.Module_Info
      RE_Register_Module,           --  PolyORB.Initialization.Register_Module
      RE_Run,                       --  PolyORB.ORB.Run
      RE_Ref_3,                     --  PolyORB.References.Ref
      RE_Arguments,                 --  PolyORB.Requests.Arguments
      RE_Request_Access,            --  PolyORB.Requests.Request_Access
      RE_Servant,                   --  PolyORB.Servants.Servant
      RE_Execute_Request,           --  PolyORB.Servants.Iface.Execute_Request
      RE_Executed_Request,          --  PolyORB.Servants.Iface.Executed_Request
      RE_The_ORB,                   --  PolyORB.Setup.The_ORB
      RE_Boolean_1,                 --  PolyORB.Types.Boolean
      RE_Character_1,               --  PolyORB.Types.Character
      RE_Wide_Character_1,          --  PolyORB.Types.Wide_Character
      RE_Float_1,                   --  PolyORB.Types.Float
      RE_Long,                      --  PolyORB.Types.Long
      RE_String_1,                  --  PolyORB.Types.String
      RE_Wide_String_1,             --  PolyORB.Types.Wide_String
      RE_To_PolyORB_String,         --  PolyORB.Types.To_PolyORB_String
      RE_To_Standard_String,        --  PolyORB.Types.To_Standard_String
      RE_To_PolyORB_Wide_String,    --  PolyORB.Types.To_PolyORB_String
      RE_To_Standard_Wide_String,   --  PolyORB.Types.To_Standard_String
      RE_String_Ptr,                --  PolyORB_Utils_Strings.String_Ptr
      RE_Add,                       --  PolyORB.Utils.Strings."+"
      RE_And,                       --  PolyORB.Utils.Strings.Lists."&"
      RE_Empty,                     --  PolyORB.Utils.Strings.Lists.Empty
      RE_Boolean_2,                 --  Standard.Boolean
      RE_Character_2,               --  Standard.Character
      RE_Wide_Character_2,          --  Standard.Wide_Character
      RE_Integer,                   --  Standard.Integer
      RE_Natural,                   --  Standard.Natural
      RE_Float_2,                   --  Standard.Float
      RE_String_2,                  --  Standard.String
      RE_Wide_String_2,             --  Standard.Wide_String
      RE_Thread_Properties,         --  ARAO.Threads.Thread_Properties
      RE_Thread_Properties_Array,   --  ARAO.Threads.Thread_Properties_Array
      RE_Thread_Array_Access,       --  ARAO.Threads.Thread_Array_Access
      RE_Parameters_Source,         --  PolyORB.Parameters
      RE_Default_Priority,          --  System.Default_Priority
      RE_Priority,                  --  System.Priority
      RE_Mutex_Access,              --  PolyORB.Tasking.Mutexes.Mutex_Access
      RE_Create_2,                  --  PolyORB.Tasking.Advanced_Mutexes.Create
      RE_Enter,                     --  PolyORB.Tasking.Advanced_Mutexes.Enter
      RE_Leave);                    --  PolyORB.Tasking.Advanced_Mutexes.Leave

   RE_Unit_Table : constant array (RE_Id) of RU_Id
     := (RE_Null                      => RU_Null,
         RE_Ref_1                     => RU_Null,
         RE_From_Any_1                => RU_Null,
         RE_To_Any_1                  => RU_Null,
         RE_False                     => RU_Null,
         RE_True                      => RU_Null,
         RE_On                        => RU_Null,
         RE_Off                       => RU_Null,
         RE_Get                       => RU_Null,
         RE_Append                    => RU_Null,
         RE_Push_Back                 => RU_Null,
         RE_Link_To_Obj_Adapter       => RU_ARAO_Obj_Adapters,
         RE_Link_To_Obj_Adapter_2     => RU_ARAO_RT_Obj_Adapters,
         RE_Create_Periodic_Thread    => RU_ARAO_Periodic_Threads,
         RE_Emit_Msg                  => RU_ARAO_Requests,
         RE_Get_Ref                   => RU_ARAO_Utils,
         RE_Get_GIOP_Ref              => RU_ARAO_Utils,
         RE_Clock                     => RU_Ada_Real_Time,
         RE_Nanoseconds               => RU_Ada_Real_Time,
         RE_Microseconds              => RU_Ada_Real_Time,
         RE_Milliseconds              => RU_Ada_Real_Time,
         RE_Seconds                   => RU_Ada_Real_Time,
         RE_Minutes                   => RU_Ada_Real_Time,
         RE_Time                      => RU_Ada_Real_Time,
         RE_Time_Span                 => RU_Ada_Real_Time,
         RE_Add_Aggregate_Element     => RU_PolyORB_Any,
         RE_Any                       => RU_PolyORB_Any,
         RE_ARG_IN                    => RU_PolyORB_Any,
         RE_ARG_INOUT                 => RU_PolyORB_Any,
         RE_ARG_OUT                   => RU_PolyORB_Any,
         RE_From_Any_2                => RU_PolyORB_Any,
         RE_Get_Aggregate_Element     => RU_PolyORB_Any,
         RE_Get_Empty_Any             => RU_PolyORB_Any,
         RE_Get_Empty_Any_Aggregate   => RU_PolyORB_Any,
         RE_Set_Type                  => RU_PolyORB_Any,
         RE_TC_Boolean                => RU_PolyORB_Any,
         RE_TC_Float                  => RU_PolyORB_Any,
         RE_TC_Long                   => RU_PolyORB_Any,
         RE_TC_Character              => RU_PolyORB_Any,
         RE_TC_Wide_Character         => RU_PolyORB_Any,
         RE_TC_String                 => RU_PolyORB_Any,
         RE_TC_Wide_String            => RU_PolyORB_Any,
         RE_To_Any_2                  => RU_PolyORB_Any,
         RE_NamedValue                => RU_PolyORB_Any,
         RE_Add_Item                  => RU_PolyORB_Any_NVList,
         RE_Create                    => RU_PolyORB_Any_NVList,
         RE_Ref_2                     => RU_PolyORB_Any_NVList,
         RE_Add_Parameter             => RU_PolyORB_Any_TypeCode,
         RE_Local_Ref                 => RU_PolyORB_Any_TypeCode,
         RE_Object_Ptr                => RU_PolyORB_Any_TypeCode,
         RE_Object_Of                 => RU_PolyORB_Any_TypeCode,
         RE_To_Ref                    => RU_PolyORB_Any_TypeCode,
         RE_Disable_Reference_Counting => RU_PolyORB_Any_TypeCode,
         RE_TC_Alias                  => RU_PolyORB_Any_TypeCode,
         RE_TC_Struct                 => RU_PolyORB_Any_TypeCode,
         RE_Message                   => RU_PolyORB_Components,
         RE_Error_Container           => RU_PolyORB_Errors,
         RE_Catch                     => RU_PolyORB_Errors,
         RE_Found                     => RU_PolyORB_Errors,
         RE_Initialize_World          => RU_PolyORB_Initialization,
         RE_Module_Info               => RU_PolyORB_Initialization,
         RE_Register_Module           => RU_PolyORB_Initialization,
         RE_Run                       => RU_PolyORB_ORB,
         RE_Ref_3                     => RU_PolyORB_References,
         RE_Arguments                 => RU_PolyORB_Requests,
         RE_Request_Access            => RU_PolyORB_Requests,
         RE_Servant                   => RU_PolyORB_Servants,
         RE_Execute_Request           => RU_PolyORB_Servants_Iface,
         RE_Executed_Request          => RU_PolyORB_Servants_Iface,
         RE_The_ORB                   => RU_PolyORB_Setup,
         RE_Boolean_1                 => RU_PolyORB_Types,
         RE_Character_1               => RU_PolyORB_Types,
         RE_Wide_Character_1          => RU_PolyORB_Types,
         RE_Float_1                   => RU_PolyORB_Types,
         RE_Long                      => RU_PolyORB_Types,
         RE_String_1                  => RU_PolyORB_Types,
         RE_Wide_String_1             => RU_PolyORB_Types,
         RE_To_PolyORB_String         => RU_PolyORB_Types,
         RE_To_Standard_String        => RU_PolyORB_Types,
         RE_To_PolyORB_Wide_String    => RU_PolyORB_Types,
         RE_To_Standard_Wide_String   => RU_PolyORB_Types,
         RE_String_Ptr                => RU_PolyORB_Utils_Strings,
         RE_Add                       => RU_PolyORB_Utils_Strings,
         RE_And                       => RU_PolyORB_Utils_Strings_Lists,
         RE_Empty                     => RU_PolyORB_Utils_Strings_Lists,
         RE_Boolean_2                 => RU_Standard,
         RE_Character_2               => RU_Standard,
         RE_Wide_Character_2          => RU_Standard,
         RE_Integer                   => RU_Standard,
         RE_Natural                   => RU_Standard,
         RE_Float_2                   => RU_Standard,
         RE_String_2                  => RU_Standard,
         RE_Wide_String_2             => RU_Standard,
         RE_Thread_Properties         => RU_ARAO_Threads,
         RE_Thread_Properties_Array   => RU_ARAO_Threads,
         RE_Thread_Array_Access       => RU_ARAO_Threads,
         RE_Parameters_Source         => RU_PolyORB_Parameters,
         RE_Default_Priority          => RU_System,
         RE_Priority                  => RU_System,
         RE_Mutex_Access              => RU_PolyORB_Tasking_Mutexes,
         RE_Create_2                  => RU_PolyORB_Tasking_Mutexes,
         RE_Enter                     => RU_PolyORB_Tasking_Mutexes,
         RE_Leave                     => RU_PolyORB_Tasking_Mutexes);

   procedure Initialize;
   procedure Reset;

   function RE (Id : RE_Id; Withed : Boolean := True) return Node_Id;
   --  Return a designator for entity Id

   function RU (Id : RU_Id; Withed : Boolean := True) return Node_Id;
   --  Return a node for Unit Id

end Ocarina.Backends.PO_QoS_Ada.Runtime;
