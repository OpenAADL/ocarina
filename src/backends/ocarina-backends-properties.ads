------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . P R O P E R T I E S           --
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

--  This packege contains the Ada-side definition of all AADL
--  properties that are relevent to the code generation. It extracts
--  brute information and doeas only sonsistency check.

package Ocarina.Backends.Properties is

   type Name_Array is array (Nat range <>) of Name_Id;
   Empty_Name_Array : constant Name_Array;

   type ULL_Array is array (Nat range <>) of Unsigned_Long_Long;
   Empty_ULL_Array  : constant ULL_Array;

   type LL_Array is array (Nat range <>) of Long_Long;
   Empty_LL_Array   : constant LL_Array;

   type LD_Array is array (Nat range <>) of Long_Double;
   Empty_LD_Array   : constant LD_Array;

   --  Common types to several components and entities

   type Size_Units is (Bit, Byte, Kilo_Byte, Mega_Byte, Giga_Byte, Tera_Byte);

   type Size_Type is record
      S : Unsigned_Long_Long;
      U : Size_Units;
   end record;

   Null_Size : constant Size_Type;

   type Supported_Source_Language is
     (Language_Ada_95,
      Language_ASN1,
      Language_C,
      Language_Esterel,
      Language_Device,
      Language_Gui,
      Language_Lua,
      Language_Lustre,
      Language_Rhapsody,
      Language_SDL_RTDS,
      Language_CPP,
      Language_SDL_OpenGEODE,
      Language_RTSJ,
      Language_Scade,
      Language_SDL,
      Language_Simulink,
      Language_System_C,
      Language_VHDL,
      Language_None);

   type Supported_Scheduling_Protocol is
     (PARAMETRIC_PROTOCOL,
      EARLIEST_DEADLINE_FIRST_PROTOCOL,
      LEAST_LAXITY_FIRST_PROTOCOL,
      RATE_MONOTONIC_PROTOCOL,
      DEADLINE_MONOTONIC_PROTOCOL,
      ROUND_ROBIN_PROTOCOL,
      TIME_SHARING_BASED_ON_WAIT_TIME_PROTOCOL,
      POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL,
      D_OVER_PROTOCOL,
      MAXIMUM_URGENCY_FIRST_BASED_ON_LAXITY_PROTOCOL,
      MAXIMUM_URGENCY_FIRST_BASED_ON_DEADLINE_PROTOCOL,
      TIME_SHARING_BASED_ON_CPU_USAGE_PROTOCOL,
      NO_SCHEDULING_PROTOCOL,
      HIERARCHICAL_CYCLIC_PROTOCOL,
      HIERARCHICAL_ROUND_ROBIN_PROTOCOL,
      HIERARCHICAL_FIXED_PRIORITY_PROTOCOL,
      HIERARCHICAL_PARAMETRIC_PROTOCOL,
      Unknown_Scheduler);

   type Supported_POK_Scheduler is
     (RMS, EDF, LLF, RR, Static, Invalid_Scheduler);

   type Supported_POK_Architectures is
     (POK_Arch_x86, POK_Arch_Sparc, POK_Arch_Ppc, Invalid_Architecture);

   type Supported_POK_BSP is
     (POK_BSP_x86_qemu,
      POK_BSP_x86_qemu_vmm,
      POK_BSP_Leon,
      POK_BSP_prep,
      Invalid_BSP);

   type ARINC653_Queuing_Discipline is (Fifo, Priority_Based, Invalid);

   type ARINC653_Access_Type is (Read, Write, Read_Write, Invalid);

   type ARINC653_Memory_Kind is (Code, Data, Invalid);

   type Supported_POK_Error is
     (POK_Error_Deadline_Missed,
      POK_Error_Application,
      POK_Error_Numeric,
      POK_Error_Illegal_Request,
      POK_Error_Stack_Overflow,
      POK_Error_Memory_Violation,
      POK_Error_Hardware_Fault,
      POK_Error_Power_Fail,
      POK_Error_Partition_Configuration,
      POK_Error_Partition_Init,
      POK_Error_Partition_Scheduling,
      POK_Error_Kernel_Init,
      POK_Error_Kernel_Scheduling,
      POK_Error_Invalid);

   type POK_Handled_Errors is array (Nat range <>) of Supported_POK_Error;
   POK_Empty_Handled_Errors : constant POK_Handled_Errors;

   type Supported_ARINC653_Error is
     (ARINC653_Error_Deadline_Miss,
      ARINC653_Error_Application,
      ARINC653_Error_Numeric,
      ARINC653_Error_Illegal_Request,
      ARINC653_Error_Stack_Overflow,
      ARINC653_Error_Memory_Violation,
      ARINC653_Error_Hardware_Fault,
      ARINC653_Error_Power_Fail,
      ARINC653_Error_Partition_Config,
      ARINC653_Error_Partition_Handler,
      ARINC653_Error_Partition_Init,
      ARINC653_Error_Partition_Scheduling,
      ARINC653_Error_Module_Init,
      ARINC653_Error_Module_Config,
      ARINC653_Error_Module_Scheduling,
      ARINC653_Error_Invalid);

   type ARINC653_Errors is array (Nat range <>) of Supported_ARINC653_Error;
   ARINC653_Empty_Errors : constant ARINC653_Errors;

   type Supported_POK_Action is
     (POK_Action_Ignore,
      POK_Action_Confirm,
      POK_Action_Thread_Restart,
      POK_Action_Thread_Stop_And_Start_Another,
      POK_Action_Thread_Stop,
      POK_Action_Partition_Restart,
      POK_Action_Partition_stop,
      POK_Action_Kernel_Stop,
      POK_Action_Kernel_Restart,
      POK_Action_Nothing,
      POK_Action_Invalid);

   type POK_Handled_Actions is array (Nat range <>) of Supported_POK_Action;
   POK_Empty_Handled_Actions : constant POK_Handled_Actions;

   type Supported_ARINC653_Action is
     (ARINC653_Action_Ignore,
      ARINC653_Action_Confirm,
      ARINC653_Action_Partition_Stop,
      ARINC653_Action_Module_Stop,
      ARINC653_Action_Process_Stop,
      ARINC653_Action_Process_Stop_And_Start_Another,
      ARINC653_Action_Process_Restart,
      ARINC653_Action_Partition_Restart,
      ARINC653_Action_Module_Restart,
      ARINC653_Action_Nothing,
      ARINC653_Action_Invalid);

   type ARINC653_Actions is array (Nat range <>) of Supported_ARINC653_Action;
   ARINC653_Empty_Actions : constant ARINC653_Actions;

   function Get_Source_Language (E : Node_Id) return Supported_Source_Language;
   --  Return the language of the entity instance (subprogram, thread,
   --  data, port, etc...) implementation. Language_None is returned
   --  if no language has been specified bu the user. It is up to the
   --  user to interpret it as an error or to assign a default value.

   function Get_Source_Text (E : Node_Id) return Name_Array;
   --  Return the list of files (source, objects or libraries)
   --  involved in the implementation of the entity E (data
   --  subprogram, thread, port, etc...).

   function Get_Source_Name (E : Node_Id) return Name_Id;
   --  Return the implementation name of the entity instance E (data
   --  subprogram, thread, port, etc...). No_Name is returned if no
   --  implementation has been specified by the user.

   function Get_Transfo_Source_Name (E : Node_Id) return Name_Id;
   --  Same as above, but with the transformations::source_name
   --  generated by the transfo package.

   function Get_Type_Source_Name (E : Node_Id) return Name_Id;
   --  Return the implementation name of the entity instance E (data
   --  subprogram, thread, port, etc...). No_Name is returned if no
   --  implementation has been specified by the user.

   -------------------------------
   -- Data Component Properties --
   -------------------------------

   --  Supported data types

   type Supported_Data_Representation is
     (Data_Array,
      Data_Boolean,
      Data_Character,
      Data_Wide_Character,
      Data_Enum,
      Data_Float,
      Data_Fixed,
      Data_Integer,
      Data_String,
      Data_Wide_String,
      Data_Struct,
      Data_Union,
      Data_With_Accessors,
      Data_None); --  Leads to an error

   type Supported_Data_Access is
     (Access_Read_Only,
      Access_Write_Only,
      Access_Read_Write,
      Access_By_Method,
      Access_None);

   type Supported_Concurrency_Control_Protocol is --  XXX
     (None_Specified,
      Priority_Inheritance,
      Priority_Ceiling);

   type Supported_IEEE754_Precision is
     (Precision_Simple, Precision_Double, Precision_None);

   type Supported_Number_Representation is
     (Signed, Unsigned, None);

   function Get_Base_Type (D : Node_Id) return List_Id;
   --  Return the component instance that defines the base data type
   --  of component instance D or the list of component instances in
   --  case of a data structure.

   function Get_Code_Set (D : Node_Id) return Unsigned_Long_Long;
   --  Return the code set associated to the data instance D

   function Get_Element_Names (D : Node_Id) return Name_Array;
   --  Return the element names of fields name of a data structure

   function Get_Enumerators (D : Node_Id) return Name_Array;
   --  Return the enumerators of enumeration type D

   function Get_IEEE754_Precision
     (D : Node_Id) return Supported_IEEE754_Precision;
   --  Return the precision of a floating point data type

   function Get_Initial_Value (D : Node_Id) return Name_Array;
   --  Return the initial value list of a data type

   function Get_Integer_Range (D : Node_Id) return LL_Array;
   --  Return the range associated to an integer data type

   function Get_Real_Range (D : Node_Id) return LD_Array;
   --  Return the range associated to an real data type

   function Get_Mesurement_Unit (D : Node_Id) return Name_Id;
   --  Return the measurement unit of a data being communicated

   function Get_Number_Representation
     (D : Node_Id) return Supported_Number_Representation;
   --  Return the return the number representation (signed, unsigned)
   --  of the data D.

   function Get_Data_Representation
     (D : Node_Id) return Supported_Data_Representation;
   --  Return the representation kind of a data component. If no
   --  representation kind has been specified by the user, returns
   --  Data_None.

   function Get_Data_Digits (D : Node_Id) return Unsigned_Long_Long;
   --  Return the value of the "data_digits" aadl property of a data
   --  component. Return 0 if the property is not defined for the
   --  component.

   function Get_Data_Scale (D : Node_Id) return Unsigned_Long_Long;
   --  Return the value of the "data_scale" aadl property of a data
   --  component. Return 0 if the property is not defined for the
   --  component.

   function Get_Data_Size (D : Node_Id) return Size_Type;
   --  Return the size of the data. Returns Null_Size if not defined.

   function Get_Provided_Data_Access
     (D : Node_Id) return Supported_Data_Access;
   --  Return the provided data access of a data component instance
   --  D. Access_None is returned when the provided data access
   --  property is not defined for D.

   function Get_Priority_Celing_Of_Data_Access
     (D : Node_Id) return Unsigned_Long_Long;
   --  Return the prioriry associated to the data component. Otherwise,
   --  return 0.

   function Get_Required_Data_Access
     (D : Node_Id) return Supported_Data_Access;
   --  Return the required data access of a data component instance
   --  D. Access_None is returned when the required data access
   --  property is not defined for D.

   function Get_Dimension (D : Node_Id) return ULL_Array;
   --  Return the maximum length for [wide] string types and the
   --  lengths for an array.

   function Get_Concurrency_Protocol
     (D : Node_Id) return Supported_Concurrency_Control_Protocol;
   --  Return the concurrency protocol of a data type with
   --  accessors. Return Concurrency_NoneSpecified if the property is
   --  not defined.

   ---------------------------------
   -- Thread Component Properties --
   ---------------------------------

   type Supported_Thread_Dispatch_Protocol is
     (Thread_Periodic,
      Thread_Aperiodic,
      Thread_Sporadic,
      Thread_Hybrid,
      Thread_Timed,
      Thread_Background,
      Thread_ISR,
      Thread_None);

   type Time_Units is
     (Picosecond, Nanosecond, Microsecond, Millisecond, Second, Minute, Hour);

   type Time_Type is record
      T : Unsigned_Long_Long;
      U : Time_Units;
   end record;

   Null_Time : constant Time_Type;

   function Convert_Value_To_Time_Type (V : Node_Id) return Time_Type;

   type Supported_Thread_Implementation is
     (Thread_With_Call_Sequence,
      Thread_With_Compute_Entrypoint,
      Thread_With_Port_Compute_Entrypoint,
      Thread_Unknown);

   type Supported_POSIX_Scheduling_Policy is
     (SCHED_FIFO, SCHED_RR, SCHED_OTHERS, None);

   function Get_Dispatch_Offset (T : Node_Id) return Time_Type;
   --  Return the thread dispatch offset for periodic task.

   function Get_Thread_Dispatch_Protocol
     (T : Node_Id) return Supported_Thread_Dispatch_Protocol;
   --  Return the dispatching protocol of a given thread component. If
   --  no protocol has been specified by the user, then return
   --  Thread_None.

   function Get_Thread_POSIX_Scheduling_Policy
     (T : Node_Id) return Supported_POSIX_Scheduling_Policy;
   --  Return the POSIX scheduling policy of a given thread
   --  component. If no policy has been specified, then return none.

   function Get_Thread_Period (T : Node_Id) return Time_Type;
   --  Return the period (for periodic threads) or the minumum
   --  dispatch interval (for sporadic threads). The returned value is
   --  converted to the closest time unit depending on its unit in
   --  AADL. Raises an error if the thread is not periodic nor
   --  sporadic.

   function Get_Thread_Deadline (T : Node_Id) return Time_Type;
   --  Return the deadline of a thread. The returned value is
   --  converted to the closest time unit depending on its unit in
   --  AADL. If the thread has no deadline property defined, return
   --  the value of the thread period.

   function Get_Thread_First_Dispatch_Time (T : Node_Id) return Time_Type;
   --  Return the first dispact time of a thread. The returned value
   --  is converted to the closest time unit depending on its unit in
   --  AADL. If the thread has no deadline property defined, return
   --  the value of the thread period.

   function Get_Thread_Priority (T : Node_Id) return Unsigned_Long_Long;
   --  Return the thread priority given by the user. Otherwise, return
   --  0.

   function Get_Thread_Stack_Size (T : Node_Id) return Size_Type;
   --  Return the storage size of a thread component. The returned
   --  value is associated with its unit given in the AADL model. If
   --  the user did not give a storage size value, then returns 0 bits.

   function Get_Thread_Implementation_Kind
     (T : Node_Id) return Supported_Thread_Implementation;
   --  Return the kind of the thread implementation. Return
   --  Thread_Unknown if no valid kind can be deduced.

   function Get_Thread_Compute_Entrypoint
     (T       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Return the compute entry point of the given thread. Return
   --  No_Name in case the property is not defined for the thread.  If
   --  In_Mode is a valid mode name for the thread, return the
   --  property association declared in the contect of this mode.

   function Get_Thread_Compute_Entrypoint
     (T       : Node_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Same as above, but returns the whole property association

   function Get_Implementation (E : Node_Id) return Node_Id;
   --  Return the abstract component implementation attached to a
   --  component through the Implemented_As property.

   function Get_Thread_Activate_Entrypoint (T : Node_Id) return Name_Id;
   --  Return the activate entry point of the given thread. Return
   --  No_Name in case the property is not defined for the thread.

   function Get_Thread_Activate_Entrypoint (T : Node_Id) return Node_Id;
   --  Same as before but returns the classifier associated with the
   --  property (used in AADLv2).

   function Get_Thread_Initialize_Entrypoint (T : Node_Id) return Name_Id;
   --  Return the initialize entry point of the given thread. Return
   --  No_Name in case the property is not defined for the thread.

   function Get_Thread_Initialize_Entrypoint (T : Node_Id) return Node_Id;
   --  Same as before but returns the classifier associated with the
   --  property (used in AADLv2).

   function Get_Thread_Recover_Entrypoint (T : Node_Id) return Name_Id;
   --  Return the recovery entry point of the given thread. Return
   --  No_Name in case the property is not defined for the thread.

   function Get_Thread_Recover_Entrypoint (T : Node_Id) return Node_Id;
   --  Same as before but returns the classifier associated with the
   --  property (used in AADLv2).

   function Is_Fusioned (E : Node_Id) return Boolean;
   --  Return True IFF the entity instance E results from a thread
   --  fusion.

   function Is_Priority_Shifter (E : Node_Id) return Boolean;
   --  Return True IFF the entity instance E is a priority shifter data.

   function Get_Thread_Scheduler (E : Node_Id) return Name_Id;
   --  Return the scheduler name

   function Get_Thread_Reference_Name (E : Node_Id) return Name_Id;
   --  Return the name original name, before without regard for later
   --  move operation

   -------------------------------------
   -- Subprogram Component Properties --
   -------------------------------------

   type Supported_Subprogram_Kind is
     (Subprogram_Unknown,
      Subprogram_Default,
      Subprogram_Empty,
      Subprogram_Opaque_Ada_95,
      Subprogram_Opaque_Ada_95_Transfo,
      Subprogram_Opaque_C,
      Subprogram_Opaque_CPP,
      Subprogram_Opaque_RTSJ,
      Subprogram_Simulink,
      Subprogram_Scade,
      Subprogram_Lustre,
      Subprogram_Esterel,
      Subprogram_Lua,
      Subprogram_Pure_Call_Sequence,
      Subprogram_Hybrid_Ada_95);

   function Get_Subprogram_Kind (S : Node_Id) return Supported_Subprogram_Kind;
   --  Return the kind of a subprogram depending on its internal
   --  structure. Subprogram_Unknown is returned if the subprogram
   --  kind cannot be deduced.

   ----------------------------------
   -- Process Component Properties --
   ----------------------------------

   function Get_Bound_Processor (P : Node_Id) return Node_Id;
   --  Return the processor component to which the process P is
   --  bound. Raises an error if P is not bound to any processor.

   function Get_Bound_Function (P : Node_Id) return Node_Id;
   --  Return the function bound to this component.

   function Get_Bound_Memory (P : Node_Id) return Node_Id;
   --  Return the memory component to which the process P is
   --  bound. Returns No_Node is no memory component is bounded.

   function Get_Needed_Memory_Size (P : Node_Id) return Unsigned_Long_Long;
   --  Return the needed memory size for a partition

   function Get_Port_Number (P : Node_Id) return Value_Id;
   --  Return the port number of the process or device P

   function Get_Byte_Count (S : Node_Id) return Unsigned_Long_Long;
   --  Get the Byte_Count property of a memory component. Return 0
   --  if the component is not set.

   function Get_Word_Size (S : Node_Id) return Size_Type;
   --  Get the Word_Size property of a memory component. Return Null_Size
   --  if the component is not set.

   function Get_Scheduling_Protocol
     (P : Node_Id) return Supported_Scheduling_Protocol;
   --  Get the Scheduling_Protocol of process. Return
   --  Unknown_Scheduler if undefined.

   ------------------------------------
   -- Processor Component Properties --
   ------------------------------------

   type Supported_Execution_Platform is
     (Platform_Native,
      Platform_Native_Compcert,
      Platform_Bench,
      Platform_Gumstix_RTEMS,
      Platform_Gumstix_RTEMS_POSIX,
      Platform_NDS_RTEMS,
      Platform_NDS_RTEMS_POSIX,
      Platform_LEON_RTEMS,
      Platform_LEON_RTEMS_POSIX,
      Platform_X86_RTEMS,
      Platform_X86_RTEMS_POSIX,
      Platform_X86_LINUXTASTE,
      Platform_LEON_GNAT,
      Platform_LEON3_SCOC3,
      Platform_LEON3_XM3,
      Platform_LEON3_XTRATUM,
      Platform_LEON_ORK,
      Platform_WIN32,
      Platform_LINUX32,
      Platform_LINUX32_XENOMAI_NATIVE,
      Platform_LINUX32_XENOMAI_POSIX,
      Platform_LINUX64,
      Platform_ERC32_ORK,
      Platform_ARM_DSLINUX,
      Platform_ARM_N770,
      Platform_MARTE_OS,
      Platform_VxWorks,
      Platform_None); --  Unspecified

   function Get_Execution_Platform
     (P : Node_Id) return Supported_Execution_Platform;
   function Get_Execution_Platform (P : Node_Id) return Name_Id;
   --  Return the execution platform of the given processor P

   function Get_Location (P : Node_Id) return Name_Id;
   --  Return the location of the processor or device P. No_Name is
   --  returned if the processor to which O is mapped does not have a
   --  location and an error is raised if P is not mapped to any
   --  processor.

   function Get_Scheduler_Quantum (P : Node_Id) return Time_Type;

   --------------------------------
   -- AADL Connection Properties --
   --------------------------------

   function Get_Bound_Bus
     (C     : Node_Id;
      Check : Boolean := True) return Node_Id;
   --  Return the bus component to which the connection C is
   --  bound. Return an error if no bus is bound and Check is true,
   --  and No_Node if C is not bound to any bus and Check is false.

   ------------------------------
   -- Bus Component Properties --
   ------------------------------

   type Supported_Transport_APIs is
     (Transport_BSD_Sockets,
      Transport_User,
      Transport_SpaceWire,
      Transport_None); --  Leads to an error

   function Get_Transport_API
     (B : Node_Id;
      E : Node_Id := No_Node) return Supported_Transport_APIs;
   --  Return the transport layer supported by the bus B

   ----------------------
   --  Port properties --
   ----------------------

   type Supported_Port_Timing is
     (Port_Timing_Sampled,
      Port_Timing_Immediate,
      Port_Timing_Delayed,
      Port_Timing_None); --  Unspecified

   type Supported_Overflow_Handling_Protocol is
     (Overflow_Handling_Protocol_DropOldest,
      Overflow_Handling_Protocol_DropNewest,
      Overflow_Handling_Protocol_Error,
      Overflow_Handling_Protocol_None); --  Unspecified

   Default_Queue_Size : constant := 16;
   --  FIXME: Find a way to fetch the value from AADL_Project

   function Get_Queue_Size (P : Node_Id) return Long_Long;
   --  Return the size of the queue correspodning to and event [data]
   --  port. Return -1 if non specified by the user.

   function Get_Port_Timing (P : Node_Id) return Supported_Port_Timing;
   --  Return the timing (sampled, immediate or delayed) of the given
   --  port. Return Port_Timing_None in case the property is not defined
   --  for the port.

   function Get_Overflow_Handling_Protocol
     (P : Node_Id) return Supported_Overflow_Handling_Protocol;
   --  Return the overflow handling protocol of the given port. Return
   --  Overflow_Handling_Protocol_None in case the property is not
   --  defined.

   function Get_Port_Urgency (P : Node_Id) return Unsigned_Long_Long;
   --  Return the urgency of the given port. Return 0 in case the
   --  property is not defined.

   function Get_Port_Compute_Entrypoint
     (P       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Return the compute entry point of the given port. Return
   --  No_Name in case the property is not defined for the port.

   function Get_Port_Compute_Entrypoint
     (P       : Node_Id;
      In_Mode : Name_Id := No_Name) return Node_Id;
   --  Same as above, but returns the whole property association

   ---------------------------------
   -- System component properties --
   ---------------------------------

   type Protocol_Type is (Protocol_IIOP, Protocol_DIOP, Protocol_None);

   function Get_Protocol (S : Node_Id) return Protocol_Type;
   --  Return the protocol of a distributed application.

   ----------
   -- Misc --
   ----------

   procedure Init;
   --  Initialize some internal parts of the package

   No_Value : constant Value_Id := 0;
   --  Used to indicate a null value. This value does not depend on
   --  whether we use AADL values or other language values.

   function Get_ARINC653_Queuing_Discipline
     (Port : Node_Id) return ARINC653_Queuing_Discipline;
   --  Returns the corresponding Queuing_Discipline for the ARINC653
   --  property set.

   function Get_POK_Recovery_Errors (E : Node_Id) return POK_Handled_Errors;
   --  Returns the list of handled errors.

   function Get_POK_Recovery_Actions (E : Node_Id) return POK_Handled_Actions;
   --  Returns the list of handled actions for each error.

   function Get_ARINC653_HM_Errors (E : Node_Id) return ARINC653_Errors;
   --  Returns the list of errors declared in the Health Monitoring service
   --  of ARINC653.

   function Get_ARINC653_HM_Actions (E : Node_Id) return ARINC653_Actions;
   --  Returns the list of actions used to recover error with ARINC653.

   function Get_POK_Architecture
     (P : Node_Id) return Supported_POK_Architectures;
   --  Returns the value of POK_Architecture value bounded to a
   --  processor.

   function Get_POK_BSP (P : Node_Id) return Supported_POK_BSP;
   --  Returns the value of the POK_BSP property.

   function Get_POK_Scheduler (P : Node_Id) return Supported_POK_Scheduler;
   --  Returns the value of the POK::Scheduler property.

   function Get_Source_Location (E : Node_Id) return Name_Id;
   --  Returns the value of the property POK::Source_Location.

   function Get_Timeslice (T : Node_Id) return Time_Type;
   --  Returns the Time_Type associated with the property
   --  POK::Timeslice.

   function Get_Provided_Virtual_Bus_Class (E : Node_Id) return Node_Id;

   function Get_Security_Level (E : Node_Id) return Unsigned_Long_Long;

   function Get_Security_Level_Through_Virtual_Bus
     (E : Node_Id) return Unsigned_Long_Long;

   type Time_Array is array (Nat range <>) of Time_Type;
   Empty_Time_Array : constant Time_Array;

   function Get_Execution_Time (E : Node_Id) return Time_Array;
   --  Returns the Execution Time values associated with the
   --  Execution_Time property. Returns the value Empty_Time_Array
   --  if the property is not defined.

   function Get_Bounded_Virtual_Bus_Classes (E : Node_Id) return List_Id;
   --  Returns a list of classifier with the bounded Virtual bus classes.

   function Get_POK_Mils_Verified (E : Node_Id) return Boolean;
   --  Get the POK::Mils_Verifies property and tells if it is true
   --  or false. If the property is not defined, it returns false.

   function Get_Compute_Deadline (E : Node_Id) return Time_Type;
   --  Returns the Time associated with the Compute_Deadline property.
   --  If the component does not have this property, it returns
   --  the Null_Time value.

   function Get_ARINC653_Timeout (E : Node_Id) return Time_Type;
   --  Returns the Time associated with the ARINC653::Timeout
   --  property. Returns Null_Time if not set.

   function Get_POK_Refresh_Time (E : Node_Id) return Time_Type;
   --  Returns the value of the property POK::Refresh_Time associated
   --  to a data port.

   function Get_Wait_For_All_Events (E : Node_Id) return Boolean;
   --  Return the value of the property SEI::Wait_For_All_Events.
   --  Check if a thread must wait for all events port before the execution
   --  of its subprograms.

   function Get_Miss_Rate (E : Node_Id) return Unsigned_Long_Long;
   --  Return the value of the property SEI::Stream_Miss_Rate. Returns
   --  the value as an unsigned_long_long.

   function Get_POK_Slots (E : Node_Id) return Time_Array;
   --  Return the list of time slots

   function Get_POK_Slots_Allocation (E : Node_Id) return List_Id;
   --  Get the POK::Slots_Allocation property that describes the use
   --  of the slots

   function Get_POK_Major_Frame (E : Node_Id) return Time_Type;
   --  Return the major frame value specified on a processor

   function Get_Scade_Signal
     (E       : Node_Id;
      In_Mode : Name_Id := No_Name) return Name_Id;
   --  Returns the name_id that correspond to the scade::signal
   --  property on a parameter.

   function Get_Code_Size (E : Node_Id) return Size_Type;
   --  Returns the value of the Source_Code_Size property.

   function Get_ARINC653_Access_Type
     (Memory : Node_Id) return ARINC653_Access_Type;
   --  Returns the access_type property value of a memory component.

   function Get_ARINC653_Memory_Kind
     (Memory : Node_Id) return ARINC653_Memory_Kind;
   --  Returns the memory_kind property value of a memory component.

   -----------------------
   -- Device properties --
   -----------------------

   function Get_Configuration (Device : Node_Id) return Name_Id;
   --  Returns the configuraton string attached to a device

   function Get_Driver_Name (Device : Node_Id) return Name_Id;
   --  Returns the name of the driver.

   function Get_Send_Function_Name (Device : Node_Id) return Name_Id;
   --  Returns the sender function name of a device.
   --  It corresponds to the receiver subcomponent in the abstract
   --  device that models the content of the device.

   function Get_Execution_Time (T : Node_Id) return Time_Type;
   function Get_Period (T : Node_Id) return Time_Type;

   function Get_Base_Address (D : Node_Id) return Unsigned_Long_Long;
   function Get_Memory_Size (D : Node_Id) return Size_Type;

   --------------------------
   -- Processor properties --
   --------------------------

   function Get_Core_Id (D : Node_Id) return Unsigned_Long_Long;

private

   Empty_Name_Array : constant Name_Array (1 .. 0) := (others => No_Name);
   Empty_ULL_Array  : constant ULL_Array (1 .. 0)  := (others => 0);
   Empty_LL_Array   : constant LL_Array (1 .. 0)   := (others => 0);
   Empty_LD_Array   : constant LD_Array (1 .. 0)   := (others => 0.0);
   Empty_Time_Array : constant Time_Array (1 .. 0) :=
     (others => (0, Picosecond));

   POK_Empty_Handled_Actions : constant POK_Handled_Actions (1 .. 0) :=
     (others => POK_Action_Invalid);

   POK_Empty_Handled_Errors : constant POK_Handled_Errors (1 .. 0) :=
     (others => POK_Error_Invalid);

   ARINC653_Empty_Actions : constant ARINC653_Actions (1 .. 0) :=
     (others => ARINC653_Action_Invalid);

   ARINC653_Empty_Errors : constant ARINC653_Errors (1 .. 0) :=
     (others => ARINC653_Error_Invalid);

   Null_Size : constant Size_Type := (0, Bit);

   Null_Time : constant Time_Type := (0, Picosecond);

end Ocarina.Backends.Properties;
