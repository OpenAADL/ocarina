------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . C _ C O M M O N . M A P P I N G     --
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

with Ocarina.Backends.Properties; use Ocarina.Backends.Properties;

package Ocarina.Backends.C_Common.Mapping is

   procedure Call_Remote_Functions
     (Caller_Thread : Node_Id;
      Spg_Call      : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id);
   --  Calls all the remote subprograms connected to the features of
   --  Spg_Call. Appends the possibly added declaration to the
   --  Declarations lists and the call statements to the
   --  Statements. These two lists have to be created before the call
   --  to Call_Remote_Subprograms.

   --  Tree binding operations
   function Map_Distributed_Application (E : Node_Id) return Node_Id;
   function Map_HI_Node
     (E      : Node_Id;
      Kernel : Boolean := False) return Node_Id;
   function Map_HI_Unit (E : Node_Id) return Node_Id;
   procedure Bind_AADL_To_Activity (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Global_Names (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Global_Model_Names (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Naming (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Job (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Main (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Enumerator (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Stub (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Feature_Subprogram (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Subprogram (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Servers (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Entities (G : Node_Id; A : Node_Id);
   function Map_Task_Job_Identifier
     (E                : Node_Id;
      Prefix_Component : Node_Id := No_Node) return Node_Id;
   function Map_Time
     (T        : Time_Type;
      Variable : Name_Id := No_Name) return Node_Id;
   function Map_C_Enum_Name (E : Node_Id; Enumerator : Name_Id) return Name_Id;
   function Map_C_Enumerator_Name
     (E                    : Node_Id;
      Custom_Parent        : Node_Id := No_Node;
      Fully_Qualify_Parent : Boolean := False;
      Entity               : Boolean := False;
      Server               : Boolean := False;
      Port_Type            : Boolean := False;
      Local_Port           : Boolean := False) return Name_Id;

   function Map_C_Define_Name
     (E                                             : Node_Id;
      Nb_Ports                                      : Boolean := False;
      Nb_States                                     : Boolean := False;
      Max_Dispatch_Transitions_Per_Complete_State   : Boolean := False;
      Max_Dispatch_Triggers_Per_Dispatch_Transition : Boolean := False)
     return Name_Id;

   function Map_C_Full_Parameter_Name
     (Spg    : Node_Id;
      P      : Node_Id;
      Suffix : Character := ASCII.NUL) return Name_Id;

   function Map_C_Feature_Subprogram
     (A     : Node_Id;
      Owner : Node_Id := No_Node) return Node_Id;
   --  Maps an Identifier from the given Subprogram spec instance. If
   --  Owner is not given (typically when mapping a data component
   --  instance to a C type) deduce it from the parent component of
   --  the feature A. If Owner is given (typically for mapping feature
   --  subprogram calls that are actually connected to a data
   --  subcomponent using AADL accesses), use the given Owner
   --  identifier to map the final name.

   function Map_C_Data_Type_Designator (E : Node_Id) return Node_Id;
   function Map_C_Defining_Identifier
     (A          : Node_Id;
      Is_Pointer : Boolean := False) return Node_Id;
   procedure Bind_AADL_To_Type_Definition (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Process_Request (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Types (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Object (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Request_Type (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Deployment (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Request (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Marshaller (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Unmarshaller (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Local_Port (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Global_Port (G : Node_Id; A : Node_Id);
   function Map_Stub_Identifier (E : Node_Id) return Node_Id;
   function Map_C_Subprogram_Spec
     (S                 : Node_Id;
      Containing_Device : Node_Id := No_Node) return Node_Id;
   function Map_C_Subprogram_Body
     (S                 : Node_Id;
      Containing_Device : Node_Id := No_Node) return Node_Id;
   function Map_C_Subprogram_Identifier (E : Node_Id) return Node_Id;
   function Map_C_Marshaller_Subprogram
     (A             : Node_Id;
      Is_Unmarshall : Boolean := False;
      Is_Request    : Boolean := False) return Node_Id;
   procedure Bind_AADL_To_Default_Value (G : Node_Id; A : Node_Id);
   function Map_Task_Deliver_Identifier (E : Node_Id) return Node_Id;
   function Map_C_Operation_Name (E : Node_Id) return Name_Id;
   function Map_C_Port_Data_Name (E : Node_Id; P : Node_Id) return Name_Id;
   function Map_C_Variable_Name
     (E                   : Node_Id;
      Port_Variable       : Boolean := False;
      Port_History        : Boolean := False;
      Port_Woffsets       : Boolean := False;
      Port_Empties        : Boolean := False;
      Port_First          : Boolean := False;
      Port_Queue          : Boolean := False;
      Port_Recent         : Boolean := False;
      Port_Fifo_Size      : Boolean := False;
      Port_Offsets        : Boolean := False;
      Port_Used_Size      : Boolean := False;
      Port_N_Dest         : Boolean := False;
      Port_Local_Dest     : Boolean := False;
      Port_Destinations   : Boolean := False;
      Port_Total_Fifo     : Boolean := False;
      Port_Request        : Boolean := False;
      Request_Variable    : Boolean := False;
      State_Name_T        : Boolean := False;
      State_T             : Boolean := False;
      States_Array        : Boolean := False;
      Current_State       : Boolean := False) return Name_Id;

   function Map_C_BA_Related_Function_Name
     (E                          : Node_Id;
      BA_Body                    : Boolean := False;
      States_Initialization      : Boolean := False;
      BA_Initialization          : Boolean := False;
      Update_Next_Complete_State : Boolean := False) return Name_Id;

   function Map_Port_Data_With_Virtual_Bus
     (E                    : Node_Id;
      Virtual_Bus          : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   --  Map the name of the data variable when we receive on a port.
   --  Here, this function is dedicated to virtual bus and should be
   --  used when we map a port with a particular virtual bus as protocol.
   --  Parameter E and Containing_Component come from the instance
   --  model whereas parameter Virtual_Bus is a component from the AADL tree.

   function Map_Port_Name
     (E                    : Node_Id;
      Is_Global            : Boolean := False;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   function Map_Port_Data
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   function Map_Port_Var
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   function Map_Port_Var_Length
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   function Map_Port_Var_Length_With_Virtual_Bus
     (E                    : Node_Id;
      Virtual_Bus          : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   --  Map the name of the length variable when we receive on a port.
   --  Here, this function is dedicated to virtual bus and should be
   --  used when we map a port with a particular virtual bus as protocol.
   --  Parameter E and Containing_Component come from the instance
   --  model whereas parameter Virtual_Bus is a component from the AADL tree.

   function Map_Port_Var_Valid
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   function Map_Port
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   function Map_Port_Deployment_Destinations
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id;

   function Map_Port_Deployment_Partition (E : Node_Id) return Name_Id;

   function Map_Queue_Size (Port : Node_Id) return Node_Id;
   --  Return a node that represents the required size of the port (in bytes).

   function Map_Queue_Size_With_Data (Port : Node_Id) return Node_Id;
   --  Return a node that represents the required size of the port (in bytes).

   function Map_Time_To_Millisecond (T : Time_Type) return Node_Id;
   --  Return a amount of millisecond from a time

   function Map_Time_To_Nanosecond (T : Time_Type) return Node_Id;
   --  Return a amount of nanosecond from a time

   function Map_Simulink_Var (Corresponding_Feature : Node_Id) return Node_Id;
   --  Map simulink variable name

   function Map_Simulink_Header (Subprogram : Node_Id) return Node_Id;
   --  Map the header that must be included to bind simulink subprograms

   function Map_Simulink_Model_Type (Subprogram : Node_Id) return Node_Id;
   --  Map the type of model that should be used in the simulink application
   --  code.

   function Map_Simulink_Init_Func (Subprogram : Node_Id) return Node_Id;
   --  Map the name of the initialization function that creates all
   --  resources in the applicaton code.

   function Map_Scade_Function_Name (Subprogram : Node_Id) return Node_Id;
   --  Map the name of the SCADE function.

   function Map_Scade_Struct_Out (Subprogram : Node_Id) return Node_Id;
   --  Map the name of the struct used to store arguments of SCADE functions.

   function Map_Scade_Struct_In (Subprogram : Node_Id) return Node_Id;
   --  Map the name of the struct used to store arguments of SCADE functions.

   function Map_Scade_Parameter (Parameter : Node_Id) return Node_Id;
   --  Map a SCADE parameter

   function Map_Lustre_Output_Function_Name
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id;
   --  Generate the name of the output function for the context to use for
   --  a given Lustre node and a given port.

   function Map_Lustre_Input_Function_Name
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id;
   --  Generate the name of the input function for the context to use for
   --  a given Lustre node and a given port.

   function Map_Lustre_Step_Name (Subprogram : Node_Id) return Node_Id;
   --  Generate the name of the step function for the context to use for
   --  a given Lustre node.

   function Map_Lustre_Context_Name (Subprogram : Node_Id) return Node_Id;
   --  Map the name of the context variable for a given lustre subprogram.

   function Map_Source_Name (Subprogram : Node_Id) return Node_Id;
   --  A generic function that map the property Source_Name to an identifier.

   function Map_Lustre_Context_Init (Subprogram : Node_Id) return Node_Id;
   --  Generate the name of the init function for the context to use for
   --  a given Lustre node.

   function Map_Lustre_Context_Reset (Subprogram : Node_Id) return Node_Id;
   --  Generate the name of the reset function for the context to use for
   --  a given Lustre node.

   function Map_Lustre_Context_Type (Subprogram : Node_Id) return Node_Id;
   --  Generate the name of the type for the context to use for
   --  a given Lustre node.

   function Map_Lustre_Temp_Var
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id;
   --  When we use the Lustre academic version, we need to make a temporary
   --  variable between the thread and other generated functions.
   --  This function generates the name of the temporary variable.

   function Map_POK_Action
     (Action              : Supported_POK_Action;
      Thread_Id           : Unsigned_Long_Long := 0;
      Corresponding_Error : Node_Id            := No_Node) return Node_Id;
   --  For a given recovery action, map it and make the right
   --  function call to recover the error. This function is
   --  dedicated to threads. There is another function
   --  that maps recovery actions for the kernel or the
   --  partitions.

   function Map_POK_Kernel_Action
     (Action       : Supported_POK_Action;
      Partition_Id : Unsigned_Long_Long := 0;
      Kernel_Level : Boolean            := True) return Node_Id;

   function Map_POK_Kernel_Action
     (Action       : Supported_ARINC653_Action;
      Partition_Id : Unsigned_Long_Long := 0;
      Kernel_Level : Boolean            := True) return Node_Id;

   function Map_POK_Action
     (Action              : Supported_ARINC653_Action;
      Thread_Id           : Unsigned_Long_Long := 0;
      Corresponding_Error : Node_Id            := No_Node) return Node_Id;

   function Map_Esterel_Output_Function
     (Subprogram : Node_Id;
      Feature    : Node_Id) return Node_Id;
   --  Generate the name of the function
   --  used to store out signals.

   function Map_Esterel_Input_Function
     (Subprogram : Node_Id;
      Feature    : Node_Id) return Node_Id;
   --  Map the name of a given function to transmit
   --  the input signals.

   function Map_Esterel_Temp_Var
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id;
   --  Map a temporary variable to store output signals.

   function Map_Esterel_Reset_Function (Subprogram : Node_Id) return Node_Id;
   --  Map the reset function used in the Esterel application
   --  code. This function should be called before any reaction
   --  of the Esterel application code.

   function Map_Node_Name (Processor : Node_Id) return Name_Id;
   --  Return the used name for a node in POK.

   function Map_Bus_Name (Bus : Node_Id) return Name_Id;

   function Map_Needs_Macro (Name : Name_Id) return Node_Id;

   function Map_Associated_Locking_Entity_Name (E : Node_Id) return Name_Id;

   function Map_ARINC653_Error
     (Error : Supported_ARINC653_Error) return Node_Id;
   --  Map an ARINC653 error property into a runtime entity.

   function Map_POK_Error (Error : Supported_POK_Error) return Node_Id;
   --  Map a POK error property into a runtime entity.

   function Map_Device_Function_Read (Device : Node_Id) return Name_Id;
   --  Gives the read function of a device

   function Map_Device_Function_Write (Device : Node_Id) return Name_Id;
   --  Gives the write function of a device

   type Virtual_Bus_Call_Kind is (Sending, Receiving);

   procedure Map_Virtual_Bus_Calls
     (Port                 :     Node_Id;
      Declarations         :     List_Id;
      Statements           :     List_Id;
      Handled_Kind         :     Virtual_Bus_Call_Kind;
      New_Data             : out Node_Id;
      New_Size             : out Node_Id;
      Containing_Component :     Node_Id := No_Node);
   --  Makes calls to the protocol stack designed by a port.

   function Get_Type_Identifier_Associated_With_Virtual_Bus
     (Port : Node_Id) return Node_Id;
   --  Returns the type identifier that should be used with a port
   --  if this port is associated to virtual bus layers.

   function Map_Port_Name_For_Asn1 (E : Node_Id) return Name_Id;
   --  Map the name of a port for ASN1 marshalling.

   function Map_Port_Name_Present_For_Asn1 (E : Node_Id) return Name_Id;
   --  Map the name of a port for ASN1 marshalling.

   procedure Handle_Virtual_Buses_Properties (Port : Node_Id);
   --  Add properties relevant to virtual buses in the current C file.
   --  It declares the following macros in deployment.h:
   --  * POK_PROTOCOLS_DES_KEY
   --  * POK_PROTOCOLS_DES_INIT
   --  * POK_PROTOCOLS_BLOWFISH_KEY
   --  * POK_PROTOCOLS_BLOWFISH_INIT
   --  * POK_NEEDS_PROTOCOLS_CEASAR
   --  * POK_NEEDS_PROTOCOLS_BLOWFISH
   --  * POK_NEEDS_PROTOCOLS_DES

   function Map_Devices_Buses_Array_Name (E : Node_Id) return Name_Id;
   --  Provide function to create the name of the array that contains
   --  all accessed buses for a specific device.

   function Map_Device_Confvar_Name (E : Node_Id) return Name_Id;
   --  Map the device identifier name into a variable name
   --  that is supposed to contain the device configuration.

   function Map_ASN_Type (ASN_Name : Name_Id) return Name_Id;
   --  Map the name of an ASN.1 type into the C mapping.

   function Map_Thread_Port_Variable_Name (E : Node_Id) return Name_Id;
   --  Map the thread identifier name into a variable name
   --  that is used in port sending or reading.

end Ocarina.Backends.C_Common.Mapping;
