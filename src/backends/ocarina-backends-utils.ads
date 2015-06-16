------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               O C A R I N A . B A C K E N D S . U T I L S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--    Copyright (C) 2005-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

package Ocarina.Backends.Utils is

   type Browsing_Kind is (By_Source, By_Destination, Default);

   --------------------------
   -- Directory Operations --
   --------------------------

   procedure Create_Directory (Dir_Full_Name : Name_Id);
   --  Trys to create a directory and handle some creation errors

   procedure Enter_Directory (Dirname : Name_Id);
   --  Change the current directory to Dirname, and saves the
   --  old current_directory value.

   procedure Leave_Directory;
   --  Leave the latest entered directory and change to the latest
   --  left directory.

   function Add_Directory_Separator (Path : Name_Id) return Name_Id;
   --  If there is no directory separator at the end of the path,
   --  then add it and return the result. Else, return the same
   --  string.

   function Remove_Directory_Separator (Path : Name_Id) return Name_Id;
   --  If there is a directory separator at the end of the path, then
   --  remove it and return the result. Else, return the same string.

   ----------
   -- Misc --
   ----------

   function Normalize_Name
     (Name      : Name_Id;
      Ada_Style : Boolean := False) return Name_Id;
   --  Rewrite Name so that it only contains characters that are legal
   --  in most languages: a-z, 0-9, _.
   --  If Ada_Style is set to True, '.' is replaced by "__"

   function Fully_Qualified_Instance_Name (E : Node_Id) return Name_Id;
   --  Return a fully qualified name for component instance E, the
   --  fully qualified name is built from
   --  <system_name>_<process_name>_<thread_name> to avoid name
   --  collision

   -----------------------------------------------------
   -- Routines that are common to the code generators --
   -----------------------------------------------------

   function Is_Namespace (N : Node_Id) return Boolean;
   --  Return True if the node N corresponds to a namespace instance
   --  node.

   function Is_Delayed (E : Node_Id) return Boolean;
   --  Return True if the in data port instance E is involved in a
   --  delayed AADL connection.

   function Has_In_Parameters (E : Node_Id) return Boolean;
   --  Return True IFF the subprogram instance E contains at least one
   --  IN or IN OUT parameter.

   function Has_Out_Parameters (E : Node_Id) return Boolean;
   --  Return True IFF the subprogram instance E contains at least one
   --  OUT or IN OUT parameter.

   function Has_In_Ports (E : Node_Id) return Boolean;
   --  Return True IFF the component instance E contains at least one
   --  IN or IN OUT port.

   function Has_In_Event_Ports (E : Node_Id) return Boolean;
   --  Return True IFF the component instance E contains at least one
   --  IN or IN OUT event [data] port.

   function Has_Out_Ports (E : Node_Id) return Boolean;
   --  Return True IFF the component instance E contains at least one
   --  OUT or IN OUT port.

   function Has_Out_Event_Ports (E : Node_Id) return Boolean;
   --  Return True IFF the component instance E contains at least one
   --  OUT or IN OUT event [data] port.

   function Has_Ports (E : Node_Id) return Boolean;
   --  Return True IFF the component instance E contains at least one
   --  port.

   function Has_Output_Ports (E : Node_Id) return Boolean;
   --  Return True IFF the component instance E contains at least one
   --  output port.

   function Has_Input_Ports (E : Node_Id) return Boolean;
   --  Return True IFF the component instance E contains at least one
   --  input port.

   function Has_Modes (E : Node_Id) return Boolean;
   --  Return True IFF the entity instance E has AADL operational
   --  modes.

   function Get_Source_Ports (P : Node_Id) return List_Id;
   --  Return a node container list of all the THREAD OUT ports that
   --  are endpoint sources of the IN port P. If the connection path
   --  between P and one of its sources involves a connection
   --  bound to a bus instance, the bus instance will be the
   --  Extra_Item of the node container corresponding to the
   --  destination.

   function Get_Destination_Ports
     (P             : Node_Id;
      Custom_Parent : Node_Id := No_Node) return List_Id;
   --  Return a node container list of all the THREAD IN ports that
   --  are endpoint sources of the OUT port P. If the connection path
   --  between P and one of its destinations involves a connection
   --  bound to a bus instance, the bus instance will be the
   --  Extra_Item of the node container corresponding to the
   --  destination.
   --  Note: the list may be empty in the case of an event out port
   --  connected to a port that triggers a mode change.

   function Get_Actual_Owner (Spg_Call : Node_Id) return Node_Id;
   --  Return the data *subcomponent* whose corresponding component
   --  has the corresponding subprogram of Spg_Call as a
   --  feature. No_Node is returned id Spg_Call is not a method call
   --  or if its not connected to the data access provider.

   function Get_Container_Process (E : Node_Id) return Node_Id;
   --  Return the process subcomponent instance that contains the
   --  subprogram call or thread instance E, return No_Node if it
   --  cannot be determined.

   function Get_Container_Thread (E : Node_Id) return Node_Id;
   --  Return the thread subcomponent instance that contains the
   --  subprogram call E.

   function Get_Device_Of_Process
     (Bus     : Node_Id;
      Process : Node_Id) return Node_Id;
   --  Return the device the allows access to Process through Bus

   function Is_Connected (Bus : Node_Id; Device : Node_Id) return Boolean;
   --  Return true iff Device is used to interact with Bus

   type Comparison_Kind is (By_Name, By_Node);
   --  When we want to "handle" a node only once, the "once" criterion
   --  may be the name of the node (in which case two different nodes
   --  having the same name will be seen as the same node) or by node
   --  in which case two different nodes are seen as two different
   --  nodes.

   type Handling_Kind is
     (H_Ada_Activity_Interr_Body,
      H_Ada_Activity_Interr_Spec,
      H_Ada_Deployment_Spec,
      H_Ada_Helpers_Body,
      H_Ada_Helpers_Spec,
      H_Ada_Marshallers_Body,
      H_Ada_Marshallers_Spec,
      H_Ada_Namespaces_Body,
      H_Ada_Namespaces_Spec,
      H_Ada_Subprogram_Body,
      H_Ada_Subprogram_Spec,
      H_Ada_Type_Body,
      H_Ada_Type_Default_Value,
      H_Ada_Type_Spec,
      H_Add_Case_Alternative_Internals_Spec,
      H_Add_Case_Alternative_SSRA_Body,
      H_C_Marshall_Body,
      H_C_Marshall_Spec,
      H_C_Stub_Body,
      H_C_Stub_Spec,
      H_C_Deployment_Spec,
      H_C_Subprogram_Body,
      H_C_Subprogram_Spec,
      H_C_Type_Body,
      H_C_Type_Default_Value,
      H_C_Type_Spec,
      H_C_Request_Spec,
      H_C_Unmarshall_Body,
      H_C_Unmarshall_Spec,
      H_X_Virtual_Bus,
      H_PN_Cpn_Var,
      H_PN_Interconnection,
      H_PN_To_Delete,
      H_PN_Port_Creation,
      H_PN_Proc_Creation,
      H_RTSJ_Subprogram_Spec,
      H_RTSJ_Subprogram_Body,
      H_RTSJ_Type,
      H_RTSJ_Deployment);
   --  These are tags to precise the meaning of "handle"

   type Connection_Pattern_Kind is (Inter_Process, Intra_Process);

   procedure Set_Handling
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind;
      A          : Node_Id);
   --  Set the AADL node E as 'Handling'-handled by 'Comparison' and
   --  the handling result being the Ada node 'A'.

   function Get_Handling
     (E          : Node_Id;
      Comparison : Comparison_Kind;
      Handling   : Handling_Kind) return Node_Id;
   --  If the AADL node E has been already 'Handling'-handled by
   --  'Comparison', then return the Ada node which is the handling
   --  result. Otherwise, return No_Node.

   procedure Start_Recording_Handlings;
   --  After a call to this procedure. All the new registered handling
   --  will be appended to the internal handling repository.
   --  IMPORTANT NOTE: No consecutive calls to Start_Recording are
   --  allowed.

   procedure Stop_Recording_Handlings;
   --  No more Handlings will be added to the internal handling
   --  repository after the call to this procedure.

   procedure Reset_Handlings;
   --  All the handlings present in the internal handling repository
   --  are reset. The internal handling repository is DEALLOCATED then
   --  REINITIALIZED at the end of the call.

   function Bind_Two_Nodes (N_1 : Node_Id; N_2 : Node_Id) return Node_Id;
   --  Bind the couple of given nodes to a newly created node. The
   --  firts call of this function to a couple of nodes will create a
   --  new binding node. The further calls will return the node
   --  created at the first call.

   procedure Bind_Transport_API (P : Node_Id; T : Supported_Transport_APIs);
   --  Create a link between the process instance P and the transport layer T

   function Fetch_Transport_API (P : Node_Id) return Supported_Transport_APIs;
   --  Fetch a transport layer bound using 'Bind_Transport_API'

   function Map_Ada_Subprogram_Spec (S : Node_Id) return Node_Id;
   --  Create an Ada subprogram specification from the AADL subprogram
   --  instance S.

   function Map_Ada_Subprogram_Body (S : Node_Id) return Node_Id;
   --  Create an Ada subprogram body from the AADL subprogram instance
   --  S.

   function Map_Ada_Call_Seq_Subprogram_Spec
     (Spg : Node_Id;
      Seq : Node_Id) return Node_Id;
   --  Create the spec of the subprogram generated for a call sequence
   --  of a hybrid subprogram.

   function Map_Ada_Call_Seq_Subprogram_Body
     (Spg : Node_Id;
      Seq : Node_Id) return Node_Id;
   --  Create the body of the subprogram generated for a call sequence
   --  of a hybrid subprogram.

   function Map_Ada_Call_Seq_Access (S : Node_Id) return Node_Id;
   --  Map an access type declaration for a subprogram mapped from a
   --  call sequence of the hybrid subprogram S.

   function Map_Ada_Subprogram_Status (S : Node_Id) return Node_Id;
   --  Create an Ada record definition corresponding to the status of
   --  a hybrid AADL subprogram instance.

   function Map_Ada_Data_Type_Designator (E : Node_Id) return Node_Id;
   --  Fetch the Ada type corresponding to the data component instance
   --  E.

   procedure Handle_Call_Sequence
     (Caller       : Node_Id;
      Caller_State : Node_Id;
      Call_Seq     : Node_Id;
      Declarations : List_Id;
      Statements   : List_Id);
   --  This procedure generate the Ada code corresponding to a call
   --  sequence of a thread or a subprogram. All local variable
   --  declarations will be generated in the Declarations list and all
   --  the subprogram calls will be generated in the Statements
   --  list. These two lists have to be initialized before the call to
   --  this subprogram. Caller_State is the "opaque" variable
   --  representing the internal state of the "Caller" instance. Its
   --  value depends on the nature of "Caller" (Thread,
   --  Subprogram). We prefer to give it to the subprogram to keep it
   --  independant from the backend.

   function Get_Ada_Default_Value (D : Node_Id) return Node_Id;
   --  Return an Ada "default value" for the Ada type mapped from the
   --  data component instance D.

   function Map_Ada_Full_Parameter_Name
     (Spg    : Node_Id;
      P      : Node_Id;
      Suffix : Character := ASCII.NUL) return Name_Id;
   --  Maps a name by concatening the subprogram instance full Spg
   --  name to the parameter instance P name (separated by '_') and
   --  verifying that the resulting name is an Ada-valid identifier
   --  name.

   function Map_Ada_Full_Feature_Name
     (E      : Node_Id;
      Suffix : Character := ASCII.NUL) return Name_Id;
   --  Maps a name by concatening the full name of the feature
   --  instance E and to Suffix (separated by '_') and verifying that
   --  the resulting name is an Ada-valid identifier name. Any root
   --  system name will be troncated.

   function Map_Ada_Enumerator_Name
     (E      : Node_Id;
      Server : Boolean := False) return Name_Id;
   --  Maps an Ada-valid enumerator name from the component or
   --  subcomponent instance E. If Server is True, appends the _Server
   --  suffix to the enumerator identifier.

   function Map_Ada_Defining_Identifier
     (A      : Node_Id;
      Suffix : String := "") return Node_Id;
   function Map_Ada_Defining_Identifier
     (A      : Node_Id;
      Suffix : String := "") return Name_Id;
   --  Maps an Ada-valid defining identifier form the name of the
   --  given AADL entity. If suffix is not nul, concat it to the
   --  obtained name and separate them by a '_'.

   function Map_Ada_Component_Name (F : Node_Id) return Name_Id;
   --  Maps a name corresponding to a component corresponding to an
   --  [event] data port in a thread interface.

   function Map_Ada_Protected_Aggregate_Identifier
     (S : Node_Id;
      A : Node_Id) return Node_Id;
   --  Maps an identifier corresponding to the subcomponent A of the
   --  required data S.

   function Map_Ada_Default_Value_Identifier (D : Node_Id) return Node_Id;
   --  Maps an identifier for the default value variable corresponding
   --  to the AADL data component instance D.

   function Map_Ada_Package_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the package instantiation corresponding
   --  to IN port or Bounded string data type E

   function Map_Ada_Subprogram_Identifier (E : Node_Id) return Node_Id;
   function Map_Ada_Subprogram_Identifier (N : Name_Id) return Node_Id;
   --  Return a node that designate the implementation subprogram
   --  corresponding to the entity instance E. E is either a thread,
   --  subprogram or a port instance. If a 'with' clause is necessary,
   --  this subprogram add it.

   function Map_Ada_Namespace_Defining_Identifier
     (N      : Node_Id;
      Prefix : String := "") return Node_Id;
   --  Creates an Ada defining identifier corresponding to the
   --  namespace instance E. The parent unit names of the Identifier
   --  are correctly placed depending on the namespace hierarchy. If
   --  'Prefix' is specified, then all the full name is prefixed by
   --  it.

   function To_Bytes (S : Size_Type) return Unsigned_Long_Long;
   --  Converts the given size S into bytes and return it. If the size
   --  cannot be converted into byte, then return 0.

   function To_Bits (S : Size_Type) return Unsigned_Long_Long;
   --  Converts the given size S into bits and return it

   procedure Check_Connection_Consistency (C : Node_Id);
   --  Perform some legality rules check on the connection instance S
   --  and raises an error if one of these rules is broken. See the
   --  comments in the body for more detail on the checked rules.

   procedure Check_Thread_Consistency (T : Node_Id);
   --  Check the thread consistency and raises an error if something
   --  is wrong (Unknown implementation kind).

   function Get_Subcomponent_Access_Source (S : Node_Id) return Node_Id;
   --  Return the source subcomponent of the required subprogram
   --  access S. Raises an error if S is not connected to any source.

   function Get_First_Processor (P : Node_Id) return Node_Id;
   --  Return the first processor of an AADL model

   function Get_Connection_Pattern
     (E : Node_Id) return Connection_Pattern_Kind;
   --  Return the connection pattern for a port

   function "<=" (T1 : Time_Type; T2 : Time_Type) return Boolean;
   --  Compare two times. Returns true if the first value is less than
   --  the second value.

   function To_Seconds (S : Time_Type) return Long_Double;
   function To_Milliseconds (S : Time_Type) return Unsigned_Long_Long;
   function To_Nanoseconds (S : Time_Type) return Unsigned_Long_Long;

   function Get_Accessed_Data (Data_Access : Node_Id) return Node_Id;
   --  Get accessed data when a thread or subprogram has a data access.

   function Is_Protected_Data (Data_Component : Node_Id) return Boolean;
   --  Indicate if a data should be protected by mutexes/semaphores.

   function Get_Port_By_Name
     (Port      : Node_Id;
      Component : Node_Id) return Node_Id;
   --  Returns a port in Component parameter that has the same
   --  name of the Port parameter.

   function Is_Virtual (Port : Node_Id) return Boolean;
   --  Returns True is the port is virtual. Used by the POK
   --  backend only.

   function Get_Associated_Virtual_Buses (Port : Node_Id) return List_Id;
   --  Returns a list that contains virtual bus components bound
   --  to a port. Returns elements that are stored in the DECLARATIVE model
   --  and takes a port that is stored in the INSTANCE model.

   function Look_For_Property_In_Declarative
     (Component     : Node_Id;
      Property_Name : String) return Node_Id;
   --  Look_For_Property_In_Declarative is just a function that
   --  browses a component and its inherited/refined components
   --  and look for a particular property. It is just a function
   --  that is used to bypass bugs on properties in the instance model.

   function Look_For_Subcomponent_In_Declarative
     (Component         : Node_Id;
      Subcomponent_Name : String) return Node_Id;
   --  Look_For_Subcomponent_In_Declarative search a subcomponent
   --  named like Subcomponent_Name in Component. It returns a Node_Id
   --  with the subcomponent if it exists or return No_Node if it fails.
   --  This function is more a workaround to handle errors in the
   --  instance model.

   function Is_Pure_Device_Port (Port : Node_Id) return Boolean;
   --  Is_Pure_Device_Port returns True if the port is
   --  a pure device port. A pure device port is a port located
   --  in a device that communicates with another device.
   --  Port is a node in the instance model.

   function Get_Instance_Type_Associated_With_Virtual_Bus
     (Port : Node_Id) return Node_Id;
   --  Returns the instance component of the data associated with
   --  a virtual bus.

   function Is_Using_Virtual_Bus (Port : Node_Id) return Boolean;
   --  Indicate is an inter-partition port should use
   --  virtual bus mechanisms, browsing the components hierarchy.

   function Get_Corresponding_Port_In_Component
     (Port : Node_Id) return Node_Id;
   --  Return the port connected the port given in parameter
   --  inside the component.

   function Process_Use_Defaults_Sockets
     (The_Process : Node_Id) return Boolean;

   function Get_Associated_Bus (Port : Node_Id) return Node_Id;

   function Find_Associated_Process (Runtime       : Node_Id;
                                     Root_Node     : Node_Id := No_Node)
                                     return Node_Id;

   function Get_Partition_Runtime (Process    : Node_Id;
                                   Root_Node  : Node_Id := No_Node)
                                     return Node_Id;

   function Get_Root_Component (C : Node_Id)
                               return Node_Id;

end Ocarina.Backends.Utils;
