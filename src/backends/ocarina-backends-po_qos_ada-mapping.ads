------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A . M A P P I N G   --
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

--  This package contains routines to map entities from the AADL
--  instance tree into entities of the Ada tree.

with Ocarina.Backends.Properties;

package Ocarina.Backends.PO_QoS_Ada.Mapping is

   use Ocarina.Backends.Properties;

   --  The routines below map AADL entities into API dependant
   --  entities.

   function Map_Distributed_Application (E : Node_Id) return Node_Id;
   --  Create a PolyORB-QoS Ada distributed application node mapped
   --  from the AADL architecture instance node E.

   function Map_QoS_Node (E : Node_Id) return Node_Id;
   --  Create a PolyORB-QoS Ada node of a distributed application

   function Map_QoS_Unit
     (E : Node_Id;
      F : Node_Id := No_Node)
     return Node_Id;
   --  Create the node that contain all the Ada units generated for a
   --  particular node of the distributed application.

   function Map_TC_Variable_Identifier (E : Node_Id) return Node_Id;
   --  Maps a TypeCode variable identifier corresponding to the data
   --  component instance E.

   function Map_Record_Field_Identifier (S : Node_Id) return Node_Id;
   --  Maps a record field identifier from the data subcomponent E

   function Map_Initialize_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the initialize routine of the data
   --  component E

   function Map_Initialized_Flag_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the boolean flag corresponding to the
   --  Data component E.

   function Map_Package_Instantiation_Designator (E : Node_Id) return Node_Id;
   --  Returns a designator for the instantiated package corresponding
   --  to the node E.

   function Map_Dependency (Dep : Node_Id) return Node_Id;
   --  Deduces a dependency string from a package name

   function Map_Object_Type_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the servant corresponding to thread
   --  E.

   function Map_Reference_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the reference corresponding to the
   --  destination port E.

   function Map_Thread_Controller_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the subprogram that performs the `job'
   --  of thread E.

   function Map_Ada_Time (T : Time_Type) return Node_Id;
   --  Create the call from the corresponding Ada real time routines
   --  to create an Ada time span that corresponds to the given time
   --  T. If an error occurs (for example if the given time is too
   --  small to be generated by Ada or the used unit is too fine
   --  grained (picoseconds), return No_Node.

   function Map_Ada_Priority (P : Unsigned_Long_Long) return Node_Id;
   --  Maps the given AADL priority into an Ada priority

   function Map_Buffer_Instance_Identifier (P : Node_Id) return Node_Id;
   --  Maps an identifier for a buffer instance corresponding to an
   --  event [data] port P.

   function Map_Variable_Identifier (P : Node_Id) return Node_Id;
   --  Maps an identifier for a local variable corresponding to port P

   function Map_Mutex_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the mutex generated for thead E

   function Map_Port_Argument_Identifier (E : Node_Id) return Node_Id;
   function Map_Port_Boolean_Identifier (E : Node_Id) return Node_Id;
   --  Mapping for entities to handle connections between threads and
   --  subprograms.

   function Map_Get_Subprogram_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the Get_<...> subprogram generated for
   --  port or a proptected data E.

   function Map_Put_Subprogram_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for the Put_<...> subprogram generated for
   --  port E.

   function Map_Push_Back_Subprogram_Identifier (E : Node_Id) return Node_Id;
   --  Maps an identifier for Push_back_<...> subprogram generated for
   --  port E.

   function Must_Have_Port_Number (E : Node_Id) return Boolean;
   --  Return True iff the process instance E must have been assigned
   --  a port number. A process that must have a port number is a
   --  process that has IN ports or a process that has threads with IN
   --  ports. The second case is due tp the fact that inter-thread
   --  communication is handled using PolyORB-QoS refernces.

   procedure Compute_Servant_Index (T : Node_Id);
   --  Computes a new index for the servant corresponding to the
   --  thread T and set it as the name_table_info of a conventional
   --  name_id. The incrementation of the index is done for thread in
   --  the same process.

   function Get_Servant_Index (T : Node_Id) return Nat;
   --  Deduce the conventional name_id from the thread T
   --  names, gets the name_table_info of the obtained name_id and
   --  returns it.

   procedure Bind_Ada_Identifier_To_Package (I : Node_Id; P : Node_Id);
   --  Links the Ada Identifier I to the Package declaration P. The
   --  binding is performed 'by name'.

   function Get_Bound_Package (I : Node_Id) return Node_Id;
   --  Return the package declaration bound to the Ada identifier
   --  I. Returns No_Node if no package has been bound to I, I is
   --  No_Node or the name of I is No_Name.

   --  The Bind_AADL_To_<..> subprograms creates bindings between a
   --  node from the AADL instance tree and a <..> node.

   procedure Bind_AADL_To_Setup (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Parameters (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Obj_Adapters (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Helpers (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Main (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Servants (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Namespaces (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_TypeCode (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Execute_Servant (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_From_Any (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Initialize (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Reference (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Subprogram (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Feature_Subprogram (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Set (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Build (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_To_Any (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Thread_Controller (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Type_Definition (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Put (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Push_Back (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Get (G : Node_Id; A : Node_Id);
   procedure Bind_AADL_To_Package (G : Node_Id; A : Node_Id);

end Ocarina.Backends.PO_QoS_Ada.Mapping;
