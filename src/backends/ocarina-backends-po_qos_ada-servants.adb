------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.PO_QOS_ADA.SERVANTS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
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
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Namet;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_QoS_Ada.Mapping;
with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_QoS_Ada.Servants is

   use Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.PO_QoS_Ada.Mapping;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.Ada_Values;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);

      function Thread_Controller_Spec (E : Node_Id) return Node_Id;
      --  Makes a spec for the 'Thread_Name'_Controller subprogram

      function Object_Type_Declaration (T : Node_Id) return Node_Id;
      --  Makes the Object type declaration corresponding to a thread
      --  having IN ports.

      function Get_Spec (P : Node_Id) return Node_Id;
      function Put_Spec (P : Node_Id) return Node_Id;
      function Push_Back_Spec (P : Node_Id) return Node_Id;
      --  Make the specs of the port buffer manipulating subprograms

      procedure Protected_Object_Routines_Specs (P : Node_Id);
      --  Creates the specs of the routines that handle the Buffer of
      --  the port P in the spec of the protected object.

      function Reference_Declaration (E : Node_Id) return Node_Id;
      --  Makes the Reference declaration corresponding to a thread
      --  having IN ports or corresponding to a destination port

      function Execute_Servant_Spec (T : Node_Id) return Node_Id;
      --  Makes the spec of the Execute_Servant subprogram relative to
      --  an Object

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when K_Component_Instance =>
               Visit_Component_Instance (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------

      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category
           := Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id := ADN.Distributed_Application_Unit
           (ADN.Helpers_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         S : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);

         Set_Servants_Spec;

         --  Visit recursively all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (Ada_Root);

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         N : Node_Id;
         F : Node_Id;
         D : Node_Id;
      begin
         Set_Servants_Spec;

         N := Message_Comment
           ("Thread: " &
            Get_Name_String (AAU.Compute_Full_Name_Of_Instance (E)));
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  The thread is mapped to a parameterless subprogram which
         --  controls its execution.

         N := Thread_Controller_Spec (E);
         Bind_AADL_To_Thread_Controller (Identifier (E), N);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  Handle the thread features

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
                  --  For each destination of the OUT Port, we
                  --  generate a Reference to the destination.

                  D := First_Node (Get_Destination_Ports (F));

                  while Present (D) loop
                     N := Reference_Declaration (Item (D));
                     Append_Node_To_List
                       (N, ADN.Visible_Part (Current_Package));

                     D := Next_Node (D);
                  end loop;
               elsif Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
                  --  Routines of the protected object that handles
                  --  the port buffer.

                  Protected_Object_Routines_Specs (F);
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Create the protected object and the Servant routines when
         --  the thread has IN ports.

         if Has_In_Ports (E) then
            --  IMPORTANT: If the node contains more that 2 thread
            --  that have IN ports, the other nodes that send messages
            --  must know the index of the destination thread in the
            --  Ocarina Object Adapter static table.

            Compute_Servant_Index (E);

            N := Object_Type_Declaration (E);
            Bind_AADL_To_Type_Definition (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Reference_Declaration (E);
            Bind_AADL_To_Reference (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Execute_Servant_Spec (E);
            Bind_AADL_To_Execute_Servant (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;
      end Visit_Thread_Instance;

      ------------------------
      -- Object_Declaration --
      ------------------------

      function Object_Type_Declaration (T : Node_Id) return Node_Id is
         O_Identifier : constant Node_Id := Map_Object_Type_Identifier (T);
         O_Type_Spec  : constant Node_Id := RE (RE_Servant);
         N            : Node_Id;
      begin
         N := Make_Full_Type_Declaration
           (Defining_Identifier => O_Identifier,
            Type_Definition     => Make_Derived_Type_Definition
              (Subtype_Indication    => O_Type_Spec,
               Record_Extension_Part => Make_Record_Definition (No_List)));
         return N;
      end Object_Type_Declaration;

      ---------------------------
      -- Reference_Declaration --
      ---------------------------

      function Reference_Declaration (E : Node_Id) return Node_Id is
         R_Identifier : constant Node_Id := Map_Reference_Identifier (E);
         R_Type_Spec  : constant Node_Id := RE (RE_Ref_3);
         N            : Node_Id;
      begin
         N := Make_Object_Declaration
           (Defining_Identifier => R_Identifier,
            Object_Definition   => R_Type_Spec);
         return N;
      end Reference_Declaration;

      --------------------------
      -- Execute_Servant_Spec --
      --------------------------

      function Execute_Servant_Spec (T : Node_Id) return Node_Id is
         S_Identifier : constant Node_Id := Make_Defining_Identifier
           (SN (S_Execute_Servant));
         O_Designator : constant Node_Id := Map_Object_Type_Identifier (T);
         N            : Node_Id;
         Param_List   : constant List_Id := New_List (ADN.K_Parameter_Profile);
      begin
         --  First parameter

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Obj)),
            Subtype_Mark        => Make_Access_Type_Definition
              (O_Designator, Is_Not_Null => True));
         Append_Node_To_List (N, Param_List);

         --  Second Parameter

         N := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Msg)),
            Subtype_Mark        => Make_Attribute_Designator
              (RE (RE_Message), A_Class));
         Append_Node_To_List (N, Param_List);

         N := Make_Subprogram_Specification
           (Defining_Identifier => S_Identifier,
            Parameter_Profile   => Param_List,
            Return_Type         => Make_Attribute_Designator
              (RE (RE_Message), A_Class));

         return N;
      end Execute_Servant_Spec;

      ----------------------------
      -- Thread_Controller_Spec --
      ----------------------------

      function Thread_Controller_Spec (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => Map_Thread_Controller_Identifier (E),
            Parameter_Profile   => No_List,
            Return_Type         => No_Node);
         return N;
      end Thread_Controller_Spec;

      -------------------------------------
      -- Protected_Object_Routines_Specs --
      -------------------------------------

      procedure Protected_Object_Routines_Specs (P : Node_Id)
      is
         N : Node_Id;
      begin
         --  Visible Part: for each IN event data port, we declare:

         --  1) A Put_<Port_Name> procedure that puts its argument in
         --     the Port buffer

         N := Put_Spec (P);
         Bind_AADL_To_Put (Identifier (P), N);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  2) A Get_<Port_Name> procedure that puts its argument in
         --     the Port buffer

         N := Get_Spec (P);
         Bind_AADL_To_Get (Identifier (P), N);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  3) A Push_Back_<Port_Name> procedure that puts its argument in
         --     the Port buffer when the user wants it. This is needed
         --     only for event data ports.

         if Is_Event (P) then
            N := Push_Back_Spec (P);
            Bind_AADL_To_Push_Back (Identifier (P), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;
      end Protected_Object_Routines_Specs;

      --------------
      -- Put_Spec --
      --------------

      function Put_Spec (P : Node_Id) return Node_Id is
         S_Identifier : constant Node_Id := Map_Put_Subprogram_Identifier (P);
         P_Identifier : constant Node_Id := Map_Ada_Defining_Identifier (P);
         N            : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => S_Identifier,
            Parameter_Profile   => Make_List_Id
              (Make_Parameter_Specification
               (P_Identifier,
                Map_Ada_Data_Type_Designator (Corresponding_Instance (P)))),
            Return_Type         => No_Node);
         ADN.Set_Parent (N, No_Node);
         return N;
      end Put_Spec;

      --------------------
      -- Push_Back_Spec --
      --------------------

      function Push_Back_Spec (P : Node_Id) return Node_Id
      is
         S_Identifier : constant Node_Id
           := Map_Push_Back_Subprogram_Identifier (P);
         P_Identifier : constant Node_Id := Map_Ada_Defining_Identifier (P);
         N            : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => S_Identifier,
            Parameter_Profile   => Make_List_Id
            (Make_Parameter_Specification
               (P_Identifier,
                Map_Ada_Data_Type_Designator (Corresponding_Instance (P)))),
            Return_Type         => No_Node);
         ADN.Set_Parent (N, No_Node);
         return N;
      end Push_Back_Spec;

      --------------
      -- Get_Spec --
      --------------

      function Get_Spec (P : Node_Id) return Node_Id
      is
         S_Identifier : constant Node_Id := Map_Get_Subprogram_Identifier (P);
         P_Identifier : constant Node_Id := Map_Ada_Defining_Identifier (P);
         B_Identifier : constant Node_Id := Map_Port_Boolean_Identifier (P);
         Profile      : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N            : Node_Id;
      begin
         N := Make_Parameter_Specification
           (P_Identifier,
            Map_Ada_Data_Type_Designator (Corresponding_Instance (P)),
            Mode_Out);
         Append_Node_To_List (N, Profile);

         --  If the port is an event data port add a boolean flag

         if Is_Event (P) then
            N := Make_Parameter_Specification
              (B_Identifier,
               RE (RE_Boolean_2),
               Mode_Out);
            Append_Node_To_List (N, Profile);
         end if;

         N := Make_Subprogram_Specification
           (Defining_Identifier => S_Identifier,
            Parameter_Profile   => Profile,
            Return_Type         => No_Node);
         ADN.Set_Parent (N, No_Node);
         return N;
      end Get_Spec;
   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);

      Thread_Mutex : Name_Id;
      --  A reference on current thread's mutex name

      Initialize_Statements : List_Id := No_List;
      --  The statements of the Initialize procedure

      function Thread_Controller_Body (E : Node_Id) return Node_Id;
      --  Makes a body for the 'Thread_Name'_Controller subprogram

      procedure Protected_Object_Routines_Bodies (P : Node_Id);
      --  Creates the bodies of the routines that handle the Buffer of
      --  the port P in the body of the protected object.

      function Put_Body (P : Node_Id) return Node_Id;
      function Get_Body (P : Node_Id) return Node_Id;
      function Push_Back_Body (P : Node_Id) return Node_Id;
      --  Make the bodies of the port buffer manipulating subprograms

      function Execute_Servant_Body (T : Node_Id) return Node_Id;
      --  Makes the body of the Execute_Servant subprogram relative to
      --  an Object

      function Request_Handling (P : Node_Id; T : Node_Id) return Node_Id;
      --  Makes the request handling portion corresponding to the IN
      --  port P of the thread T

      function Buffer_Instance_Declaration (P : Node_Id) return Node_Id;
      --  Makes the buffer instance declaration for an event data port

      function Protected_Variable_Declaration (P : Node_Id) return Node_Id;
      --  Makes the variable declaration for a data port

      function Buffer_Package_Instantiation (P : Node_Id)
        return Node_Id;
      --  Make the package instantiation of a buffer corresponding to
      --  the FIFO of the IN port P.

      function Call_Subprogram
        (S : Node_Id;
         T : Node_Id;
         L : List_Id)
        return Node_Id;
      --  Makes a call to subprogram_call S which belongs the a call
      --  sequence in the thread T. All necessary local variables are
      --  added to the given list.

      function Servant_Initialization return Node_Id;
      --  Initialization routines of the Servants package

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when K_Component_Instance =>
               Visit_Component_Instance (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------

      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category
           := Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         N                   : Node_Id;
         Data_Representation : constant Supported_Data_Representation
           := Get_Data_Representation (E);
      begin
         Set_Servants_Body;

         --  This is a shared variable

         --  Declare the global variable corresponding to the shared
         --  variable.

         N := Make_Object_Declaration
           (Defining_Identifier => Map_Ada_Defining_Identifier
              (Parent_Subcomponent (E)),
            Object_Definition   => Map_Ada_Data_Type_Designator (E));
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  If the shared variable is protected, add the routine that
         --  initializes it to the Initialize procedure statements.

         if (Data_Representation = Data_With_Accessors or else
             Data_Representation = Data_Struct) and then
           Get_Concurrency_Protocol (E) = Concurrency_Protected_Access
         then
            N := Make_Subprogram_Call
              (Extract_Designator
               (ADN.Build_Node
                (Backend_Node
                 (Identifier
                  (E)))),
               Make_List_Id
               (Map_Ada_Defining_Identifier
                (Parent_Subcomponent
                 (E))));
            Append_Node_To_List (N, Initialize_Statements);
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id := ADN.Distributed_Application_Unit
           (ADN.Helpers_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         N : Node_Id;
         S : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);

         Set_Servants_Body;

         --  Reset the Initialize_Statements list

         Initialize_Statements := New_List (ADN.K_Statement_List);

         --  Visit all the data subcomponents of the process, since
         --  data have to be declared before their use.

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               if AAU.Is_Data (Corresponding_Instance (S)) then
                  Visit (Corresponding_Instance (S));
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  Visit recursively all the threads

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               if AAU.Is_Thread (Corresponding_Instance (S)) then
                  Visit (Corresponding_Instance (S));
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  Create the Initialize procedure when necessary

         if not Is_Empty (Initialize_Statements) then
            --  The spec

            N := Make_Subprogram_Specification
              (Make_Defining_Identifier (SN (S_Initialize)), No_List);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  The body

            N := Make_Subprogram_Implementation
              (N, No_List, Initialize_Statements);

            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  Servants package initialization

            N := Servant_Initialization;
            ADN.Set_Package_Initialization (Current_Package, Make_List_Id (N));
         end if;

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (Ada_Root);

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         N            : Node_Id;
         F            : Node_Id;
         Has_In_Ports : constant Boolean := Utils.Has_In_Ports (E);
      begin
         Set_Servants_Body;

         N := Message_Comment
           ("Thread: "
            & Get_Name_String (AAU.Compute_Full_Name_Of_Instance (E)));
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  If the thread has IN ports, begin by declaring a mutex

         if Has_In_Ports then
            N := Make_Object_Declaration
              (Defining_Identifier => Map_Mutex_Identifier (E),
               Object_Definition   => RE (RE_Mutex_Access));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            Thread_Mutex := ADN.Name (ADN.Defining_Identifier (N));
            --  Append mutex initialization to the Initialize
            --  procedure statements.

            N := Make_Subprogram_Call
              (RE (RE_Create_2),
               Make_List_Id
               (Map_Mutex_Identifier (E)));
            Append_Node_To_List (N, Initialize_Statements);
         end if;

         --  Handle the thread features

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
                  --  For each IN port, we declare:

                  if Is_Event (F) then

                     --  1) A buffer package that plays the role of
                     --     the port FIFO (EVENT ports only).

                     N := Buffer_Package_Instantiation (F);
                     Append_Node_To_List (N, ADN.Statements (Current_Package));

                     --  Link the feature to the table type
                     Bind_AADL_To_Package
                       (Identifier (F),
                        Make_Selected_Component
                        (Map_Ada_Package_Identifier (F),
                         Make_Defining_Identifier (TN (T_Table))));
                  end if;

                  --  2) Routines of the protected object that handles
                  --     the port buffer.

                  --  Private Part: for each port, we declare a buffer instance
                  --  if we deal with an event data port and a simple variable
                  --  if we deal with a data port

                  if Is_Event (F) then
                     N := Buffer_Instance_Declaration (F);
                  else
                     N := Protected_Variable_Declaration (F);
                  end if;

                  Append_Node_To_List (N, ADN.Statements (Current_Package));

                  Protected_Object_Routines_Bodies (F);

               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  The body of the thread controller subprogram

         N := Thread_Controller_Body (E);
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  If the thread has IN ports, end by generating the body of
         --  the Execute_Servant procedure.

         if Has_In_Ports then
            N := Execute_Servant_Body (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;
      end Visit_Thread_Instance;

      ----------------------------
      -- Thread_Controller_Body --
      ----------------------------

      function Thread_Controller_Body (E : Node_Id) return Node_Id is
         Spec             : constant Node_Id := ADN.Thread_Controller_Node
           (Backend_Node (Identifier (E)));
         Declarative_Part : constant List_Id := New_List
           (ADN.K_Declaration_List);
         Statements       :          List_Id := New_List
           (ADN.K_Statement_List);

         If_Condition     : Node_Id := RE (RE_True);
         Then_Statements  : constant List_Id := New_List
           (ADN.K_Statement_List);
         Else_Statements  : constant List_Id := New_List
           (ADN.K_Statement_List);

         Has_In_ED_Ports  : Boolean := False;

         N                : Node_Id;
         F                : Node_Id;
         C                : Node_Id;
         S                : Node_Id;
         P                : Node_Id;
         SD               : Node_Id;
         D                : Node_Id;
         Aggregate        : Node_Id;
      begin

         --  If the thread a a compute entrypoint, all we do is calling it

         if Get_Thread_Implementation_Kind (E)
           = Thread_With_Compute_Entrypoint
         then
            N := Make_Subprogram_Call
              (Map_Ada_Subprogram_Identifier (E),
               No_List);
            Append_Node_To_List (N, Then_Statements);
         else
            --  Read the shared variables

            if not AAU.Is_Empty (Features (E)) then
               F := First_Node (Features (E));

               while Present (F) loop
                  if Kind (F) = K_Subcomponent_Access_Instance then
                     --  The access to the subcomponent has to be REQUIRED

                     if Is_Provided (F) then
                        Display_Located_Error
                          (Loc (F),
                           "Thread providing access to a data not supported",
                           Fatal => True);
                     end if;

                     --  Get the source subcomponent of the data access

                     SD := Get_Subcomponent_Access_Source (F);
                     D  := Corresponding_Instance (SD);

                     if Get_Data_Representation (D) = Data_With_Accessors
                       or else Get_Data_Representation (D) = Data_Struct
                     then
                        Aggregate := First_Node (Subcomponents (D));

                        while Present (Aggregate) loop
                           --  Declare the local variable

                           N := Make_Object_Declaration
                             (Defining_Identifier =>
                                Map_Ada_Protected_Aggregate_Identifier
                                (F, Aggregate),
                              Object_Definition   =>
                                Map_Ada_Data_Type_Designator
                                (Corresponding_Instance (Aggregate)));
                           Append_Node_To_List (N, Declarative_Part);

                           --  Get the field value

                           if Get_Concurrency_Protocol (D) =
                             Concurrency_Protected_Access
                           then
                              --  For protected data with synchronous
                              --  update policy, we read the value of
                              --  each field.

                              N := Make_Subprogram_Call
                                (Extract_Designator
                                 (ADN.Get_Node
                                  (Backend_Node
                                   (Identifier (Aggregate)))),
                                 Make_List_Id
                                 (Map_Ada_Defining_Identifier (SD),
                                  Map_Ada_Protected_Aggregate_Identifier
                                  (F, Aggregate)));
                              Append_Node_To_List (N, Statements);
                           else
                              --  Non-protected shared object we
                              --  simply perform an assignment.

                              N := Make_Assignment_Statement
                                (Map_Ada_Protected_Aggregate_Identifier
                                 (F, Aggregate),
                                 Make_Selected_Component
                                 (Map_Ada_Defining_Identifier (SD),
                                  Map_Ada_Defining_Identifier (Aggregate)));
                              Append_Node_To_List (N, Statements);
                           end if;

                           Aggregate := Next_Node (Aggregate);
                        end loop;
                     end if;
                  end if;

                  F := Next_Node (F);
               end loop;
            end if;

            --  Handling the thread IN ports

            if not AAU.Is_Empty (Features (E)) then

               P := First_Node (Features (E));
               while Present (P) loop
                  if Kind (P) = K_Port_Spec_Instance
                    and then AAN.Is_Data (P)
                    and then Is_In (P) then

                     --  For pure data ports, if the thread has
                     --  compute entrypoints associted to its other IN
                     --  EVENT [DATA] ports, the extraction of the
                     --  received value is done by the user.

                     if AAN.Is_Event (P) or else
                       Get_Thread_Implementation_Kind (E)
                       /= Thread_With_Port_Compute_Entrypoint
                     then
                        --  Declare a local variable having the port type

                        N := Make_Object_Declaration
                          (Defining_Identifier =>
                             Map_Ada_Defining_Identifier (P),
                           Object_Definition   => Map_Ada_Data_Type_Designator
                             (Corresponding_Instance (P)));
                        Append_Node_To_List (N, Declarative_Part);
                     end if;

                     --  Handle data coherence in case of event data
                     --  ports.

                     if Is_Event (P) then
                        Has_In_ED_Ports := True;

                        --  The boolean flag corresponding to the port

                        N := Make_Object_Declaration
                          (Defining_Identifier =>
                             Map_Port_Boolean_Identifier (P),
                           Object_Definition   => RE (RE_Boolean_2));
                        Append_Node_To_List (N, Declarative_Part);

                        --  Get the port value from the corresponding
                        --  port buffer.

                        N := Extract_Designator
                          (ADN.Get_Node
                           (Backend_Node
                            (Identifier (P))));

                        N := Make_Subprogram_Call
                          (N, Make_List_Id
                           (Map_Ada_Defining_Identifier (P),
                            Map_Port_Boolean_Identifier (P)));
                        Append_Node_To_List (N, Statements);

                        --  Update the IF statement condition

                        If_Condition := Make_Expression
                          (If_Condition,
                           Op_And_Then,
                           Map_Port_Boolean_Identifier (P));

                        --  If the port has a compute entrypoint,
                        --  append a call to Then_Statement.

                        if Get_Port_Compute_Entrypoint (P) /= No_Name then
                           N := Make_Subprogram_Call
                             (Map_Ada_Subprogram_Identifier (P),
                              Make_List_Id
                              (Map_Ada_Defining_Identifier (P)));
                           Append_Node_To_List (N, Then_Statements);
                        end if;

                        --  Update the ELSE statement

                        N := Extract_Designator
                          (ADN.Push_Back_Node
                           (Backend_Node
                            (Identifier
                             (P))));
                        N := Make_Subprogram_Call
                          (N, Make_List_Id
                           (Map_Ada_Defining_Identifier (P)));
                        N := Make_If_Statement
                          (Condition       =>
                             Map_Port_Boolean_Identifier (P),
                           Then_Statements => Make_List_Id (N));
                        Append_Node_To_List (N, Else_Statements);
                     elsif Get_Thread_Implementation_Kind (E)
                       /= Thread_With_Port_Compute_Entrypoint
                     then
                        --  Get the port value from the
                        --  corresponding port buffer.

                        N := Extract_Designator
                          (ADN.Get_Node
                           (Backend_Node
                            (Identifier (P))));
                        N := Make_Subprogram_Call
                          (N,
                           Make_List_Id
                           (Map_Ada_Defining_Identifier (P)));
                        Append_Node_To_List (N, Statements);
                     end if;
                  end if;

                  P := Next_Node (P);
               end loop;
            end if;

            --  Call the subprograms in the thread call sequences

            if not AAU.Is_Empty (Calls (E)) then
               Check_Thread_Consistency (E);

               C := First_Node (Calls (E));

               if not AAU.Is_Empty (Subprogram_Calls (C)) then

                  S := First_Node (Subprogram_Calls (C));

                  while Present (S) loop
                     N := Call_Subprogram (S, E, Declarative_Part);
                     Append_Node_To_List (N, Then_Statements);
                     S := Next_Node (S);
                  end loop;
               end if;
            end if;

            --  Handling the thread OUT ports. If the thread has
            --  entrypoints associated to its IN ports, the emission
            --  of message is done by the user.

            if Get_Thread_Implementation_Kind (E)
              /= Thread_With_Port_Compute_Entrypoint
            then
               if not AAU.Is_Empty (Features (E)) then
                  P := First_Node (Features (E));
                  while Present (P) loop
                     if Kind (P) = K_Port_Spec_Instance
                       and then Is_Out (P)
                     then
                        --  Declare a local variable having the port
                        --  type.

                        N := Make_Object_Declaration
                          (Defining_Identifier =>
                             Map_Ada_Defining_Identifier (P),
                           Object_Definition   =>
                             Map_Ada_Data_Type_Designator
                             (Corresponding_Instance (P)));
                        Append_Node_To_List (N, Declarative_Part);

                        --  For each destination of the port, emit a
                        --  message.

                        D := First_Node (Get_Destination_Ports (P));
                        while Present (D) loop
                           --  Get the designator of the To_Any
                           --  function corresponding to the type_spec
                           --  of port P.

                           N := Extract_Designator
                             (ADN.To_Any_Node
                              (Backend_Node
                               (Identifier
                                (Corresponding_Instance
                                 (P)))));

                           N := Make_Subprogram_Call
                             (N,
                              Make_List_Id
                              (Map_Ada_Defining_Identifier (P)));
                           N := Make_Subprogram_Call
                             (RE (RE_Emit_Msg),
                              Make_List_Id
                              (N,
                               Map_Reference_Identifier (Item (D)),
                               Make_Literal
                               (New_String_Value
                                (Name
                                 (Identifier
                                  (Item
                                   (D)))))));
                           Append_Node_To_List (N, Then_Statements);

                           D := Next_Node (D);
                        end loop;
                     end if;

                     P := Next_Node (P);
                  end loop;
               end if;
            end if;
         end if;

         --  If the thread contains IN event data ports, we build an
         --  IF statement that controls the data polling from port
         --  buffers.

         if Has_In_ED_Ports then
            N := Make_If_Statement
              (Condition       => If_Condition,
               Then_Statements => Then_Statements,
               Else_Statements => Else_Statements);
            Append_Node_To_List (N, Statements);
         else
            Append_Node_To_List (ADN.First_Node (Then_Statements), Statements);
         end if;

         --  If the thread is periodic we put all the statements in a
         --  loop

         if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic then
            --  Extra declarations

            N := Make_Used_Type (RE (RE_Time));
            Append_Node_To_List (N, Declarative_Part);

            N := Make_Used_Type (RE (RE_Time_Span));
            Append_Node_To_List (N, Declarative_Part);

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Next_Start)),
               Object_Definition   => RE (RE_Time),
               Expression          => Make_Subprogram_Call (RE (RE_Clock)));
            Append_Node_To_List (N, Declarative_Part);

            --  Get the thread period

            N := Map_Ada_Time (Get_Thread_Period (E));

            if No (N) then
               Display_Located_Error
                 (Loc (E),
                  "Unable to convert picoseconds period into nanoseconds",
                  Fatal => True);
            end if;

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Period)),
               Constant_Present    => True,
               Object_Definition   => RE (RE_Time_Span),
               Expression          => N);
            Append_Node_To_List (N, Declarative_Part);

            --  Add the delay for the next period

            N := Make_Expression
              (Make_Defining_Identifier (PN (P_Next_Start)),
               Op_Plus,
               Make_Defining_Identifier (PN (P_Period)));
            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Next_Start)), N);
            Append_Node_To_List (N, Statements);

            N := Make_Delay_Statement
              (Expression => Make_Defining_Identifier (PN (P_Next_Start)),
               Is_Until   => True);
            Append_Node_To_List (N, Statements);

            --  Make the global loop statement

            N := Make_Loop_Statement (Statements);
            Statements := Make_List_Id (N);
         end if;

         --  Write back the shared variables

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Subcomponent_Access_Instance then
                  --  The access to the subcomponent has to be REQUIRED

                  if Is_Provided (F) then
                     Display_Located_Error
                       (Loc (F),
                        "Thread providing access to a data not supported",
                        Fatal => True);
                  end if;

                  --  Get the source subcomponent of the data access

                  SD := Get_Subcomponent_Access_Source (F);
                  D  := Corresponding_Instance (SD);

                  if Get_Data_Representation (D) = Data_With_Accessors
                    or else Get_Data_Representation (D) = Data_Struct
                  then
                     --  For protected data with synchronous update
                     --  policy, we read the value of each field.

                     Aggregate := First_Node (Subcomponents (D));

                     while Present (Aggregate) loop
                        if Get_Concurrency_Protocol (D) =
                          Concurrency_Protected_Access
                        then

                           N := Make_Subprogram_Call
                             (Extract_Designator
                              (ADN.Set_Node
                               (Backend_Node
                                (Identifier
                                 (Aggregate)))),
                              Make_List_Id
                              (Map_Ada_Defining_Identifier (SD),
                               Map_Ada_Protected_Aggregate_Identifier
                               (F, Aggregate)));
                           Append_Node_To_List (N, Statements);
                        else
                           N := Make_Assignment_Statement
                             (Make_Selected_Component
                              (Map_Ada_Defining_Identifier (SD),
                               Map_Ada_Defining_Identifier (Aggregate)),
                              Map_Ada_Protected_Aggregate_Identifier
                              (F, Aggregate));
                           Append_Node_To_List (N, Statements);
                        end if;

                        Aggregate := Next_Node (Aggregate);
                     end loop;
                  end if;
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Build the subprogram body

         N := Make_Subprogram_Implementation (Spec,
                                              Declarative_Part,
                                              Statements);
         return N;
      end Thread_Controller_Body;

      ------------------------------------
      -- Protected_Object_Routines_Body --
      ------------------------------------

      procedure Protected_Object_Routines_Bodies (P : Node_Id)
      is
         N : Node_Id;
      begin
         --  1) Put_<Port_Name>

         N := Put_Body (P);
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  2) Get_<Port_Name>

         N := Get_Body (P);
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  3) Push_Back_<Port_Name>

         if Is_Event (P) then
            N := Push_Back_Body (P);
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;
      end Protected_Object_Routines_Bodies;

      --------------
      -- Put_Body --
      --------------

      function Put_Body (P : Node_Id) return Node_Id is
         Spec       : constant Node_Id := ADN.Put_Node
           (Backend_Node (Identifier (P)));
         Statements : constant List_Id := New_List (ADN.K_Statement_List);
         Dcl_Part   : constant List_Id := New_List (ADN.K_Declaration_List);
         F          : Node_Id;
         N          : Node_Id;
      begin
         N := Make_Subprogram_Call
           (RE (RE_Enter),
            Make_List_Id
            (Make_Defining_Identifier (Thread_Mutex)));
         Append_Node_To_List (N, Statements);

         if Is_Event (P) then
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Error)),
               Object_Definition   => RE (RE_Boolean_1));
            Append_Node_To_List (N, Dcl_Part);

            F := Make_Selected_Component
              (Map_Ada_Package_Identifier (P),
               RE (RE_Append));

            N := Make_Subprogram_Call
              (F,
               Make_List_Id
               (Map_Buffer_Instance_Identifier (P),
                Map_Ada_Defining_Identifier (P),
                Make_Defining_Identifier (PN (P_Error))));
            Append_Node_To_List (N, Statements);
         else
            N := Make_Assignment_Statement
              (Variable_Identifier => Map_Variable_Identifier (P),
               Expression          => Map_Ada_Defining_Identifier (P));
            Append_Node_To_List (N, Statements);
         end if;

         N := Make_Subprogram_Call
           (RE (RE_Leave),
            Make_List_Id
            (Make_Defining_Identifier (Thread_Mutex)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Put_Body;

      --------------------
      -- Push_Back_Body --
      --------------------

      function Push_Back_Body (P : Node_Id) return Node_Id is
         Spec       : constant Node_Id := ADN.Push_Back_Node
           (Backend_Node (Identifier (P)));
         Statements : constant List_Id := New_List (ADN.K_Statement_List);
         Dcl_Part   : constant List_Id := New_List (ADN.K_Declaration_List);
         F          : Node_Id;
         N          : Node_Id;
      begin
         N := Make_Subprogram_Call
           (RE (RE_Enter),
            Make_List_Id
            (Make_Defining_Identifier (Thread_Mutex)));
         Append_Node_To_List (N, Statements);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Error)),
            Object_Definition   => RE (RE_Boolean_1));
         Append_Node_To_List (N, Dcl_Part);

         F := Make_Selected_Component
              (Map_Ada_Package_Identifier (P),
               RE (RE_Push_Back));

         N := Make_Subprogram_Call
           (F,
            Make_List_Id
            (Map_Buffer_Instance_Identifier (P),
             Map_Ada_Defining_Identifier (P),
             Make_Defining_Identifier (PN (P_Error))));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Call
           (RE (RE_Leave),
            Make_List_Id
            (Make_Defining_Identifier (Thread_Mutex)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Push_Back_Body;

      --------------
      -- Get_Body --
      --------------

      function Get_Body (P : Node_Id) return Node_Id is
         Spec       : constant Node_Id := ADN.Get_Node
           (Backend_Node (Identifier (P)));
         Statements : constant List_Id := New_List (ADN.K_Statement_List);
         Dcl_Part   : constant List_Id := New_List (ADN.K_Declaration_List);
         F          : Node_Id;
         N          : Node_Id;
      begin
         N := Make_Subprogram_Call
           (RE (RE_Enter),
            Make_List_Id
            (Make_Defining_Identifier (Thread_Mutex)));
         Append_Node_To_List (N, Statements);

         if Is_Event (P) then
            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Error)),
               Object_Definition   => RE (RE_Boolean_1));
            Append_Node_To_List (N, Dcl_Part);

            F := Make_Selected_Component
              (Map_Ada_Package_Identifier (P),
               RE (RE_Get));

            N := Make_Subprogram_Call
              (F, Make_List_Id (Map_Buffer_Instance_Identifier (P),
                                Map_Ada_Defining_Identifier (P),
                                Make_Defining_Identifier (PN (P_Error))));
            Append_Node_To_List (N, Statements);

            N := Make_Expression
              (Make_Defining_Identifier (PN (P_Error)), Op_Not);

            N := Make_Assignment_Statement
              (Map_Port_Boolean_Identifier (P), N);
            Append_Node_To_List (N, Statements);
         else
            N := Make_Assignment_Statement
              (Variable_Identifier => Map_Ada_Defining_Identifier (P),
               Expression          => Map_Variable_Identifier (P));
            Append_Node_To_List (N, Statements);
         end if;

         N := Make_Subprogram_Call
           (RE (RE_Leave),
            Make_List_Id
            (Make_Defining_Identifier (Thread_Mutex)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Dcl_Part, Statements);
         return N;
      end Get_Body;

      --------------------------
      -- Execute_Servant_Body --
      --------------------------

      function Execute_Servant_Body (T : Node_Id) return Node_Id is
         Spec             : constant Node_Id := ADN.Execute_Servant_Node
           (Backend_Node (Identifier (T)));
         Declarative_Part : constant List_Id := New_List
           (ADN.K_Declaration_List);
         Statements       : constant List_Id := New_List
           (ADN.K_Statement_List);

         If_Condition     : Node_Id;
         Then_Statements  : constant List_Id := New_List
           (ADN.K_Statement_List);
         Else_Statements  : constant List_Id := New_List
           (ADN.K_Statement_List);

         Block_Dcl        : constant List_Id := New_List
           (ADN.K_Declaration_List);
         Block_Statements : constant List_Id := New_List
           (ADN.K_Statement_List);

         N                : Node_Id;
         P                : Node_Id;
      begin
         --  Adding a pragma unreferenced for tha Obj parameter

         N := Make_Pragma_Statement
           (Pragma_Unreferenced,
            Make_List_Id
            (Make_Defining_Identifier
             (PN (P_Obj))));
         Append_Node_To_List (N, Declarative_Part);

         --  Build the global block

         --  Fill the block's declarative part

         N := Make_Subprogram_Call
           (RE (RE_Execute_Request),
            Make_List_Id (Make_Defining_Identifier (PN (P_Msg))));
         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_E_Req)),
            Object_Definition   => RE (RE_Execute_Request),
            Renamed_Object      => N);
         Append_Node_To_List (N, Block_Dcl);

         N := Make_Selected_Component
           (Make_Defining_Identifier (PN (P_E_Req)),
            Make_Defining_Identifier (PN (P_Req)));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Req)),
            Object_Definition   => RE (RE_Request_Access),
            Renamed_Object      => N);
         Append_Node_To_List (N, Block_Dcl);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Arg_List)),
            Object_Definition   => RE (RE_Ref_2));
         Append_Node_To_List (N, Block_Dcl);

         --  Handle each one of the IN ports

         if not AAU.Is_Empty (Features (T)) then
            P := First_Node (Features (T));

            while Present (P) loop
               if Kind (P) = K_Port_Spec_Instance and then Is_In (P) then
                  N := Request_Handling (P, T);
                  Append_Node_To_List (N, Block_Statements);
               end if;

               P := Next_Node (P);
            end loop;
         end if;

         --  The return statement

         N := Make_Component_Association
           (Make_Defining_Identifier (PN (P_Req)),
            Make_Defining_Identifier (PN (P_Req)));
         N := Make_Record_Aggregate (Make_List_Id (N));
         N := Make_Qualified_Expression
           (RE (RE_Executed_Request),
            Aggregate => N);
         N := Make_Return_Statement (N);
         Append_Node_To_List (N, Block_Statements);

         N := Make_Block_Statement
           (Declarative_Part => Block_Dcl,
            Statements       => Block_Statements);
         Append_Node_To_List (N, Then_Statements);

         --  Build the global IF statement

         If_Condition := Make_Expression
           (Make_Defining_Identifier (PN (P_Msg)),
            Op_In,
            RE (RE_Execute_Request));

         N := Make_Raise_Statement
           (Make_Defining_Identifier (EN (E_Program_Error)));
         Append_Node_To_List (N, Else_Statements);

         N := Make_If_Statement
           (Condition       => If_Condition,
            Then_Statements => Then_Statements,
            Else_Statements => Else_Statements);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, Declarative_Part, Statements);
         return N;
      end Execute_Servant_Body;

      ----------------------
      -- Request_Handling --
      ----------------------

      function Request_Handling (P : Node_Id; T : Node_Id) return Node_Id is
         If_Condition     : Node_Id;
         Then_Statements  : constant List_Id := New_List
           (ADN.K_Statement_List);

         Block_Dcl        : constant List_Id := New_List
           (ADN.K_Declaration_List);
         Block_Statements : constant List_Id := New_List
           (ADN.K_Statement_List);

         N                : Node_Id;
         Profile          : constant List_Id := New_List (ADN.K_List_Id);
      begin
         --  Declarative part of the external record

         --  Get the TypeCode varable corresponding to the type_spec
         --  of port P

         N := Extract_Designator
           (ADN.TypeCode_Node
            (Backend_Node
             (Identifier
              (Corresponding_Instance
               (P)))));

         N := Make_Subprogram_Call (RE (RE_To_Ref), Make_List_Id (N));

         N := Make_Subprogram_Call (RE (RE_Get_Empty_Any), Make_List_Id (N));

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Argument)),
            Object_Definition   => RE (RE_Any),
            Constant_Present    => True,
            Expression          => N);
         Append_Node_To_List (N, Block_Dcl);

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (PN (P_Error)),
            Object_Definition   => RE (RE_Error_Container));
         Append_Node_To_List (N, Block_Dcl);

         --  Statements of the external record...

         --  ...Create the NVList

         N := Make_Subprogram_Call
           (RE (RE_Create),
            Make_List_Id (Make_Defining_Identifier (PN (P_Arg_List))));
         Append_Node_To_List (N, Block_Statements);

         --  ...Add the port to the NVList

         N := Make_Defining_Identifier (PN (P_Arg_List));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call
           (RE (RE_To_PolyORB_String),
            Make_List_Id
            (Make_Literal
             (New_String_Value
              (Name
               (Identifier
                (P))))));
         Append_Node_To_List (N, Profile);

         N := Make_Defining_Identifier (PN (P_Argument));
         Append_Node_To_List (N, Profile);

         N := RE (RE_ARG_IN);
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Call (RE (RE_Add_Item), Profile);
         Append_Node_To_List (N, Block_Statements);

         --  ...Call the Arguments procedure

         N := Make_Subprogram_Call
           (RE (RE_Arguments),
            Make_List_Id
            (Make_Defining_Identifier (PN (P_Req)),
             Make_Defining_Identifier (PN (P_Arg_List)),
             Make_Defining_Identifier (PN (P_Error))));
         Append_Node_To_List (N, Block_Statements);

         --  ...Inner block statement

         declare
            Inner_Dcl        : constant List_Id
              := New_List (ADN.K_Declaration_List);
            Inner_Statements : constant List_Id :=
              New_List (ADN.K_Statement_List);
         begin
            N := Extract_Designator
              (ADN.From_Any_Node
               (Backend_Node
                (Identifier
                 (Corresponding_Instance
                  (P)))));
            N := Make_Subprogram_Call
              (N,
               Make_List_Id (Make_Defining_Identifier (PN (P_Argument))));
            N := Make_Object_Declaration
              (Defining_Identifier => Map_Port_Argument_Identifier (P),
               Constant_Present    => True,
               Object_Definition   => Map_Ada_Data_Type_Designator
                 (Corresponding_Instance (P)),
               Expression          => N);
            Append_Node_To_List (N, Inner_Dcl);

            --  Put the new received value

            N := Extract_Designator
              (ADN.Put_Node (Backend_Node (Identifier (P))));
            N := Make_Subprogram_Call
              (N, Make_List_Id (Map_Port_Argument_Identifier (P)));
            Append_Node_To_List (N, Inner_Statements);

            --  If the port is an event data port, call the thread
            --  controller

            if Is_Event (P) then
               N := Extract_Designator
                 (ADN.Thread_Controller_Node (Backend_Node (Identifier (T))));
               N := Make_Subprogram_Call (N, No_List);
               Append_Node_To_List (N, Inner_Statements);
            end if;

            N := Make_Block_Statement
              (Declarative_Part => Inner_Dcl,
               Statements       => Inner_Statements);
            Append_Node_To_List (N, Block_Statements);
         end;
         --  Make the external block statement

         N := Make_Block_Statement
           (Declarative_Part => Block_Dcl,
            Statements       => Block_Statements);
         Append_Node_To_List (N, Then_Statements);

         --  Make the IF statement

         N := Make_Designator
           (PN (P_Operation), PN (P_Req), True);

         If_Condition := Make_Expression
           (N,
            Op_Equal,
            Make_Literal
            (New_String_Value (Name (Identifier (P)))));

         N := Make_If_Statement
           (Condition       => If_Condition,
            Then_Statements => Then_Statements);
         return N;
      end Request_Handling;

      ----------------------------------
      -- Buffer_Package_Instantiation --
      ----------------------------------

      function Buffer_Package_Instantiation (P : Node_Id) return Node_Id is
         B_Identifier : constant Node_Id := Map_Ada_Package_Identifier (P);
         B_Type_Spec  : constant Node_Id := Map_Ada_Data_Type_Designator
           (Corresponding_Instance (P));
         Queue_Size   : constant Long_Long := Get_Queue_Size (P);
         B_Size       : Value_Id;
         N            : Node_Id;
      begin
         if Queue_Size = -1 then
            --  Allocate a default size

            B_Size := New_Integer_Value (Default_Queue_Size, 1, 10);
         elsif Queue_Size = 0 then
            --  0 length queues are not supported

            Display_Located_Error
              (Loc (P),
               "Zero length port queues are not supported",
               Fatal => True);
         else
            B_Size := New_Integer_Value
              (Unsigned_Long_Long (Queue_Size), 1, 10);
         end if;

         N := Make_Package_Instantiation
           (B_Identifier,
            RU (RU_ARAO_Cyclic_Array),
            Make_List_Id (B_Type_Spec, Make_Literal (B_Size)));

         return N;
      end Buffer_Package_Instantiation;

      ---------------------------------
      -- Buffer_Instance_Declaration --
      ---------------------------------

      function Buffer_Instance_Declaration (P : Node_Id) return Node_Id is
         V_Identifier : constant Node_Id := Map_Buffer_Instance_Identifier (P);
         B_Identifier : constant Node_Id := ADN.Package_Node
           (Backend_Node (Identifier (P)));
         N            : Node_Id;
      begin
         N := Make_Object_Declaration
           (Defining_Identifier => V_Identifier,
            Object_Definition   => B_Identifier);
         return N;
      end Buffer_Instance_Declaration;

      ------------------------------------
      -- Protected_Variable_Declaration --
      ------------------------------------

      function Protected_Variable_Declaration (P : Node_Id) return Node_Id is
         V_Identifier : constant Node_Id := Map_Variable_Identifier (P);
         V_Type_Spec  : constant Node_Id :=
           Map_Ada_Data_Type_Designator (Corresponding_Instance (P));
         N            : Node_Id;
      begin
         N := Make_Object_Declaration
           (Defining_Identifier => V_Identifier,
            Object_Definition   => V_Type_Spec);
         return N;
      end Protected_Variable_Declaration;

      ---------------------
      -- Call_Subprogram --
      ---------------------

      function Call_Subprogram
        (S : Node_Id;
         T : Node_Id;
         L : List_Id)
        return Node_Id
      is
         N             :  Node_Id;
         Call_Profile  : constant List_Id := New_List (ADN.K_List_Id);
         Spg           : constant Node_Id := Corresponding_Instance (S);
         F             : Node_Id;
         D             : Node_Id;
         Field         : Node_Id;
         Destination_F : Node_Id;
         Source_F      : Node_Id;
         Param_Value   : Node_Id;
         Source_Parent : Node_Id;
      begin
         if not AAU.Is_Empty (Features (Spg)) then
            F := First_Node (Features (Spg));

            while Present (F) loop
               if Kind (F) = K_Parameter_Instance and then Is_Out (F) then
                  --  Raise an error if the parameter is not connected
                  --  to any source.

                  if AAU.Length (Destinations (F)) = 0 then
                     Display_Located_Error
                       (Loc (F),
                        "This OUT parameter is not connected to"
                        & " any destination",
                        Fatal => True);
                  elsif AAU.Length (Destinations (F)) > 1 then
                     Display_Located_Error
                       (Loc (F),
                        "This IN parameter has too many destinations",
                        Fatal => True);
                  end if;

                  --  At this point, we have a subprogram call
                  --  parameter that has exactly one destination.

                  Destination_F := Item (First_Node (Destinations (F)));

                  --  For each OUT parameter, we declare a local
                  --  variable if the OUT parameter is connected to
                  --  another subprogram call or if the caller is a
                  --  thread. Otherwise, we use the corresponding
                  --  caller subprogram parameter.

                  --  The parameter association value takes 2 possible
                  --  values (see the (1) and (2) comments below).

                  if Parent_Component (Destination_F) /= T then
                     --  Here, we map the variable name from the
                     --  subprogram *call* name and the feature
                     --  name. This avoids name clashing when a thread
                     --  calls twice the same subprogram.

                     N := Make_Object_Declaration
                       (Defining_Identifier => Make_Defining_Identifier
                          (Map_Ada_Full_Parameter_Name (S, F)),
                        Object_Definition   => Map_Ada_Data_Type_Designator
                          (Corresponding_Instance (F)));
                     Append_Node_To_List (N, L);

                     --  (1) If we declared a local variable, we use it
                     --      as parameter value.

                     Param_Value := Make_Designator
                       (Map_Ada_Full_Parameter_Name (S, F));
                  else
                     --  (2) If the S parameter is connected to
                     --      a T port, then we use simply the
                     --      corresponding paremeter of S.

                     Param_Value := Make_Designator
                       (To_Ada_Name
                        (Display_Name
                         (Identifier
                          (Destination_F))));
                  end if;

                  --  For each OUT parameter we build a parameter
                  --  association of the actual profile of the
                  --  implmentaion subprogram call <Param> =>
                  --  <Param_Value>.

                  N := Make_Parameter_Association
                    (Selector_Name    => Map_Ada_Defining_Identifier (F),
                     Actual_Parameter => Param_Value);
                  Append_Node_To_List (N, Call_Profile);

               elsif Kind (F) = K_Parameter_Instance and then Is_In (F) then
                  --  Raise an error if the parameter is not connected
                  --  to any source.

                  if AAU.Length (Sources (F)) = 0 then
                     Display_Located_Error
                       (Loc (F),
                        "This IN parameter is not connected to"
                        & " any source",
                        Fatal => True);
                  elsif AAU.Length (Sources (F)) > 1 then
                     Display_Located_Error
                       (Loc (F),
                        "This IN parameter has too many sources",
                        Fatal => True);
                  end if;

                  --  Here we have an IN parameter with exactly one
                  --  source.

                  Source_F := Item (First_Node (Sources (F)));

                  --  Get the source feature parent

                  Source_Parent := Parent_Component (Source_F);

                  --  The parameter value of the built parameter
                  --  association can take 4 different values. (see
                  --  comments (1), (2), (3) and (4) above).

                  if AAU.Is_Thread (Source_Parent) then
                     --  (1) If the Parent of 'Source_F' is a thread,
                     --  then we use the temporary declared variable
                     --  corresponding to the thread port

                     Param_Value := Map_Ada_Defining_Identifier (Source_F);
                  else
                     --  (2) If the the source parent is another
                     --      subprogram call we use the previously
                     --      declared variable.

                     Param_Value := Make_Designator
                       (Map_Ada_Full_Parameter_Name
                        (Parent_Subcomponent (Source_Parent), Source_F));
                  end if;

                  --  For each IN parameter we build a parameter
                  --  association association of the actual profile of
                  --  the implmentaion subprogram call <Param> =>
                  --  <Param_Value>.

                  N := Make_Parameter_Association
                    (Selector_Name    => Map_Ada_Defining_Identifier (F),
                     Actual_Parameter => Param_Value);
                  Append_Node_To_List (N, Call_Profile);
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  2 - The list of all record fileds given

         --  FIXME: Respect the mapping rules by setting the correct
         --  parameter orientation. For now all parameter are
         --  considered IN OUT. Provide all necessary routines
         --  (passing through intermediate variables, to prevent the
         --  user from cheating).

         if not AAU.Is_Empty (Features (Spg)) then
            F := First_Node (Features (Spg));

            while Present (F) loop
               if Kind (F) = K_Subcomponent_Access_Instance then
                  D := Corresponding_Instance (F);

                  case Get_Data_Representation (D) is
                     when Data_Integer
                       | Data_Boolean
                       | Data_Enum
                       | Data_Float
                       | Data_Fixed
                       | Data_String
                       | Data_Wide_String
                       | Data_Character
                       | Data_Wide_Character
                       | Data_Array =>
                        --  If the data component is a simple data
                        --  component (not a structure), we simply add
                        --  a parameter association mpped from the
                        --  data component.

                        N := Make_Parameter_Association
                          (Selector_Name    => Map_Ada_Defining_Identifier
                             (F),
                           Actual_Parameter => Map_Ada_Defining_Identifier
                             (D));

                        Append_Node_To_List (N, Call_Profile);

                     when Data_Struct | Data_With_Accessors =>
                        --  If the data component is a complex data
                        --  component (which has subcomponents), we add a
                        --  parameter with the computed mode and with a
                        --  type mapped from each subcomponent type.

                        Field := First_Node (Subcomponents (D));

                        while Present (Field) loop
                           N := Make_Parameter_Association
                             (Selector_Name    =>
                                Map_Ada_Protected_Aggregate_Identifier
                                (F, Field),
                              Actual_Parameter =>
                                Map_Ada_Protected_Aggregate_Identifier
                                (F, Field));
                           Append_Node_To_List (N, Call_Profile);

                           Field := Next_Node (Field);
                        end loop;

                     when others =>
                        Display_Located_Error
                          (Loc (F),
                           "Unsupported data type",
                           Fatal => True);
                  end case;
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         N := Make_Subprogram_Call
           (Extract_Designator
            (ADN.Subprogram_Node
             (Backend_Node
              (Identifier
               (Spg)))),
            Call_Profile);

         return N;
      end Call_Subprogram;

      ----------------------------
      -- Servant_Initialization --
      ----------------------------

      function Servant_Initialization return Node_Id is
         N                : Node_Id;
         V                : Value_Id;
         Aggregates       : constant List_Id := New_List
           (ADN.K_Component_List);
         Declarative_Part : constant List_Id := New_List
           (ADN.K_Declaration_List);
         Statements       : constant List_Id := New_List
           (ADN.K_Statement_List);
      begin
         --  Declarative part
         --  Adding 'use' clauses to make the code more readable

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings_Lists));
         Append_Node_To_List (N, Declarative_Part);

         --  Statements

         --  The package name

         N := ADN.Defining_Identifier
           (ADN.Package_Declaration (Current_Package));
         V := New_String_Value (Fully_Qualified_Name (N));
         N := Make_Expression (Make_Literal (V), Op_Plus);
         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Name)),
            Expression     => N);
         Append_Node_To_List (N, Aggregates);

         --  The conflicts

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Conflicts)),
            Expression     => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Building the dependancy list of the package

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Depends)),
            Expression     => Make_Expression
              (Map_Dependency (RU (RU_PolyORB_Any_Initialization, False)),
               Op_Plus));
         Append_Node_To_List (N, Aggregates);

         --  Provides

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Provides)),
            Expression     => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Implicit)),
            Expression     => RE (RE_False));
         Append_Node_To_List (N, Aggregates);

         --  Init procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Init)),
            Expression     => Make_Type_Attribute
              (Make_Designator (SN (S_Initialize)),
               A_Access));
         Append_Node_To_List (N, Aggregates);

         --  Shutdown procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Shutdown)),
            Expression     => Make_Null_Statement);
         Append_Node_To_List (N, Aggregates);

         --  Registering the module

         N := Make_Record_Aggregate
           (Aggregates);

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Aggregate    => N);

         N := Make_Subprogram_Call (RE (RE_Register_Module), Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         return N;
      end Servant_Initialization;

   end Package_Body;

end Ocarina.Backends.PO_QoS_Ada.Servants;
