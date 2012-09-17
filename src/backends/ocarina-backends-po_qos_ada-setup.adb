------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A . S E T U P     --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.PO_QoS_Ada.Mapping;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_QoS_Ada.Setup is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.PO_QoS_Ada.Mapping;
   use Ocarina.Backends.Ada_Values;

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
         N : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);

         Set_Setup_Spec;

         N := Make_Pragma_Statement (Pragma_Elaborate_Body);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

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

      function Initialize_Spec return Node_Id;
      function Initialize_Body return Node_Id;
      function Setup_Initialization return Node_Id;
      --  Setup package initialization routines

      Element_List : List_Id := No_List;
      --  The value of the thread property array constant

      Current_Distributed_Application : Node_Id;

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
         U             : constant Node_Id := ADN.Distributed_Application_Unit
           (ADN.Helpers_Node (Backend_Node (Identifier (E))));
         P             : constant Node_Id := ADN.Entity (U);
         Real_Time     : Boolean := False;
         Protocol      : constant Protocol_Type := Get_Protocol
           (Current_Distributed_Application);
         Thread_Number : Nat := 0;
         N             : Node_Id;
         S             : Node_Id;
         C             : Node_Id;
         Params        : List_Id;
         N_Servants    : Unsigned_Long_Long := 0;
      begin
         Push_Entity (P);
         Push_Entity (U);

         Set_Setup_Body;

         --  First loop to determin the process properties

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               C := Corresponding_Instance (S);

               if AAU.Is_Thread (C) then
                  if  Has_In_Ports (C) then
                     N_Servants := N_Servants + 1;
                  end if;

                  declare
                     P : constant Unsigned_Long_Long :=
                       Get_Thread_Priority (C);
                  begin
                     Thread_Number := Thread_Number + 1;

                     if P > 0 then
                        Real_Time := True;
                     end if;
                  end;
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  We take in account thread priorities if there are more
         --  than one thread in the node and at least one of these
         --  threads has been assigned a priority.

         Real_Time := Real_Time and then Thread_Number > 1;

         --  Reset the element list

         Element_List := New_List (ADN.K_List_Id);

         --  The base setup

         Add_With_Package
           (E            => RU (RU_ARAO_Setup_Base, False),
            Used         => False,
            Warnings_Off => True,
            Elaborated   => True);

         --  If the process must have a port number, we must setup
         --  PolyORB.Parameter.Partition in order for the node to set
         --  its IP address and its port number(s).

         if Must_Have_Port_Number (E) then
            Add_With_Package
              (E            => RU (RU_PolyORB_Parameters_Partition, False),
               Used         => False,
               Warnings_Off => True,
               Elaborated   => True);
         end if;

         --  Protocol

         case Protocol is
            when Protocol_DIOP =>
               N := Message_Comment ("Protocol: GIOP/DIOP");
               Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));

               Add_With_Package
                 (E            => RU (RU_PolyORB_Setup_DIOP, False),
                  Used         => False,
                  Warnings_Off => True,
                  Elaborated   => True);
               Add_With_Package
                 (E            => RU
                    (RU_PolyORB_Setup_Access_Points_DIOP, False),
                  Used         => False,
                  Warnings_Off => True,
                  Elaborated   => True);
            when others =>
               --  Default protocol is IIOP

               N := Message_Comment ("Protocol: GIOP/IIOP");
               Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));

               Add_With_Package
                 (E            => RU (RU_PolyORB_Setup_IIOP, False),
                  Used         => False,
                  Warnings_Off => True,
                  Elaborated   => True);
               Add_With_Package
                 (E            =>
                    RU (RU_PolyORB_Setup_Access_Points_IIOP, False),
                  Used         => False,
                  Warnings_Off => True,
                  Elaborated   => True);
         end case;

         --  ORB controller

         N := Message_Comment ("ORB controller : workers");
         Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));
         Add_With_Package
           (E            => RU (RU_PolyORB_ORB_Controller_Workers, False),
            Used         => False,
            Warnings_Off => True,
            Elaborated   => True);

         if Real_Time then
            --  Visit recursively all the subcomponents of the process

            if not AAU.Is_Empty (Subcomponents (E)) then
               S := First_Node (Subcomponents (E));

               while Present (S) loop
                  --  Visit the corresponding component instance

                  Visit (Corresponding_Instance (S));

                  S := Next_Node (S);
               end loop;
            end if;

            N := Message_Comment ("Priority handling");
            Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));

            Add_With_Package
              (E            => RU (RU_ARAO_Setup_OA_Multithreaded_Prio, False),
               Used         => False,
               Warnings_Off => True,
               Elaborated   => True);

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier
                 (VN (V_Threads_Array)),
               Constant_Present    => True,
               Object_Definition   => RE (RE_Thread_Properties_Array),
               Expression          => Make_Array_Aggregate (Element_List));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            Params := Make_List_Id
              (Make_Defining_Identifier (VN (V_Threads_Array)));

            N := Make_Package_Instantiation
              (Defining_Identifier => Make_Defining_Identifier
                 (PN (P_Priority_Manager)),
               Generic_Package     => RU (RU_ARAO_Setup_OA_Multithreaded_Prio),
               Parameter_List      => Params);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N := Initialize_Spec;
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N := Initialize_Body;
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            ADN.Set_Package_Initialization
              (Current_Package, Make_List_Id (Setup_Initialization));
         else
            --  In case of a non real-time application, do not add the
            --  dependency on the object adapter only if the node
            --  contains at least one servant.

            if N_Servants > 0 then
               Add_With_Package
                 (E            => RU (RU_ARAO_Setup_Ocarina_OA, False),
                  Used         => False,
                  Warnings_Off => True,
                  Elaborated   => True);
            end if;

            N := Message_Comment ("No request priority management");
            Append_Node_To_List (N, ADN.Statements (Current_Package));
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
         Current_Distributed_Application := E;

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
         Priority        : constant Unsigned_Long_Long :=
           Get_Thread_Priority (E);
         Byte_Stack_Size : constant Unsigned_Long_Long :=
           To_Bytes (Get_Thread_Stack_Size (E));
         Src_Number      : Natural := 0;
         F               : Node_Id;
         N               : Node_Id;
         L               : constant List_Id := New_List (ADN.K_List_Id);
      begin
         --  The thread priority

         if Priority /= 0 then
            N := Make_Literal (New_Integer_Value (Priority, 1, 10));
         else
            N := RE (RE_Default_Priority);
         end if;

         Append_Node_To_List (N, L);

         --  The thread stack size

         N := Make_Literal
           (New_Integer_Value
            (Byte_Stack_Size, 1, 10));
         Append_Node_To_List (N, L);

         --  The thread name

         N := Make_Literal
           (New_String_Value
            (Name
             (Identifier
              (Parent_Subcomponent
               (E)))));
         N := Make_Subprogram_Call
           (RE (RE_To_PolyORB_String),
            Make_List_Id (N));
         Append_Node_To_List (N, L);

         --  The thread source number

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
                  Src_Number := Src_Number
                    + AAU.Length (Get_Source_Ports (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         N := Make_Literal
           (New_Integer_Value
            (Unsigned_Long_Long (Src_Number), 0, 10));
         Append_Node_To_List (N, L);

         N := Make_Record_Aggregate (L);
         N := Make_Qualified_Expression (RE (RE_Thread_Properties), N);
         Append_Node_To_List (N, Element_List);
      end Visit_Thread_Instance;

      ---------------------
      -- Initialize_Spec --
      ---------------------

      function Initialize_Spec return Node_Id is
         N : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_Initialize)),
            Parameter_Profile   => No_List,
            Return_Type         => No_Node);
         return N;
      end Initialize_Spec;

      ---------------------
      -- Initialize_Body --
      ---------------------

      function Initialize_Body return Node_Id is
         Specs        : constant Node_Id := Initialize_Spec;
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         N            : Node_Id;
      begin
         N := Make_Subprogram_Call
           (Make_Selected_Component
            (Make_Defining_Identifier
             (PN (P_Priority_Manager)),
             Make_Defining_Identifier
             (SN (S_Initialize))));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Specs, No_List, Statements);
         return N;
      end Initialize_Body;

      --------------------------
      -- Setup_Initialization --
      --------------------------

      function Setup_Initialization return Node_Id is
         use ADN;

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

         N := Defining_Identifier (Package_Declaration (Current_Package));
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

         N := Make_Expression
           (Make_Expression
            (Map_Dependency (RU (RU_ARAO_Setup_OA_Multithreaded, False)),
             Op_Plus),
            Op_And_Symbol,
            Map_Dependency (RU (RU_PolyORB_Setup_OA_Basic_RT_POA, False)));

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Depends)),
            Expression     => N);
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

         N := Make_Record_Aggregate (Aggregates);

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Aggregate    => N);

         N := Make_Subprogram_Call
           (RE (RE_Register_Module),
            Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         return N;
      end Setup_Initialization;

   end Package_Body;

end Ocarina.Backends.PO_QoS_Ada.Setup;
