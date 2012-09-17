------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.PO_QOS_ADA.PARAMETERS                   --
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

with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.PO_QoS_Ada.Mapping;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_QoS_Ada.Parameters is

   use Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.PO_QoS_Ada.Mapping;
   use Ocarina.Backends.Ada_Values;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
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

         --  We only set parameters for processes that must have a
         --  port number.

         if not Must_Have_Port_Number (E) then
            return;
         end if;

         Set_Parameters_Spec;

         --  Elaborate the body of the package

         N := Make_Pragma_Statement (Pragma_Elaborate_Body, No_List);
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

         if not AINU.Is_Empty (Subcomponents (E)) then
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

      function Parameter_Entry_Type return Node_Id;
      --  Declare a new record for parameter entry

      function Array_Type return Node_Id;
      --  Declare an 2D-array type of Parameter_Entry

      function Array_Initialization (E : Node_Id) return Node_Id;
      --  Build the array static initialization

      function Partition_Source_Type return Node_Id;
      --  The Partition_Source type declaration

      function Partition_Source_Variable return Node_Id;
      --  Declare a variable of type Partition_Source

      function Get_Conf_Spec return Node_Id;
      --  Spec of the Get_Conf subprogram

      function Get_Conf_Body return Node_Id;
      --  Body of the Get_Conf subprogram

      function Initialize_Spec return Node_Id;
      --  Spec of the Initialize procedure

      function Initialize_Body return Node_Id;
      --  Body of the Initialize procedure

      function Parameters_Initialization return Node_Id;
      --  Initilization block of the Parameters package

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

         --  We only set parameters for processes that must have a
         --  port number.

         if not Must_Have_Port_Number (E) then
            return;
         end if;

         Set_Parameters_Body;

         --  The Parameter_Entry record type declaration

         N := Parameter_Entry_Type;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  The Conf_Table static stable declaration

         N := Make_Defining_Identifier (CN (C_Conf_Table));
         N := Make_Object_Declaration
           (Defining_Identifier => N,
            Constant_Present    => True,
            Object_Definition   => Array_Type,
            Expression          => Array_Initialization (E));
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  The Partition_Source type declaration

         N := Partition_Source_Type;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Spec of the overriding function Get_Conf. It should
         --  appear immediately after the type.

         N := Get_Conf_Spec;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  The The_Partition_Source global variable declaratin

         N := Partition_Source_Variable;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Spec for the Initialize function of the Parameters
         --  package.

         N := Initialize_Spec;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Bodies of the package subprograms

         N := Get_Conf_Body;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         N := Initialize_Body;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         ADN.Set_Package_Initialization
           (Current_Package,
            Make_List_Id (Parameters_Initialization));

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

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      --------------------------
      -- Parameter_Entry_Type --
      --------------------------

      function Parameter_Entry_Type return Node_Id is
         N      : Node_Id;
         C_List : constant List_Id := New_List (ADN.K_Component_List);
      begin
         N := Make_Component_Declaration
           (Make_Defining_Identifier (PN (P_Key)),
            RE (RE_String_Ptr));
         Append_Node_To_List (N, C_List);

         N := Make_Component_Declaration
           (Make_Defining_Identifier (PN (P_Value)),
            RE (RE_String_Ptr));
         Append_Node_To_List (N, C_List);

         N := Make_Record_Type_Definition (Make_Record_Definition (C_List));
         N := Make_Full_Type_Declaration
           (Make_Defining_Identifier (TN (T_Parameter_Entry)),
            N);

         return N;
      end Parameter_Entry_Type;

      ----------------
      -- Array_Type --
      ----------------

      function Array_Type return Node_Id is
         N : Node_Id;
      begin
         N := Make_Range_Constraint
           (Make_Literal (New_Integer_Value (1, 1, 10)),
            Make_Literal (New_Integer_Value (2, 1, 10)));

         N := Make_Array_Type_Definition
           (Make_List_Id (N),
            Make_Defining_Identifier (TN (T_Parameter_Entry)));

         return N;
      end Array_Type;

      --------------------------
      -- Array_Initialization --
      --------------------------

      function Array_Initialization (E : Node_Id) return Node_Id is
         L           : constant List_Id := New_List (ADN.K_List_Id);
         Protocol    : constant Protocol_Type := Get_Protocol
           (Current_Distributed_Application);
         Location    : constant Name_Id := Get_Location
           (Get_Bound_Processor (E));
         Port_Number : constant Value_Id := Get_Port_Number (E);
         N           : Node_Id;
         Inner_L     : List_Id;
      begin
         --  Check that the process has been assigned a port number

         if Port_Number = Properties.No_Value then
            Display_Located_Error
              (Loc (Parent_Subcomponent (E)),
               "This process does not have a port number. Processes with"
               & " IN ports or those with threads that have IN ports must"
               & " be assigned a port number",
               Fatal => True);
         end if;

         --  The address

         Inner_L := New_List (ADN.K_List_Id);

         case Protocol is
            when Protocol_DIOP =>
               Set_Str_To_Name_Buffer ("polyorb.protocols.diop.default_addr");
            when others =>
               Set_Str_To_Name_Buffer ("polyorb.protocols.iiop.default_addr");
         end case;

         N := Make_Literal (New_String_Value (Name_Find));
         N := Make_Record_Aggregate (Make_List_Id (N));
         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_String_2),
            Aggregate    => N);
         N := Make_Object_Instantiation (N);
         Append_Node_To_List (N, Inner_L);

         N := Make_Literal (New_String_Value (Location));
         N := Make_Record_Aggregate (Make_List_Id (N));
         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_String_2),
            Aggregate    => N);
         N := Make_Object_Instantiation (N);
         Append_Node_To_List (N, Inner_L);

         N := Make_Record_Aggregate (Inner_L);
         Append_Node_To_List (N, L);

         --  The port number

         Inner_L := New_List (ADN.K_List_Id);

         case Protocol is
            when Protocol_DIOP =>
               Set_Str_To_Name_Buffer ("polyorb.protocols.diop.default_port");
            when others =>
               Set_Str_To_Name_Buffer ("polyorb.protocols.iiop.default_port");
         end case;

         N := Make_Literal (New_String_Value (Name_Find));
         N := Make_Record_Aggregate (Make_List_Id (N));
         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_String_2),
            Aggregate    => N);
         N := Make_Object_Instantiation (N);
         Append_Node_To_List (N, Inner_L);

         Set_Str_To_Name_Buffer (Image (To_Ada_Value (Port_Number)));

         N := Make_Literal (New_String_Value (Name_Find));
         N := Make_Record_Aggregate (Make_List_Id (N));
         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_String_2),
            Aggregate    => N);
         N := Make_Object_Instantiation (N);
         Append_Node_To_List (N, Inner_L);

         N := Make_Record_Aggregate (Inner_L);
         Append_Node_To_List (N, L);

         N := Make_Array_Aggregate (L);
         return N;
      end Array_Initialization;

      ---------------------------
      -- Partition_Source_Type --
      ---------------------------

      function Partition_Source_Type return Node_Id is
         N            : Node_Id;
         T            : Node_Id;
         T_Identifier : constant Node_Id := Make_Defining_Identifier
           (TN (T_Partition_Source));
      begin
         T := Make_Derived_Type_Definition
           (Subtype_Indication    => RE (RE_Parameters_Source),
            Record_Extension_Part => Make_Record_Definition (No_List),
            Is_Subtype            => False);

         N := Make_Full_Type_Declaration
           (Defining_Identifier => T_Identifier,
            Type_Definition     => T);

         return N;
      end Partition_Source_Type;

      -------------------------------
      -- Partition_Source_Variable --
      -------------------------------

      function Partition_Source_Variable return Node_Id is
         N : Node_Id;
         I : Node_Id;
         T : Node_Id;
      begin
         I := Make_Defining_Identifier (PN (P_The_Partition_Source));
         T := Make_Defining_Identifier (TN (T_Partition_Source));

         N := Make_Object_Declaration
           (Defining_Identifier => I,
            Object_Definition   => T,
            Aliased_Present     => True);

         return N;
      end Partition_Source_Variable;

      -------------------
      -- Get_Conf_Spec --
      -------------------

      function Get_Conf_Spec return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N  : Node_Id;
      begin
         N := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Source)),
            Make_Access_Type_Definition
            (Make_Defining_Identifier (TN (T_Partition_Source))));
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Section)),
            RE (RE_String_2));
         Append_Node_To_List (N, Profile);

         N := Make_Parameter_Specification
           (Make_Defining_Identifier (PN (P_Key)),
            RE (RE_String_2));
         Append_Node_To_List (N, Profile);

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier (SN (S_Get_Conf)),
            Parameter_Profile   => Profile,
            Return_Type         => RE (RE_String_2));

         return N;
      end Get_Conf_Spec;

      -------------------
      -- Get_Conf_Body --
      -------------------

      function Get_Conf_Body return Node_Id is
         Spec           : constant Node_Id := Get_Conf_Spec;
         Statements     : constant List_Id := New_List (ADN.K_Statement_List);
         Declarations   : constant List_Id := New_List
           (ADN.K_Declaration_List);
         For_Statements : constant List_Id := New_List (ADN.K_Statement_List);
         N              : Node_Id;
         C              : Node_Id;
         R              : Node_Id;
      begin
         --  Declarative part

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (VN (V_Temp)),
            Object_Definition   => Make_Defining_Identifier
              (TN (T_Parameter_Entry)));
         Append_Node_To_List (N, Declarations);

         --  Unused parameters

         N := Make_Pragma_Statement
           (Pragma_Unreferenced,
            Make_List_Id
            (Make_Defining_Identifier (PN (P_Source)),
             Make_Defining_Identifier (PN (P_Section))));
         Append_Node_To_List (N, Declarations);

         --  The 'for' loop statements

         N := Make_Assignment_Statement
           (Make_Defining_Identifier (VN (V_Temp)),
            Make_Subprogram_Call
            (Make_Defining_Identifier (CN (C_Conf_Table)),
             Make_List_Id (Make_Defining_Identifier (VN (V_Index)))));
         Append_Node_To_List (N, For_Statements);

         --  The inner 'if' statement

         N := Make_Explicit_Dereference
           (Make_Selected_Component
            (Make_Defining_Identifier (VN (V_Temp)),
             Make_Defining_Identifier (PN (P_Key))));

         C := Make_Expression
           (N, Op_Equal, Make_Defining_Identifier (PN (P_Key)));

         N := Make_Return_Statement
           (Make_Explicit_Dereference
            (Make_Selected_Component
             (Make_Defining_Identifier (VN (V_Temp)),
              Make_Defining_Identifier (PN (P_Value)))));

         N := Make_If_Statement (C, Make_List_Id (N));
         Append_Node_To_List (N, For_Statements);

         R := Make_Range_Constraint
           (Make_Literal (New_Integer_Value (1, 1, 10)),
            Make_Literal (New_Integer_Value (2, 1, 10)));

         N := Make_For_Statement
           (Make_Defining_Identifier (VN (V_Index)),
            R,
            For_Statements);
         Append_Node_To_List (N, Statements);

         --  The last chance return statement

         N := Make_Return_Statement
           (Make_Literal (New_String_Value (No_Name)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, Declarations, Statements);

         return N;
      end Get_Conf_Body;

      ---------------------
      -- Initialize_Spec --
      ---------------------

      function Initialize_Spec return Node_Id is
         N  : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_Initialize)),
            Parameter_Profile   => No_List);

         return N;
      end Initialize_Spec;

      ---------------------
      -- Initialize_Body --
      ---------------------

      function Initialize_Body return Node_Id is
         Spec         : constant Node_Id := Initialize_Spec;
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
         N            : Node_Id;
         I            : Node_Id;
      begin
         I := Make_Defining_Identifier (SN (S_Register_Source));
         N := Make_Attribute_Designator
           (Make_Defining_Identifier (PN (P_The_Partition_Source)),
            A_Access);

         N := Make_Subprogram_Call (I, Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, Declarations, Statements);
         return N;
      end Initialize_Body;

      -------------------------------
      -- Parameters_Initialization --
      -------------------------------

      function Parameters_Initialization return Node_Id is
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
            Expression     => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Provides
         Set_Str_To_Name_Buffer ("parameters_sources");
         N := Make_Literal (New_String_Value (Name_Find));
         N := Make_Expression (N, Op_Plus);
         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Provides)),
            Expression     => N);
         Append_Node_To_List (N, Aggregates);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Implicit)),
            Expression     => RE (RE_True));
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

         N := Make_Subprogram_Call
           (RE (RE_Register_Module),
            Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         return N;
      end Parameters_Initialization;

   end Package_Body;

end Ocarina.Backends.PO_QoS_Ada.Parameters;
