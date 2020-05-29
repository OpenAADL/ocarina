------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ H I _ C . D E P L O Y M E N T   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2020 ESA & ISAE.      --
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

with Ocarina.Namet;
with Utils; use Utils;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Common.BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;

with Ocarina.Instances.Queries;

with Locations;

package body Ocarina.Backends.PO_HI_C.Deployment is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.C_Values;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Instances.Queries;
   use Ocarina.Backends.C_Common.BA;

   use Locations;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CV renames Ocarina.Backends.C_Values;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package BATN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   package BANu renames Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   Entity_Array           : Node_Id;
   Devices_Array          : Node_Id;
   Devices_Nb_Buses_Array : Node_Id;
   Devices_Confvars       : Node_Id;
   Protocols_Conf         : Node_Id;
   Devices_Buses_Array    : Node_Id;
   Port_To_Devices        : Node_Id;
   Devices_To_Nodes       : Node_Id;
   Global_Port_Kind       : Node_Id;
   Global_Port_Queue_Size : Node_Id;
   Global_Port_Data_Size  : Node_Id;
   Global_Ports           : List_Id;
   Protocol_Identifier    : Unsigned_Long_Long := 0;

   function Is_Added (P : Node_Id; E : Node_Id) return Boolean;
   function Added_Internal_Name (P : Node_Id; E : Node_Id) return Name_Id;

   --------------
   -- Is_Added --
   --------------

   function Is_Added (P : Node_Id; E : Node_Id) return Boolean is
      I_Name : constant Name_Id := Added_Internal_Name (P, E);
   begin
      return Get_Name_Table_Byte (I_Name) = 1;
   end Is_Added;

   -------------------------
   -- Added_Internal_Name --
   -------------------------

   function Added_Internal_Name (P : Node_Id; E : Node_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("%add%enumerator%");
      Add_Nat_To_Name_Buffer (Nat (P));
      Add_Char_To_Name_Buffer ('%');
      Add_Nat_To_Name_Buffer (Nat (E));

      return Name_Find;
   end Added_Internal_Name;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Bus_Instance (E : Node_Id);
      procedure Visit_Virtual_Bus_Instance (E : Node_Id);

      procedure Set_Added (P : Node_Id; E : Node_Id);

      procedure Append_Existing
        (S         :        Node_Id;
         L         :        List_Id;
         Id        : in out Unsigned_Long_Long;
         Is_Entity :        Boolean := False);

      --  Append a node in a List. If the node was node already processed,
      --  it assigns a value (using the Id) argument to the node and bind
      --  it to the Backend Node of S. Is a value was already assigned, it
      --  simply uses it and append the it in the list.
      --  This function is used to warrant that all entities will have
      --  the same values on each node.

      Nb_Ports_List           : List_Id;
      Node_Enumerator_List    : List_Id;
      Tasks_Enumerator_List   : List_Id;
      Devices_Enumerator_List : List_Id;
      Buses_Enumerator_List   : List_Id;
      Entity_Enumerator_List  : List_Id;
      Global_Port_List        : List_Id;
      Protocol_List           : List_Id;
      Global_Port_Names       : Node_Id;
      Global_Port_Model_Names : Node_Id;
      Local_Port_List         : List_Id;

      Nb_Nodes : Unsigned_Long_Long;

      Current_Process_Instance : Node_Id := No_Node;

      Global_Port_To_Entity : Node_Id;
      Global_Port_To_Local  : Node_Id;
      Local_Port_Values     : Node_Id;

      Invalid_Local_Port_Added  : Boolean := False;
      Invalid_Protocol_Added    : Boolean := False;
      Invalid_Global_Port_Added : Boolean := False;
      Invalid_Entity_Added      : Boolean := False;

      Current_Device : Node_Id := No_Node;
      --  Current_Process : Node_Id := No_Node;

      --  Point to the process currently visited. When we visit a process
      --  we look at all its ports and visit the called subprograms. So,
      --  we need to know if these subprograms are linked with the currrent
      --  process.

      Node_Identifier        : Unsigned_Long_Long := 0;
      Global_Port_Identifier : Unsigned_Long_Long := 0;
      Local_Port_Identifier  : Unsigned_Long_Long := 0;
      Entity_Identifier      : Unsigned_Long_Long := 0;
      Task_Identifier        : Unsigned_Long_Long := 0;
      Tasks_Stack            : Unsigned_Long_Long := 0;
      Nb_Protected           : Unsigned_Long_Long := 0;
      Device_Id              : Unsigned_Long_Long := 0;
      Bus_Id                 : Unsigned_Long_Long := 0;
      Nb_Ports_In_Process    : Unsigned_Long_Long := 0;
      Nb_Ports_Total         : Unsigned_Long_Long := 0;
      Total_Ports_Node       : Node_Id            := No_Node;
      Nb_Entities_Node       : Node_Id            := No_Node;
      Nb_Devices_Node        : Node_Id            := No_Node;
      Nb_Buses_Node          : Node_Id            := No_Node;
      Nb_Protocols_Node      : Node_Id            := No_Node;

      --  The information from Simulink can come
      --  from both data and subprograms. So, to avoid
      --  conflict, we add relevant informations from
      --  the first component that have them. And for other
      --  components, we add nothing.

      ---------------------
      -- Append_Existing --
      ---------------------

      procedure Append_Existing
        (S         :        Node_Id;
         L         :        List_Id;
         Id        : in out Unsigned_Long_Long;
         Is_Entity :        Boolean := False)
      is
         N : Node_Id;
      begin
         if No (Backend_Node (Identifier (S)))
           or else
           (Present (Backend_Node (Identifier (S)))
            and then No (CTN.Enumerator_Node (Backend_Node (Identifier (S)))))
         then

            N :=
              Make_Expression
                (Make_Defining_Identifier
                   (Map_C_Enumerator_Name
                      (S,
                       Custom_Parent => Current_Device,
                       Entity        => Is_Entity)),
                 Op_Equal,
                 Make_Literal (CV.New_Int_Value (Id, 0, 10)));
            Bind_AADL_To_Enumerator (Identifier (S), N);
            Append_Node_To_List (N, L);
            Id := Id + 1;
         end if;
      end Append_Existing;

      ---------------
      -- Set_Added --
      ---------------

      procedure Set_Added (P : Node_Id; E : Node_Id) is
         I_Name : constant Name_Id := Added_Internal_Name (P, E);
      begin
         Set_Name_Table_Byte (I_Name, 1);
      end Set_Added;

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
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when CC_Device =>
               Visit_Device_Instance (E);

            when CC_Bus =>
               Visit_Bus_Instance (E);

            when CC_Virtual_Bus =>
               Visit_Virtual_Bus_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ------------------------
      -- Visit_Bus_Instance --
      ------------------------

      procedure Visit_Bus_Instance (E : Node_Id) is
         N : Node_Id;
      begin
         N :=
           Make_Expression
             (Make_Defining_Identifier
                (Map_C_Enumerator_Name (E, Entity => False)),
              Op_Equal,
              (Make_Literal (CV.New_Int_Value (Bus_Id, 0, 10))));
         Append_Node_To_List (N, Buses_Enumerator_List);

         Bus_Id := Bus_Id + 1;

         CTN.Set_Value (Nb_Buses_Node, New_Int_Value (Bus_Id, 1, 10));

      end Visit_Bus_Instance;

      --------------------------------
      -- Visit_Virtual_Bus_Instance --
      --------------------------------

      procedure Visit_Virtual_Bus_Instance (E : Node_Id) is
         N        : Node_Id;
         C        : Node_Id;
         S        : Node_Id;
         Impl     : Node_Id;
         Found    : Boolean;
         PName    : Name_Id;
         Bus_Conf : constant Node_Id := Make_Array_Values;
      begin
         --  A virtual bus describe a user-defined protocol.
         --  We are expecting that such a component defines some
         --  sub-components that model how it works.
         --  We are expecting at least one subprogram
         --  for marshalling (called marshaller in the subcomponent)
         --  and one for unmarshalling.

         --  Here, we retrieve the implementation of the protocol, as
         --  an abstract component. Then, we look for the marshaller
         --  and unmarshaller components that model protocol internals.

         Impl := Get_Implementation (E);

         --  If there is no abstract component associated with the virtual
         --  bus, this is useless to continue.

         if Impl = No_Node then
            return;
         end if;

         --  Make sure we include the header of subprograms
         --  in order to avoid any compilation issue (warning/errors).
         Set_Deployment_Source;
         Add_Include (RH (RH_Subprograms));
         Set_Deployment_Header;

         --  Add a maccro __PO_HI_USE_PROTOCOL_<NAME> so that we can
         --  make conditional compilation depending on the protocol
         --  that are used within the distributed system.
         Set_Str_To_Name_Buffer ("__PO_HI_USE_PROTOCOL_");
         Get_Name_String_And_Append (Name (Identifier (E)));

         PName := Name_Find;
         PName := To_Upper (To_C_Name (PName));

         Add_Define_Deployment
           (Make_Defining_Identifier (PName, C_Conversion => False));

         --  If there is a backend node and a naming node associated
         --  with it, it means that we already processed this protocol
         --  and correctly mapped it in the generated code.

         if Backend_Node (Identifier (E)) /= No_Node
           and then CTN.Naming_Node (Backend_Node (Identifier (E))) /= No_Node
         then
            return;
         end if;

         --  First, we are looking for the marshaller subcomponent.
         --  If not found, we raise an error.

         Found := False;

         if not AAU.Is_Empty (Subcomponents (Impl)) then
            S := First_Node (Subcomponents (Impl));
            while Present (S) loop
               C := Corresponding_Instance (S);
               if AAU.Is_Subprogram (C)
                 and then
                   Get_Name_String (Name (Identifier (S))) =
                   "marshaller"
               then
                  Append_Node_To_List
                    (Map_C_Defining_Identifier (C),
                     CTN.Values (Bus_Conf));
                  Found := True;
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         if not Found then
            Display_Error
              ("User-defined protocol does define a marshaller",
               Fatal => True);
         end if;

         --  First, we are looking for the unmarshaller subcomponent.
         --  If not found, we display an error.

         Found := False;

         if not AAU.Is_Empty (Subcomponents (Impl)) then
            S := First_Node (Subcomponents (Impl));
            while Present (S) loop
               C := Corresponding_Instance (S);
               if AAU.Is_Subprogram (C)
                 and then
                   Get_Name_String (Name (Identifier (S))) =
                   "unmarshaller"
               then

                  Append_Node_To_List
                    (Map_C_Defining_Identifier (C),
                     CTN.Values (Bus_Conf));
                  Found := True;
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         if not Found then
            Display_Error
              ("User-defined protocol does define a unmarshaller",
               Fatal => True);
         end if;

--         Found := False;
--
--         if not AAU.Is_Empty (Subcomponents (Impl)) then
--            S := First_Node (Subcomponents (Impl));
--            while Present (S) loop
--               C := Corresponding_Instance (S);
--               if AAU.Is_Data (C) and then
--                  Append_Node_To_List
--                     (Map_C_Defining_Identifier (C),
--                     CTN.Values (Bus_Conf));
--               end if;
--               S := Next_Node (S);
--            end loop;
--         end if;
--
--         if not Found then
--            Display_Error
--               ("User-defined protocol does define an associated type",
--                Fatal => True);
--         end if;

         --  Here, we add the array we just built and that describe
         --  the protocol configuration into the global array
         --  that contain all protocols configuration.

         Append_Node_To_List (Bus_Conf, CTN.Values (Protocols_Conf));

         --  Finally, we assigned a unique protocol identifier to the
         --  virtual bus, that will be mapped in the protocol_t enumeration
         --  in deployment.h.

         N :=
           Make_Expression
             (Make_Defining_Identifier (Map_C_Enumerator_Name (E)),
              Op_Equal,
              (Make_Literal (CV.New_Int_Value (Protocol_Identifier, 1, 10))));
         Append_Node_To_List (N, Protocol_List);
         Protocol_Identifier := Protocol_Identifier + 1;

         CTN.Set_Value
           (Nb_Protocols_Node,
            New_Int_Value (Protocol_Identifier, 1, 10));

         Bind_AADL_To_Naming
           (Identifier (E),
            Make_Defining_Identifier (Map_C_Enumerator_Name (E)));
      end Visit_Virtual_Bus_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         N                     : Node_Id;
         U                     : Node_Id;
         P                     : Node_Id;
         Q                     : Node_Id;
         F                     : Node_Id;
         Conf_Str              : Name_Id          := No_Name;
         Tmp_Name              : Name_Id;
         Nb_Connected_Buses    : Unsigned_Long_Long;
         Accessed_Buses        : constant Node_Id := Make_Array_Values;
         Accessed_Bus          : Node_Id;
         Device_Implementation : Node_Id;
         Configuration_Data    : Node_Id;
         The_System            : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Associated_Process : Node_Id := No_Node;
      begin
         Current_Device := E;

         if Current_Process_Instance /= No_Node
           and then
             Get_Bound_Processor (E) =
             Get_Bound_Processor (Current_Process_Instance)
         then

            if Backend_Node (Identifier (E)) /= No_Node
              and then
                CTN.Enumerator_Node (Backend_Node (Identifier (E))) /=
                No_Node
            then
               return;
            end if;

            Bind_AADL_To_Enumerator
              (Identifier (E),
               Make_Defining_Identifier
                 (Map_C_Enumerator_Name (E, Entity => False)));

            --  Try to find the process bounded with the device.
            Q := First_Node (Subcomponents (The_System));

            while Present (Q) loop
               if AAU.Is_Process (Corresponding_Instance (Q))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (Q)) =
                   Get_Bound_Processor (E)
               then
                  Associated_Process := Q;
               end if;
               Q := Next_Node (Q);
            end loop;

            N :=
              Make_Expression
                (Make_Defining_Identifier
                   (Map_C_Enumerator_Name (E, Entity => False)),
                 Op_Equal,
                 (Make_Literal (CV.New_Int_Value (Device_Id, 0, 10))));
            Append_Node_To_List (N, Devices_Enumerator_List);

            if Associated_Process /= No_Node then
               Append_Node_To_List
                 (Make_Defining_Identifier
                    (Map_C_Enumerator_Name (Associated_Process)),
                  CTN.Values (Devices_To_Nodes));
            else
               Append_Node_To_List
                 (RE (RE_Unused_Node),
                  CTN.Values (Devices_To_Nodes));
            end if;

            Device_Id := Device_Id + 1;

            CTN.Set_Value (Nb_Devices_Node, New_Int_Value (Device_Id, 1, 10));

            if Get_Location (E) /= No_Name then
               Get_Name_String (Get_Location (E));
               Conf_Str := Name_Find;
               if Get_Port_Number (E) /= Properties.No_Value then
                  Set_Str_To_Name_Buffer (":");
                  Add_Str_To_Name_Buffer
                    (Value_Id'Image (Get_Port_Number (E)));
                  Tmp_Name := Name_Find;
               end if;
               Get_Name_String (Conf_Str);
               Get_Name_String_And_Append (Tmp_Name);
               Conf_Str := Name_Find;
            elsif Is_Defined_Property (E, "deployment::channel_address") then
               Set_Str_To_Name_Buffer
                 (Unsigned_Long_Long'Image
                    (Get_Integer_Property (E, "deployment::channel_address")));
               Conf_Str := Name_Find;
               if Is_Defined_Property (E, "deployment::process_id") then
                  Set_Str_To_Name_Buffer
                    (Unsigned_Long_Long'Image
                       (Get_Integer_Property (E, "deployment::process_id")));
                  Tmp_Name := Name_Find;
                  Get_Name_String (Conf_Str);
                  Add_Str_To_Name_Buffer (":");
                  Get_Name_String_And_Append (Tmp_Name);
                  Conf_Str := Name_Find;
               end if;
            elsif Is_Defined_Property (E, "deployment::configuration")
              and then
                Get_String_Property (E, "deployment::configuration") /=
                No_Name
            then
               Get_Name_String
                 (Get_String_Property (E, "deployment::configuration"));
               Conf_Str := Name_Find;
            end if;

            --  Now, we look at the amount of buses connected to
            --  the device and which bus is connected to which
            --  device. As a result, the arrays
            --  __po_hi_devices_nb_accessed_bus and
            --  __po_hi_devices_accessed_bus will be created.

            Nb_Connected_Buses := 0;

            if not AAU.Is_Empty (Features (E)) then
               F := First_Node (Features (E));

               while Present (F) loop
                  if Kind (F) = K_Subcomponent_Access_Instance
                    and then AAU.Is_Bus (Corresponding_Instance (F))
                    and then First_Node (Sources (F)) /= No_Node
                  then

                     Accessed_Bus := Item (First_Node (Sources (F)));

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_C_Enumerator_Name
                             (Corresponding_Instance (Accessed_Bus))),
                        CTN.Values (Accessed_Buses));

                     Nb_Connected_Buses := Nb_Connected_Buses + 1;
                  end if;
                  F := Next_Node (F);
               end loop;
            end if;

            --  If the device accesses at least one bus, we declare
            --  an array in ALL processed that detail the buses
            --  it accesses. Then, this array is contained
            --  in the global array __po_hi_devices_accessed_buses.
            --  For this reason, we are forced to process again
            --  all processes to add the new array to all processes.

            if not Is_Empty (CTN.Values (Accessed_Buses)) then
               Set_Deployment_Source;

               --  Here, we browse all the process components
               --  of the root system.
               Q := First_Node (Subcomponents (The_System));

               while Present (Q) loop
                  if AAU.Is_Process (Corresponding_Instance (Q)) then
                     U :=
                       CTN.Distributed_Application_Unit
                         (CTN.Naming_Node
                            (Backend_Node
                               (Identifier (Corresponding_Instance (Q)))));
                     P := CTN.Entity (U);

                     Push_Entity (P);
                     Push_Entity (U);

                     Set_Deployment_Source;

                     --  Here, we build the array that details the buses
                     --  accessed by the device and add it to each
                     --  deployment.c file of ALL processes.

                     N :=
                       Make_Expression
                         (Left_Expr =>
                            Make_Variable_Declaration
                              (Defining_Identifier =>
                                 Make_Array_Declaration
                                   (Defining_Identifier =>
                                      Make_Defining_Identifier
                                        (Map_Devices_Buses_Array_Name (E)),
                                    Array_Size =>
                                      Make_Literal
                                        (CV.New_Int_Value
                                           (Nb_Connected_Buses,
                                            1,
                                            10))),
                               Used_Type => RE (RE_Bus_Id)),
                          Operator   => Op_Equal,
                          Right_Expr => Accessed_Buses);

                     Append_Node_To_List (N, CTN.Declarations (Current_File));
                     Pop_Entity;
                     Pop_Entity;
                     Set_Deployment_Header;
                  end if;
                  Q := Next_Node (Q);
               end loop;

               Set_Deployment_Header;

               --  Finally, here, we reference the array name that
               --  contains connected buses to the main array
               --  __po_hi_devices_accessed_buses.

               Append_Node_To_List
                 (Make_Defining_Identifier (Map_Devices_Buses_Array_Name (E)),
                  CTN.Values (Devices_Buses_Array));
            else
               --  If no bus is connected, we just specify
               --  a null pointer, meaning that no bus is
               --  connected to this device.
               Append_Node_To_List
                 (Make_Defining_Identifier
                    (CONST (C_Null),
                     C_Conversion => False),
                  CTN.Values (Devices_Buses_Array));
            end if;

            Append_Node_To_List
              (Make_Literal (CV.New_Int_Value (Nb_Connected_Buses, 1, 10)),
               CTN.Values (Devices_Nb_Buses_Array));

            if Is_Defined_Property (E, "source_text") then
               Append_Node_To_List
                 (Make_Type_Conversion
                    (Make_Pointer_Type (RE (RE_Uint32_T)),
                     Make_Variable_Address
                       (Make_Defining_Identifier
                          (Map_Device_Confvar_Name (E)))),
                  CTN.Values (Devices_Confvars));
               Set_Deployment_Source;

               Device_Implementation := Get_Implementation (Current_Device);

               if Is_Defined_Property
                   (Device_Implementation,
                    "deployment::configuration_type")
               then
                  Configuration_Data :=
                    Get_Classifier_Property
                      (Device_Implementation,
                       "deployment::configuration_type");
                  if Configuration_Data /= No_Node
                    and then Is_Defined_Property
                      (Configuration_Data,
                       "type_source_name")
                  then
                     --  Here, we browse all the process components
                     --  of the root system to declare external
                     --  variable that contain the configuration
                     --  of the device.

                     Q := First_Node (Subcomponents (The_System));

                     while Present (Q) loop
                        if AAU.Is_Process (Corresponding_Instance (Q)) then
                           U :=
                             CTN.Distributed_Application_Unit
                               (CTN.Naming_Node
                                  (Backend_Node
                                     (Identifier
                                        (Corresponding_Instance (Q)))));
                           P := CTN.Entity (U);

                           Push_Entity (P);
                           Push_Entity (U);

                           Set_Deployment_Source;

                           N :=
                             Make_Extern_Entity_Declaration
                               (Make_Variable_Declaration
                                  (Defining_Identifier =>
                                     Make_Defining_Identifier
                                       (Map_Device_Confvar_Name (E)),
                                   Used_Type =>
                                     Make_Pointer_Type
                                       (Make_Defining_Identifier
                                          (Map_ASN_Type
                                             (Get_String_Property
                                                (Configuration_Data,
                                                 "type_source_name"))))));
                           Append_Node_To_List
                             (N,
                              CTN.Declarations (Current_File));
                           declare
                              ST : constant Name_Array :=
                                Get_Source_Text (Configuration_Data);
                              Include_Name : Name_Id;
                           begin
                              Set_Deployment_Header;

                              if ST'Length = 0 then
                                 Display_Error
                                   ("Source_Text property of " &
                                    "configuration data" &
                                    " must have at least one element " &
                                    "(the header file).",
                                    Fatal => True);
                              end if;

                              Include_Name := No_Name;

                              for Index in ST'Range loop
                                 Get_Name_String (ST (Index));
                                 if Name_Buffer (Name_Len - 1 .. Name_Len) =
                                   ".h"
                                 then
                                    Include_Name :=
                                      Get_String_Name
                                        (Name_Buffer (1 .. Name_Len - 2));
                                 end if;
                              end loop;

                              if Include_Name = No_Name then
                                 Display_Error
                                   ("Cannot find header file " &
                                    "that implements the data type",
                                    Fatal => True);
                              end if;

                              Add_Include
                                (Make_Include_Clause
                                   (Make_Defining_Identifier (Include_Name)));
                           end;
                           Pop_Entity;
                           Pop_Entity;
                        end if;
                        Q := Next_Node (Q);
                     end loop;
                  end if;
               end if;
               Append_Node_To_List
                 (Make_Literal
                    (CV.New_Pointed_Char_Value (Get_String_Name ("noaddr"))),
                  CTN.Values (Devices_Array));

               Set_Deployment_Header;
            else
               Append_Node_To_List
                 (Make_Literal (CV.New_Int_Value (0, 0, 10)),
                  CTN.Values (Devices_Confvars));

               if Conf_Str /= No_Name then
                  Append_Node_To_List
                    (Make_Literal (CV.New_Pointed_Char_Value (Conf_Str)),
                     CTN.Values (Devices_Array));
               else
                  Append_Node_To_List
                    (Make_Literal
                       (CV.New_Pointed_Char_Value
                          (Get_String_Name ("noaddr"))),
                     CTN.Values (Devices_Array));
               end if;
            end if;
         end if;
         Current_Device := No_Node;
      end Visit_Device_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P        : constant Node_Id := CTN.Entity (U);
         S        : constant Node_Id := Parent_Subcomponent (E);
         Root_Sys : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Driver_Name : Name_Id;
         Q           : Node_Id;
         N           : Node_Id;
         C           : Node_Id;
         F           : Node_Id;
         Data        : Node_Id;
         Src         : Node_Id;
         Dst         : Node_Id;
         Parent      : Node_Id;
         The_System  : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Device_Implementation : Node_Id;
         Node_Name   : Name_Id;

      begin
         pragma Assert (AAU.Is_System (Root_Sys));

         Nb_Nodes := 0;

         Set_Added (E, E);

         Current_Process_Instance := E;

         Tasks_Enumerator_List := New_List (CTN.K_Enumeration_Literals);
         Node_Enumerator_List  := New_List (CTN.K_Enumeration_Literals);

         Nb_Ports_List := New_List (CTN.K_List_Id);

         Push_Entity (P);
         Push_Entity (U);
         Set_Deployment_Header;

         Node_Identifier     := 0;
         Task_Identifier     := 0;
         Tasks_Stack         := 0;
         Nb_Protected        := 0;
         Nb_Ports_In_Process := 0;

         --  Define the name of the current node

         N :=
           Make_Define_Statement
             (Defining_Identifier => (RE (RE_My_Node)),
              Value               =>
                Make_Defining_Identifier
                  (Map_C_Enumerator_Name (Parent_Subcomponent (E))));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         Node_Name :=
           Map_C_Enumerator_Name (Parent_Subcomponent (E));
         Set_Str_To_Name_Buffer ("");
         Get_Name_String (Token_Image (Tok_Quote));
         Get_Name_String_And_Append (Node_Name);
         Get_Name_String_And_Append (Token_Image (Tok_Quote));

         Node_Name := Name_Find;

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_My_Node_Name),
              Value               =>
                Make_Defining_Identifier (Node_Name));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AAU.Is_Device (Corresponding_Instance (C))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (C)) =
                   Get_Bound_Processor (E)
               then
                  --  Build the enumerator corresponding to the device
                  --  Note: we reuse the process name XXX

                  Visit_Device_Instance (Corresponding_Instance (C));

                  Current_Device := Corresponding_Instance (C);

                  Device_Implementation := Get_Implementation (Current_Device);

                  if Device_Implementation /= No_Node then
                     if not AAU.Is_Empty
                         (AAN.Subcomponents (Device_Implementation))
                     then
                        N :=
                          First_Node (Subcomponents (Device_Implementation));
                        while Present (N) loop
                           Visit_Component_Instance
                             (Corresponding_Instance (N));
                           N := Next_Node (N);
                        end loop;
                     end if;
                  end if;

                  Current_Device := No_Node;

               end if;
               C := Next_Node (C);
            end loop;
         end if;

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            C := First_Node (Subcomponents (E));

            while Present (C) loop
               if AAU.Is_Data (Corresponding_Instance (C)) then
                  N := Make_Literal (New_Int_Value (Nb_Protected, 1, 10));
                  Bind_AADL_To_Default_Value (Identifier (C), N);

                  Nb_Protected := Nb_Protected + 1;
               else
                  --  Visit the component instance corresponding to the
                  --  subcomponent S.
                  Visit (Corresponding_Instance (C));
               end if;

               C := Next_Node (C);
            end loop;
         end if;

         --  For each of the processes P connected to E, (1) we add an
         --  enumerator corresponding to P and (2) for each one of the
         --  threads of P, we add an enumerator.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               --  The sources of F

               if not AAU.Is_Empty (Sources (F)) then
                  Src := First_Node (Sources (F));

                  while Present (Src) loop

                     Parent := Parent_Component (Item (Src));

                     if AAU.Is_Process (Parent) and then Parent /= E then
                        if Get_Provided_Virtual_Bus_Class (Extra_Item (Src)) /=
                          No_Node
                        then
                           Visit
                             (Get_Provided_Virtual_Bus_Class
                                (Extra_Item (Src)));
                        end if;

                        Set_Added (Parent, E);
                        --  Traverse all the subcomponents of Parent

                        if not AAU.Is_Empty (Subcomponents (Parent)) then
                           C := First_Node (Subcomponents (Parent));

                           while Present (C) loop
                              Visit (Corresponding_Instance (C));

                              C := Next_Node (C);
                           end loop;
                        end if;

                     --  Mark P as being Added
                     elsif AAU.Is_Device (Parent) and then Parent /= E then
                        Driver_Name := Get_Driver_Name (Parent);

                        if Driver_Name /= No_Name then
                           Set_Str_To_Name_Buffer ("__PO_HI_NEED_DRIVER_");
                           Get_Name_String_And_Append (Driver_Name);

                           Driver_Name := Name_Find;
                           Driver_Name := To_Upper (Driver_Name);

                           Add_Define_Deployment
                             (Make_Defining_Identifier
                                (Driver_Name,
                                 C_Conversion => False));
                        end if;
                     end if;

                     Src := Next_Node (Src);
                  end loop;
               end if;

               --  The destinations of F

               if not AAU.Is_Empty (Destinations (F)) then
                  Dst := First_Node (Destinations (F));

                  while Present (Dst) loop
                     Parent := Parent_Component (Item (Dst));

                     if AAU.Is_Process (Parent) and then Parent /= E then
                        if Get_Provided_Virtual_Bus_Class (Extra_Item (Dst)) /=
                          No_Node
                        then
                           Visit
                             (Get_Provided_Virtual_Bus_Class
                                (Extra_Item (Dst)));
                        end if;

                        Set_Added (Parent, E);

                        if not AAU.Is_Empty (Subcomponents (Parent)) then
                           C := First_Node (Subcomponents (Parent));

                           while Present (C) loop
                              Visit (Corresponding_Instance (C));

                              C := Next_Node (C);
                           end loop;
                        end if;
                     elsif AAU.Is_Device (Parent) and then Parent /= E then
                        Driver_Name := Get_Driver_Name (Parent);

                        if Driver_Name /= No_Name then
                           Set_Str_To_Name_Buffer ("__PO_HI_NEED_DRIVER_");
                           Get_Name_String_And_Append (Driver_Name);

                           Driver_Name := Name_Find;
                           Driver_Name := To_Upper (Driver_Name);

                           Append_Node_To_List
                             (Make_Define_Statement
                                (Defining_Identifier =>
                                   (Make_Defining_Identifier
                                      (Driver_Name,
                                       C_Conversion => False)),
                                 Value =>
                                   (Make_Literal
                                      (CV.New_Int_Value (1, 1, 10)))),
                              CTN.Declarations (Current_File));
                        end if;
                     end if;

                     Dst := Next_Node (Dst);
                  end loop;
               end if;

               if Is_Data (F) then
                  if Get_Source_Language (Corresponding_Instance (F)) =
                    Language_Simulink
                  then
                     Data := Corresponding_Instance (F);

                     if Get_Source_Name (Data) /= No_Name then
                        N :=
                          Make_Define_Statement
                            (Defining_Identifier => (RE (RE_Simulink_Node)),
                             Value               =>
                               Make_Defining_Identifier
                                 (Get_Source_Name (Data)));
                        Append_Node_To_List
                          (N,
                           CTN.Declarations (Current_File));

                        N :=
                          Make_Define_Statement
                            (Defining_Identifier =>
                               (RE (RE_Simulink_Init_Func)),
                             Value => Map_Simulink_Init_Func (Data));
                        Append_Node_To_List
                          (N,
                           CTN.Declarations (Current_File));

                        N :=
                          Make_Define_Statement
                            (Defining_Identifier =>
                               (RE (RE_Simulink_Model_Type)),
                             Value => Map_Simulink_Model_Type (Data));
                        Append_Node_To_List
                          (N,
                           CTN.Declarations (Current_File));
                     end if;
                  end if;

               end if;

               F := Next_Node (F);
            end loop;
         end if;

         Q := First_Node (Subcomponents (Root_Sys));

         while Present (Q) loop
            if AAU.Is_Process (Corresponding_Instance (Q)) then
               if Is_Added (Corresponding_Instance (Q), E) then
                  N :=
                    Make_Expression
                      (Make_Defining_Identifier (Map_C_Enumerator_Name (Q)),
                       Op_Equal,
                       Make_Literal
                         (CV.New_Int_Value (Node_Identifier, 0, 10)));
                  Append_Node_To_List (N, Node_Enumerator_List);
                  Node_Identifier := Node_Identifier + 1;
                  Nb_Nodes        := Nb_Nodes + 1;
               else
                  N :=
                    Make_Expression
                      (Make_Defining_Identifier (Map_C_Enumerator_Name (Q)),
                       Op_Equal,
                       RE (RE_Unused_Node));
                  Append_Node_To_List (N, Node_Enumerator_List);
               end if;
            end if;

            Q := Next_Node (Q);
         end loop;

         --  Create the node enumeration type declaration. Note that
         --  the type creation is possible even the enumeration list
         --  is incomplete. We can do this in the first traversal
         --  since we are sure that the enumerator list is not empty.

         N :=
           Message_Comment
             ("For each node in the distributed" &
              " application add an enumerator");
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier => RE (RE_Node_T),
              Type_Definition => Make_Enum_Aggregate (Node_Enumerator_List));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  Make sure the __po_hi_protocol_t enum type is defined
         --  with at least the invalid_protocol value.

         if not Invalid_Protocol_Added then
            Set_Str_To_Name_Buffer ("invalid_protocol");
            N :=
              Make_Expression
                (Make_Defining_Identifier (Name_Find),
                 Op_Equal,
                 (Make_Literal (CV.New_Int_Value (1, -1, 10))));
            Append_Node_To_List (N, Protocol_List);

            Invalid_Protocol_Added := True;
         end if;

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier => RE (RE_Protocol_T),
              Type_Definition     => Make_Enum_Aggregate (Protocol_List));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  Create the thread enumeration type declaration. Note that
         --  the type creation is possible even the enumeration list
         --  is incomplete. This type may not be generated in case the
         --  application is local.

         if not Is_Empty (Entity_Enumerator_List) then
            N :=
              Message_Comment
                ("For each thread in the distributed" &
                 " application nodes, add an" &
                 " enumerator");
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            if not Invalid_Entity_Added then
               Set_Str_To_Name_Buffer ("invalid_entity");
               N :=
                 Make_Expression
                   (Make_Defining_Identifier (Name_Find),
                    Op_Equal,
                    (Make_Literal (CV.New_Int_Value (1, -1, 10))));
               Append_Node_To_List (N, Entity_Enumerator_List);
               Invalid_Entity_Added := True;
            end if;

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Entity_T),
                 Type_Definition     =>
                   Make_Enum_Aggregate (Entity_Enumerator_List));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         Set_Str_To_Name_Buffer ("invalid_task_id");
         N :=
           Make_Expression
             (Make_Defining_Identifier (Name_Find),
              Op_Equal,
              (Make_Literal (CV.New_Int_Value (1, -1, 10))));
         Append_Node_To_List (N, Tasks_Enumerator_List);

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier => RE (RE_Task_Id),
              Type_Definition => Make_Enum_Aggregate (Tasks_Enumerator_List));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier => RE (RE_Device_Id),
              Type_Definition     =>
                Make_Enum_Aggregate (Devices_Enumerator_List));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier => RE (RE_Bus_Id),
              Type_Definition => Make_Enum_Aggregate (Buses_Enumerator_List));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Nb_Tasks),
              Value => Make_Literal (New_Int_Value (Task_Identifier, 1, 10)));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Tasks_Stack),
              Value => Make_Literal (New_Int_Value (Tasks_Stack, 1, 10)));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  Add an enumerator corresponding to an INVALID server
         --  entity to the entity list.

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Nb_Protected),
              Value => Make_Literal (New_Int_Value (Nb_Protected, 1, 10)));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Nb_Nodes),
              Value => Make_Literal (New_Int_Value (Nb_Nodes, 1, 10)));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Nb_Entities),
              Value               => Nb_Entities_Node);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  If there are ports in the local process, we generate a
         --  macro indicating the total number of ports in the
         --  application, otherwise we generate a value of 0 to avoid
         --  dragging the whole transport logic. This may happen in
         --  corner cases when using external API for communication.

         if Nb_Ports_In_Process > 0 then
            N :=
              Make_Define_Statement
              (Defining_Identifier => RE (RE_Nb_Ports),
               Value               => Total_Ports_Node);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         else
            N :=
              Make_Define_Statement
              (Defining_Identifier => RE (RE_Nb_Ports),
               Value               => Make_Literal (New_Int_Value (0, 1, 10)));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if not Is_Empty (Global_Port_List) then
            if not Invalid_Global_Port_Added then
               Set_Str_To_Name_Buffer ("invalid_port_t");
               N :=
                 Make_Expression
                   (Make_Defining_Identifier (Name_Find),
                    Op_Equal,
                    (Make_Literal (CV.New_Int_Value (1, -1, 10))));
               Append_Node_To_List (N, Global_Port_List);

               Invalid_Global_Port_Added := True;
            end if;

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Port_T),
                 Type_Definition => Make_Enum_Aggregate (Global_Port_List));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if not Is_Empty (CTN.Values (Global_Port_To_Local)) then
            Bind_AADL_To_Local_Port (Identifier (S), Global_Port_To_Local);
         end if;

         if not Is_Empty (CTN.Values (Global_Port_To_Entity)) then
            Bind_AADL_To_Global_Port (Identifier (S), Global_Port_To_Entity);
         end if;

         if not Is_Empty (CTN.Values (Global_Port_Names)) then
            Bind_AADL_To_Global_Names (Identifier (S), Global_Port_Names);
         end if;

         if not Is_Empty (CTN.Values (Global_Port_Model_Names)) then
            Bind_AADL_To_Global_Model_Names
              (Identifier (S),
               Global_Port_Model_Names);
         end if;

         if not Invalid_Local_Port_Added then
            Set_Str_To_Name_Buffer ("invalid_local_port_t");
            N :=
              Make_Expression
              (Make_Defining_Identifier (Name_Find),
               Op_Equal,
               (Make_Literal (CV.New_Int_Value (1, -1, 10))));
            Append_Node_To_List (N, Local_Port_List);

            Invalid_Local_Port_Added := True;
         end if;

         N :=
           Make_Full_Type_Declaration
           (Defining_Identifier => RE (RE_Local_Port_T),
            Type_Definition     => Make_Enum_Aggregate (Local_Port_List));
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Nb_Devices),
              Value               => Nb_Devices_Node);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Nb_Buses),
              Value               => Nb_Buses_Node);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N :=
           Make_Define_Statement
             (Defining_Identifier => RE (RE_Nb_Protocols),
              Value               => Nb_Protocols_Node);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  Define the PORT_TYPE_CONTENT macro for the monitoring of entities

         declare
            K                  : Node_Id;
            Nb_Ports_List_Name : Name_Id := No_Name;
         begin
            K := CTN.First_Node (Nb_Ports_List);

            if Present (K) then
               Get_Name_String (CTN.Name (K));
               K := CTN.Next_Node (K);
               while Present (K) loop
                  Add_Str_To_Name_Buffer
                    (", " & Get_Name_String (CTN.Name (K)));
                  K := CTN.Next_Node (K);
               end loop;
            end if;
            Nb_Ports_List_Name := Name_Find;

            N :=
              Make_Define_Statement
                (Defining_Identifier => RE (RE_Port_Type_Content),
                 Value => Make_Defining_Identifier (Nb_Ports_List_Name));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end;

         Current_Process_Instance := No_Node;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
         N : Node_Id;
      begin
         Push_Entity (C_Root);

         Global_Ports           := AAU.New_List (K_List_Id, No_Location);
         Devices_Array          := Make_Array_Values;
         Devices_Nb_Buses_Array := Make_Array_Values;
         Devices_Confvars       := Make_Array_Values;
         Protocols_Conf         := Make_Array_Values;
         Devices_Buses_Array    := Make_Array_Values;
         Port_To_Devices        := Make_Array_Values;
         Devices_To_Nodes       := Make_Array_Values;

         Devices_Enumerator_List := New_List (CTN.K_Enumeration_Literals);
         Buses_Enumerator_List   := New_List (CTN.K_Enumeration_Literals);
         Global_Port_List        := New_List (CTN.K_Enumeration_Literals);
         Protocol_List           := New_List (CTN.K_Enumeration_Literals);
         Global_Port_Names       := Make_Array_Values;
         Global_Port_Model_Names := Make_Array_Values;
         Global_Port_Kind        := Make_Array_Values;
         Global_Port_Queue_Size  := Make_Array_Values;
         Global_Port_Data_Size   := Make_Array_Values;
         Global_Port_To_Entity   := Make_Array_Values;
         Global_Port_To_Local    := Make_Array_Values;
         Entity_Enumerator_List  := New_List (CTN.K_Enumeration_Literals);
         Local_Port_Values       := Make_Array_Values;
         Local_Port_List         := New_List (CTN.K_Enumeration_Literals);
         Total_Ports_Node        := Make_Literal (New_Int_Value (0, 1, 10));
         Nb_Entities_Node        := Make_Literal (New_Int_Value (0, 1, 10));
         Nb_Devices_Node         := Make_Literal (New_Int_Value (0, 1, 10));
         Nb_Buses_Node           := Make_Literal (New_Int_Value (0, 1, 10));
         Nb_Protocols_Node       := Make_Literal (New_Int_Value (0, 1, 10));
         Entity_Array            := Make_Array_Values;
         Device_Id               := 0;

         --  Automatically define invalid identifier
         --  for devices and buses. These are fixed identifier
         --  so that developpers can ALWAYS rely on it.

         Set_Str_To_Name_Buffer ("invalid_device_id");
         N :=
           Make_Expression
             (Make_Defining_Identifier (Name_Find),
              Op_Equal,
              (Make_Literal (CV.New_Int_Value (1, -1, 10))));
         Append_Node_To_List (N, Devices_Enumerator_List);

         Set_Str_To_Name_Buffer ("invalid_bus_id");
         N :=
           Make_Expression
             (Make_Defining_Identifier (Name_Find),
              Op_Equal,
              (Make_Literal (CV.New_Int_Value (1, -1, 10))));
         Append_Node_To_List (N, Buses_Enumerator_List);

         --  Visit all the subcomponents of the system

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               if Get_Current_Backend_Kind = PolyORB_HI_C
                 and then not AAU.Is_Device (Corresponding_Instance (S))
               then
                  Visit (Corresponding_Instance (S));
               else
                  Visit (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         Set_Str_To_Name_Buffer ("constant_out_identifier");
         N :=
           Make_Expression
             (Make_Defining_Identifier (Name_Find),
              Op_Equal,
              (Make_Literal
                 (CV.New_Int_Value (Global_Port_Identifier + 1, 1, 10))));
         Append_Node_To_List (N, Global_Port_List);

         CTN.Set_Value
           (Total_Ports_Node,
            New_Int_Value (Nb_Ports_Total, 1, 10));

         CTN.Set_Value
           (Nb_Entities_Node,
            New_Int_Value (Entity_Identifier, 1, 10));

         Pop_Entity; --  C_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         N           : Node_Id;
         F           : Node_Id;
         P           : Node_Id;
         S           : constant Node_Id := Parent_Subcomponent (E);
         Call_Seq    : Node_Id;
         Spg_Call    : Node_Id;
         Used_Bus    : Node_Id;
         Used_Device : Node_Id;
         Impl_Kind   : constant Supported_Thread_Implementation :=
           Get_Thread_Implementation_Kind (E);
         Dispatch_Protocol : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         BA          : Node_Id;
      begin

         Local_Port_Identifier := 0;
         --  Build the enumerator corresponding to the thread. The
         --  enumerator name is mapped from the the thread name and
         --  its containing process name.

         Append_Existing
           (S,
            Entity_Enumerator_List,
            Entity_Identifier,
            Is_Entity => True);

         if not (Present (Backend_Node (Identifier (S))))
           or else
           (Present (Backend_Node (Identifier (S)))
            and then No (CTN.Naming_Node (Backend_Node (Identifier (S)))))
         then
            if Current_Device /= No_Node then
               N :=
                 Make_Defining_Identifier
                   (Map_C_Enumerator_Name
                      (Parent_Subcomponent (Current_Process_Instance)));
            else
               N :=
                 Make_Defining_Identifier
                   (Map_C_Enumerator_Name
                      (Parent_Subcomponent
                         (Parent_Component (Parent_Subcomponent (E)))));
            end if;
            Bind_AADL_To_Naming (Identifier (S), N);
            Append_Node_To_List (N, CTN.Values (Entity_Array));
         end if;

         if Parent_Component (Parent_Subcomponent (E)) =
           Current_Process_Instance
         then
            N :=
              Make_Expression
                (Make_Defining_Identifier
                   (Map_C_Enumerator_Name (S, Entity => False)),
                 Op_Equal,
                 (Make_Literal (CV.New_Int_Value (Task_Identifier, 0, 10))));
            Append_Node_To_List (N, Tasks_Enumerator_List);
            Task_Identifier := Task_Identifier + 1;

            Tasks_Stack := Tasks_Stack + To_Bytes (Get_Thread_Stack_Size (E));

            if Impl_Kind = Thread_With_Behavior_Specification then
               BA := Get_Behavior_Specification (E);
               if BANu.Length (BATN.States (BA)) > 1 then
                  Create_Enum_Type_Of_States_Names (E);
                  Create_State_Type (E);
                  if Dispatch_Protocol = Thread_Sporadic and then
                    Compute_Nb_On_Dispatch_Transitions (E) > 1
                  then
                     N :=
                      Make_Define_Statement
                        (Defining_Identifier =>
                          Make_Defining_Identifier
                           (Map_C_Define_Name
                              (S,
                               Max_Dispatch_Transitions_Per_Complete_State =>
                                 True)),
                         Value =>
                          Make_Literal
                           (New_Int_Value
                           (Compute_Max_Dispatch_Transitions_Per_Complete_State
                              (E),
                            1,
                            10)));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                     N := Make_Define_Statement
                       (Defining_Identifier =>
                          Make_Defining_Identifier
                            (Map_C_Define_Name
                                 (S,
                                  Max_Dispatch_Triggers_Per_Dispatch_Transition
                                  => True)),
                        Value =>
                          Make_Literal
                            (New_Int_Value
                         (Compute_Max_Dispatch_Triggers_Per_Dispatch_Transition
                                    (E), 1, 10)));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));
                  end if;
               end if;
            end if;
         end if;

         if Current_Device /= No_Node
           and then Current_Process_Instance /= No_Node
           and then
             Get_Bound_Processor (Current_Device) =
             Get_Bound_Processor (Current_Process_Instance)
         then
            N :=
              Make_Expression
                (Make_Defining_Identifier
                   (Map_C_Enumerator_Name
                      (S,
                       Custom_Parent => Current_Device,
                       Entity        => False)),
                 Op_Equal,
                 (Make_Literal (CV.New_Int_Value (Task_Identifier, 0, 10))));
            Append_Node_To_List (N, Tasks_Enumerator_List);
            Task_Identifier := Task_Identifier + 1;
            Tasks_Stack := Tasks_Stack + To_Bytes (Get_Thread_Stack_Size (E));
         end if;

         --  Get the Process parent of the thread

         if Current_Device /= No_Node then
            P := Current_Device;
         else
            P := Parent_Component (S);
         end if;

         N :=
           Make_Defining_Identifier
             (Map_C_Enumerator_Name (Parent_Subcomponent (P)));

         if Has_Ports (E) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  if No (Backend_Node (Identifier (F)))
                    or else
                    (Present (Backend_Node (Identifier (F)))
                     and then No
                       (CTN.Local_Port_Node (Backend_Node (Identifier (F)))))
                  then

                     N :=
                       Make_Expression
                         (Make_Defining_Identifier
                            (Map_C_Enumerator_Name (F, Local_Port => True)),
                          Op_Equal,
                          (Make_Literal
                             (CV.New_Int_Value
                                (Local_Port_Identifier,
                                 0,
                                 10))));
                     Append_Node_To_List (N, Local_Port_List);

                     Append_Node_To_List
                       (Make_Defining_Identifier (Map_C_Enumerator_Name (F)),
                        CTN.Values (Local_Port_Values));
                     Bind_AADL_To_Local_Port
                       (Identifier (F),
                        Local_Port_Values);

                     Nb_Ports_Total := Nb_Ports_Total + 1;
                  end if;

                  Local_Port_Identifier := Local_Port_Identifier + 1;
                  Nb_Ports_In_Process   := Nb_Ports_In_Process + 1;

                  if No (Backend_Node (Identifier (F)))
                    or else No
                      (CTN.Global_Port_Node (Backend_Node (Identifier (F))))
                  then
                     N := (Make_Literal (CV.New_Int_Value (1, -1, 10)));

                     Used_Bus := Get_Associated_Bus (F);

                     if Used_Bus /= No_Node then
                        if AAU.Is_Virtual_Bus (Used_Bus) then
                           Used_Bus :=
                             Parent_Component (Parent_Subcomponent (Used_Bus));
                        end if;

                        Used_Device :=
                          Get_Device_Of_Process
                            (Used_Bus,
                             (Parent_Component (Parent_Subcomponent (E))));
                        if Used_Device /= No_Node then
                           N :=
                             Make_Defining_Identifier
                               (Map_C_Enumerator_Name
                                  (Corresponding_Instance (Used_Device)));
                        end if;
                     end if;

                     Append_Node_To_List (N, CTN.Values (Port_To_Devices));

                     N :=
                       Make_Defining_Identifier
                         (Map_C_Enumerator_Name (F, Local_Port => True));
                     Append_Node_To_List
                       (N,
                        CTN.Values (Global_Port_To_Local));

                     N :=
                       Make_Defining_Identifier
                         (Map_C_Enumerator_Name
                            (S,
                             Entity        => True,
                             Custom_Parent => Current_Device));
                     Append_Node_To_List
                       (N,
                        CTN.Values (Global_Port_To_Entity));

                     --  For each feature of a thread, we define a
                     --  global port name as the name of the process
                     --  port.
                     --
                     --  Note: these names are also used by some other
                     --  backend, e.g. as part of TSP configuration.

                     declare
                        F_L : constant List_Id :=
                          (if Is_In (F) then
                             AAN.Sources (F) else
                             AAN.Destinations (F));
                        F_N : constant Node_Id := AAN.First_Node (F_L);
                     begin
                        if Present (F_N) then
                           N :=
                             Make_Literal
                             (CV.New_Pointed_Char_Value
                                (Map_C_Enumerator_Name
                                   (Item (F_N),
                                    Fully_Qualify_Parent => True)));
                        else
                           --  There is no process port (e.g. thread
                           --  to thread connection), then we just use
                           --  the feature name.
                           N :=
                             Make_Literal
                             (CV.New_Pointed_Char_Value
                                (Map_C_Enumerator_Name
                                   (F,
                                    Fully_Qualify_Parent => True)));
                        end if;
                        Append_Node_To_List
                          (N, CTN.Values (Global_Port_Names));
                     end;

                     N :=
                       Make_Literal
                         (CV.New_Pointed_Char_Value
                            (To_Lower (Display_Name (Identifier (F)))));
                     Append_Node_To_List
                       (N,
                        CTN.Values (Global_Port_Model_Names));

                     if Is_In (F) then
                        if Get_Connection_Pattern (F) = Intra_Process then
                           if Is_Data (F) and then Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_In_Event_Data_Intra_Process),
                                 CTN.Values (Global_Port_Kind));
                           elsif Is_Data (F) and then not Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_In_Data_Intra_Process),
                                 CTN.Values (Global_Port_Kind));
                           else
                              Append_Node_To_List
                                (RE (RE_In_Event_Intra_Process),
                                 CTN.Values (Global_Port_Kind));
                           end if;
                        else
                           if Is_Data (F) and then Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_In_Event_Data_Inter_Process),
                                 CTN.Values (Global_Port_Kind));
                           elsif Is_Data (F) and then not Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_In_Data_Inter_Process),
                                 CTN.Values (Global_Port_Kind));
                           else
                              Append_Node_To_List
                                (RE (RE_In_Event_Inter_Process),
                                 CTN.Values (Global_Port_Kind));
                           end if;

                        end if;
                     elsif Is_Out (F) then
                        if Get_Connection_Pattern (F) = Intra_Process then
                           if Is_Data (F) and then Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_Out_Event_Data_Intra_Process),
                                 CTN.Values (Global_Port_Kind));
                           elsif Is_Data (F) and then not Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_Out_Data_Intra_Process),
                                 CTN.Values (Global_Port_Kind));
                           else
                              Append_Node_To_List
                                (RE (RE_Out_Event_Intra_Process),
                                 CTN.Values (Global_Port_Kind));
                           end if;
                        else
                           if Is_Data (F) and then Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_Out_Event_Data_Inter_Process),
                                 CTN.Values (Global_Port_Kind));
                           elsif Is_Data (F) and then not Is_Event (F) then
                              Append_Node_To_List
                                (RE (RE_Out_Data_Inter_Process),
                                 CTN.Values (Global_Port_Kind));
                           else
                              Append_Node_To_List
                                (RE (RE_Out_Event_Inter_Process),
                                 CTN.Values (Global_Port_Kind));
                           end if;
                        end if;
                     end if;

                     if Is_Data (F) then
                        Append_Node_To_List
                          (Get_Data_Size (Corresponding_Instance (F),
                                          Maximum_Size => True),
                           CTN.Values (Global_Port_Data_Size));
                     end if;

                     --  Map port queue size in Global_Port_Size We
                     --  consider process ports, and then thread port
                     --  when computing queue size. This is to be
                     --  consistent with process (or TSP partitions)
                     --  defining a queue size for inter-partition
                     --  communications.

                     if Is_Event (F) then
                        declare
                           F_L : constant List_Id :=
                             (if Is_In (F) then
                             AAN.Sources (F) else
                             AAN.Destinations (F));
                           F_N : constant Node_Id := AAN.First_Node (F_L);
                        begin
                           if Present (F_N)  and then
                             Is_Event (Item (F_N)) and then
                             Get_Queue_Size (Item (F_N)) /= -1
                           then
                              Append_Node_To_List
                                (Make_Literal
                                   (CV.New_Int_Value
                                      (Unsigned_Long_Long
                                         (Get_Queue_Size (Item (F_N))),
                                       0,
                                       10)),
                                 CTN.Values (Global_Port_Queue_Size));
                           else
                              Append_Node_To_List
                                (Make_Literal (CV.New_Int_Value (1, 0, 10)),
                                 CTN.Values (Global_Port_Queue_Size));
                           end if;
                        end;
                     else
                        if Is_Event (F) and then Get_Queue_Size (F) /= -1 then
                           Append_Node_To_List
                             (Make_Literal
                                (CV.New_Int_Value
                                   (Unsigned_Long_Long (Get_Queue_Size (F)),
                                    0,
                                    10)),
                              CTN.Values (Global_Port_Queue_Size));
                        else
                           Append_Node_To_List
                             (Make_Literal (CV.New_Int_Value (1, 0, 10)),
                              CTN.Values (Global_Port_Queue_Size));
                        end if;
                     end if;

                     --  We associate a unique identifier to the port
                     --  within the global distributed architecture.

                     N :=
                       Make_Expression
                         (Make_Defining_Identifier (Map_C_Enumerator_Name (F)),
                          Op_Equal,
                          (Make_Literal
                             (CV.New_Int_Value
                                (Global_Port_Identifier,
                                 0,
                                 10))));
                     Append_Node_To_List (N, Global_Port_List);

                     --  We also store the port in a list to process
                     --  it later. By doing so, we ensure that we have
                     --  each port only one time in this time and in
                     --  the same order than the identifiers.

                     AAU.Append_Node_To_List
                       (AAU.Make_Node_Container (F),
                        Global_Ports);

                     --  Finally, we associate the backend node
                     --  with something to indicate that we already
                     --  processed this port. It avoids any double
                     --  processing of the same port.

                     Bind_AADL_To_Global_Port
                       (Identifier (F),
                        Make_Defining_Identifier (Map_C_Enumerator_Name (F)));

                     Global_Port_Identifier := Global_Port_Identifier + 1;
                  end if;

               end if;

               F := Next_Node (F);
            end loop;

            if Parent_Component (Parent_Subcomponent (E)) =
              Current_Process_Instance
              or else
              (Current_Device /= No_Node
               and then
                 Get_Bound_Processor (Current_Device) =
                 Get_Bound_Processor (Current_Process_Instance))
            then
               N :=
                 Make_Define_Statement
                   (Defining_Identifier =>
                      Make_Defining_Identifier
                        (Map_C_Define_Name (S, Nb_Ports => True)),
                    Value =>
                      Make_Literal
                        (New_Int_Value (Local_Port_Identifier, 1, 10)));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               N :=
                 Make_Defining_Identifier
                   (Map_C_Define_Name (S, Nb_Ports => True));
               Append_Node_To_List (N, Nb_Ports_List);
            end if;
         end if;

         if Parent_Component (Parent_Subcomponent (E)) =
           Current_Process_Instance
           and then not AAU.Is_Empty (Calls (E))
         then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;

      end Visit_Thread_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         N : Node_Id;
         S : constant Node_Id := Parent_Subcomponent (E);
      begin
         if Get_Subprogram_Kind (E) = Subprogram_Simulink then

            if Get_Source_Name (E) = No_Name then
               Display_Error
                 ("Simulink subprogram must have a" & " source_name property",
                  Fatal => True);
            end if;

            if not (Present (Backend_Node (Identifier (S))))
              or else
              (Present (Backend_Node (Identifier (S)))
               and then No (CTN.Naming_Node (Backend_Node (Identifier (S)))))
            then
               N :=
                 Make_Define_Statement
                   (Defining_Identifier => (RE (RE_Simulink_Node)),
                    Value => Make_Defining_Identifier (Get_Source_Name (E)));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               N :=
                 Make_Define_Statement
                   (Defining_Identifier => (RE (RE_Simulink_Init_Func)),
                    Value               => Map_Simulink_Init_Func (E));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               N :=
                 Make_Define_Statement
                   (Defining_Identifier => (RE (RE_Simulink_Model_Type)),
                    Value               => Map_Simulink_Model_Type (E));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               Bind_AADL_To_Naming (Identifier (S), N);
            end if;

         end if;
      end Visit_Subprogram_Instance;

   end Header_File;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);

      Protocols_Ports_Array : Node_Id;

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
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
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
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P        : constant Node_Id := CTN.Entity (U);
         S        : constant Node_Id := Parent_Subcomponent (E);
         N        : Node_Id;
         Q        : Node_Id;
         C        : Node_Id;
         Root_Sys : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Endiannesses            : constant Node_Id := Make_Array_Values;
         Execution_Platform      : Supported_Execution_Platform;
         Protected_Configuration : constant Node_Id := Make_Array_Values;
         Protected_Priorities    : constant Node_Id := Make_Array_Values;
         Protected_Protocol      : Node_Id;
         Protected_Priority      : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Deployment_Source;

         if not AAU.Is_Empty (Subcomponents (E)) then
            C := First_Node (Subcomponents (E));

            while Present (C) loop
               if AAU.Is_Data (Corresponding_Instance (C)) then
                  Protected_Priority :=
                    Make_Literal (New_Int_Value (0, 1, 10));

                  case Get_Concurrency_Protocol (Corresponding_Instance (C)) is
                     when Priority_Ceiling =>
                        Protected_Priority :=
                          Make_Literal
                            (New_Int_Value
                               (Get_Priority_Celing_Of_Data_Access
                                  (Corresponding_Instance (C)),
                                1,
                                10));
                        Protected_Protocol := RE (RE_Protected_PCP);

                     when others =>
                        Protected_Protocol := RE (RE_Protected_Regular);
                  end case;

                  Append_Node_To_List
                    (Protected_Protocol,
                     CTN.Values (Protected_Configuration));

                  Append_Node_To_List
                    (Protected_Priority,
                     CTN.Values (Protected_Priorities));
               end if;

               Visit (Corresponding_Instance (C));
               C := Next_Node (C);
            end loop;
         end if;

         if not Is_Empty (CTN.Values (Protected_Configuration)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Protected_Configuration),
                           Array_Size => RE (RE_Nb_Protected)),
                      Used_Type => RE (RE_Protected_Protocol_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Protected_Configuration);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Protected_Priorities),
                           Array_Size          => RE (RE_Nb_Protected)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Protected_Priorities);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if Present (Backend_Node (Identifier (S)))
           and then Present
             (CTN.Global_Port_Node (Backend_Node (Identifier (S))))
         then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Port_Global_To_Entity),
                           Array_Size => RE (RE_Nb_Ports)),
                      Used_Type => RE (RE_Entity_T)),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   CTN.Global_Port_Node (Backend_Node (Identifier (S))));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if Present (Backend_Node (Identifier (S)))
           and then Present
             (CTN.Global_Names_Node (Backend_Node (Identifier (S))))
         then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Port_Global_Names),
                           Array_Size          => RE (RE_Nb_Ports)),
                      Used_Type =>
                        CTU.Make_Pointer_Type (New_Node (CTN.K_Char))),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   CTN.Global_Names_Node (Backend_Node (Identifier (S))));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if Present (Backend_Node (Identifier (S)))
           and then Present
             (CTN.Global_Model_Names_Node (Backend_Node (Identifier (S))))
         then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Port_Global_Model_Names),
                           Array_Size => RE (RE_Nb_Ports)),
                      Used_Type =>
                        CTU.Make_Pointer_Type (New_Node (CTN.K_Char))),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   CTN.Global_Model_Names_Node
                     (Backend_Node (Identifier (S))));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Port_Global_Kind),
                           Array_Size          => RE (RE_Nb_Ports)),
                      Used_Type => RE (RE_Port_Kind_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Global_Port_Kind);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Port_Global_Data_Size),
                           Array_Size => RE (RE_Nb_Ports)),
                      Used_Type => RE (RE_Uint32_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Global_Port_Data_Size);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Port_Global_Queue_Size),
                           Array_Size => RE (RE_Nb_Ports)),
                      Used_Type => RE (RE_Uint32_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Global_Port_Queue_Size);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            --  Add the array that contains the protocols
            --  used for each port.

            if not CTU.Is_Empty (CTN.Values (Protocols_Ports_Array))
              and then not CTU.Is_Empty
                (CTN.Values
                   (CTN.First_Node (CTN.Values (Protocols_Ports_Array))))
              and then not AAU.Is_Empty (Global_Ports)
              and then Protocol_Identifier > 0
            then
               N :=
                 Make_Expression
                   (Left_Expr =>
                      Make_Variable_Declaration
                        (Defining_Identifier =>
                           Make_Array_Declaration
                             (Defining_Identifier =>
                                Make_Array_Declaration
                                  (Defining_Identifier =>
                                     RE (RE_Ports_Protocols),
                                   Array_Size => RE (RE_Nb_Ports)),
                              Array_Size => RE (RE_Nb_Ports)),
                         Used_Type => RE (RE_Protocol_T)),
                    Operator   => Op_Equal,
                    Right_Expr => Protocols_Ports_Array);
               Append_Node_To_List (N, CTN.Declarations (Current_File));
            end if;
         end if;

         if Present (Backend_Node (Identifier (S)))
           and then Present
             (CTN.Local_Port_Node (Backend_Node (Identifier (S))))
         then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Port_Global_To_Local),
                           Array_Size          => RE (RE_Nb_Ports)),
                      Used_Type => RE (RE_Local_Port_T)),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   CTN.Local_Port_Node (Backend_Node (Identifier (S))));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

--         if Present (Backend_Node (Identifier (S))) and then
--           Present (CTN.Entities_Node (Backend_Node (Identifier (S)))) then
         N :=
           Make_Expression
             (Left_Expr =>
                Make_Variable_Declaration
                  (Defining_Identifier =>
                     Make_Array_Declaration
                       (Defining_Identifier => RE (RE_Entity_Table),
                        Array_Size          => RE (RE_Nb_Entities)),
                   Used_Type => RE (RE_Node_T)),
              Operator   => Op_Equal,
              Right_Expr => Entity_Array);
--              CTN.Entities_Node (Backend_Node (Identifier (S))));
         Append_Node_To_List (N, CTN.Declarations (Current_File));
--         end if;

         Q := First_Node (Subcomponents (Root_Sys));

         while Present (Q) loop
            if AAU.Is_Process (Corresponding_Instance (Q))
              and then Is_Added (Corresponding_Instance (Q), E)
            then
               Execution_Platform :=
                 Get_Execution_Platform
                   (Get_Bound_Processor (Corresponding_Instance (Q)));
               case Execution_Platform is
                  when Platform_Native              |
                    Platform_None                   |
                    Platform_LINUX32_XENOMAI_NATIVE |
                    Platform_Native_Compcert        |
                    Platform_LINUX32_XENOMAI_POSIX  |
                    Platform_WIN32                  |
                    Platform_LINUX_DLL              |
                    Platform_LINUX64                |
                    Platform_LINUX32                =>
                     Append_Node_To_List
                       (RE (RE_Littleendian),
                        CTN.Values (Endiannesses));

                  when Platform_AIR              |
                    Platform_LEON_RTEMS          |
                    Platform_LEON_RTEMS_POSIX    |
                    Platform_LEON_ORK            |
                    Platform_LEON3_XM3           =>
                     Append_Node_To_List
                       (RE (RE_Bigendian),
                        CTN.Values (Endiannesses));

                  when others =>
                     Append_Node_To_List
                       (RE (RE_Bigendian),
                        CTN.Values (Endiannesses));
                     Display_Error
                       ("Unknown endianess of " & Execution_Platform'Img,
                        Fatal => False);
               end case;
            end if;
            Q := Next_Node (Q);
         end loop;

         N :=
           Make_Expression
             (Left_Expr =>
                Make_Variable_Declaration
                  (Defining_Identifier =>
                     Make_Array_Declaration
                       (Defining_Identifier => RE (RE_Deployment_Endiannesses),
                        Array_Size          => RE (RE_Nb_Nodes)),
                   Used_Type => RE (RE_Uint8_T)),
              Operator   => Op_Equal,
              Right_Expr => Endiannesses);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         if not Is_Empty (CTN.Values (Devices_Array)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Devices_Naming),
                           Array_Size          => RE (RE_Nb_Devices)),
                      Used_Type => Make_Pointer_Type (New_Node (CTN.K_Char))),
                 Operator   => Op_Equal,
                 Right_Expr => Devices_Array);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         --  Here, we define an array that contains reference
         --  to all configuration variables.

         if not Is_Empty (CTN.Values (Devices_Confvars)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Devices_Configuration_Values),
                           Array_Size => RE (RE_Nb_Devices)),
                      Used_Type => Make_Pointer_Type (RE (RE_Uint32_T))),
                 Operator   => Op_Equal,
                 Right_Expr => Devices_Confvars);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if not Is_Empty (CTN.Values (Protocols_Conf)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Protocols_Configuration),
                           Array_Size => RE (RE_Nb_Protocols)),
                      Used_Type => RE (RE_Protocol_Conf_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Protocols_Conf);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         --  In the following, we add arrays previously filled arrays
         --  in the header part of the package. These arrays describe
         --  the number of buses accessed by each device and which
         --  bus is under control of which device.

         if not Is_Empty (CTN.Values (Devices_Nb_Buses_Array)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Devices_Nb_Accessed_Buses),
                           Array_Size => RE (RE_Nb_Devices)),
                      Used_Type => RE (RE_Uint32_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Devices_Nb_Buses_Array);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if not Is_Empty (CTN.Values (Devices_Buses_Array)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Devices_Accessed_Buses),
                           Array_Size => RE (RE_Nb_Devices)),
                      Used_Type => Make_Pointer_Type (RE (RE_Bus_Id))),
                 Operator   => Op_Equal,
                 Right_Expr => Devices_Buses_Array);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         --  In the following, we describe the association between
         --  ports and devices. It corresponds to the
         --  __po_hi_port_to_devices array generated in deployment.c.

         if not Is_Empty (CTN.Values (Port_To_Devices)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Port_To_Device),
                           Array_Size          => RE (RE_Nb_Ports)),
                      Used_Type => RE (RE_Device_Id)),
                 Operator   => Op_Equal,
                 Right_Expr => Port_To_Devices);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         --  In the following, we describe the association between
         --  devices and their nodes.

         if not Is_Empty (CTN.Values (Devices_To_Nodes)) then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Devices_To_Nodes),
                           Array_Size          => RE (RE_Nb_Devices)),
                      Used_Type => RE (RE_Port_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Devices_To_Nodes);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
      begin
         if Present (Backend_Node (Identifier (S)))
           and then Present
             (CTN.Local_Port_Node (Backend_Node (Identifier (S))))
         then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (Map_C_Variable_Name
                                  (S,
                                   Port_Variable => True)),
                           Array_Size =>
                             Make_Defining_Identifier
                               (Map_C_Define_Name (S, Nb_Ports => True))),
                      Used_Type => RE (RE_Port_T)),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   CTN.Local_Port_Node (Backend_Node (Identifier (S))));
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;
      end Visit_Thread_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S             : Node_Id;
         S2            : Node_Id;
         A             : Node_Id;
         Lst           : List_Id;
         Tmp           : Node_Id;
         Port          : Node_Id;
         Port2         : Node_Id;
         Virtual_Bus   : Node_Id;
         Protocol_Name : Name_Id;
      begin
         Push_Entity (C_Root);

         Protocols_Ports_Array := Make_Array_Values;

         if not AAU.Is_Empty (Global_Ports) then
            S := AAN.First_Node (Global_Ports);
            while Present (S) loop
               A := Make_Array_Values;

               if S = No_Node then
                  Display_Located_Error
                    (AAN.Loc (S),
                     "Port is not connected",
                     Fatal => True);
               end if;

               Port := AAN.Item (S);

               if Port = No_Node then
                  Display_Located_Error
                    (AAN.Loc (Port),
                     "Port is not connected",
                     Fatal => True);
               end if;

               if Is_In (Port) and then not AAU.Is_Empty (Sources (Port)) then
                  Port := Item (AAN.First_Node (Sources (Port)));
                  Lst  := Sources (Port);
               else
                  if AAU.Is_Empty (Destinations (Port)) then
                     Display_Located_Error
                       (AAN.Loc (Port),
                        "Port destination empty",
                        Fatal => True);
                  end if;

                  Port := Item (AAN.First_Node (Destinations (Port)));
                  Lst  := Destinations (Port);
               end if;

               S2 := AAN.First_Node (Global_Ports);
               while Present (S2) loop
                  Port2 := AAN.Item (S2);
                  if not AAU.Is_Empty (Sources (Port2)) then
                     Port2 := Item (AAN.First_Node (Sources (Port2)));
                  else
                     if AAU.Is_Empty (Destinations (Port2)) then
                        Display_Located_Error
                          (AAN.Loc (Port2),
                           "Port destination empty",
                           Fatal => True);
                     end if;

                     Port2 := Item (AAN.First_Node (Destinations (Port2)));
                  end if;

                  Tmp := First_Node (Lst);

                  Protocol_Name := Get_String_Name ("invalid_protocol");

                  while Present (Tmp) loop
                     if AAN.Item (Tmp) = Port2
                       and then Extra_Item (Tmp) /= No_Node
                       and then
                         Get_Provided_Virtual_Bus_Class (Extra_Item (Tmp)) /=
                         No_Node
                     then
                        Virtual_Bus :=
                          Get_Provided_Virtual_Bus_Class (Extra_Item (Tmp));
                        Protocol_Name := Map_C_Enumerator_Name (Virtual_Bus);
                     end if;
                     Tmp := Next_Node (Tmp);
                  end loop;

                  Append_Node_To_List
                    (Make_Defining_Identifier (Protocol_Name),
                     CTN.Values (A));

                  S2 := Next_Node (S2);
               end loop;

               Append_Node_To_List (A, CTN.Values (Protocols_Ports_Array));
               S := Next_Node (S);
            end loop;
         end if;

         --  Visit all the subcomponents of the system

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  C_Root
      end Visit_System_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_C.Deployment;
