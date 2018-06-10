------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . P O _ H I _ C . N A M I N G       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.C_Tree.Nutils;

package body Ocarina.Backends.PO_HI_C.Naming is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.Properties;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CV renames Ocarina.Backends.C_Values;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

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
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
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
         P        : constant Node_Id := Map_HI_Node (E);
         U        : Node_Id;
         Root_Sys : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Platform_Name : constant Name_Id :=
           Get_Execution_Platform (Get_Bound_Processor (E));
      begin
         pragma Assert (AAU.Is_System (Root_Sys));

         if Platform_Name = No_Name then
            Display_Located_Error
              (Loc (Parent_Subcomponent (E)),
               "This process subcomponent is bound to a processor without" &
               " execution platform specification",
               Fatal => True);
         end if;

         Push_Entity (P);
         U := Map_HI_Unit (E);
         Push_Entity (U);

         Set_Naming_Header (U);

         Pop_Entity;
         Pop_Entity;
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         A : Node_Id;
         C : Node_Id;
         S : Node_Id;
      begin
         if No (C_Root) then
            --  In the case of deep hierarchy of systems, we store in
            --  C_Root the root of the C trees, and reuse it for other
            --  systems.
            C_Root := Map_Distributed_Application (E);
         end if;

         A := C_Root;
         Push_Entity (A);

         --  Verify the consistency of the distributed application
         --  hierachy.

         if not AAU.Is_Empty (Connections (E)) then
            C := First_Node (Connections (E));
            while Present (C) loop
               Check_Connection_Consistency (C);

               C := Next_Node (C);
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

         Pop_Entity; --  A
      end Visit_System_Instance;

   end Header_File;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      function Added_Internal_Name (P : Node_Id; E : Node_Id) return Name_Id;
      function Is_Added (P : Node_Id; E : Node_Id) return Boolean;
      procedure Set_Added (P : Node_Id; E : Node_Id);

      Inetport_Enumerator_List : Node_Id;
      Inetaddr_Enumerator_List : Node_Id;

      -------------------------
      -- Added_Internal_Name --
      -------------------------

      function Added_Internal_Name (P : Node_Id; E : Node_Id) return Name_Id is
      begin
         Set_Str_To_Name_Buffer ("%naming%info%");
         Add_Nat_To_Name_Buffer (Nat (P));
         Add_Char_To_Name_Buffer ('%');
         Add_Nat_To_Name_Buffer (Nat (E));

         return Name_Find;
      end Added_Internal_Name;

      --------------
      -- Is_Added --
      --------------

      function Is_Added (P : Node_Id; E : Node_Id) return Boolean is
         I_Name : constant Name_Id := Added_Internal_Name (P, E);
      begin
         return Get_Name_Table_Byte (I_Name) = 1;
      end Is_Added;

      ---------------
      -- Set_Added --
      ---------------

      procedure Set_Added (P : Node_Id; E : Node_Id) is
         I_Name : constant Name_Id := Added_Internal_Name (P, E);
      begin
         Set_Name_Table_Byte (I_Name, 1);
      end Set_Added;

      -------------------------------
      -- Socket_Naming_Information --
      -------------------------------

      procedure Socket_Naming_Information (E : Node_Id) is
         Location    : Name_Id;
         Port_Number : Value_Id;
         L           : Node_Id;
         P           : Node_Id;
      begin
         pragma Assert (AAU.Is_Process (E));

         Location    := Get_Location (Get_Bound_Processor (E));
         Port_Number := Get_Port_Number (E);

         --  If the node does not have a port number, we don't assign
         --  information to it.

         if Port_Number = CV.No_Value then
            L := RE (RE_Noaddr);
            P := RE (RE_Noport);
         else
            --  Every node that has a port number must be bound to a
            --  processor that have a location.

            if Location = No_Name then
               Display_Located_Error
                 (Loc (Parent_Subcomponent (E)),
                  "A process that has a port number must be bound" &
                  " to a processor that has a location",
                  Fatal => True);
            end if;

            L := Make_Literal (CV.New_Pointed_Char_Value (Location));

            P := Make_Literal (CV.To_C_Value (Port_Number));
         end if;

         Append_Node_To_List (P, CTN.Values (Inetport_Enumerator_List));
         Append_Node_To_List (L, CTN.Values (Inetaddr_Enumerator_List));
      end Socket_Naming_Information;

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
         P                : constant Node_Id := CTN.Entity (U);
         N                : Node_Id;
         S                : Node_Id;
         F                : Node_Id;
         B                : Node_Id;
         C                : Node_Id;
         C_End            : Node_Id;
         End_List         : List_Id;
         Parent           : Node_Id;
         Accessing_Device : Node_Id;
         Driver_Name      : Name_Id;
         Root_Sys         : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Transport_API : Supported_Transport_APIs := Transport_None;
         My_Node       : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);

         Inetport_Enumerator_List := Make_Array_Values;
         Inetaddr_Enumerator_List := Make_Array_Values;

         Set_Naming_Source;

         Set_Added (E, E);

         My_Node :=
           CTU.Make_Defining_Identifier
             (Map_C_Enumerator_Name (Parent_Subcomponent (E)));

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop

               --  We make two iteration to traverse (1) the sources
               --  of F then (2) the destinations of F.

               End_List := Sources (F);

               for I in Boolean'Range loop
                  if not AAU.Is_Empty (End_List) then
                     C_End := First_Node (End_List);

                     while Present (C_End) loop
                        Parent := Parent_Component (Item (C_End));

                        if AAU.Is_Process (Parent) then
                           if Parent /= E then
                              --  Mark the parent component of the
                              --  remote feature as involved with the
                              --  current process.

                              Set_Added (Parent, E);
                           end if;

                           --  Get the connection involving C_End

                           C := Extra_Item (C_End);

                           if No (C) then
                              --  There has been definitly a bug while
                              --  expanding connections.

                              raise Program_Error
                                with "Wrong expansion of connections";
                           end if;

                           --  Get the bus of the connection

                           if ((Get_Execution_Platform
                                  (Get_Bound_Processor (Parent)) =
                                  Platform_Air)
                                 or else
                                 (Get_Execution_Platform
                                    (Get_Bound_Processor (E)) =
                                    Platform_LEON3_XM3
                                    and then
                                    Get_Execution_Platform
                                      (Get_Bound_Processor (Parent)) =
                                    Platform_LEON3_XM3))
                             and then
                             Parent_Component
                               (Parent_Subcomponent
                                  (Get_Bound_Processor (E))) =
                             Parent_Component
                             (Parent_Subcomponent
                                (Get_Bound_Processor (Parent)))

                           then
                              B             := No_Node;
                              Transport_API := Transport_None;

                           else
                              B := Get_Bound_Bus (C);
                           end if;

                           --  Get the transport layer of the Bus and
                           --  verify that all the features use the
                           --  same transport layer for thir
                           --  connections.

                           if Transport_API /= Transport_None
                             and then Transport_API /= Get_Transport_API (B, E)
                           then
                              Display_Located_Error
                                (Loc (Parent_Subcomponent (E)),
                                 "The features of this process are involved" &
                                 " in connetions that do not use the same" &
                                 " transport layer. This is not supported" &
                                 " for now.",
                                 Fatal => True);
                           else
                              Transport_API := Get_Transport_API (B, E);

                              --  If we have a bus for which no
                              --  transport layer has been specified,
                              --  we raise an error.

                              if B /= No_Node
                                and then Transport_API = Transport_None
                              then
                                 Display_Located_Error
                                   (Loc (B),
                                    "No transport layer has been specified" &
                                    " for this bus",
                                    Fatal => True);
                              end if;
                           end if;

                           case Transport_API is
                              when Transport_BSD_Sockets =>
                                 Set_Deployment_Header;

                                 Add_Define_Deployment
                                   (RE (RE_Need_Driver_Sockets));

                              when Transport_User =>
                                 if AAU.Is_Bus (B) then
                                    Accessing_Device :=
                                      Get_Device_Of_Process (B, E);
                                 elsif AAU.Is_Virtual_Bus (B) then
                                    Accessing_Device :=
                                      Get_Device_Of_Process
                                        (Parent_Component
                                           (Parent_Subcomponent (B)),
                                         E);
                                 else
                                    Display_Located_Error
                                      (Loc (B),
                                       "Unknown bus kind !",
                                       Fatal => True);
                                 end if;

                                 if Accessing_Device = No_Node then
                                    Display_Located_Error
                                      (Loc (B),
                                       "No device is accessing this bus !",
                                       Fatal => True);
                                 end if;

                                 Driver_Name :=
                                   Get_Driver_Name
                                     (Corresponding_Instance
                                        (Accessing_Device));

                                 if Driver_Name = No_Name then
                                    Display_Located_Error
                                      (Loc (B),
                                       "Driver must have a name" &
                                       "(see Deployment::Driver_Name) !",
                                       Fatal => True);
                                 end if;

                                 Set_Str_To_Name_Buffer
                                   ("__PO_HI_NEED_DRIVER_");
                                 Get_Name_String_And_Append (Driver_Name);

                                 Driver_Name := Name_Find;
                                 Driver_Name := To_Upper (Driver_Name);

                                 Add_Define_Deployment
                                   (Make_Defining_Identifier
                                      (Driver_Name,
                                       C_Conversion => False));

                              when others =>
                                 null;
                           end case;
                        end if;

                        C_End := Next_Node (C_End);
                     end loop;
                  end if;

                  --  In the next iteration, we traverse the
                  --  Destinations of F.

                  End_List := Destinations (F);
               end loop;

               F := Next_Node (F);
            end loop;
         end if;

         Bind_Transport_API (E, Transport_API);

         case Transport_API is
            when Transport_BSD_Sockets =>
               --  Build the node information for all the application
               --  nodes involved with the current one and append it
               --  to the naming list.

               Set_Deployment_Header;

               Add_Define_Deployment (RE (RE_Need_Driver_Sockets));

               Set_Naming_Source;

               --  Here, by default, we activate the socket
               --  transport layer. This is the default behavior
               --  if no device is specified for transport concerns.

               S := First_Node (Subcomponents (Root_Sys));

               while Present (S) loop
                  if AAU.Is_Process (Corresponding_Instance (S))
                    and then Is_Added (Corresponding_Instance (S), E)
                  then
                     Socket_Naming_Information (Corresponding_Instance (S));
                  end if;

                  S := Next_Node (S);
               end loop;

               --  Declare the Naming Table

               N := Message_Comment ("Naming Table");
               Append_Node_To_List (N, CTN.Declarations (Current_File));

            when Transport_User =>
               if AAU.Is_Bus (B) then
                  Accessing_Device := Get_Device_Of_Process (B, E);
               elsif AAU.Is_Virtual_Bus (B) then
                  Accessing_Device :=
                    Get_Device_Of_Process
                      (Parent_Component (Parent_Subcomponent (B)),
                       E);
               else
                  Display_Located_Error
                    (Loc (B),
                     "Unknown bus kind !",
                     Fatal => True);
               end if;

               if Accessing_Device = No_Node then
                  Display_Located_Error
                    (Loc (B),
                     "No device is accessing this bus !",
                     Fatal => True);
               end if;

               Driver_Name :=
                 Get_Driver_Name (Corresponding_Instance (Accessing_Device));

               if Driver_Name = No_Name then
                  Display_Located_Error
                    (Loc (B),
                     "Driver must have a name" &
                     "(see Deployment::Driver_Name) !",
                     Fatal => True);
               end if;

               Set_Deployment_Header;
               Set_Str_To_Name_Buffer ("__PO_HI_NEED_DRIVER_");
               Get_Name_String_And_Append (Driver_Name);

               Driver_Name := Name_Find;
               Driver_Name := To_Upper (Driver_Name);

               Add_Define_Deployment
                 (Make_Defining_Identifier
                    (Driver_Name,
                     C_Conversion => False));

               Set_Naming_Source;

            when others =>
               --  If we did not fetch a meaningful transport layer,
               --  this means the application does not use the
               --  network. No naming table will be generated.

               null;
         end case;

         N :=
           Make_Expression
             (Left_Expr =>
                Make_Variable_Declaration
                  (Defining_Identifier => RE (RE_Mynode),
                   Used_Type           => RE (RE_Node_T)),
              Operator   => Op_Equal,
              Right_Expr => My_Node);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (C_Root);

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

end Ocarina.Backends.PO_HI_C.Naming;
