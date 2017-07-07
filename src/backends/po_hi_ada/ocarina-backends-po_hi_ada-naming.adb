------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P O _ H I _ A D A . N A M I N G     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Instances.Queries;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.PO_HI_Ada.Mapping;
with Ocarina.Backends.PO_HI_Ada.Runtime;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;

with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_HI_Ada.Naming is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.PO_HI_Ada.Mapping;
   use Ocarina.Backends.PO_HI_Ada.Runtime;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Ada_Values;
   use Ocarina.Instances.Queries;

   package ADV renames Ocarina.Backends.Ada_Values;
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

      function Added_Internal_Name
        (P : Node_Id;
         B : Node_Id;
         E : Node_Id) return Name_Id;
      function Is_Added (P : Node_Id; B : Node_Id; E : Node_Id) return Boolean;
      procedure Set_Added (P : Node_Id; B : Node_Id; E : Node_Id);
      --  Used to ensure that the naming information are added only
      --  for the nodes connected to a particular node.

      function Naming_Information (E : Node_Id) return Node_Id;
      --  Build an array element association that contains the
      --  informations about a particular node of the distributed
      --  application.

      -------------------------
      -- Added_Internal_Name --
      -------------------------

      function Added_Internal_Name
        (P : Node_Id;
         B : Node_Id;
         E : Node_Id) return Name_Id
      is
      begin
         Set_Str_To_Name_Buffer ("%naming%info%");
         Add_Nat_To_Name_Buffer (Nat (P));
         Add_Char_To_Name_Buffer ('%');
         Add_Nat_To_Name_Buffer (Nat (B));
         Add_Char_To_Name_Buffer ('%');
         Add_Nat_To_Name_Buffer (Nat (E));

         return Name_Find;
      end Added_Internal_Name;

      --------------
      -- Is_Added --
      --------------

      function Is_Added
        (P : Node_Id;
         B : Node_Id;
         E : Node_Id) return Boolean
      is
         I_Name : constant Name_Id := Added_Internal_Name (P, B, E);
      begin
         return Get_Name_Table_Byte (I_Name) = 1;
      end Is_Added;

      ---------------
      -- Set_Added --
      ---------------

      procedure Set_Added (P : Node_Id; B : Node_Id; E : Node_Id) is
         I_Name : constant Name_Id := Added_Internal_Name (P, B, E);
      begin
         Set_Name_Table_Byte (I_Name, 1);
      end Set_Added;

      ------------------------
      -- Naming_Information --
      ------------------------

      function Naming_Information (E : Node_Id) return Node_Id is
         Location           : Name_Id;
         Port_Number        : Value_Id;
         N                  : Node_Id;
         L                  : Node_Id;
         P                  : Node_Id;
         V                  : Node_Id;
         Configuration_Data : Name_Id := No_Name;

      begin
         if AAU.Is_Process (E) then
            Location    := Get_Location (Get_Bound_Processor (E));
            Port_Number := Get_Port_Number (E);

         elsif AAU.Is_Device (E) then
            Location           := Get_Location (E);
            Port_Number        := Get_Port_Number (E);
            Configuration_Data := Get_Type_Source_Name (E);
         end if;

         if Location = No_Name then
            if Is_Defined_Property (E, "deployment::configuration")
              and then
                Get_String_Property (E, "deployment::configuration") /=
                No_Name
            then
               Get_Name_String
                 (Get_String_Property (E, "deployment::configuration"));
               L :=
                 Make_Subprogram_Call
                   (RE (RE_To_HI_String),
                    Make_List_Id
                      (Make_Literal (New_String_Value (Name_Find))));
            else
               L :=
                 Make_Subprogram_Call
                   (RE (RE_To_HI_String),
                    Make_List_Id (Make_Literal (New_String_Value (No_Name))));
            end if;
         else
            L :=
              Make_Subprogram_Call
                (RE (RE_To_HI_String),
                 Make_List_Id (Make_Literal (New_String_Value (Location))));
         end if;

         if Port_Number = ADV.No_Value then
            P := Make_Literal (New_Integer_Value (0, 1, 10));
         else
            P := Make_Literal (To_Ada_Value (Port_Number));
         end if;

         if Configuration_Data = No_Name then
            V := RE (RE_Null_Address);
         else
            V :=
              Make_Attribute_Designator
                (Map_Ada_Subprogram_Identifier (Configuration_Data),
                 A_Address);
         end if;

         --  Build the record aggregate

         N := Make_Record_Aggregate (Make_List_Id (L, P, V));
         N := Make_Element_Association (Extract_Enumerator (E), N);
         return N;
      end Naming_Information;

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

      ------------------------
      -- Visit_Bus_Instance --
      ------------------------

      procedure Visit_Bus_Instance (Bus : Node_Id; E : Node_Id);

      procedure Visit_Bus_Instance (Bus : Node_Id; E : Node_Id) is
         N                 : Node_Id;
         S                 : Node_Id;
         F                 : Node_Id;
         B                 : Node_Id;
         C                 : Node_Id;
         C_End             : Node_Id;
         End_List          : List_Id;
         Parent            : Node_Id;
         Naming_Table_List : constant List_Id := New_List (ADN.K_List_Id);
         Root_Sys          : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Transport_API : Supported_Transport_APIs := Transport_None;
      begin
         --  We perform a first loop to designate the nodes to be
         --  included in the naming table. For a particular node, the
         --  nodes in its naming table are (1) itself and (2) all the
         --  nodes directly connected to it. This factorizes a lot of
         --  code between the handling of the different platforms.

         --  In parallel, we check the consistency of the transport
         --  layers that have to be used by the connection involving
         --  these features.

         --  (1) Add current process E to the naming table

         if Is_Added (E, Bus, E) then
            return;
         end if;

         Set_Added (E, Bus, E);

         --  (2) Add other processes connected to E

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               --  We make two iterations to traverse (1) the sources
               --  of F then (2) the destinations of F.

               End_List := Sources (F);

               for J in 1 .. 2 loop
                  if not AAU.Is_Empty (End_List) then
                     C_End := First_Node (End_List);

                     while Present (C_End) loop
                        Parent := Parent_Component (Item (C_End));

                        if AAU.Is_Process (Parent) then
                           if Parent /= E then
                              --  Mark the parent component of the
                              --  remote feature as involved with the
                              --  current process.

                              Set_Added (Parent, Bus, E);
                           end if;

                           --  Get the connection involving C_End

                           C := Extra_Item (C_End);
                           pragma Assert (Present (C));

                           --  Get the bus of the connection

                           B := Get_Bound_Bus (C);

                           --  Get the transport layer of the Bus and
                           --  verify that all the features use the
                           --  same transport layer for their
                           --  connections.

                           if Transport_API /= Transport_None
                             and then Transport_API /= Get_Transport_API (B, E)
                           then
                              Display_Located_Error
                                (Loc (Parent_Subcomponent (E)),
                                 "The features of this process are involved" &
                                 " in connections that do not use the same" &
                                 " transport layer. This is not supported" &
                                 " yet.",
                                 Fatal => True);
                           else
                              Transport_API := Get_Transport_API (B, E);

                              --  If we have a bus for which no
                              --  transport layer has been specified,
                              --  we raise an error.

                              if Transport_API = Transport_None then
                                 Display_Located_Error
                                   (Loc (B),
                                    "No transport layer has been specified" &
                                    " for this bus",
                                    Fatal => True);
                              end if;
                           end if;
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

         --  Generate the naming table

         case Transport_API is
            when Transport_BSD_Sockets | Transport_User =>
               --  Build the node information for all the nodes
               --  involved with the current one and append it to the
               --  naming list.

               S := First_Node (Subcomponents (Root_Sys));

               while Present (S) loop
                  if Transport_API = Transport_BSD_Sockets
                    and then AAU.Is_Process (Corresponding_Instance (S))
                    and then Is_Added (Corresponding_Instance (S), Bus, E)
                  then
                     N := Naming_Information (Corresponding_Instance (S));
                     Append_Node_To_List (N, Naming_Table_List);

                  elsif Transport_API = Transport_User
                    and then AAU.Is_Device (Corresponding_Instance (S))
                    and then Is_Connected (Bus, S)
                  then
                     N := Naming_Information (Corresponding_Instance (S));
                     Append_Node_To_List (N, Naming_Table_List);
                  end if;

                  S := Next_Node (S);
               end loop;

               N :=
                 Make_Element_Association
                   (No_Node,
                    Make_Record_Aggregate
                      (Make_List_Id
                         (Make_Subprogram_Call
                            (RE (RE_To_HI_String),
                             Make_List_Id
                               (Make_Literal (New_String_Value (No_Name)))),
                          Make_Literal (New_Integer_Value (0, 1, 10)),
                          RE (RE_Null_Address))));
               Append_Node_To_List (N, Naming_Table_List);

               --  Declare the Naming Table

               N :=
                 Message_Comment
                   ("Naming Table for bus " &
                    Get_Name_String
                      (Name (Identifier (Parent_Subcomponent (Bus)))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               if Transport_API = Transport_User then
                  --  We are building a name table specific to a bus

                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier => Map_Bus_Name (Bus),
                       Constant_Present    => True,
                       Object_Definition   => RE (RE_Naming_Table_Type),
                       Expression => Make_Array_Aggregate (Naming_Table_List));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               else
                  --  We generate the default name table

                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier =>
                         Make_Defining_Identifier (PN (P_Naming_Table)),
                       Constant_Present  => True,
                       Object_Definition => RE (RE_Naming_Table_Type),
                       Expression => Make_Array_Aggregate (Naming_Table_List));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
               end if;

            when Transport_SpaceWire =>
               Display_Located_Error
                 (Loc (E),
                  "SpaceWire bus is no longer supported",
                  Fatal => True);

            when Transport_None =>
               --  If we did not fetch a meaningful transport layer,
               --  this means the application does not use the
               --  network. No naming table will be generated.

               null;
         end case;
      end Visit_Bus_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P        : constant Node_Id := ADN.Entity (U);
         S        : Node_Id;
         Parent   : Node_Id;
         Root_Sys : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         F         : Node_Id;
         B         : Node_Id;
         C         : Node_Id;
         C_End     : Node_Id;
         End_List  : List_Id;
         Transport : Supported_Transport_APIs;

      begin
         pragma Assert (AAU.Is_System (Root_Sys));
         Push_Entity (P);
         Push_Entity (U);
         Set_Naming_Spec;

         --  We go through all bus

         Transport := Transport_None;

         S := First_Node (Subcomponents (Root_Sys));

         Main_Loop :
         while Present (S) loop
            if AAU.Is_Bus (Corresponding_Instance (S)) then
               if not AAU.Is_Empty (Features (E)) then
                  F := First_Node (Features (E));

                  while Present (F) loop
                     --  We make two iterations to traverse (1) the
                     --  sources of F then (2) the destinations of F.

                     End_List := Sources (F);
                     for J in 1 .. 2 loop
                        if not AAU.Is_Empty (End_List) then
                           C_End := First_Node (End_List);

                           while Present (C_End) loop
                              Parent := Parent_Component (Item (C_End));
                              if AAU.Is_Process (Parent) then
                                 --  Get the connection involving C_End

                                 C := Extra_Item (C_End);
                                 pragma Assert (Present (C));

                                 --  Get the bus of the connection

                                 B := Get_Bound_Bus (C);

                                 Transport := Get_Transport_API (B);

                                 if Present (B)
                                   and then B = Corresponding_Instance (S)
                                 then
                                    Visit_Bus_Instance
                                      (Corresponding_Instance (S),
                                       E);
                                    if Transport /= Transport_User
                                      and then Transport /= Transport_None
                                    then
                                       exit Main_Loop;
                                    end if;
                                 end if;
                              end if;
                              C_End := Next_Node (C_End);
                           end loop;

                           --  In the next iteration, we traverse the
                           --  Destinations of F.

                           End_List := Destinations (F);

                        end if;
                     end loop;

                     F := Next_Node (F);
                  end loop;
               end if;
            end if;
            S := Next_Node (S);

         end loop Main_Loop;

         Bind_Transport_API (E, Transport);
         --  XXX dubious. Actually, it is used only through
         --  Fetch_Transport_API to run Transport_API initialization
         --  in main thread.

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         C : Node_Id;
         S : Node_Id;
      begin
         Push_Entity (Ada_Root);

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

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

   end Package_Spec;

end Ocarina.Backends.PO_HI_Ada.Naming;
