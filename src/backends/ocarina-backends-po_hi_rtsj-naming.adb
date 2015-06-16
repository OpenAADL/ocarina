------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B A C K E N D S . P O _ H I _ R T S J . N A M I N G    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.PO_HI_RTSJ.Mapping;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.RTSJ_Values;

package body Ocarina.Backends.PO_HI_RTSJ.Naming is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.PO_HI_RTSJ.Mapping;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.RTSJ_Values;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;
   package RV renames Ocarina.Backends.RTSJ_Values;

   package body Source_File is

      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Bus_Instance (Bus : Node_Id; E : Node_Id);
      function Added_Internal_Name
        (P : Node_Id;
         B : Node_Id;
         E : Node_Id) return Name_Id;
      function Is_Added (P : Node_Id; B : Node_Id; E : Node_Id) return Boolean;
      procedure Set_Added (P : Node_Id; B : Node_Id; E : Node_Id);
      procedure Socket_Naming_Information (E : Node_Id);

      --  Global variables declaration
      Inetport_Enumerator_List    : List_Id;
      Inetaddress_Enumerator_List : List_Id;

      Nb_Nodes : Unsigned_Long_Long;

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

      -------------------------------
      -- Socket_Naming_Information --
      -------------------------------
      procedure Socket_Naming_Information (E : Node_Id) is
         Location    : Name_Id;
         Port_Number : Value_Id;
         L           : Node_Id;
         P           : Node_Id;
         N           : Node_Id;
      begin
         pragma Assert (AINU.Is_Process (E));

         Location    := Get_Location (Get_Bound_Processor (E));
         Port_Number := Get_Port_Number (E);

         if Location = No_Name then
            L :=
              Make_Pointed_Notation
                (Make_Defining_Identifier (ON (O_Utils)),
                 RE (RE_No_Inet_Address));
         else
            L := Make_Literal (New_String_Value (Location));
         end if;

         if Port_Number = RV.No_Value then
            P :=
              Make_Pointed_Notation
                (Make_Defining_Identifier (ON (O_Utils)),
                 RE (RE_No_Port));
         else
            P := Make_Literal (To_RTSJ_Value (Port_Number));
         end if;

         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Make_Defining_Identifier (ON (O_Context)),
                   Make_Array_Value
                     (Defining_Identifier => RE (RE_Nodes_Inet_Address),
                      Array_Item          =>
                        Make_Pointed_Notation
                          (Make_Defining_Identifier (ON (O_Deployment)),
                           Make_Defining_Identifier
                             (Map_RTSJ_Enumerator_Name
                                (Parent_Subcomponent (E)))))),
              Expression => L);
         RTU.Append_Node_To_List (N, Inetaddress_Enumerator_List);

         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Make_Defining_Identifier (ON (O_Context)),
                   Make_Array_Value
                     (Defining_Identifier => RE (RE_Nodes_Inet_Port),
                      Array_Item          =>
                        Make_Pointed_Notation
                          (Make_Defining_Identifier (ON (O_Deployment)),
                           Make_Defining_Identifier
                             (Map_RTSJ_Enumerator_Name
                                (Parent_Subcomponent (E)))))),
              Expression => P);
         RTU.Append_Node_To_List (N, Inetport_Enumerator_List);

         Nb_Nodes := Nb_Nodes + 1;
      end Socket_Naming_Information;

      -----------
      -- Visit --
      -----------
      procedure Visit (E : Node_Id) is
      begin
         case AIN.Kind (E) is
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

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------
      procedure Visit_System_Instance (E : Node_Id) is
         N : constant Node_Id := Map_Distributed_Application (E);
         P : Node_Id;
      begin
         RTSJ_Root := N;
         Push_Entity (N);

         --  Verify the consistency of the distributed application
         --  hierarchy.
         if not AINU.Is_Empty (Connections (E)) then
            P := AIN.First_Node (Connections (E));
            while Present (P) loop
               Check_Connection_Consistency (P);
               P := AIN.Next_Node (P);
            end loop;
         end if;

         --  Visit all the subcomponents of the system
         if not AINU.Is_Empty (Subcomponents (E)) then
            P := AIN.First_Node (Subcomponents (E));
            while Present (P) loop
               Visit (Corresponding_Instance (P));
               P := AIN.Next_Node (P);
            end loop;
         end if;

         Pop_Entity;
      end Visit_System_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------
      procedure Visit_Process_Instance (E : Node_Id) is
         P          : constant Node_Id := Map_HI_Node (E);
         U          : Node_Id;
         S          : Node_Id;
         F          : Node_Id;
         N          : Node_Id;
         C          : Node_Id;
         B          : Node_Id;
         C_End      : Node_Id;
         Spec       : Node_Id;
         Impl       : Node_Id;
         Parent     : Node_Id;
         Main_Class : Node_Id;
         Root_Sys   : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         Platform : constant Supported_Execution_Platform :=
           Get_Execution_Platform (Get_Bound_Processor (E));
         End_List          : List_Id;
         Method_Statements : constant List_Id := New_List (K_Statement_List);
         Class_Methods     : constant List_Id := New_List (K_Method_List);
         Transport         : Supported_Transport_APIs;
      begin
         pragma Assert (AINU.Is_System (Root_Sys));

         if Platform = Platform_None then
            Display_Located_Error
              (AIN.Loc (Parent_Subcomponent (E)),
               "This process subcomponent is bound to a processor without" &
               " execution platform specification",
               Fatal => True);
         end if;

         Push_Entity (P);
         U := Map_HI_Unit (E);
         Push_Entity (U);

         RTU.Set_Naming_Source;

         --  Global variables initialization
         Inetport_Enumerator_List    := New_List (K_Enumeration_Literals);
         Inetaddress_Enumerator_List := New_List (K_Enumeration_Literals);
         Nb_Nodes                    := 0;

         --  We go through all bus
         Transport := Transport_None;

         S := AIN.First_Node (Subcomponents (Root_Sys));

         Main_Loop :
         while Present (S) loop
            if AINU.Is_Bus (Corresponding_Instance (S)) then
               if not AINU.Is_Empty (Features (E)) then
                  F := AIN.First_Node (Features (E));

                  while Present (F) loop
                     --  We make two iterations to traverse (1) the
                     --  sources of F then (2) the destinations of F.

                     End_List := AIN.Sources (F);
                     for J in 1 .. 2 loop
                        if not AINU.Is_Empty (End_List) then
                           C_End := AIN.First_Node (End_List);

                           while Present (C_End) loop
                              Parent := Parent_Component (Item (C_End));
                              if AINU.Is_Process (Parent) then
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
                              C_End := AIN.Next_Node (C_End);
                           end loop;

                           --  In the next iteration, we traverse the
                           --  Destinations of F.

                           End_List := Destinations (F);

                        end if;
                     end loop;

                     F := AIN.Next_Node (F);
                  end loop;
               end if;
            end if;

            S := AIN.Next_Node (S);
         end loop Main_Loop;

         --  nodesInetAddress declaration
         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Make_Defining_Identifier (ON (O_Context)),
                   RE (RE_Nodes_Inet_Address)),
              Expression =>
                Make_New_Statement
                  (Defining_Identifier => New_Node (K_String),
                   Parameters          =>
                     Make_List_Id
                       (Make_Literal (New_Int_Value (Nb_Nodes, 0, 100))),
                   Is_Array => True));

         N :=
           Make_Full_Array_Declaration
             (Array_Declaration => N,
              Array_Assignments => Inetaddress_Enumerator_List);
         RTU.Append_Node_To_List (N, Method_Statements);

         --  nodesInetPort declaration
         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Make_Defining_Identifier (ON (O_Context)),
                   RE (RE_Nodes_Inet_Port)),
              Expression =>
                Make_New_Statement
                  (Defining_Identifier => New_Node (K_Int),
                   Parameters          =>
                     Make_List_Id
                       (Make_Literal (New_Int_Value (Nb_Nodes, 0, 10))),
                   Is_Array => True));

         N :=
           Make_Full_Array_Declaration
             (Array_Declaration => N,
              Array_Assignments => Inetport_Enumerator_List);
         RTU.Append_Node_To_List (N, Method_Statements);

         Spec :=
           Make_Function_Specification
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Defining_Identifier =>
                Make_Defining_Identifier (MN (M_Initialization)),
              Return_Type => New_Node (K_Void));

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Method_Statements);
         RTU.Append_Node_To_List (Impl, Class_Methods);

         Main_Class :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier => Make_Defining_Identifier (ON (O_Naming)),
              Methods             => Class_Methods);
         RTU.Append_Node_To_List (Main_Class, RTN.Statements (Current_File));

         Pop_Entity;
         Pop_Entity;
      end Visit_Process_Instance;

      ------------------------
      -- Visit_Bus_Instance --
      ------------------------
      procedure Visit_Bus_Instance (Bus : Node_Id; E : Node_Id) is
         S        : Node_Id;
         F        : Node_Id;
         B        : Node_Id;
         C        : Node_Id;
         C_End    : Node_Id;
         Parent   : Node_Id;
         End_List : List_Id;
         Root_Sys : constant Node_Id :=
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
         Set_Added (E, Bus, E);

         --  (2) Add other processes connected to E
         if not AINU.Is_Empty (Features (E)) then
            F := AIN.First_Node (Features (E));

            while Present (F) loop
               --  We make two iterations to traverse (1) the sources
               --  of F then (2) the destinations of F
               End_List := AIN.Sources (F);

               for J in 1 .. 2 loop
                  if not AINU.Is_Empty (End_List) then
                     C_End := AIN.First_Node (End_List);

                     while Present (C_End) loop
                        Parent := Parent_Component (Item (C_End));

                        if AINU.Is_Process (Parent) then
                           if Parent /= E then
                              --  Mark the parent component of the
                              --  remote feature as involved with
                              --  the current process

                              Set_Added (Parent, Bus, E);
                           end if;

                           --  Get the connection involving C_End
                           C := Extra_Item (C_End);
                           pragma Assert (Present (C));

                           --  Get the bus of the connection
                           B := Get_Bound_Bus (C);

                           --  Get the transport layer of the Bus and
                           --  verify that all the features use
                           --  the same transport layer for their
                           --  connections
                           if Transport_API /= Transport_None
                             and then Transport_API /= Get_Transport_API (B, E)
                           then
                              Display_Located_Error
                                (AIN.Loc (Parent_Subcomponent (E)),
                                 "The features of this process are involved" &
                                 " in connections that do not use the same" &
                                 "transport layer. This is not supported" &
                                 " yet.",
                                 Fatal => True);
                           else
                              Transport_API := Get_Transport_API (B, E);

                              --  If we have a bus for which no
                              --  transport layer has been specified,
                              --  we raise an error.
                              if Transport_API = Transport_None then
                                 Display_Located_Error
                                   (AIN.Loc (B),
                                    "No transport layer has been specified" &
                                    " for this bus.",
                                    Fatal => True);
                              end if;
                           end if;
                        end if;

                        C_End := AIN.Next_Node (C_End);
                     end loop;
                  end if;

                  --  In the next iteration, we traverse the
                  --  destinations of F
                  End_List := Destinations (F);
               end loop;

               F := AIN.Next_Node (F);
            end loop;
         end if;

         --  Generate the naming table
         case Transport_API is
            when Transport_BSD_Sockets =>
               --  Build the node information for all the nodes
               --  involved with the current one and append it to
               --  the naming list.

               S := AIN.First_Node (Subcomponents (Root_Sys));

               while Present (S) loop
                  if AINU.Is_Process (Corresponding_Instance (S))
                    and then Is_Added (Corresponding_Instance (S), Bus, E)
                  then
                     Socket_Naming_Information (Corresponding_Instance (S));
                  end if;

                  S := AIN.Next_Node (S);
               end loop;

            when others =>
               null;
         end case;

      end Visit_Bus_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_RTSJ.Naming;
