------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . C _ C O M M O N . M A P P I N G     --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.Namet;
with Utils; use Utils;

with Ocarina.Backends.Messages;
with Ocarina.Backends.Utils;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.POK_C.Runtime;
with Ocarina.Backends.POK_C;

with Ocarina.Instances.Queries;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

package body Ocarina.Backends.C_Common.Mapping is

   use Ocarina.Namet;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.C_Values;
   use Ocarina.Backends.C_Tree.Nodes;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.Backends.POK_C.Runtime;
   use Ocarina.Backends.POK_C;
   use Ocarina.Instances.Queries;

   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   package AAN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
--   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package PKR renames Ocarina.Backends.POK_C.Runtime;
   package PHR renames Ocarina.Backends.PO_HI_C.Runtime;

   ---------------------------
   -- Call_Remote_Functions --
   ---------------------------

   procedure Call_Remote_Functions
     (Caller_Thread : Node_Id;
      Spg_Call      : Node_Id;
      Declarations  : List_Id;
      Statements    : List_Id)
   is
      procedure Check_Connection_Consistency (F : Node_Id);
      --  Verifies that the feature F is connected to at least one
      --  destination.

      procedure Call_Remote_Functions
        (Root_F         : Node_Id;
         Intermediate_F : Node_Id);
      --  This recursive procedure will unwind recursively all the
      --  destinations of the Root_F feature until it finds a
      --  subprogram. If the remote subprogram is found, add a call
      --  to the subprogram call table (if necessary) and add a
      --  parameter association corresponding to Root_F the the
      --  call to this subprogram. At the top level call to
      --  Call_Remote_Subprograms, Intermediate_F = Root_F.

      procedure Update_Remote_Calls
        (Remote_Spg  : Node_Id;
         Param_Value : Node_Id);
      --  If the call to Spg has not been added yet, create
      --  it. Adds a parameter association Param_Name =>
      --  Param_Value to the call profile.

      Call_List : constant List_Id := New_List (CTN.K_Statement_List);
      --  List that contains the calls to all remote subprogram
      --  connected to Spg.

      ----------------------------------
      -- Check_Connection_Consistency --
      ----------------------------------

      procedure Check_Connection_Consistency (F : Node_Id) is
      begin
         if AINU.Length (Destinations (F)) = 0 then
            Display_Located_Error
              (AIN.Loc (F),
               "This feature does not have any destinations",
               Fatal => True);
         end if;
      end Check_Connection_Consistency;

      ---------------------------
      -- Call_Remote_Functions --
      ---------------------------

      procedure Call_Remote_Functions
        (Root_F         : Node_Id;
         Intermediate_F : Node_Id)
      is
         Destination_F : Node_Id;
         C             : Node_Id;
      begin
         --  Root F has to be a parameter instance

         pragma Assert (Kind (Root_F) = K_Parameter_Instance);

         --  The container of the subprogram containing F has to be
         --  necessarily a Thread.

         pragma Assert
           (AINU.Is_Thread
              (Parent_Component
                 (Parent_Sequence
                    (Parent_Subcomponent (Parent_Component (Root_F))))));

         --  Check the feature consistency

         Check_Connection_Consistency (Intermediate_F);

         Destination_F := AIN.First_Node (Destinations (Intermediate_F));

         while Present (Destination_F) loop
            C := Parent_Component (Item (Destination_F));

            --  If C is also the parent component of Root_F (which
            --  is necessarily a subprogram). Then display an error
            --  and exit.

            if C = Parent_Component (Root_F) then
               Display_Located_Error
                 (AIN.Loc (C),
                  "This subprogram is involved in a connection cycle",
                  Fatal => True);
            end if;

            if AINU.Is_Subprogram (C) then
               --  If C is a subprogram, then append it to the call
               --  list (if necessary, and add a parameter
               --  association corresponding to Root_F and
               --  Destination_F.

               Update_Remote_Calls (C, Root_F);
            else
               --  Otherwise, keep unwinding the destinations

               Call_Remote_Functions (Root_F, Item (Destination_F));
            end if;

            Destination_F := AIN.Next_Node (Destination_F);
         end loop;
      end Call_Remote_Functions;

      -------------------------
      -- Update_Remote_Calls --
      -------------------------

      procedure Update_Remote_Calls
        (Remote_Spg  : Node_Id;
         Param_Value : Node_Id)
      is
         R_Name  : Name_Id;
         N       : Node_Id;
         Profile : List_Id;
      begin
         --  Check whether a call corresponding to Spg has already
         --  been added to Call_List (we use name table infos
         --  instead of looping on Call list, which is a very fast
         --  way.

         Set_Nat_To_Name_Buffer (Nat (Remote_Spg));
         Add_Str_To_Name_Buffer ("%RemoteCall%");
         Add_Nat_To_Name_Buffer (Nat (Spg_Call));
         R_Name := Name_Find;

         if Get_Name_Table_Info (R_Name) = 0 then
            N := Message_Comment ("Call stub ");
            Append_Node_To_List (N, Statements);

            Profile := New_List (CTN.K_List_Id);

            --  Add the FROM argument (first argument)
            N :=
              Make_Defining_Identifier
                (Map_C_Enumerator_Name
                   (Parent_Subcomponent (Caller_Thread),
                    Entity => True));
            Append_Node_To_List (N, Profile);

            --  Add the TO argument (second argument)

            N :=
              Make_Defining_Identifier
                (Map_C_Enumerator_Name
                   (Parent_Subcomponent (Get_Container_Thread (Remote_Spg)),
                    Entity => True));

            Append_Node_To_List (N, Profile);

            --  Add the message argument (third argument)
            N :=
              Make_Variable_Address
                (Make_Defining_Identifier (PN (P_Message)));
            Append_Node_To_List (N, Profile);

            --  Create the subprogram call

            N := Make_Call_Profile (Map_Stub_Identifier (Remote_Spg), Profile);
            Append_Node_To_List (N, Statements);

            --  Mark the call as being added. The info we associate
            --  to the name is the value of the profile list to be
            --  able to get it to add parameter associations.

            Set_Name_Table_Info (R_Name, Nat (Profile));
         else
            Profile := List_Id (Get_Name_Table_Info (R_Name));
         end if;

         N :=
           Make_Defining_Identifier
             (Map_C_Full_Parameter_Name (Spg_Call, Param_Value));
         Append_Node_To_List (N, Profile);

      end Update_Remote_Calls;

      Spg : constant Node_Id := Corresponding_Instance (Spg_Call);
      F   : Node_Id;
   begin
      pragma Assert (AINU.Is_Thread (Caller_Thread));

      --  The lists have to be created

      if Declarations = No_List or else Statements = No_List then
         raise Program_Error
           with "Lists have to be created before any " &
           "call to Get_Remote_Subprogram";
      end if;

      if not AINU.Is_Empty (Features (Spg)) then
         F := AIN.First_Node (Features (Spg));

         while Present (F) loop
            if Kind (F) = K_Parameter_Instance and then Is_Out (F) then
               --  Call all the remote subprograms connected the
               --  feature F.

               Call_Remote_Functions (F, F);
            end if;

            F := AIN.Next_Node (F);
         end loop;
      end if;

      --  Append the calls to the Statements list

      if not CTU.Is_Empty (Call_List) then
         Append_Node_To_List (CTN.First_Node (Call_List), Statements);
      end if;
   end Call_Remote_Functions;

   ---------------------------------
   -- Map_Distributed_Application --
   ---------------------------------

   function Map_Distributed_Application (E : Node_Id) return Node_Id is
      D : constant Node_Id := New_Node (CTN.K_HI_Distributed_Application);
   begin
      pragma Assert (AINU.Is_System (E) or else AINU.Is_Processor (E));

      --  Update the global variable to be able to fetch the root of
      --  the distributed application and generate the source files.

      if Get_Current_Backend_Kind = PolyORB_Kernel_C then
         CTN.Set_Name
           (D,
            To_C_Name
              (AIN.Name (AIN.Identifier (Parent_Subcomponent (E))),
               Keyword_Check => False));
      else
         CTN.Set_Name (D, To_C_Name (AIN.Name (AIN.Identifier (E))));
      end if;

      CTN.Set_Units (D, New_List (CTN.K_List_Id));
      CTN.Set_HI_Nodes (D, New_List (CTN.K_List_Id));

      return D;
   end Map_Distributed_Application;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node
     (E      : Node_Id;
      Kernel : Boolean := False) return Node_Id
   is
      N : constant Node_Id := New_Node (CTN.K_HI_Node);
   begin
      pragma Assert
        (AINU.Is_Process_Or_Device (E)
         or else AINU.Is_System (E)
         or else AINU.Is_Processor (E));

      --  The name of the node is not the name of the process
      --  component instance, but the name of the process subcomponent
      --  corresponding to this instance.

      if Kernel then
         Set_Str_To_Name_Buffer ("kernel");
         CTN.Set_Name (N, Name_Find);
      else
         CTN.Set_Name
           (N,
            To_C_Name
              (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E))),
               Keyword_Check => False));
      end if;

      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list of the PolyORB-HI
      --  distributed application. We are sure that the top of the
      --  entity stack contains the C distributed application node.

      Append_Node_To_List (N, HI_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------

   function Map_HI_Unit (E : Node_Id) return Node_Id is
      U : Node_Id;
      S : List_Id;
      H : List_Id;
      N : Node_Id;
      P : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process_Or_Device (E)
         or else AINU.Is_Processor (E));

      U := New_Node (CTN.K_HI_Unit, AIN.Identifier (E));
      S := New_List (K_Sources);
      H := New_List (K_Headers);

      --  Packages that are common to all nodes

      if AINU.Is_Process_Or_Device (E) then
         Set_Str_To_Name_Buffer ("subprograms");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Source_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Subprograms_Source (U, P);
         Append_Node_To_List (P, S);

         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            Set_Str_To_Name_Buffer ("gtypes");
         elsif Get_Current_Backend_Kind = PolyORB_HI_C then
            Set_Str_To_Name_Buffer ("types");
         end if;
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Source_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Types_Source (U, P);
         Append_Node_To_List (P, S);

         Set_Str_To_Name_Buffer ("marshallers");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Source_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Marshallers_Source (U, P);
         Append_Node_To_List (P, S);

         Set_Str_To_Name_Buffer ("request");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Source_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Request_Source (U, P);
         Append_Node_To_List (P, S);

         Set_Str_To_Name_Buffer ("activity");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Source_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Activity_Source (U, P);
         Append_Node_To_List (P, S);
         Bind_AADL_To_Activity (Identifier (E), P);

         Set_Str_To_Name_Buffer ("activity");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Header_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Activity_Header (U, P);
         Append_Node_To_List (P, H);

         Set_Str_To_Name_Buffer ("main");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Source_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Main_Source (U, P);
         Append_Node_To_List (P, S);
         Bind_AADL_To_Main (Identifier (E), P);

         Set_Str_To_Name_Buffer ("request");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Header_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Request_Header (U, P);
         Append_Node_To_List (P, H);

         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            Set_Str_To_Name_Buffer ("gtypes");
         elsif Get_Current_Backend_Kind = PolyORB_HI_C then
            Set_Str_To_Name_Buffer ("types");
         end if;
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Header_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Types_Header (U, P);
         Append_Node_To_List (P, H);

         Set_Str_To_Name_Buffer ("marshallers");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Header_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Marshallers_Header (U, P);
         Append_Node_To_List (P, H);

         Set_Str_To_Name_Buffer ("subprograms");
         N := Make_Defining_Identifier (Name_Find);
         P := Make_Header_File (N);
         Set_Distributed_Application_Unit (P, U);
         CTN.Set_Subprograms_Header (U, P);
         Append_Node_To_List (P, H);

      end if;

      Set_Str_To_Name_Buffer ("deployment");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      CTN.Set_Deployment_Source (U, P);
      Append_Node_To_List (P, S);

      Set_Str_To_Name_Buffer ("deployment");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Header_File (N);
      Set_Distributed_Application_Unit (P, U);
      CTN.Set_Deployment_Header (U, P);
      Append_Node_To_List (P, H);

      Set_Str_To_Name_Buffer ("naming");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Header_File (N);
      Set_Distributed_Application_Unit (P, U);
      CTN.Set_Naming_Header (U, P);
      Append_Node_To_List (P, H);

      Set_Str_To_Name_Buffer ("naming");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      CTN.Set_Naming_Source (U, P);
      Append_Node_To_List (P, S);
      Bind_AADL_To_Naming (Identifier (E), P);

      --  Append the Unit to the units list of the current C
      --  partition.

      CTN.Set_Sources (U, S);
      CTN.Set_Headers (U, H);

      Append_Node_To_List (U, Units (Current_Entity));
      CTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   -------------------------------------
   -- Bind_AADL_To_Feature_Subprogram --
   -------------------------------------

   procedure Bind_AADL_To_Feature_Subprogram (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Feature_Subprogram_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Feature_Subprogram;

   -----------------------------
   -- Bind_AADL_To_Deployment --
   -----------------------------

   procedure Bind_AADL_To_Deployment (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Deployment_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Deployment;

   ------------------------------
   -- Bind_AADL_To_Global_Port --
   ------------------------------

   procedure Bind_AADL_To_Global_Port (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Global_Port_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Global_Port;

   -------------------------------
   -- Bind_AADL_To_Global_Names --
   -------------------------------

   procedure Bind_AADL_To_Global_Names (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Global_Names_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Global_Names;

   -------------------------------------
   -- Bind_AADL_To_Global_Model_Names --
   -------------------------------------

   procedure Bind_AADL_To_Global_Model_Names (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Global_Model_Names_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Global_Model_Names;

   -----------------------------
   -- Bind_AADL_To_Local_Port --
   -----------------------------

   procedure Bind_AADL_To_Local_Port (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Local_Port_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Local_Port;

   --------------------------
   -- Bind_AADL_To_Request --
   --------------------------

   procedure Bind_AADL_To_Request (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Request_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Request;

   ---------------------------
   -- Bind_AADL_To_Entities --
   ---------------------------

   procedure Bind_AADL_To_Entities (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Entities_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Entities;

   --------------------------
   -- Bind_AADL_To_Servers --
   --------------------------

   procedure Bind_AADL_To_Servers (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Servers_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Servers;

   -----------------------------
   -- Bind_AADL_To_Subprogram --
   -----------------------------

   procedure Bind_AADL_To_Subprogram (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Subprogram_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Subprogram;

   ---------------------------
   -- Bind_AADL_To_Activity --
   ---------------------------

   procedure Bind_AADL_To_Activity (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Activity_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Activity;

   -------------------------
   -- Bind_AADL_To_Naming --
   -------------------------

   procedure Bind_AADL_To_Naming (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Naming_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Naming;

   ----------------------
   -- Bind_AADL_To_Job --
   ----------------------

   procedure Bind_AADL_To_Job (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Job_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Job;

   -----------------------------
   -- Bind_AADL_To_Enumerator --
   -----------------------------

   procedure Bind_AADL_To_Enumerator (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Enumerator_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Enumerator;

   ----------------------------------
   -- Bind_AADL_To_Type_Definition --
   ----------------------------------

   procedure Bind_AADL_To_Type_Definition (G : Node_Id; A : Node_Id) is
      pragma Assert (Present (A));

      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Type_Definition_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Type_Definition;

   ----------------------------------
   -- Bind_AADL_To_Process_Request --
   ----------------------------------

   procedure Bind_AADL_To_Process_Request (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Process_Request_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Process_Request;

   -------------------------------
   -- Bind_AADL_To_Request_Type --
   -------------------------------

   procedure Bind_AADL_To_Request_Type (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Request_Type_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Request_Type;

   -----------------------
   -- Bind_AADL_To_Main --
   -----------------------

   procedure Bind_AADL_To_Main (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Main_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Main;

   -----------------------------
   -- Map_Task_Job_Identifier --
   -----------------------------

   function Map_Task_Job_Identifier
     (E                : Node_Id;
      Prefix_Component : Node_Id := No_Node) return Node_Id
   is
      Name : Name_Id;
      C1   : Name_Id := No_Name;
      C2   : Name_Id := No_Name;
   begin
      if Prefix_Component /= No_Node then
         Get_Name_String
           (To_C_Name (AIN.Display_Name (Identifier (Prefix_Component))));
         C1 := Name_Find;
      end if;
      Get_Name_String (To_C_Name (AIN.Display_Name (Identifier (E))));
      C2 := Name_Find;

      Set_Str_To_Name_Buffer ("");

      if C1 /= No_Name then
         Get_Name_String (C1);
         Add_Str_To_Name_Buffer ("_");
      end if;

      Get_Name_String_And_Append (C2);
      Add_Str_To_Name_Buffer ("_job");
      Name := Name_Find;
      Name := To_Lower (Name);
      return Make_Defining_Identifier (Name);
   end Map_Task_Job_Identifier;

   ---------------------------------
   -- Map_Task_Deliver_Identifier --
   ---------------------------------

   function Map_Task_Deliver_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String (To_C_Name (AIN.Display_Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_deliver");
      Name := Name_Find;
      Name := To_Lower (Name);
      return Make_Defining_Identifier (Name);
   end Map_Task_Deliver_Identifier;

   ---------------------
   -- Map_C_Enum_Name --
   ---------------------

   function Map_C_Enum_Name
     (E          : Node_Id;
      Enumerator : Name_Id) return Name_Id
   is
      Enum_Name : Name_Id;
   begin
      --  Note: we need to adapt the name of the enumerator to avoid
      --  collision of enumerators, as C does not allow twice the same
      --  enumerators in two different enum types.

      Enum_Name := To_C_Name (Display_Name (Identifier (E)));

      Get_Name_String_And_Append (Enum_Name);
      Add_Str_To_Name_Buffer ("_");
      Get_Name_String_And_Append (Enumerator);
      return Name_Find;
   end Map_C_Enum_Name;

   ---------------------------
   -- Map_C_Enumerator_Name --
   ---------------------------

   function Map_C_Enumerator_Name
     (E             : Node_Id;
      Custom_Parent : Node_Id := No_Node;
      Entity        : Boolean := False;
      Server        : Boolean := False;
      Port_Type     : Boolean := False;
      Local_Port    : Boolean := False) return Name_Id
   is
      C_Name_1 : Name_Id;
      C_Name_2 : Name_Id;
   begin
      if Kind (E) = K_Port_Spec_Instance then
         C_Name_1 := CTU.To_C_Name (Display_Name (Identifier (E)));

         Get_Name_String
           (CTU.To_C_Name
              (Display_Name
                 (Identifier (Parent_Subcomponent (Parent_Component (E))))));
         if Local_Port then
            Add_Str_To_Name_Buffer ("_local_");
            Get_Name_String_And_Append (C_Name_1);
         else
            Add_Str_To_Name_Buffer ("_global_");
            Get_Name_String_And_Append (C_Name_1);
         end if;
      elsif AINU.Is_Bus (E) then
         Set_Str_To_Name_Buffer ("bus_");
         Get_Name_String_And_Append
           (AIN.Name (AIN.Identifier (Parent_Subcomponent (E))));
      elsif AINU.Is_Virtual_Bus (E) then
         Set_Str_To_Name_Buffer ("virtual_bus_");
         if Parent_Subcomponent (E) /= No_Node then
            Get_Name_String_And_Append
              (AIN.Name (AIN.Identifier (Parent_Subcomponent (E))));
         else
            Get_Name_String_And_Append (AIN.Name (AIN.Identifier (E)));
         end if;
      elsif AINU.Is_Subprogram (E) then
         --  For subprograms and processes, the enemerator name is
         --  mapped from the entity name.

         Get_Name_String (CTU.To_C_Name (Display_Name (Identifier (E))));
         Add_Str_To_Name_Buffer ("_k");
      elsif Get_Current_Backend_Kind = PolyORB_HI_C
        and then AINU.Is_Device (E)
      then
         Get_Name_String
           (CTU.To_C_Name
              (Display_Name (Identifier (Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_device_id");
      elsif
        (Present (Corresponding_Instance (E))
         and then AINU.Is_Process_Or_Device (Corresponding_Instance (E)))
      then

         Get_Name_String (CTU.To_C_Name (Display_Name (Identifier (E))));
         Add_Str_To_Name_Buffer ("_k");
      elsif AINU.Is_Thread (Corresponding_Instance (E)) then
         --  For threads, the enumerator name is mapped from the
         --  containing process name and the thread subcomponent name.
         if Custom_Parent /= No_Node then
            C_Name_1 :=
              CTU.To_C_Name
                (Display_Name
                   (Identifier (Parent_Subcomponent (Custom_Parent))));
         else
            C_Name_1 :=
              CTU.To_C_Name
                (Display_Name
                   (Identifier (Parent_Subcomponent (Parent_Component (E)))));
         end if;

         C_Name_2 := CTU.To_C_Name (Display_Name (Identifier (E)));

         if Port_Type then
            Set_Str_To_Name_Buffer ("__po_hi_");
            Get_Name_String_And_Append (C_Name_2);
            Add_Str_To_Name_Buffer ("_t");
         else
            Get_Name_String (C_Name_1);
            Add_Char_To_Name_Buffer ('_');
            Get_Name_String_And_Append (C_Name_2);
            Add_Str_To_Name_Buffer ("_k");
         end if;

         if Server then
            Add_Str_To_Name_Buffer ("_server");
         end if;

         if Entity then
            Add_Str_To_Name_Buffer ("_entity");
         end if;
      else
         raise Program_Error with "Wrong node kind for Map_C_Enumerator_Name";
      end if;

      C_Name_1 := Name_Find;
      C_Name_1 := To_Lower (C_Name_1);

      return C_Name_1;
   end Map_C_Enumerator_Name;

   -----------------------
   -- Map_C_Define_Name --
   -----------------------

   function Map_C_Define_Name
     (E        : Node_Id;
      Nb_Ports : Boolean := False) return Name_Id
   is
      C_Name : Name_Id;
   begin
      if AINU.Is_Thread (Corresponding_Instance (E)) then
         --  For threads, the enumerator name is mapped from the
         --  containing process name and the thread subcomponent name.

         C_Name := Display_Name (Identifier (E));

         Set_Str_To_Name_Buffer ("__PO_HI_");

         Get_Name_String_And_Append (C_Name);
         if Nb_Ports then
            Add_Str_To_Name_Buffer ("_NB_PORTS");
         end if;
      else
         raise Program_Error with "Wrong node kind for Map_C_Enumerator_Name";
      end if;

      C_Name := Name_Find;
      C_Name := To_Upper (C_Name);

      return C_Name;
   end Map_C_Define_Name;

   -------------------------
   -- Map_C_Variable_Name --
   -------------------------

   function Map_C_Variable_Name
     (E                 : Node_Id;
      Port_Variable     : Boolean := False;
      Port_History      : Boolean := False;
      Port_Woffsets     : Boolean := False;
      Port_Empties      : Boolean := False;
      Port_First        : Boolean := False;
      Port_Queue        : Boolean := False;
      Port_Recent       : Boolean := False;
      Port_Fifo_Size    : Boolean := False;
      Port_Offsets      : Boolean := False;
      Port_Used_Size    : Boolean := False;
      Port_N_Dest       : Boolean := False;
      Port_Local_Dest   : Boolean := False;
      Port_Destinations : Boolean := False;
      Port_Total_Fifo   : Boolean := False;
      Port_Request      : Boolean := False;
      Request_Variable  : Boolean := False) return Name_Id
   is
      C_Name : Name_Id;
   begin

      C_Name := To_C_Name (Display_Name (Identifier (E)));

      if not Port_Request and then not Request_Variable then
         Set_Str_To_Name_Buffer ("__po_hi_");

         if Port_Local_Dest then
            Get_Name_String_And_Append
              (AIN.Name
                 (Identifier (Parent_Subcomponent (Parent_Component (E)))));
            Add_Str_To_Name_Buffer ("_");
         end if;

         Get_Name_String_And_Append (C_Name);
      else
         Get_Name_String (C_Name);
      end if;

      if Port_Variable then
         Add_Str_To_Name_Buffer ("_local_to_global");
      elsif Port_History then
         Add_Str_To_Name_Buffer ("_history");
      elsif Port_Woffsets then
         Add_Str_To_Name_Buffer ("_woffsets");
      elsif Port_Empties then
         Add_Str_To_Name_Buffer ("_empties");
      elsif Port_First then
         Add_Str_To_Name_Buffer ("_first");
      elsif Port_Queue then
         Add_Str_To_Name_Buffer ("_queue");
      elsif Port_Recent then
         Add_Str_To_Name_Buffer ("_recent");
      elsif Port_Fifo_Size then
         Add_Str_To_Name_Buffer ("_fifo_size");
      elsif Port_Offsets then
         Add_Str_To_Name_Buffer ("_offsets");
      elsif Port_Used_Size then
         Add_Str_To_Name_Buffer ("_used_size");
      elsif Port_N_Dest then
         Add_Str_To_Name_Buffer ("_n_dest");
      elsif Port_Local_Dest then
         Add_Str_To_Name_Buffer ("_local_destinations");
      elsif Port_Destinations then
         Add_Str_To_Name_Buffer ("_destinations");
      elsif Port_Total_Fifo then
         Add_Str_To_Name_Buffer ("_total_fifo_size");
      elsif Port_Request then
         Add_Str_To_Name_Buffer ("_request");
      elsif Request_Variable then
         Add_Str_To_Name_Buffer ("_request_var");
      end if;

      C_Name := Name_Find;

      return C_Name;
   end Map_C_Variable_Name;

   --------------------------
   -- Map_C_Operation_Name --
   --------------------------

   function Map_C_Operation_Name (E : Node_Id) return Name_Id is
      C_Name : Name_Id;
   begin
      Get_Name_String (CTU.To_C_Name (Display_Name (Identifier (E))));
      C_Name := Name_Find;
      return C_Name;
   end Map_C_Operation_Name;

   -------------------------------
   -- Map_C_Full_Parameter_Name --
   -------------------------------

   function Map_C_Full_Parameter_Name
     (Spg    : Node_Id;
      P      : Node_Id;
      Suffix : Character := ASCII.NUL) return Name_Id
   is
   begin
      pragma Assert (Kind (P) = K_Parameter_Instance);

      if Kind (Spg) = K_Component_Instance
        and then AINU.Is_Subprogram (Spg)
      then
         Get_Name_String (AINU.Compute_Full_Name_Of_Instance (Spg, True));
      elsif Kind (Spg) = K_Call_Instance then
         Get_Name_String (Display_Name (Identifier (Spg)));
      else
         raise Program_Error with "Wrong subprogram kind";
      end if;

      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Display_Name (Identifier (P)));

      Get_Name_String (CTU.To_C_Name (Name_Find));

      if Suffix /= ASCII.NUL then
         Add_Str_To_Name_Buffer ('_' & Suffix);
      end if;

      return Name_Find;
   end Map_C_Full_Parameter_Name;

   --------------------------
   -- Map_C_Port_Data_Name --
   --------------------------

   function Map_C_Port_Data_Name (E : Node_Id; P : Node_Id) return Name_Id is
   begin
      Get_Name_String (AINU.Compute_Full_Name_Of_Instance (E, True));

      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Display_Name (Identifier (P)));

      Get_Name_String (CTU.To_C_Name (Name_Find));

      return Name_Find;
   end Map_C_Port_Data_Name;

   --------------------------------
   -- Map_C_Data_Type_Designator --
   --------------------------------

   function Map_C_Data_Type_Designator (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      if Get_Current_Backend_Kind = PolyORB_Kernel_C then
         Add_Include (PKR.RH (RH_Gtypes));

      elsif Get_Current_Backend_Kind = PolyORB_HI_C then
         Add_Include (PHR.RH (RH_Types));
      end if;

      if Present (Backend_Node (Identifier (E)))
        and then Present
          (CTN.Type_Definition_Node (Backend_Node (Identifier (E))))
      then
         N :=
           Type_Name
             (CTN.Type_Definition_Node (Backend_Node (Identifier (E))));

         if Kind (N) = K_Defining_Identifier then
            return N;

         elsif Kind (N) = K_Array_Declaration then
            return Defining_Identifier (N);
         end if;
      else
         --  XXX why do we need this hack?
         return Map_C_Defining_Identifier (E);
      end if;

      return No_Node;
   end Map_C_Data_Type_Designator;

   ---------------------------------
   -- Map_C_Subprogram_Identifier --
   ---------------------------------

   function Map_C_Subprogram_Identifier (E : Node_Id) return Node_Id is
      Result   : Node_Id;
      Spg_Name : Name_Id;
   begin
      pragma Assert
        (AINU.Is_Thread (E)
         or else AINU.Is_Subprogram (E)
         or else Kind (E) = K_Port_Spec_Instance);

      if AINU.Is_Subprogram (E)
        and then Get_Source_Language (E) /= Language_C
      then
         Display_Error ("This is not a C function", Fatal => True);
      end if;

      --  Get the subprogram name

      if AINU.Is_Subprogram (E) then
         Spg_Name := Get_Source_Name (E);
      elsif AINU.Is_Thread (E) then
         Spg_Name := Get_Thread_Compute_Entrypoint (E);
      else
         Spg_Name := Get_Port_Compute_Entrypoint (E);
      end if;

      --  Get the full implementation name

      Get_Name_String (Spg_Name);
      Result := Make_Defining_Identifier (Name_Find);
      return Result;
   end Map_C_Subprogram_Identifier;

   -------------------------------
   -- Map_C_Defining_Identifier --
   -------------------------------

   function Map_C_Defining_Identifier
     (A          : Node_Id;
      Is_Pointer : Boolean := False) return Node_Id
   is
      I, J      : Node_Id;
      Result    : Node_Id;
      Name_List : List_Id;

   begin
      if Kind (A) /= K_Identifier then
         I := Identifier (A);
      end if;

      if Kind (A) = K_Component_Instance
        and then Display_Name (Identifier (Namespace (A))) /= No_Name
      then
         --  If this is a component instance, use both namespace and
         --  identifier to build the C defining identifier, to avoid
         --  collisions in the C namespace.

         Name_List := AINU.Split_Name (Namespace (A));

         J := AIN.First_Node (Name_List);

         if Present (J) then
            Get_Name_String (To_C_Name (Display_Name (J)));
            J := AIN.Next_Node (J);

            while Present (J) loop
               Add_Str_To_Name_Buffer
                 ("__" & Get_Name_String (Display_Name (J)));
               J := AIN.Next_Node (J);
            end loop;
         end if;

         Add_Str_To_Name_Buffer ("__" & Get_Name_String (Display_Name (I)));

      else
         Get_Name_String (Display_Name (I));
      end if;

      Result :=
        CTU.Make_Defining_Identifier (To_C_Name (Name_Find), True, Is_Pointer);

      return Result;
   end Map_C_Defining_Identifier;

   ------------------------
   -- Map_Scade_Function --
   ------------------------

   function Map_Scade_Function_Name (Subprogram : Node_Id) return Node_Id is
   begin
      Get_Name_String (Get_Source_Name (Subprogram));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Scade_Function_Name;

   -------------------------
   -- Map_Scade_Struct_In --
   -------------------------

   function Map_Scade_Struct_In (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("inC_");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Scade_Struct_In;

   --------------------------
   -- Map_Scade_Struct_Out --
   --------------------------

   function Map_Scade_Struct_Out (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("outC_");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Scade_Struct_Out;

   -------------------------
   -- Map_Scade_Parameter --
   -------------------------

   function Map_Scade_Parameter (Parameter : Node_Id) return Node_Id is
      Scade_Name : Name_Id;
   begin
      Scade_Name := Get_Scade_Signal (Parameter);

      if Scade_Name = No_Name then
         Scade_Name := Get_Source_Name (Parameter);
      end if;

      if Scade_Name = No_Name then
         Display_Located_Error
            (AIN.Loc (Parameter),
             "The Parameter does not specify a SCADE mapping",
             Fatal => True);
      end if;

      return CTU.Make_Defining_Identifier (Scade_Name, C_Conversion => False);
   end Map_Scade_Parameter;

   ----------------------
   -- Map_Simulink_Var --
   ----------------------

   function Map_Simulink_Var
     (Corresponding_Feature : Node_Id) return Node_Id
   is
      Variable_Name : Name_Id;
      Simulink_Var  : Node_Id;
   begin
      Set_Str_To_Name_Buffer ("");

      Get_Name_String
        (Get_Source_Name (Parent_Component (Corresponding_Feature)));

      if Is_In (Corresponding_Feature) then
         Add_Str_To_Name_Buffer ("_U");
      end if;

      if Is_Out (Corresponding_Feature) then
         Add_Str_To_Name_Buffer ("_Y");
      end if;

      Variable_Name := Name_Find;

      Simulink_Var :=
        Make_Member_Designator
          (Make_Defining_Identifier
             (Get_Source_Name (Corresponding_Feature),
              False),
           Make_Defining_Identifier (Variable_Name, False),
           False);
      return Simulink_Var;
   end Map_Simulink_Var;

   ----------------------------
   -- Map_Simulink_Init_Func --
   ----------------------------

   function Map_Simulink_Init_Func (Subprogram : Node_Id) return Node_Id is
   begin
      Get_Name_String (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_InitializeDataMapInfo");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Simulink_Init_Func;

   -----------------------------
   -- Map_Simulink_Model_Type --
   -----------------------------

   function Map_Simulink_Model_Type (Subprogram : Node_Id) return Node_Id is
   begin

      Set_Str_To_Name_Buffer ("RT_MODEL_");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Simulink_Model_Type;

   -------------------------
   -- Map_Simulink_Header --
   -------------------------

   function Map_Simulink_Header (Subprogram : Node_Id) return Node_Id is
      N : Name_Id;
   begin

      N := Get_Source_Name (Subprogram);

      if N = No_Name then
         return No_Node;
      end if;

      Get_Name_String (N);
      Add_Str_To_Name_Buffer ("_capi");

      return CTU.Make_Defining_Identifier (Name_Find);
   end Map_Simulink_Header;

   ---------------------
   -- Map_Source_Name --
   ---------------------

   function Map_Source_Name (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Source_Name;

   -----------------------------
   -- Map_Lustre_Context_Name --
   -----------------------------

   function Map_Lustre_Context_Name (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_context");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Context_Name;

   ------------------------------
   -- Map_Lustre_Context_Reset --
   ------------------------------

   function Map_Lustre_Context_Reset (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_reset");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Context_Reset;

   -----------------------------
   -- Map_Lustre_Context_Init --
   -----------------------------

   function Map_Lustre_Context_Init (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_new_ctx");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Context_Init;

   -----------------------------
   -- Map_Lustre_Context_Type --
   -----------------------------

   function Map_Lustre_Context_Type (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("struct ");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_ctx");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Context_Type;

   --------------------------
   -- Map_Lustre_Step_Name --
   --------------------------

   function Map_Lustre_Step_Name (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_step");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Step_Name;

   ------------------------------------
   -- Map_Lustre_Input_Function_Name --
   ------------------------------------

   function Map_Lustre_Input_Function_Name
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id
   is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_I_");
      Get_Name_String_And_Append (Get_Source_Name (Port));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Input_Function_Name;

   -------------------------
   -- Map_Lustre_Temp_Var --
   -------------------------

   function Map_Lustre_Temp_Var
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id
   is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_");
      Get_Name_String_And_Append (Get_Source_Name (Port));
      Add_Str_To_Name_Buffer ("_lustre_tmpvar");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Temp_Var;

   -------------------------------------
   -- Map_Lustre_Output_Function_Name --
   -------------------------------------

   function Map_Lustre_Output_Function_Name
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id
   is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_O_");
      Get_Name_String_And_Append (Get_Source_Name (Port));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Lustre_Output_Function_Name;

   ------------------------
   -- Map_Esterel_Header --
   ------------------------

   function Map_Esterel_Header (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Location (Subprogram));
      Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len - 2));
      Add_Str_To_Name_Buffer ("_strl");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Esterel_Header;

   --------------------------------
   -- Map_Esterel_Reset_Function --
   --------------------------------

   function Map_Esterel_Reset_Function (Subprogram : Node_Id) return Node_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_reset");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Esterel_Reset_Function;

   --------------------------------
   -- Map_Esterel_Input_Function --
   --------------------------------

   function Map_Esterel_Input_Function
     (Subprogram : Node_Id;
      Feature    : Node_Id) return Node_Id
   is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_I_");
      Get_Name_String_And_Append (Get_Source_Name (Feature));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Esterel_Input_Function;

   ---------------------------------
   -- Map_Esterel_Output_Function --
   ---------------------------------

   function Map_Esterel_Output_Function
     (Subprogram : Node_Id;
      Feature    : Node_Id) return Node_Id
   is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_O_");
      Get_Name_String_And_Append (Get_Source_Name (Feature));

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Esterel_Output_Function;

   --------------------------
   -- Map_Esterel_Temp_Var --
   --------------------------

   function Map_Esterel_Temp_Var
     (Subprogram : Node_Id;
      Port       : Node_Id) return Node_Id
   is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append (Get_Source_Name (Subprogram));
      Add_Str_To_Name_Buffer ("_");
      Get_Name_String_And_Append (Get_Source_Name (Port));
      Add_Str_To_Name_Buffer ("_esterel_tmpvar");

      return CTU.Make_Defining_Identifier (Name_Find, C_Conversion => False);
   end Map_Esterel_Temp_Var;

   ------------------------------
   -- Map_C_Feature_Subprogram --
   ------------------------------

   function Map_C_Feature_Subprogram
     (A     : Node_Id;
      Owner : Node_Id := No_Node) return Node_Id
   is
      I      : Node_Id;
      Result : Node_Id;
      M      : Name_Id;
      N      : Name_Id;
   begin
      if Kind (A) /= K_Identifier then
         I := Identifier (A);
      end if;

      if Present (Owner) then
         M := CTU.To_C_Name (Display_Name (Identifier (Owner)));
      else
         M := CTU.To_C_Name (Display_Name (Identifier (Parent_Component (A))));
      end if;

      N := CTU.To_C_Name (Display_Name (I));

      Get_Name_String (M);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (N);

      N      := Name_Find;
      Result := CTU.Make_Defining_Identifier (N);

      return Result;
   end Map_C_Feature_Subprogram;

   ---------------------------------
   -- Map_C_Marshaller_Subprogram --
   ---------------------------------

   function Map_C_Marshaller_Subprogram
     (A             : Node_Id;
      Is_Unmarshall : Boolean := False;
      Is_Request    : Boolean := False) return Node_Id
   is
      I      : Node_Id;
      Result : Node_Id;
      N      : Name_Id;
      C_Name : Name_Id;
   begin
      I := Identifier (A);

      if Kind (A) = K_Port_Spec_Instance then
         C_Name :=
           CTU.To_C_Name
             (Display_Name
                (Identifier (Parent_Subcomponent (Parent_Component (A)))));
      end if;

      N := CTU.To_C_Name (Display_Name (I));

      if Is_Request then
         if Is_Unmarshall then
            Set_Str_To_Name_Buffer ("__po_hi_unmarshall_request_");
         else
            Set_Str_To_Name_Buffer ("__po_hi_marshall_request_");
         end if;
      else
         if Is_Unmarshall then
            Set_Str_To_Name_Buffer ("__po_hi_unmarshall_type_");
         else
            Set_Str_To_Name_Buffer ("__po_hi_marshall_type_");
         end if;
      end if;

      if Kind (A) = K_Port_Spec_Instance then
         Get_Name_String_And_Append (C_Name);
         Add_Str_To_Name_Buffer ("_");
      end if;

      Get_Name_String_And_Append (N);

      N      := Name_Find;
      Result := CTU.Make_Defining_Identifier (N);
      return Result;
   end Map_C_Marshaller_Subprogram;

   -----------------------
   -- Bind_AADL_To_Stub --
   -----------------------

   procedure Bind_AADL_To_Stub (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Stub_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Stub;

   ------------------------
   -- Bind_AADL_To_Types --
   ------------------------

   procedure Bind_AADL_To_Types (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Types_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Types;

   --------------------------------
   -- Bind_AADL_To_Default_Value --
   --------------------------------

   procedure Bind_AADL_To_Default_Value (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Default_Value_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Default_Value;

   -------------------------
   -- Bind_AADL_To_Object --
   -------------------------

   procedure Bind_AADL_To_Object (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Object_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Object;

   -----------------------------
   -- Bind_AADL_To_Marshaller --
   -----------------------------

   procedure Bind_AADL_To_Marshaller (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Marshaller_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Marshaller;

   -------------------------------
   -- Bind_AADL_To_Unmarshaller --
   -------------------------------

   procedure Bind_AADL_To_Unmarshaller (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (CTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      CTN.Set_Unmarshaller_Node (N, A);
      CTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Unmarshaller;

   -------------------------
   -- Map_Stub_Identifier --
   -------------------------

   function Map_Stub_Identifier (E : Node_Id) return Node_Id is
   begin
      Get_Name_String (To_C_Name (AIN.Display_Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_stub");
      return Make_Defining_Identifier (Name_Find);
   end Map_Stub_Identifier;

   ---------------------------
   -- Map_C_Subprogram_Spec --
   ---------------------------

   function Map_C_Subprogram_Spec
     (S                 : Node_Id;
      Containing_Device : Node_Id := No_Node) return Node_Id
   is
      Profile : constant List_Id := CTU.New_List (CTN.K_Parameter_Profile);
      Param   : Node_Id;
      Mode    : Mode_Id;
      F       : Node_Id;
      N       : Node_Id;
      D       : Node_Id;
      Field   : Node_Id;
   begin
      pragma Assert (AINU.Is_Subprogram (S));

      if Get_Current_Backend_Kind = PolyORB_HI_C
        and then Containing_Device /= No_Node
      then
         Param :=
           CTU.Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (VN (V_Dev_Id)),
              Parameter_Type      => (RE (RE_Device_Id)));
         CTU.Append_Node_To_List (Param, Profile);
      end if;

      --  We build the parameter profile of the subprogram instance by
      --  adding:

      --  First, the parameter features mapping

      if not AINU.Is_Empty (Features (S)) then
         F := AIN.First_Node (Features (S));

         while Present (F) loop
            if Kind (F) = K_Parameter_Instance then
               if Is_In (F) and then Is_Out (F) then
                  Mode := Mode_Inout;
               elsif Is_Out (F) then
                  Mode := Mode_Out;
               elsif Is_In (F) then
                  Mode := Mode_In;
               else
                  Display_Located_Error
                    (AIN.Loc (F),
                     "Unspecified parameter mode",
                     Fatal => True);
               end if;

               D := Corresponding_Instance (F);

               if Mode = Mode_In then
                  Param :=
                    CTU.Make_Parameter_Specification
                      (Defining_Identifier => Map_C_Defining_Identifier (F),
                       Parameter_Type      => Map_C_Data_Type_Designator (D));
               else
                  Param :=
                    CTU.Make_Parameter_Specification
                      (Defining_Identifier => Map_C_Defining_Identifier (F),
                       Parameter_Type      =>
                         CTU.Make_Pointer_Type
                           (Map_C_Data_Type_Designator (D)));
               end if;
               CTU.Append_Node_To_List (Param, Profile);
            end if;

            F := AIN.Next_Node (F);
         end loop;
      end if;

      --  Second, the data access mapping. The data accesses are not
      --  mapped in the case of pure call sequence subprogram because
      --  they are used only to close the access chain.

      if not AINU.Is_Empty (Features (S)) then
         F := AIN.First_Node (Features (S));

         while Present (F) loop
            if Kind (F) = K_Subcomponent_Access_Instance then
               case Get_Required_Data_Access (Corresponding_Instance (F)) is
                  when Access_Read_Only =>
                     Mode := Mode_In;
                  when Access_Write_Only =>
                     Mode := Mode_Out;
                  when Access_Read_Write =>
                     Mode := Mode_Inout;
                  when Access_None =>
                     --  By default, we allow read/write access
                     Mode := Mode_Inout;
                  when others =>
                     Display_Located_Error
                       (AIN.Loc (F),
                        "Unsupported required access",
                        Fatal => True);
               end case;

               D := Corresponding_Instance (F);

               case Get_Data_Representation (D) is
                  when Data_Integer     |
                    Data_Boolean        |
                    Data_Float          |
                    Data_Fixed          |
                    Data_Struct         |
                    Data_String         |
                    Data_Wide_String    |
                    Data_Character      |
                    Data_Wide_Character |
                    Data_Array          =>
                     --  If the data component is a simple data
                     --  component (not a structure), we simply add a
                     --  parameter with the computed mode and with a
                     --  type mapped from the data component.

                     if Mode = Mode_In then
                        Param :=
                          CTU.Make_Parameter_Specification
                            (Defining_Identifier =>
                               Map_C_Defining_Identifier (F),
                             Parameter_Type =>
                               Map_C_Data_Type_Designator (D));
                     else
                        Param :=
                          CTU.Make_Parameter_Specification
                            (Defining_Identifier =>
                               Map_C_Defining_Identifier (F),
                             Parameter_Type =>
                               CTU.Make_Pointer_Type
                                 (Map_C_Data_Type_Designator (D)));
                     end if;

                     CTU.Append_Node_To_List (Param, Profile);

                  when Data_With_Accessors =>
                     --  If the data component is a complex data
                     --  component (which has subcomponents), we add a
                     --  parameter with the computed mode and with a
                     --  type mapped from each subcomponent type.

                     Field := AIN.First_Node (Subcomponents (D));

                     while Present (Field) loop
                        if AINU.Is_Data
                            (Corresponding_Instance (Field))
                        then
                           if Mode = Mode_In then
                              Param :=
                               CTU.Make_Parameter_Specification
                                 (Defining_Identifier =>
                                     Map_C_Defining_Identifier (Field),
                                   Parameter_Type =>
                                     Map_C_Data_Type_Designator
                                       (Corresponding_Instance (Field)));
                           else
                              Param :=
                                CTU.Make_Parameter_Specification
                                  (Defining_Identifier =>
                                     Map_C_Defining_Identifier (Field),
                                   Parameter_Type =>
                                     Make_Pointer_Type
                                       (Map_C_Data_Type_Designator
                                          (Corresponding_Instance
                                             (Field))));
                           end if;
                           CTU.Append_Node_To_List (Param, Profile);
                        end if;
                        Field := AIN.Next_Node (Field);
                     end loop;

                  when others =>
                     Display_Located_Error
                       (AIN.Loc (F),
                        "Unsupported data type",
                        Fatal => True);
               end case;
            end if;

            F := AIN.Next_Node (F);
         end loop;
      end if;

      N :=
        CTU.Make_Function_Specification
          (Defining_Identifier => Map_C_Defining_Identifier (S),
           Parameters          => Profile,
           Return_Type         => New_Node (CTN.K_Void));

      return N;
   end Map_C_Subprogram_Spec;

   ---------------------------
   -- Map_C_Subprogram_Body --
   ---------------------------

   function Map_C_Subprogram_Body
     (S                 : Node_Id;
      Containing_Device : Node_Id := No_Node) return Node_Id
   is
      Spec : constant Node_Id := Map_C_Subprogram_Spec (S, Containing_Device);
      User_Spec : constant Node_Id :=
        Map_C_Subprogram_Spec (S, Containing_Device);
      Declarations : constant List_Id := New_List (CTN.K_Declaration_List);
      Statements : constant List_Id := New_List (CTN.K_Statement_List);
      Call_Profile         : List_Id := New_List (CTN.K_Parameter_Profile);
      D                    : Node_Id;
      N                    : Node_Id;
      P                    : Node_Id;
      T                    : Node_Id;
      Z                    : Node_Id;
      Feature              : Node_Id;
      Left_Assign          : Node_Id;
      Right_Assign         : Node_Id;
      Call_Parameters      : List_Id;
      Return_Code_Declared : Boolean          := False;
      Function_Call        : Node_Id;
      Function_Name        : Node_Id;
      Param_Representation : Supported_Data_Representation;
   begin
      case Get_Subprogram_Kind (S) is
         when Subprogram_Empty =>
            --  An empty AADL subprogram is mapped into a C subprogram
            --  that does nothing.

            N := Message_Comment ("Empty subprogram");
            CTU.Append_Node_To_List (N, Statements);

            return Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Lua =>
            if Get_Source_Text (S)'Size = 0 then
               Display_Located_Error
                 (AIN.Loc (S),
                  "This subprogram must have the property Source_Text",
                  Fatal => True);
            end if;

            Append_Node_To_List
              (Make_Variable_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Lua_Context)),
                  Used_Type => RE (RE_Lua_Context_T)),
               Declarations);

            Append_Node_To_List
              (Make_Variable_Address
                 (Make_Defining_Identifier (VN (V_Lua_Context))),
               Call_Profile);

            Append_Node_To_List
              (Make_Literal
                 (C_Values.New_Pointed_Char_Value (Get_Source_Text (S) (1))),
               Call_Profile);

            Append_Node_To_List
              (CTU.Make_Call_Profile (RE (RE_LUA_Load), Call_Profile),
               Statements);

            if Get_Source_Name (S) /= No_Name then
               --  Init the function call of the LUA function
               Call_Profile := New_List (CTN.K_Parameter_Profile);

               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier (VN (V_Lua_Context))),
                  Call_Profile);

               Append_Node_To_List
                 (Make_Literal
                    (C_Values.New_Pointed_Char_Value (Get_Source_Name (S))),
                  Call_Profile);

               Append_Node_To_List
                 (CTU.Make_Call_Profile
                    (RE (RE_LUA_Init_Function_Call),
                     Call_Profile),
                  Statements);

               --  Push all the IN parameters on the stack
               if not AINU.Is_Empty (Features (S)) then
                  Feature := AIN.First_Node (Features (S));

                  while Present (Feature) loop
                     if Kind (Feature) = K_Parameter_Instance
                       and then Is_In (Feature)
                     then

                        D := Corresponding_Instance (Feature);

                        Param_Representation := Get_Data_Representation (D);

                        Call_Profile := New_List (CTN.K_Parameter_Profile);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (VN (V_Lua_Context))),
                           Call_Profile);

                        Append_Node_To_List
                          (Map_C_Defining_Identifier (Feature),
                           Call_Profile);

                        if Param_Representation = Data_Integer then
                           Function_Name := RE (RE_LUA_Push_Number);
                        elsif Param_Representation = Data_String then
                           Function_Name := RE (RE_LUA_Push_String);
                        elsif Param_Representation = Data_Boolean then
                           Function_Name := RE (RE_LUA_Push_Boolean);
                        else
                           Display_Located_Error
                             (AIN.Loc (Feature),
                              "Type of this feature is not supported " &
                              "for LUA interface",
                              Fatal => True);
                        end if;

                        Append_Node_To_List
                          (CTU.Make_Call_Profile (Function_Name, Call_Profile),
                           Statements);
                     end if;
                     Feature := AIN.Next_Node (Feature);
                  end loop;
               end if;

               --  Perform the function call of the LUA function
               Call_Profile := New_List (CTN.K_Parameter_Profile);

               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier (VN (V_Lua_Context))),
                  Call_Profile);

               Append_Node_To_List
                 (CTU.Make_Call_Profile
                    (RE (RE_LUA_Perform_Function_Call),
                     Call_Profile),
                  Statements);

               --  Get all the OUT parameters from LUA to C.
               if not AINU.Is_Empty (Features (S)) then
                  Feature := AIN.First_Node (Features (S));

                  while Present (Feature) loop
                     if Kind (Feature) = K_Parameter_Instance
                       and then Is_Out (Feature)
                     then

                        D := Corresponding_Instance (Feature);

                        Param_Representation := Get_Data_Representation (D);

                        Call_Profile := New_List (CTN.K_Parameter_Profile);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (VN (V_Lua_Context))),
                           Call_Profile);

                        Append_Node_To_List
                          (Make_Literal
                             (C_Values.New_Pointed_Char_Value
                                (To_C_Name
                                   (AIN.Name (AIN.Identifier (Feature))))),
                           Call_Profile);

                        Append_Node_To_List
                          (Map_C_Defining_Identifier (Feature),
                           Call_Profile);

                        if Param_Representation = Data_Integer then
                           Function_Name := RE (RE_LUA_Get_Number);
                        elsif Param_Representation = Data_String then
                           Function_Name := RE (RE_LUA_Get_String);
                        elsif Param_Representation = Data_Boolean then
                           Function_Name := RE (RE_LUA_Get_Boolean);
                        else
                           Display_Located_Error
                             (AIN.Loc (Feature),
                              "Type of this feature is not supported " &
                              "for LUA interface",
                              Fatal => True);
                        end if;

                        Append_Node_To_List
                          (CTU.Make_Call_Profile (Function_Name, Call_Profile),
                           Statements);
                     end if;
                     Feature := AIN.Next_Node (Feature);
                  end loop;
               end if;

            end if;

            return Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Opaque_C |
           Subprogram_Opaque_CPP  |
           Subprogram_Default     =>

            --  Create function to lock the data.

            if not AINU.Is_Empty (Features (S))
              and then Get_Current_Backend_Kind = PolyORB_Kernel_C
            then
               Feature := AIN.First_Node (Features (S));

               while Present (Feature) loop
                  if Kind (Feature) = K_Subcomponent_Access_Instance
                    and then Is_Protected_Data
                      (Corresponding_Instance (Feature))
                  then
                     --  Declare the return variable added to
                     --  function-call.  This variable must be
                     --  declared only one time in the function.
                     if not Return_Code_Declared then
                        POK_Declare_Return_Variable (Declarations);
                        Return_Code_Declared := True;
                     end if;

                     Call_Parameters := New_List (CTN.K_Parameter_List);

                     if Get_Accessed_Data (Feature) = No_Node then
                        Display_Located_Error
                          (AIN.Loc (Feature),
                           "This parameter is not connected correctly",
                           Fatal => True);
                     end if;

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_Associated_Locking_Entity_Name
                             (Get_Accessed_Data (Feature))),
                        Call_Parameters);

                     Append_Node_To_List
                       (Make_Literal (New_Int_Value (0, 1, 10)),
                        Call_Parameters);

                     if POK_Flavor = ARINC653 then
                        Add_Return_Variable_In_Parameters (Call_Parameters);

                        Function_Call :=
                          POK_Make_Function_Call_With_Assert
                            (RF (RE_Wait_Semaphore),
                             Call_Parameters);
                     else
                        Function_Call :=
                          POK_Make_Function_Call_With_Assert
                            (RF (RE_Pok_Sem_Wait),
                             Call_Parameters);
                     end if;
                     Append_Node_To_List (Function_Call, Statements);
                  end if;
                  Feature := AIN.Next_Node (Feature);
               end loop;
            end if;

            if not Is_Empty (Parameters (Spec)) then
               P := CTN.First_Node (CTN.Parameters (Spec));
               while Present (P) loop
                  Append_Node_To_List
                    (Copy_Node (Defining_Identifier (P)),
                     Call_Profile);
                  P := CTN.Next_Node (P);
               end loop;
            end if;

            --  Add the definition of the function provided by the user
            --  Don't use definition before use can cause some problems
            --  at the run-time.

            Set_Defining_Identifier
              (User_Spec,
               (Make_Defining_Identifier (Get_Source_Name (S))));
            Append_Node_To_List (User_Spec, CTN.Declarations (Current_File));

            --  Then, call the function provided by the user in our
            --  subprogram.

            N :=
              Make_Call_Profile
                (Make_Defining_Identifier (Get_Source_Name (S)),
                 Call_Profile);
            Append_Node_To_List (N, Statements);

            --  Create function to unlock the data.

            if not AINU.Is_Empty (Features (S))
              and then Get_Current_Backend_Kind = PolyORB_Kernel_C
            then
               Feature := AIN.First_Node (Features (S));

               while Present (Feature) loop
                  if Kind (Feature) = K_Subcomponent_Access_Instance
                    and then Is_Protected_Data
                      (Corresponding_Instance (Feature))
                  then
                     Call_Parameters := New_List (CTN.K_Parameter_List);

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_Associated_Locking_Entity_Name
                             (Get_Accessed_Data (Feature))),
                        Call_Parameters);

                     if POK_Flavor = ARINC653 then
                        Add_Return_Variable_In_Parameters (Call_Parameters);

                        Function_Call :=
                          POK_Make_Function_Call_With_Assert
                            (RF (RE_Signal_Semaphore),
                             Call_Parameters);
                     else
                        Function_Call :=
                          POK_Make_Function_Call_With_Assert
                            (RF (RE_Pok_Sem_Signal),
                             Call_Parameters);
                     end if;
                     Append_Node_To_List (Function_Call, Statements);
                  end if;
                  Feature := AIN.Next_Node (Feature);
               end loop;
            end if;

            return CTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Opaque_Ada_95 =>
            if not Is_Empty (Parameters (Spec)) then
               P := CTN.First_Node (CTN.Parameters (Spec));
               while Present (P) loop
                  Append_Node_To_List
                    (Copy_Node (Defining_Identifier (P)),
                     Call_Profile);
                  P := CTN.Next_Node (P);
               end loop;
            end if;

            --  Add the definition of the function provided by the user
            --  Don't use definition before use can cause some problems
            --  at the run-time.

            Set_Defining_Identifier
              (User_Spec,
               (Make_Defining_Identifier
                  (Get_Source_Name (S),
                   Ada_Conversion => True)));
            Append_Node_To_List (User_Spec, CTN.Declarations (Current_File));

            --  Then, call the function provided by the user in our
            --  subprogram.

            N :=
              Make_Call_Profile
                (Make_Defining_Identifier
                   (Get_Source_Name (S),
                    Ada_Conversion => True),
                 Call_Profile);

            Append_Node_To_List (N, Statements);

            return CTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Simulink =>
            Add_Include
              (Make_Include_Clause
                 (Make_Defining_Identifier (Get_Source_Name (S), True),
                  True),
               True);

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_In (P) then
                     Left_Assign  := Map_Simulink_Var (P);
                     Right_Assign := Map_C_Defining_Identifier (P);

                     if Get_Data_Representation (Corresponding_Instance (P)) =
                       Data_Array
                     then
                        Append_Node_To_List
                          (Make_Call_Profile
                             (RE (RE_Copy_Array),
                              Make_List_Id
                                (Make_Variable_Address (Left_Assign),
                                 Make_Variable_Address (Right_Assign),
                                 Get_Data_Size (Corresponding_Instance (P)))),
                           Statements);
                     else
                        Append_Node_To_List
                          (Make_Expression
                             (Left_Assign,
                              Op_Equal,
                              Right_Assign),
                           Statements);
                     end if;
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            Append_Node_To_List
              (CTU.Make_Call_Profile (RE (RE_Simulink_Update), No_List),
               Statements);

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_Out (P) then
                     Left_Assign  := Map_C_Defining_Identifier (P);
                     Right_Assign := Map_Simulink_Var (P);
                     CTN.Set_Is_Pointer (Left_Assign, True);

                     if Get_Data_Representation (Corresponding_Instance (P)) =
                       Data_Array
                     then
                        Set_Is_Pointer (Left_Assign, False);
                        Append_Node_To_List
                          (Make_Call_Profile
                             (RE (RE_Copy_Array),
                              Make_List_Id
                                (Left_Assign,
                                 Make_Variable_Address (Right_Assign),
                                 Get_Data_Size (Corresponding_Instance (P)))),
                           Statements);
                     else
                        Append_Node_To_List
                          (Make_Expression
                             (Left_Assign,
                              Op_Equal,
                              Right_Assign),
                           Statements);
                     end if;
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            return CTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Scade =>
            Add_Include
              (Make_Include_Clause (Map_Scade_Function_Name (S), True),
               Preserve_Case => True);
            --  At first, we declare structures used as parameters
            --  for SCADE functions. So, if the subprograms has IN
            --  parameters, we declare as such function. The same
            --  if the subprogram as OUT parameters

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_In (P) then
                     Z := AIN.Corresponding_Instance (P);
                     T := No_Node;

                     if Present (Backend_Node (Identifier (Z)))
                       and then Present
                         (CTN.Type_Definition_Node
                            (Backend_Node (Identifier (Z))))
                     then
                        T :=
                          CTN.Type_Name
                            (CTN.Type_Definition_Node
                               (Backend_Node (Identifier (Z))));
                     end if;

                     Left_Assign :=
                       Make_Member_Designator
                         (Map_Scade_Parameter (P),
                          Make_Defining_Identifier (VN (V_In)));
                     Right_Assign := Map_C_Defining_Identifier (P);

                     if T /= No_Node
                       and then CTN.Kind (T) = CTN.K_Array_Declaration
                     then
                        Call_Profile := New_List (CTN.K_Parameter_Profile);

                        Append_Node_To_List
                          (Make_Variable_Address (Left_Assign),
                           Call_Profile);

                        Append_Node_To_List
                          (Make_Variable_Address (Right_Assign),
                           Call_Profile);

                        Append_Node_To_List (Get_Data_Size (Z), Call_Profile);

                        N :=
                          CTU.Make_Call_Profile
                            (RE (RE_Copy_Array),
                             Call_Profile);
                        Append_Node_To_List (N, Statements);
                     else
                        Append_Node_To_List
                          (Make_Expression
                             (Left_Assign,
                              Op_Equal,
                              Right_Assign),
                           Statements);
                     end if;
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            --  Now, we call the SCADE function

            Call_Profile := New_List (CTN.K_Parameter_Profile);

            Append_Node_To_List
              (Make_Variable_Address (Make_Defining_Identifier (VN (V_In))),
               Call_Profile);

            Append_Node_To_List
              (Make_Variable_Address (Make_Defining_Identifier (VN (V_Out))),
               Call_Profile);

            Append_Node_To_List
              (CTU.Make_Call_Profile
                 (Map_Scade_Function_Name (S),
                  Call_Profile),
               Statements);

            --  Then, we take the out signal from the SCADE code

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_Out (P) then
                     Z := AIN.Corresponding_Instance (P);
                     T := No_Node;

                     if Present (Backend_Node (Identifier (Z)))
                       and then Present
                         (CTN.Type_Definition_Node
                            (Backend_Node (Identifier (Z))))
                     then
                        T :=
                          CTN.Type_Name
                            (CTN.Type_Definition_Node
                               (Backend_Node (Identifier (Z))));
                     end if;

                     Left_Assign  := Map_C_Defining_Identifier (P);
                     Right_Assign :=
                       Make_Member_Designator
                         (Map_Scade_Parameter (P),
                          Make_Defining_Identifier (VN (V_Out)));

                     if T /= No_Node
                       and then CTN.Kind (T) = CTN.K_Array_Declaration
                     then
                        Call_Profile := New_List (CTN.K_Parameter_Profile);

                        Append_Node_To_List (Left_Assign, Call_Profile);

                        Append_Node_To_List
                          (Make_Variable_Address (Right_Assign),
                           Call_Profile);

                        Append_Node_To_List (Get_Data_Size (Z), Call_Profile);

                        N :=
                          CTU.Make_Call_Profile
                            (RE (RE_Copy_Array),
                             Call_Profile);

                        Append_Node_To_List (N, Statements);
                     else
                        CTN.Set_Is_Pointer (Left_Assign, True);

                        Append_Node_To_List
                          (Make_Expression
                             (Left_Assign,
                              Op_Equal,
                              Right_Assign),
                           Statements);
                     end if;
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            return CTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Lustre =>
            Add_Include
              (Make_Include_Clause (Map_Source_Name (S)),
               Preserve_Case => True);

            Append_Node_To_List
              (Make_Extern_Entity_Declaration
                 (Make_Variable_Declaration
                    (Defining_Identifier => Map_Lustre_Context_Name (S),
                     Used_Type           =>
                       Make_Pointer_Type ((Map_Lustre_Context_Type (S))))),
               CTN.Declarations (Current_File));

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_In (P) then
                     Append_Node_To_List
                       (Make_Call_Profile
                          (Map_Lustre_Input_Function_Name (S, P),
                           Make_List_Id
                             (Map_Lustre_Context_Name (S),
                              Map_C_Defining_Identifier (P))),
                        Statements);
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            Append_Node_To_List
              (CTU.Make_Call_Profile
                 (Map_Lustre_Step_Name (S),
                  Make_List_Id (Map_Lustre_Context_Name (S))),
               Statements);

            --  Then, we take the out signal from the Lustre code

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_Out (P) then
                     Z := AIN.Corresponding_Instance (P);
                     T := No_Node;

                     if Present (Backend_Node (Identifier (Z)))
                       and then Present
                         (CTN.Type_Definition_Node
                            (Backend_Node (Identifier (Z))))
                     then
                        T :=
                          CTN.Type_Name
                            (CTN.Type_Definition_Node
                               (Backend_Node (Identifier (Z))));
                     end if;

                     declare
                        Spec  : Node_Id;
                        Stats : constant List_Id :=
                          New_List (CTN.K_Statement_List);
                     begin
                        Append_Node_To_List
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                Map_Lustre_Temp_Var (S, P),
                              Used_Type => CTU.Copy_Node (T)),
                           CTN.Declarations (Current_File));

                        Spec :=
                          CTU.Make_Function_Specification
                            (Defining_Identifier =>
                               Map_Lustre_Output_Function_Name (S, P),
                             Parameters =>
                               Make_List_Id
                                 (Make_Parameter_Specification
                                    (Defining_Identifier =>
                                       Make_Defining_Identifier
                                         (PN (P_Unused)),
                                     Parameter_Type =>
                                       Make_Pointer_Type
                                         (Make_Defining_Identifier
                                            (TN (T_Void)))),
                                  Make_Parameter_Specification
                                    (Defining_Identifier =>
                                       Map_Source_Name (P),
                                     Parameter_Type => CTU.Copy_Node (T))),
                             Return_Type => New_Node (CTN.K_Void));

                        Append_Node_To_List
                          (Make_Expression
                             (Left_Expr  => Map_Lustre_Temp_Var (S, P),
                              Operator   => Op_Equal,
                              Right_Expr => Map_Source_Name (P)),
                           Stats);

                        Append_Node_To_List
                          (Make_Function_Implementation (Spec, No_List, Stats),
                           CTN.Declarations (Current_File));

                        Append_Node_To_List
                          (Make_Expression
                             (Left_Expr => Map_C_Defining_Identifier (P, True),
                              Operator   => Op_Equal,
                              Right_Expr => Map_Lustre_Temp_Var (S, P)),
                           Statements);
                     end;
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            return CTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Esterel =>
            Add_Include
              (Make_Include_Clause (Map_Esterel_Header (S)),
               Preserve_Case => True);

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_In (P) then
                     Append_Node_To_List
                       (Make_Call_Profile
                          (Map_Esterel_Input_Function (S, P),
                           Make_List_Id (Map_C_Defining_Identifier (P))),
                        Statements);
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            Append_Node_To_List
              (Make_Call_Profile (Map_Source_Name (S)),
               Statements);

            if not AINU.Is_Empty (Features (S)) then
               P := AIN.First_Node (Features (S));

               while Present (P) loop
                  if AIN.Is_Out (P) then
                     Z := AIN.Corresponding_Instance (P);
                     T := No_Node;

                     if Present (Backend_Node (Identifier (Z)))
                       and then Present
                         (CTN.Type_Definition_Node
                            (Backend_Node (Identifier (Z))))
                     then
                        T :=
                          CTN.Type_Name
                            (CTN.Type_Definition_Node
                               (Backend_Node (Identifier (Z))));
                     end if;

                     declare
                        Spec  : Node_Id;
                        Stats : constant List_Id :=
                          New_List (CTN.K_Statement_List);
                     begin
                        Append_Node_To_List
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                Map_Esterel_Temp_Var (S, P),
                              Used_Type => CTU.Copy_Node (T)),
                           CTN.Declarations (Current_File));

                        Spec :=
                          CTU.Make_Function_Specification
                            (Defining_Identifier =>
                               Map_Esterel_Output_Function (S, P),
                             Parameters =>
                               Make_List_Id
                                 (Make_Parameter_Specification
                                    (Defining_Identifier =>
                                       Map_Source_Name (P),
                                     Parameter_Type => CTU.Copy_Node (T))),
                             Return_Type => New_Node (CTN.K_Void));

                        Append_Node_To_List
                          (Make_Expression
                             (Left_Expr  => Map_Esterel_Temp_Var (S, P),
                              Operator   => Op_Equal,
                              Right_Expr => Map_Source_Name (P)),
                           Stats);

                        Append_Node_To_List
                          (Make_Function_Implementation (Spec, No_List, Stats),
                           CTN.Declarations (Current_File));

                        Append_Node_To_List
                          (Make_Expression
                             (Left_Expr => Map_C_Defining_Identifier (P, True),
                              Operator   => Op_Equal,
                              Right_Expr => Map_Esterel_Temp_Var (S, P)),
                           Statements);
                     end;
                  end if;

                  P := AIN.Next_Node (P);
               end loop;
            end if;

            return CTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Pure_Call_Sequence =>
            --  A pure call sequence subprogram is a subprogram that
            --  has exactly one call sequence. The behaviour of this
            --  subprogram is simply the call to the subprograms
            --  present in its call list.

            CTU.Handle_Call_Sequence
              (S,
               AIN.First_Node (AIN.Calls (S)),
               Declarations,
               Statements);

            return CTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when others =>
            Display_Located_Error
              (AIN.Loc (S),
               "This kind of subprogram is not supported " &
               Get_Subprogram_Kind (S)'Img,
               Fatal => True);
            return No_Node;
      end case;
   end Map_C_Subprogram_Body;

   --------------
   -- Map_Port --
   --------------

   function Map_Port
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      F      : Node_Id;
      Parent : Node_Id;
   begin
      Set_Str_To_Name_Buffer ("");

      Parent := No_Node;  -- default init.

      if Get_Connection_Pattern (E) = Inter_Process
        or else Get_Port_By_Name (E, Containing_Component) /= No_Node
      then
         if Get_Category_Of_Component
             (Parent_Subcomponent (Parent_Component (E))) =
           CC_Thread
         then
            if Is_In (E) and then not Is_Out (E) then
               if AIN.Sources (E) /= No_List
                 and then not AINU.Is_Empty (AIN.Sources (E))
               then
                  F := Item (AIN.First_Node (AIN.Sources (E)));
               else
                  F      := E;
                  Parent :=
                    Parent_Component
                      (Parent_Subcomponent (Parent_Component (E)));
               end if;
            else
               if AIN.Destinations (E) /= No_List
                 and then not AINU.Is_Empty (AIN.Destinations (E))
               then
                  F := Item (AIN.First_Node (AIN.Destinations (E)));
               else
                  F      := E;
                  Parent :=
                    Parent_Component
                      (Parent_Subcomponent (Parent_Component (E)));
               end if;
            end if;
         elsif Get_Category_Of_Component
             (Parent_Subcomponent (Parent_Component (E))) =
           CC_Process
         then
            F := E;
         elsif Get_Category_Of_Component
             (Parent_Subcomponent (Parent_Component (E))) =
           CC_Device
         then
            F := E;
         end if;

         if Containing_Component = No_Node then
            Parent := Parent_Subcomponent (Parent_Component (F));
         else
            Parent := Parent_Subcomponent (Containing_Component);
         end if;

         Get_Name_String (Display_Name (Identifier (Parent)));
         Add_Str_To_Name_Buffer ("_");

      else
         if Is_Out (E) then
            F := Item (AIN.First_Node (Get_Destination_Ports (E)));
         else
            F := E;
         end if;
      end if;

      Get_Name_String_And_Append (Display_Name (Identifier (F)));

      return Name_Find;
   end Map_Port;

   -------------------
   -- Map_Node_Name --
   -------------------

   function Map_Node_Name (Processor : Node_Id) return Name_Id is
      N : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String
        (Display_Name (Identifier (Parent_Subcomponent (Processor))));
      N := Name_Find;
      return N;
   end Map_Node_Name;

   -------------------
   -- Map_Port_Name --
   -------------------

   function Map_Port_Name
     (E                    : Node_Id;
      Is_Global            : Boolean := False;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin

      --
      --  Typically, the following block of code is used
      --  to prefix the port name by the parent subcomponent
      --  name. For DeOS, we do not prefix by the subcomponent,
      --  Port Name have to be the same.
      --

      if Get_Connection_Pattern (E) = Inter_Process and then
         POK_Flavor = DEOS
      then
         Get_Name_String (Display_Name (Identifier (E)));
         N := Name_Find;
      else
         N := Map_Port (E, Containing_Component);
      end if;

      if Is_Global then
         Get_Name_String (N);
         Add_Str_To_Name_Buffer ("_global");
         N := Name_Find;
      end if;

      return (To_Lower (N));
   end Map_Port_Name;

   ------------------
   -- Map_Bus_Name --
   ------------------

   function Map_Bus_Name (Bus : Node_Id) return Name_Id is
      N : Name_Id;
   begin
      if Bus /= No_Node then
         Set_Str_To_Name_Buffer ("bus_");
         Get_Name_String_And_Append
           (Display_Name (Identifier (Parent_Subcomponent (Bus))));
      else
         Set_Str_To_Name_Buffer ("invalid_bus");
      end if;
      N := Name_Find;
      return (To_Lower (N));
   end Map_Bus_Name;

   ----------------------------
   -- Map_Port_Name_For_Asn1 --
   ----------------------------

   function Map_Port_Name_For_Asn1 (E : Node_Id) return Name_Id is
      N              : Name_Id;
      Thread_Name    : Name_Id;
      Parent_Name    : Name_Id;
      Containing_Thr : Node_Id;
   begin
      Containing_Thr := Parent_Subcomponent (Parent_Component (E));

      Set_Str_To_Name_Buffer ("thread-");
      Parent_Name :=
        Display_Name
          (Identifier
             (Parent_Subcomponent (Parent_Component (Containing_Thr))));
      Get_Name_String_And_Append (Parent_Name);
      Add_Str_To_Name_Buffer ("-");
      Get_Name_String_And_Append (Display_Name (Identifier (Containing_Thr)));
      Thread_Name := Name_Find;

      Thread_Name := To_Lower (Thread_Name);

      Set_Str_To_Name_Buffer ("port-");
      Get_Name_String_And_Append (Thread_Name);
      Add_Str_To_Name_Buffer ("-");
      Get_Name_String_And_Append (Display_Name (Identifier (E)));
      N := Name_Find;
      return (To_Lower (N));
   end Map_Port_Name_For_Asn1;

   ------------------------------------
   -- Map_Port_Name_Present_For_Asn1 --
   ------------------------------------

   function Map_Port_Name_Present_For_Asn1 (E : Node_Id) return Name_Id is
      N : Name_Id;
   begin
      Get_Name_String (Map_Port_Name_For_Asn1 (E));
      Add_Str_To_Name_Buffer ("_PRESENT");
      N := Name_Find;
      N := Replace_Char (N, '-', '_');
      return (Name_Find);
   end Map_Port_Name_Present_For_Asn1;

   --------------------------------------
   -- Map_Port_Deployment_Destinations --
   --------------------------------------

   function Map_Port_Deployment_Destinations
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin
      N := Map_Port (E, Containing_Component);
      Get_Name_String (N);
      Add_Str_To_Name_Buffer ("_deployment_destinations");
      N := To_Lower (Name_Find);
      return N;
   end Map_Port_Deployment_Destinations;

   ------------------
   -- Map_Port_Var --
   ------------------

   function Map_Port_Var
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin
      N := Map_Port (E, Containing_Component);
      Get_Name_String (N);
      Add_Str_To_Name_Buffer ("_id");

      N := To_Lower (Name_Find);

      return N;
   end Map_Port_Var;

   -------------------
   -- Map_Port_Data --
   -------------------

   function Map_Port_Data
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin
      N := Map_Port (E, Containing_Component);
      Get_Name_String (N);
      Add_Str_To_Name_Buffer ("_dvalue");

      N := To_Lower (Name_Find);

      return N;
   end Map_Port_Data;

   ------------------------------------
   -- Map_Port_Data_With_Virtual_Bus --
   ------------------------------------

   function Map_Port_Data_With_Virtual_Bus
     (E                    : Node_Id;
      Virtual_Bus          : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin
      N := Map_Port_Data (E, Containing_Component);
      Get_Name_String (N);

      Add_Str_To_Name_Buffer ("_virtual_bus_");

      Get_Name_String_And_Append
        (AAN.Display_Name (AAN.Identifier (Virtual_Bus)));

      return Name_Find;
   end Map_Port_Data_With_Virtual_Bus;

   ------------------------------------------
   -- Map_Port_Var_Length_With_Virtual_Bus --
   ------------------------------------------

   function Map_Port_Var_Length_With_Virtual_Bus
     (E                    : Node_Id;
      Virtual_Bus          : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin
      N := Map_Port_Var_Length (E, Containing_Component);
      Get_Name_String (N);

      Add_Str_To_Name_Buffer ("_virtual_bus_");

      Get_Name_String_And_Append
        (AAN.Display_Name (AAN.Identifier (Virtual_Bus)));

      return Name_Find;
   end Map_Port_Var_Length_With_Virtual_Bus;

   -------------------------
   -- Map_Port_Var_Length --
   -------------------------

   function Map_Port_Var_Length
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin
      N := Map_Port (E, Containing_Component);
      Get_Name_String (N);
      Add_Str_To_Name_Buffer ("_length");

      N := To_Lower (Name_Find);

      return N;
   end Map_Port_Var_Length;

   ------------------------
   -- Map_Port_Var_Valid --
   ------------------------

   function Map_Port_Var_Valid
     (E                    : Node_Id;
      Containing_Component : Node_Id := No_Node) return Name_Id
   is
      N : Name_Id;
   begin
      N := Map_Port (E, Containing_Component);
      Get_Name_String (N);
      Add_Str_To_Name_Buffer ("_valid");

      N := To_Lower (Name_Find);

      return N;
   end Map_Port_Var_Valid;

   -----------------------------------
   -- Map_Port_Deployment_Partition --
   -----------------------------------

   function Map_Port_Deployment_Partition (E : Node_Id) return Name_Id is
   begin
      Get_Name_String
        (To_C_Name (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));
      Add_Str_To_Name_Buffer ("_partport");
      return (To_Lower (Name_Find));
   end Map_Port_Deployment_Partition;

   --------------
   -- Map_Time --
   --------------

   function Map_Time
     (T        : Time_Type;
      Variable : Name_Id := No_Name) return Node_Id
   is
      Time       : Unsigned_Long_Long;
      S          : Node_Id;
      Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
   begin
      case T.U is
         when Picosecond =>
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               --  POK framework only support microseconds
               --  Picosecond and Nanosecond are ignored
               if T.T mod 1_000_000_000 = 0 then
                  Time := T.T / 1_000_000_000;
                  S    := PKR.RF (RE_Pok_Time_Milliseconds);
               else
                  return No_Node;
               end if;
            elsif Get_Current_Backend_Kind = PolyORB_HI_C then
               if T.T mod 1_000_000 = 0 then
                  Time := T.T / 1_000_000;
                  S    := PHR.RE (RE_Microseconds);
               else
                  return No_Node;
               end if;
            end if;

         when Nanosecond =>
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               if T.T mod 1000 = 0 then
                  Time := T.T / 1_000_000;
                  S    := PKR.RF (RE_Pok_Time_Milliseconds);
               else
                  return No_Node;
               end if;
            elsif Get_Current_Backend_Kind = PolyORB_HI_C then
               if T.T mod 1000 = 0 then
                  Time := T.T / 1000;
                  S    := PHR.RE (RE_Microseconds);
               else
                  return No_Node;
               end if;
               S := RE (RE_Microseconds);
            end if;

         when Microsecond =>
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               Time := T.T * 1_000;
               S    := PKR.RF (RE_Pok_Time_Milliseconds);
            elsif Get_Current_Backend_Kind = PolyORB_HI_C then
               Time := T.T;
               S    := PHR.RE (RE_Microseconds);
            end if;

         when Millisecond =>
            Time := T.T;

            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               S := PKR.RF (RE_Pok_Time_Milliseconds);
            elsif Get_Current_Backend_Kind = PolyORB_HI_C then
               S := PHR.RE (RE_Milliseconds);
            end if;

         when Second =>
            Time := T.T;
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               S := PKR.RF (RE_Pok_Time_Seconds);
            elsif Get_Current_Backend_Kind = PolyORB_HI_C then
               S := PHR.RE (RE_Seconds);
            end if;

         when Minute =>
            Time := T.T;
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               S := PKR.RF (RE_Pok_Time_Minutes);
            elsif Get_Current_Backend_Kind = PolyORB_HI_C then
               S := PHR.RE (RE_Minutes);
            end if;

         when Hour =>
            --  Convert it into minutes
            Time := T.T * 60;
            if Get_Current_Backend_Kind = PolyORB_Kernel_C then
               S := PKR.RF (RE_Pok_Time_Minutes);
            elsif Get_Current_Backend_Kind = PolyORB_HI_C then
               S := PHR.RE (RE_Minutes);
            end if;
      end case;

      if Variable /= No_Name then
         Append_Node_To_List
           (Make_Variable_Address (Make_Defining_Identifier (Variable)),
            Parameters);
      end if;

      Append_Node_To_List
        (Make_Literal (New_Int_Value (Time, 1, 10)),
         Parameters);

      return Make_Call_Profile (S, Parameters);
   end Map_Time;

   ----------------------------
   -- Map_Time_To_Nanosecond --
   ----------------------------

   function Map_Time_To_Nanosecond (T : Time_Type) return Node_Id is
      Time : Unsigned_Long_Long;
   begin
      case T.U is
         when Millisecond =>
            Time := T.T * 1000_000;

         when Second =>
            Time := T.T * 1000_000_000;

         when Microsecond =>
            Time := T.T * 1000;

         when Nanosecond =>
            Time := T.T;

         when others =>
            raise Program_Error with "time value not handled at this time";
      end case;

      return Make_Literal (New_Int_Value (Time, 1, 10));
   end Map_Time_To_Nanosecond;

   -----------------------------
   -- Map_Time_To_Millisecond --
   -----------------------------

   function Map_Time_To_Millisecond (T : Time_Type) return Node_Id is
      Time : Unsigned_Long_Long;
   begin
      case T.U is
         when Millisecond =>
            Time := T.T;

         when Second =>
            Time := T.T * 1000;

         when Microsecond =>
            Time := T.T / 1000;

         when Nanosecond =>
            Time := T.T / 1_000_000;

         when others =>
            raise Program_Error with "time value not handled at this time";
      end case;

      return Make_Literal (New_Int_Value (Time, 1, 10));
   end Map_Time_To_Millisecond;

   ---------------------
   -- Map_Needs_Macro --
   ---------------------

   function Map_Needs_Macro (Name : Name_Id) return Node_Id is
      R : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("POK_NEEDS_");
      Get_Name_String_And_Append (Name);
      R := Name_Find;
      R := To_Upper (R);
      return Make_Defining_Identifier (R, False, False);
   end Map_Needs_Macro;

   ----------------------
   --  Map_Queue_Size  --
   ----------------------

   function Map_Queue_Size (Port : Node_Id) return Node_Id is
      Queue_Size : Unsigned_Long_Long := 1;
   begin
      if Get_Queue_Size (Port) /= -1 then
         Queue_Size := Unsigned_Long_Long (Get_Queue_Size (Port));
      end if;

      return Make_Literal (New_Int_Value (Queue_Size, 1, 10));
   end Map_Queue_Size;

   --------------------------------
   --  Map_Queue_Size_With_Data  --
   --------------------------------

   function Map_Queue_Size_With_Data (Port : Node_Id) return Node_Id is
      Size   : Node_Id;
      Result : Node_Id;
   begin
      Size := CTU.Get_Data_Size (Corresponding_Instance (Port));

      Result :=
        Make_Expression
          (Left_Expr  => Map_Queue_Size (Port),
           Operator   => Op_Asterisk,
           Right_Expr => Size);

      return Result;
   end Map_Queue_Size_With_Data;

   ----------------------
   --  Map_POK_Action  --
   ----------------------

   function Map_POK_Action
     (Action              : Supported_POK_Action;
      Thread_Id           : Unsigned_Long_Long := 0;
      Corresponding_Error : Node_Id            := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      case Action is
         when POK_Action_Ignore =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Error_Ignore),
                 Make_List_Id
                   (Copy_Node (Corresponding_Error),
                    Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when POK_Action_Confirm =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Error_Ignore),
                 Make_List_Id
                   (Copy_Node (Corresponding_Error),
                    Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when POK_Action_Thread_Restart =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Thread_Restart),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when POK_Action_Thread_Stop_And_Start_Another =>
            Display_Error
              ("The process stop and start another is not supported",
               Fatal => True);

         when POK_Action_Thread_Stop =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Thread_Stop),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when POK_Action_Partition_Restart =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Partition_Set_Mode),
                 Make_List_Id (RE (RE_Pok_Partition_Mode_Init_Warm)));

         when POK_Action_Partition_stop =>
            N :=
              Make_Call_Profile
                (RF (RE_Pok_Partition_Set_Mode),
                 Make_List_Id (RE (RE_Pok_Partition_Mode_Stopped)));

         when POK_Action_Nothing =>
            Display_Error
              ("The nothing action cannot be raised at thread level",
               Fatal => True);

         when others =>
            Display_Error ("Invalid action for a thread", Fatal => True);
      end case;

      return N;
   end Map_POK_Action;

   function Map_POK_Action
     (Action              : Supported_ARINC653_Action;
      Thread_Id           : Unsigned_Long_Long := 0;
      Corresponding_Error : Node_Id            := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      case Action is
         when ARINC653_Action_Ignore =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Error_Ignore),
                 Make_List_Id
                   (Copy_Node (Corresponding_Error),
                    Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when ARINC653_Action_Confirm =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Error_Confirm),
                 Make_List_Id
                   (Copy_Node (Corresponding_Error),
                    Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when ARINC653_Action_Process_Restart =>
            N :=
              Make_Call_Profile
                (RF (RE_Pok_Thread_Restart),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when ARINC653_Action_Process_Stop_And_Start_Another =>
            Display_Error
              ("The process stop and start another is not supported",
               Fatal => True);

         when ARINC653_Action_Process_Stop =>
            N :=
              Make_Call_Profile
                (RF (RE_Pok_Thread_Stop),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Thread_Id, 1, 10))));

         when ARINC653_Action_Partition_Restart =>
            N :=
              Make_Call_Profile
                (RF (RE_Pok_Partition_Set_Mode),
                 Make_List_Id (RE (RE_Pok_Partition_Mode_Init_Warm)));

         when ARINC653_Action_Partition_Stop =>
            N :=
              Make_Call_Profile
                (RF (RE_Pok_Partition_Set_Mode),
                 Make_List_Id (RE (RE_Pok_Partition_Mode_Stopped)));

         when ARINC653_Action_Nothing =>
            Display_Error
              ("The nothing action cannot be raised at thread level",
               Fatal => True);

         when others =>
            Display_Error ("Invalid action for a thread", Fatal => True);
      end case;

      return N;
   end Map_POK_Action;

   -----------------------------
   --  Map_POK_Kernel_Action  --
   -----------------------------

   function Map_POK_Kernel_Action
     (Action       : Supported_POK_Action;
      Partition_Id : Unsigned_Long_Long := 0;
      Kernel_Level : Boolean            := True) return Node_Id
   is
      N : Node_Id;
   begin
      case Action is
         when POK_Action_Ignore =>
            N := Make_Call_Profile (RE (RE_Pok_Error_Ignore), No_List);

         when POK_Action_Kernel_Stop =>
            N := Make_Call_Profile (RE (RE_Pok_Kernel_Stop), No_List);

         when POK_Action_Kernel_Restart =>
            N := Make_Call_Profile (RE (RE_Pok_Kernel_Restart), No_List);

         when POK_Action_Partition_Restart =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Partition_Set_Mode),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Partition_Id, 1, 10)),
                    RE (RE_Pok_Partition_Mode_Init_Warm)));

         when POK_Action_Partition_stop =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Partition_Set_Mode),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Partition_Id, 1, 10)),
                    RE (RE_Pok_Partition_Mode_Stopped)));

         when POK_Action_Nothing =>
            if Kernel_Level then
               N :=
                 Make_Call_Profile
                   (RE (RE_Pok_Error_Kernel_Callback),
                    No_List);
            else
               N :=
                 Make_Call_Profile
                   (RE (RE_Pok_Error_Partition_Callback),
                    Make_List_Id
                      (Make_Literal (New_Int_Value (Partition_Id, 1, 10))));
            end if;

         when others =>
            Display_Error
              ("Invalid action for a partition or the kernel",
               Fatal => True);
      end case;

      return N;
   end Map_POK_Kernel_Action;

   function Map_POK_Kernel_Action
     (Action       : Supported_ARINC653_Action;
      Partition_Id : Unsigned_Long_Long := 0;
      Kernel_Level : Boolean            := True) return Node_Id
   is
      N : Node_Id;
   begin
      case Action is
         when ARINC653_Action_Ignore =>
            N := Make_Call_Profile (RE (RE_Pok_Error_Ignore), No_List);

         when ARINC653_Action_Module_Stop =>
            N := Make_Call_Profile (RE (RE_Pok_Kernel_Stop), No_List);

         when ARINC653_Action_Module_Restart =>
            N := Make_Call_Profile (RE (RE_Pok_Kernel_Restart), No_List);

         when ARINC653_Action_Partition_Restart =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Partition_Set_Mode),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Partition_Id, 1, 10)),
                    RE (RE_Pok_Partition_Mode_Init_Warm)));

         when ARINC653_Action_Partition_Stop =>
            N :=
              Make_Call_Profile
                (RE (RE_Pok_Partition_Set_Mode),
                 Make_List_Id
                   (Make_Literal (New_Int_Value (Partition_Id, 1, 10)),
                    RE (RE_Pok_Partition_Mode_Stopped)));

         when ARINC653_Action_Nothing =>
            if Kernel_Level then
               N :=
                 Make_Call_Profile
                   (RE (RE_Pok_Error_Kernel_Callback),
                    No_List);
            else
               N :=
                 Make_Call_Profile
                   (RE (RE_Pok_Error_Partition_Callback),
                    Make_List_Id
                      (Make_Literal (New_Int_Value (Partition_Id, 1, 10))));
            end if;

         when others =>
            Display_Error
              ("Invalid action for a partition or the kernel",
               Fatal => True);
      end case;

      return N;
   end Map_POK_Kernel_Action;

   ------------------------------------------
   --  Map_Associated_Locking_Entity_Name  --
   ------------------------------------------

   function Map_Associated_Locking_Entity_Name (E : Node_Id) return Name_Id is
      Locking_Entity_Name : Name_Id;
   begin
      Get_Name_String (To_C_Name (AIN.Display_Name (Identifier (E))));

      Add_Str_To_Name_Buffer ("_locking_entity");

      Locking_Entity_Name := Name_Find;

      return Locking_Entity_Name;
   end Map_Associated_Locking_Entity_Name;

   ---------------------
   --  Map_POK_Error  --
   ---------------------

   function Map_POK_Error (Error : Supported_POK_Error) return Node_Id is
   begin
      case Error is
         --  Here, for each error, we create a switch
         --  case that will handle each declared error.

         when POK_Error_Deadline_Missed =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Deadline_Missed);
            else
               return RE (RE_Pok_Error_Kind_Deadline_Missed);
            end if;

         when POK_Error_Application =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Application_Error);
            else
               return RE (RE_Pok_Error_Kind_Application_Error);
            end if;

         when POK_Error_Numeric =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Numeric_Error);
            else
               return RE (RE_Pok_Error_Kind_Numeric_Error);
            end if;

         when POK_Error_Illegal_Request =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Illegal_Request);
            else
               return RE (RE_Pok_Error_Kind_Illegal_Request);
            end if;

         when POK_Error_Stack_Overflow =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Stack_Overflow);
            else
               return RE (RE_Pok_Error_Kind_Stack_Overflow);
            end if;

         when POK_Error_Memory_Violation =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Memory_Violation);
            else
               return RE (RE_Pok_Error_Kind_Memory_Violation);
            end if;

         when POK_Error_Hardware_Fault =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Hardware_Fault);
            else
               return RE (RE_Pok_Error_Kind_Hardware_Fault);
            end if;

         when POK_Error_Power_Fail =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Power_Fail);
            else
               return RE (RE_Pok_Error_Kind_Power_Fail);
            end if;

         when others =>
            return No_Node;
      end case;
   end Map_POK_Error;

   --------------------------
   --  Map_ARINC653_Error  --
   --------------------------

   function Map_ARINC653_Error
     (Error : Supported_ARINC653_Error) return Node_Id
   is
   begin
      case Error is
         --  Here, for each error, we create a switch
         --  case that will handle each declared error.

         when ARINC653_Error_Deadline_Miss =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Deadline_Missed);
            else
               return RE (RE_Pok_Error_Kind_Deadline_Missed);
            end if;

         when ARINC653_Error_Application =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Application_Error);
            else
               return RE (RE_Pok_Error_Kind_Application_Error);
            end if;

         when ARINC653_Error_Numeric =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Numeric_Error);
            else
               return RE (RE_Pok_Error_Kind_Numeric_Error);
            end if;

         when ARINC653_Error_Illegal_Request =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Illegal_Request);
            else
               return RE (RE_Pok_Error_Kind_Illegal_Request);
            end if;

         when ARINC653_Error_Stack_Overflow =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Stack_Overflow);
            else
               return RE (RE_Pok_Error_Kind_Stack_Overflow);
            end if;

         when ARINC653_Error_Memory_Violation =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Memory_Violation);
            else
               return RE (RE_Pok_Error_Kind_Memory_Violation);
            end if;

         when ARINC653_Error_Hardware_Fault =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Hardware_Fault);
            else
               return RE (RE_Pok_Error_Kind_Hardware_Fault);
            end if;

         when ARINC653_Error_Power_Fail =>
            if POK_Flavor = ARINC653 then
               return RE (RE_Power_Fail);
            else
               return RE (RE_Pok_Error_Kind_Power_Fail);
            end if;

         when others =>
            return No_Node;
      end case;
   end Map_ARINC653_Error;

   --------------------------------
   --  Map_Device_Function_Read  --
   --------------------------------

   function Map_Device_Function_Read (Device : Node_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String
        (To_C_Name (Get_String_Property (Device, "pok::device_name")));
      Add_Str_To_Name_Buffer ("_read");
      return Name_Find;
   end Map_Device_Function_Read;

   ---------------------------------
   --  Map_Device_Function_Write  --
   ---------------------------------

   function Map_Device_Function_Write (Device : Node_Id) return Name_Id is
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String
        (To_C_Name (Get_String_Property (Device, "pok::device_name")));
      Add_Str_To_Name_Buffer ("_write");
      return Name_Find;
   end Map_Device_Function_Write;

   -----------------------------
   --  Map_Virtual_Bus_Calls  --
   -----------------------------

   procedure Map_Virtual_Bus_Calls
     (Port                 :     Node_Id;
      Declarations         :     List_Id;
      Statements           :     List_Id;
      Handled_Kind         :     Virtual_Bus_Call_Kind;
      New_Data             : out Node_Id;
      New_Size             : out Node_Id;
      Containing_Component :     Node_Id := No_Node)
   is
      Virtual_Buses   : List_Id;
      Virtual_Bus     : Node_Id;
      Implementation  : Node_Id;
      Subprogram      : Node_Id;
      Function_Call   : Node_Id;
      Property_Node   : Node_Id;
      Tmp_Node        : Node_Id;
      VB_Type         : Node_Id;
      Marshall_Type   : Node_Id := No_Node;
      Call_Parameters : List_Id;
   begin
      New_Data := No_Node;
      New_Size := No_Node;

      Virtual_Buses := Get_Associated_Virtual_Buses (Port);

      if Virtual_Buses = No_List then
         return;
      end if;

      Tmp_Node := AAN.First_Node (Virtual_Buses);

      while Present (Tmp_Node) loop
         Virtual_Bus   := AAN.Entity (Tmp_Node);
         Property_Node :=
           Look_For_Property_In_Declarative (Virtual_Bus, "implemented_as");

         --  Now, we are trying to catch the value of the "Implemented_As"
         --  property of the virtual bus. The virtual bus should have
         --  a property Implemented_As with an abstract component that
         --  describe how the virtual bus is implemented (subprograms, data,
         --  ...). Then, we will browse this abstract component to call
         --  the right functions.

         if Property_Node = No_Node then
            exit;
         end if;

         Implementation := AAN.Entity (AAN.Single_Value (Property_Node));

         --  Here, get the type we use to marshall data

         VB_Type :=
           Look_For_Subcomponent_In_Declarative
             (Implementation,
              "marshalling_type");
         if VB_Type /= No_Node then
            Property_Node :=
              Look_For_Property_In_Declarative (VB_Type, "type_source_name");

            if Property_Node /= No_Node then
               Marshall_Type :=
                 Make_Defining_Identifier
                   (Get_String_Of_Property_Value
                      (AAN.Single_Value (Property_Node)));
            end if;
         end if;

         if Marshall_Type = No_Node then
            Marshall_Type := Make_Defining_Identifier (TN (T_Int));
         end if;

         --  Finished to retrieve the type used to marshall data

         if Handled_Kind = Sending then
            Subprogram :=
              Look_For_Subcomponent_In_Declarative (Implementation, "send");
         else
            Subprogram :=
              Look_For_Subcomponent_In_Declarative (Implementation, "receive");
         end if;

         --  We catch the subprogram relevant for sending/receiving data.

         if Subprogram = No_Node then
            return;
         end if;

         Property_Node :=
           Look_For_Property_In_Declarative (Subprogram, "source_name");
         --  Here, the Source_Name property corresponds to the
         --  name of the subprogram. This subprogram is contained
         --  in an abstract component.

         if Property_Node /= No_Node then
            Append_Node_To_List
              (Make_Variable_Declaration
                 (Defining_Identifier =>
                    (Make_Defining_Identifier
                       (Map_Port_Data_With_Virtual_Bus
                          (Port,
                           Virtual_Bus,
                           Containing_Component))),
                  Used_Type =>
                    Get_Type_Identifier_Associated_With_Virtual_Bus (Port)),
               Declarations);

            --  Declare the variable that will store the result
            --  of the virtual bus invokation.

            Append_Node_To_List
              (Make_Variable_Declaration
                 (Defining_Identifier =>
                    (Make_Defining_Identifier
                       (Map_Port_Var_Length_With_Virtual_Bus
                          (Port,
                           Virtual_Bus,
                           Containing_Component))),
                  Used_Type => (RE (RE_Size_T))),
               Declarations);

            --  Declare the variable that will store data length
            --  of the virtual bus invokation.

            Call_Parameters := New_List (CTN.K_Parameter_List);

            if Handled_Kind = Sending then
               --  Here, it is the order of the sending side.
               --  We invoke the function like this:
               --
               --  receiving_function
               --  (data_to_marshall, size, data_marshalled, size)

               if Get_Data_Representation (Corresponding_Instance (Port)) =
                 Data_Array
               then
                  Append_Node_To_List
                    (Make_Defining_Identifier
                       (Map_Port_Data (Port, Containing_Component)),
                     Call_Parameters);
               else
                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Defining_Identifier
                          (Map_Port_Data (Port, Containing_Component))),
                     Call_Parameters);
               end if;

               Append_Node_To_List
                 (Get_Data_Size (Corresponding_Instance (Port)),
                  Call_Parameters);

               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier
                       (Map_Port_Data_With_Virtual_Bus
                          (Port,
                           Virtual_Bus,
                           Containing_Component))),
                  Call_Parameters);

               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier
                       (Map_Port_Var_Length_With_Virtual_Bus
                          (Port,
                           Virtual_Bus,
                           Containing_Component))),
                  Call_Parameters);

            elsif Handled_Kind = Receiving then

               --  Here, it is the order of the receiving side.
               --  We invoke the function like this:
               --
               --  receiving_function
               --  (data_to_unmarshall, size, data_unmarshalled, size)

               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier
                       (Map_Port_Data_With_Virtual_Bus
                          (Port,
                           Virtual_Bus,
                           Containing_Component))),
                  Call_Parameters);

               Append_Node_To_List
                 (CTU.Get_Data_Size (Corresponding_Instance (Port)),
                  Call_Parameters);

               if Get_Data_Representation (Corresponding_Instance (Port)) =
                 Data_Array
               then
                  Append_Node_To_List
                    (Make_Defining_Identifier
                       (Map_Port_Data (Port, Containing_Component)),
                     Call_Parameters);
               else
                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Defining_Identifier
                          (Map_Port_Data (Port, Containing_Component))),
                     Call_Parameters);
               end if;

               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier
                       (Map_Port_Var_Length (Port, Containing_Component))),
                  Call_Parameters);

            end if;

            Function_Call :=
              Make_Call_Profile
                (Make_Defining_Identifier
                   (Get_String_Of_Property_Value
                      (AAN.Single_Value (Property_Node))),
                 Call_Parameters);
            Append_Node_To_List (Function_Call, Statements);

            --  Finally, we make the final function call
            --  that invokes our marshaller/unmarshaller.

            New_Data :=
              Make_Defining_Identifier
                (Map_Port_Data_With_Virtual_Bus
                   (Port,
                    Virtual_Bus,
                    Containing_Component));

            New_Size :=
              Make_Defining_Identifier
                (Map_Port_Var_Length_With_Virtual_Bus
                   (Port,
                    Virtual_Bus,
                    Containing_Component));
            --  We create identifier so that the backend
            --  can use the identifier we used in our marshall
            --  functions to invoke code, receive/send data/...

         end if;

         Tmp_Node := AAN.Next_Node (Tmp_Node);
      end loop;
   end Map_Virtual_Bus_Calls;

   -------------------------------------------------------
   --  Get_Type_Identifier_Associated_With_Virtual_Bus  --
   -------------------------------------------------------

   function Get_Type_Identifier_Associated_With_Virtual_Bus
     (Port : Node_Id) return Node_Id
   is
      Instance_Type : Node_Id;
   begin
      Instance_Type := Get_Instance_Type_Associated_With_Virtual_Bus (Port);
      if Instance_Type /= No_Node
        and then Is_Defined_Property (Instance_Type, "type_source_name")
      then
         return Make_Defining_Identifier
             (To_C_Name
                (Get_String_Property (Instance_Type, "type_source_name")));
      end if;

      return Map_C_Data_Type_Designator (Corresponding_Instance (Port));
   end Get_Type_Identifier_Associated_With_Virtual_Bus;

   ---------------------------------------
   --  Handle_Virtual_Buses_Properties  --
   ---------------------------------------

   procedure Handle_Virtual_Buses_Properties (Port : Node_Id) is
      Virtual_Buses : List_Id;
      Tmp_Node      : Node_Id;
      Virtual_Bus   : Node_Id;
      Macro_Value   : Node_Id;
      Macro_Name    : Node_Id;
      Property_Node : Node_Id;
   begin
      Virtual_Buses := Get_Associated_Virtual_Buses (Port);

      if ATU.Is_Empty (Virtual_Buses) then
         return;
      end if;

      Tmp_Node := AAN.First_Node (Virtual_Buses);

      while Present (Tmp_Node) loop
         Virtual_Bus := AAN.Entity (Tmp_Node);

         Property_Node :=
           Look_For_Property_In_Declarative (Virtual_Bus, "pok::protocol");

         if Property_Node /= No_Node then
            if Get_Enumeration_Of_Property_Value
                (AAN.Single_Value (Property_Node)) =
              "ceasar"
            then
               CTU.Append_Node_To_List
                 (CTU.Make_Define_Statement
                    (Defining_Identifier => RE (RE_Pok_Needs_Protocols_Ceasar),
                     Value => CTU.Make_Literal (New_Int_Value (1, 1, 10))),
                  CTN.Declarations (CTU.Current_File));
            end if;
         end if;

         if Property_Node /= No_Node then
            if Get_Enumeration_Of_Property_Value
                (AAN.Single_Value (Property_Node)) =
              "des"
            then
               CTU.Append_Node_To_List
                 (CTU.Make_Define_Statement
                    (Defining_Identifier => RE (RE_Pok_Needs_Protocols_Des),
                     Value => CTU.Make_Literal (New_Int_Value (1, 1, 10))),
                  CTN.Declarations (CTU.Current_File));
            end if;
         end if;

         if Property_Node /= No_Node then
            if Get_Enumeration_Of_Property_Value
                (AAN.Single_Value (Property_Node)) =
              "blowfish"
            then
               CTU.Append_Node_To_List
                 (CTU.Make_Define_Statement
                    (Defining_Identifier =>
                       RE (RE_Pok_Needs_Protocols_Blowfish),
                     Value => CTU.Make_Literal (New_Int_Value (1, 1, 10))),
                  CTN.Declarations (CTU.Current_File));
            end if;
         end if;

         --  In the previous lines of code, we check the POK::Protocol
         --  property. This property declares which protocol we are
         --  using. Depending on the detected protocol, we activate
         --  it in POK by declaring an appropriate macro.

         Property_Node :=
           Look_For_Property_In_Declarative (Virtual_Bus, "pok::des_key");

         if Property_Node /= No_Node then
            Macro_Name := RE (RE_Pok_Protocols_Des_Key);

            Macro_Value :=
              Make_Defining_Identifier
                (Get_String_Of_Property_Value
                   (AAN.Single_Value (Property_Node)));
            Append_Node_To_List
              (Make_Define_Statement
                 (Defining_Identifier => Macro_Name,
                  Value               => Macro_Value),
               CTN.Declarations (CTU.Current_File));
         end if;

         Property_Node :=
           Look_For_Property_In_Declarative (Virtual_Bus, "pok::des_init");

         if Property_Node /= No_Node then
            Macro_Name := RE (RE_Pok_Protocols_Des_Init);

            Macro_Value :=
              Make_Defining_Identifier
                (Get_String_Of_Property_Value
                   (AAN.Single_Value (Property_Node)));
            Append_Node_To_List
              (Make_Define_Statement
                 (Defining_Identifier => Macro_Name,
                  Value               => Macro_Value),
               CTN.Declarations (CTU.Current_File));
         end if;

         --  The part declares all the code required
         --  to configure the DES cipher algorithm.

         Property_Node :=
           Look_For_Property_In_Declarative
             (Virtual_Bus,
              "pok::blowfish_init");

         if Property_Node /= No_Node then
            Macro_Name := RE (RE_Pok_Protocols_Blowfish_Init);

            Macro_Value :=
              Make_Defining_Identifier
                (Get_String_Of_Property_Value
                   (AAN.Single_Value (Property_Node)));
            Append_Node_To_List
              (Make_Define_Statement
                 (Defining_Identifier => Macro_Name,
                  Value               => Macro_Value),
               CTN.Declarations (CTU.Current_File));
         end if;

         Property_Node :=
           Look_For_Property_In_Declarative (Virtual_Bus, "pok::blowfish_key");

         if Property_Node /= No_Node then
            Macro_Name := RE (RE_Pok_Protocols_Blowfish_Key);

            Macro_Value :=
              Make_Defining_Identifier
                (Get_String_Of_Property_Value
                   (AAN.Single_Value (Property_Node)));
            Append_Node_To_List
              (Make_Define_Statement
                 (Defining_Identifier => Macro_Name,
                  Value               => Macro_Value),
               CTN.Declarations (CTU.Current_File));

            --  The part declares all the code required
            --  to configure the blowfish cipher algorithm.
         end if;

         Tmp_Node := AAN.Next_Node (Tmp_Node);
      end loop;
   end Handle_Virtual_Buses_Properties;

   ----------------------------------
   -- Map_Devices_Buses_Array_Name --
   ----------------------------------

   function Map_Devices_Buses_Array_Name (E : Node_Id) return Name_Id is
   begin
      if AINU.Is_Device (E) then
         Set_Str_To_Name_Buffer ("device_");
         Get_Name_String_And_Append
           (AIN.Name (AIN.Identifier (Parent_Subcomponent (E))));
         Add_Str_To_Name_Buffer ("_accessed_buses");
         return Name_Find;
      end if;
      return No_Name;
   end Map_Devices_Buses_Array_Name;

   -----------------------------
   -- Map_Device_Confvar_Name --
   -----------------------------

   function Map_Device_Confvar_Name (E : Node_Id) return Name_Id is
   begin
      if AINU.Is_Device (E) then
         Set_Str_To_Name_Buffer ("pohidrv_");
         Get_Name_String_And_Append
           (AIN.Name (AIN.Identifier (Parent_Subcomponent (E))));
         return Name_Find;
      end if;
      return No_Name;
   end Map_Device_Confvar_Name;

   ------------------
   -- Map_ASN_Type --
   ------------------

   function Map_ASN_Type (ASN_Name : Name_Id) return Name_Id is
      Converted : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("__po_hi_c_");
      Get_Name_String_And_Append (ASN_Name);
      Converted := Name_Find;
      Converted := Replace_Char (Converted, '-', '_');
      return To_Lower (Converted);
   end Map_ASN_Type;

end Ocarina.Backends.C_Common.Mapping;
