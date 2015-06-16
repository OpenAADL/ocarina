------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B A C K E N D S . P O _ H I _ A D A . M A P P I N G    --
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

with Ocarina.Namet; use Ocarina.Namet;
with Utils; use Utils;

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.PO_HI_Ada.Runtime;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_HI_Ada.Mapping is

   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Ada_Tree.Nodes;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_HI_Ada.Runtime;
   use Ocarina.Backends.Ada_Values;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   ---------------------------
   -- Bind_AADL_To_Activity --
   ---------------------------

   procedure Bind_AADL_To_Activity (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Activity_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Activity;

   -----------------------------
   -- Bind_AADL_To_Deployment --
   -----------------------------

   procedure Bind_AADL_To_Deployment (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Deployment_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Deployment;

   -----------------------------
   -- Bind_AADL_To_Enumerator --
   -----------------------------

   procedure Bind_AADL_To_Enumerator (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Enumerator_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Enumerator;

   ----------------------
   -- Bind_AADL_To_Job --
   ----------------------

   procedure Bind_AADL_To_Job (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Job_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Job;

   -----------------------
   -- Bind_AADL_To_Main --
   -----------------------

   procedure Bind_AADL_To_Main (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Main_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Main;

   ---------------------------
   -- Bind_AADL_To_Marshall --
   ---------------------------

   procedure Bind_AADL_To_Marshall (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Marshall_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Marshall;

   ------------------------------
   -- Bind_AADL_To_Marshallers --
   ------------------------------

   procedure Bind_AADL_To_Marshallers (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Marshallers_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Marshallers;

   -------------------------
   -- Bind_AADL_To_Naming --
   -------------------------

   procedure Bind_AADL_To_Naming (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Naming_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Naming;

   ----------------------------
   -- Bind_AADL_To_Transport --
   ----------------------------

   procedure Bind_AADL_To_Transport (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Transport_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Transport;

   -------------------------
   -- Bind_AADL_To_Object --
   -------------------------

   procedure Bind_AADL_To_Object (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Object_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Object;

   -------------------------------------
   -- Bind_AADL_To_Feature_Subprogram --
   -------------------------------------

   procedure Bind_AADL_To_Feature_Subprogram (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Feature_Subprogram_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Feature_Subprogram;

   -----------------------------
   -- Bind_AADL_To_Subprogram --
   -----------------------------

   procedure Bind_AADL_To_Subprogram (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Subprogram_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Subprogram;

   ------------------------------
   -- Bind_AADL_To_Subprograms --
   ------------------------------

   procedure Bind_AADL_To_Subprograms (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Subprograms_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Subprograms;

   ----------------------------------
   -- Bind_AADL_To_Type_Definition --
   ----------------------------------

   procedure Bind_AADL_To_Type_Definition (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Type_Definition_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Type_Definition;

   --------------------------------
   -- Bind_AADL_To_Default_Value --
   --------------------------------

   procedure Bind_AADL_To_Default_Value (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Default_Value_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Default_Value;

   ---------------------------------
   -- Bind_AADL_To_Port_Interface --
   ---------------------------------

   procedure Bind_AADL_To_Port_Interface (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Port_Interface_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Port_Interface;

   -----------------------------------
   -- Bind_AADL_To_Port_Enumeration --
   -----------------------------------

   procedure Bind_AADL_To_Port_Enumeration (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Port_Enumeration_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Port_Enumeration;

   ------------------------
   -- Bind_AADL_To_Types --
   ------------------------

   procedure Bind_AADL_To_Types (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Types_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Types;

   -----------------------------
   -- Bind_AADL_To_Unmarshall --
   -----------------------------

   procedure Bind_AADL_To_Unmarshall (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Unmarshall_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Unmarshall;

   ----------------------------
   -- Bind_AADL_To_Get_Value --
   ----------------------------

   procedure Bind_AADL_To_Get_Value (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Get_Value_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Get_Value;

   ----------------------------
   -- Bind_AADL_To_Put_Value --
   ----------------------------

   procedure Bind_AADL_To_Put_Value (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Put_Value_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Put_Value;

   ----------------------------
   -- Bind_AADL_To_Get_Count --
   ----------------------------

   procedure Bind_AADL_To_Get_Count (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Get_Count_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Get_Count;

   -----------------------------
   -- Bind_AADL_To_Next_Value --
   -----------------------------

   procedure Bind_AADL_To_Next_Value (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Next_Value_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Next_Value;

   -----------------------------------------
   -- Bind_AADL_To_Store_Received_Message --
   -----------------------------------------

   procedure Bind_AADL_To_Store_Received_Message (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Store_Received_Message_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Store_Received_Message;

   --------------------------
   -- Bind_AADL_To_Deliver --
   --------------------------

   procedure Bind_AADL_To_Deliver (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Deliver_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Deliver;

   -----------------------
   -- Bind_AADL_To_Send --
   -----------------------

   procedure Bind_AADL_To_Send (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Send_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Send;

   ------------------------
   -- Extract_Enumerator --
   ------------------------

   function Extract_Enumerator
     (E : Node_Id;
      D : Boolean := True) return Node_Id
   is
      I : Node_Id;
   begin
      pragma Assert
        (AAU.Is_Process (E)
         or else AAU.Is_Thread (E)
         or else AAU.Is_Device (E)
         or else Kind (E) = K_Port_Spec_Instance);

      if AAU.Is_Process (E) or else AAU.Is_Thread (E) or else D then
         declare
            S : Node_Id := E;
         begin
            if AAU.Is_Process (E) or else AAU.Is_Thread (E) then
               S := Parent_Subcomponent (E);
            end if;

            I := Copy_Node (Enumerator_Node (Backend_Node (Identifier (S))));
            Set_Homogeneous_Parent_Unit_Name
              (I,
               RU (RU_PolyORB_HI_Generated_Deployment));
         end;
      else
         declare
            T : constant Node_Id := Parent_Component (E);
            P : constant Node_Id :=
              Extract_Designator
                (ADN.Parent
                   (ADN.Port_Enumeration_Node
                      (Backend_Node (Identifier (T)))));
         begin
            I := Map_Ada_Defining_Identifier (E);
            Set_Homogeneous_Parent_Unit_Name (I, P);
         end;
      end if;

      return I;
   end Extract_Enumerator;

   ---------------------------------
   -- Map_Distributed_Application --
   ---------------------------------

   function Map_Distributed_Application (E : Node_Id) return Node_Id is
      D : constant Node_Id := New_Node (ADN.K_HI_Distributed_Application);
   begin
      pragma Assert (AAU.Is_System (E));

      --  Update the global variable to be able to fetch the root of
      --  the distributed application and generate the source files.

      Ada_Root := D;

      ADN.Set_Name (D, To_Ada_Name (AIN.Name (AIN.Identifier (E))));
      ADN.Set_Units (D, New_List (ADN.K_List_Id));
      ADN.Set_HI_Nodes (D, New_List (ADN.K_List_Id));

      return D;
   end Map_Distributed_Application;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (ADN.K_HI_Node);
   begin
      pragma Assert (AAU.Is_Process (E));

      --  The name of the node is not the name of the process
      --  component instance, but the name of the process subcomponent
      --  corresponding to this instance.

      ADN.Set_Name
        (N,
         To_Ada_Name
           (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));

      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list of the PolyORB-HI
      --  distributed application. We are sure that the top of the
      --  entity stack contains the Ada distributed application node.

      Append_Node_To_List (N, HI_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------

   function Map_HI_Unit (E : Node_Id) return Node_Id is
      U        : Node_Id;
      L        : List_Id;
      N        : Node_Id;
      P        : Node_Id;
      RG       : Node_Id;
      Ada_Name : Name_Id;
   begin
      pragma Assert (AAU.Is_Process (E));

      U := New_Node (ADN.K_HI_Unit, AIN.Identifier (E));
      L := New_List (K_Packages);
      Set_Packages (U, L);

      Ada_Name :=
        To_Ada_Name
          (AIN.Display_Name (AIN.Identifier (AIN.Parent_Subcomponent (E))));

      --  We build a virtual root corresponding to the
      --  PolyORB_HI_Generated package. This is only done to assign
      --  the correct parent to all the packages below and does not
      --  lead to the generation of the root package.

      N  := Defining_Identifier (RU (RU_PolyORB_HI_Generated, False));
      RG := Make_Package_Declaration (N);
      Set_Distributed_Application_Unit (RG, U);

      --  The 'Naming' package

      N := Defining_Identifier (RU (RU_PolyORB_HI_Generated_Naming, False));
      P := Make_Package_Declaration (N);
      ADN.Set_Parent (P, RG);
      Set_Distributed_Application_Unit (P, U);
      Set_Naming_Package (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Naming (Identifier (E), P);

      --  The 'Deployment' package

      N :=
        Defining_Identifier (RU (RU_PolyORB_HI_Generated_Deployment, False));
      P := Make_Package_Declaration (N);
      ADN.Set_Parent (P, RG);
      Set_Distributed_Application_Unit (P, U);
      Set_Deployment_Package (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Deployment (Identifier (E), P);

      --  The 'Types' package

      N := Defining_Identifier (RU (RU_PolyORB_HI_Generated_Types, False));
      P := Make_Package_Declaration (N);
      ADN.Set_Parent (P, RG);
      Set_Distributed_Application_Unit (P, U);
      Set_Types_Package (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Types (Identifier (E), P);

      --  The 'Marshallers' package

      N :=
        Defining_Identifier (RU (RU_PolyORB_HI_Generated_Marshallers, False));
      P := Make_Package_Declaration (N);
      ADN.Set_Parent (P, RG);
      Set_Distributed_Application_Unit (P, U);
      Set_Marshallers_Package (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Marshallers (Identifier (E), P);

      --  The 'Subprograms' package

      N :=
        Defining_Identifier (RU (RU_PolyORB_HI_Generated_Subprograms, False));
      P := Make_Package_Declaration (N);
      ADN.Set_Parent (P, RG);
      Set_Distributed_Application_Unit (P, U);
      Set_Subprograms_Package (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Subprograms (Identifier (E), P);

      --  The 'Activity' package

      N := Defining_Identifier (RU (RU_PolyORB_HI_Generated_Activity, False));
      P := Make_Package_Declaration (N);
      ADN.Set_Parent (P, RG);
      Set_Distributed_Application_Unit (P, U);
      Set_Activity_Package (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Activity (Identifier (E), P);

      --  The 'Transport' package

      N := Defining_Identifier (RU (RU_PolyORB_HI_Generated_Transport, False));
      P := Make_Package_Declaration (N);
      ADN.Set_Parent (P, RG);
      Set_Distributed_Application_Unit (P, U);
      Set_Transport_Package (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Transport (Identifier (E), P);

      --  Main suprogram

      P :=
        Make_Main_Subprogram_Implementation
          (Make_Defining_Identifier (Ada_Name));
      Set_Distributed_Application_Unit (P, U);
      Set_Main_Subprogram (U, P);
      Append_Node_To_List (P, L);
      Bind_AADL_To_Main (Identifier (E), P);

      --  Append the Unit to the units list of the current Ada
      --  partition.

      Append_Node_To_List (U, Units (Current_Entity));
      ADN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   ------------------
   -- Map_Ada_Time --
   ------------------

   Ada_Time_Routine : constant array (Time_Units) of RE_Id :=
     (Picosecond  => RE_Null,
      Nanosecond  => RE_Nanoseconds,
      Microsecond => RE_Microseconds,
      Millisecond => RE_Milliseconds,
      Second      => RE_Seconds,
      Minute      => RE_Minutes,
      Hour        => RE_Null);

   function Map_Ada_Time (T : Time_Type) return Node_Id is
      Time : Unsigned_Long_Long;
      S    : Node_Id;
   begin
      case T.U is
         when Picosecond =>
            --  If we can convert it into nanosecond, we are
            --  OK. Otherwise this is an error because Ada.Real_Time
            --  does not support picoseconds

            if T.T mod 1000 = 0 then
               Time := T.T / 1000;
               S    := RE (RE_Nanoseconds);
            else
               return No_Node;
            end if;

         when Hour =>
            --  Convert it into minutes

            Time := T.T * 60;
            S    := RE (RE_Minutes);

         when others =>
            Time := T.T;
            S    := RE (Ada_Time_Routine (T.U));
      end case;

      return Make_Subprogram_Call
          (S,
           Make_List_Id (Make_Literal (New_Integer_Value (Time, 1, 10))));
   end Map_Ada_Time;

   ----------------------
   -- Map_Ada_Priority --
   ----------------------

   function Map_Ada_Priority (P : Unsigned_Long_Long) return Node_Id is
   begin
      --  XXX we should use the priority_mapping property from AADLv2

      return Make_Literal (New_Integer_Value (P, 1, 10));
   end Map_Ada_Priority;

   --------------------------
   -- Map_Marshallers_Name --
   --------------------------

   function Map_Marshallers_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Data (E));

      return Map_Ada_Defining_Identifier (E, "Marshallers");
   end Map_Marshallers_Name;

   -----------------------------
   -- Map_Task_Job_Identifier --
   -----------------------------

   function Map_Task_Job_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "Job");
   end Map_Task_Job_Identifier;

   ------------------------------
   -- Map_Task_Init_Identifier --
   ------------------------------

   function Map_Task_Init_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (AAU.Is_Thread (E) or else AAU.Is_Device (E));

      return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "Init");
   end Map_Task_Init_Identifier;

   ---------------------------------
   -- Map_Task_Recover_Identifier --
   ---------------------------------

   function Map_Task_Recover_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "Recover");
   end Map_Task_Recover_Identifier;

   -------------------------
   -- Map_Task_Identifier --
   -------------------------

   function Map_Task_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "Task");
   end Map_Task_Identifier;

   -------------------------------
   -- Map_Port_Enumeration_Name --
   -------------------------------

   function Map_Port_Enumeration_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E) or else AAU.Is_Subprogram (E));

      return Map_Ada_Defining_Identifier (E, "Port_Type");
   end Map_Port_Enumeration_Name;

   -----------------------------
   -- Map_Port_Interface_Name --
   -----------------------------

   function Map_Port_Interface_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E) or else AAU.Is_Subprogram (E));

      return Map_Ada_Defining_Identifier (E, "Interface");
   end Map_Port_Interface_Name;

   --------------------------
   -- Map_Port_Status_Name --
   --------------------------

   function Map_Port_Status_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Subprogram (E));

      return Map_Ada_Defining_Identifier (E, "Status");
   end Map_Port_Status_Name;

   --------------------------
   -- Map_Port_Enumeration --
   --------------------------

   function Map_Port_Enumeration (E : Node_Id) return Node_Id is
      Enumerators : constant List_Id := New_List (ADN.K_Enumeration_Literals);
      F           : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := AIN.First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               Append_Node_To_List
                 (Map_Ada_Defining_Identifier (F),
                  Enumerators);
            end if;

            F := AIN.Next_Node (F);
         end loop;
      end if;

      if Is_Empty (Enumerators) then
         return No_Node;
      else
         return Make_Full_Type_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
              Type_Definition =>
                Make_Enumeration_Type_Definition (Enumerators));
      end if;
   end Map_Port_Enumeration;

   ------------------------
   -- Map_Port_Interface --
   ------------------------

   function Map_Port_Interface (E : Node_Id) return Node_Id is
      Variants   : constant List_Id := New_List (ADN.K_Variant_List);
      Variant    : Node_Id;
      Choice     : Node_Id;
      Component  : Node_Id;
      Components : List_Id;
      F          : Node_Id;
      N          : Node_Id;
   begin
      if not AAU.Is_Empty (Features (E)) then
         F := AIN.First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               --  Create a variant with a choice corresponding to
               --  the enumerator mapped from the port and with a
               --  component having the type of the port (if it is
               --  a data port).

               Variant := New_Node (ADN.K_Variant);
               Append_Node_To_List (Variant, Variants);
               Choice := Map_Ada_Defining_Identifier (F);
               ADN.Set_Discrete_Choices (Variant, Make_List_Id (Choice));

               if AIN.Is_Data (F) then
                  Component :=
                    Make_Component_Declaration
                      (Defining_Identifier =>
                         Make_Defining_Identifier (Map_Ada_Component_Name (F)),
                       Subtype_Indication =>
                         Map_Ada_Data_Type_Designator
                           (Corresponding_Instance (F)));
                  ADN.Set_Component_List (Variant, Make_List_Id (Component));
               end if;
            end if;

            F := AIN.Next_Node (F);
         end loop;
      end if;

      if Is_Empty (Variants) then
         return No_Node;
      else
         Components := New_List (K_Component_List);
         N          :=
           Make_Variant_Part
             (Discriminant => Make_Defining_Identifier (CN (C_Port)),
              Variant_List => Variants);
         Append_Node_To_List (N, Components);

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (Map_Port_Interface_Name (E)),
              Discriminant_Spec =>
                Make_Component_Declaration
                  (Defining_Identifier =>
                     Make_Defining_Identifier (CN (C_Port)),
                   Subtype_Indication =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Expression =>
                     Make_Attribute_Designator
                       (Make_Designator (Map_Port_Enumeration_Name (E)),
                        A_First)),
              Type_Definition =>
                Make_Record_Type_Definition
                  (Make_Record_Definition (Components)));

         return N;
      end if;
   end Map_Port_Interface;

   ---------------------
   -- Map_Port_Status --
   ---------------------

   function Map_Port_Status
     (E                : Node_Id;
      Full_Declaration : Boolean) return Node_Id
   is
      Component_List : List_Id;
      F              : Node_Id;
      N              : Node_Id;
   begin
      --  FIXME: this implementation assumes that the size of the
      --  FIFOs is 1. we shoulds use arrays of the size of each FIFO.

      F := AIN.First_Node (Features (E));

      if Full_Declaration then
         Component_List := New_List (ADN.K_Component_List);

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               --  For each port, we declare a boolean component to
               --  indicate whether the port is triggered or not.

               N :=
                 Make_Component_Declaration
                   (Defining_Identifier => Map_Ada_Defining_Identifier (F),
                    Subtype_Indication  => RE (RE_Boolean),
                    Expression          => RE (RE_False));
               Append_Node_To_List (N, Component_List);

               --  If the port is an event data port, we add a
               --  component having the type of the port.

               if AIN.Is_Data (F) then
                  N :=
                    Make_Component_Declaration
                      (Defining_Identifier =>
                         Make_Defining_Identifier (Map_Ada_Component_Name (F)),
                       Subtype_Indication =>
                         Map_Ada_Data_Type_Designator
                           (Corresponding_Instance (F)));
                  Append_Node_To_List (N, Component_List);
               end if;
            end if;

            F := AIN.Next_Node (F);
         end loop;

         N :=
           Make_Record_Type_Definition
             (Make_Record_Definition (Component_List));
      else
         N := Make_Private_Type_Definition;
      end if;

      N :=
        Make_Full_Type_Declaration
          (Defining_Identifier =>
             Make_Defining_Identifier (Map_Port_Status_Name (E)),
           Type_Definition => N);
      return N;
   end Map_Port_Status;

   ------------------------------
   -- Map_Node_Name_Identifier --
   ------------------------------

   function Map_Node_Name_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (Kind (E) = K_Subcomponent_Instance);

      return Map_Ada_Defining_Identifier (E, "Node_Name");
   end Map_Node_Name_Identifier;

   ------------------
   -- Map_Bus_Name --
   ------------------

   function Map_Bus_Name (E : Node_Id) return Node_Id is
   begin
      if AIN.Kind (E) = K_Component_Instance then
         return Map_Ada_Defining_Identifier
             (Parent_Subcomponent (E),
              "Naming_Table");
      else
         return Map_Ada_Defining_Identifier (E, "Naming_Table");
      end if;
   end Map_Bus_Name;

   ----------------------------
   -- Map_Integer_Array_Name --
   ----------------------------

   function Map_Integer_Array_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (E, "Integer_Array");
   end Map_Integer_Array_Name;

   -------------------------
   -- Map_Kind_Array_Name --
   -------------------------

   function Map_Kind_Array_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (E, "Port_Kind_Array");
   end Map_Kind_Array_Name;

   --------------------------
   -- Map_Image_Array_Name --
   --------------------------

   function Map_Image_Array_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (E, "Port_Image_Array");
   end Map_Image_Array_Name;

   ----------------------------
   -- Map_Address_Array_Name --
   ----------------------------

   function Map_Address_Array_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (E, "Address_Array");
   end Map_Address_Array_Name;

   --------------------------------------
   -- Map_Overflow_Protocol_Array_Name --
   --------------------------------------

   function Map_Overflow_Protocol_Array_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (E, "Overflow_Protocol_Array");
   end Map_Overflow_Protocol_Array_Name;

   -------------------------
   -- Map_Port_Kinds_Name --
   -------------------------

   function Map_Port_Kinds_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "Port_Kinds");
   end Map_Port_Kinds_Name;

   ---------------------------------
   -- Map_Overflow_Protocols_Name --
   ---------------------------------

   function Map_Overflow_Protocols_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "Overflow_Protocols");
   end Map_Overflow_Protocols_Name;

   ------------------------
   -- Map_Urgencies_Name --
   ------------------------

   function Map_Urgencies_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "Urgencies");
   end Map_Urgencies_Name;

   --------------------------
   -- Map_Port_Images_Name --
   --------------------------

   function Map_Port_Images_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "Port_Images");
   end Map_Port_Images_Name;

   -------------------------
   -- Map_FIFO_Sizes_Name --
   -------------------------

   function Map_FIFO_Sizes_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "FIFO_Sizes");
   end Map_FIFO_Sizes_Name;

   ----------------------
   -- Map_Offsets_Name --
   ----------------------

   function Map_Offsets_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "Offsets");
   end Map_Offsets_Name;

   -------------------------
   -- Map_Total_Size_Name --
   -------------------------

   function Map_Total_Size_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "Total_FIFO_Size");
   end Map_Total_Size_Name;

   --------------------------
   -- Map_Destination_Name --
   --------------------------

   function Map_Destination_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert
        (AAU.Is_Thread (E) or else Kind (E) = K_Port_Spec_Instance);

      if AAU.Is_Thread (E) then
         Get_Name_String
           (To_Ada_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      else
         declare
            Thread_Name : constant Name_Id :=
              To_Ada_Name
                (Display_Name
                   (Identifier (Parent_Subcomponent (Parent_Component (E)))));
            Port_Name : constant Name_Id :=
              To_Ada_Name (Display_Name (Identifier (E)));
         begin
            Get_Name_String (Thread_Name);
            Add_Char_To_Name_Buffer ('_');
            Get_Name_String_And_Append (Port_Name);
         end;
      end if;

      Add_Str_To_Name_Buffer ("_Destinations");
      return Name_Find;
   end Map_Destination_Name;

   ----------------------------
   -- Map_N_Destination_Name --
   ----------------------------

   function Map_N_Destination_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "N_Destinations");
   end Map_N_Destination_Name;

   ----------------------------
   -- Map_Interrogators_Name --
   ----------------------------

   function Map_Interrogators_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "Interrogators");
   end Map_Interrogators_Name;

   ----------------------
   -- Map_Deliver_Name --
   ----------------------

   function Map_Deliver_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "Deliver");
   end Map_Deliver_Name;

   -------------------
   -- Map_Send_Name --
   -------------------

   function Map_Send_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "Send");
   end Map_Send_Name;

   --------------------------------
   -- Map_Modes_Enumeration_Name --
   --------------------------------

   function Map_Modes_Enumeration_Name (E : Node_Id) return Name_Id is
      N : constant Name_Id := Get_Thread_Reference_Name (E);
   begin
      pragma Assert (AAU.Is_Thread (E));

      if N = No_Name then
         return Map_Ada_Defining_Identifier
             (Parent_Subcomponent (E),
              "Mode_Type");
      else
         return Get_String_Name (Get_Name_String (N) & "_" & "Mode_Type");
      end if;
   end Map_Modes_Enumeration_Name;

   ---------------------------
   -- Map_Current_Mode_Name --
   ---------------------------

   function Map_Current_Mode_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Map_Ada_Defining_Identifier
          (Parent_Subcomponent (E),
           "Current_Mode");
   end Map_Current_Mode_Name;

   ---------------------------------
   -- Map_Scheduler_Instance_Name --
   ---------------------------------

   function Map_Scheduler_Instance_Name (E : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      return Get_Thread_Scheduler (E);
   end Map_Scheduler_Instance_Name;

   ----------------------------------------
   -- Map_Scheduler_Instance_Object_Name --
   ----------------------------------------

   function Map_Scheduler_Instance_Object_Name (E : Node_Id) return Name_Id is
      N : constant Name_Id := Get_Thread_Reference_Name (E);
   begin
      pragma Assert (AAU.Is_Thread (E));

      if N = No_Name then
         return Map_Ada_Defining_Identifier (Parent_Subcomponent (E), "mode");
      else
         return Get_String_Name (Get_Name_String (N) & "_mode");
      end if;

   end Map_Scheduler_Instance_Object_Name;

   --------------------------------
   -- Map_Exported_Length_Symbol --
   --------------------------------

   function Map_Exported_Length_Symbol (E : Node_Id) return Name_Id is
   begin
      pragma Assert (Get_Data_Representation (E) = Data_Array);

      return To_Lower (Map_Ada_Defining_Identifier (E, "_length"));
   end Map_Exported_Length_Symbol;

   ------------------
   -- Need_Deliver --
   ------------------

   function Need_Deliver (E : Node_Id) return Boolean is
      Result : Boolean := Has_In_Ports (E);
      S      : Node_Id;
   begin
      pragma Assert (AAU.Is_Process (E));

      if not Result and then not AAU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));

         while Present (S) and then not Result loop
            if AAU.Is_Thread (Corresponding_Instance (S)) then
               Result :=
                 Result or else Has_In_Ports (Corresponding_Instance (S));
            end if;

            S := AIN.Next_Node (S);
         end loop;
      end if;

      return Result;
   end Need_Deliver;

   ---------------
   -- Need_Send --
   ---------------

   function Need_Send (E : Node_Id) return Boolean is
      Result : Boolean := Has_Out_Ports (E);
      S      : Node_Id;
   begin
      pragma Assert (AAU.Is_Process (E));

      if not Result and then not AAU.Is_Empty (Subcomponents (E)) then
         S := AIN.First_Node (Subcomponents (E));

         while Present (S) and then not Result loop
            if AAU.Is_Thread (Corresponding_Instance (S)) then
               Result :=
                 Result or else Has_Out_Ports (Corresponding_Instance (S));
            end if;

            S := AIN.Next_Node (S);
         end loop;
      end if;

      return Result;
   end Need_Send;

   --  The two arrays above give the elementary value to compute
   --  Object sizes for the types mapped from AADL data components. We
   --  cannot rely on the value of 'Size, 'Object_Size and 'Alignment
   --  since the value given by these attributes is not always static.
   --  The values indicate the smallest size for the type.

   Elementary_Type_Sizes : constant array
   (Supported_Data_Representation) of Unsigned_Long_Long :=
     (Data_Integer        => 32,  --  Unique size
      Data_Boolean        => 8,   --  Unique size
      Data_Enum           => 0,   --  Depends on the number of enumerators
      Data_Float          => 0,   --  Unsupported
      Data_Fixed          => 64,  --  Unique size
      Data_String         => 64,  --  MAX_LENGTH'Size + CURRENT_LENGTH'Size
      Data_Wide_String    => 64,  --  MAX_LENGTH'Size + CURRENT_LENGTH'Size
      Data_Character      => 8,   --  Unique size
      Data_Wide_Character => 16,  --  Unique size
      Data_Array          => 0,   --  Initial size
      Data_Struct         => 0,   --  Initial size
      Data_Union          => 0,   --  Initial size
      Data_With_Accessors => 0,   --  Unsupported
      Data_None           => 0);  --  Unsupported

   Elementary_Type_Alignments : constant array
   (Supported_Data_Representation) of Unsigned_Long_Long :=
     (Data_Integer        => 4,
      Data_Boolean        => 1,
      Data_Enum           => 4,
      Data_Float          => 1,    --  Unsupported
      Data_Fixed          => 8,
      Data_String         => 4,
      Data_Wide_String    => 4,
      Data_Character      => 1,
      Data_Wide_Character => 2,
      Data_Array          => 4,
      Data_Struct         => 4,
      Data_Union          => 4,
      Data_With_Accessors => 1,    --  Unsupported
      Data_None           => 1);   --  Unsupported

   ------------------------
   -- Estimate_Data_Size --
   ------------------------

   function Estimate_Data_Size (E : Node_Id) return Unsigned_Long_Long is
      function Next_Multiple
        (D : Unsigned_Long_Long;
         M : Unsigned_Long_Long) return Unsigned_Long_Long;
      --  Return the smallest multiple of D greater than or equal M

      -------------------
      -- Next_Multiple --
      -------------------

      function Next_Multiple
        (D : Unsigned_Long_Long;
         M : Unsigned_Long_Long) return Unsigned_Long_Long
      is
      begin
         if M mod D = 0 then
            return M;
         else
            return D * (1 + M / D);
         end if;
      end Next_Multiple;

      Data_Representation : constant Supported_Data_Representation :=
        Get_Data_Representation (E);
   begin
      if Get_Data_Size (E) /= Null_Size then
         --  The user provided an exact size, use it

         return To_Bits (Get_Data_Size (E));
      end if;

      case Data_Representation is
         when Data_Integer     |
           Data_Boolean        |
           Data_Fixed          |
           Data_Character      |
           Data_Wide_Character =>
            return Elementary_Type_Sizes (Data_Representation);

         when Data_Enum =>
            declare
               N_Enumerators : constant Unsigned_Long_Long :=
                 Get_Enumerators (E)'Length;
               pragma Unreferenced (N_Enumerators);
            begin
               --  FIXME: Compute the correct size
               return 16;
            end;

         when Data_String | Data_Wide_String =>
            declare
               Dimension : constant ULL_Array          := Get_Dimension (E);
               L : constant Unsigned_Long_Long := Dimension (Dimension'First);
               Result    : constant Unsigned_Long_Long :=
                 L * Elementary_Type_Sizes (Data_Representation);
            begin
               --  The type size must be integral multiple of the type
               --  alignment.

               return Next_Multiple
                   (Elementary_Type_Alignments (Data_Representation),
                    Result);
            end;

         when Data_Array =>
            declare
               Dimension : constant ULL_Array := Get_Dimension (E);
               Elt       : constant Node_Id   :=
                 ATN.Entity (ATN.First_Node (Get_Base_Type (E)));
               Elt_Type : constant Supported_Data_Representation :=
                 Get_Data_Representation (Elt);
               Elt_Size : constant Unsigned_Long_Long :=
                 Estimate_Data_Size (Elt);
               Result : Unsigned_Long_Long :=
                 Elementary_Type_Sizes (Data_Representation);
            begin
               for D in Dimension'Range loop
                  for J in 1 .. Dimension (D) loop
                     Result :=
                       Next_Multiple
                         (Elementary_Type_Alignments (Elt_Type),
                          Result);
                     Result := Result + Elt_Size;
                  end loop;
               end loop;

               --  The type size must be integral multiple of the type
               --  alignment.

               return Next_Multiple
                   (Elementary_Type_Alignments (Data_Representation),
                    Result);
            end;

         when Data_Struct =>
            declare
               Elt      : Node_Id := AIN.First_Node (Subcomponents (E));
               Elt_Type : Supported_Data_Representation :=
                 Get_Data_Representation (Corresponding_Instance (Elt));
               Elt_Size : Unsigned_Long_Long :=
                 Estimate_Data_Size (Corresponding_Instance (Elt));
               Result : Unsigned_Long_Long :=
                 Elementary_Type_Sizes (Data_Representation) + Elt_Size;
            begin
               loop
                  Elt := AIN.Next_Node (Elt);
                  exit when No (Elt);

                  Elt_Type :=
                    Get_Data_Representation (Corresponding_Instance (Elt));
                  Elt_Size :=
                    Estimate_Data_Size (Corresponding_Instance (Elt));
                  Result :=
                    Next_Multiple
                      (Elementary_Type_Alignments (Elt_Type),
                       Result);
                  Result := Result + Elt_Size;
               end loop;

               --  The type size must be integral multiple of the type
               --  alignment.

               return Next_Multiple
                   (Elementary_Type_Alignments (Data_Representation),
                    Result);
            end;

         when Data_Union =>
            raise Program_Error with "Union type size not implemented yet";

         when Data_Float | Data_With_Accessors | Data_None =>
            return 0;
      end case;
   end Estimate_Data_Size;

end Ocarina.Backends.PO_HI_Ada.Mapping;
