------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . P O _ H I _ A D A . M A I N       --
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
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.PO_HI_Ada.Mapping;
use Ocarina.Backends.PO_HI_Ada.Mapping;

with Ocarina.Backends.Messages;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_HI_Ada.Runtime;

package body Ocarina.Backends.PO_HI_Ada.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_HI_Ada.Runtime;
   use Ocarina.Backends.Messages;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   ---------------------
   -- Subprogram_Body --
   ---------------------

   package body Subprogram_Body is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);

      Has_Hybrid_Threads : Boolean := False;

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

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Entrypoint : constant Node_Id := Get_Thread_Initialize_Entrypoint (E);
         N          : Node_Id;

         function Get_Bus (Device : Node_Id) return Node_Id;

         function Get_Bus (Device : Node_Id) return Node_Id is
            The_System : Node_Id;
            S          : Node_Id;
         begin
            The_System :=
              Parent_Component
                (Parent_Subcomponent
                   (Corresponding_Instance (Parent_Subcomponent (Device))));

            pragma Assert (AINU.Is_System (The_System));

            if not AAU.Is_Empty (Connections (The_System)) then
               S := First_Node (Connections (The_System));

               --  Check whether a device is attached to this bus

               while Present (S) loop
                  if Kind (S) = K_Connection_Instance
                    and then Get_Category_Of_Connection (S) = CT_Access_Bus
                  then
                     if True
                        --  This device is connected to the bus

                       and then
                         Parent_Subcomponent (Device) =
                         Item (First_Node (Path (Destination (S))))
                     then
                        --  Note, for now, we assume there is only one
                        --  device at each end of the bus.

                        return Item (First_Node (Path (Source (S))));
                     end if;
                  end if;
                  S := Next_Node (S);
               end loop;
            end if;

            return No_Node;
         end Get_Bus;

      begin
         if Entrypoint /= No_Node then
            N :=
              Message_Comment
                ("Initialize device " &
                 Get_Name_String (Name (Identifier (E))));
            Append_Node_To_List (N, ADN.Statements (Current_Package));
            Add_With_Package
              (E    => RU (RU_PolyORB_HI_Generated_Naming),
               Used => True);

            N :=
              Make_Subprogram_Call
                (Map_Ada_Subprogram_Identifier (Entrypoint),
                 Make_List_Id (Map_Bus_Name (Get_Bus (E))));
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;
      end Visit_Device_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P             : constant Node_Id                  := ADN.Entity (U);
         N             : Node_Id;
         S             : Node_Id;
         Transport_API : constant Supported_Transport_APIs :=
           Fetch_Transport_API (E);
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         C : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Main_Body;

         --  Check that the process has indeed an execution platform

         if Get_Execution_Platform (Get_Bound_Processor (E)) =
           Platform_None
         then
            Display_Located_Error
              (Loc (Parent_Subcomponent (E)),
               "This process subcomponent is bound to a processor without" &
               " execution platform specification",
               Fatal => True);
         end if;

         --  Reset hybrid thread related global variables

         Has_Hybrid_Threads := False;

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         if Has_Hybrid_Threads then
            --  Unblock the hybrid task driver

            N :=
              Make_Subprogram_Call
                (RE (RE_Set_True),
                 Make_List_Id (RE (RE_Driver_Suspender)));
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;

         --  Declarative part

         N :=
           Make_Pragma_Statement
             (Pragma_Priority,
              Make_List_Id
                (Make_Attribute_Designator (RE (RE_Priority), A_Last)));
         Append_Node_To_List (N, ADN.Declarations (Current_Package));

         --  Statements

         --  Initialize default transport, if any

         if Transport_API /= Transport_None
           and then Transport_API /= Transport_User
         then
            N :=
              Message_Comment ("Initialize default communication subsystem");
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N := Make_Subprogram_Call (RE (RE_Initialize), No_List);
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;

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
                  Visit_Device_Instance (Corresponding_Instance (C));
               end if;
               C := Next_Node (C);
            end loop;
         end if;

         N := Message_Comment ("Unblock all user tasks");
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         N := Make_Subprogram_Call (RE (RE_Unblock_All_Tasks));
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Suspend forever the main task

         N :=
           Message_Comment
             ("Suspend forever instead of putting an" &
              " endless loop. This saves the CPU" &
              " resources.");
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         N := Make_Subprogram_Call (RE (RE_Suspend_Forever));
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (Ada_Root);

         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
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

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         P : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
      begin
         case P is
            when Thread_Periodic |
              Thread_Sporadic    |
              Thread_Hybrid      |
              Thread_Aperiodic   |
              Thread_Background  |
              Thread_ISR         =>
               Add_With_Package
                 (E            => RU (RU_PolyORB_HI_Generated_Activity, False),
                  Used         => False,
                  Warnings_Off => True,
                  Elaborated   => True);

               if P = Thread_Hybrid then
                  Has_Hybrid_Threads := True;
               end if;

               declare
                  Initialize_Entrypoint : constant Name_Id :=
                    Get_Thread_Initialize_Entrypoint (E);
                  N : Node_Id;
               begin
                  if Initialize_Entrypoint /= No_Name then
                     N :=
                       Message_Comment
                         ("Initialize thread " &
                          Get_Name_String
                            (Name
                               (Identifier
                                  (Corresponding_Instance
                                     (Parent_Subcomponent (E))))));

                     Append_Node_To_List (N, ADN.Statements (Current_Package));

                     N :=
                       Make_Subprogram_Call
                         (Map_Ada_Subprogram_Identifier
                            (Initialize_Entrypoint),
                          No_List);
                     Append_Node_To_List (N, ADN.Statements (Current_Package));
                  end if;
               end;

            when others =>
               raise Program_Error;
         end case;
      end Visit_Thread_Instance;

   end Subprogram_Body;

end Ocarina.Backends.PO_HI_Ada.Main;
