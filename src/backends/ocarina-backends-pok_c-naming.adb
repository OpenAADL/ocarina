------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . B A C K E N D S . P O K _ C . N A M I N G         --
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

with Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Tree.Nutils;

package body Ocarina.Backends.POK_C.Naming is

   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

      procedure Visit_First_Pass_Component (E : Node_Id);
      procedure Visit_First_Pass_System (E : Node_Id);
      procedure Visit_First_Pass_Process_Or_Device (E : Node_Id);
      procedure Visit_First_Pass_Processor (E : Node_Id);
      procedure Visit_First_Pass_Bus (E : Node_Id);
      procedure Visit_First_Pass_Virtual_Processor (E : Node_Id);

      procedure Visit_Second_Pass_Component (E : Node_Id);
      procedure Visit_Second_Pass_System (E : Node_Id);
      procedure Visit_Second_Pass_Process_Or_Device (E : Node_Id);
      procedure Visit_Second_Pass_Processor (E : Node_Id);
      procedure Visit_Second_Pass_Virtual_Processor (E : Node_Id);

      ----------------------
      -- Visit_First_Pass --
      ----------------------

      procedure Visit_First_Pass (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_First_Pass (Root_System (E));

            when K_Component_Instance =>
               Visit_First_Pass_Component (E);

            when others =>
               null;
         end case;
      end Visit_First_Pass;

      --------------------------------
      -- Visit_First_Pass_Component --
      --------------------------------

      procedure Visit_First_Pass_Component (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_First_Pass_System (E);

            when CC_Process =>
               Visit_First_Pass_Process_Or_Device (E);

            when CC_Device =>
               Visit_First_Pass_Process_Or_Device (E);

            when CC_Processor =>
               Visit_First_Pass_Processor (E);

            when CC_Bus =>
               Visit_First_Pass_Bus (E);

            when CC_Virtual_Processor =>
               Visit_First_Pass_Virtual_Processor (E);

            when others =>
               null;
         end case;
      end Visit_First_Pass_Component;

      ----------------------------------------
      -- Visit_First_Pass_Process_or_Device --
      ----------------------------------------

      procedure Visit_First_Pass_Process_Or_Device (E : Node_Id) is
         N              : Node_Id;
         Processes_List : List_Id;

         Proc : constant Node_Id := Get_Bound_Processor (E);
      begin
         if No (Proc) then
            Display_Located_Error
              (Loc (Parent_Subcomponent (E)),
               "This device or process is not bound to a processor",
               Fatal => True, Warning => False);
         end if;

         Processes_List :=
           CTN.Processes (Backend_Node (Identifier (Proc)));

         N := AINU.Make_Node_Container (E);

         AINU.Append_Node_To_List (N, Processes_List);
      end Visit_First_Pass_Process_Or_Device;

      --------------------------
      -- Visit_First_Pass_Bus --
      --------------------------

      procedure Visit_First_Pass_Bus (E : Node_Id) is
         N             : Node_Id;
         Accessed_List : List_Id;
      begin
         N := New_Node (CTN.K_HI_Tree_Bindings);

         AIN.Set_Backend_Node (Identifier (Parent_Subcomponent (E)), N);

         Accessed_List := AINU.New_List (AIN.K_List_Id, No_Location);

         CTN.Set_Processes (N, Accessed_List);
      end Visit_First_Pass_Bus;

      -------------------------------------------------
      -- Visit_First_Pass_Virtual_Processor_Instance --
      -------------------------------------------------

      procedure Visit_First_Pass_Virtual_Processor (E : Node_Id) is
         Processes : List_Id;
         N         : Node_Id;
      begin
         N := New_Node (CTN.K_HI_Tree_Bindings);

         AIN.Set_Backend_Node (Identifier (E), N);

         Processes := AINU.New_List (AIN.K_List_Id, No_Location);

         CTN.Set_Processes (N, Processes);

      end Visit_First_Pass_Virtual_Processor;

      --------------------------------
      -- Visit_First_Pass_Processor --
      --------------------------------

      procedure Visit_First_Pass_Processor (E : Node_Id) is
         A         : constant Node_Id := Map_Distributed_Application (E);
         S         : Node_Id;
         P         : Node_Id;
         U         : Node_Id;
         N         : Node_Id;
         Processes : List_Id;
      begin
         Push_Entity (C_Root);
         Push_Entity (A);

         P := Map_HI_Node (E, Kernel => True);
         Push_Entity (P);

         U := Map_HI_Unit (E);
         Push_Entity (U);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit_First_Pass (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         N := New_Node (CTN.K_HI_Tree_Bindings);

         Processes := AINU.New_List (K_List_Id, No_Location);

         CTN.Set_Processes (N, Processes);

         CTN.Set_Deployment_Node (N, A);
         CTN.Set_Naming_Node (N, U);

         AIN.Set_Backend_Node (Identifier (E), N);

         Append_Node_To_List (A, CTN.HI_Nodes (C_Root));

         Pop_Entity;
         Pop_Entity;
         Pop_Entity; --  A
      end Visit_First_Pass_Processor;

      -----------------------------
      -- Visit_First_Pass_System --
      -----------------------------

      procedure Visit_First_Pass_System (E : Node_Id) is
         S                  : Node_Id;
         Component_Instance : Node_Id;
      begin
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               Component_Instance := Corresponding_Instance (S);
               if AINU.Is_Processor (Component_Instance) then
                  Visit_First_Pass_Processor (Component_Instance);
               elsif AINU.Is_Bus (Component_Instance) then
                  Visit_First_Pass_Bus (Component_Instance);
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.
               if AINU.Is_Process_Or_Device (Corresponding_Instance (S)) then
                  Visit_First_Pass (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

      end Visit_First_Pass_System;

      ----------------------
      -- Visit_Second_Pass --
      ----------------------

      procedure Visit_Second_Pass (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit_Second_Pass (Root_System (E));

            when K_Component_Instance =>
               Visit_Second_Pass_Component (E);

            when others =>
               null;
         end case;
      end Visit_Second_Pass;

      --------------------------------
      -- Visit_Second_Pass_Component --
      --------------------------------

      procedure Visit_Second_Pass_Component (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_Second_Pass_System (E);

            when CC_Process =>
               Visit_Second_Pass_Process_Or_Device (E);

            when CC_Device =>
               Visit_Second_Pass_Process_Or_Device (E);

            when CC_Processor =>
               Visit_Second_Pass_Processor (E);

            when CC_Virtual_Processor =>
               Visit_Second_Pass_Virtual_Processor (E);

            when others =>
               null;
         end case;
      end Visit_Second_Pass_Component;

      -------------------------------
      -- Visit_Second_Pass_Process --
      -------------------------------

      procedure Visit_Second_Pass_Process_Or_Device (E : Node_Id) is
         P : constant Node_Id := Map_HI_Node (E);
         U : Node_Id;
      begin
         Push_Entity (P);
         U := Map_HI_Unit (E);
         Push_Entity (U);

         Set_Naming_Header (U);

         Pop_Entity;
         Pop_Entity;
      end Visit_Second_Pass_Process_Or_Device;

      --------------------------------
      -- Visit_Second_Pass_Processor --
      --------------------------------

      procedure Visit_Second_Pass_Processor (E : Node_Id) is
         S : Node_Id;
         A : constant Node_Id :=
           CTN.Deployment_Node (Backend_Node (Identifier (E)));
      begin
         Push_Entity (C_Root);
         Push_Entity (A);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit_Second_Pass (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity;
         Pop_Entity;
      end Visit_Second_Pass_Processor;

      -----------------------------------------
      -- Visit_Second_Pass_Virtual_Processor --
      -----------------------------------------

      procedure Visit_Second_Pass_Virtual_Processor (E : Node_Id) is
         Processes : List_Id;
         S         : Node_Id;
      begin
         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            S         := AIN.First_Node (Processes);
            while Present (S) loop
               Visit_Second_Pass (AIN.Item (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_Second_Pass_Virtual_Processor;

      -----------------------------
      -- Visit_Second_Pass_System --
      -----------------------------

      procedure Visit_Second_Pass_System (E : Node_Id) is
         Component_Instance : Node_Id;
         S                  : Node_Id;
      begin
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               Component_Instance := Corresponding_Instance (S);
               if AINU.Is_Processor (Component_Instance) then
                  Visit_Second_Pass_Processor (Component_Instance);
               end if;
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_Second_Pass_System;
   end Header_File;
end Ocarina.Backends.POK_C.Naming;
