------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.ARINC653_CONF.SYSTEM_HM                  --
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

with Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.ARINC653_Conf.Mapping;

package body Ocarina.Backends.ARINC653_Conf.System_HM is

   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.ARINC653_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);

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
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Bus =>
            Visit_Bus_Instance (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if AINU.Is_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
            end if;
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System_Instance;

   ------------------------
   -- Visit_Bus_Instance --
   ------------------------

   procedure Visit_Bus_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Bus_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S : Node_Id;
      U : Node_Id;
      P : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      P := XTN.Node (Backend_Node (Identifier (E)));

      Push_Entity (P);
      Push_Entity (U);

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Append_Node_To_List
        (Map_System_HM_Table (E),
         XTN.Subitems (Current_XML_Node));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Pop_Entity;
      Pop_Entity;
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Virtual_Processor_Instance;

   ------------------
   --  First_Pass  --
   ------------------

   package body First_Pass is

      procedure Visit_Component (E : Node_Id);
      procedure Visit_System (E : Node_Id);
      procedure Visit_Process (E : Node_Id);
      procedure Visit_Processor (E : Node_Id);
      procedure Visit_Virtual_Processor (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
            when K_Architecture_Instance =>
               Visit (Root_System (E));

            when K_Component_Instance =>
               Visit_Component (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------
      -- Visit_Component --
      ---------------------

      procedure Visit_Component (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System (E);

            when CC_Process =>
               Visit_Process (E);

            when CC_Device =>
               Visit_Process (E);

            when CC_Processor =>
               Visit_Processor (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor (E);

            when others =>
               null;
         end case;
      end Visit_Component;

      -------------------
      -- Visit_Process --
      -------------------

      procedure Visit_Process (E : Node_Id) is
         N              : Node_Id;
         Processes_List : List_Id;
      begin
         Processes_List :=
           XTN.Processes (Backend_Node (Identifier (Get_Bound_Processor (E))));

         N := XTU.Make_Container (E);

         XTU.Append_Node_To_List (N, Processes_List);
      end Visit_Process;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor (E : Node_Id) is
         Processes : List_Id;
         N         : Node_Id;
      begin
         N := New_Node (XTN.K_HI_Tree_Bindings);

         AIN.Set_Backend_Node (Identifier (E), N);

         Processes := XTU.New_List (XTN.K_List_Id);

         XTN.Set_Processes (N, Processes);

      end Visit_Virtual_Processor;

      ---------------------
      -- Visit_Processor --
      ---------------------

      procedure Visit_Processor (E : Node_Id) is
         S         : Node_Id;
         P         : Node_Id;
         U         : Node_Id;
         N         : Node_Id;
         Processes : List_Id;
      begin
         P := Map_HI_Node (E);
         Push_Entity (P);

         U := Map_HI_Unit (E);
         Push_Entity (U);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         N := New_Node (XTN.K_HI_Tree_Bindings);

         Processes := AINU.New_List (K_Node_Id, No_Location);

         XTN.Set_Processes (N, Processes);

         XTN.Set_Unit (N, U);
         XTN.Set_Node (N, P);

         AIN.Set_Backend_Node (Identifier (E), N);

         Pop_Entity;
         Pop_Entity;
      end Visit_Processor;

      ------------------
      -- Visit_System --
      ------------------

      procedure Visit_System (E : Node_Id) is
         S                  : Node_Id;
         Component_Instance : Node_Id;
      begin
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               Component_Instance := Corresponding_Instance (S);
               if Get_Category_Of_Component (Component_Instance) =
                 CC_Processor
               then
                  Visit_Processor (Component_Instance);
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
                  Visit_Process (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

      end Visit_System;
   end First_Pass;

end Ocarina.Backends.ARINC653_Conf.System_HM;
