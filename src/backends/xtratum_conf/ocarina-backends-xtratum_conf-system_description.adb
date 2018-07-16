------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            OCARINA.BACKENDS.XTRATUM_CONF.SYSTEM_DESCRIPTION              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2018 ESA & ISAE.                    --
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

with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Xtratum_Conf.Mapping;

package body Ocarina.Backends.Xtratum_Conf.System_Description is

   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Xtratum_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XV renames Ocarina.Backends.XML_Values;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

   Partition_Identifier : Unsigned_Long_Long := 0;

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
   begin
      Visit_Subcomponents_Of (E);
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
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
            --  Visit processor subcomponents

            if AINU.Is_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
            end if;
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
   end Visit_System_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      N         : Node_Id;
      Processes : List_Id;
   begin
      Visit_Subcomponents_Of (E);

      N := New_Node (XTN.K_HI_Tree_Bindings);

      Processes := AINU.New_List (K_Node_Id, No_Location);

      XTN.Set_Processes (N, Processes);

      AIN.Set_Backend_Node (Identifier (E), N);
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      Partition_Identifier_Node : Node_Id;
   begin
      Partition_Identifier_Node :=
        Make_Literal (XV.New_Numeric_Value (Partition_Identifier, 1, 10));

      AIN.Set_Backend_Node (Identifier (E), Partition_Identifier_Node);

      Visit_Subcomponents_Of (E);

      Partition_Identifier := Partition_Identifier + 1;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Xtratum_Conf.System_Description;
