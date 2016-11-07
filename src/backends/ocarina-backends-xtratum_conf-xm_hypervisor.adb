------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--               OCARINA.BACKENDS.XTRATUM_CONF.XM_HYPERVISOR                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2011-2016 ESA & ISAE.                    --
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
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

package body Ocarina.Backends.Xtratum_Conf.Xm_Hypervisor is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.XML_Tree.Nutils;

   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

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

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      U : Node_Id;
      R : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      R := XTN.Node (Backend_Node (Identifier (E)));

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Push_Entity (U);
      Push_Entity (R);

      Visit_Subcomponents_Of (E);

      Pop_Entity;
      Pop_Entity;
   end Visit_System_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      Xm_Hypervisor_Node : Node_Id;
      Memory_Area_Node   : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      --  Create the main XMHypervisor node.

      Xm_Hypervisor_Node := Make_XML_Node ("XMHypervisor");

      Set_Str_To_Name_Buffer ("console");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("Uart");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List
        (Make_Assignement (P, Q),
         XTN.Items (Xm_Hypervisor_Node));

      --  Create the PhysicalMemoryArea node associated
      --  to the XMHypervisor node.
      Memory_Area_Node := Make_XML_Node ("PhysicalMemoryArea");

      Append_Node_To_List
        (Memory_Area_Node,
         XTN.Subitems (Xm_Hypervisor_Node));

      Set_Str_To_Name_Buffer ("size");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("512KB");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List
        (Make_Assignement (P, Q),
         XTN.Items (Memory_Area_Node));

      Append_Node_To_List
        (Xm_Hypervisor_Node,
         XTN.Subitems (Current_XML_Node));

      Visit_Subcomponents_Of (E);
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
   begin
      Visit_Subcomponents_Of (E);
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Xtratum_Conf.Xm_Hypervisor;
