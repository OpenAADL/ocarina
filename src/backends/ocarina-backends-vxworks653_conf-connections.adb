------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.BACKENDS.VXWORKS653_CONF.CONNECTIONS                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2015 ESA & ISAE.                       --
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ocarina.Backends.Utils;
with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Vxworks653_Conf.Mapping;

package body Ocarina.Backends.Vxworks653_Conf.Connections is

--   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.Backends.Utils;

   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;

   use Ocarina.Backends.Vxworks653_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   Root_Node : Node_Id := No_Node;
   Connections_Node : Node_Id := No_Node;
   Channel_Identifier : Unsigned_Long_Long := 0;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
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
      Root_Node := Root_System (E);
      Visit (Root_Node);
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

         when CC_Bus =>
            Visit_Bus_Instance (E);

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
      U                 : Node_Id;
      P                 : Node_Id;
      S                 : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      P := XTN.Node (Backend_Node (Identifier (E)));

      Push_Entity (P);
      Push_Entity (U);

      Channel_Identifier := 0;

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Connections_Node := Make_XML_Node ("Connections");

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if AINU.Is_Virtual_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      Append_Node_To_List
        (Connections_Node,
         XTN.Subitems (Current_XML_Node));

      Pop_Entity;
      Pop_Entity;
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      Corresponding_Process : Node_Id;
      Channel_Node          : Node_Id;
      Source_Node           : Node_Id;
      Destination_Node      : Node_Id;
      Feature               : Node_Id;
      Port_Source           : Node_Id;
      Port_Destination      : Node_Id;
      Partition_Destination : Node_Id;
   begin
      Corresponding_Process := Find_Associated_Process (E);

      Feature := First_Node (Features (Corresponding_Process));

      while Present (Feature) loop
         if Is_Data (Feature) and then Is_Out (Feature) then
            Port_Source := Feature;
            Port_Destination := Item (First_Node (Destinations (Feature)));
            Partition_Destination := Parent_Component (Port_Destination);

            Channel_Node := Make_XML_Node ("Channel");
            Add_Attribute ("Id",
                        Trim (Unsigned_Long_Long'Image
                           (Channel_Identifier), Left),
                         Channel_Node);

            Source_Node := Make_XML_Node ("Source");
            Add_Attribute ("PartitionNameRef",
                           Get_Name_String
                              (Map_Partition_Name (E)),
                           Source_Node);

            Add_Attribute ("PortNameRef",
                           Get_Name_String
                              (C_Common.Mapping.Map_Port_Name
                                 (Port_Source)),
                           Source_Node);

            Append_Node_To_List (Source_Node,
                                 XTN.Subitems (Channel_Node));

            Destination_Node := Make_XML_Node ("Destination");
            Add_Attribute ("PortNameRef",
                           Get_Name_String
                              (C_Common.Mapping.Map_Port_Name
                                 (Port_Destination)),
                           Destination_Node);

            Add_Attribute ("PartitionNameRef",
                           Get_Name_String
                              (Map_Partition_Name
                                 (Get_Partition_Runtime
                                          (Partition_Destination))),
                         Destination_Node);

            Append_Node_To_List (Destination_Node,
                                 XTN.Subitems (Channel_Node));
            Append_Node_To_List (Channel_Node,
                                 XTN.Subitems (Connections_Node));

            Channel_Identifier := Channel_Identifier + 1;
         end if;
         Feature := Next_Node (Feature);
      end loop;

   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Vxworks653_Conf.Connections;
