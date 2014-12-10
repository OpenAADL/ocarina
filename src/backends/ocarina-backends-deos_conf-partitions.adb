------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.DEOS_CONF.PARTITIONS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2014 ESA & ISAE.                       --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.ME_AADL.AADL_Instances.Debug;
use Ocarina.ME_AADL.AADL_Instances.Debug;
with Ocarina.Backends.Properties.ARINC653;
use Ocarina.Backends.Properties.ARINC653;

--  with Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Deos_Conf.Mapping;

package body Ocarina.Backends.Deos_Conf.Partitions is

--   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Deos_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   Root_Node : Node_Id := No_Node;
   Partitions_Node : Node_Id := No_Node;
   Memory_Regions : Node_Id := No_Node;
   Partition_Identifier : Integer := 1;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   function Find_Associated_Process (Runtime : Node_Id;
                                     Current_Node : Node_Id := Root_Node)
                                     return Node_Id;

   function Make_Default_Memory_Region return Node_Id;

   --------------------------------
   -- Make_Default_Memory_Region --
   --------------------------------

   function Make_Default_Memory_Region return Node_Id is
      N : Node_Id;
   begin
      N := Make_XML_Node ("MemoryRegion");

      XTU.Add_Attribute ("Name", "Initial RAM Pool", N);
      XTU.Add_Attribute ("Type", "Initial RAM Pool", N);
      XTU.Add_Attribute ("Address", "0x0", N);
      XTU.Add_Attribute ("Size", "0x19000", N);
      XTU.Add_Attribute ("AccessRights", "READ_WRITE", N);
      XTU.Add_Attribute ("PlatformMemoryPool", "0", N);

      return N;
   end Make_Default_Memory_Region;

   -----------------------------
   -- Find_Associated_Process --
   -----------------------------

   function Find_Associated_Process (Runtime : Node_Id;
                                     Current_Node : Node_Id := Root_Node)
                                     return Node_Id is
      T : Node_Id;
      S : Node_Id;
   begin
      if Get_Category_Of_Component (Current_Node) = CC_Process and then
         Get_Bound_Processor (Current_Node) = Runtime
      then
         return Current_Node;
      end if;

      if not AINU.Is_Empty (Subcomponents (Current_Node)) then
         S := First_Node (Subcomponents (Current_Node));
         while Present (S) loop
            T := Find_Associated_Process
               (Runtime, Corresponding_Instance (S));

            if T /= No_Node then
               return T;
            end if;

            S := Next_Node (S);
         end loop;
      end if;

      return No_Node;
   end Find_Associated_Process;

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

      Partition_Identifier := 1;

      declare
         Module_Schedule : constant Schedule_Window_Record_Term_Array
           := Get_Module_Schedule_Property (E);
      begin
         for J in Module_Schedule'Range loop
            Put_Line ("Module Schedule slot #" & J'Img);
            Put_Line (" -> "
                        & Get_Name_String (Module_Schedule (J).Partition));
            Put_Line (" -> "
                        & Module_Schedule (J).Duration.T'Img
                        & " " & Module_Schedule (J).Duration.U'Img);
            Put_Line (" -> "
                        & Module_Schedule (J).Periodic_Processing_Start'Img);
         end loop;
      end;

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Partitions_Node := Make_XML_Node ("Partitions");

      Append_Node_To_List
        (Partitions_Node,
         XTN.Subitems (Current_XML_Node));

      --
      --  First, make the <Partition/> nodes
      --

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

      Pop_Entity;
      Pop_Entity;
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S : Node_Id;
      Corresponding_Process : Node_Id := No_Node;
      Partition_Node : Node_Id;
   begin
      Corresponding_Process := Find_Associated_Process (E);

      if Corresponding_Process /= No_Node then
         --
         --  First, we create the description of the partition.
         --
         Partition_Node := Map_Partition (Corresponding_Process,
                            E,
                            Partition_Identifier);
         Append_Node_To_List
            (Partition_Node,
             XTN.Subitems (Partitions_Node));

         --
         --  Then, we associate the partition with memory region
         --

         Memory_Regions := Make_XML_Node ("MemoryRegions");

         --
         --  FIXME: for now, we associate with the default
         --  memory. Has to work to get the AADL memory component.
         --
         Append_Node_To_List
            (Make_Default_Memory_Region,
            XTN.Subitems (Memory_Regions));

         Append_Node_To_List
            (Memory_Regions,
            XTN.Subitems (Partition_Node));
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Deos_Conf.Partitions;
