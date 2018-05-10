------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . D E O S _ C O N F . S C H E D U L E   --
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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ocarina.Namet;     use Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Instances.Queries;

with Ocarina.Backends.Properties.ARINC653;

--  with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
--  with Ocarina.Backends.Deos_Conf.Mapping;

package body Ocarina.Backends.Deos_Conf.Schedule is

--   use Locations;
   use Ocarina.ME_AADL;

   use Ocarina.Instances.Queries;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties.ARINC653;
--   use Ocarina.Backends.Properties;
--   use Ocarina.Backends.Deos_Conf.Mapping;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   Root_Node     : Node_Id := No_Node;
   Schedule_Node : Node_Id := No_Node;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   procedure Fill_Scheduling_Slots (Processor : Node_Id);
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

   ---------------------------
   -- Fill_Scheduling_Slots --
   ---------------------------

   procedure Fill_Scheduling_Slots (Processor : Node_Id) is
      Time_Window_Node : Node_Id;
      Module_Schedule  : constant Schedule_Window_Record_Term_Array :=
        Get_Module_Schedule_Property (Processor);
      Offset        : Unsigned_Long_Long;
      Slot_Duration : Unsigned_Long_Long;
   begin
      Offset := 0;
      for J in Module_Schedule'Range loop
         Time_Window_Node := Make_XML_Node ("PartitionTimeWindow");

         Append_Node_To_List (Time_Window_Node, XTN.Subitems (Schedule_Node));

--            XTU.Add_Attribute ("Duration", "6000000", Time_Window_Node);
         --
         --  For now, we assume the partition duration
         --  is in milliseconds.
         --
         Slot_Duration := To_Nanoseconds (Module_Schedule (J).Duration);
         XTU.Add_Attribute
           ("Duration",
            Trim (Unsigned_Long_Long'Image (Slot_Duration), Left),
            Time_Window_Node);
         XTU.Add_Attribute
           ("Offset",
            Trim (Unsigned_Long_Long'Image (Offset), Left),
            Time_Window_Node);
         XTU.Add_Attribute
           ("PeriodicProcessingStart",
            "true",
            Time_Window_Node);
         XTU.Add_Attribute
           ("RepeatWindowAtNanosecondInterval",
            "PartitionPeriod",
            Time_Window_Node);
         XTU.Add_Attribute
           ("InhibitEarlyCompletion",
            "false",
            Time_Window_Node);
         XTU.Add_Attribute
           ("PartitionNameRef",
            Get_Name_String
              (ATN.Display_Name
                 (ATN.Identifier (Module_Schedule (J).Partition))),
            Time_Window_Node);
         Offset := Offset + Slot_Duration;
      end loop;
   end Fill_Scheduling_Slots;

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

      Schedule_Node := Make_XML_Node ("Schedule");

      XTU.Add_Attribute ("MajorFrameLength", "Automatic", Schedule_Node);

      Append_Node_To_List (Schedule_Node, XTN.Subitems (Current_XML_Node));

      if Is_Defined_Property (E, "arinc653::module_schedule") then
         Fill_Scheduling_Slots (E);
      end if;

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
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Deos_Conf.Schedule;
