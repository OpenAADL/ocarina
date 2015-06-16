------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.VXWORKS653_CONF.NAMING                  --
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

with Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Vxworks653_Conf.Mapping;
with Ocarina.Backends.XML_Values;
with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.Vxworks653_Conf.Naming is

   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Vxworks653_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;
   package XV  renames Ocarina.Backends.XML_Values;

   procedure Visit_Component (E : Node_Id);
   procedure Visit_System (E : Node_Id);
   procedure Visit_Process (E : Node_Id);
   procedure Visit_Processor (E : Node_Id);
   procedure Visit_Virtual_Processor (E : Node_Id);

   procedure Add_Applications
      (AADL_Processor : Node_Id; XML_Node : Node_Id);
   procedure Add_Shared_Data_Regions
      (AADL_Processor : Node_Id; XML_Node : Node_Id);
   procedure Add_Shared_Library_Regions
      (AADL_Processor : Node_Id; XML_Node : Node_Id);
   procedure Add_Application
      (AADL_Virtual_Processor : Node_Id;
       XML_Node : Node_Id);

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

      Add_Applications (E, XTN.Root_Node (XTN.XML_File (U)));
      Add_Shared_Data_Regions (E, XTN.Root_Node (XTN.XML_File (U)));
      Add_Shared_Library_Regions (E, XTN.Root_Node (XTN.XML_File (U)));

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

   -----------------------
   --  Add_Application  --
   -----------------------

   procedure Add_Application
      (AADL_Virtual_Processor : Node_Id;
       XML_Node : Node_Id) is
      Application_Node              : Node_Id;
      Application_Description_Node  : Node_Id;
      Memory_Size_Node              : Node_Id;
      Ports_Node                    : Node_Id;
      Port_Node                     : Node_Id;
      Corresponding_Process         : Node_Id;
      Feature                       : Node_Id;
      Size                          : Unsigned_Long_Long;
      Queue_Size                    : Long_Long;
      Refresh_Period                : Time_Type;
      Refresh_Period_Second         : Long_Double;
   begin
      --  Application Node that is the child of Applications

      Corresponding_Process := Find_Associated_Process
         (AADL_Virtual_Processor);

      Application_Node := Make_XML_Node ("Application");
      XTU.Add_Attribute ("Name",
                        Get_Name_String
                           (Map_Application_Name
                              (AADL_Virtual_Processor, True)),
                        Application_Node);
      Append_Node_To_List (Application_Node,
                           XTN.Subitems (XML_Node));

      --  Application Description with MemorySize and Ports nodes

      Application_Description_Node :=
         Make_XML_Node ("ApplicationDescription");
      Append_Node_To_List (Application_Description_Node,
                           XTN.Subitems (Application_Node));

      Memory_Size_Node := Make_XML_Node ("MemorySize");
      Append_Node_To_List (Memory_Size_Node,
                           XTN.Subitems (Application_Description_Node));
      XTU.Add_Attribute ("MemorySizeBss", "0x10000", Memory_Size_Node);
      XTU.Add_Attribute ("MemorySizeText", "0x10000", Memory_Size_Node);
      XTU.Add_Attribute ("MemorySizeData", "0x10000", Memory_Size_Node);
      XTU.Add_Attribute ("MemorySizeRoData", "0x10000", Memory_Size_Node);
      XTU.Add_Attribute ("MemorySizePersistentData",
                         "0x10000",
                         Memory_Size_Node);
      XTU.Add_Attribute ("MemorySizePersistentBss",
                         "0x10000", Memory_Size_Node);

      Ports_Node := Make_XML_Node ("Ports");
      Append_Node_To_List (Ports_Node,
                           XTN.Subitems (Application_Description_Node));

      Feature := First_Node (Features (Corresponding_Process));

      while Present (Feature) loop
         if Is_Event (Feature) and then Is_Data (Feature) then
            Size := To_Bytes (Get_Data_Size
                        (Corresponding_Instance (Feature)));
            Queue_Size := Get_Queue_Size (Feature);

            if Queue_Size = -1 then
               Queue_Size := 1;
            end if;

            Port_Node := Make_XML_Node ("QueuingPort");

            XTU.Add_Attribute ("MessageSize",
                              Trim (Unsigned_Long_Long'Image
                                 (Size), Left),
                              Port_Node);

            XTU.Add_Attribute ("Name",
                               Get_Name_String
                                 (C_Common.Mapping.Map_Port_Name (Feature)),
                               Port_Node);

            XTU.Add_Attribute ("QueueLength",
                               Trim (Long_Long'Image
                                 (Queue_Size), Left),
                              Port_Node);

            if not Is_In (Feature) and then
               Is_Out (Feature)
            then
               XTU.Add_Attribute ("Direction",
                                  "SOURCE",
                                  Port_Node);

               XTU.Add_Attribute ("Protocol",
                                  "SENDER_BLOCK",
                                  Port_Node);
            end if;

            if Is_In (Feature) and then
               not Is_Out (Feature)
            then
               XTU.Add_Attribute ("Direction",
                                  "DESTINATION",
                                  Port_Node);
               XTU.Add_Attribute ("Protocol",
                                  "NOT_APPLICABLE",
                                  Port_Node);
            end if;

            Append_Node_To_List (Port_Node,
                                 XTN.Subitems (Ports_Node));
         end if;

         if not Is_Event (Feature) and then Is_Data (Feature) then
            Size := To_Bytes (Get_Data_Size
                        (Corresponding_Instance (Feature)));

            Port_Node := Make_XML_Node ("SamplingPort");
            XTU.Add_Attribute ("MessageSize",
                              Trim (Unsigned_Long_Long'Image
                                 (Size), Left),
                              Port_Node);

            XTU.Add_Attribute ("Name",
                               Get_Name_String
                                 (C_Common.Mapping.Map_Port_Name (Feature)),
                               Port_Node);

            if Is_In (Feature) and then
               not Is_Out (Feature)
            then
               Refresh_Period := Get_POK_Refresh_Time (Feature);
               Refresh_Period_Second :=
                  (Long_Double (To_Milliseconds (Refresh_Period)) /
                   Long_Double (1000.0));

               XTU.Add_Attribute ("Direction",
                                  "DESTINATION",
                                  Port_Node);

               XTU.Add_Attribute ("RefreshRate",
                           XV.New_Floating_Point_Value (Refresh_Period_Second),
                           Port_Node);
            end if;

            if not Is_In (Feature) and then
               Is_Out (Feature)
            then
               XTU.Add_Attribute ("Direction",
                                  "SOURCE",
                                  Port_Node);

               XTU.Add_Attribute ("RefreshRate",
                                  "INFINITE_TIME",
                                  Port_Node);
            end if;

            Append_Node_To_List (Port_Node,
                                 XTN.Subitems (Ports_Node));
         end if;

         Feature := Next_Node (Feature);
      end loop;
   end Add_Application;

   ------------------------
   --  Add_Applications  --
   ------------------------

   procedure Add_Applications
      (AADL_Processor : Node_Id; XML_Node : Node_Id) is
      Applications_Node : Node_Id;
      S : Node_Id;
   begin

      --  Applications Node first

      Applications_Node := Make_XML_Node ("Applications");
      Append_Node_To_List (Applications_Node, XTN.Subitems (XML_Node));

      if not AINU.Is_Empty (Subcomponents (AADL_Processor)) then
         S := First_Node (Subcomponents (AADL_Processor));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            if AINU.Is_Virtual_Processor (Corresponding_Instance (S)) then
               Add_Application (Corresponding_Instance (S), Applications_Node);
            end if;
            S := Next_Node (S);
         end loop;
      end if;
   end Add_Applications;

   -------------------------------
   --  Add_Shared_Data_Regions  --
   -------------------------------

   procedure Add_Shared_Data_Regions
      (AADL_Processor : Node_Id; XML_Node : Node_Id) is
      pragma Unreferenced (AADL_Processor);

      Shared_Data_Node : Node_Id;
   begin
      Shared_Data_Node := Make_XML_Node ("SharedDataRegions");
      Append_Node_To_List (Shared_Data_Node, XTN.Subitems (XML_Node));
   end Add_Shared_Data_Regions;

   ----------------------------------
   --  Add_Shared_Library_Regions  --
   ----------------------------------

   procedure Add_Shared_Library_Regions
      (AADL_Processor : Node_Id; XML_Node : Node_Id) is
      pragma Unreferenced (AADL_Processor);

      Shared_Library_Node : Node_Id;
      Shared_Library_Regions_Node : Node_Id;
      Shared_Library_Description_Node : Node_Id;
      Shared_Memory_Size_Node : Node_Id;
   begin
      --  look like the following.
      --
      --  <SharedLibraryRegions>
      --    <SharedLibrary Name="vxSysLib">
      --      <SharedLibraryDescription ...>
      --        <MemorySize
      --           MemorySizeText="0x40000"
      --           MemorySizeRoData="0x10000"
      --           MemorySizeData="0x10000"
      --           MemorySizeBss="0x10000"
      --           MemorySizePersistentData="0x10000"
      --           MemorySizePersistentBss="0x10000"/>
      --      </SharedLibraryDescription>
      --    </SharedLibrary>
      --  </SharedLibraryRegions>

      --  First, add a central node for all shared library regions.

      Shared_Library_Regions_Node := Make_XML_Node ("SharedLibraryRegions");
      Append_Node_To_List (Shared_Library_Regions_Node,
                           XTN.Subitems (XML_Node));

      --  Take care of all sub shared libraries.

      Shared_Library_Node := Make_XML_Node ("SharedLibrary");
      XTU.Add_Attribute ("Name", "vxSysLib", Shared_Library_Node);
      Append_Node_To_List (Shared_Library_Node,
                           XTN.Subitems
                              (Shared_Library_Regions_Node));

      --  The SharedLibraryDescription node now.

      Shared_Library_Description_Node
         := Make_XML_Node ("SharedLibraryDescription");
      XTU.Add_Attribute ("SystemSharedLibrary",
                         "true",
                         Shared_Library_Description_Node);
      XTU.Add_Attribute ("VirtualAddress",
                         "0x50000000",
                         Shared_Library_Description_Node);
      Append_Node_To_List (Shared_Library_Description_Node,
                           XTN.Subitems (Shared_Library_Node));

      --  The MemorySize node now.

      Shared_Memory_Size_Node := Make_XML_Node ("MemorySize");
      XTU.Add_Attribute ("MemorySizeText",
                         "0x40000",
                         Shared_Memory_Size_Node);

      XTU.Add_Attribute ("MemorySizeRoData",
                         "0x10000",
                         Shared_Memory_Size_Node);

      XTU.Add_Attribute ("MemorySizeData",
                         "0x10000",
                         Shared_Memory_Size_Node);

      XTU.Add_Attribute ("MemorySizeBss",
                         "0x10000",
                         Shared_Memory_Size_Node);

      XTU.Add_Attribute ("MemorySizePersistentData",
                         "0x10000",
                         Shared_Memory_Size_Node);

      XTU.Add_Attribute ("MemorySizePersistentBss",
                         "0x10000",
                         Shared_Memory_Size_Node);

      Append_Node_To_List (Shared_Memory_Size_Node,
                           XTN.Subitems (Shared_Library_Description_Node));
   end Add_Shared_Library_Regions;

end Ocarina.Backends.Vxworks653_Conf.Naming;
