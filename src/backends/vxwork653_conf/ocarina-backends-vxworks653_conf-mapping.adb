------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                OCARINA.BACKENDS.VXWORKS653_CONF.MAPPING                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2015-2018 ESA & ISAE.                    --
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
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Properties;

package body Ocarina.Backends.Vxworks653_Conf.Mapping is

   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.XML_Tree.Nodes;
   use Ocarina.Backends.XML_Tree.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (XTN.K_HI_Node);
   begin
      pragma Assert
        (AINU.Is_Process (E)
         or else AINU.Is_System (E)
         or else AINU.Is_Processor (E));

      if AINU.Is_System (E) then
         Set_Str_To_Name_Buffer ("general");
      else
         Get_Name_String
           (To_XML_Name
              (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_vxworks653");
      end if;

      XTN.Set_Name (N, Name_Find);

      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list

      Append_Node_To_List (N, HI_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------

   function Map_HI_Unit (E : Node_Id) return Node_Id is
      U            : Node_Id;
      N            : Node_Id;
      P            : Node_Id;
      Root         : Node_Id;
      Core_OS_Node : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process (E)
         or else AINU.Is_Processor (E));

      U := New_Node (XTN.K_HI_Unit, AIN.Identifier (E));

      --  Packages that are common to all nodes
      Get_Name_String
        (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Add_Str_To_Name_Buffer ("_vxworks653-conf");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_XML_File (N);
      Set_Distributed_Application_Unit (P, U);
      XTN.Set_XML_File (U, P);

      Root := Make_XML_Node ("Module");

      XTU.Add_Attribute ("hmShutdownHyperstartIndex", "2", Root);
      XTU.Add_Attribute ("minimumWindowDurationInNs", "100000", Root);
      XTU.Add_Attribute ("hmShutdownRegistry", "platreg.bin", Root);
      XTU.Add_Attribute ("comment", "please insert comment", Root);

      XTU.Add_Attribute
        ("xsi:schemaLocation",
         "http://www.windriver.com/vxWorks653/ConfigRecord " &
         "../xml/cleanschena/Module.xsd",
         Root);

      XTU.Add_Attribute ("xmlns:xi", "http://www.w3.org/2001/XInclude", Root);

      XTU.Add_Attribute
        ("xmlns:xsi",
         "http://www.w3.org/2001/XMLSchema-instance",
         Root);

      XTU.Add_Attribute
        ("xmlns",
         "http://www.windriver.com/vxWorks653/ConfigRecord",
         Root);

      Core_OS_Node := Make_XML_Node ("CoreOS");
      Append_Node_To_List (Core_OS_Node, XTN.Subitems (Root));

      XTN.Set_Root_Node (P, Root);

      Append_Node_To_List (U, Units (Current_Entity));
      XTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   -------------------
   -- Map_Partition --
   -------------------

   function Map_Partition
     (Process              : Node_Id;
      Runtime              : Node_Id;
      Partition_Identifier : Integer;
      Nb_Threads           : Unsigned_Long_Long;
      Nb_Buffers           : Unsigned_Long_Long;
      Nb_Events            : Unsigned_Long_Long;
      Nb_Lock_Objects      : Unsigned_Long_Long;
      Nb_Blackboards       : Unsigned_Long_Long;
      Blackboards_Size     : Unsigned_Long_Long;
      Buffers_Size         : Unsigned_Long_Long) return Node_Id
   is
      pragma Unreferenced (Nb_Buffers);
      pragma Unreferenced (Nb_Events);
      pragma Unreferenced (Nb_Threads);
      pragma Unreferenced (Nb_Blackboards);
      pragma Unreferenced (Blackboards_Size);
      pragma Unreferenced (Nb_Lock_Objects);
      pragma Unreferenced (Buffers_Size);
      pragma Unreferenced (Process);
      Partition_Node             : Node_Id;
      Partition_Description_Node : Node_Id;
      Application_Node           : Node_Id;
      Shared_Library_Region_Node : Node_Id;
      Settings_Node              : Node_Id;
   begin
      Partition_Node             := Make_XML_Node ("Partition");
      Partition_Description_Node := Make_XML_Node ("PartitionDescription");
      Append_Node_To_List
        (Partition_Description_Node,
         XTN.Subitems (Partition_Node));

      XTU.Add_Attribute
        ("Name",
         Get_Name_String (Map_Partition_Name (Runtime)),
         Partition_Node);
      --
      --  Integer'Image adds a space in the beginning. To avoid that,
      --  see http://rosettacode.org/wiki/
      --  Strip_whitespace_from_a_string/Top_and_tail#Ada
      --
      XTU.Add_Attribute
        ("Id",
         Trim (Integer'Image (Partition_Identifier), Left),
         Partition_Node);

      --  Create the <Application/> sub-node.

      Application_Node := Make_XML_Node ("Application");
      XTU.Add_Attribute
        ("NameRef",
         Get_Name_String (Map_Application_Name (Runtime, True)),
         Application_Node);
      Append_Node_To_List
        (Application_Node,
         XTN.Subitems (Partition_Description_Node));

      --  Create the <SharedLibraryRegion/> sub-node.

      Shared_Library_Region_Node := Make_XML_Node ("SharedLibraryRegion");
      XTU.Add_Attribute ("NameRef", "vxSysLib", Shared_Library_Region_Node);
      Append_Node_To_List
        (Shared_Library_Region_Node,
         XTN.Subitems (Partition_Description_Node));

      --  Create the <Settings/> sub-node.

      Settings_Node := Make_XML_Node ("Settings");
      Append_Node_To_List
        (Settings_Node,
         XTN.Subitems (Partition_Description_Node));
      XTU.Add_Attribute ("RequiredMemorySize", "0x300000", Settings_Node);
      XTU.Add_Attribute
        ("PartitionHMTable",
         Get_Name_String (Map_Partition_Name (Runtime)) & "_hmtable",
         Settings_Node);
      XTU.Add_Attribute ("watchDogDuration", "0", Settings_Node);
      XTU.Add_Attribute ("allocDisable", "0", Settings_Node);
      XTU.Add_Attribute ("numWorkerTasks", "0", Settings_Node);
      XTU.Add_Attribute ("numStackGuardPages", "0xffffffff", Settings_Node);
      XTU.Add_Attribute ("isrStackSize", "0xffffffff", Settings_Node);
      XTU.Add_Attribute ("selSvrQSize", "0xffffffff", Settings_Node);
      XTU.Add_Attribute ("syscallPermissions", "0xffffffff", Settings_Node);
      XTU.Add_Attribute ("numFiles", "0xffffffff", Settings_Node);
      XTU.Add_Attribute ("numDrivers", "0xffffffff", Settings_Node);
      XTU.Add_Attribute ("numLogMsgs", "0xffffffff", Settings_Node);
      XTU.Add_Attribute ("maxGlobalFDs", "10", Settings_Node);
      XTU.Add_Attribute ("fpExcEnable", "1", Settings_Node);
      XTU.Add_Attribute
        ("maxEventQStallDuration",
         "INFINITE_TIME",
         Settings_Node);

      return Partition_Node;
   end Map_Partition;

   function Map_Partition_Name
     (Runtime         : Node_Id;
      Use_Source_Name : Boolean := True) return Name_Id
   is
      Result : Name_Id;
   begin
      Result := Get_Source_Name (Runtime);
      if Result /= No_Name and then Use_Source_Name then
         return Result;
      end if;
      Result := AIN.Name (Identifier (Parent_Subcomponent (Runtime)));
      return Result;
   end Map_Partition_Name;

   function Map_Application_Name
     (Runtime         : Node_Id;
      Use_Source_Name : Boolean := True) return Name_Id
   is
      Result : Name_Id;
   begin
      Result := Map_Partition_Name (Runtime, Use_Source_Name);
      Get_Name_String (Result);
      Add_Str_To_Name_Buffer ("_app");
      Result := Name_Find;
      return Result;
   end Map_Application_Name;

end Ocarina.Backends.Vxworks653_Conf.Mapping;
