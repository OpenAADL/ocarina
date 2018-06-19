------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B A C K E N D S . D E O S _ C O N F . M A P P I N G    --
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

with Utils; use Utils;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Common;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

package body Ocarina.Backends.Deos_Conf.Mapping is

   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Utils;
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
         Add_Str_To_Name_Buffer ("_deos");
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
      U    : Node_Id;
      N    : Node_Id;
      P    : Node_Id;
      Q    : Node_Id;
      R    : Node_Id;
      Root : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process (E)
         or else AINU.Is_Processor (E));

      U := New_Node (XTN.K_HI_Unit, AIN.Identifier (E));

      --  Packages that are common to all nodes
      Get_Name_String
        (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Add_Str_To_Name_Buffer ("_deos-conf");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_XML_File (N);
      Set_Distributed_Application_Unit (P, U);
      XTN.Set_XML_File (U, P);

      Root := Make_XML_Node ("Deos653Config");

      XTU.Add_Attribute ("validityKey", "0000", Root);
      XTU.Add_Attribute ("toolVersion", "1.10.2", Root);
      XTU.Add_Attribute ("hmShutdownHyperstartIndex", "2", Root);
      XTU.Add_Attribute ("minimumWindowDurationInNs", "100000", Root);
      XTU.Add_Attribute ("hmShutdownRegistry", "platreg.bin", Root);
      XTU.Add_Attribute ("xsi:schemaLocation", "deos653.xsd", Root);
      XTU.Add_Attribute ("comment", "please insert comment", Root);
      XTU.Add_Attribute
        ("xmlns:xsi",
         "http://www.w3.org/2001/XMLSchema-instance",
         Root);
      XTU.Add_Attribute ("xmlns", "http://ddci.com/ARINC653", Root);

      Set_Str_To_Name_Buffer ("name");
      R := Make_Defining_Identifier (Name_Find);
      Get_Name_String (To_XML_Name (Display_Name (Identifier (E))));
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (Root));

      XTN.Set_Root_Node (P, Root);

      Append_Node_To_List (U, Units (Current_Entity));
      XTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   -----------------------
   -- Map_Sampling_Port --
   -----------------------

   function Map_Sampling_Port (Port : Node_Id) return Node_Id is
      Sampling_Port  : Node_Id;
      Size           : Unsigned_Long_Long;
      Source_Port    : Node_Id;
      Source_Process : Node_Id;
      Source_Runtime : Node_Id;
   begin
      Sampling_Port := Make_XML_Node ("SamplingPort");
      Size := To_Bytes (Get_Data_Size (Corresponding_Instance (Port)));

      XTU.Add_Attribute
        ("Name",
         Get_Name_String (Map_Port_Name (Port)),
         Sampling_Port);
      XTU.Add_Attribute
        ("MaxMessageSize",
         Trim (Unsigned_Long_Long'Image (Size), Left),
         Sampling_Port);

      if Is_In (Port) then
         XTU.Add_Attribute ("Direction", "DESTINATION", Sampling_Port);
      elsif Is_Out (Port) then
         XTU.Add_Attribute ("Direction", "SOURCE", Sampling_Port);
      end if;

      if Is_In (Port) then
         Source_Port    := Item (AIN.First_Node (Sources (Port)));
         Source_Process := Parent_Component (Source_Port);
         Source_Runtime :=
           Parent_Subcomponent (Get_Partition_Runtime (Source_Process));
         XTU.Add_Attribute
           ("SourcePortName",
            Get_Name_String (Map_Port_Name (Source_Port)),
            Sampling_Port);
         XTU.Add_Attribute
           ("SourcePartitionName",
            Get_Name_String (Display_Name (Identifier (Source_Runtime))),
            Sampling_Port);
      else
         XTU.Add_Attribute ("SourcePartitionName", "", Sampling_Port);
         XTU.Add_Attribute ("SourcePortName", "", Sampling_Port);
      end if;

      XTU.Add_Attribute ("CustomIOFunction", "", Sampling_Port);
      XTU.Add_Attribute ("AccessRateInNanoseconds", "12500000", Sampling_Port);
      return Sampling_Port;
   end Map_Sampling_Port;

   ----------------------
   -- Map_Queuing_Port --
   ----------------------

   function Map_Queuing_Port (Port : Node_Id) return Node_Id is
      Queuing_Port   : Node_Id;
      Size           : Unsigned_Long_Long;
      Queue_Size     : Long_Long;
      Source_Port    : Node_Id;
      Source_Process : Node_Id;
      Source_Runtime : Node_Id;
   begin
      Queuing_Port := Make_XML_Node ("QueuingPort");
      Size         := To_Bytes (Get_Data_Size (Corresponding_Instance (Port)));
      Queue_Size   := Get_Queue_Size (Port);

      if Queue_Size = -1 then
         Queue_Size := 1;
      end if;

      XTU.Add_Attribute
        ("Name",
         Get_Name_String (Map_Port_Name (Port)),
         Queuing_Port);
      XTU.Add_Attribute
        ("MaxMessageSize",
         Trim (Unsigned_Long_Long'Image (Size), Left),
         Queuing_Port);

      XTU.Add_Attribute
        ("MaxNbMessage",
         Trim (Long_Long'Image (Queue_Size), Left),
         Queuing_Port);

      if Is_In (Port) then
         XTU.Add_Attribute ("Direction", "DESTINATION", Queuing_Port);
      elsif Is_Out (Port) then
         XTU.Add_Attribute ("Direction", "SOURCE", Queuing_Port);
      end if;

      if Is_In (Port) then
         Source_Port    := Item (AIN.First_Node (Sources (Port)));
         Source_Process := Parent_Component (Source_Port);
         Source_Runtime :=
           Parent_Subcomponent (Get_Partition_Runtime (Source_Process));
         XTU.Add_Attribute
           ("SourcePortName",
            Get_Name_String (Map_Port_Name (Source_Port)),
            Queuing_Port);
         XTU.Add_Attribute
           ("SourcePartitionName",
            Get_Name_String (Display_Name (Identifier (Source_Runtime))),
            Queuing_Port);
      else
         XTU.Add_Attribute ("SourcePartitionName", "", Queuing_Port);
         XTU.Add_Attribute ("SourcePortName", "", Queuing_Port);
      end if;

      XTU.Add_Attribute ("CustomIOFunction", "", Queuing_Port);
      return Queuing_Port;
   end Map_Queuing_Port;

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
      pragma Unreferenced (Nb_Lock_Objects);
      pragma Unreferenced (Buffers_Size);
      pragma Unreferenced (Process);
      Partition_Node     : Node_Id;
      Partition_Period   : Time_Type;
      Partition_Duration : Time_Type;
      Period_Ns          : Unsigned_Long_Long;
      Duration_Ns        : Unsigned_Long_Long;
   begin
      Partition_Period   := Get_Period (Runtime);
      Partition_Duration := Get_Execution_Time (Runtime);

      if Partition_Period = Null_Time then
         Period_Ns := 0;
      else
         Period_Ns := To_Nanoseconds (Partition_Period);
      end if;

      if Partition_Duration = Null_Time then
         Duration_Ns := 0;
      else
         Duration_Ns := To_Nanoseconds (Partition_Duration);
      end if;

      Partition_Node := Make_XML_Node ("Partition");

      XTU.Add_Attribute
        ("Name",
         Get_Name_String
           (AIN.Name (Identifier (Parent_Subcomponent (Runtime)))),
         Partition_Node);
      --
      --  Integer'Image adds a space in the beginning. To avoid that,
      --  see http://rosettacode.org/wiki/
      --  Strip_whitespace_from_a_string/Top_and_tail#Ada
      --
      XTU.Add_Attribute
        ("Identifier",
         Trim (Integer'Image (Partition_Identifier), Left),
         Partition_Node);

      XTU.Add_Attribute
        ("Period",
         Trim (Unsigned_Long_Long'Image (Period_Ns), Left),
         Partition_Node);

      XTU.Add_Attribute
        ("Duration",
         Trim (Unsigned_Long_Long'Image (Duration_Ns), Left),
         Partition_Node);

      if Get_Source_Name (Runtime) = No_Name then
         XTU.Add_Attribute
           ("ExecutableImageName",
            Get_Name_String
              (AIN.Name (Identifier (Parent_Subcomponent (Runtime)))) &
            ".exe",
            Partition_Node);
      else
         XTU.Add_Attribute
           ("ExecutableImageName",
            Get_Name_String (Get_Source_Name (Runtime)),
            Partition_Node);
      end if;
      XTU.Add_Attribute ("MainProcessStackSizeInPages", "1", Partition_Node);
      XTU.Add_Attribute ("BreakAtStartup", "no", Partition_Node);
      XTU.Add_Attribute ("InDebugSet", "no", Partition_Node);
      XTU.Add_Attribute ("MapConfigurationFileTo", "RAM", Partition_Node);
      XTU.Add_Attribute ("ExecuteFrom", "RAM", Partition_Node);
      XTU.Add_Attribute ("PartitionUsesFPU", "no", Partition_Node);
      XTU.Add_Attribute ("ProcessStackSpaceInPages", "6", Partition_Node);
      XTU.Add_Attribute
        ("MinimumProcessStackSizeInBytes",
         "512",
         Partition_Node);
      XTU.Add_Attribute
        ("ProcessQuota",
         Trim (Unsigned_Long_Long'Image (Nb_Threads + 2), Left),
         Partition_Node);
      XTU.Add_Attribute
        ("BlackboardQuota",
         Trim (Unsigned_Long_Long'Image (Nb_Blackboards), Left),
         Partition_Node);

      XTU.Add_Attribute
        ("BlackboardMessageSpaceInBytes",
         Trim (Unsigned_Long_Long'Image (Blackboards_Size), Left),
         Partition_Node);
      XTU.Add_Attribute ("BufferQuota", "0", Partition_Node);
      XTU.Add_Attribute ("BufferMessageSpaceInBytes", "0", Partition_Node);
      XTU.Add_Attribute ("SemaphoreQuota", "0", Partition_Node);
      XTU.Add_Attribute ("EventQuota", "1", Partition_Node);
      XTU.Add_Attribute ("MaximumPartitionLockLevel", "16", Partition_Node);
      XTU.Add_Attribute ("MinimumProcessPriority", "1", Partition_Node);
      XTU.Add_Attribute ("MaximumProcessPriority", "239", Partition_Node);
      XTU.Add_Attribute ("LoggingFunction", "", Partition_Node);
      XTU.Add_Attribute ("DeosKernelAttributeAccess", "no", Partition_Node);
      XTU.Add_Attribute ("ProcessStackGapSizeInDwords", "0", Partition_Node);
      XTU.Add_Attribute
        ("ProcessStackTagIntervalInDwords",
         "0",
         Partition_Node);
      XTU.Add_Attribute
        ("SourcePortSharedMemoryType",
         "DeosSharedMemory",
         Partition_Node);
      XTU.Add_Attribute
        ("PlatformResourcePhysicalAddress",
         "0x0",
         Partition_Node);
      XTU.Add_Attribute ("PlatformResourceSizeInPages", "0", Partition_Node);
      XTU.Add_Attribute ("PlatformResourceCachePolicy", "off", Partition_Node);
      XTU.Add_Attribute ("HealthMonitorEventLogSize", "30", Partition_Node);
      XTU.Add_Attribute ("EventLoggingEnabled", "yes", Partition_Node);
      return Partition_Node;
   end Map_Partition;

   -------------------
   -- Map_Port_Name --
   -------------------

   function Map_Port_Name (E : Node_Id) return Name_Id is
      N : Name_Id;
   begin
      Get_Name_String (Display_Name (Identifier (E)));

      N := Name_Find;
      return (To_Lower (N));
   end Map_Port_Name;

end Ocarina.Backends.Deos_Conf.Mapping;
