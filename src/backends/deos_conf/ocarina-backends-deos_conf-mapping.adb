------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--   O C A R I N A . B A C K E N D S . D E O S _ C O N F . M A P P I N G    --
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

with Utils; use Utils;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.XML_Common.Mapping;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

package body Ocarina.Backends.Deos_Conf.Mapping is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.XML_Common.Mapping;
   use Ocarina.Backends.XML_Tree.Nodes;
   use Ocarina.Backends.XML_Tree.Nutils;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XV renames Ocarina.Backends.XML_Values;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   procedure Map_Scheduler (E : Node_Id; N : Node_Id) is
      Scheduler : Supported_POK_Scheduler;
      R         : Node_Id;
      Q         : Node_Id;
   begin
      Scheduler := Get_POK_Scheduler (E);

      Set_Str_To_Name_Buffer ("scheduler");
      R := Make_Defining_Identifier (Name_Find);

      if Scheduler = RMS then
         Set_Str_To_Name_Buffer ("rms");
      elsif Scheduler = EDF then
         Set_Str_To_Name_Buffer ("edf");
      elsif Scheduler = Static then
         Set_Str_To_Name_Buffer ("static");
      else
         Set_Str_To_Name_Buffer ("unknown");
      end if;

      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));
   end Map_Scheduler;

   ---------------------------------
   -- Map_Distributed_Application --
   ---------------------------------

   function Map_Distributed_Application (E : Node_Id) return Node_Id is
      D : constant Node_Id := New_Node (XTN.K_HI_Distributed_Application);
   begin
      pragma Assert (AINU.Is_System (E) or else AINU.Is_Processor (E));

      --  Update the global variable to be able to fetch the root of
      --  the distributed application and generate the source files.

      XML_Root := D;

      XTN.Set_Name (D, To_XML_Name (AIN.Name (AIN.Identifier (E))));
      XTN.Set_Units (D, New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (D, New_List (XTN.K_List_Id));

      return D;
   end Map_Distributed_Application;

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

   -------------------------
   -- Map_Port_Connection --
   -------------------------

   function Map_Port_Connection (E : Node_Id) return Node_Id is
      N   : Node_Id;
      R   : Node_Id;
      Q   : Node_Id;
      Src : Node_Id;
      Dst : Node_Id;
   begin
      N := Make_XML_Node ("Connection");

      Set_Str_To_Name_Buffer ("type");
      R := Make_Defining_Identifier (Name_Find);

      if Get_Category_Of_Connection (E) = CT_Data then
         if Get_Category_Of_Connection (E) = CT_Event then
            Set_Str_To_Name_Buffer ("eventdata");
         else
            Set_Str_To_Name_Buffer ("data");
         end if;
      elsif Get_Category_Of_Connection (E) = CT_Event then
         Set_Str_To_Name_Buffer ("event");
      end if;
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      Src := Item (AIN.First_Node (Path (Source (E))));
      Dst := Item (AIN.First_Node (Path (Destination (E))));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Src))));
      Set_Str_To_Name_Buffer ("src");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Dst))));
      Set_Str_To_Name_Buffer ("dst");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));
      return N;
   end Map_Port_Connection;

   --------------------
   -- Map_Bus_Access --
   --------------------

   function Map_Bus_Access (E : Node_Id) return Node_Id is
      N   : Node_Id;
      R   : Node_Id;
      Q   : Node_Id;
      Src : Node_Id;
      Dst : Node_Id;
   begin
      N := Make_XML_Node ("access");

      Set_Str_To_Name_Buffer ("type");
      R := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("bus");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      Src := Item (AIN.First_Node (Path (Source (E))));
      Dst := Item (AIN.First_Node (Path (Destination (E))));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Src))));
      Set_Str_To_Name_Buffer ("src");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Dst))));
      Set_Str_To_Name_Buffer ("dst");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      return N;
   end Map_Bus_Access;

   ---------------------
   -- Map_Data_Access --
   ---------------------

   function Map_Data_Access (E : Node_Id) return Node_Id is
      N   : Node_Id;
      R   : Node_Id;
      Q   : Node_Id;
      Src : Node_Id;
      Dst : Node_Id;
   begin
      N := Make_XML_Node ("access");

      Set_Str_To_Name_Buffer ("type");
      R := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("data");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      Src := Item (AIN.First_Node (Path (Source (E))));
      Dst := Item (AIN.First_Node (Path (Destination (E))));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Src))));
      Set_Str_To_Name_Buffer ("src");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      R :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Dst))));
      Set_Str_To_Name_Buffer ("dst");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, R), XTN.Items (N));

      return N;
   end Map_Data_Access;

   ----------------
   -- Map_System --
   ----------------

   function Map_System (E : Node_Id) return Node_Id is
      N : Node_Id;
      Q : Node_Id;
      P : Node_Id;
   begin
      N := Make_XML_Node ("ARINC653_Module");

      --  Set the name of the system

      P := Make_Defining_Identifier (To_XML_Name (AIN.Name (Identifier (E))));

      Set_Str_To_Name_Buffer ("ModuleName");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      return N;
   end Map_System;

   -----------------
   -- Map_Process --
   -----------------

   function Map_Process
     (E                    : Node_Id;
      Partition_Identifier : Unsigned_Long_Long) return Node_Id
   is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("Partition");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("PartitionName");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      P := Make_Literal (XV.New_Numeric_Value (Partition_Identifier, 1, 10));
      Set_Str_To_Name_Buffer ("PartitionIdentifier");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      AIN.Set_Backend_Node (Identifier (E), Copy_Node (P));

      Set_Str_To_Name_Buffer ("main");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("EntryPoint");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      Set_Str_To_Name_Buffer ("true");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("SystemPartition");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      return N;
   end Map_Process;

   -------------------
   -- Map_Processor --
   -------------------

   function Map_Processor (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("processor");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      Map_Scheduler (E, N);
      return N;
   end Map_Processor;

   -------------
   -- Map_Bus --
   -------------

   function Map_Bus (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("bus");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      return N;
   end Map_Bus;

   ---------------------------
   -- Map_Virtual_Processor --
   ---------------------------

   function Map_Virtual_Processor (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("virtual_processor");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      Map_Scheduler (E, N);

      return N;
   end Map_Virtual_Processor;

   --------------
   -- Map_Data --
   --------------

   function Map_Data (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("data");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      return N;
   end Map_Data;

   --------------------
   -- Map_Connection --
   --------------------

   function Map_Connection
     (Connection         : Node_Id;
      Channel_Identifier : Unsigned_Long_Long) return Node_Id
   is
      Channel_Node               : Node_Id;
      Source_Node                : Node_Id;
      Destination_Node           : Node_Id;
      P                          : Node_Id;
      N                          : Node_Id;
      Q                          : Node_Id;
      Source_Port_Name           : Name_Id;
      Destination_Port_Name      : Name_Id;
      Source_Component_Name      : Name_Id;
      Destination_Component_Name : Name_Id;
      Partition_Source           : Node_Id;
      Partition_Destination      : Node_Id;
   begin
      Channel_Node     := Make_XML_Node ("Channel");
      Source_Node      := Make_XML_Node ("Source");
      Destination_Node := Make_XML_Node ("Destination");

      Set_Str_To_Name_Buffer ("ChannelIdentifier");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal (XV.New_Numeric_Value (Channel_Identifier, 1, 10));
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (Channel_Node));

      N := Make_XML_Node ("Standard_Partition");

      Partition_Source :=
        AIN.Corresponding_Instance
          (AIN.Item (AIN.First_Node (AIN.Path (AIN.Source (Connection)))));

      Partition_Destination :=
        AIN.Corresponding_Instance
          (AIN.Item
             (AIN.First_Node (AIN.Path (AIN.Destination (Connection)))));

      Source_Component_Name :=
        AIN.Name
          (AIN.Identifier
             (AIN.Item (AIN.First_Node (AIN.Path (AIN.Source (Connection))))));

      Source_Port_Name :=
        AIN.Name
          (AIN.Identifier
             (AIN.Item
                (AIN.Next_Node
                   (AIN.First_Node (AIN.Path (AIN.Source (Connection)))))));

      Set_Str_To_Name_Buffer ("PortName");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Defining_Identifier (To_XML_Name (Source_Port_Name));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      Set_Str_To_Name_Buffer ("PartitionName");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Defining_Identifier (To_XML_Name (Source_Component_Name));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      if Present (Backend_Node (Identifier (Partition_Source))) then
         Set_Str_To_Name_Buffer ("PartitionIdentifier");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List
           (Make_Assignement
              (Q,
               Copy_Node (Backend_Node (Identifier (Partition_Source)))),
            XTN.Items (N));
      end if;

      Append_Node_To_List (N, XTN.Subitems (Source_Node));

      Destination_Component_Name :=
        AIN.Name
          (AIN.Identifier
             (AIN.Item
                (AIN.First_Node (AIN.Path (AIN.Destination (Connection))))));

      Destination_Port_Name :=
        AIN.Name
          (AIN.Identifier
             (AIN.Item
                (AIN.Next_Node
                   (AIN.First_Node
                      (AIN.Path (AIN.Destination (Connection)))))));

      N := Make_XML_Node ("Standard_Partition");
      Set_Str_To_Name_Buffer ("PortName");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Defining_Identifier (To_XML_Name (Destination_Port_Name));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      Set_Str_To_Name_Buffer ("PartitionName");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Defining_Identifier (To_XML_Name (Destination_Component_Name));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      if Present (Backend_Node (Identifier (Partition_Destination))) then
         Set_Str_To_Name_Buffer ("PartitionIdentifier");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List
           (Make_Assignement
              (Q,
               Copy_Node (Backend_Node (Identifier (Partition_Destination)))),
            XTN.Items (N));
      end if;

      Append_Node_To_List (N, XTN.Subitems (Destination_Node));

      Append_Node_To_List (Source_Node, XTN.Subitems (Channel_Node));
      Append_Node_To_List (Destination_Node, XTN.Subitems (Channel_Node));

      return Channel_Node;
   end Map_Connection;

   ------------------------
   -- Map_Process_Memory --
   ------------------------

   function Map_Process_Memory (Process : Node_Id) return Node_Id is
      N      : Node_Id;
      M      : Node_Id;
      P      : Node_Id;
      Q      : Node_Id;
      Memory : Node_Id;
   begin
      N := Make_XML_Node ("Partition_Memory");

      Set_Str_To_Name_Buffer ("PartitionName");
      P := Make_Defining_Identifier (Name_Find);
      Q :=
        Make_Defining_Identifier
          (To_XML_Name
             (Display_Name (Identifier (Parent_Subcomponent (Process)))));
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      if Present (Backend_Node (Identifier (Process))) then
         Set_Str_To_Name_Buffer ("PartitionIdentifier");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List
           (Make_Assignement
              (Q,
               Copy_Node (Backend_Node (Identifier (Process)))),
            XTN.Items (N));
      end if;

      if Get_Data_Size (Process) /= Null_Size then
         M := Make_XML_Node ("Memory_Requirements");

         Set_Str_To_Name_Buffer ("SizeBytes");
         P := Make_Defining_Identifier (Name_Find);
         Q :=
           Make_Literal
             (XV.New_Numeric_Value
                (To_Bytes (Get_Data_Size (Process)),
                 1,
                 10));
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));

         Set_Str_To_Name_Buffer ("Type");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer ("DATA");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));

         Set_Str_To_Name_Buffer ("Access");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer ("READ-WRITE");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));

         Append_Node_To_List (M, XTN.Subitems (N));
      end if;

      if Get_Code_Size (Process) /= Null_Size then
         M := Make_XML_Node ("Memory_Requirements");

         Set_Str_To_Name_Buffer ("SizeBytes");
         P := Make_Defining_Identifier (Name_Find);
         Q :=
           Make_Literal
             (XV.New_Numeric_Value
                (To_Bytes (Get_Code_Size (Process)),
                 1,
                 10));
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));

         Set_Str_To_Name_Buffer ("Type");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer ("CODE");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));

         Set_Str_To_Name_Buffer ("Access");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer ("READ-WRITE");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));

         Append_Node_To_List (M, XTN.Subitems (N));
      end if;

      if Get_Bound_Memory (Process) /= No_Node then
         Memory := Get_Bound_Memory (Process);
         declare
            Byte_Count : Unsigned_Long_Long := 1;
            Word_Size  : Unsigned_Long_Long := 1;
         begin
            if Get_Byte_Count (Get_Bound_Memory (Process)) /= 0 then
               Byte_Count := Get_Byte_Count (Get_Bound_Memory (Process));
            end if;

            if Get_Word_Size (Get_Bound_Memory (Process)) /= Null_Size then
               Word_Size :=
                 To_Bytes (Get_Word_Size (Get_Bound_Memory (Process)));
            end if;
            M := Make_XML_Node ("Memory_Requirements");

            Set_Str_To_Name_Buffer ("SizeBytes");
            P := Make_Defining_Identifier (Name_Find);
            Q :=
              Make_Literal
                (XV.New_Numeric_Value (Byte_Count * Word_Size, 1, 10));
            Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));

            if Get_ARINC653_Memory_Kind (Memory) = Data then
               Set_Str_To_Name_Buffer ("Type");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("DATA");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));
            elsif Get_ARINC653_Memory_Kind (Memory) = Code then
               Set_Str_To_Name_Buffer ("Type");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("CODE");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));
            end if;

            if Get_ARINC653_Access_Type (Memory) = Read then
               Set_Str_To_Name_Buffer ("Access");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("READ");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));
            elsif Get_ARINC653_Access_Type (Memory) = Read_Write then
               Set_Str_To_Name_Buffer ("Access");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("READ-WRITE");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));
            elsif Get_ARINC653_Access_Type (Memory) = Write then
               Set_Str_To_Name_Buffer ("Access");
               P := Make_Defining_Identifier (Name_Find);
               Set_Str_To_Name_Buffer ("WRITE");
               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (M));
            end if;

            Append_Node_To_List (M, XTN.Subitems (N));
         end;
      end if;

      return N;
   end Map_Process_Memory;

   ----------------------------
   -- Map_Process_Scheduling --
   ----------------------------

   procedure Map_Process_Scheduling
     (Process       :        Node_Id;
      Window_Number : in out Unsigned_Long_Long;
      N             :    out Node_Id)
   is
      P                            : Node_Id;
      Q                            : Node_Id;
      Window_Node                  : Node_Id;
      Associated_Virtual_Processor : constant Node_Id :=
        Get_Bound_Processor (Process);
      Associated_Processor : constant Node_Id :=
        Parent_Component (Parent_Subcomponent (Associated_Virtual_Processor));
      Slots : constant Time_Array := Get_POK_Slots (Associated_Processor);
      Slots_Allocation  : List_Id;
      S                 : Node_Id;
      Referenced_Entity : Node_Id;
      Start_Time        : Long_Double         := 0.0;
      Duration_Time     : Long_Double         := 0.0;
   begin
      N := Make_XML_Node ("Partition_Schedule");

      Slots_Allocation := Get_POK_Slots_Allocation (Associated_Processor);

      if Slots_Allocation = No_List then
         Display_Error
           ("You must provide the slots allocation for each processor",
            Fatal => True);
      end if;

      Set_Str_To_Name_Buffer ("PartitionName");
      P := Make_Defining_Identifier (Name_Find);
      Q :=
        Make_Defining_Identifier
          (To_XML_Name
             (Display_Name (Identifier (Parent_Subcomponent (Process)))));
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      if Present (Backend_Node (Identifier (Process))) then
         Set_Str_To_Name_Buffer ("PartitionIdentifier");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List
           (Make_Assignement
              (Q,
               Copy_Node (Backend_Node (Identifier (Process)))),
            XTN.Items (N));
      end if;

      Set_Str_To_Name_Buffer ("PeriodSeconds");
      P := Make_Defining_Identifier (Name_Find);
      Q :=
        Make_Literal
          (XV.New_Floating_Point_Value
             (To_Seconds (Get_POK_Major_Frame (Associated_Processor))));
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      Start_Time := 0.0;

      S := ATN.First_Node (Slots_Allocation);
      for I in Slots'Range loop
         Referenced_Entity := ATE.Get_Referenced_Entity (S);

         if Referenced_Entity = Associated_Virtual_Processor then
            Window_Node := Make_XML_Node ("Window_Schedule");

            Set_Str_To_Name_Buffer ("WindowStartSeconds");
            P := Make_Defining_Identifier (Name_Find);
            Q := Make_Literal (XV.New_Floating_Point_Value (Start_Time));
            Append_Node_To_List
              (Make_Assignement (P, Q),
               XTN.Items (Window_Node));

            Set_Str_To_Name_Buffer ("WindowIdentifier");
            P := Make_Defining_Identifier (Name_Find);
            Q := Make_Literal (XV.New_Numeric_Value (Window_Number, 1, 10));
            Append_Node_To_List
              (Make_Assignement (P, Q),
               XTN.Items (Window_Node));

            Set_Str_To_Name_Buffer ("WindowDurationSeconds");
            P := Make_Defining_Identifier (Name_Find);
            Q :=
              Make_Literal
                (XV.New_Floating_Point_Value (To_Seconds (Slots (I))));
            Append_Node_To_List
              (Make_Assignement (P, Q),
               XTN.Items (Window_Node));

            Append_Node_To_List (Window_Node, XTN.Subitems (N));
            Window_Number := Window_Number + 1;
            Duration_Time := Duration_Time + To_Seconds (Slots (I));
         end if;

         Start_Time := Start_Time + To_Seconds (Slots (I));
         S          := ATN.Next_Node (S);
      end loop;

      Set_Str_To_Name_Buffer ("PeriodDurationSeconds");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal (XV.New_Floating_Point_Value (Duration_Time));
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

   end Map_Process_Scheduling;

   --------------
   -- Map_Port --
   --------------

   function Map_Port (F : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
      R : Node_Id;
   begin
      if AIN.Is_Event (F) and then AIN.Is_Data (F) then
         N := Make_XML_Node ("Queueing_Port");
      elsif AIN.Is_Data (F) and then not AIN.Is_Event (F) then
         N := Make_XML_Node ("Sampling_Port");
      else
         return No_Node;
      end if;

      --  Add the direction of the port as attribute

      Set_Str_To_Name_Buffer ("Direction");
      P := Make_Defining_Identifier (Name_Find);
      if AIN.Is_In (F) and then not AIN.Is_Out (F) then
         Set_Str_To_Name_Buffer ("DESTINATION");
      elsif AIN.Is_Out (F) and then not AIN.Is_In (F) then
         Set_Str_To_Name_Buffer ("SOURCE");
      else
         return No_Node;
      end if;
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      --  Add the name of the port as an attribute

      Set_Str_To_Name_Buffer ("Name");
      R := Make_Defining_Identifier (Name_Find);

      Q :=
        Make_Defining_Identifier (To_XML_Name (Display_Name (Identifier (F))));

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));

      if AIN.Is_Data (F) and then not AIN.Is_Event (F) then
         Set_Str_To_Name_Buffer ("RefreshRateSeconds");
         R := Make_Defining_Identifier (Name_Find);

         if Get_POK_Refresh_Time (F) /= Null_Time then
            Q :=
              Make_Literal
                (XV.New_Floating_Point_Value
                   (To_Seconds (Get_POK_Refresh_Time (F))));
         else
            Q := Map_Time (Null_Time);
         end if;
         Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));
      elsif AIN.Is_Data (F) and then AIN.Is_Event (F) then
         Set_Str_To_Name_Buffer ("MaxNbMessages");
         P := Make_Defining_Identifier (Name_Find);

         if Get_Queue_Size (F) /= -1 then
            Q :=
              Make_Literal
                (XV.New_Numeric_Value
                   (Unsigned_Long_Long (Get_Queue_Size (F)),
                    1,
                    10));
         else
            Q := Make_Literal (XV.New_Numeric_Value (1, 1, 10));
         end if;

         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));
      end if;

      if AIN.Is_Data (F) then
         Set_Str_To_Name_Buffer ("MaxMessageSize");
         P := Make_Defining_Identifier (Name_Find);

         if Get_Data_Size (Corresponding_Instance (F)) /= Null_Size then
            Q :=
              Make_Literal
                (XV.New_Numeric_Value
                   (To_Bytes (Get_Data_Size (Corresponding_Instance (F))),
                    1,
                    10));
         else
            Q := Make_Literal (XV.New_Numeric_Value (1, 1, 10));
         end if;

         Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));
      end if;
      return N;
   end Map_Port;

   -------------------
   -- Map_Data_Size --
   -------------------

   function Map_Data_Size (T : Size_Type) return Unsigned_Long_Long is
      pragma Unreferenced (T);
   begin
      return 0;
   end Map_Data_Size;

   --------------------------
   -- Map_Process_HM_Table --
   --------------------------

   function Map_Process_HM_Table (Process : Node_Id) return Node_Id is
      Partition_HM : Node_Id;
      pragma Unreferenced (Process);
   begin
      Partition_HM := Make_XML_Node ("Partition_HM_Table");
      return Partition_HM;
   end Map_Process_HM_Table;

   ----------------------------
   -- Map_Processor_HM_Table --
   ----------------------------

   function Map_Processor_HM_Table (Processor : Node_Id) return Node_Id is
      Module_HM : Node_Id;
      pragma Unreferenced (Processor);
   begin
      Module_HM := Make_XML_Node ("Module_HM_Table");
      return Module_HM;
   end Map_Processor_HM_Table;

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
