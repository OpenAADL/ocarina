------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.AIR_CONF.PARTITIONS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2018-2020 ESA & ISAE.                    --
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
with Utils;         use Utils;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Tree.Entities;

with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.Utils;
with Ocarina.Instances.Queries;

with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Properties.ARINC653;

with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Values;

package body Ocarina.Backends.AIR_Conf.Partitions is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Instances.Queries;
   use Ocarina.Backends.C_Common.Mapping;

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Properties.ARINC653;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;
   package XV renames Ocarina.Backends.XML_Values;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

   Current_System : Node_Id := No_Node;

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

   -------------------
   -- Map_Partition --
   -------------------

   procedure Map_Partition (E : Node_Id)
      with Pre => (Get_Category_Of_Component (E) = CC_Process);

   procedure Map_Partition (E : Node_Id) is
      Partition_Node       : Node_Id;
      Byte_Count_Value     : Unsigned_Long_Long;
      Associated_Processor : Node_Id;
      Associated_Module    : Node_Id;
      Associated_Memory    : Node_Id;
      PartitionConfiguration : Node_Id;
      Port_Node            : Node_Id;
      Memory_Node          : Node_Id;
      Libs_Node            : Node_Id;
      Devices_Node         : Node_Id;
      Cache_Node : Node_Id;
      Permissions_Node : Node_Id;
      P                    : Node_Id;
      Q                    : Node_Id;
      F                    : Node_Id;
      Personnality         : Supported_Execution_Platform;
   begin
      Associated_Processor := Get_Bound_Processor (E);
      Associated_Memory    := Get_Bound_Memory (E);
      Associated_Module    :=
        Parent_Component (Parent_Subcomponent (Associated_Processor));
      Personnality := Get_Execution_Platform (Associated_Processor);

      --  Some checks on the model in order to make sure that
      --  everything is correctly defined.

      if Associated_Processor = No_Node then
         Display_Located_Error
           (AIN.Loc (E),
            "A partition has to be associated with one virtual processor.",
            Fatal => True);
      end if;

      if Associated_Memory = No_Node then
         Display_Located_Error
           (AIN.Loc (E),
            "A partition has to be associated with one memory.",
            Fatal => True);
      end if;

      if Associated_Module = No_Node then
         Display_Located_Error
           (AIN.Loc (E),
            "Unable to retrieve the module that executes this partition.",
            Fatal => True);
      end if;

      --  Create the Partition node, that defines all partition requirements

      Append_Node_To_List
        (Make_XML_Comment (Get_String_Name ("Partition")),
         XTN.Subitems (Current_XML_Node));

      Partition_Node := Make_XML_Node ("Partition");

      --  a) id of the partition

      Set_Str_To_Name_Buffer ("PartitionIdentifier");
      P := Make_Defining_Identifier (Name_Find);
      Q :=
        Make_Literal
          (XV.New_Numeric_Value
             (Get_Partition_Identifier
                (Associated_Processor),
              0,
              10));
      Append_Node_To_List
        (Make_Assignement (P, Q),
         XTN.Items (Partition_Node));

      --  b) name of the partition

      Set_Str_To_Name_Buffer ("PartitionName");
      P := Make_Defining_Identifier (Name_Find);
      Get_Name_String
        (To_Lower
           (Display_Name
              (Identifier (Parent_Subcomponent (E)))));

      --  Note: we store the node_id of the process bound to the
      --  current virtual processor (partition). We use the name of
      --  the process to match code generation rules, where processes
      --  name are used for each sub-directory.

      AIN.Set_Backend_Node (Identifier (Associated_Processor), E);

      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List
        (Make_Assignement (P, Q),
         XTN.Items (Partition_Node));

      --  c) XXX hard-coded configuration parameters

      XTU.Add_Attribute ("Criticality", "LEVEL_A", Partition_Node);
      XTU.Add_Attribute ("EntryPoint", "entry_func", Partition_Node);
      XTU.Add_Attribute ("SystemPartition", "false", Partition_Node);

      --  Now, handle the ports of the partition.

      if Has_Ports (E) then
         F := First_Node (Features (E));
         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then

               --  XXX move out as REAL checks

               if not Is_Data (F) then
                  Display_Located_Error
                    (AIN.Loc (F),
                     "Pure events ports are not allowed.",
                     Fatal => True);
               end if;

               if Is_In (F) and then Is_Out (F) then
                  Display_Located_Error
                    (AIN.Loc (F),
                     "in/out ports are not allowed.",
                     Fatal => True);
               end if;

               --  Type of port

               if Is_Data (F) and then not Is_Event (F) then
                  Port_Node := Make_XML_Node ("Sampling_Port");
               else
                  Port_Node := Make_XML_Node ("Queuing_Port");
               end if;

               --  Port name: partition + port name

               Set_Str_To_Name_Buffer ("Name");
               P := Make_Defining_Identifier (Name_Find);

               Get_Name_String
                 (Map_C_Enumerator_Name (F,
                                         Fully_Qualify_Parent => True));

               Q := Make_Defining_Identifier (To_Lower (Name_Find));
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Port_Node));

               --  Direction

               Set_Str_To_Name_Buffer ("Direction");
               P := Make_Defining_Identifier (Name_Find);

               if Is_In (F) then
                  Set_Str_To_Name_Buffer ("DESTINATION");
               else
                  Set_Str_To_Name_Buffer ("SOURCE");
               end if;

               Q := Make_Defining_Identifier (Name_Find);
               Append_Node_To_List
                 (Make_Assignement (P, Q),
                  XTN.Items (Port_Node));

               --  MaxMessageSize

               if Get_Data_Size (Corresponding_Instance (F)) /= Null_Size then
                  --  If data size is specified, use this value, add
                  --  40 to take into account PolyORB-HI/C header
                  --  (conservative value).

                  Q :=
                    Make_Literal
                      (XV.New_Numeric_Value
                         (40 + To_Bytes
                            (Get_Data_Size (Corresponding_Instance (F))),
                          1,
                          10));
               else
                  Display_Located_Error
                    (Loc (F),
                     "No data size given for data size",
                     Fatal => True);
               end if;

               Set_Str_To_Name_Buffer ("MaxMessageSize");
               P := Make_Defining_Identifier (Name_Find);

               Append_Node_To_List
                 (Make_Assignement (P, Q), XTN.Items (Port_Node));

               --  MaxNbMessages

               if Is_Event (F) then
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

                  Set_Str_To_Name_Buffer ("MaxNbMessages");
                  P := Make_Defining_Identifier (Name_Find);

                  Append_Node_To_List
                    (Make_Assignement (P, Q), XTN.Items (Port_Node));
               end if;

               if Is_Data (F) then
                  --  RefreshRateSeconds

                  XTU.Add_Attribute ("RefreshRateSeconds", "1.0", Port_Node);
                  --  XXX hardcoded

                  Append_Node_To_List
                    (Port_Node, XTN.Subitems (Partition_Node));
               end if;

            end if;
            F := Next_Node (F);
         end loop;
      end if;

      --  Create the PartitionConfiguration associated with the partition.

      PartitionConfiguration := Make_XML_Node ("PartitionConfiguration");

      if Personnality = Platform_AIR then
         XTU.Add_Attribute ("Personality", "RTEMS5",
                            PartitionConfiguration);
      elsif Personnality = Platform_AIR_IOP then
         XTU.Add_Attribute ("Personality", "Bare",
                            PartitionConfiguration);
      else
         raise Program_Error with "Unsupported platform " & Personnality'Img;
      end if;

      XTU.Add_Attribute ("Cores", "1",
                         PartitionConfiguration); -- XXX hardcoded

      Append_Node_To_List (PartitionConfiguration,
                           XTN.Subitems (Partition_Node));

      --  Libs node, child of PartitionConfiguration

      Libs_Node := Make_XML_Node ("Libs");
      Append_Node_To_List (Libs_Node, XTN.Subitems (Partitionconfiguration));

      if Personnality = Platform_AIR then
         Append_Node_To_List
           (Make_Defining_Identifier
              (Get_String_Name ("LIBAIR; IMASPEX; LIBPRINTF")),
            XTN.Subitems (Libs_Node));
      elsif Personnality = Platform_AIR_IOP then
         Append_Node_To_List
           (Make_Defining_Identifier
              (Get_String_Name ("LIBIOP")),
            XTN.Subitems (Libs_Node));
      end if;

      --  Devices node, child of PartitionConfiguration

      Devices_Node := Make_XML_Node ("Devices");
      Append_Node_To_List (Devices_Node,
                           XTN.Subitems (Partitionconfiguration));

      --  Cache node, child of PartitionConfiguration

      Cache_Node := Make_XML_Node ("Cache");
      Append_Node_To_List (Cache_Node, XTN.Subitems (Partitionconfiguration));

      Append_Node_To_List
        (Make_Defining_Identifier
           (Get_String_Name ("CODE; DATA")), --  XXX hardcoded
         XTN.Subitems (Cache_Node));

      --  Memory node, child of PartitionConfiguration

      Memory_Node := Make_XML_Node ("Memory");

      Byte_Count_Value :=
        Get_Integer_Property (Associated_Memory, "byte_count");
      Set_Str_To_Name_Buffer ("Size");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("0x");
      Add_ULL_To_Name_Buffer (Byte_Count_Value, 16);

      Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (Memory_Node));

      Append_Node_To_List (Memory_Node, XTN.Subitems (Partitionconfiguration));

      --  Permissions node, child of PartitionConfiguration

      Permissions_Node := Make_XML_Node ("Permissions");

      if Personnality = Platform_AIR then
         Append_Node_To_List
           (Make_Defining_Identifier
              (Get_String_Name
                 ("FPU_CONTROL; GLOBAL_TIME; CACHE_CONTROL;"
                    & "SET_TOD; SET_PARTITION_MODE;")), --  XXX hardcoded
            XTN.Subitems (Permissions_Node));
      elsif Personnality = Platform_AIR_IOP then
         Append_Node_To_List
           (Make_Defining_Identifier (Get_String_Name ("SUPERVISOR;")),
            XTN.Subitems (Permissions_Node));
      end if;

      Append_Node_To_List (Permissions_Node,
                           XTN.Subitems (Partitionconfiguration));

      Append_Node_To_List (Partition_Node, XTN.Subitems (Current_XML_Node));
   end Map_Partition;

   -------------------------
   -- Map_Module_Schedule --
   -------------------------

   procedure Map_Module_Schedule (E : Node_Id)
      with Pre => (Get_Category_Of_Component (E) = CC_Processor);

   procedure Map_Module_Schedule (E : Node_Id) is
      Schedule_Identifier : constant Unsigned_Long_Long := 1;
      Window_Identifier : Unsigned_Long_Long := 0;

      Module_Schedule : constant Schedule_Window_Record_Term_Array :=
        Get_Module_Schedule_Property (E);

      Module_Schedule_Node : Node_Id;
      Window_Schedule_Node : Node_Id;

      P, Q, S : Node_Id;

      Partition_Node : Node_Id;
      Partition       : Node_Id;

      Start_Time      : Long_Double := 0.0;

   begin
      if Module_Schedule'Length = 0 then
         Display_Error
           ("You must provide the slots allocation for each processor",
            Fatal => True);
      end if;

      Append_Node_To_List
        (Make_XML_Comment
           (Get_String_Name ("Schedule" & Schedule_Identifier'Img)),
         XTN.Subitems (Current_XML_Node));

      --  Module_Schedule root node

      Module_Schedule_Node := Make_XML_Node ("Module_Schedule");

      Append_Node_To_List (Module_Schedule_Node,
                           XTN.Subitems (Current_XML_Node));

      --  Associate a fixed identifier to the slot.

      Set_Str_To_Name_Buffer ("ScheduleIdentifier");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal (XV.New_Numeric_Value (Schedule_Identifier, 0, 10));
      Append_Node_To_List (Make_Assignement (P, Q),
                           XTN.Items (Module_Schedule_Node));

      --  Schedule name

      Set_Str_To_Name_Buffer ("ScheduleName");
      P := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer
        ("schedule_" & Schedule_Identifier'Img);
      Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));

      Append_Node_To_List (Make_Assignement (P, Q),
                           XTN.Items (Module_Schedule_Node));

      --  InitialModuleSchedule

      XTU.Add_Attribute
        ("InitialModuleSchedule", "true", Module_Schedule_Node);

      --  MajorFrameSeconds

      Set_Str_To_Name_Buffer ("MajorFrameSeconds");
      P := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer
        (Long_Double'Image (To_Seconds (Get_POK_Major_Frame (E))));
      Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));

      Append_Node_To_List (Make_Assignement (P, Q),
                           XTN.Items (Module_Schedule_Node));

      --  For each time slot for a partition, we declare
      --  it in the scheduling plan.

      for J in Module_Schedule'Range loop
         Partition := Module_Schedule (J).Partition;

         --  Create the node that corresponds to the slot.
         Partition_Node := Make_XML_Node ("Partition_Schedule");
         Append_Node_To_List (Partition_Node,
                              XTN.Subitems (Module_Schedule_Node));

         --  XXX The following is a work-around to address limit in
         --  instantiation of record properties ????

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               if Corresponding_Declaration (S) =
                 Module_Schedule (J).Partition
               then
                  Partition := Corresponding_Instance (S);
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         --  PartitionName

         Set_Str_To_Name_Buffer ("PartitionName");
         P := Make_Defining_Identifier (Name_Find);

         --  Note: we retrieve the identifier of the process bound to
         --  this virtual processor, stored in backend_node

         Get_Name_String
           (To_Lower
              (Display_Name
                 (Identifier (Parent_Subcomponent
                                (AIN.Backend_Node
                                   (Identifier (Partition)))))));

         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List
           (Make_Assignement (P, Q),
            XTN.Items (Partition_Node));

         --  PartitionIdentifier

         Set_Str_To_Name_Buffer ("PartitionIdentifier");
         P := Make_Defining_Identifier (Name_Find);

         Q :=
           Make_Literal
             (XV.New_Numeric_Value
                (Get_Partition_Identifier (Partition),
                 0,
                 10));
         Append_Node_To_List (Make_Assignement (P, Q),
                              XTN.Items (partition_Node));

         --  PeriodDurationSeconds

         Set_Str_To_Name_Buffer ("PeriodDurationSeconds");
         P := Make_Defining_Identifier (Name_Find);

         Set_Str_To_Name_Buffer
           (Long_Double'Image
              (To_Seconds (Module_Schedule (J).Duration)));
         Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));
         Append_Node_To_List (Make_Assignement (P, Q),
                              XTN.Items (Partition_Node));

         --  PeriodSeconds

         Set_Str_To_Name_Buffer ("PeriodSeconds");
         P := Make_Defining_Identifier (Name_Find);

         Set_Str_To_Name_Buffer
           (Long_Double'Image (To_Seconds (Get_POK_Major_Frame (E))));
         Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));

         Append_Node_To_List (Make_Assignement (P, Q),
                              XTN.Items (Partition_Node));

         --  WindowSchedule node, child of Partition_Schedule node

         Window_Schedule_Node := Make_XML_Node ("Window_Schedule");
         Append_Node_To_List (Window_Schedule_Node,
                              XTN.Subitems (Partition_Node));

         Xtu.Add_Attribute ("PartitionPeriodStart", "true",
                            Window_Schedule_Node); --  XXX hardcoded

         Set_Str_To_Name_Buffer ("WindowDurationSeconds");
         P := Make_Defining_Identifier (Name_Find);

         Set_Str_To_Name_Buffer
           (Long_Double'Image
              (To_Seconds (Module_Schedule (J).Duration)));
         Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));
         Append_Node_To_List (Make_Assignement (P, Q),
                              XTN.Items (Window_Schedule_Node));

         Set_Str_To_Name_Buffer ("WindowIdentifier");
         P := Make_Defining_Identifier (Name_Find);
         Q := Make_Literal (XV.New_Numeric_Value
                              (Schedule_Identifier * 10 + Window_Identifier,
                               0, 10));
         Append_Node_To_List (Make_Assignement (P, Q),
                              XTN.Items (Window_Schedule_Node));

         --  Definition of the Cores attribute

         declare
            Cores : constant List_Id := Get_Bound_Processor_L (Partition);
            S : Node_Id;
            Core : Node_Id;
            List_Of_Cores : Name_Id := No_Name;
            First_Run : Boolean := True;
         begin
            --  For partition Partition, we iterate on the list of
            --  processors this partition is bound to, for each
            --  partition, we get is Core_Id and build a ';' separated
            --  string with numerical Core_Id value.

            if not ATNU.Is_Empty (Cores) then
               S := ATN.First_Node (Cores);
               while Present (S) loop
                  Core :=
                    Ocarina.ME_AADL.AADL_Tree.Entities.Get_Referenced_Entity
                    (S);
                  if First_Run then
                     First_Run := False;
                     Set_Str_To_Name_Buffer
                       (Ocarina.Backends.Properties.Get_Core_Id (Core)'Img);
                  else
                     Get_Name_String (List_Of_Cores);
                     Add_Char_To_Name_Buffer (';');
                     Add_Str_To_Name_Buffer
                       (Ocarina.Backends.Properties.Get_Core_Id (Core)'Img);
                  end if;
                  List_Of_Cores := Name_Find;
                  S := ATN.Next_Node (S);
               end loop;

               Set_Str_To_Name_Buffer ("Cores");
               P := Make_Defining_Identifier (Name_Find);
               Q := Make_Defining_Identifier
                 (Remove_Char (List_Of_Cores, ' '));
               Append_Node_To_List (Make_Assignement (P, Q),
                                    XTN.Items (Window_Schedule_Node));
            end if;

         end;

         Window_Identifier := Window_Identifier + 1;
         if Window_Identifier = 10 then
            raise Program_Error;
         end if;

         --  Define the start attribute of the <slot/> element.
         Set_Str_To_Name_Buffer ("WindowStartSeconds");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer (Long_Double'Image (Start_Time));
         Q := Make_Defining_Identifier (Remove_Char (Name_Find, ' '));
         Append_Node_To_List (Make_Assignement (P, Q),
                              XTN.Items (Window_Schedule_Node));

         Start_Time := Start_Time + To_Seconds (Module_Schedule (J).Duration);

      end loop;

   end Map_Module_Schedule;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
   begin
      Map_Partition (E);
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
      U : Node_Id;
      R : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      R := XTN.Node (Backend_Node (Identifier (E)));

      Current_System := E;

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Push_Entity (U);
      Push_Entity (R);

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

      Pop_Entity;
      Pop_Entity;
   end Visit_System_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
   begin
      Visit_Subcomponents_Of (E);
      Map_Module_Schedule (E);
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (Current_System)) then
         S := First_Node (Subcomponents (Current_System));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if AINU.Is_Process (Corresponding_Instance (S))
              and then Get_Bound_Processor (Corresponding_Instance (S)) = E
            then
               Visit (Corresponding_Instance (S));
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      Visit_Subcomponents_Of (E);
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.AIR_Conf.Partitions;
