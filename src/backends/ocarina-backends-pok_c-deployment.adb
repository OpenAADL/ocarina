------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P O K _ C . D E P L O Y M E N T     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Namet;
with Utils; use Utils;
with Locations;

with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Utils;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.POK_C.Runtime;
with Ocarina.Backends.C_Common.Mapping;

with Ocarina.Instances.Queries;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Entities.Properties;

package body Ocarina.Backends.POK_C.Deployment is

   use Ocarina.Namet;
   use Locations;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.POK_C.Runtime;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Instances.Queries;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package ATNU renames Ocarina.ME_AADL.AADL_Tree.Nutils;
   package AIE renames Ocarina.ME_AADL.AADL_Instances.Entities;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AIEP renames Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CV renames Ocarina.Backends.C_Values;

   Array_Global_Ports_Nodes          : Node_Id;
   Array_Local_Ports_To_Global_Ports : Node_Id;
   Array_Global_Ports_To_Local_Ports : Node_Id;
   Array_Global_Ports_Bus            : Node_Id;
   Array_Buses_Partitions            : Node_Id;
   All_Ports                         : List_Id;
   Used_Buses                        : List_Id;
   Node_Identifier                   : Unsigned_Long_Long := 0;
   Current_Device                    : Node_Id            := No_Node;

   --------------------
   -- Is_Bus_Handled --
   --------------------

   function Is_Bus_Handled (Bus : Node_Id) return Boolean is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Used_Buses) then
         S := AIN.First_Node (Used_Buses);
         while Present (S) loop
            if (Item (S) = Bus) then
               return True;
            end if;
            S := AIN.Next_Node (S);
         end loop;

      end if;

      return False;
   end Is_Bus_Handled;

   ------------------------------
   -- Handle_Buses_Connections --
   ------------------------------

   procedure Handle_Buses_Connections (Port : Node_Id) is
      L          : List_Id;
      S          : Node_Id;
      Connection : Node_Id;

      procedure Handle_Bus (My_Bus : Node_Id) is
         Bus : Node_Id;
      begin
         Bus := My_Bus;

         if AINU.Is_Bus (Bus) and then not Is_Bus_Handled (Bus) then
            AINU.Append_Node_To_List
              (AINU.Make_Node_Container (Bus),
               Used_Buses);
         elsif AINU.Is_Virtual_Bus (Bus) then

            Bus := Parent_Component (Parent_Subcomponent (Bus));
            if AINU.Is_Bus (Bus) and then not Is_Bus_Handled (Bus) then
               AINU.Append_Node_To_List
                 (AINU.Make_Node_Container (Bus),
                  Used_Buses);
            end if;
         end if;
      end Handle_Bus;
   begin
      if AIN.Is_In (Port) then
         L := Sources (Port);
         if not AINU.Is_Empty (L) then
            S := AIN.First_Node (L);
            while Present (S) loop
               Connection := Extra_Item (S);

               if Present (Connection)
                 and then Get_Bound_Bus (Connection, False) /= No_Node
               then
                  Handle_Bus (Get_Bound_Bus (Connection, False));
               end if;

               S := AIN.Next_Node (S);
            end loop;
         end if;
      elsif AIN.Is_Out (Port) then
         L := Destinations (Port);
         if not AINU.Is_Empty (L) then
            S := AIN.First_Node (L);
            while Present (S) loop
               Connection := Extra_Item (S);

               if Present (Connection)
                 and then Get_Bound_Bus (Connection, False) /= No_Node
               then
                  Handle_Bus (Get_Bound_Bus (Connection, False));
               end if;

               S := AIN.Next_Node (S);
            end loop;
         end if;
      end if;
   end Handle_Buses_Connections;

   -------------
   -- Get_Bus --
   -------------

   function Get_Bus (Port : Node_Id) return Node_Id is
      L          : List_Id;
      S          : Node_Id;
      Bus        : Node_Id;
      Connection : Node_Id;
   begin
      if AIN.Is_In (Port) then
         L := Sources (Port);
         if not AINU.Is_Empty (L) then
            S := AIN.First_Node (L);
            while Present (S) loop
               Connection := Extra_Item (S);

               if Present (Connection)
                 and then Get_Bound_Bus (Connection, False) /= No_Node
               then
                  Bus := Get_Bound_Bus (Connection, False);

                  if AINU.Is_Bus (Bus) then
                     return Bus;
                  elsif AINU.Is_Virtual_Bus (Bus) then
                     Bus := Parent_Component (Parent_Subcomponent (Bus));
                     if AINU.Is_Bus (Bus) then
                        return Bus;
                     end if;
                  end if;
               end if;

               S := AIN.Next_Node (S);
            end loop;
         end if;
      elsif AIN.Is_Out (Port) then
         L := Destinations (Port);
         if not AINU.Is_Empty (L) then
            S := AIN.First_Node (L);
            while Present (S) loop
               Connection := Extra_Item (S);

               if Present (Connection)
                 and then Get_Bound_Bus (Connection, False) /= No_Node
               then
                  Bus := Get_Bound_Bus (Connection, False);

                  if AINU.Is_Bus (Bus) then
                     return Bus;
                  elsif AINU.Is_Virtual_Bus (Bus) then
                     Bus := Parent_Component (Parent_Subcomponent (Bus));
                     if AINU.Is_Bus (Bus) then
                        return Bus;
                     end if;
                  end if;
               end if;

               S := AIN.Next_Node (S);
            end loop;
         end if;
      end if;

      return No_Node;

   end Get_Bus;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
        (E                    : Node_Id;
         Real_Process         : Boolean := True;
         Associated_Component : Node_Id := No_Node);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);

      System_Nb_Partitions    : Unsigned_Long_Long := 0;
      System_Nb_Threads       : Unsigned_Long_Long := 0;
      Process_Nb_Threads      : Unsigned_Long_Long := 0;
      Process_Nb_Buffers      : Unsigned_Long_Long := 0;
      Process_Nb_Events       : Unsigned_Long_Long := 0;
      Process_Nb_Lock_Objects : Unsigned_Long_Long := 0;
      System_Nb_Lock_Objects  : Unsigned_Long_Long := 0;
      Process_Nb_Blackboards  : Unsigned_Long_Long := 0;
      System_Nb_Ports         : Unsigned_Long_Long := 0;
      System_Stacks_Size      : Unsigned_Long_Long := 0;
      Partition_Stacks_Size   : Unsigned_Long_Long := 0;

      Kernel_Unit : Node_Id;

      Global_Nb_Ports_Node   : Node_Id;
      Kernel_Needs_Sched_Rms : Boolean;
      Kernel_Needs_Sched_Llf : Boolean;
      Kernel_Needs_Sched_Edf : Boolean;
      Kernel_Needs_Sched_Rr  : Boolean;

      System_Partitions_Ports : Node_Id;
      --  This is an array that contain all the partitions
      --  identifier for each port.
      --  Consequently, we have an array like that:
      --  pok_ports_partitions_id[POK_NB_PORTS] = {....}

      System_Partitions_Size : Node_Id;
      --  This array represents the size of each partition. We
      --  have an array like:
      --  POK_PARTTIONS_SIZE {xxx,xxx}

      System_Partitions_Scheduler       : Node_Id;
      System_Partitions_Nb_Threads      : Node_Id;
      System_Partitions_Nb_Lock_Objects : Node_Id;
      Major_Frame                       : Node_Id;
      System_Slots                      : Node_Id;
      System_Slots_Allocation           : Node_Id;
      System_Nb_Slots                   : Unsigned_Long_Long := 0;
      Local_Port_List                   : List_Id;
      Global_Port_List                  : List_Id;
      Node_List                         : List_Id;
      Local_Port_Identifier             : Unsigned_Long_Long := 0;
      Global_Port_Identifier            : Unsigned_Long_Long := 0;
      System_Use_Queueing_Ports         : Boolean;
      System_Use_Sampling_Ports         : Boolean;
      System_Use_Virtual_Ports          : Boolean;
      System_Partition_Identifier       : Unsigned_Long_Long := 0;
      Partition_Use_Error_Handling      : Boolean            := False;
      Kernel_Use_Error_Handling         : Boolean            := False;
      Kernel_Use_Pci                    : Boolean            := False;
      Kernel_Use_Io                     : Boolean            := False;
      Kernel_Use_Gettick                : Boolean            := False;

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
         All_Ports        := AINU.New_List (AIN.K_List_Id, No_Location);
         Global_Port_List := New_List (CTN.K_Enumeration_Literals);
         Node_List        := New_List (CTN.K_Enumeration_Literals);

         Array_Global_Ports_Nodes := CTU.Make_Array_Values;

         Global_Nb_Ports_Node := Make_Literal (CV.New_Int_Value (0, 1, 10));

         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           AIE.Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Device =>
               Visit_Device_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
         N         : Node_Id;
         S         : Node_Id;
         U         : Node_Id;
         Processes : List_Id;
      begin
         if POK_Flavor = ARINC653 then
            CTU.Append_Node_To_List
              (RE (RE_Pok_Sched_Rr),
               CTN.Values (System_Partitions_Scheduler));
            Kernel_Needs_Sched_Rr := True;
         elsif POK_Flavor = POK then
            if Get_POK_Scheduler (E) = RR then
               Kernel_Needs_Sched_Rr := True;
               CTU.Append_Node_To_List
                 (RE (RE_Pok_Sched_Rr),
                  CTN.Values (System_Partitions_Scheduler));
            elsif Get_POK_Scheduler (E) = RMS then
               Kernel_Needs_Sched_Rms := True;
               CTU.Append_Node_To_List
                 (RE (RE_Pok_Sched_Rms),
                  CTN.Values (System_Partitions_Scheduler));
            elsif Get_POK_Scheduler (E) = EDF then
               Kernel_Needs_Sched_Edf := True;
               CTU.Append_Node_To_List
                 (RE (RE_Pok_Sched_Edf),
                  CTN.Values (System_Partitions_Scheduler));
            elsif Get_POK_Scheduler (E) = LLF then
               Kernel_Needs_Sched_Llf := True;
               CTU.Append_Node_To_List
                 (RE (RE_Pok_Sched_Llf),
                  CTN.Values (System_Partitions_Scheduler));
            else
               Display_Error
                 ("POK does not support a such scheduler in partitions",
                  Fatal => True);
            end if;
         end if;

         N :=
           CTU.Make_Literal
             (CV.New_Int_Value (System_Partition_Identifier, 1, 10));
         Set_Handling (E, By_Node, H_C_Deployment_Spec, N);

         N :=
           CTU.Make_Literal
             (CV.New_Int_Value (System_Partition_Identifier, 1, 10));

         CTN.Set_Deployment_Node (Backend_Node (Identifier (E)), N);

         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            S         := AIN.First_Node (Processes);
            while Present (S) loop
               U := Current_Entity;
               Pop_Entity;
               Visit (AIN.Item (S));
               Push_Entity (U);
               S := AIN.Next_Node (S);
            end loop;
         end if;

         System_Partition_Identifier := System_Partition_Identifier + 1;
      end Visit_Virtual_Processor_Instance;

      ------------------------------
      -- Visit_Processor_Instance --
      ------------------------------

      procedure Visit_Processor_Instance (E : Node_Id) is
         N : Node_Id;
         S : Node_Id;
         K : Node_Id;
         L : List_Id;
         U : Node_Id;
         P : Node_Id;
         A : Node_Id;
      begin
         U := CTN.Naming_Node (Backend_Node (Identifier (E)));
         A := CTN.Deployment_Node (Backend_Node (Identifier (E)));
         P := CTN.Entity (U);

         Push_Entity (C_Root);
         Push_Entity (A);

         Kernel_Unit := U;

         Start_Recording_Handlings;

         Used_Buses := AINU.New_List (AIN.K_List_Id, No_Location);

         System_Partition_Identifier := 0;
         System_Stacks_Size          := 0;
         System_Nb_Threads           := 0;
         System_Nb_Partitions        := 0;
         System_Nb_Slots             := 0;
         System_Nb_Ports             := 0;
         System_Nb_Lock_Objects      := 0;
         Kernel_Use_Pci              := False;
         Kernel_Use_Io               := False;
         Kernel_Use_Gettick          := False;

         Kernel_Needs_Sched_Rms := False;
         Kernel_Needs_Sched_Edf := False;
         Kernel_Needs_Sched_Llf := False;
         Kernel_Needs_Sched_Rr  := False;

         Local_Port_List       := New_List (CTN.K_Enumeration_Literals);
         Local_Port_Identifier := 0;

         System_Partitions_Ports           := CTU.Make_Array_Values;
         System_Partitions_Size            := CTU.Make_Array_Values;
         System_Partitions_Scheduler       := CTU.Make_Array_Values;
         System_Partitions_Nb_Threads      := CTU.Make_Array_Values;
         System_Partitions_Nb_Lock_Objects := CTU.Make_Array_Values;
         System_Slots                      := CTU.Make_Array_Values;
         System_Slots_Allocation           := CTU.Make_Array_Values;
         System_Use_Queueing_Ports         := False;
         System_Use_Virtual_Ports          := False;
         System_Use_Sampling_Ports         := False;
         Major_Frame                       := No_Node;
         Kernel_Use_Error_Handling         := False;

         Pop_Entity;
         Push_Entity (P);
         Push_Entity (U);

         CTU.Set_Deployment_Header;

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         --
         --  If we are using DEOS or VxWorks, we do not need
         --  to write code to configure the kernel, this
         --  is done through the XML file of the deos_conf
         --  backend.
         --

         if POK_Flavor = DEOS or else POK_Flavor = VXWORKS then
            Reset_Handlings;

            Pop_Entity;
            Pop_Entity;
            Pop_Entity;
            return;
         end if;

         Runtime.Kernel_Mode;

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Local_Node),
              Value               =>
                CTU.Make_Literal (CV.New_Int_Value (Node_Identifier, 1, 10)));

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         --  Add the Local node identifier in the
         --  kernel/deployment.h file.

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Generated_Code),
              Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         --  Define the maccro POK_GENERATED_CODE so that the runtime
         --  is aware that we use generated code. Consequently,
         --  the runtime can restrict included functions in the binary.
         --  Here, we do that at the kernel level.

         N :=
           Message_Comment
             ("The POK_LOCAL_NODE macro corresponds to the identifier " &
              "for this node in the distributed system. This identifier " &
              "is unique for each node" &
              "In this case, this identifier was deduced from the" &
              Get_Name_String
                (Display_Name (Identifier (Parent_Subcomponent (E)))) &
              " processor component.");
         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           Make_Expression
             (CTU.Make_Defining_Identifier (Map_Node_Name (E)),
              Op_Equal,
              (Make_Literal (CV.New_Int_Value (Node_Identifier, 0, 10))));
         Append_Node_To_List (N, Node_List);

         Node_Identifier := Node_Identifier + 1;

         if Get_POK_Major_Frame (E) /= Null_Time then
            Major_Frame := Map_Time_To_Millisecond (Get_POK_Major_Frame (E));
         end if;

         if Get_POK_Slots (E) /= Empty_Time_Array then
            declare
               T : constant Time_Array := Get_POK_Slots (E);
            begin
               for I in T'Range loop
                  N := Map_Time_To_Millisecond (T (I));
                  CTU.Append_Node_To_List (N, CTN.Values (System_Slots));
                  System_Nb_Slots := System_Nb_Slots + 1;
               end loop;
            end;
         else
            Display_Error
              ("You must define partitions time slots",
               Fatal => True);
         end if;

         if POK_Flavor = POK and then Get_POK_Scheduler (E) /= Static then
            Display_Error
              ("POK systems must have a static scheduler",
               Fatal => True);
         end if;

         L := Get_POK_Slots_Allocation (E);
         if L /= No_List then
            S := ATN.First_Node (L);
            while Present (S) loop
               K := ATE.Get_Referenced_Entity (S);

               N := Get_Handling (K, By_Node, H_C_Deployment_Spec);

               if Present (N) then
                  CTU.Append_Node_To_List
                    (CTU.Copy_Node (N),
                     CTN.Values (System_Slots_Allocation));
               else
                  CTU.Append_Node_To_List
                    (Make_Literal (CV.New_Int_Value (0, 1, 10)),
                     CTN.Values (System_Slots_Allocation));
               end if;

               S := ATN.Next_Node (S);

            end loop;
         else
            Display_Error
              ("You must define partitions time slots allocation",
               Fatal => True);
         end if;

         if Kernel_Use_Error_Handling then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Error_Handling),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Kernel_Use_Pci then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Pci),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Kernel_Use_Io then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Io),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Kernel_Use_Gettick then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Gettick),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if System_Nb_Threads > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Threads),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if System_Nb_Partitions > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Partitions),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Sched),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

--         N := CTU.Make_Define_Statement
--               (Defining_Identifier => RE (RE_Pok_Needs_Debug),
--                  Value => CTU.Make_Literal
--                     (CV.New_Int_Value (1, 1, 10)));
--         CTU.Append_Node_To_List
--                  (N, CTN.Declarations (CTU.Current_File));

--         N := Message_Comment
--            ("The maccro POK_NEEDS_DEBUG "&
--             "activates debug in the kernel (not in libpok).");

--         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

--  Try to remove the debug mode, automatically added previously.

         N := Message_Comment ("#define POK_NEEDS_DEBUG 1");
         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           Message_Comment
             ("If you want to activate DEBUG mode, uncomment previous line");
         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         --  Define useful constants that configures the kernel : partition
         --  names, sizes and so on.

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Nb_Partitions),
              Value               =>
                CTU.Make_Literal
                  (CV.New_Int_Value (System_Nb_Partitions, 1, 10)));
         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           Message_Comment
             ("The maccro POK_CONFIG_NB_PARTITIONS " &
              "indicates the amount of partitions in the current system." &
              "It corresponds to the amount of process components" &
              "in the system.");

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         System_Nb_Threads := System_Nb_Threads + System_Nb_Partitions;

         --  We add one thread per partitions

         System_Nb_Threads := System_Nb_Threads + 2;

         --  We add the kernel and idle threads

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Nb_Threads),
              Value               =>
                CTU.Make_Literal
                  (CV.New_Int_Value (System_Nb_Threads, 1, 10)));

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           Message_Comment
             ("The maccro POK_CONFIG_NB_THREADS " &
              "indicates the amount of threads used in the kernel." &
              "It comprises the tasks for the partition and the main " &
              "task of each partition that initialize all ressources.");

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Partitions_Nthreads),
              Value               => System_Partitions_Nb_Threads);
         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           Message_Comment
             ("The maccro POK_CONFIG_NB_PARTITIONS_NTHREADS " &
              "indicates the amount of threads in each partition " &
              "we also add an additional process that initialize " &
              "all partition's entities (communication, threads, ...)");

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         --  Declare only used schedulers

         if Kernel_Needs_Sched_Rms then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Sched_Rms),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Kernel_Needs_Sched_Rr then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Sched_Rr),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Kernel_Needs_Sched_Llf then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Sched_Llf),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Kernel_Needs_Sched_Edf then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Sched_Edf),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if System_Nb_Lock_Objects > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Lockobjects),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Nb_Lockobjects),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Int_Value (System_Nb_Lock_Objects, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Config_Partitions_Nlockobjects),
                 Value => System_Partitions_Nb_Lock_Objects);

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              Message_Comment
                ("The maccro POK_CONFIG_NB_PARTITIONS_NLOCKOBJECTS " &
                 "indicates the amount of lockobjects in each partition ");

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Partitions_Size),
              Value               => System_Partitions_Size);

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Partitions_Scheduler),
              Value               => System_Partitions_Scheduler);

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         N :=
           Message_Comment
             ("The maccro POK_CONFIG_PARTTITIONS_SIZE " &
              "indicates the required amount of memory for each partition." &
              "This value was deduced from the property " &
              "POK::Needed_Memory_Size of each process");

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         if not CTU.Is_Empty (CTN.Values (System_Slots)) then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Scheduling_Slots),
                 Value               => System_Slots);

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if not CTU.Is_Empty (CTN.Values (System_Slots_Allocation)) then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Config_Scheduling_Slots_Allocation),
                 Value => System_Slots_Allocation);

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Scheduling_Nbslots),
              Value               =>
                CTU.Make_Literal (CV.New_Int_Value (System_Nb_Slots, 1, 10)));

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         if Major_Frame /= No_Node then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Config_Scheduling_Major_Frame),
                 Value => Major_Frame);

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if System_Use_Sampling_Ports then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Ports_Sampling),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
            N :=
              Message_Comment
                ("The maccro POK_NEEDS_PORTS_SAMPLING indicates " &
                 "that we need sampling ports facilities in POK." &
                 "It also means that we have intra-partition " &
                 "communication between several processes through " &
                 "event data port.");
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if System_Use_Queueing_Ports then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Ports_Queueing),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
            N :=
              Message_Comment
                ("The maccro POK_NEEDS_PORTS_QUEUEING indicates " &
                 "that we need queueing ports facilities in POK." &
                 "It also means that we have inter-partition " &
                 "communication between several processes through " &
                 "event data port.");
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if System_Use_Virtual_Ports then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Ports_Virtual),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
            N :=
              Message_Comment
                ("The maccro POK_NEEDS_PORTS_VIRTUAL indicates " &
                 "that we need virtual ports facilities in POK." &
                 "It also means that we have inter-partition " &
                 "communication between several processes through " &
                 "[event] data port. Virtual ports are uses " &
                 "for device drivers interfacing");
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Stacks_Size),
              Value               =>
                CTU.Make_Literal
                  (CV.New_Int_Value (System_Stacks_Size, 1, 10)));
         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         if System_Nb_Ports > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Partitions_Ports),
                 Value               => System_Partitions_Ports);

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Nb_Ports),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Int_Value (System_Nb_Ports, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Nb_Global_Ports),
                 Value               => Global_Nb_Ports_Node);

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              Message_Comment
                ("The maccro POK_CONFIG_NB_PORTS represent the amount " &
                 "of inter-partition communication in the system." &
                 "Sampling and Queueing ports are counted.");
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            Set_Str_To_Name_Buffer ("invalid_local_port");
            N :=
              Make_Expression
                (CTU.Make_Defining_Identifier (Name_Find),
                 Op_Equal,
                 (Make_Literal
                    (CV.New_Int_Value (Local_Port_Identifier, 0, 10))));
            Append_Node_To_List (N, Local_Port_List);

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Pok_Port_Local_Identifier_T),
                 Type_Definition     => Make_Enum_Aggregate (Local_Port_List));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Pok_Node_Identifier_T),
                 Type_Definition     => Make_Enum_Aggregate (Node_List));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Pok_Port_Identifier_T),
                 Type_Definition => Make_Enum_Aggregate (Global_Port_List));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Message_Comment
                ("This enumeration describes all ports on the current " &
                 "nodes. It is used by the configuration arrays in " &
                 "the generated file deployment.c.");
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         declare
            Bus_Id   : Unsigned_Long_Long := 0;
            Bus_List : constant List_Id   :=
              New_List (CTN.K_Enumeration_Literals);
         begin
            S := AIN.First_Node (Used_Buses);

            while Present (S) loop
               N :=
                 Make_Expression
                   (CTU.Make_Defining_Identifier (Map_Bus_Name (Item (S))),
                    Op_Equal,
                    (Make_Literal (CV.New_Int_Value (Bus_Id, 0, 10))));
               Append_Node_To_List (N, Bus_List);

               Bus_Id := Bus_Id + 1;

               S := AIN.Next_Node (S);
            end loop;

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Nb_Buses),
                 Value => CTU.Make_Literal (CV.New_Int_Value (Bus_Id, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              Make_Expression
                (CTU.Make_Defining_Identifier
                   (Get_String_Name ("invalid_bus")),
                 Op_Equal,
                 (Make_Literal (CV.New_Int_Value (Bus_Id, 0, 10))));
            Append_Node_To_List (N, Bus_List);

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier => RE (RE_Pok_Bus_Identifier_T),
                 Type_Definition     => Make_Enum_Aggregate (Bus_List));
            Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end;

         Reset_Handlings;

         CTN.Set_Value
           (Global_Nb_Ports_Node,
            CV.New_Int_Value (Global_Port_Identifier, 1, 10));
         --  Update the value of the number of global ports,
         --  taking in accounts new procesed ports.

         Runtime.User_Mode;
         --  Fallback in user mode now, since we dont need
         --  functionnalities dedicated to kernelland.

         Pop_Entity;
         Pop_Entity;
         Pop_Entity;
      end Visit_Processor_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S                  : Node_Id;
         Component_Instance : Node_Id;
      begin
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               Component_Instance := Corresponding_Instance (S);
               if AINU.Is_Processor (Component_Instance) then
                  Visit (Component_Instance);
               end if;
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_System_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
        (E                    : Node_Id;
         Real_Process         : Boolean := True;
         Associated_Component : Node_Id := No_Node)
      is
         C                       : Node_Id;
         N                       : Node_Id;
         O                       : Node_Id;
         F                       : Node_Id;
         U                       : Node_Id;
         P                       : Node_Id;
         Bound_Processor         : Node_Id;
         Communicating_Component : Node_Id;
         Property_Memory_Size    : Unsigned_Long_Long := 0;
         Bound_Memory_Component  : Node_Id;
         Bound_Memory_Size       : Unsigned_Long_Long := 0;
         Used_Memory_Size        : Unsigned_Long_Long := 0;
         Additional_Features     : List_Id;
      begin
         if Real_Process then
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);
            CTU.Push_Entity (P);
            CTU.Push_Entity (U);
            Communicating_Component := E;
         else
            Communicating_Component := Associated_Component;
            U                       :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node
                   (Backend_Node (Identifier (Communicating_Component))));
            P := CTN.Entity (U);
         end if;

         CTU.Set_Deployment_Header;

         Partition_Stacks_Size := 0;

         if Real_Process then
            Bound_Processor := Get_Bound_Processor (E);
         else
            Bound_Processor := Get_Bound_Processor (Associated_Component);
         end if;

         --
         --  Add a maccro to specify on which platform we are deploying
         --  the code. This can be used to make some adjustment in the
         --  user code.
         --

         case POK_Flavor is
            when DEOS =>
               N :=
                  CTU.Make_Define_Statement
                     (Defining_Identifier => RE (RE_Ocarina_Runtime_Deos),
                     Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

               CTU.Append_Node_To_List
                  (N, CTN.Declarations (CTU.Current_File));
            when Vxworks =>
               N :=
                  CTU.Make_Define_Statement
                     (Defining_Identifier =>
                        RE (RE_Ocarina_Runtime_Vxworks653),
                     Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

               CTU.Append_Node_To_List
                  (N, CTN.Declarations (CTU.Current_File));

            when POK | Arinc653 =>
               N :=
                  CTU.Make_Define_Statement
                     (Defining_Identifier =>
                        RE (RE_Ocarina_Runtime_Pok),
                     Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

               CTU.Append_Node_To_List
                  (N, CTN.Declarations (CTU.Current_File));
         end case;

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Generated_Code),
              Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         --  Define the maccro POK_GENERATED_CODE so that the runtime
         --  is aware that we use generated code. Consequently,
         --  the runtime can restrict included functions in the binary.
         --  Here, we do that at the partition level.

         if Is_Defined_Property
             (Bound_Processor,
              "pok::additional_features")
         then
            Additional_Features :=
              ATN.Multi_Value
                (AIN.Property_Association_Value
                   (AIEP.Find_Property_Association_From_Name
                      (Property_List => AIN.Properties (Bound_Processor),
                       Property_Name =>
                         Get_String_Name ("pok::additional_features"),
                       In_Mode => No_Name)));

            if not ATNU.Is_Empty (Additional_Features) then
               declare
                  S        : Node_Id;
                  Tmp_Name : Name_Id;
               begin
                  S := ATN.First_Node (Additional_Features);
                  while Present (S) loop
                     Set_Str_To_Name_Buffer ("pok_needs_");
                     Get_Name_String_And_Append
                       (ATN.Name (ATN.Identifier (S)));

                     Tmp_Name := To_Upper (Name_Find);

                     N :=
                       CTU.Make_Define_Statement
                         (Defining_Identifier =>
                            Make_Defining_Identifier
                              (Tmp_Name,
                               C_Conversion => False),
                          Value =>
                            CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
                     CTU.Append_Node_To_List
                       (N,
                        CTN.Declarations (CTU.Current_File));

                     if Get_Name_String (ATN.Name (ATN.Identifier (S))) =
                       "console"
                     then
                        CTU.Pop_Entity;
                        CTU.Pop_Entity;

                        Push_Entity (Kernel_Unit);
                        Set_Deployment_Header;

                        N :=
                          CTU.Make_Define_Statement
                            (Defining_Identifier => RE (RE_Pok_Needs_Console),
                             Value               =>
                               CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
                        CTU.Append_Node_To_List
                          (N,
                           CTN.Declarations (CTU.Current_File));

                        Pop_Entity;

                        CTU.Push_Entity (P);
                        CTU.Push_Entity (U);

                        Set_Deployment_Header;
                     end if;

                     if Get_Name_String (ATN.Name (ATN.Identifier (S))) =
                       "x86_vmm"
                     then
                        CTU.Pop_Entity;
                        CTU.Pop_Entity;

                        Push_Entity (Kernel_Unit);
                        Set_Deployment_Header;

                        N :=
                          CTU.Make_Define_Statement
                            (Defining_Identifier => RE (RE_Pok_Needs_X86_Vmm),
                             Value               =>
                               CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
                        CTU.Append_Node_To_List
                          (N,
                           CTN.Declarations (CTU.Current_File));

                        Pop_Entity;

                        CTU.Push_Entity (P);
                        CTU.Push_Entity (U);

                        Set_Deployment_Header;
                     end if;

                     S := ATN.Next_Node (S);
                  end loop;
               end;
            end if;
         end if;

         Partition_Use_Error_Handling := False;

         Process_Nb_Threads      := 0;
         Process_Nb_Buffers      := 0;
         Process_Nb_Events       := 0;
         Process_Nb_Lock_Objects := 0;
         Process_Nb_Blackboards  := 0;
         System_Nb_Partitions    := System_Nb_Partitions + 1;

         --  The Needed_Memory_Size is required, it must be
         --  declared in the model. It defines the allocated
         --  size for each partition.

         Property_Memory_Size := Get_Needed_Memory_Size (E);

         if Real_Process then
            Bound_Memory_Component := Get_Bound_Memory (E);
         else
            Bound_Memory_Component := Get_Bound_Memory (Associated_Component);
         end if;

         if Bound_Memory_Component /= No_Node then
            declare
               Byte_Count : Unsigned_Long_Long := 1;
               Word_Size  : Unsigned_Long_Long := 1;
            begin
               if Get_Byte_Count (Bound_Memory_Component) /= 0 then
                  Byte_Count := Get_Byte_Count (Bound_Memory_Component);
               end if;

               if Get_Word_Size (Bound_Memory_Component) /= Null_Size then
                  Word_Size :=
                    To_Bytes (Get_Word_Size (Bound_Memory_Component));
               end if;
               Bound_Memory_Size := Byte_Count * Word_Size;

               --  byte_count override other properties.
               if Is_Defined_Property
                   (Bound_Memory_Component,
                    "byte_count")
               then
                  Bound_Memory_Size :=
                    Get_Integer_Property
                      (Bound_Memory_Component,
                       "byte_count");
               end if;
            end;
         end if;

         if Bound_Memory_Size /= 0 then
            Used_Memory_Size := Bound_Memory_Size;
         elsif Property_Memory_Size /= 0 then
            Used_Memory_Size := Property_Memory_Size;
         else
            Display_Located_Error
              (Loc (E),
               "You must provide the size of the parttion",
               Fatal => True);
         end if;

         if Real_Process then
            Communicating_Component := E;
         else
            Communicating_Component := Associated_Component;
         end if;

         if not AINU.Is_Empty (Features (Communicating_Component)) then
            F := First_Node (Features (Communicating_Component));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Get_Connection_Pattern (F) = Inter_Process
               then

                  if Is_Defined_Property
                      (F,
                       "allowed_connection_binding_class")
                  then
                     N :=
                       CTU.Make_Define_Statement
                         (Defining_Identifier => RE (RE_Pok_Needs_Protocols),
                          Value               =>
                            CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
                     CTU.Append_Node_To_List
                       (N,
                        CTN.Declarations (CTU.Current_File));

                     N :=
                       CTU.Make_Define_Statement
                         (Defining_Identifier => RE (RE_Pok_Needs_Libc_String),
                          Value               =>
                            CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
                     CTU.Append_Node_To_List
                       (N,
                        CTN.Declarations (CTU.Current_File));

                     Handle_Virtual_Buses_Properties (F);

                     Set_Activity_Source;
                     Add_Include (RH (RH_Protocols));
                     Set_Deployment_Header;
                  end if;

                  AINU.Append_Node_To_List
                    (AINU.Make_Node_Container (F),
                     All_Ports);

                  Handle_Buses_Connections (F);

                  N :=
                    Make_Expression
                      (CTU.Make_Defining_Identifier (Map_Port_Name (F)),
                       Op_Equal,
                       (Make_Literal
                          (CV.New_Int_Value (Local_Port_Identifier, 0, 10))));
                  Append_Node_To_List (N, Local_Port_List);

                  N :=
                    Make_Expression
                      (CTU.Make_Defining_Identifier (Map_Port_Name (F, True)),
                       Op_Equal,
                       (Make_Literal
                          (CV.New_Int_Value (Global_Port_Identifier, 0, 10))));
                  Append_Node_To_List (N, Global_Port_List);

                  CTU.Append_Node_To_List
                    (CTU.Copy_Node
                       (Get_Handling
                          (Bound_Processor,
                           By_Node,
                           H_C_Deployment_Spec)),
                     CTN.Values (System_Partitions_Ports));
                  --  Declare which partition would use this port.

                  Local_Port_Identifier  := Local_Port_Identifier + 1;
                  Global_Port_Identifier := Global_Port_Identifier + 1;

                  CTU.Append_Node_To_List
                    (Make_Literal (CV.New_Int_Value (Node_Identifier, 0, 10)),
                     CTN.Values (Array_Global_Ports_Nodes));

                  if not AIN.Is_Event (F) and then AIN.Is_Data (F) then
                     System_Use_Sampling_Ports := True;
                  elsif AIN.Is_Data (F) and then AIN.Is_Event (F) then
                     System_Use_Queueing_Ports := True;
                  end if;

                  System_Nb_Ports := System_Nb_Ports + 1;
               end if;
               F := Next_Node (F);
            end loop;
         end if;

         --  Handle subcomponents.

         if not AINU.Is_Empty (Subcomponents (E)) then
            C := First_Node (Subcomponents (E));

            while Present (C) loop
               --  If we have a protected data, we create semaphore variables.
               if AINU.Is_Data (Corresponding_Instance (C))
                 and then Is_Protected_Data (Corresponding_Instance (C))
               then

                  --  If we have a protected shared data, we declare
                  --  a new semaphore in the kernel and in the current
                  --  partitions. This dimension resources of kernel
                  --  and partition.

                  System_Nb_Lock_Objects  := System_Nb_Lock_Objects + 1;
                  Process_Nb_Lock_Objects := Process_Nb_Lock_Objects + 1;
               else
                  Visit (Corresponding_Instance (C));
               end if;
               C := Next_Node (C);
            end loop;
         end if;
         --  Append the partition name and its size to a global

         --  array that will configure the kernel

         O := CTU.Make_Literal (CV.New_Int_Value (Used_Memory_Size, 1, 10));

         CTU.Append_Node_To_List (O, CTN.Values (System_Partitions_Size));

         if Partition_Use_Error_Handling then
            Process_Nb_Threads := Process_Nb_Threads + 1;
            System_Nb_Threads  := System_Nb_Threads + 1;
            --  If we have error handling in this partition, we need
            --  to add one thread in the partitions so we declare
            --  an additional thread in the partition and in the kernel.
         end if;

         Process_Nb_Threads := Process_Nb_Threads + 1;

         CTU.Append_Node_To_List
           (CTU.Make_Literal (CV.New_Int_Value (Process_Nb_Threads, 1, 10)),
            CTN.Values (System_Partitions_Nb_Threads));

         CTU.Append_Node_To_List
           (CTU.Make_Literal
              (CV.New_Int_Value (Process_Nb_Lock_Objects, 1, 10)),
            CTN.Values (System_Partitions_Nb_Lock_Objects));

         --  Finally, declare the amount of threads inside the partition

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Nb_Threads),
              Value               =>
                CTU.Make_Literal
                  (CV.New_Int_Value (Process_Nb_Threads, 1, 10)));

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         if Process_Nb_Buffers > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Nb_Buffers),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Int_Value (Process_Nb_Buffers, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Buffers),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Process_Nb_Events > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Nb_Events),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Int_Value (Process_Nb_Events, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Events),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         --  Define NB_BLACKBOARDS or NB_BUFFERS directive in the deployment.h
         --  of each partition.

         if Process_Nb_Blackboards > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_Nb_Blackboards),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Int_Value (Process_Nb_Blackboards, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Blackboards),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         --  Define POK_NEEDS_MIDDLEWARE in each partition

         if Process_Nb_Blackboards > 0 or else Process_Nb_Buffers > 0 then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Needs_Middleware),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Stacks_Size),
              Value               =>
                CTU.Make_Literal
                  (CV.New_Int_Value (Partition_Stacks_Size, 1, 10)));
         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         if Real_Process then
            CTU.Pop_Entity;
            CTU.Pop_Entity;
         end if;
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         N : Node_Id;
         F : Node_Id;
         S : Node_Id;
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P              : constant Node_Id := CTN.Entity (U);
         Implementation : Node_Id;
      begin
         CTU.Push_Entity (P);
         CTU.Push_Entity (U);

         CTU.Set_Deployment_Header;

         Current_Device := E;

         if Is_Defined_Property (E, "pok::hw_addr") then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Hw_Addr),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Pointed_Char_Value
                        (Get_String_Property (E, "pok::hw_addr"))));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Is_Defined_Property (E, "pok::pci_device_id") then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Pci_Device_Id),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Pointed_Char_Value
                        (Get_String_Property (E, "pok::pci_device_id"))));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Is_Defined_Property (E, "pok::device_name") then
            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   Map_Needs_Macro
                     (Get_String_Property (E, "pok::device_name")),
                 Value =>
                   CTU.Make_Literal
                     (CV.New_Int_Value (Process_Nb_Threads, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Is_Defined_Property (E, "pok::pci_vendor_id") then
            Kernel_Use_Pci := True;
            Kernel_Use_Io  := True;
            N              :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Pci_Vendor_Id),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Pointed_Char_Value
                        (Get_String_Property (E, "pok::pci_vendor_id"))));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Get_String_Property (E, "pok::device_name") =
           Get_String_Name ("rtl8029")
         then
            Kernel_Use_Pci := True;
            Kernel_Use_Io  := True;
         end if;

         if not AINU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Get_Connection_Pattern (F) = Inter_Process
                 and then Is_Virtual (Get_Port_By_Name (F, Current_Device))
               then

                  System_Use_Virtual_Ports := True;
               end if;

               if Kind (F) = K_Subcomponent_Access_Instance
                 and then AINU.Is_Bus (Corresponding_Instance (F))
               then
                  declare
                     Accessed     : List_Id;
                     Accessed_Bus : Node_Id;
                     D            : Node_Id;
                  begin
                     if not AINU.Is_Empty (Sources (F)) then
                        D := First_Node (Sources (F));
                        while Present (D) loop
                           Accessed_Bus := Item (D);
                           Accessed     :=
                             CTN.Processes
                               (Backend_Node (Identifier (Accessed_Bus)));
                           AINU.Append_Node_To_List
                             (AINU.Make_Node_Container (E),
                              Accessed);

                           D := Next_Node (D);
                        end loop;
                     end if;
                  end;
               end if;
               F := Next_Node (F);
            end loop;
         end if;

         Implementation := Get_Classifier_Property (E, "implemented_as");

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  if Get_Category_Of_Component (S) = CC_Process then
                     Visit_Process_Instance
                       (Corresponding_Instance (S),
                        False,
                        E);
                  else
                     Visit (Corresponding_Instance (S));
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;

         end if;

         Current_Device := No_Node;

         CTU.Pop_Entity;
         CTU.Pop_Entity;
      end Visit_Device_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         F : Node_Id;
      begin
         System_Nb_Threads  := System_Nb_Threads + 1;
         Process_Nb_Threads := Process_Nb_Threads + 1;

         if Get_POK_Recovery_Errors (E) /= POK_Empty_Handled_Errors
           or else Get_ARINC653_HM_Errors (E) /= ARINC653_Empty_Errors
         then
            Kernel_Use_Error_Handling    := True;
            Partition_Use_Error_Handling := True;
         end if;

         if Get_Thread_Stack_Size (E) /= Null_Size then
            Partition_Stacks_Size :=
              Partition_Stacks_Size + To_Bytes (Get_Thread_Stack_Size (E));
            System_Stacks_Size :=
              System_Stacks_Size + To_Bytes (Get_Thread_Stack_Size (E));
         else
            Partition_Stacks_Size := Partition_Stacks_Size + 8192;
            System_Stacks_Size    := System_Stacks_Size + 8192;
         end if;

         --  Fetch ports and declare correct number of intra-partitions
         --  elements (buffers, blackboards and so on). Crappy hack :
         --  we only handle out port since it permit us to handle
         --  the whole connection in the process.

         if not AINU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  Kernel_Use_Gettick := True;
                  if Get_Connection_Pattern (F) = Intra_Process
                    and then Get_Port_By_Name (F, Current_Device) = No_Node
                    and then Is_In (F)
                  then
                     if AIN.Is_Data (F) and then not AIN.Is_Event (F) then
                        Process_Nb_Blackboards := Process_Nb_Blackboards + 1;
                     elsif AIN.Is_Data (F) and then AIN.Is_Event (F) then
                        Process_Nb_Buffers := Process_Nb_Buffers + 1;
                     elsif AIN.Is_Event (F) and then not AIN.Is_Data (F) then
                        Process_Nb_Events := Process_Nb_Events + 1;
                     else
                        Display_Communication_Error;
                     end if;

                     Process_Nb_Lock_Objects := Process_Nb_Lock_Objects + 1;
                     System_Nb_Lock_Objects  := System_Nb_Lock_Objects + 1;
                  end if;
               end if;
               F := Next_Node (F);
            end loop;
         end if;

      end Visit_Thread_Instance;

   end Header_File;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is
      Process_Buffers       : Node_Id;
      Process_Blackboards   : Node_Id;
      Nb_Ports_By_Partition : Node_Id;
      Ports_By_Partition    : Node_Id;
      Ports_Names           : Node_Id;

      Partition_ARINC653_Semaphores_Names : Node_Id;
      Partition_ARINC653_Events_Names     : Node_Id;
      --  Name of semaphores for ARINC653 flavor.

      Partition_ARINC653_Nb_Semaphores : Unsigned_Long_Long := 0;
      Partition_ARINC653_Nb_Events     : Unsigned_Long_Long := 0;
      --  Number of semaphores inside a partition.

      Ports_Identifiers                      : Node_Id;
      Nb_Destinations                        : Node_Id;
      Ports_Destinations                     : Node_Id;
      Ports_Kind                             : Node_Id;
      Kernel_Error_Handling                  : Boolean            := False;
      Partition_Error_Handling               : Boolean            := False;
      Partition_Error_Callback               : Boolean            := False;
      Nports                                 : Unsigned_Long_Long := 0;
      Kernel_Error_Switch_Alternatives       : List_Id;
      Partition_Error_Switch_Alternatives    : List_Id;
      Partitions_Error_Callback_Alternatives : List_Id;
      Partition_Id                           : Unsigned_Long_Long := 0;
      Kernel_Unit                            : Node_Id;

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
        (E                       : Node_Id;
         Real_Process            : Boolean := True;
         Corresponding_Component : Node_Id := No_Node);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);

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
         Category : constant Component_Category :=
           AIE.Get_Category_Of_Component (E);
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

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Device =>
               Visit_Device_Instance (E);

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

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
        (E                       : Node_Id;
         Real_Process            : Boolean := True;
         Corresponding_Component : Node_Id := No_Node)
      is
         S                       : Node_Id;
         N                       : Node_Id;
         F                       : Node_Id;
         U                       : Node_Id;
         P                       : Node_Id;
         Nb_Ports_Partition      : Unsigned_Long_Long;
         Nb_Ports_Destinations   : Unsigned_Long_Long;
         Q                       : Node_Id;
         T                       : Node_Id;
         O                       : Node_Id;
         Communicating_Component : Node_Id;
      begin

         if Real_Process then
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);

            CTU.Push_Entity (P);
            CTU.Push_Entity (U);
            Communicating_Component := E;
         else
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node
                   (Backend_Node (Identifier (Corresponding_Component))));
            P := CTN.Entity (U);

            Communicating_Component := Corresponding_Component;
         end if;

         CTU.Set_Deployment_Source;

         T := CTU.Make_Array_Values;

         Process_Buffers := CTU.Make_Array_Values;

         Process_Blackboards := CTU.Make_Array_Values;

         Partition_ARINC653_Semaphores_Names := CTU.Make_Array_Values;
         Partition_ARINC653_Events_Names     := CTU.Make_Array_Values;

         Partition_ARINC653_Nb_Semaphores := 0;
         Partition_ARINC653_Nb_Events     := 0;

         Nb_Ports_Partition := 0;

         --  Handle threads (most of subcomponents).

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         --  Handle shared data

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  If we have a protected data, we create semaphore variables.
               if AINU.Is_Data (Corresponding_Instance (S))
                 and then Is_Protected_Data (Corresponding_Instance (S))
                 and then POK_Flavor = ARINC653
               then

                  --  If we use the ARINC653 flavor, we must configure
                  --  the ARINC653 layer to handle different semaphores
                  --  names.

                  Append_Node_To_List
                    (CTU.Make_Literal
                       (CV.New_Pointed_Char_Value
                          (Map_Associated_Locking_Entity_Name (S))),
                     CTN.Values (Partition_ARINC653_Semaphores_Names));

                  Partition_ARINC653_Nb_Semaphores :=
                    Partition_ARINC653_Nb_Semaphores + 1;

               end if;
               S := Next_Node (S);
            end loop;
         end if;

         --  Add the table pok_buffers_names[POK_CONFIG_NB_BUFFERS] = {...}
         --  This variable contains the names of the buffers.

         if not CTU.Is_Empty (CTN.Values (Process_Buffers)) then
            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Pok_Buffers_Names),
                           Array_Size => RE (RE_Pok_Config_Nb_Buffers)),
                      Used_Type => CTU.Make_Pointer_Type (RE (RE_Char))),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Process_Buffers);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if Partition_ARINC653_Nb_Semaphores > 0 then
            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_ARINC653_Semaphores_Names),
                           Array_Size =>
                             RE (RE_Pok_Config_ARINC653_Nb_Semaphores)),
                      Used_Type => CTU.Make_Pointer_Type (RE (RE_Char))),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Partition_ARINC653_Semaphores_Names);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Config_ARINC653_Nb_Semaphores),
                 Value =>
                   CTU.Make_Literal
                     (CV.New_Int_Value
                        (Partition_ARINC653_Nb_Semaphores,
                         1,
                         10)));
            Set_Deployment_Header;
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
            Set_Deployment_Source;
         end if;

         if Partition_ARINC653_Nb_Events > 0 then
            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_ARINC653_Events_Names),
                           Array_Size =>
                             RE (RE_Pok_Config_ARINC653_Nb_Events)),
                      Used_Type => CTU.Make_Pointer_Type (RE (RE_Char))),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Partition_ARINC653_Events_Names);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier => RE (RE_Pok_Config_ARINC653_Nb_Events),
                 Value               =>
                   CTU.Make_Literal
                     (CV.New_Int_Value (Partition_ARINC653_Nb_Events, 1, 10)));
            Set_Deployment_Header;
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
            Set_Deployment_Source;
         end if;

         --  If there is at least one blackboard for intra-partition
         --  communication in this partition, we declare its name
         --  in the pok_blackboards_names variable.

         if not CTU.Is_Empty (CTN.Values (Process_Blackboards)) then
            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Blackboards_Names),
                           Array_Size => RE (RE_Pok_Config_Nb_Blackboards)),
                      Used_Type => CTU.Make_Pointer_Type (RE (RE_Char))),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Process_Blackboards);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         if not AINU.Is_Empty (Features (Communicating_Component)) then
            F := First_Node (Features (Communicating_Component));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Get_Connection_Pattern (F) = Inter_Process
               then

                  --  Add the port to the list of ports available
                  --  inside the partition

                  CTU.Append_Node_To_List
                    (CTU.Make_Defining_Identifier (Map_Port_Name (F)),
                     CTN.Values (T));

                  CTU.Append_Node_To_List
                    (CTU.Make_Defining_Identifier (Map_Port_Name (F, True)),
                     CTN.Values (Array_Local_Ports_To_Global_Ports));

                  --  Add the name of the port in the variable
                  --  pok_ports_names

                  N :=
                    CTU.Make_Literal
                      (CV.New_Pointed_Char_Value (Map_Port_Name (F)));
                  CTU.Append_Node_To_List (N, CTN.Values (Ports_Names));

                  --  Add the identifier of the port
                  N := CTU.Make_Defining_Identifier (Map_Port_Name (F));
                  CTU.Append_Node_To_List (N, CTN.Values (Ports_Identifiers));

                  --  Add the kind of the port

                  if Is_Virtual (Get_Port_By_Name (F, Current_Device)) then
                     CTU.Append_Node_To_List
                       (RE (RE_Pok_Port_Kind_Virtual),
                        CTN.Values (Ports_Kind));
                  elsif AIN.Is_Data (F) and then not AIN.Is_Event (F) then
                     CTU.Append_Node_To_List
                       (RE (RE_Pok_Port_Kind_Sampling),
                        CTN.Values (Ports_Kind));
                  elsif AIN.Is_Event (F) and then AIN.Is_Data (F) then
                     CTU.Append_Node_To_List
                       (RE (RE_Pok_Port_Kind_Queueing),
                        CTN.Values (Ports_Kind));
                  end if;

                  --  If you have an out port, we add its destinations
                  --  in the deployment.c file
                  Nb_Ports_Destinations := 0;

                  if AIN.Is_Out (F) then
                     Q := CTU.Make_Array_Values;
                     O := First_Node (Destinations (F));
                     while Present (O) loop
                        CTU.Append_Node_To_List
                          (CTU.Make_Defining_Identifier
                             (Map_Port_Name (Item (O), True)),
                           CTN.Values (Q));
                        Nb_Ports_Destinations := Nb_Ports_Destinations + 1;
                        O                     := Next_Node (O);
                     end loop;
                     CTU.Append_Node_To_List
                       (CTU.Make_Defining_Identifier
                          (Map_Port_Deployment_Destinations (F)),
                        CTN.Values (Ports_Destinations));

                     --  Add an array which contain the destinations
                     --  of the port if needed.

                     N :=
                       CTU.Make_Variable_Declaration
                         (Defining_Identifier =>
                            CTU.Make_Array_Declaration
                              (Defining_Identifier =>
                                 CTU.Make_Defining_Identifier
                                   (Map_Port_Deployment_Destinations (F)),
                               Array_Size =>
                                 CTU.Make_Literal
                                   (CV.New_Int_Value
                                      (Nb_Ports_Destinations,
                                       1,
                                       10))),
                          Used_Type => RE (RE_Uint8_T));
                     N :=
                       CTU.Make_Expression
                         (Left_Expr  => N,
                          Operator   => CTU.Op_Equal,
                          Right_Expr => Q);

                     --  Add new declaration inside the deployment.c
                     --  inside the kernel directory.

                     CTU.Pop_Entity;
                     CTU.Pop_Entity;

                     Push_Entity (Kernel_Unit);
                     Set_Deployment_Source;
                     CTU.Append_Node_To_List
                       (N,
                        CTN.Declarations (CTU.Current_File));
                     Pop_Entity;

                     CTU.Push_Entity (P);
                     CTU.Push_Entity (U);

                  else
                     CTU.Append_Node_To_List
                       (RE (RE_Null),
                        CTN.Values (Ports_Destinations));
                  end if;

                  --  Add the amount of destinations
                  --  FIXME we fix the destination to one at this moment

                  N :=
                    CTU.Make_Literal
                      (CV.New_Int_Value (Nb_Ports_Destinations, 1, 10));
                  CTU.Append_Node_To_List (N, CTN.Values (Nb_Destinations));

                  Nports             := Nports + 1;
                  Nb_Ports_Partition := Nb_Ports_Partition + 1;
               end if;
               F := Next_Node (F);
            end loop;
         end if;

         if Has_Ports (Communicating_Component) or else Has_Ports (E) then
            N :=
              CTU.Make_Variable_Declaration
                (Defining_Identifier =>
                   CTU.Make_Array_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Defining_Identifier
                          (Map_Port_Deployment_Partition (E)),
                      Array_Size =>
                        CTU.Make_Literal
                          (CV.New_Int_Value (Nb_Ports_Partition, 1, 10))),
                 Used_Type => RE (RE_Uint8_T));

            N :=
              CTU.Make_Expression
                (Left_Expr  => N,
                 Operator   => CTU.Op_Equal,
                 Right_Expr => T);

            --  Add new declaration inside the deployment.c
            --  inside the kernel directory.

            CTU.Pop_Entity;
            CTU.Pop_Entity;

            CTU.Push_Entity (Kernel_Unit);
            Set_Deployment_Source;
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
            CTU.Pop_Entity;

            CTU.Push_Entity (P);
            CTU.Push_Entity (U);

            CTU.Append_Node_To_List
              (CTU.Make_Defining_Identifier
                 (Map_Port_Deployment_Partition (E)),
               CTN.Values (Ports_By_Partition));
         else
            CTU.Append_Node_To_List
              (RE (RE_Null),
               CTN.Values (Ports_By_Partition));
         end if;

         --  Now, we know precisely how many ports are available for each
         --  partition. So we can fill the pok_ports_nb_ports_by_partition
         --  array.

         N := CTU.Make_Literal (CV.New_Int_Value (Nb_Ports_Partition, 1, 10));
         CTU.Append_Node_To_List (N, CTN.Values (Nb_Ports_By_Partition));

         if Real_Process then
            CTU.Pop_Entity;
            CTU.Pop_Entity;
         end if;
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Subcomponent : Node_Id;
         F            : Node_Id;
         N            : Node_Id;
         Type_Used    : Node_Id;
      begin
         if not AINU.Is_Empty (Subcomponents (E)) then
            Subcomponent := First_Node (Subcomponents (E));
            while Present (Subcomponent) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (Subcomponent));
               Subcomponent := Next_Node (Subcomponent);
            end loop;
         end if;

         if not AINU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            --  For each port in the thread that represent an intra partition
            --  communication, we add the port name to the array
            --  pok_buffers_names or pok_blackboards_names.
            --  In addition, we create a variable that will contain the
            --  identifier of the buffer/blackboard.

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then Get_Port_By_Name (F, Current_Device) = No_Node
                 and then Get_Connection_Pattern (F) = Intra_Process
               then
                  if AIN.Is_Data (F) and then not AIN.Is_Event (F) then
                     if Use_ARINC653_API then
                        Type_Used := RE (RE_Blackboard_Id_Type);
                     else
                        Type_Used := RE (RE_Uint8_T);
                     end if;

                     N :=
                       CTU.Make_Literal
                         (CV.New_Pointed_Char_Value (Map_Port_Name (F)));
                     CTU.Append_Node_To_List
                       (N,
                        CTN.Values (Process_Blackboards));

                     N :=
                       CTU.Make_Variable_Declaration
                         (Defining_Identifier =>
                            CTU.Make_Defining_Identifier (Map_Port_Var (F)),
                          Used_Type => Type_Used);

                     CTU.Append_Node_To_List
                       (N,
                        CTN.Declarations (CTU.Current_File));
                  elsif AIN.Is_Data (F) and then AIN.Is_Event (F) then
                     if Use_ARINC653_API then
                        Type_Used := RE (RE_Buffer_Id_Type);
                     else
                        Type_Used := RE (RE_Uint8_T);
                     end if;

                     N :=
                       CTU.Make_Literal
                         (CV.New_Pointed_Char_Value (Map_Port_Name (F)));
                     CTU.Append_Node_To_List (N, CTN.Values (Process_Buffers));

                     N :=
                       CTU.Make_Variable_Declaration
                         (Defining_Identifier =>
                            CTU.Make_Defining_Identifier (Map_Port_Var (F)),
                          Used_Type => Type_Used);

                     CTU.Append_Node_To_List
                       (N,
                        CTN.Declarations (CTU.Current_File));
                  elsif AIN.Is_Event (F) and then not AIN.Is_Data (F) then
                     if Use_ARINC653_API then
                        Type_Used                    := RE (RE_Event_Id_Type);
                        Partition_ARINC653_Nb_Events :=
                          Partition_ARINC653_Nb_Events + 1;

                        Append_Node_To_List
                          (CTU.Make_Literal
                             (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                           CTN.Values (Partition_ARINC653_Events_Names));
                     else
                        Type_Used := RE (RE_Pok_Event_Id_T);
                     end if;

                     N :=
                       CTU.Make_Variable_Declaration
                         (Defining_Identifier =>
                            CTU.Make_Defining_Identifier (Map_Port_Var (F)),
                          Used_Type => Type_Used);

                     CTU.Append_Node_To_List
                       (N,
                        CTN.Declarations (CTU.Current_File));
                  end if;
               end if;
               F := Next_Node (F);
            end loop;
         end if;
      end Visit_Thread_Instance;

      ------------------------------
      -- Visit_Processor_Instance --
      ------------------------------

      procedure Visit_Processor_Instance (E : Node_Id) is
         S : Node_Id;

         POK_Errors : constant POK_Handled_Errors :=
           Get_POK_Recovery_Errors (E);

         POK_Actions : constant POK_Handled_Actions :=
           Get_POK_Recovery_Actions (E);

         POK_Error  : Supported_POK_Error;
         POK_Action : Supported_POK_Action;

         ARINC653_Handled_Errors : constant ARINC653_Errors :=
           Get_ARINC653_HM_Errors (E);

         ARINC653_Handled_Actions : constant ARINC653_Actions :=
           Get_ARINC653_HM_Actions (E);

         ARINC653_Error  : Supported_ARINC653_Error;
         ARINC653_Action : Supported_ARINC653_Action;

         Connected_Processor : Node_Id;

         U : Node_Id;
         P : Node_Id;
         N : Node_Id;
      begin
         U := CTN.Naming_Node (Backend_Node (Identifier (E)));
         P := CTN.Entity (U);

         Used_Buses := AINU.New_List (AIN.K_List_Id, No_Location);

         Kernel_Unit := U;

         Array_Local_Ports_To_Global_Ports := CTU.Make_Array_Values;

         Array_Buses_Partitions := CTU.Make_Array_Values;

         Partition_Error_Callback := False;

         Partitions_Error_Callback_Alternatives :=
           New_List (CTN.K_Alternatives_List);

         Push_Entity (C_Root);
         Push_Entity (P);
         Push_Entity (U);

         --  begin imported code

         CTU.Set_Deployment_Source;

         Kernel_Error_Switch_Alternatives :=
           New_List (CTN.K_Alternatives_List);

         Partition_Error_Switch_Alternatives :=
           New_List (CTN.K_Alternatives_List);

         Nb_Ports_By_Partition := CTU.Make_Array_Values;
         Ports_By_Partition    := CTU.Make_Array_Values;
         Ports_Names           := CTU.Make_Array_Values;
         Ports_Identifiers     := CTU.Make_Array_Values;
         Nb_Destinations       := CTU.Make_Array_Values;
         Ports_Destinations    := CTU.Make_Array_Values;
         Ports_Kind            := CTU.Make_Array_Values;

         --  For each system, we set this variable to 0. Then,
         --  subcomponents will modify it and tell us if there is
         --  some ports for inter-partition communication.

         Nports := 0;

         Set_Deployment_Source;

         Runtime.Kernel_Mode;

         Set_Deployment_Header;

         N :=
           CTU.Make_Define_Statement
             (Defining_Identifier => RE (RE_Pok_Config_Nb_Nodes),
              Value               =>
                CTU.Make_Literal (CV.New_Int_Value (Node_Identifier, 1, 10)));

         CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

         Set_Deployment_Source;

         --  Here, we declare the number of nodes in deployment.h file
         --  We do that here because doing it in the Header part
         --  would imply another parsing and would consume resources.

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         --
         --  If we are using DEOS, we do not need
         --  to write code to configure the kernel, this
         --  is done through the XML file of the deos_conf
         --  backend.
         --

         if POK_Flavor = DEOS or else POK_Flavor = VXWORKS then

            Pop_Entity;
            Pop_Entity;
            Pop_Entity;
            return;
         end if;

         if Nports > 0 then
            Add_Include (RH (RH_Types));
            Add_Include (RH (RH_Port));

            Array_Global_Ports_To_Local_Ports := CTU.Make_Array_Values;
            Array_Global_Ports_Bus            := CTU.Make_Array_Values;

            if not AINU.Is_Empty (All_Ports) then
               S := AIN.First_Node (All_Ports);
               while Present (S) loop
                  Connected_Processor :=
                    Parent_Component
                      (Parent_Subcomponent
                         (Get_Bound_Processor (Parent_Component (Item (S)))));
                  if Connected_Processor = E then
                     Append_Node_To_List
                       (Make_Defining_Identifier (Map_Port_Name (Item (S))),
                        CTN.Values (Array_Global_Ports_To_Local_Ports));

                     Set_Str_To_Name_Buffer ("invalid_bus");
                     Append_Node_To_List
                       (Make_Defining_Identifier (Name_Find),
                        CTN.Values (Array_Global_Ports_Bus));
                  else
                     Set_Str_To_Name_Buffer ("invalid_local_port");
                     Append_Node_To_List
                       (Make_Defining_Identifier (Name_Find),
                        CTN.Values (Array_Global_Ports_To_Local_Ports));

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_Bus_Name (Get_Bus (Item (S)))),
                        CTN.Values (Array_Global_Ports_Bus));
                  end if;
                  S := AIN.Next_Node (S);
               end loop;
            end if;

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Global_Ports_To_Local_Ports),
                           Array_Size => RE (RE_Pok_Config_Nb_Global_Ports)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Array_Global_Ports_To_Local_Ports);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Global_Ports_To_Bus),
                           Array_Size => RE (RE_Pok_Config_Nb_Global_Ports)),
                      Used_Type => RE (RE_Pok_Bus_Identifier_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Array_Global_Ports_Bus);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Pok_Buses_Partitions),
                           Array_Size          => RE (RE_Pok_Config_Nb_Buses)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Array_Buses_Partitions);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Local_Ports_To_Global_Ports),
                           Array_Size => RE (RE_Pok_Config_Nb_Ports)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Array_Local_Ports_To_Global_Ports);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Pok_Ports_Nodes),
                           Array_Size => RE (RE_Pok_Config_Nb_Global_Ports)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Array_Global_Ports_Nodes);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              Message_Comment
                ("This array indicates on which node is located each port. " &
                 "For example, it means that the first port is located on " &
                 "the node that is represented in this array with the first " &
                 "value. You can check node identifier values in the " &
                 "deployment.h file");

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  Build the variable pok_ports_nb_ports_by_partitions.
            --  It gives the amount of ports available inside a partition.

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Ports_Nb_Ports_By_Partition),
                           Array_Size => RE (RE_Pok_Config_Nb_Partitions)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Nb_Ports_By_Partition);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  Build the varible pok_ports_by_partitions which point to all
            --  accessible ports inside a partition.

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Ports_By_Partition),
                           Array_Size => RE (RE_Pok_Config_Nb_Partitions)),
                      Used_Type => CTU.Make_Pointer_Type (RE (RE_Uint8_T))),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Ports_By_Partition);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  Build the variable pok_ports_names

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Pok_Ports_Names),
                           Array_Size          => RE (RE_Pok_Config_Nb_Ports)),
                      Used_Type => CTU.Make_Pointer_Type (RE (RE_Char))),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Ports_Names);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            N :=
              Message_Comment
                ("This array contains the identifier of each port " &
                 "involved in inter-partition communication. These names " &
                 "are used in pok_port_sampling_create() and " &
                 "pok_port_ queueing_create");
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  Now, build the variable pok_ports_identifiers

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Ports_Identifiers),
                           Array_Size => RE (RE_Pok_Config_Nb_Ports)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Ports_Identifiers);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  Now, pok_ports_nb_destinations which give us the amount
            --  of destination per port.

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Ports_Nb_Destinations),
                           Array_Size => RE (RE_Pok_Config_Nb_Ports)),
                      Used_Type => RE (RE_Uint8_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Nb_Destinations);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  Build the pok_ports_destinations which is a pointer to other
            --  variable that contains the ports destination of each port.

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier =>
                             RE (RE_Pok_Ports_Destinations),
                           Array_Size => RE (RE_Pok_Config_Nb_Ports)),
                      Used_Type => CTU.Make_Pointer_Type (RE (RE_Uint8_T))),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Ports_Destinations);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  Build the pok_ports_kind variable that tell the kernel
            --  that is the kind of each port (sampling, queueing, ...)

            N :=
              CTU.Make_Expression
                (Left_Expr =>
                   CTU.Make_Variable_Declaration
                     (Defining_Identifier =>
                        CTU.Make_Array_Declaration
                          (Defining_Identifier => RE (RE_Pok_Ports_Kind),
                           Array_Size          => RE (RE_Pok_Config_Nb_Ports)),
                      Used_Type => RE (RE_Pok_Port_Kind_T)),
                 Operator   => CTU.Op_Equal,
                 Right_Expr => Ports_Kind);
            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));
         end if;

         ----------------------------------------------------------------
         --  From this line, we handle ONLY health monitoring service  --
         ----------------------------------------------------------------

         if Partition_Error_Callback then
            Set_Deployment_Header;

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Use_Generated_Partition_Error_Callback),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            Set_Deployment_Source;

            declare
               Spec         : Node_Id;
               Statements   : List_Id;
               Declarations : List_Id;
            begin
               Declarations := New_List (CTN.K_Declaration_List);
               Statements   := New_List (CTN.K_Statement_List);

               Append_Node_To_List
                 (Make_Switch_Statement
                    (Make_Defining_Identifier (PN (P_Partition)),
                     Partitions_Error_Callback_Alternatives),
                  Statements);

               Spec :=
                 Make_Function_Specification
                   (Defining_Identifier =>
                      RE (RE_Pok_Error_Partition_Callback),
                    Parameters =>
                      Make_List_Id
                        (Make_Parameter_Specification
                           (Make_Defining_Identifier (PN (P_Partition)),
                            Make_Defining_Identifier (TN (T_Uint32_T)))),
                    Return_Type => Make_Defining_Identifier (TN (T_Void)));

               --  Make the function that calls user-defined callback
               --  for each partition.

               Append_Node_To_List
                 (Make_Function_Implementation
                    (Spec,
                     Declarations,
                     Statements),
                  CTN.Declarations (Current_File));

               --  Append this function to deployment.c in the kernel part.
            end;
         end if;

         --  In the previous if statement, we analyse if partitions use
         --  a callback mecanism. If so, we generate a dedicated function
         --  called pok_error_partition_callback() that will execute
         --  the health-moniroting functions defined by the user().
         --  In the AADL model, the HM callback is specified using
         --  The ARINC653::HM_Callback property on AADL processors or
         --  virtual processors.

         if Partition_Error_Handling then
            Set_Deployment_Header;

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Use_Generated_Partition_Error_Handler),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            Set_Deployment_Source;
            declare
               Spec         : Node_Id;
               Statements   : List_Id;
               Declarations : List_Id;
            begin
               Declarations := New_List (CTN.K_Declaration_List);
               Statements   := New_List (CTN.K_Statement_List);

               Append_Node_To_List
                 (Make_Switch_Statement
                    (Make_Defining_Identifier (PN (P_Partition)),
                     Partition_Error_Switch_Alternatives),
                  Statements);

               Spec :=
                 Make_Function_Specification
                   (Defining_Identifier => RE (RE_Pok_Partition_Error),
                    Parameters          =>
                      Make_List_Id
                        (Make_Parameter_Specification
                           (Make_Defining_Identifier (PN (P_Partition)),
                            Make_Defining_Identifier (TN (T_Uint8_T))),
                         Make_Parameter_Specification
                           (Make_Defining_Identifier (PN (P_Error)),
                            Make_Defining_Identifier (TN (T_Uint32_T)))),

                    Return_Type => Make_Defining_Identifier (TN (T_Void)));

               Append_Node_To_List
                 (Make_Function_Implementation
                    (Spec,
                     Declarations,
                     Statements),
                  CTN.Declarations (Current_File));
            end;
         end if;

         --  We generate a function with a big switch/case statement. This
         --  switch/case will execute appropriate function to recover
         --  errors for each partitions. Recovery actions are different
         --  for each partitions and for each error.
         --  The Partition_Error_Switch_Alternatives node is filled
         --  during the execution of Visit_Virtual_Processor procedure.

         --  In the next lines, we analyse which errors are declared in the
         --  kernel side and we build a function that recover each error.
         --  It follows the same strategy as partitions : a single function
         --  with a switch/case statement to make a difference between
         --  all errors code. Note that we do that for the POK and ARINC653
         --  property sets.

         if Get_POK_Recovery_Errors (E) /= POK_Empty_Handled_Errors then
            Kernel_Error_Handling := True;

            for Error_Id in POK_Errors'Range loop
               --  Here, for each error, we take its associated
               --  actions and call the right function to recover
               --  the error.

               POK_Error := POK_Errors (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Errors.

               POK_Action := POK_Actions (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Actions.

               case POK_Error is
                  --  Here, for each error, we create a switch
                  --  case that will handle each declared error.

                  when POK_Error_Kernel_Init =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id (RE (RE_Pok_Error_Kind_Kernel_Init)),
                           Make_List_Id
                             (Map_POK_Kernel_Action (POK_Action, 0, True))),
                        Kernel_Error_Switch_Alternatives);

                  when POK_Error_Kernel_Scheduling =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Kernel_Scheduling)),
                           Make_List_Id
                             (Map_POK_Kernel_Action (POK_Action, 0, True))),
                        Kernel_Error_Switch_Alternatives);

                  when others =>
                     Display_Error
                       ("One error kind cannot be raised " &
                        "at the kernel level",
                        Fatal => True);
               end case;
            end loop;
         end if;

         if Get_ARINC653_HM_Errors (E) /= ARINC653_Empty_Errors then
            Kernel_Error_Handling := True;

            for Error_Id in ARINC653_Handled_Errors'Range loop
               --  Here, for each error, we take its associated
               --  actions and call the right function to recover
               --  the error.

               ARINC653_Error := ARINC653_Handled_Errors (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Errors.

               ARINC653_Action := ARINC653_Handled_Actions (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Actions.

               case ARINC653_Error is
                  --  Here, for each error, we create a switch
                  --  case that will handle each declared error.

                  when ARINC653_Error_Module_Init =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id (RE (RE_Pok_Error_Kind_Kernel_Init)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (ARINC653_Action,
                                 0,
                                 True))),
                        Kernel_Error_Switch_Alternatives);

                  when ARINC653_Error_Module_Scheduling =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Kernel_Scheduling)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (ARINC653_Action,
                                 0,
                                 True))),
                        Kernel_Error_Switch_Alternatives);

                  when ARINC653_Error_Module_Config =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id (RE (RE_Pok_Error_Kind_Kernel_Config)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (ARINC653_Action,
                                 0,
                                 True))),
                        Kernel_Error_Switch_Alternatives);

                  when others =>
                     Display_Error
                       ("One error kind cannot be raised " &
                        "at the kernel level",
                        Fatal => True);
               end case;
            end loop;
         end if;

         if Kernel_Error_Handling then
            Set_Deployment_Header;

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Use_Generated_Kernel_Error_Handler),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            --  We indicate in deployment.h that an error handler is generated.

            Set_Deployment_Source;

            declare
               Spec          : Node_Id;
               Callback_AADL : Node_Id;
               Statements    : List_Id;
            begin
               Set_Deployment_Header;

               N :=
                 CTU.Make_Define_Statement
                   (Defining_Identifier =>
                      RE (RE_Pok_Use_Generated_Kernel_Error_Callback),
                    Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

               CTU.Append_Node_To_List
                 (N,
                  CTN.Declarations (CTU.Current_File));

               Set_Deployment_Source;

               if Is_Defined_Property (E, "arinc653::hm_callback") then
                  Callback_AADL :=
                    Get_Classifier_Property (E, "arinc653::hm_callback");

                  --  Declare the user callback as extern.

                  Spec :=
                    Make_Function_Specification
                      (Defining_Identifier =>
                         Make_Defining_Identifier
                           (Get_Source_Name (Callback_AADL)),
                       Parameters  => No_List,
                       Return_Type => Make_Defining_Identifier (TN (T_Void)));
                  Append_Node_To_List
                    (Make_Extern_Entity_Declaration (Spec),
                     CTN.Declarations (CTU.Current_File));

                  --  Declare the generic call.

                  Spec :=
                    Make_Function_Specification
                      (Defining_Identifier =>
                         RE (RE_Pok_Error_Kernel_Callback),
                       Parameters  => No_List,
                       Return_Type => Make_Defining_Identifier (TN (T_Void)));

                  Statements := New_List (CTN.K_Statement_List);
                  Append_Node_To_List
                    (Make_Call_Profile
                       (Make_Defining_Identifier
                          (Get_Source_Name (Callback_AADL)),
                        No_List),
                     Statements);

                  Append_Node_To_List
                    (Make_Function_Implementation (Spec, No_List, Statements),
                     CTN.Declarations (Current_File));
               end if;

               --  If the ARINC653::HM_Callback property is added on the
               --  processor, we automatically generate a function
               --  that call the callback. We also declare the user-defined
               --  function since we don't have a correct header file.
               --  The function generated here is pok_error_kernel_callback().

            end;

            declare
               Spec         : Node_Id;
               Statements   : List_Id;
               Declarations : List_Id;
            begin
               Declarations := New_List (CTN.K_Declaration_List);
               Statements   := New_List (CTN.K_Statement_List);

               Append_Node_To_List
                 (Make_Switch_Statement
                    (Make_Defining_Identifier (PN (P_Error)),
                     Kernel_Error_Switch_Alternatives),
                  Statements);

               Spec :=
                 Make_Function_Specification
                   (Defining_Identifier => RE (RE_Pok_Kernel_Error),
                    Parameters          =>
                      Make_List_Id
                        (Make_Parameter_Specification
                           (Make_Defining_Identifier (PN (P_Error)),
                            Make_Defining_Identifier (TN (T_Uint32_T)))),
                    Return_Type => Make_Defining_Identifier (TN (T_Void)));

               Append_Node_To_List
                 (Make_Function_Implementation
                    (Spec,
                     Declarations,
                     Statements),
                  CTN.Declarations (Current_File));
            end;

            --  We generate the pok_kernel_error() function which is
            --  used to recover kernel errors. The content of the
            --  function is a big switch/case statement that executes
            --  recover functions depending on the detected error.
         end if;

         Runtime.User_Mode;

         --  Fallback in user mode, we don't generate files in the
         --  kernel side any more.

         Pop_Entity;
         Pop_Entity;
         Pop_Entity;
      end Visit_Processor_Instance;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
         Switch_Alternatives : constant List_Id :=
           New_List (CTN.K_Alternatives_List);

         POK_Errors : constant POK_Handled_Errors :=
           Get_POK_Recovery_Errors (E);

         POK_Actions : constant POK_Handled_Actions :=
           Get_POK_Recovery_Actions (E);

         POK_Error : Supported_POK_Error;

         POK_Action : Supported_POK_Action;

         ARINC653_Handled_Errors : constant ARINC653_Errors :=
           Get_ARINC653_HM_Errors (E);

         ARINC653_Handled_Actions : constant ARINC653_Actions :=
           Get_ARINC653_HM_Actions (E);

         ARINC653_Error : Supported_ARINC653_Error;

         ARINC653_Action : Supported_ARINC653_Action;

         Processes : List_Id;
         S         : Node_Id;
         U         : Node_Id;
      begin

         --  In the following, we handle all necessary stuff to handle
         --  Health Moniroting in partitions. We analyse the POK and
         --  the ARINC653 errors and actions as well as the health
         --  monitoring callback.

         if Get_POK_Recovery_Errors (E) /= POK_Empty_Handled_Errors then
            Partition_Error_Handling := True;

            for Error_Id in POK_Errors'Range loop
               --  Here, for each error, we take its associated
               --  actions and call the right function to recover
               --  the error.

               POK_Error := POK_Errors (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Errors.

               POK_Action := POK_Actions (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Actions.

               case POK_Error is
                  --  Here, for each error, we create a switch
                  --  case that will handle each declared error.

                  when POK_Error_Partition_Init =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Partition_Init)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (POK_Action,
                                 Partition_Id,
                                 False))),
                        Switch_Alternatives);

                  when POK_Error_Partition_Scheduling =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Partition_Scheduling)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (POK_Action,
                                 Partition_Id,
                                 False))),
                        Switch_Alternatives);

                  when POK_Error_Partition_Configuration =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Partition_Configuration)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (POK_Action,
                                 Partition_Id,
                                 False))),
                        Switch_Alternatives);

                  when others =>
                     Display_Error
                       ("One error kind cannot be raised " &
                        "at the partition-level",
                        Fatal => True);
               end case;
            end loop;

            Append_Node_To_List
              (Make_Switch_Alternative
                 (Make_List_Id
                    (Make_Literal (CV.New_Int_Value (Partition_Id, 1, 10))),
                  Make_List_Id
                    (Make_Switch_Statement
                       (Make_Defining_Identifier (PN (P_Error)),
                        Switch_Alternatives))),
               Partition_Error_Switch_Alternatives);
         end if;

         if Get_ARINC653_HM_Errors (E) /= ARINC653_Empty_Errors then
            Partition_Error_Handling := True;

            for Error_Id in ARINC653_Handled_Errors'Range loop
               --  Here, for each error, we take its associated
               --  actions and call the right function to recover
               --  the error.

               ARINC653_Error := ARINC653_Handled_Errors (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Errors.

               ARINC653_Action := ARINC653_Handled_Actions (Error_Id);
               --  This array correspond to the property
               --  POK_Recovery_Actions.

               case ARINC653_Error is
                  --  Here, for each error, we create a switch
                  --  case that will handle each declared error.

                  when ARINC653_Error_Partition_Init =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Partition_Init)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (ARINC653_Action,
                                 Partition_Id,
                                 False))),
                        Switch_Alternatives);

                  when ARINC653_Error_Partition_Handler =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Partition_Handler)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (ARINC653_Action,
                                 Partition_Id,
                                 False))),
                        Switch_Alternatives);

                  when ARINC653_Error_Partition_Scheduling =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Partition_Scheduling)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (ARINC653_Action,
                                 Partition_Id,
                                 False))),
                        Switch_Alternatives);

                  when ARINC653_Error_Partition_Config =>
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id
                             (RE (RE_Pok_Error_Kind_Partition_Configuration)),
                           Make_List_Id
                             (Map_POK_Kernel_Action
                                (ARINC653_Action,
                                 Partition_Id,
                                 False))),
                        Switch_Alternatives);

                  when others =>
                     Display_Error
                       ("One error kind cannot be raised " &
                        "at the partition-level",
                        Fatal => True);
               end case;
            end loop;

            Append_Node_To_List
              (Make_Switch_Alternative
                 (Make_List_Id
                    (Make_Literal (CV.New_Int_Value (Partition_Id, 1, 10))),
                  Make_List_Id
                    (Make_Switch_Statement
                       (Make_Defining_Identifier (PN (P_Error)),
                        Switch_Alternatives))),
               Partition_Error_Switch_Alternatives);
         end if;

         --  If the HM Callback is defined on the virtual
         --  processor, then, we handled it. We add the
         --  procedure described in the callback subprogram
         --  it will be treated when an error is raised.

         if Is_Defined_Property (E, "arinc653::hm_callback") then

            Partition_Error_Callback := True;

            --  This variable is set to true to know that some partitions
            --  use the hm callback mecanism and that we have to generate
            --  a function that will execute the hm callback of each
            --  partition if necessary.

            declare
               Callback_AADL : Node_Id;
               --  The corresponding AADL subprogram.

               Spec : Node_Id;
            --  The Spec of the user-defined function.
            begin
               Callback_AADL :=
                 Get_Classifier_Property (E, "arinc653::hm_callback");

               Append_Node_To_List
                 (Make_Switch_Alternative
                    (Make_List_Id
                       (Make_Literal (CV.New_Int_Value (Partition_Id, 1, 10))),
                     Make_List_Id
                       (Make_Call_Profile
                          (Make_Defining_Identifier
                             (Get_Source_Name (Callback_AADL)),
                           No_List))),
                  Partitions_Error_Callback_Alternatives);

               Spec :=
                 Make_Function_Specification
                   (Defining_Identifier =>
                      Make_Defining_Identifier
                        (Get_Source_Name (Callback_AADL)),
                    Parameters  => No_List,
                    Return_Type => Make_Defining_Identifier (TN (T_Void)));

               Append_Node_To_List
                 (Make_Extern_Entity_Declaration (Spec),
                  CTN.Declarations (CTU.Current_File));
               --  We declare the user-defined function to avoid
               --  compilation error since we don't have any header
               --  file to include (and non standardized name).
            end;
         end if;

         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            U         := Current_Entity;
            Pop_Entity;
            Runtime.User_Mode;
            S := AIN.First_Node (Processes);
            while Present (S) loop
               Visit (AIN.Item (S));
               S := AIN.Next_Node (S);
            end loop;
            Push_Entity (U);
            Runtime.Kernel_Mode;
         end if;

         Partition_Id := Partition_Id + 1;

      end Visit_Virtual_Processor_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P              : constant Node_Id := CTN.Entity (U);
         Implementation : Node_Id;
         S              : Node_Id;
      begin
         CTU.Push_Entity (P);
         CTU.Push_Entity (U);

         CTU.Set_Deployment_Header;

--         CTU.Append_Node_To_List
--            (CTU.Make_Literal
--               (CV.New_Int_Value
--                  (Partition_Id, 1, 10)),
--            CTN.Values (Array_Buses_Partitions));
--        Buses no more handled in POK.
--        this piece of code would still stay here until
--        we really device we don't need it.

         Implementation := Get_Classifier_Property (E, "implemented_as");

         Current_Device := E;

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  if Get_Category_Of_Component (S) = CC_Process then
                     Visit_Process_Instance
                       (Corresponding_Instance (S),
                        False,
                        E);
                  else
                     Visit (Corresponding_Instance (S));
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;

         end if;

         Current_Device := No_Node;

         CTU.Pop_Entity;
         CTU.Pop_Entity;
      end Visit_Device_Instance;

   end Source_File;

end Ocarina.Backends.POK_C.Deployment;
