------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . P O K _ C . M A K E F I L E       --
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

with Locations; use Locations;
with Ocarina.Namet;
with GNAT.OS_Lib;
with Ocarina.Output;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;

with Ocarina.Instances.Queries;

package body Ocarina.Backends.POK_C.Makefile is
   use Ocarina.Namet;
   use Ocarina.Output;
   use GNAT.OS_Lib;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Instances.Queries;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Or_Device_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);
   procedure Visit_Subprogram_Instance (E : Node_Id);
   procedure Build_Architecture_Instance (E : Node_Id);
   procedure Build_Component_Instance (E : Node_Id);
   procedure Build_System_Instance (E : Node_Id);
   procedure Build_Processor_Instance (E : Node_Id);
   procedure Handle_Source_File (N : Name_Id);

   Partition_Names        : List_Id;
   Partition_Object_Files : List_Id;
   Kernel_Object_Files    : List_Id;
   Lustre_Directory       : Name_Id;
   Use_Lustre             : Boolean;
   Current_Processor      : Node_Id;
   Current_Partition      : Node_Id;
   Mac_Addr               : Name_Id;
   Nb_Ports_Intra         : Unsigned_Long_Long := 0;
   Nb_Shared_Data         : Unsigned_Long_Long := 0;
   Nb_Ports_Inter         : Unsigned_Long_Long := 0;
   Use_HM_Services        : Boolean            := False;
   Qemu_Server            : Boolean            := True;

   Kernel_Mode : Boolean := True;

   --  The kernel mode variable indicates where we are
   --  in the generation and which part of the system
   --  we are currently generating. If set to True, we
   --  are generating the kernel part of the system. Else,
   --  we are generating partitions.

   ------------------------
   -- Handle_Source_File --
   ------------------------

   procedure Handle_Source_File (N : Name_Id) is
      M : Name_Id;
   begin
      Set_Str_To_Name_Buffer ("%POK%Makefile%");
      Get_Name_String_And_Append (N);
      Get_Name_String_And_Append
        (Display_Name (Identifier (Parent_Subcomponent (Current_Processor))));

      if not Kernel_Mode then
         Get_Name_String_And_Append
           (Display_Name
              (Identifier (Parent_Subcomponent (Current_Partition))));
      end if;

      M := Name_Find;

      if Get_Name_Table_Info (M) /= 0 then
         return;
      end if;

      Set_Name_Table_Info (M, 1);

      if Kernel_Mode then
         Append_Node_To_List
           (Make_Defining_Identifier (N, False),
            Kernel_Object_Files);
      else
         Append_Node_To_List
           (Make_Defining_Identifier (N, False),
            Partition_Object_Files);
      end if;
   end Handle_Source_File;

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
            Visit_Process_Or_Device_Instance (E);

         when CC_Device =>
            Visit_Process_Or_Device_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor_Instance (E);

         when CC_Thread =>
            Visit_Thread_Instance (E);

         when CC_Subprogram =>
            Visit_Subprogram_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   -------------------------------
   -- Visit_Subprogram_Instance --
   -------------------------------

   procedure Visit_Subprogram_Instance (E : Node_Id) is
      N            : Name_Id;
      Source_Files : constant Name_Array := Get_Source_Text (E);
   begin
      if Get_Subprogram_Kind (E) = Subprogram_Lustre then
         Use_Lustre       := True;
         Lustre_Directory := Get_Source_Location (E);
         Set_Str_To_Name_Buffer ("");
         Get_Name_String_And_Append (Get_Source_Location (E));
         Get_Name_String_And_Append (Get_Source_Name (E));

         Add_Str_To_Name_Buffer (".o");
         N := Name_Find;
      else
         N := Get_Source_Location (E);
      end if;

      if N /= No_Name then
         Handle_Source_File (N);
      else
         for J in Source_Files'Range loop
            Handle_Source_File (Source_Files (J));
         end loop;
      end if;
   end Visit_Subprogram_Instance;

   ---------------------------
   -- Visit_Thread_Instance --
   ---------------------------

   procedure Visit_Thread_Instance (E : Node_Id) is
      Call_Seq : Node_Id;
      Spg_Call : Node_Id;
      F        : Node_Id;
   begin
      if Get_Thread_Initialize_Entrypoint (E) /= No_Node then
         Visit (Get_Thread_Initialize_Entrypoint (E));
      end if;

      --  Visit all the call sequences of the thread

      if not AINU.Is_Empty (Calls (E)) then
         Call_Seq := First_Node (Calls (E));

         while Present (Call_Seq) loop
            --  For each call sequence visit all the called
            --  subprograms.

            if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
               Spg_Call := AIN.First_Node (Subprogram_Calls (Call_Seq));

               while Present (Spg_Call) loop
                  Visit (Corresponding_Instance (Spg_Call));

                  Spg_Call := Next_Node (Spg_Call);
               end loop;
            end if;

            Call_Seq := Next_Node (Call_Seq);
         end loop;
      end if;

      if not AINU.Is_Empty (Features (E)) then
         F := AIN.First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               if Get_Connection_Pattern (F) = Inter_Process then
                  Nb_Ports_Inter := Nb_Ports_Inter + 1;
               else
                  Nb_Ports_Intra := Nb_Ports_Intra + 1;
               end if;

               if Is_In (F)
                 and then Is_Event (F)
                 and then Get_Port_Compute_Entrypoint (F) /= No_Node
               then
                  Visit (Get_Port_Compute_Entrypoint (F));
               end if;
            end if;
            F := Next_Node (F);
         end loop;
      end if;

   end Visit_Thread_Instance;

   --------------------------------------
   -- Visit_Process_Or_Device_Instance --
   --------------------------------------

   procedure Visit_Process_Or_Device_Instance (E : Node_Id) is
      U : constant Node_Id :=
        CTN.Distributed_Application_Unit
          (CTN.Naming_Node (Backend_Node (Identifier (E))));
      P                   : constant Node_Id := CTN.Entity (U);
      N                   : Node_Id;
      S                   : Node_Id;
      Fd                  : File_Descriptor;
      Inspected_Component : Node_Id;
   begin
      Push_Entity (P);
      Push_Entity (U);

      Nb_Ports_Intra := 0;
      Nb_Shared_Data := 0;

      Use_Lustre := False;

      if Is_Defined_Property (E, "pok::hw_addr") then
         Mac_Addr := Get_String_Property (E, "pok::hw_addr");
      end if;

      Enter_Directory
        (To_C_Name
           (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E))),
            Keyword_Check => False));
      Fd := Create_File ("Makefile", Text);

      Partition_Object_Files := CTU.New_List (CTN.K_Element_List);

      Add_Str_To_Name_Buffer (".elf");

      if AINU.Is_Device (E) then
         Inspected_Component :=
           Corresponding_Instance
             (First_Node
                (AIN.Subcomponents
                   (Get_Classifier_Property (E, "implemented_as"))));
      else
         Inspected_Component := E;
      end if;

      if Inspected_Component /= No_Node
        and then not AINU.Is_Empty (Subcomponents (Inspected_Component))
      then
         S := First_Node (Subcomponents (Inspected_Component));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if AINU.Is_Data (Corresponding_Instance (S))
              and then Is_Protected_Data (Corresponding_Instance (S))
            then
               Nb_Shared_Data := Nb_Shared_Data + 1;
            else
               Visit (Corresponding_Instance (S));
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      if Fd = Invalid_FD then
         raise Program_Error;
      end if;

      Set_Str_To_Name_Buffer ("");
      Get_Name_String_And_Append
        (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E))));

      Append_Node_To_List
        (Make_Defining_Identifier
           (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))),
         Partition_Names);

      Add_Str_To_Name_Buffer (".elf");

      Set_Output (Fd);

      Write_Line ("export DEPLOYMENT_HEADER=$(shell pwd)/deployment.h");
      Write_Line ("include $(POK_PATH)/misc/mk/config.mk");
      Write_Str ("TARGET = ");
      Write_Name (Name_Find);
      Write_Eol;

      if Use_Lustre then
         Write_Str ("LUSTRE_DIRECTORY = ");
         Write_Name (Lustre_Directory);
         Write_Eol;
      end if;

      Write_Str ("OBJS = main.o activity.o subprograms.o gtypes.o");

      if not CTU.Is_Empty (Partition_Object_Files) then
         N := CTN.First_Node (Partition_Object_Files);
         while Present (N) loop
            Write_Space;
            Write_Name (CTN.Name (N));
            N := CTN.Next_Node (N);
         end loop;
      end if;

      if Nb_Ports_Intra > 0 or else Nb_Shared_Data > 0 then
         Write_Str (" deployment.o");
      end if;

      Write_Eol;

      Write_Line ("all: libpok $(TARGET)");
      Write_Line ("clean: common-clean");
      Write_Line ("include $(POK_PATH)/misc/mk/common-$(ARCH).mk");
      Write_Line ("include $(POK_PATH)/misc/mk/rules-partition.mk");
      Write_Line ("include $(POK_PATH)/misc/mk/rules-common.mk");

      Close (Fd);

      Leave_Directory;

      Pop_Entity; -- U
      Pop_Entity; -- P
   end Visit_Process_Or_Device_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      Fd           : File_Descriptor;
      S            : Node_Id;
      System_Nodes : List_Id;
   begin
      --  Visit all the subcomponents of the system
      Push_Entity (C_Root);

      System_Nodes := AINU.New_List (K_List_Id, No_Location);

      Set_Str_To_Name_Buffer ("generated-code");
      Enter_Directory (Name_Find);

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if AINU.Is_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
               AINU.Append_Node_To_List
                 (AINU.Make_Node_Container (Corresponding_Instance (S)),
                  System_Nodes);
            end if;
            S := Next_Node (S);
         end loop;
      end if;

      Fd := Create_File ("Makefile", Text);

      if Fd = Invalid_FD then
         raise Program_Error;
      end if;

      Set_Output (Fd);

      if not AINU.Is_Empty (System_Nodes) then
         S := AIN.First_Node (System_Nodes);
         Write_Line ("all:");
         while Present (S) loop
            Write_Char (ASCII.HT);
            Write_Str ("$(MAKE) -C ");
            Write_Name
              (AIN.Display_Name
                 (AIN.Identifier (AIN.Parent_Subcomponent (Item (S)))));
            Write_Str (" all");
            Write_Eol;
            S := Next_Node (S);
         end loop;
      end if;

      Write_Eol;

      if not AINU.Is_Empty (System_Nodes) then
         S := AIN.First_Node (System_Nodes);
         Write_Line ("clean:");
         while Present (S) loop
            Write_Char (ASCII.HT);
            Write_Str ("$(MAKE) -C ");
            Write_Name
              (AIN.Display_Name
                 (AIN.Identifier (AIN.Parent_Subcomponent (Item (S)))));
            Write_Str (" clean");
            Write_Eol;
            S := Next_Node (S);
         end loop;
      end if;

      Write_Eol;

      if not AINU.Is_Empty (System_Nodes) then
         S := AIN.First_Node (System_Nodes);
         Write_Line ("run:");
         while Present (S) loop
            Write_Char (ASCII.HT);
            Write_Str ("$(MAKE) -C ");
            Write_Name
              (AIN.Display_Name
                 (AIN.Identifier (AIN.Parent_Subcomponent (Item (S)))));
            Write_Str (" run&");
            Write_Eol;
            S := Next_Node (S);
         end loop;
      end if;

      Close (Fd);

      Set_Standard_Output;

      Leave_Directory; --  leave generated code directory

      Pop_Entity;
   end Visit_System_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S         : Node_Id;
      U         : Node_Id;
      Processes : List_Id;
   begin

      Current_Partition := E;

      if Is_Defined_Property (E, "arinc653::hm_errors")
        or else Is_Defined_Property (E, "pok::recovery_errors")
      then
         Use_HM_Services := True;
      end if;

      if Is_Defined_Property (E, "arinc653::hm_callback") then
         Kernel_Mode := True;
         Visit_Subprogram_Instance
           (Get_Classifier_Property (E, "arinc653::hm_callback"));
         Kernel_Mode := False;
      end if;

      --  Here, we add the HM_Callback defined for the HM service
      --  of ARINC653. We add the subprogram specified by this
      --  property in the list of compiled files in the kernel.
      --  We do that in the kernel since partitions health monitoring
      --  functionnalities reside in the kernel.

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
   end Visit_Virtual_Processor_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S    : Node_Id;
      U    : Node_Id;
      P    : Node_Id;
      Fd   : File_Descriptor;
      N    : Node_Id;
      Arch : constant Supported_POK_Architectures := Get_POK_Architecture (E);
      BSP  : constant Supported_POK_BSP           := Get_POK_BSP (E);
   begin

      Kernel_Object_Files := CTU.New_List (CTN.K_Element_List);

      Kernel_Mode := True;

      Use_HM_Services := False;

      Mac_Addr := No_Name;

      U := CTN.Naming_Node (Backend_Node (Identifier (E)));
      P := CTN.Entity (U);

      Push_Entity (P);
      Push_Entity (U);

      Nb_Ports_Inter := 0;

      Current_Processor := E;

      --  Reset the amount of inter-partitions ports.

      Partition_Names := CTU.New_List (CTN.K_Element_List);

      --  Reset the partition names table for this node.

      Enter_Directory
        (To_C_Name
           (AIN.Name (AIN.Identifier (Parent_Subcomponent (E))),
            Keyword_Check => False));

      Kernel_Mode := False;

      --  Visit all the subcomponents of the system

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Kernel_Mode := True;

      if Is_Defined_Property (E, "arinc653::hm_errors")
        or else Is_Defined_Property (E, "pok::recovery_errors")
      then
         Use_HM_Services := True;
      end if;

      if Is_Defined_Property (E, "arinc653::hm_callback") then
         Visit_Subprogram_Instance
           (Get_Classifier_Property (E, "arinc653::hm_callback"));
      end if;

      Fd := Create_File ("Makefile", Text);

      if Fd = Invalid_FD then
         raise Program_Error;
      end if;

      Set_Output (Fd);

      case Arch is
         when POK_Arch_x86 =>
            Write_Line ("export ARCH=x86");
         when POK_Arch_Sparc =>
            Write_Line ("export ARCH=sparc");
         when POK_Arch_Ppc =>
            Write_Line ("export ARCH=ppc");
         when others =>
            Display_Error ("Unknown architecture", Fatal => True);
      end case;

      case BSP is
         when POK_BSP_x86_qemu =>
            Write_Line ("export BSP=x86-qemu");
         when POK_BSP_x86_qemu_vmm =>
            Write_Line ("export BSP=x86-qemu-vmm");
         when POK_BSP_Leon =>
            Write_Line ("export BSP=leon3");
         when POK_BSP_prep =>
            Write_Line ("export BSP=prep");
         when others =>
            Display_Error ("Unknown BSP", Fatal => True);
      end case;

      Write_Line ("export POK_CONFIG_OPTIMIZE_FOR_GENERATED_CODE=1");

      if Mac_Addr /= No_Name then
         Write_Str ("MAC_ADDR=");
         Write_Name (Mac_Addr);
         Write_Eol;

         if Qemu_Server then
            Write_Line ("QEMU_NETWORK_MODE=listen");
            Qemu_Server := False;
         else
            Write_Line ("QEMU_NETWORK_MODE=connect");
         end if;
      end if;

      Write_Line ("include $(POK_PATH)/misc/mk/config.mk");
      Write_Line ("include $(POK_PATH)/misc/mk/common-$(ARCH).mk");

      Write_Line ("TARGET=$(shell pwd)/pok.elf");
      Write_Str ("PARTITIONS=");

      if not CTU.Is_Empty (Partition_Names) then
         N := CTN.First_Node (Partition_Names);
         while Present (N) loop
            Write_Space;
            Write_Name (CTN.Name (N));
            Write_Str ("/");
            Write_Name (CTN.Name (N));
            Write_Str (".elf");
            N := CTN.Next_Node (N);
         end loop;
      end if;
      Write_Eol;
      Write_Eol;

      Write_Line ("KERNEL=kernel/kernel.lo");

      Write_Line ("all: build-kernel partitions $(TARGET)");

      Write_Line ("build-kernel:");
      Write_Char (ASCII.HT);
      Write_Line ("$(CD) kernel && $(MAKE)");
      Write_Eol;

      Write_Line ("partitions:");
      if not CTU.Is_Empty (Partition_Names) then
         N := CTN.First_Node (Partition_Names);
         while Present (N) loop
            Write_Char (ASCII.HT);
            Write_Str ("$(CD) ");
            Write_Name (CTN.Name (N));
            Write_Str ("&& $(MAKE)");
            N := CTN.Next_Node (N);
            Write_Eol;
         end loop;
      end if;
      Write_Eol;
      Write_Eol;

      Write_Line ("clean: common-clean");
      if not CTU.Is_Empty (Partition_Names) then
         Write_Char (ASCII.HT);
         Write_Str ("$(CD) kernel && $(MAKE) clean");
         Write_Eol;
         N := CTN.First_Node (Partition_Names);
         while Present (N) loop
            Write_Char (ASCII.HT);
            Write_Str ("$(CD) ");
            Write_Name (CTN.Name (N));
            Write_Str ("&& $(MAKE) clean");
            Write_Eol;
            N := CTN.Next_Node (N);
         end loop;
      end if;
      Write_Eol;
      Write_Eol;

      Write_Line ("distclean: clean");
      if not CTU.Is_Empty (Partition_Names) then
         Write_Char (ASCII.HT);
         Write_Str ("$(CD) kernel && $(MAKE) distclean");
         Write_Eol;
         N := CTN.First_Node (Partition_Names);
         while Present (N) loop
            Write_Char (ASCII.HT);
            Write_Str ("$(CD) ");
            Write_Name (CTN.Name (N));
            Write_Str ("&& $(MAKE) distclean");
            Write_Eol;
            N := CTN.Next_Node (N);
         end loop;
      end if;
      Write_Eol;
      Write_Eol;

      Write_Line ("include $(POK_PATH)/misc/mk/rules-common.mk");
      Write_Line ("include $(POK_PATH)/misc/mk/rules-main.mk");
      Write_Line ("include $(POK_PATH)/misc/mk/install-rules.mk");

      Close (Fd);

      --  now, generate kernel configuration
      Set_Str_To_Name_Buffer ("kernel");
      Enter_Directory (Name_Find);

      Fd := Create_File ("Makefile", Text);

      if Fd = Invalid_FD then
         raise Program_Error;
      end if;

      Set_Output (Fd);

      Write_Line ("export DEPLOYMENT_HEADER=$(shell pwd)/deployment.h");
      Write_Line ("include $(POK_PATH)/misc/mk/config.mk");
      Write_Line ("LO_TARGET = kernel.lo");

      Write_Str ("LO_OBJS =");

      if not CTU.Is_Empty (Kernel_Object_Files) then
         N := CTN.First_Node (Kernel_Object_Files);
         while Present (N) loop
            Write_Space;
            Write_Name (CTN.Name (N));
            N := CTN.Next_Node (N);
         end loop;
      end if;

      --  The Kernel_Objects_Files array contains
      --  health monitoring functions (the hm callback)
      --  of both kernel and partitions.

      if Nb_Ports_Inter > 0 or else Use_HM_Services then
         Write_Str (" deployment.o");
      end if;

      Write_Eol;

      Write_Line ("LO_DEPS = pok.lo");
      Write_Line ("all: kernel copy-kernel $(LO_TARGET)");
      Write_Line ("clean: common-clean");
      Write_Line ("include $(POK_PATH)/misc/mk/common-$(ARCH).mk");
      Write_Line ("include $(POK_PATH)/misc/mk/rules-common.mk");
      Write_Line ("include $(POK_PATH)/misc/mk/rules-kernel.mk");

      Close (Fd);

      Leave_Directory; --  leave kernel directory

      Leave_Directory; --  leave main directory

      Set_Standard_Output;

      Pop_Entity;
      Pop_Entity;
   end Visit_Processor_Instance;

   -----------
   -- Build --
   -----------

   procedure Build (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Build_Architecture_Instance (E);

         when K_Component_Instance =>
            Build_Component_Instance (E);

         when others =>
            null;
      end case;
   end Build;

   ---------------------------------
   -- Build_Architecture_Instance --
   ---------------------------------

   procedure Build_Architecture_Instance (E : Node_Id) is
   begin
      Build (Root_System (E));
   end Build_Architecture_Instance;

   ------------------------------
   -- Build_Component_Instance --
   ------------------------------

   procedure Build_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Build_System_Instance (E);

         when CC_Processor =>
            Build_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Build_Component_Instance;

   ---------------------------
   -- Build_System_Instance --
   ---------------------------

   procedure Build_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      --  Build all process subcomponents

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));

         while Present (S) loop
            Build (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Build_System_Instance;

   ------------------------------
   -- Build_Processor_Instance --
   ------------------------------

   procedure Build_Processor_Instance (E : Node_Id) is
      Pid     : Process_Id;
      Out_Pid : Process_Id := Invalid_Pid;
      Success : Boolean;
      Args    : Argument_List (1 .. 1);
   begin

      --  Enter the directories

      Set_Str_To_Name_Buffer ("generated-code");
      Enter_Directory (Name_Find);
      Enter_Directory
        (To_C_Name (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));

      --  If the user set the BUILD environment variable to some
      --  value, we pass it the GNU make command.

      declare
         Build_Kind    : String_Access := Getenv ("BUILD");
         GNU_Make_Path : String_Access := Locate_Exec_On_Path (GNU_Make_Cmd);
      begin
         Change_If_Empty (String_Ptr (Build_Kind), "Debug");
         Args (1) := new String'("BUILD=" & Build_Kind.all);

         --  Invoke the 'make' command

         Pid :=
           Non_Blocking_Spawn
             (Program_Name => GNU_Make_Path.all,
              Args         => Args);

         --  Wait until the command achieves its execution

         while Out_Pid /= Pid loop
            Wait_Process (Out_Pid, Success);
            exit when Out_Pid = Pid or else Out_Pid = Invalid_Pid;
         end loop;

         if Out_Pid = Pid then
            if not Success then
               Display_Error
                 (GNU_Make_Path.all & " died unexpectedly",
                  Fatal => True);
            else
               pragma Debug
                 (Display_Debug_Message
                    (GNU_Make_Cmd & " terminated normally",
                     Force => True));
               null;
            end if;
         end if;

         Free (Build_Kind);
         Free (GNU_Make_Path);

         for J in Args'Range loop
            Free (Args (J));
         end loop;
      end;

      --  Leave the directories

      Leave_Directory;
      Leave_Directory;
   end Build_Processor_Instance;
end Ocarina.Backends.POK_C.Makefile;
