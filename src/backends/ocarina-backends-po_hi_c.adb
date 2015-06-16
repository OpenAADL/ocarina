------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . B A C K E N D S . P O _ H I _ C              --
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

with GNAT.OS_Lib;
with Ocarina.Output;

with GNAT.Directory_Operations;

with Ocarina.Instances;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.C_Tree.Generator;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.PO_HI_C.Activity;
with Ocarina.Backends.PO_HI_C.Main;
with Ocarina.Backends.PO_HI_C.Naming;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.PO_HI_C.Deployment;
with Ocarina.Backends.PO_HI_C.Request;
with Ocarina.Backends.C_Common.Types;
with Ocarina.Backends.C_Common.Subprograms;
with Ocarina.Backends.PO_HI_C.Marshallers;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Build_Utils;
with Ocarina.Backends.Execution_Utils;
with Ocarina.Backends.Execution_Tests;
with Ocarina.Backends.Properties;
with Ocarina.Backends.ASN1;
with GNAT.Command_Line;

with Ocarina.Namet;

package body Ocarina.Backends.PO_HI_C is

   use GNAT.Command_Line;
   use GNAT.OS_Lib;

   use Ocarina.Namet;
   use Ocarina.Output;

   use GNAT.Directory_Operations;

   use Ocarina.Backends.Properties;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.PO_HI_C.Activity;
   use Ocarina.Backends.PO_HI_C.Deployment;
   use Ocarina.Backends.PO_HI_C.Main;
   use Ocarina.Backends.PO_HI_C.Naming;
   use Ocarina.Backends.PO_HI_C.Request;
   use Ocarina.Backends.C_Common.Types;
   use Ocarina.Backends.C_Common.Subprograms;
   use Ocarina.Backends.PO_HI_C.Marshallers;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Tree.Generator;
   use Ocarina.Backends.ASN1;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Expander;
   use Ocarina.Instances;
   use Ocarina.Backends.Build_Utils;
   use Ocarina.Backends.Execution_Utils;
   use Ocarina.Backends.Execution_Tests;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;

   Generate_ASN1_Deployment    : Boolean := False;
   Compile_Generated_Sources   : Boolean := False;
   Remove_Generated_Sources    : Boolean := False;
   Add_Performance_Analysis    : Boolean := False;
   Do_Regression_Test          : Boolean := False;
   Do_Coverage_Test            : Boolean := False;
   Generated_Sources_Directory : Name_Id := No_Name;
   Verbose_Mode                : Boolean := False;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   procedure PolyORB_HI_C_Makefile
     (Appli_Name              : Name_Id;
      Node_Name               : Name_Id;
      Execution_Platform      : Supported_Execution_Platform := Platform_None;
      Execution_Platform_Name : Name_Id;
      Transport_API           : Supported_Transport_APIs;
      Ada_Sources             : Name_Tables.Instance;
      Asn_Sources             : Name_Tables.Instance;
      C_Sources               : Name_Tables.Instance;
      C_Libraries             : Name_Tables.Instance;
      User_Source_Dirs        : Name_Tables.Instance;
      Use_Transport           : Boolean;
      Use_Simulink            : Boolean;
      Simulink_Directory      : Name_Id;
      Simulink_Node           : Name_Id;
      Use_Scade               : Boolean;
      Scade_Directory         : Name_Id);

   ---------------------------
   -- PolyORB_HI_C_Makefile --
   ---------------------------

   procedure PolyORB_HI_C_Makefile
     (Appli_Name              : Name_Id;
      Node_Name               : Name_Id;
      Execution_Platform      : Supported_Execution_Platform := Platform_None;
      Execution_Platform_Name : Name_Id;
      Transport_API           : Supported_Transport_APIs;
      Ada_Sources             : Name_Tables.Instance;
      Asn_Sources             : Name_Tables.Instance;
      C_Sources               : Name_Tables.Instance;
      C_Libraries             : Name_Tables.Instance;
      User_Source_Dirs        : Name_Tables.Instance;
      Use_Transport           : Boolean;
      Use_Simulink            : Boolean;
      Simulink_Directory      : Name_Id;
      Simulink_Node           : Name_Id;
      Use_Scade               : Boolean;
      Scade_Directory         : Name_Id)
   is
      pragma Unreferenced (Transport_API, C_Sources, Ada_Sources, C_Libraries);
      O_File : Name_Id;
   begin
      if Use_ASN1_Deployment then
         Write_Str ("ASN_OBJS += asn1_deployment.o ");
         if Length (Asn_Sources) > 0 then
            for J in Name_Tables.First .. Name_Tables.Last (Asn_Sources) loop

               Get_Name_String (Asn_Sources.Table (J));
               Name_Buffer (Name_Len - 2) := 'o';

               Set_Str_To_Name_Buffer
                 (Base_Name (Name_Buffer (1 .. Name_Len - 2)));
               O_File := Name_Find;

               Write_Str ("");
               Write_Name (O_File);

               exit when J = Name_Tables.Last (Asn_Sources);

               Write_Space;
            end loop;
         end if;
         Write_Eol;
      end if;

      if Appli_Name /= No_Name then
         Write_Str ("MAINAPP = ");
         Write_Name (Appli_Name);
         Write_Eol;
      end if;

      Write_Str ("BINARY = ");
      Write_Name (Node_Name);
      Write_Eol;

      Write_Str ("TARGET = ");

      case Execution_Platform is
         when Platform_Native =>
            Write_Str ("native");

         when Platform_None =>
            if Execution_Platform_Name /= No_Name then
               Write_Name (Execution_Platform_Name);
            else
               Write_Str ("native");
            end if;

         when Platform_Native_Compcert =>
            Write_Str ("compcert");

         when Platform_NDS_RTEMS =>
            Write_Str ("nds.rtems");

         when Platform_NDS_RTEMS_POSIX =>
            Write_Str ("nds.rtems_posix");

         when Platform_Gumstix_RTEMS =>
            Write_Str ("gumstix.rtems");

         when Platform_Gumstix_RTEMS_POSIX =>
            Write_Str ("gumstix.rtems_posix");

         when Platform_Bench =>
            Write_Str ("bench");

         when Platform_LEON_RTEMS =>
            Write_Str ("leon.rtems");

         when Platform_LEON_RTEMS_POSIX =>
            Write_Str ("leon.rtems_posix");

         when Platform_X86_LINUXTASTE =>
            Write_Str ("x86.linuxtaste");

         when Platform_X86_RTEMS =>
            Write_Str ("x86.rtems");

         when Platform_X86_RTEMS_POSIX =>
            Write_Str ("x86.rtems_posix");

         when Platform_ARM_DSLINUX =>
            Write_Str ("arm.dslinux");

         when Platform_ARM_N770 =>
            Write_Str ("arm.n770");

         when Platform_LINUX64 =>
            Write_Str ("linux64");

         when Platform_LINUX32 =>
            Write_Str ("linux32");

         when Platform_WIN32 =>
            Write_Str ("win32");

         when Platform_LEON3_XM3 =>
            Write_Str ("leon3-xm3");

         when Platform_LEON3_SCOC3 =>
            Write_Str ("leon3-scoc3");

         when Platform_LINUX32_XENOMAI_NATIVE =>
            Write_Str ("linux32-xenomai-native");

         when Platform_LINUX32_XENOMAI_POSIX =>
            Write_Str ("linux32-xenomai-posix");

         when others =>
            Display_Error
              ("Unsupported platform " & Execution_Platform'Img,
               Fatal => True);
      end case;

      Write_Eol;
      Write_Str ("NEED_TRANSPORT = ");
      if Use_Transport then
         Write_Line ("yes");
      else
         Write_Line ("no");
      end if;

      if Add_Performance_Analysis then
         Write_Str ("USE_GPROF = yes");
         Write_Eol;
      end if;

      if Generate_ASN1_Deployment then
         Write_Str ("USE_ASN1_DEPLOYMENT = yes");
         Write_Eol;
      end if;

      if Use_Simulink then
         Write_Str ("SIMULINK_DIR = ");
         Write_Name (Simulink_Directory);
         Write_Eol;

         Write_Str ("SIMULINK_NODE = ");
         Write_Name (Simulink_Node);
         Write_Eol;
      end if;

      if Use_Scade then
         Write_Str ("SCADE_DIR = ");
         Write_Name (Scade_Directory);
         Write_Eol;
      end if;

      Write_Line ("RUNTIME_PATH=" & Get_Runtime_Path ("polyorb-hi-c"));

      Write_Str ("USER_SOURCES_DIRS=");
      if Length (User_Source_Dirs) > 0 then
         for J in Name_Tables.First .. Name_Tables.Last (User_Source_Dirs) loop
            Write_Space;
            Write_Str ("""-I");
            Write_Name
              (Remove_Directory_Separator (User_Source_Dirs.Table (J)));
            Write_Str ("""");
            exit when J = Name_Tables.Last (User_Source_Dirs);

            Write_Space;
         end loop;
         Write_Eol;
      else
         Write_Str (".");
         Write_Eol;
      end if;

      Write_Str ("ASN_SOURCES=");

      if Generate_ASN1_Deployment then
         Write_Str ("../asn1_deployment.asn ");
      end if;

      if Length (Asn_Sources) > 0 then
         for J in Name_Tables.First .. Name_Tables.Last (Asn_Sources) loop
            Write_Str ("");
            Write_Name (Asn_Sources.Table (J));

            exit when J = Name_Tables.Last (Asn_Sources);

            Write_Space;
         end loop;

         Write_Eol;
      else
         Write_Eol;
      end if;

      Write_Line ("include $(RUNTIME_PATH)/make/Makefile.common");
   end PolyORB_HI_C_Makefile;

   ------------------------------------
   -- Generate_Doxygen_Configuration --
   ------------------------------------

   procedure Generate_Doxygen_Configuration (My_System : Node_Id) is
      use Ocarina.Backends.C_Tree.Nodes;
      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case AAN.Kind (E) is
            when AAN.K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when AAN.K_Component_Instance =>
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
         Visit (AAN.Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         S : constant Node_Id := AAN.Parent_Subcomponent (E);
         A : constant Node_Id :=
           AAN.Parent_Component (AAN.Parent_Subcomponent (E));
         Fd    : File_Descriptor;
         Rpath : constant String := Get_Runtime_Path ("polyorb-hi-c");
      begin
         Enter_Directory (Normalize_Name (AAN.Name (AAN.Identifier (A))));
         Enter_Directory (Normalize_Name (AAN.Name (AAN.Identifier (S))));

         Fd := Create_File ("doxygen.cfg", Text);

         if Fd = Invalid_FD then
            raise Program_Error;
         end if;

         Set_Output (Fd);

         Write_Str ("PROJECT_NAME           = ");
         Write_Name (Normalize_Name (AAN.Name (AAN.Identifier (S))));
         Write_Eol;
         Write_Line ("OUTPUT_DIRECTORY       = generated-documentation");
         Write_Line ("GENERATE_LATEX         = YES");
         Write_Line ("GENERATE_MAN           = YES");
         Write_Line ("GENERATE_RTF           = YES");
         Write_Line ("CASE_SENSE_NAMES       = NO");
         Write_Str ("INPUT                  = . ");
         Write_Str (Rpath);
         Write_Eol;
         Write_Line ("SOURCE_BROWSER         = YES");
         Write_Line ("OPTIMIZE_OUTPUT_FOR_C  = YES");
         Write_Line ("GENERATE_TREEVIEW      = YES");
         Write_Line ("QUIET                  = YES");
         Write_Line ("HAVE_DOT               = YES");
         Write_Line ("JAVADOC_AUTOBRIEF      = YES");
         Write_Line ("EXTRACT_ALL            = YES");
         Write_Line ("EXTRACT_PRIVATE        = YES");
         Write_Line ("EXTRACT_STATIC         = YES");
         Write_Line ("TYPEDEF_HIDES_STRUCT   = YES");
         Write_Line ("INLINE_SOURCES         = YES");
         Write_Line ("REFERENCED_BY_RELATION = YES");
         Write_Line ("REFERENCES_RELATION    = YES");
         Write_Line ("SEARCHENGINE           = YES");
         Write_Line ("SERVER_BASED_SEARCH    = NO");
         Write_Line ("SEARCH_INCLUDES        = YES");
         Write_Line ("INCLUDED_BY_GRAPH      = YES");
         Write_Line ("ENABLE_PREPROCESSING   = YES");
         Write_Line ("OUTPUT_LANGUAGE        = English");
         Write_Line ("BRIEF_MEMBER_DESC      = YES");
         Write_Line ("REPEAT_BRIEF           = YES");
         Write_Line ("RECURSIVE              = YES");
         Write_Line ("EXAMPLE_RECURSIVE      = NO");
         Write_Line ("DIRECTORY_GRAPH        = YES");
         Write_Line ("CLASS_GRAPH            = YES");
         Write_Line ("COLLABORATION_GRAPH    = YES");
         Write_Line ("GROUP_GRAPHS           = YES");
         Write_Line ("INCLUDE_GRAPH          = YES");
         Write_Line ("INCLUDED_BY_GRAPH      = YES");
         Write_Line ("CALL_GRAPH             = YES");
         Write_Line ("CALLER_GRAPH           = YES");
         Write_Line ("GRAPHICAL_HIERARCHY    = YES");
         Write_Line ("DIRECTORY_GRAPH        = YES");
         Write_Line ("DOT_GRAPH_MAX_NODES    = 50");
         Write_Line ("MAX_DOT_GRAPH_DEPTH    = 0");

         Write_Eol;

         Close (Fd);

         Set_Standard_Output;

         Leave_Directory;
         Leave_Directory;
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         --  Visit all the subcomponents of the system

         if not AAU.Is_Empty (AAN.Subcomponents (E)) then
            S := AAN.First_Node (AAN.Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := AAN.Next_Node (S);
            end loop;
         end if;
      end Visit_System_Instance;
   begin
      Visit (My_System);
   end Generate_Doxygen_Configuration;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
      Success       : Boolean := True;

      procedure Generate_PolyORB_HI_C_Makefile is new Build_Utils.Makefiles
        .Generate
        (PolyORB_HI_C_Makefile);

   begin
      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);

      --  Expand the AADL instance

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);
      --  Abort if the construction of the C tree failed

      if No (C_Root) then
         Display_Error ("Code generation failed", Fatal => True);
      end if;

      --  At this point, we have a valid C tree, we can begin the
      --  generation of C source files.

      --  Enter the output directory

      Enter_Directory (Generated_Sources_Directory);
      if Verbose_Mode then
         Set_Standard_Error;
         Write_Str ("Generating code in directory: ");
         Write_Name (Generated_Sources_Directory);
         Write_Eol;
         Set_Standard_Output;
      end if;

      if Remove_Generated_Sources then
         Build_Utils.Makefiles.Clean (Instance_Root);

      else
         --  Create the source files

         C_Tree.Generator.Generate (C_Root);

         --  Create doxygen configuration files

         Generate_Doxygen_Configuration (Instance_Root);

         --  Generate the Makefiles

         Generate_PolyORB_HI_C_Makefile (Instance_Root);

         --  If we have to generate the ASN1 deployment file, then
         --  we enter the directory that contains the generated
         --  code and invoke directly the ASN1 generator with the
         --  instance root. It should automatically create an .asn1
         --  file that contains deployment/messages informations.

         if Generate_ASN1_Deployment then
            Enter_Directory (CTN.Name (C_Root));
            ASN1.Generate (Instance_Root);
            Leave_Directory;
         end if;

         --  If the user requested to build the applications then we
         --  build it.

         if Compile_Generated_Sources then
            Build_Utils.Makefiles.Build (Instance_Root);
         end if;
      end if;

      --  If the user requested to test the applications then we
      --  test it.

      if Do_Regression_Test or else Do_Coverage_Test then

         --  Execution_Utils package initialization

         Execution_Utils.Init;
         Execution_Utils.Visit (Instance_Root);
         Execution_Tests.Init;

         if Do_Regression_Test then
            Success :=
              Execute_Regression_Test
                (Scenario_Dir.all,
                 Ref_Map,
                 Integer (Timeout))
              and then Success;

            if not Create_Referencial then
               Write_Eol;
               if Success then
                  Write_Line ("--- All regression tests PASSED ---");
               else
                  Write_Line ("--- Regression tests FAILED ---");
               end if;
            end if;
         end if;

         if Do_Coverage_Test then
            Success :=
              Execute_Coverage_Test (Integer (Timeout)) and then Success;
         end if;

         --  Free memory
         Execution_Utils.Reset;
         Execution_Tests.Reset;
         Free (Scenario_Dir);

         --  Exit if one of the tests failed

         if not Success then
            OS_Exit (1);
         end if;
      end if;

      --  Leave the output directory
      Leave_Directory;
   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Generated_Sources_Directory := Get_String_Name (".");
      Initialize_Option_Scan;
      loop
         case Getopt ("* b z ec er o: perf asn1 v") is
            when ASCII.NUL =>
               exit;

            when 'a' =>
               if Full_Switch = "asn1" then
                  Generate_ASN1_Deployment := True;
               end if;

            when 'b' =>
               Compile_Generated_Sources := True;

            when 'v' =>
               Verbose_Mode := True;

            when 'z' =>
               Remove_Generated_Sources := True;

            when 'p' =>
               if Full_Switch = "perf" then
                  Add_Performance_Analysis := True;
               end if;

            when 'e' =>
               Compile_Generated_Sources := True;
               if Full_Switch = "ec" then
                  Do_Coverage_Test := True;
               elsif Full_Switch = "er" then
                  Do_Regression_Test := True;
               end if;

            when 'o' =>
               declare
                  D : constant String := Parameter;
               begin
                  if D'Length /= 0 then
                     Generated_Sources_Directory := Get_String_Name (D);
                  end if;
               end;

            when others =>
               null;
         end case;
      end loop;

      Register_Backend ("polyorb_hi_c", Generate'Access, PolyORB_HI_C);
      Ocarina.Backends.PO_HI_C.Runtime.Initialize;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ocarina.Backends.PO_HI_C.Runtime.Reset;
   end Reset;

   ------------------------------
   -- Use_Performance_Analysis --
   ------------------------------

   function Use_Performance_Analysis return Boolean is
   begin
      return Add_Performance_Analysis;
   end Use_Performance_Analysis;

   -------------------------
   -- Use_ASN1_Deployment --
   -------------------------

   function Use_ASN1_Deployment return Boolean is
   begin
      return Generate_ASN1_Deployment;
   end Use_ASN1_Deployment;

   --------------------------
   --  Set_ASN1_Deployment --
   --------------------------

   procedure Set_ASN1_Deployment (Use_It : Boolean) is
   begin
      Generate_ASN1_Deployment := Use_It;
   end Set_ASN1_Deployment;

   -------------------------------
   --  Set_Performance_Analysis --
   -------------------------------

   procedure Set_Performance_Analysis (Use_It : Boolean) is
   begin
      Add_Performance_Analysis := Use_It;
   end Set_Performance_Analysis;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      Naming.Header_File.Visit (E);
      C_Common.Types.Header_File.Visit (E, C_Root);
      Deployment.Header_File.Visit (E);
      Request.Header_File.Visit (E);
      Marshallers.Header_File.Visit (E);
      C_Common.Subprograms.Header_File.Visit (E, C_Root);
      Activity.Header_File.Visit (E);

      C_Common.Types.Source_File.Visit (E, C_Root);
      Request.Source_File.Visit (E);
      Marshallers.Source_File.Visit (E);
      Deployment.Source_File.Visit (E);
      Activity.Source_File.Visit (E);
      C_Common.Subprograms.Source_File.Visit (E);
      Naming.Source_File.Visit (E);
      Main.Source_File.Visit (E);

      --  Generate a Makefile for each node

      Build_Utils.Makefiles.Visit (E);

   end Visit_Architecture_Instance;

end Ocarina.Backends.PO_HI_C;
