------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . P O _ H I _ A D A            --
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
with Utils;

with Ocarina.Instances;
with Ocarina.Backends.Ada_Tree.Generator;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.PO_HI_Ada.Runtime;
with Ocarina.Backends.PO_HI_Ada.Naming;
with Ocarina.Backends.PO_HI_Ada.Marshallers;
with Ocarina.Backends.PO_HI_Ada.Deployment;
with Ocarina.Backends.PO_HI_Ada.Activity;
with Ocarina.Backends.PO_HI_Ada.Subprograms;
with Ocarina.Backends.PO_HI_Ada.Transport;
with Ocarina.Backends.PO_HI_Ada.Types;
with Ocarina.Backends.PO_HI_Ada.Main;
with Ocarina.Backends.ASN1;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Build_Utils;
with Ocarina.Backends.Execution_Utils;
with Ocarina.Backends.Execution_Tests;

with GNAT.Command_Line; use GNAT.Command_Line;

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.PO_HI_Ada is

   use GNAT.OS_Lib;
   use Ocarina.Output;
   use Ocarina.Instances;
   use Ocarina.Backends.Ada_Tree.Generator;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.PO_HI_Ada.Runtime;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Build_Utils;
   use Ocarina.Backends.Execution_Utils;
   use Ocarina.Backends.Execution_Tests;

   Compile_Generated_Sources   : Boolean := False;
   Remove_Generated_Sources    : Boolean := False;
   Do_Coverage_Test            : Boolean := False;
   Do_Regression_Test          : Boolean := False;
   Generated_Sources_Directory : Name_Id := No_Name;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   procedure PolyORB_HI_Ada_Makefile
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

   --  Generate the part of the Makefile that is specific to the
   --  corresponding code generator.

   procedure PolyORB_HI_Ada_Ada_Project_File
     (Appli_Name         : Name_Id;
      Node_Name          : Name_Id;
      Is_Server          : Boolean;
      Execution_Platform : Supported_Execution_Platform;
      Transport_API      : Supported_Transport_APIs;
      Spec_Names         : Name_Tables.Instance;
      Custom_Spec_Names  : Name_Tables.Instance;
      Body_Names         : Name_Tables.Instance;
      Custom_Body_Names  : Name_Tables.Instance;
      User_Source_Dirs   : Name_Tables.Instance);

   --------------------------
   --  Set_ASN1_Deployment --
   --------------------------

   Generate_ASN1_Deployment : Boolean := False;

   procedure Set_ASN1_Deployment (Use_It : Boolean) is
   begin
      Generate_ASN1_Deployment := Use_It;
   end Set_ASN1_Deployment;

   -----------------------------
   -- PolyORB_HI_Ada_Makefile --
   -----------------------------

   procedure PolyORB_HI_Ada_Makefile
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
      pragma Unreferenced
        (Appli_Name,
         Transport_API,
         Execution_Platform_Name,
         Ada_Sources,
         C_Libraries,
         User_Source_Dirs,
         Use_Transport,
         Use_Scade,
         Scade_Directory,
         Use_Simulink,
         Simulink_Directory,
         Simulink_Node);

      Target_Prefix : String_Access := Getenv ("TARGET_PREFIX");
      Target        : String_Access;

   begin
      --  Determine the compiler that will be used. If the
      --  user did specify the target prefix by mean of the
      --  environment variable "TARGET_PREFIX" then we use
      --  its value. Otherwise, we use the default compiler
      --  name.

      case Execution_Platform is
         when Platform_Native |
           Platform_LINUX32   |
           Platform_LINUX64   |
           Platform_WIN32     |
           Platform_None      =>
            Change_If_Empty (String_Ptr (Target_Prefix), "");
            Target := new String'("NATIVE");

         when Platform_LEON_ORK =>
            Change_If_Empty (String_Ptr (Target_Prefix), "sparc-elf-");
            Target := new String'("LEON_ORK");

         when Platform_LEON_GNAT =>
            Change_If_Empty (String_Ptr (Target_Prefix), "leon-elf-");
            Target := new String'("LEON_GNAT");

         when Platform_ERC32_ORK =>
            Change_If_Empty (String_Ptr (Target_Prefix), "erc32-elf-");
            Target := new String'("ERC32");

         when Platform_MARTE_OS =>
            Change_If_Empty (String_Ptr (Target_Prefix), "m");
            Target := new String'("MARTEOS");

         when Platform_LEON_RTEMS | Platform_LEON_RTEMS_POSIX =>
            --   Nothing to do: a special makefile is used for RTEMS
            null;

         when others =>
            raise Program_Error
              with "Unsupported platform: " & Execution_Platform'Img;
      end case;

      if Execution_Platform /= Platform_LEON_RTEMS
        and then Execution_Platform /= Platform_LEON_RTEMS_POSIX
      then
         Write_Line ("GNATMAKE = " & Target_Prefix.all & "gnatmake");
         Write_Line ("GNAT = " & Target_Prefix.all & "gnat");
         Write_Line ("CC = " & Target_Prefix.all & "gcc");
         Write_Line ("TARGET = " & Target.all);
         Write_Line ("BUILD = Debug");
         Write_Line ("CGCTRL = No");

         Free (Target_Prefix);
         Free (Target);

         --  Project file

         Write_Str ("PROJECT_FILE = ");
         Write_Name (Node_Name);
         Write_Line (".gpr");
         Write_Str ("ASN_SOURCES=");

         if Generate_ASN1_Deployment then
            Write_Str ("../../asn1_deployment.asn ");
         end if;

         if Length (Asn_Sources) > 0 then
            for J in Name_Tables.First .. Name_Tables.Last (Asn_Sources) loop
               Write_Str ("");
               Write_Name (Asn_Sources.Table (J));
               exit when J = Name_Tables.Last (Asn_Sources);
               Write_Space;
            end loop;
         end if;
         Write_Eol;

         --  The 'all' target

         Write_Str ("all:");

         --  First, process ASN.1 files

         if Length (Asn_Sources) > 0 then
            Write_Str (" generate-asn1-files");
         end if;

         --  If there are C files to be compiled, add a dependency on
         --  these files

         if Length (C_Sources) > 0 then
            Write_Str (" compile-c-files");
         end if;

         Write_Eol;

         Write_Char (ASCII.HT);
         Write_Line
           ("ADA_PROJECT_PATH=" &
            Standard.Utils.Quoted
              (Get_Runtime_Path ("polyorb-hi-ada") &
               Path_Separator &
               "$$ADA_PROJECT_PATH") &
            " \");
         Write_Char (ASCII.HT);
         Write_Str
           ("  $(GNATMAKE) -x -P$(PROJECT_FILE) -XTARGET=$(TARGET)" &
            " -XBUILD=$(BUILD) -XCGCTRL=$(CGCTRL) ${USER_CFLAGS}");

         --  If there are C source or C libraries, there will be more
         --  options.

         Write_Str
           (" -largs $(EXTERNAL_OBJECTS) ${C_OBJECTS} ${USER_LDFLAGS}");

         Write_Eol;

         if Execution_Platform = Platform_LEON_GNAT then
            --  Determine which portion of code is unused and recompile the
            --  the application with Eliminate pragmas.

            Write_Char (ASCII.HT);
            Write_Str ("  $(GNAT) elim -P$(PROJECT_FILE) ");
            Write_Name (Node_Name);
            Write_Str (" > local.adc");
            Write_Eol;

            Write_Char (ASCII.HT);
            Write_Str
              ("  $(GNATMAKE) -f -P$(PROJECT_FILE) -XTARGET=$(TARGET)" &
               " -XBUILD=$(BUILD) -XCGCTRL=$(CGCTRL)" &
               " -cargs -gnatec=local.adc");

            --  If there are C source or C libraries, there will be more
            --  options.

            Write_Str
              (" -largs $(EXTERNAL_OBJECTS) ${C_OBJECTS} ${USER_LDFLAGS}");
            Write_Eol;
         end if;
      else
         Write_Str ("PROGRAM = ");
         Write_Name (Node_Name);
         Write_Eol;
         Write_Eol;
         Write_Line
           ("include " &
            Get_Runtime_Path ("polyorb-hi-ada") &
            "/make/Makefile.rtems_ada");
         Write_Eol;
         Write_Line
           ("rtems_init.o: " &
            Get_Runtime_Path ("polyorb-hi-ada") &
            "/make/rtems_init.c $(FILESYSTEM_SRCS) $(NETWORK_HFILE) ");
         Write_Char (ASCII.HT);
         Write_Str ("$(CC) $(CFLAGS) -I. $(CPU_CFLAGS) -c $<");
      end if;

      Write_Eol;
      Write_Line ("generate-asn1-files: $(ASN_SOURCES)");
      Write_Char (ASCII.HT);
      Write_Line (" asn1.exe -Ada -uPER $(ASN_SOURCES)");
   end PolyORB_HI_Ada_Makefile;

   -------------------------------------
   -- PolyORB_HI_Ada_Ada_Project_File --
   -------------------------------------

   procedure PolyORB_HI_Ada_Ada_Project_File
     (Appli_Name         : Name_Id;
      Node_Name          : Name_Id;
      Is_Server          : Boolean;
      Execution_Platform : Supported_Execution_Platform;
      Transport_API      : Supported_Transport_APIs;
      Spec_Names         : Name_Tables.Instance;
      Custom_Spec_Names  : Name_Tables.Instance;
      Body_Names         : Name_Tables.Instance;
      Custom_Body_Names  : Name_Tables.Instance;
      User_Source_Dirs   : Name_Tables.Instance)
   is
      pragma Unreferenced (Appli_Name, Is_Server);

   begin
      Write_Line
        ("with """ &
         Get_Runtime_Path ("polyorb-hi-ada") &
         Directory_Separator &
         "polyorb_hi"";");

      Write_Eol;

      Write_Str ("project ");
      Write_Name (Node_Name);
      Write_Line (" is");
      Increment_Indentation;

      --  The source directory list

      Write_Indentation;
      Write_Line ("for Source_Dirs use");
      Increment_Indentation;

      Write_Indentation (-1);
      Write_Line ("(""."",");

      --  Get the PolyORB-HI/Ada runtime source directory

      Write_Indentation;
      Write_Str ("""" & Get_Runtime_Path ("polyorb-hi-ada") & """");

      --  The user provided source dirs

      if Length (User_Source_Dirs) > 0 then
         Write_Line (",");

         for J in Name_Tables.First .. Name_Tables.Last (User_Source_Dirs) loop
            Write_Indentation;
            Write_Char ('"');
            Write_Name (User_Source_Dirs.Table (J));
            Write_Char ('"');

            exit when J = Name_Tables.Last (User_Source_Dirs);

            Write_Line (",");
         end loop;
      end if;

      Write_Line (");");

      Decrement_Indentation;

      --  The main subprogram name

      Write_Eol;
      Write_Indentation;
      Write_Str ("for Main use (""");
      Write_Name (Node_Name);
      Write_Line (".adb"");");

      --  The custom file names

      Write_Eol;
      Write_Indentation;
      Write_Line ("package Naming is");
      Increment_Indentation;

      Write_Eol;
      Write_Indentation;
      Write_Line ("--  Custom middleware file names");
      Write_Eol;

      case Execution_Platform is
         when Platform_LEON_ORK | Platform_LEON_GNAT | Platform_ERC32_ORK =>
            Write_Indentation;
            Write_Line
              ("for Body (""PolyORB_HI.Output_Low_Level"")" &
               " use ""polyorb_hi-output_low_level_leon.adb"";");

         when others =>
            Write_Indentation;
            Write_Line
              ("for Body (""PolyORB_HI.Output_Low_Level"")" &
               " use ""polyorb_hi-output_low_level_native.adb"";");
      end case;

      case Transport_API is
         when Transport_BSD_Sockets =>
            Write_Indentation;
            Write_Line
              ("for Body (""PolyORB_HI.Transport_Low_Level"")" &
               " use ""polyorb_hi-transport_low_level_sockets.adb"";");

         when Transport_SpaceWire =>
            raise Program_Error;

         when Transport_None =>
            --  This means the application is monolithic
            null;

         when Transport_User =>
            --  Code to be supplied by the user
            null;
      end case;

      if Length (Spec_Names) > 0 then
         Write_Eol;
         Write_Indentation;
         Write_Line ("--  Custom user spec names");
         Write_Eol;

         for J in Name_Tables.First .. Name_Tables.Last (Spec_Names) loop
            Write_Indentation;
            Write_Str ("for Specification (""");
            Write_Name (Spec_Names.Table (J));
            Write_Str (""") use """);
            Write_Name (Custom_Spec_Names.Table (J));
            Write_Line (""";");
         end loop;
      end if;

      if Length (Body_Names) > 0 then
         Write_Eol;
         Write_Indentation;
         Write_Line ("--  Custom user body names");
         Write_Eol;

         for J in Name_Tables.First .. Name_Tables.Last (Body_Names) loop
            Write_Indentation;
            Write_Str ("for Body (""");
            Write_Name (Body_Names.Table (J));
            Write_Str (""") use """);
            Write_Name (Custom_Body_Names.Table (J));
            Write_Line (""";");
         end loop;
      end if;

      Write_Eol;
      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Naming;");
      Write_Eol;

      Write_Indentation;
      Write_Line ("package Compiler renames PolyORB_HI.Compiler;");
      Write_Indentation;
      Write_Line ("package Builder renames PolyORB_HI.Builder;");
      Write_Indentation;
      Write_Line ("package Binder renames PolyORB_HI.Binder;");
      Write_Indentation;
      Write_Line ("package Linker renames PolyORB_HI.Linker;");
      Write_Indentation;
      Write_Line ("package Check renames PolyORB_HI.Check;");
      Write_Indentation;
      Write_Line ("package Prove renames PolyORB_HI.Prove;");
      Write_Eol;

      Decrement_Indentation;
      Write_Str ("end ");
      Write_Name (Node_Name);
      Write_Line (";");
   end PolyORB_HI_Ada_Ada_Project_File;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
      Success       : Boolean := True;

      procedure Generate_PolyORB_HI_Ada_Makefile is new Build_Utils.Makefiles
        .Generate
        (PolyORB_HI_Ada_Makefile);

      procedure Generate_PolyORB_HI_Ada_Ada_Project_File is new Build_Utils
        .Ada_Project_Files
        .Generate
        (PolyORB_HI_Ada_Ada_Project_File);

   begin
      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);

      --  Expand the AADL instance

      Expand (Instance_Root);

      Visit_Architecture_Instance (Instance_Root);

      --  Abort if the construction of the Ada tree failed

      if No (Ada_Root) then
         Display_Error ("Code generation failed", Fatal => True);
      end if;

      --  At this point, we have a valid Ada tree, we can begin the
      --  Ada source file generation.

      --  Enter the output directory

      Enter_Directory (Generated_Sources_Directory);

      if not Remove_Generated_Sources then
         --  Create the source files

         Ada_Tree.Generator.Generate (Ada_Root);

         --  Generate the Makefiles

         Generate_PolyORB_HI_Ada_Makefile (Instance_Root);

         --  Generate the Ada project files

         Generate_PolyORB_HI_Ada_Ada_Project_File (Instance_Root);

         --  If we have to generate the ASN1 deployment file, then
         --  we enter the directory that contains the generated
         --  code and invoke directly the ASN1 generator with the
         --  instance root. It should automatically create an .asn1
         --  file that contains deployment/messages informations.

         if Generate_ASN1_Deployment then
            ASN1.Generate (Instance_Root);
         end if;

         --  If the user requested to build the applications then we
         --  build it.

         if Compile_Generated_Sources then
            Build_Utils.Makefiles.Build (Instance_Root);
         end if;

      else
         Build_Utils.Makefiles.Clean (Instance_Root);
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
         case Getopt ("* b z ec er o:") is
            when ASCII.NUL =>
               exit;

            when 'b' =>
               Compile_Generated_Sources := True;

            when 'z' =>
               Remove_Generated_Sources := True;

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

      --  Registration of the generator

      Register_Backend ("polyorb_hi_ada", Generate'Access, PolyORB_HI_Ada);

      --  Initialize some units

      Ocarina.Backends.PO_HI_Ada.Runtime.Initialize;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ocarina.Backends.PO_HI_Ada.Runtime.Reset;
   end Reset;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      --  Create the specs subtrees

      Deployment.Package_Spec.Visit (E);
      Naming.Package_Spec.Visit (E);
      Types.Package_Spec.Visit (E);
      Subprograms.Package_Spec.Visit (E);
      Activity.Package_Spec.Visit (E);
      Transport.Package_Spec.Visit (E);
      Marshallers.Package_Spec.Visit (E);

      --  Create the bodies subtrees

      Types.Package_Body.Visit (E);
      Subprograms.Package_Body.Visit (E);
      Transport.Package_Body.Visit (E);
      Activity.Package_Body.Visit (E);
      Marshallers.Package_Body.Visit (E);

      --  The main subprogram

      Main.Subprogram_Body.Visit (E);

      --  The makefiles

      Build_Utils.Makefiles.Visit (E);

      --  The Ada project files

      Build_Utils.Ada_Project_Files.Visit (E);
   end Visit_Architecture_Instance;

end Ocarina.Backends.PO_HI_Ada;
