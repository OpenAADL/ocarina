------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2005-2009, GET-Telecom Paris.                --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib;
with Output;

with Ocarina.Instances;
with Ocarina.Backends.Ada_Tree.Generator;
with Ocarina.Backends.Build_Utils;
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Execution_Utils;
with Ocarina.Backends.Execution_Tests;

with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.PO_QoS_Ada.Main;
with Ocarina.Backends.PO_QoS_Ada.Helpers;
with Ocarina.Backends.PO_QoS_Ada.Servants;
with Ocarina.Backends.PO_QoS_Ada.Parameters;
with Ocarina.Backends.PO_QoS_Ada.Obj_Adapters;
with Ocarina.Backends.PO_QoS_Ada.Setup;
with Ocarina.Backends.PO_QoS_Ada.Namespaces;

with GNAT.Command_Line; use GNAT.Command_Line;

with Namet; use Namet;

package body Ocarina.Backends.PO_QoS_Ada is

   use GNAT.OS_Lib;
   use Output;
   use Ocarina.Instances;
   use Ocarina.Backends.Expander;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Generator;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Build_Utils;
   use Ocarina.Backends.Execution_Utils;
   use Ocarina.Backends.Execution_Tests;
   use Ocarina.Backends.Properties;

   Compile_Generated_Sources   : Boolean := False;
   Do_Coverage_Test            : Boolean := False;
   Do_Regression_Test          : Boolean := False;
   Remove_Generated_Sources    : Boolean := False;
   Generated_Sources_Directory : Name_Id := No_Name;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   procedure PolyORB_QoS_Ada_Makefile
     (Appli_Name         : Name_Id;
      Node_Name          : Name_Id;
      Execution_Platform : Supported_Execution_Platform := Platform_None;
      Transport_API      : Supported_Transport_APIs;
      Ada_Sources        : Name_Tables.Instance;
      Asn_Sources        : Name_Tables.Instance;
      C_Sources          : Name_Tables.Instance;
      C_Libraries        : Name_Tables.Instance;
      User_Source_Dirs   : Name_Tables.Instance;
      Use_Transport      : Boolean;
      Use_Simulink       : Boolean;
      Simulink_Directory : Name_Id;
      Simulink_Node      : Name_Id;
      Use_Scade          : Boolean;
      Scade_Directory    : Name_Id);
   --  Generate the part of the Makefile that is specific to the
   --  corresponding code generator.

   procedure PolyORB_QoS_Ada_Ada_Project_File
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

   --  Generate the part of the Ada project file that is specific
   --  to the corresponding code generator.

   ------------------------------
   -- PolyORB_Qos_Ada_Makefile --
   ------------------------------

   procedure PolyORB_QoS_Ada_Makefile
     (Appli_Name         : Name_Id;
      Node_Name          : Name_Id;
      Execution_Platform : Supported_Execution_Platform := Platform_None;
      Transport_API      : Supported_Transport_APIs;
      Ada_Sources        : Name_Tables.Instance;
      Asn_Sources        : Name_Tables.Instance;
      C_Sources          : Name_Tables.Instance;
      C_Libraries        : Name_Tables.Instance;
      User_Source_Dirs   : Name_Tables.Instance;
      Use_Transport      : Boolean;
      Use_Simulink       : Boolean;
      Simulink_Directory : Name_Id;
      Simulink_Node      : Name_Id;
      Use_Scade          : Boolean;
      Scade_Directory    : Name_Id)
   is
      pragma Unreferenced (Appli_Name,
                           Transport_API,
                           Ada_Sources,
                           Asn_Sources,
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
         when Platform_Native | Platform_None =>
            Change_If_Empty (String_Ptr (Target_Prefix), "");
            Target := new String'("NATIVE");

         when others =>
            Display_Error ("Unsupported platform: " & Execution_Platform'Img,
                           Fatal => True);
      end case;

      Write_Line ("GNATMAKE = " & Target_Prefix.all & "gnatmake");
      Write_Line ("GNAT = " & Target_Prefix.all & "gnat");
      Write_Line ("GCC = " & Target_Prefix.all & "gcc");
      Write_Line ("TARGET = " & Target.all);
      Write_Line ("BUILD = Debug");
      Write_Line ("CGCTRL = No");

      Free (Target_Prefix);
      Free (Target);

      --  Project file

      Write_Str ("PROJECT_FILE = ");
      Write_Name (Node_Name);
      Write_Line (".gpr");
      Write_Eol;

      --  The 'all' target

      Write_Str ("all: ");

      --  If there are C files to be compiled add a dependency on
      --  these files

      if Length (C_Sources) > 0 then
         Write_Str (" compile-c-files");
      end if;

      Write_Eol;

      Write_Str
        (ASCII.HT & "$(GNAT) make -P$(PROJECT_FILE) `polyorb-config`");

      --  If there are C source or C libraries, there will be more
      --  options.

      Write_Str (" -largs $(EXTERNAL_OBJECTS) ${C_OBJECTS}");
   end PolyORB_QoS_Ada_Makefile;

   --------------------------------------
   -- PolyORB_Qos_Ada_Ada_Project_File --
   --------------------------------------

   procedure PolyORB_QoS_Ada_Ada_Project_File
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
      pragma Unreferenced (Appli_Name,
                           Is_Server,
                           Execution_Platform,
                           Transport_API,
                           Spec_Names,
                           Custom_Spec_Names,
                           Body_Names,
                           Custom_Body_Names);

   begin
      Write_Str ("project ");
      Write_Name (Node_Name);
      Write_Line (" is");
      Increment_Indentation;

      --  Build type

      Write_Indentation;
      Write_Line ("type Build_Type is (""Debug"", ""Release"");");
      Write_Indentation;
      Write_Line ("Build : Build_Type := external (""BUILD"", ""Debug"");");

      --  The source directory list

      Write_Eol;
      Write_Indentation;
      Write_Line ("for Source_Dirs use");
      Increment_Indentation;

      Write_Indentation (-1);
      Write_Line ("(""."",");

      --  Get the PolyORB-QoS Ocarina runtime source directory

      Write_Indentation;
      Write_Str ("""" & Get_Runtime_Path ("polyorb-qos-ada") & """");

      --  The user provided source dirs

      if Length (User_Source_Dirs) > 0 then
         Write_Line (",");

         for J in
           Name_Tables.First .. Name_Tables.Last (User_Source_Dirs) loop
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

      --  The executables are put in the '../bin' directory

      Write_Indentation;
      Write_Line ("for Exec_Dir use ""../bin"";");

      --  Create the 'bin' directory

      Create_Directory (Get_String_Name ("../bin"));

      --  The compile flags

      Write_Eol;
      Write_Indentation;
      Write_Line ("Common_Options := (""-gnatwae"");");
      Write_Indentation;
      Write_Line ("Debug_Options := (""-g"", ""-gnatoa"");");
      Write_Indentation;
      Write_Line ("Release_Options := (""-gnatp"");");

      Write_Eol;
      Write_Indentation;
      Write_Line ("Package Compiler is");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("case Build is");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("when ""Debug"" =>");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("for Default_Switches (""Ada"")"
                  & " use Common_Options & Debug_Options;");
      Write_Eol;
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("when ""Release"" =>");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("for Default_Switches (""Ada"")"
                  & " use Common_Options & Release_Options;");
      Write_Eol;
      Decrement_Indentation;
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("end Case;");

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Compiler;");

      --  The build flags

      Write_Eol;
      Write_Indentation;
      Write_Line ("Package Builder is");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("case Build is");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("when ""Debug"" =>");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("for Default_Switches (""Ada"")"
                  & " use (""-s"", ""-m"", ""-g"");");
      Write_Eol;
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("when ""Release"" =>");
      Increment_Indentation;
      Write_Indentation;
      Write_Line ("for Default_Switches (""Ada"")"
                  & " use (""-a"", ""-f"", ""-x"");");
      Write_Eol;
      Decrement_Indentation;
      Decrement_Indentation;

      Write_Indentation;
      Write_Line ("end Case;");

      Decrement_Indentation;
      Write_Indentation;
      Write_Line ("end Builder;");

      Decrement_Indentation;
      Write_Str ("end ");
      Write_Name (Node_Name);
      Write_Line (";");
   end PolyORB_QoS_Ada_Ada_Project_File;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;

      procedure Generate_PolyORB_QoS_Ada_Makefile is new
        Build_Utils.Makefiles.Generate (PolyORB_QoS_Ada_Makefile);

      procedure Generate_PolyORB_QoS_Ada_Ada_Project is new
        Build_Utils.Ada_Project_Files.Generate
        (PolyORB_QoS_Ada_Ada_Project_File);

      Success : Boolean := True;
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

         Generate_PolyORB_QoS_Ada_Makefile (Instance_Root);

         --  Generate the Ada project files

         Generate_PolyORB_QoS_Ada_Ada_Project (Instance_Root);

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
            if Execute_Regression_Test (Scenario_Dir.all,
                                        Ref_Map,
                                        Integer (Timeout)) = False then
               Success := False;
            end if;

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
            Success := Execute_Coverage_Test (Integer (Timeout)) and then
              Success;
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
         case Getopt ("* b e z o:") is
            when ASCII.NUL =>
               exit;

            when 'b' =>
               Compile_Generated_Sources := True;

            when 'e' =>
               Compile_Generated_Sources := True;
               if Full_Switch = "ec" then
                  Do_Coverage_Test := True;
               end if;
               if Full_Switch = "er" then
                  Do_Regression_Test := True;
               end if;

            when 'z' =>
               Remove_Generated_Sources := True;

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

      Register_Backend ("polyorb_qos_ada", Generate'Access, PolyORB_QoS_Ada);
      Ocarina.Backends.PO_QoS_Ada.Runtime.Initialize;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ocarina.Backends.PO_QoS_Ada.Runtime.Reset;
   end Reset;

   ---------------------------------
   -- Visit_Architecture_Instance --
   ---------------------------------

   procedure Visit_Architecture_Instance (E : Node_Id) is
   begin
      --  Create the specs subtrees

      Namespaces.Package_Spec.Visit (E);
      Helpers.Package_Spec.Visit (E);
      Servants.Package_Spec.Visit (E);
      Parameters.Package_Spec.Visit (E);
      Obj_Adapters.Package_Spec.Visit (E);
      Setup.Package_Spec.Visit (E);

      --  Create the bodies subtrees

      Namespaces.Package_Body.Visit (E);
      Helpers.Package_Body.Visit (E);
      Servants.Package_Body.Visit (E);
      Parameters.Package_Body.Visit (E);
      Setup.Package_Body.Visit (E);
      Main.Subprogram_Body.Visit (E);

      --  The makefiles

      Build_Utils.Makefiles.Visit (E);

      --  The Ada project files

      Build_Utils.Ada_Project_Files.Visit (E);
   end Visit_Architecture_Instance;

end Ocarina.Backends.PO_QoS_Ada;
