------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--             O C A R I N A . B A C K E N D S . P O _ H I _ C              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2008-2010, European Space Agency (ESA).           --
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
with Ocarina.Backends.Expander;
with Ocarina.Backends.Messages;
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

with Namet;

package body Ocarina.Backends.PO_HI_C is

   use GNAT.Command_Line;
   use GNAT.OS_Lib;

   use Namet;
   use Output;

   use Ocarina.Backends.Properties;
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

   package CTN renames Ocarina.Backends.C_Tree.Nodes;

   Generate_ASN1_Deployment    : Boolean := False;
   Compile_Generated_Sources   : Boolean := False;
   Remove_Generated_Sources    : Boolean := False;
   Add_Performance_Analysis    : Boolean := False;
   Do_Regression_Test          : Boolean := False;
   Do_Coverage_Test            : Boolean := False;
   Generated_Sources_Directory : Name_Id := No_Name;

   procedure Visit_Architecture_Instance (E : Node_Id);
   --  Most top level visitor routine. E is the root of the AADL
   --  instance tree. The procedure does a traversal for each
   --  compilation unit to be generated.

   procedure PolyORB_HI_C_Makefile
     (Appli_Name         : Name_Id;
      Node_Name          : Name_Id;
      Execution_Platform : Supported_Execution_Platform := Platform_None;
      Transport_API      : Supported_Transport_APIs;
      Ada_Sources        : Name_Tables.Instance;
      C_Sources          : Name_Tables.Instance;
      C_Libraries        : Name_Tables.Instance;
      User_Source_Dirs   : Name_Tables.Instance;
      Use_Transport      : Boolean;
      Use_Simulink       : Boolean;
      Simulink_Directory : Name_Id;
      Simulink_Node      : Name_Id;
      Use_Scade          : Boolean;
      Scade_Directory    : Name_Id);

   ---------------------------
   -- PolyORB_HI_C_Makefile --
   ---------------------------

   procedure PolyORB_HI_C_Makefile
     (Appli_Name         : Name_Id;
      Node_Name          : Name_Id;
      Execution_Platform : Supported_Execution_Platform := Platform_None;
      Transport_API      : Supported_Transport_APIs;
      Ada_Sources        : Name_Tables.Instance;
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
                           C_Sources,
                           Ada_Sources,
                           C_Libraries);
   begin
      Write_Str ("BINARY = ");
      Write_Name (Node_Name);
      Write_Eol;

      Write_Str ("TARGET = ");

      case Execution_Platform is
         when Platform_Native | Platform_None =>
            Write_Str ("native");

         when Platform_LEON_RTEMS =>
            Write_Str ("leon.rtems");

         when Platform_X86_LINUXTASTE =>
            Write_Str ("x86.linuxtaste");

         when Platform_X86_RTEMS =>
            Write_Str ("x86.rtems");

         when Platform_ARM_DSLINUX =>
            Write_Str ("arm.dslinux");

         when Platform_ARM_N770 =>
            Write_Str ("arm.n770");

         when Platform_LINUX64 =>
            Write_Str ("linux64");

         when Platform_LINUX32 =>
            Write_Str ("linux32");

         when others =>
            Display_Error ("Unsupported platform " & Execution_Platform'Img,
                           Fatal => True);
      end case;

      Write_Eol;
      Write_Str ("NEED_TRANSPORT = ");
      if Use_Transport then
         Write_Line ("yes");

--         if Transport_API = Transport_BSD_Sockets then
--            Write_Line
--              ("ACTUAL_TRANSPORT= po_hi_sockets.o ");
--         end if;
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
         for J in
           Name_Tables.First .. Name_Tables.Last (User_Source_Dirs) loop
            Write_Str ("-I");
            Write_Name (User_Source_Dirs.Table (J));

            exit when J = Name_Tables.Last (User_Source_Dirs);

            Write_Space;
         end loop;
         Write_Eol;
      else
         Write_Str (".");
         Write_Eol;
      end if;

      Write_Line ("include $(RUNTIME_PATH)/make/Makefile.common");
   end PolyORB_HI_C_Makefile;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      Instance_Root : Node_Id;
      Success       : Boolean := True;

      procedure Generate_PolyORB_HI_C_Makefile is new
        Build_Utils.Makefiles.Generate (PolyORB_HI_C_Makefile);

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

      if Remove_Generated_Sources then
         Build_Utils.Makefiles.Clean (Instance_Root);

      else
         --  Create the source files

         C_Tree.Generator.Generate (C_Root);

         --  Generate the Makefiles

         Generate_PolyORB_HI_C_Makefile (Instance_Root);

         --  If we have to generate the ASN1 deployment file, then
         --  we enter the directory that contains the generate
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
            Success := Execute_Regression_Test (Scenario_Dir.all,
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
            Success := Execute_Coverage_Test (Integer (Timeout))
              and then Success;
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
         case Getopt ("* b z ec er o: perf asn1") is
            when ASCII.NUL =>
               exit;

            when 'a' =>
               if Full_Switch = "asn1" then
                  Generate_ASN1_Deployment := True;
               end if;

            when 'b' =>
               Compile_Generated_Sources := True;

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
