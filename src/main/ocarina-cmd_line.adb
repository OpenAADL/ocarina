------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                     O C A R I N A . C M D _ L I N E                      --
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

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Command_Line.Response_File;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Ocarina.Backends;          use Ocarina.Backends;
with Ocarina.Options;           use Ocarina.Options;
with Ocarina.Files;             use Ocarina.Files;
with Ocarina.Output;            use Ocarina.Output;
with Ocarina.Namet;             use Ocarina.Namet;
with Utils;                     use Utils;
with Ocarina.FE_AADL.Parser;    use Ocarina.FE_AADL.Parser;
with Ocarina.FE_REAL.Parser;    use Ocarina.FE_REAL.Parser;
with Ocarina.Scripts;           use Ocarina.Scripts;
with Ocarina.Analyzer.REAL;     use Ocarina.Analyzer.REAL;
with Ocarina.Backends.POK_C;
with Ocarina.Backends.PO_HI_Ada;

package body Ocarina.Cmd_Line is

   Ocarina_Options : Command_Line_Configuration;
   --  Stores all information on command line flags

   procedure Parse_Command_Line
     (Switch : String; Parameter : String; Section : String);
   --  Callback for complex command line parameters, required by
   --  GNAT.Command_Line.Getopt.

   procedure Parse_Source_Filename (S : String);
   --  Parse source filenames, S denotes one file name

   procedure Register_Command_Line_Parameters;
   --  Define all supported command line parameters

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line
     (Switch : String; Parameter : String; Section : String)
   is
      pragma Unreferenced (Section);
   begin
      if Switch = "-aadlv" then
         if Parameter = "1" then
            AADL_Version := AADL_V1;
         elsif Parameter = "2" then
            AADL_Version := AADL_V2;
         else
            raise Invalid_Switch;
         end if;

      elsif Switch = "-boundt_process" then
         Boundt_Process := To_Lower (Get_String_Name (Parameter));

      elsif Switch = "-disable-annexes" then
         Reset_Annex_Action;
         Process_Annex_Action (Parameter);

      elsif Switch = "-g"  then
         Set_Current_Action (Generate_Code);
         Set_Current_Backend_Name (Parameter);

      elsif Switch = "-k" then
         Backends.POK_C.Set_POK_Flavor (Parameter);

      elsif Switch = "-i" then
         Set_Current_Action (Instantiate_Model);

      elsif Switch = "-I" then
         if Parameter'Length > 0 then
            Add_Library_Path (Parameter);
         end if;

      elsif Switch = "--list-backends" then
         Set_Current_Action (List_Backends);

      elsif Switch = "-o" then
         Output_Filename := Get_String_Name (Parameter);
         Generated_Sources_Directory := Output_Filename;

      elsif Switch = "-p" then
         After_Scenario_Action := Instantiate_Model;

      elsif Switch = "-r" then
         Root_System_Name := To_Lower (Get_String_Name (Parameter));

      elsif Switch = "-real_lib" then
         REAL_Libs.Append (Get_String_Name (Parameter));

      elsif Switch = "-real_theorem" then
         Main_Theorem := Get_String_Name (Parameter);

      elsif Switch = "-t" then
         Set_Current_Action (Shell);
         declare
            N    : constant String := Get_Argument;
            File : Ada.Text_IO.File_Type;
         begin
            if N'Length > 0 then
               Ada.Text_IO.Open (File, Ada.Text_IO.In_File, N);
               Ada.Text_IO.Set_Input (File);
               Ocarina.Scripts.Standard_Input := False;
            end if;
         end;

      elsif Switch = "-x" then
         Use_Scenario_File := True;
         Set_Current_Action (Parse_Scenario_Files_First);

      else
         Parse_Source_Filename (Switch);
      end if;

   exception
      when GNAT.Command_Line.Invalid_Parameter =>
         Write_Line
           (Base_Name (Command_Name) &
            ": invalid parameter for switch -" & Switch);
         Write_Eol;
         OS_Exit (1);
   end Parse_Command_Line;

   -----------------------------
   -- Parse_Source_Filename --
   -----------------------------

   procedure Parse_Source_Filename (S : String) is
   begin
      if S (S'First) = '@' then
         declare
            Files : constant Ada.Command_Line.Response_File
              .Argument_List :=
              Ada.Command_Line.Response_File.Arguments_From
              (S (S'First + 1 .. S'Last));
         begin
            for J in Files'Range loop
               Set_Str_To_Name_Buffer (Files (J).all);
               Ocarina.Files.Add_File_To_Parse_List
                 (Name_Find,
                  Add_Suffix => False);
            end loop;
            --  Free (Files);
         end;

      else
         Set_Str_To_Name_Buffer (S);
         Ocarina.Files.Add_File_To_Parse_List
           (Name_Find,
            Add_Suffix => False);
      end if;
   end Parse_Source_Filename;

   --------------------------------------
   -- Register_Command_Line_Parameters --
   --------------------------------------

   procedure Register_Command_Line_Parameters is
   begin
      Set_Usage (Ocarina_Options, Usage => "[switches] <aadl_files>");

      --  Note: the order of call to Define_Switch dictates the order
      --  of information when calling GNAT.Command_Line.Display_Help.

      --  --help flag, just so that it appears in the online help
      Define_Switch (Ocarina_Options,
                     Switch => "-h", Long_Switch => "--help",
                     Help => "Display help and exit");

      --  --version flag
      Define_Switch (Ocarina_Options, Display_Version'Access,
                     Switch => "", Long_Switch => "--version",
                     Help => "Display version and exit");

      --  --verbose flag
      Define_Switch (Ocarina_Options, Verbose'Access,
                     "-v", Long_Switch => "--verbose",
                     Help => "Output extra verbose information");

      --  -q flag
      Define_Switch (Ocarina_Options, Quiet'Access,
                     "-q",
                     Help => "Quiet mode (default)", Value => False);

      --  -d flag
      Define_Switch (Ocarina_Options, Debug_Mode'Access, "-d",
                     Help => "Debug mode");

      --  -s flag
      Define_Switch (Ocarina_Options, Show_Search_Directory'Access,
                     "-s",
                     Help => "Output default search directory, then exit");

      --  -aadlv? flag
      Define_Switch
        (Ocarina_Options, "-aadlv?",
         Help => "AADL version, ARG = 1 for AADL 1.0, 2 for AADL 2.x");

      --  -f flag
      Define_Switch
        (Ocarina_Options, Add_Pre_Prop_Sets'Access, "-f",
         Value => True,
         Help => "Parse predefined non-standard property sets");

      --  -disable-annexes flag
      Define_Switch (Ocarina_Options, "-disable-annexes=",
                     Help => "Deactivate annex ARG");

      --  -r flag
      Define_Switch (Ocarina_Options, "-r:", Help => "Use ARG as root system");

      --  -o flag
      Define_Switch (Ocarina_Options, "-o:",
                     Help => "Specify output file/directory");

      --  -y flag
      Define_Switch (Ocarina_Options, Auto_Load_AADL_Files'Access, "-y",
                     Help => "Automatically load AADL files");

      --  -I flag
      Define_Switch (Ocarina_Options, "-I:",
                     Help => "Add ARG to the directory search list");

      --  -p flag
      Define_Switch (Ocarina_Options, "-p",
                     Help => "Parse and instantiate the model");

      --  -i flag
      Define_Switch (Ocarina_Options, "-i",
                     Help => "Instantiate the model");

      --  -x flag
      Define_Switch (Ocarina_Options, "-x",
                     Help => "Parse AADL file as an AADL scenario file");

      --  -g flag
      Define_Switch (Ocarina_Options, "-g:",
                     Help => "Generate code using Ocarina backend 'ARG'");

      --  --list-backends flag
      Define_Switch (Ocarina_Options, Long_Switch => "--list-backends",
                     Help => "List available backends");

      --  --spark2014 flag
      Define_Switch
        (Ocarina_Options,
         Ocarina.Backends.PO_HI_Ada.Add_SPARK2014_Annotations'Access,
         Long_Switch => "--spark2014",
         Help => "Generate SPARK2014 annotations");

      --  -b flag
      Define_Switch (Ocarina_Options,
                     Backends.Compile_Generated_Sources'Access, "-b",
                     Help => "Compile generated code");

      --  -z flag
      Define_Switch (Ocarina_Options,
                     Backends.Remove_Generated_Sources'Access, "-z",
                     Help => "Clean code generated");

      --  -k flag
      Define_Switch (Ocarina_Options, "-k:",
                     Help => "Set POK flavor (arinc653/deos/pok/vxworks)");

      --  -t flag
      Define_Switch (Ocarina_Options, "-t",
                     Help => "Run Ocarina in terminal interactive mode");

      --  -real_theorem flag
      Define_Switch (Ocarina_Options, "-real_theorem:",
                     Help => "Name of the main REAL theorem to evaluate");

      --  -real_lib flag
      Define_Switch (Ocarina_Options, "-real_lib:",
                     Help => "Add external library of REAL theorems");

      --  -real_continue_eval flag
      Define_Switch
        (Ocarina_Options, Continue_Evaluation'Access,
         "-real_continue_eval",
         Help => "Continue evaluation of REAL theorems after first failure "
           & "(REAL backend)");

      --  -boundt_process flag
      Define_Switch
        (Ocarina_Options, "-boundt_process:",
         Help => "Generate .tpo file for process ARG (Bound-T backend)");

      --  -ec flag
      Define_Switch (Ocarina_Options, Backends.Do_Coverage_Test'Access,
                     "-ec", Help => "Compute coverage metrics");

      --  -er flag
      Define_Switch (Ocarina_Options, Backends.Do_Regression_Test'Access,
                     "-er", Help => "Execute system");

      --  -asn1 flag
      Define_Switch
        (Ocarina_Options, Backends.Generate_ASN1_Deployment'Access,
         "-asn1",
         Help => "Generate ASN1 deployment file (PolyORB-HI-C only)");

      --  -perf flag
      Define_Switch
        (Ocarina_Options, Backends.Add_Performance_Analysis'Access,
         "-perf", Help => "Enable profiling with gprof (PolyORB-HI-C only)");

   end Register_Command_Line_Parameters;

   -------------
   -- Process --
   -------------

   procedure Process is
   begin
      Register_Command_Line_Parameters;
      Getopt (Ocarina_Options, Parse_Command_Line'Unrestricted_Access);

      --  After this call to Getopt, the command line has elements
      --  that are not caught as regular switches. We assume these are
      --  AADL filenames. They are passed to Parse_Source_Filename.

      loop
         declare
            S : constant String := Get_Argument (Do_Expansion => True);
         begin
            exit when S'Length = 0;
            Parse_Source_Filename (S);
         end;
      end loop;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         OS_Exit (1);

   end Process;

end Ocarina.Cmd_Line;
