------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                          O C A R I N A _ C M D                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2004-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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

--  This program is used to drive Ocarina, it is a wrapper to all
--  functions provided by the library.

with Errors;    use Errors;
with Locations; use Locations;
with Ocarina.Namet;     use Ocarina.Namet;
with Ocarina.Output;    use Ocarina.Output;
with Ocarina.Types;     use Ocarina.Types;

with Ada.Text_IO;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Ocarina;                          use Ocarina;
with Ocarina.AADL_Values;              use Ocarina.AADL_Values;
with Ocarina.Analyzer;                 use Ocarina.Analyzer;
with Ocarina.Backends;                 use Ocarina.Backends;
with Ocarina.Backends.PO_HI_C;
with Ocarina.Backends.PO_HI_Ada;
with Ocarina.Backends.Execution_Tests; use Ocarina.Backends.Execution_Tests;
with Ocarina.Cmd_Line;
with Ocarina.Configuration;            use Ocarina.Configuration;
with Ocarina.Files;                    use Ocarina.Files;
with Ocarina.Instances;                use Ocarina.Instances;
with Ocarina.Instances.Queries;        use Ocarina.Instances.Queries;
with Ocarina.ME_AADL.AADL_Tree.Nodes;  use Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nutils; use Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Options;                  use Ocarina.Options;
with Ocarina.Parser;                   use Ocarina.Parser;
with Ocarina.Property_Sets;            use Ocarina.Property_Sets;
with Ocarina.FE_AADL.Parser;           use Ocarina.FE_AADL.Parser;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Scripts;                  use Ocarina.Scripts;

procedure Ocarina_Cmd is

   procedure Parse_Scenario_Files;
   --  Parse a set of scenario file and updates the global variable
   --  according to the extracted information.

   AADL_Root             : Node_Id     := No_Node;
   Success               : Boolean     := True;
   File_Name             : Name_Id;
   Buffer                : Location;
   Language              : Name_Id;

   -------------------------
   -- Parse_Scenario_File --
   -------------------------

   --  Example of an AADL scenario:

   --  -----------------
   --  -- FILE_1.aadl --
   --  -----------------

   --  system RMA
   --  properties
   --    Ocarina_Config::AADL_Files => ("rma.aadl");
   --    --  "rma.aadl" contains common AADL components (processes,
   --    --  threads, data types)
   --
   --    Ocarina_Config::Needed_Property_Sets =>
   --      (value (Ocarina_Config::ARAO),
   --       value (Ocarina_Config::Cheddar_Properties));
   --    --  The non standard predefined property sets needed by the
   --    --  application.
   --  end RMA;

   --  -----------------
   --  -- FILE_2.aadl --
   --  -----------------

   --  system implementation RMA.Impl_C
   --  properties
   --    Ocarina_Config::AADL_Files +=> ("software_c.aadl");
   --    --  Note that this is an additive property
   --    --  association.
   --
   --    Ocarina_Config::Generator => PolyORB_HI_C;
   --    --  The code generator
   --  end RMA.Impl_C;

   --  -----------------
   --  -- FILE_3.aadl --
   --  -----------------

   --  system implementation RMA.Impl_Ada
   --  properties
   --    Source_Text +=> ("software_ada.aadl");
   --    --  Note that this is an additive property
   --    --  association.
   --
   --    Ocarina_Config::Generator => PolyORB_HI_Ada;
   --    --  The code generator
   --  end RMA.Impl_Ada;

   --  Calling the compiler on FILE_1.aadl and FILE_2.aadl will
   --  generate C code for the Poly_ORB-HI framework starting from
   --  "rma.aadl" and "software_c.aadl".

   --  Calling the compiler on FILE_1.aadl and FILE_3.aadl will
   --  generate Ada code for the Poly_ORB-HI framework starting from
   --  "rma.aadl" and "software_ada.aadl".

   procedure Parse_Scenario_Files is
      AADL_Root              : Node_Id := No_Node;
      Instance_Root          : Node_Id := No_Node;
      Root_System            : Node_Id := No_Node;
      Source_Files           : List_Id;
      Ref_Files              : List_Id;
      Needed_PS              : List_Id;
      Enabled_Anx            : List_Id;
      Use_CL                 : Boolean := False;
      Used_Generator_Options : List_Id;
      Dirname                : Name_Id;
      Success                : Boolean := False;

      The_Backend : Name_Id := No_Name;
      --  The current code generator

      Current_Scenario_Dirname : Name_Id := No_Name;
      --  The current dirname of all the scenario files.

      AADL_Version_Name : Name_Id := No_Name;
      --  The AADL version under which the model will be parsed

      Temp_AADL_Version : AADL_Version_Type;

      Ocarina_Config : constant String := "ocarina_config";
      --  The property set containing the Ocarina configuration
      --  properties.

      AADL_Files : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::aadl_files");
      Use_Components_Library : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::use_components_library");
      Referencial_Files : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::referencial_files");
      The_Generator : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::generator");
      Generator_Options : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::generator_options");
      Predefined_PS : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::needed_property_sets");
      RS_Name : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::root_system_name");
      AADL_Version : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::aadl_version");
      Timeout_Property : constant Name_Id :=
        Get_String_Name (Ocarina_Config & "::timeout_property");
      Enable_Annexes   : constant Name_Id
        := Get_String_Name (Ocarina_Config & "::enable_annexes");

      -------------------------------
      -- Extract_Referencial_Files --
      -------------------------------

      procedure Extract_Referencial_Files
        (Ref_Files :        List_Id;
         Ref_Map   : in out String_String_Maps.Map)
      is
         N : Node_Id;
         Position : String_String_Maps.Cursor;
      begin
         if not Is_Empty (Ref_Files) then
            N := First_Node (Ref_Files);
            while Present (N) loop
               declare
                  App_Name : constant String :=
                    Image (Value (N), Quoted => False);
               begin
                  N := Next_Node (N);
                  if Present (N) then
                     declare
                        File : constant String :=
                          Image (Value (N), Quoted => False);
                     begin
                        Write_Line ("Inserting : " & App_Name & " / " & File);
                        String_String_Maps.Insert
                          (Container => Ref_Map,
                           Key       => App_Name,
                           New_Item  => File,
                           Position  => Position,
                           Inserted  => Success);
                     end;
                  else
                     Exit_On_Error
                       (True,
                        "Missing parameters in " &
                        "Referencial_Files property");
                  end if;
                  N := Next_Node (N);
               end;
            end loop;
         end if;
      end Extract_Referencial_Files;

      --------------------------
      -- Extract_Source_Files --
      --------------------------

      procedure Extract_Source_Files
        (Source_Files : List_Id;
         Needed_PS    : List_Id);
      --  FIXME : Update the sources with the full path of the given AADL
      --  source file list and the needed predefined property sets (???).

      procedure Extract_Source_Files
        (Source_Files : List_Id;
         Needed_PS    : List_Id)
      is
         N : Node_Id;

      begin
         if not Is_Empty (Needed_PS) then
            N := First_Node (Needed_PS);

            while Present (N) loop
               declare
                  P         : Name_Id;
                  File_Name : constant String :=
                    Image (Value (N), Quoted => False);
               begin
                  Set_Str_To_Name_Buffer (File_Name);
                  Set_Str_To_Name_Buffer
                    (Image
                       (Ocarina_Property_Set_Type'Value
                          ("O_" & Name_Buffer (1 .. Name_Len))));
                  P := Name_Find;

                  Get_Name_String (Default_Library_Path);
                  Get_Name_String_And_Append (P);
                  Add_Str_To_Name_Buffer (".aadl");

               exception
                  when others =>
                     --  If we fail here, this means that the user gave a
                     --  custom property set file, assume that this file
                     --  is located in the data directory.

                     --  Build the full-path file name
                     Get_Name_String (Default_Library_Path);
                     Add_Str_To_Name_Buffer (File_Name & ".aadl");
               end;

               Ocarina.Files.Add_File_To_Parse_List
                 (Name_Find, Add_Suffix => True);

               N := Next_Node (N);
            end loop;
         end if;

         N := First_Node (Source_Files);
         while Present (N) loop
            declare
               File_Name : constant String :=
                 Image (Value (N), Quoted => False);
            begin
               Get_Name_String (Current_Scenario_Dirname);
               Add_Str_To_Name_Buffer (File_Name);
               Ocarina.Files.Add_File_To_Parse_List
                 (Name_Find, Add_Suffix => True);
               N := Next_Node (N);
            end;
         end loop;
      end Extract_Source_Files;

      package OIQ renames Ocarina.Instances.Queries;

      F : Types.Int;
      N : Node_Id;

   begin
      Current_Scenario_Dirname := No_Name;

      AADL_Root := Process_Predefined_Property_Sets (AADL_Root);

      if Ocarina.AADL_Version = AADL_V2 then
         Sources.Append (Get_String_Name ("ocarina_library.aadl"));
      end if;

      F := Sources.First;

      loop
         Dirname :=
           Get_String_Name
             (Dir_Name
                (Normalize_Pathname (Get_Name_String (Sources.Table (F)))));

         if Current_Scenario_Dirname = No_Name then
            Current_Scenario_Dirname := Dirname;
            Scenario_Dir             :=
              new String'(Get_Name_String (Current_Scenario_Dirname));
         end if;

         File_Name := Search_File (Sources.Table (F));
         Exit_On_Error
           ((File_Name = No_Name),
            "Cannot find file " & Get_Name_String (Sources.Table (F)));
         Buffer := Load_File (File_Name);
         if Verbose then
            Write_Line ("Loading file " & Get_Name_String (Sources.Table (F)));
         end if;

         Exit_On_Error
           ((File_Name = No_Name),
            "Cannot read file " & Get_Name_String (Sources.Table (F)));
         AADL_Root := Parse (Language, AADL_Root, Buffer);
         Exit_On_Error (No (AADL_Root), "Cannot parse AADL specifications");

         exit when F = Sources.Last; -- XXX Sources.Last may be modified
         F := F + 1;
      end loop;

      --  Analyze the AADL tree

      Success := Analyze (Language, AADL_Root);
      Exit_On_Error (not Success, "Cannot analyze AADL scenarios");

      --  Instantiate the AADL tree

      Instance_Root := Instantiate_Model (AADL_Root);
      Exit_On_Error (No (Instance_Root), "Cannot instantiate AADL scenarios");

      --  Every thing is fine, extract the information from the
      --  scenario model.

      Root_System :=
        Ocarina.ME_AADL.AADL_Instances.Nodes.Root_System (Instance_Root);

      --  Extract the generator

      Exit_On_Error
        (not Is_Defined_Enumeration_Property (Root_System, The_Generator),
         "You must specify a code generator");

      The_Backend := Get_Enumeration_Property (Root_System, The_Generator);

      --  Extract the AADL files to be parsed

      Exit_On_Error
        (not Is_Defined_List_Property (Root_System, AADL_Files),
         "AADL source files have to be specified by" &
         " means of the standard property" &
         " ""Source_Text""");

      Source_Files := Get_List_Property (Root_System, AADL_Files);

      --  Extract the referencial files for regression tests

      if Is_Defined_List_Property (Root_System, Referencial_Files) then
         Ref_Files := Get_List_Property (Root_System, Referencial_Files);
      else
         Ref_Files := No_List;
      end if;

      --  Extract the timeout used to stop the test

      if OIQ.Is_Defined_Integer_Property (Root_System, Timeout_Property) then
         Timeout := OIQ.Get_Integer_Property (Root_System, Timeout_Property);
      end if;

      --  Extract the predefined property sets needed by the
      --  application

      if Is_Defined_List_Property (Root_System, Predefined_PS) then
         Needed_PS := Get_List_Property (Root_System, Predefined_PS);
      else
         Needed_PS := No_List;
      end if;

      if Is_Defined_Boolean_Property (Root_System, Use_Components_Library) then
         Use_CL := True;
      else
         Use_CL := False;
      end if;

      --  See what annexes are enabled for this model. If none are
      --  given, assume the user want to disable them all.

      if Is_Defined_List_Property (Root_System, Enable_Annexes) then
         Enabled_Anx := Get_List_Property (Root_System, Enable_Annexes);
      else
         Enabled_Anx := No_List;
      end if;

      declare
         N : Node_Id;
      begin
         if not Is_Empty (Enabled_Anx) then
            N := First_Node (Enabled_Anx);

            Reset_Annex_Action;

            while Present (N) loop
               declare
                  A : constant String := Get_Name_String
                    (Name (Identifier (N)));
               begin
                  if A = "annex_all" then
                     Unset_Annex_Action (Disable_ALL);
                     Unset_Annex_Action (Disable_REAL);
                     Unset_Annex_Action (Disable_BA);
                     Unset_Annex_Action (Disable_EMA);

                  elsif A = "annex_none" then
                     null;

                  elsif A = "behavior_specification" then
                     Unset_Annex_Action (Disable_BA);

                  elsif A = "real_specification" then
                     Unset_Annex_Action (Disable_REAL);

                  elsif A = "emv2" then
                     Unset_Annex_Action (Disable_EMA);

                  else
                     raise Program_Error
                       with "Internal error : make sure you handle "
                       & "with all annexes declared in ocarina_config.aadl";
                  end if;
               end;

               N := Next_Node (N);
            end loop;
         end if;
      end;

      --  Extract the generator options.

      if Is_Defined_List_Property (Root_System, Generator_Options) then
         Used_Generator_Options :=
           Get_List_Property (Root_System, Generator_Options);
      else
         Used_Generator_Options := No_List;
      end if;

      --  Process options.

      if not Is_Empty (Used_Generator_Options) then
         N := First_Node (Used_Generator_Options);

         while Present (N) loop
            declare
               P      : Name_Id;
               Option : constant String := Image (Value (N), Quoted => False);
            begin

               Set_Str_To_Name_Buffer (Option);
               P := Name_Find;

               if P = Get_String_Name ("gprof") then
                  Ocarina.Backends.PO_HI_C.Set_Performance_Analysis (True);
               end if;

               if P = Get_String_Name ("asn1") then
                  Ocarina.Backends.PO_HI_C.Set_ASN1_Deployment (True);
                  Ocarina.Backends.PO_HI_Ada.Set_ASN1_Deployment (True);
               end if;

               N := Next_Node (N);
            end;
         end loop;
      end if;

      --  Extract the AADL version

      if Is_Defined_Enumeration_Property (Root_System, AADL_Version) then
         AADL_Version_Name :=
           Get_Enumeration_Property (Root_System, AADL_Version);

         if Get_Name_String (AADL_Version_Name) = "aadlv1" then
            Temp_AADL_Version := Ocarina.AADL_V1;
         elsif Get_Name_String (AADL_Version_Name) = "aadlv2" then
            Temp_AADL_Version := Ocarina.AADL_V2;
         else
            raise Program_Error;
         end if;
      else
         Temp_AADL_Version := Ocarina.AADL_V1;
      end if;

      Ocarina.AADL_Version := Temp_AADL_Version;

      Sources.Free;
      Sources.Init;
      Extract_Source_Files (Source_Files, Needed_PS);

      if Use_CL then
         Set_Str_To_Name_Buffer ("ocarina_components.aadl");
         Ocarina.Files.Add_File_To_Parse_List (Name_Find, Add_Suffix => True);
         Set_Str_To_Name_Buffer ("base_types.aadl");
         Ocarina.Files.Add_File_To_Parse_List (Name_Find, Add_Suffix => True);
      end if;

      Extract_Referencial_Files (Ref_Files, Ref_Map);

      --  Extract the name of the root of the instance tree

      if Is_Defined_String_Property (Root_System, RS_Name) then
         Root_System_Name := Get_String_Property (Root_System, RS_Name);
      end if;

      --  Reset Ocarina to have a clean set up for the next step

      declare
         The_Backend_Name : constant String := Get_Name_String (The_Backend);
         Result           : Argument_List_Access :=
           new Argument_List
           (Integer (Sources.First) .. Integer (Sources.Last));
         Root_System_Name_Ptr : String_Access;
      begin
         if Root_System_Name /= No_Name then
            Root_System_Name_Ptr :=
              new String'(Get_Name_String (Root_System_Name));
         else
            Root_System_Name_Ptr := new String'("");
         end if;

         for J in Sources.First .. Sources.Last loop
            Result (Integer (J)) :=
              new String'(Get_Name_String (Sources.Table (J)));
         end loop;

         Ocarina.Configuration.Reset_Modules;
         Ocarina.Reset;
         --         Ocarina.Files.Sources.Init;

         Ocarina.Initialize;
         Language             := Get_String_Name ("aadl");
         Ocarina.AADL_Version := Temp_AADL_Version;
         Set_Current_Backend_Name (The_Backend_Name);

         Ocarina.Configuration.Init_Modules;
         Sources.Free;
         Sources.Init;

         --  Restore the root system name

         if Root_System_Name_Ptr.all /= "" then
            Set_Str_To_Name_Buffer (Root_System_Name_Ptr.all);
            Root_System_Name := Name_Find;
         else
            Root_System_Name := No_Name;
         end if;

         for J in Result'Range loop
            Set_Str_To_Name_Buffer (Result (J).all);
            Ocarina.Files.Add_File_To_Parse_List
              (Name_Find, Add_Suffix => True);
         end loop;

         --  Avoid memory leaks

         Free (Result);
         Free (Root_System_Name_Ptr);
      end;
   end Parse_Scenario_Files;

begin
   --  Init

   Ocarina.Initialize;
   Language             := Get_String_Name ("aadl");
   Default_AADL_Version := Get_Default_AADL_Version;
   AADL_Version         := Default_AADL_Version;

   --  Process the command line

   Ocarina.Cmd_Line.Process;

   --  Initialization Modules

   Ocarina.Configuration.Init_Modules;

   --  Process command line parameters

   if Verbose then
      Set_Standard_Error;
      if not Display_Version then
         Version;
      end if;
   end if;

   if Display_Version then
      Version;
      OS_Exit (0);

   elsif Show_Search_Directory then
      Write_Line (Get_Name_String (Default_Library_Path));
      OS_Exit (0);

   elsif Get_Current_Action = List_Backends then
      Write_Backends (1);
      OS_Exit (0);

   elsif Get_Current_Action = Shell then
      Ocarina_Shell;
      OS_Exit (0);

   elsif Sources.Last = 0 then
      Ada.Text_IO.Put ("No AADL files given, ");
      Try_Help;
      OS_Exit (1);
   end if;

   --  If not action has been set, set it to analyse the models only

   if Get_Current_Action = None then
      Set_Current_Action (Analyze_Model);
   end if;

   if Get_Current_Action = Parse_Scenario_Files_First then
      Parse_Scenario_Files;
      Reset_Current_Action;
      Set_Current_Action (After_Scenario_Action);
   end if;

   --  Parse the AADL files

   AADL_Root := No_Node;
   declare
      F : Types.Int := Sources.First;
   begin
      loop
         File_Name := Search_File (Sources.Table (F));
         Exit_On_Error
           ((File_Name = No_Name),
            "Cannot find file " & Get_Name_String (Sources.Table (F)));
         Buffer := Load_File (File_Name);
         Exit_On_Error
           ((File_Name = No_Name),
            "Cannot read file " & Get_Name_String (Sources.Table (F)));

         if Verbose then
            Write_Line ("Loading file " & Get_Name_String (Sources.Table (F)));
         end if;

         AADL_Root := Parse (Language, AADL_Root, Buffer);
         Exit_On_Error (No (AADL_Root), "Cannot parse AADL specifications");
         exit when F = Sources.Last;
         F := F + 1;
      end loop;
   end;
   if Verbose then
      Write_Line ("Loading of all files done");
   end if;

   Success := Analyze (Language, AADL_Root);
   Exit_On_Error (not Success, "Cannot analyze AADL specifications");

   if Verbose then
      Write_Line ("Model parsing: completed");
      Write_Eol;
   end if;

   case Get_Current_Action is
      when Analyze_Model =>
         null;

      when Instantiate_Model =>
         AADL_Root := Instantiate_Model (AADL_Root);
         Exit_On_Error (No (AADL_Root), "Cannot instantiate AADL models");
         if Verbose then
            Set_Standard_Error;
            Write_Line ("Model instantiation: completed");
            Write_Eol;
            Set_Standard_Output;
         end if;

      when Generate_Code =>
         Generate_Code (AADL_Root);
         if Verbose then
            Set_Standard_Error;
            Write_Line ("Code generation: completed");
            Write_Eol;
            Set_Standard_Output;
         end if;

      when others =>
         null;
   end case;

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      --  Expected when Getopt processes -h or --help
      null;

   when E : others =>
      Display_Bug_Box (E);

end Ocarina_Cmd;
