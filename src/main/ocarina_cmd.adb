------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                          O C A R I N A _ C M D                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2004-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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
with Utils;     use Utils;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Command_Line.Response_File;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.IO_Exceptions;
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
with Ocarina.FE_REAL;                  use Ocarina.FE_REAL;
with Ocarina.ME_REAL.Tokens;
with Ocarina.Transfo.Fusions;          use Ocarina.Transfo.Fusions;
with Ocarina.Transfo.Move;             use Ocarina.Transfo.Move;
with Ocarina.Transfo.Optim;            use Ocarina.Transfo.Optim;

with Ocarina.ME_AADL.AADL_Instances.Nodes;

procedure Ocarina_Cmd is

   procedure Process_Command_Line
     (Root_System_Name : out Name_Id;
      Success          : out Boolean);
   --  Process the command line to extract the options for the
   --  different modules of Ocarina.

   procedure Usage;
   --  Display a message describing the usage of Ocarina

   procedure Ocarina_Shell;
   --  Launch Ocarina interactive mode

   procedure Parse_Scenario_Files;
   --  Parse a set of scenario file and updates the global variable
   --  according to the extracted information.

   After_Scenario_Action : Action_Kind := Generate_Code;
   Standard_Input        : Boolean     := True;
   AADL_Root             : Node_Id     := No_Node;
   Success               : Boolean     := True;
   File_Name             : Name_Id;
   Buffer                : Location;
   Language              : Name_Id;
   Position              : String_String_Maps.Cursor;

   -------------------
   -- Ocarina_Shell --
   -------------------

   procedure Ocarina_Shell is

      use Ada.Text_IO;

      function "+" (S : String) return String_Access;
      procedure Show_Help;
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);
      function Next return String;
      function Argument (Index : Natural) return String_Access;
      function Count (Prompt : String := "> ") return Natural;

      Syntax_Error : exception;

      type Command is
        (Help,
         Analyze,
         Instantiate,
         Generate,
         Load,
         Status,
         Fusion,
         Move,
         Optimize,
         Brute_Optimize,
         Version,
         Quit);

      Args    : array (1 .. 16) of String_Access;
      Argc    : Natural;
      Line    : String (1 .. 1024);
      Last    : Natural;
      Scan    : Natural;
      Argv    : String_Access;
      Cmmd    : Command;
      Success : Boolean;

      ---------
      -- "+" --
      ---------

      function "+" (S : String) return String_Access is
      begin
         return new String'(S);
      end "+";

      ---------------
      -- Show_Help --
      ---------------

      Help_Messages : constant array (Command) of String_Access :=
        (Help           => +"print this message",
         Analyze        => +"analyse model",
         Instantiate    => +"instantiate model",
         Generate       => +"generate code",
         Load           => +"load and parse file given as argument",
         Fusion         => +"fusion threads",
         Move           => +"move a thread",
         Optimize       => +"optimize model, using greedy algorithm",
         Brute_Optimize => +"optimize model, using brute force",
         Status         => +"print configuration",
         Version        => +"print Ocarina version information",
         Quit           => +"quit this shell");

      procedure Show_Help is
      begin
         for J in Help_Messages'Range loop
            Write_Line (J'Img & ASCII.HT & Help_Messages (J).all);
         end loop;
      end Show_Help;

      ------------------
      -- Print_Status --
      ------------------

      procedure Print_Status is
      begin
         Write_Line ("AADL version: " & Ocarina.AADL_Version'Img);
         Write_Line
           ("Library Path: " & Get_Name_String (Default_Library_Path));
      end Print_Status;

      --------------
      -- Argument --
      --------------

      function Argument (Index : Natural) return String_Access is
      begin
         if Index > Argc then
            raise Constraint_Error;
         end if;
         return Args (Index);
      end Argument;

      -----------
      -- Count --
      -----------

      function Count (Prompt : String := "> ") return Natural is
      begin
         if Standard_Input then
            Put (Prompt);
         end if;

         begin
            Get_Line (Current_Input, Line, Last);
         exception
            when Ada.IO_Exceptions.End_Error =>
               --  This means the user hit CTRL-D or the script does
               --  not end with a QUIT command. Not harmful, we just
               --  simulate a QUIT.

               Argc := 1;
               if Args (Argc) /= null then
                  Free (Args (Argc));
               end if;
               Args (Argc) := new String'(Command'Image (Quit));
               return Argc;

            when E : others =>
               Write_Line ("raised " & Exception_Information (E));
               Write_Line (Exception_Message (E));
               raise;
         end;

         Scan := 1;
         Argc := 0;
         loop
            declare
               Arg : constant String := Next;
            begin
               exit when Arg = "";
               Argc := Argc + 1;
               if Args (Argc) /= null then
                  Free (Args (Argc));
               end if;
               Args (Argc) := new String'(Arg);
            end;
         end loop;
         return Argc;
      end Count;

      ----------
      -- Next --
      ----------

      function Next return String is
         use ASCII;

         F, L : Natural;
      begin
         while Scan <= Last
           and then (Line (Scan) = ' ' or else Line (Scan) = HT)
         loop
            Scan := Scan + 1;
         end loop;

         if Scan > Last then
            return "";
         end if;

         if Line (Scan) = '"' then -- "
            Scan := Scan + 1;
            F    := Scan;

            while Scan <= Last loop
               if Line (Scan) = '"' then --  "
                  L    := Scan - 1;
                  Scan := Scan + 1;
                  return Line (F .. L);

               elsif Line (Scan) = NUL then
                  return "";

               end if;

               Scan := Scan + 1;
            end loop;
            return "";

         else
            F := Scan;
            while Scan <= Last
              and then Line (Scan) /= ' '
              and then Line (Scan) /= HT
            loop
               L    := Scan;
               Scan := Scan + 1;
            end loop;
            return Line (F .. L);
         end if;
      end Next;

   begin
      if Standard_Input then
         Write_Line ("Ocarina shell, type help for information");
      end if;

      --  Console main loop: read inputs and process them

      <<Main>>
      loop
         Argc := Count;
         if Argc > 0
           and then Argument (1) /= null
           and then Argument (1).all (Argument (1).all'First) /= '#'
         then
            begin
               Argv := Argument (1);

               begin
                  Cmmd := Command'Value (Argv.all);
               exception
                  when Constraint_Error =>
                     raise Syntax_Error;
               end;

               case Cmmd is
                  when Help =>
                     Show_Help;

                  when Analyze =>
                     Success := Analyze (Language, AADL_Root);
                     if not Success then
                        Write_Line ("Cannot analyze AADL specifications");
                     else
                        Write_Line ("Model analyzed sucessfully");
                     end if;

                  when Instantiate =>
                     if Argc = 2 then
                        Root_System_Name :=
                          To_Lower (Get_String_Name (Argument (2).all));
                     end if;
                     AADL_Root := Instantiate_Model (AADL_Root);
                     if Present (AADL_Root) then
                        Write_Line ("Model instantiated sucessfully");
                     end if;

                  when Generate =>
                     if Argc /= 2 then
                        raise Syntax_Error;
                     end if;
                     Set_Current_Backend_Name (Argument (2).all);
                     Write_Line ("Generating code for " & Argument (2).all);
                     Generate_Code (AADL_Root);

                  when Load =>
                     if Argc /= 2 then
                        raise Syntax_Error;
                     end if;
                     Set_Str_To_Name_Buffer (Argument (2).all);

                     File_Name := Search_File (Name_Find);
                     if File_Name = No_Name then
                        Write_Line ("cannot find file " & Argument (2).all);
                        goto Main;
                     end if;

                     Buffer := Load_File (File_Name);
                     if File_Name = No_Name then
                        Write_Line ("cannot read file " & Argument (2).all);
                        goto Main;
                     end if;
                     AADL_Root := Parse (Language, AADL_Root, Buffer);
                     Exit_On_Error
                       (No (AADL_Root),
                        "cannot parse AADL specifications");

                     Write_Line
                       ("File " &
                        Argument (2).all &
                        " loaded and parsed sucessfully");

                  when Brute_Optimize =>
                     declare
                        Instance_Root : Node_Id;
                     begin
                        Instance_Root := Instantiate_Model (AADL_Root);
                        Exit_On_Error
                          (No (Instance_Root),
                           "Cannot instantiate AADL models");

                        Ocarina.Transfo.Optim.Init (Instance_Root);

                        Exhaustive_Space_Exploration (Instance_Root, Success);
                        Exit_On_Error
                          (not Success,
                           "cannot perform brute optimization on model");

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Optimize =>
                     declare
                        Instance_Root : Node_Id;
                     begin
                        Instance_Root := Instantiate_Model (AADL_Root);
                        Exit_On_Error
                          (No (Instance_Root),
                           "Cannot instantiate AADL models");

                        Ocarina.Transfo.Optim.Init (Instance_Root);

                        Greedy_Heuristic (Instance_Root, Success);
                        Exit_On_Error
                          (not Success,
                           "cannot perform optimization on model");

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Fusion =>
                     declare
                        Thread_To_Fusion_1 : Name_Id := No_Name;
                        Thread_To_Fusion_2 : Name_Id := No_Name;
                        Owner_Process      : Name_Id := No_Name;
                        --  Transformation-related variables

                        AADL_Instance : Node_Id;
                        New_Thread    : Node_Id;
                        Success       : Boolean;
                     begin
                        AADL_Instance := Instantiate_Model (AADL_Root);
                        Exit_On_Error
                          (No (AADL_Instance),
                           "Cannot instantiate AADL models");

                        Owner_Process := Get_String_Name (Argument (2).all);
                        Thread_To_Fusion_1 :=
                          Get_String_Name (Argument (3).all);
                        Thread_To_Fusion_2 :=
                          Get_String_Name (Argument (4).all);

                        Fusion_Threads
                          (AADL_Root,
                           Owner_Process,
                           Thread_To_Fusion_1,
                           Thread_To_Fusion_2,
                           New_Thread,
                           Success);

                        Exit_On_Error
                          (not Success,
                           "Cannot fusion the AADL threads");

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Move =>
                     declare
                        Thread_To_Move : Name_Id := No_Name;
                        Src_Process    : Name_Id := No_Name;
                        Dst_Process    : Name_Id := No_Name;
                        --  Transformation-related variables

                        AADL_Instance : Node_Id;
                     begin
                        AADL_Instance := Instantiate_Model (AADL_Root);
                        Exit_On_Error
                          (No (AADL_Instance),
                           "Cannot instantiate AADL models");

                        Thread_To_Move := Get_String_Name (Argument (2).all);
                        Src_Process    := Get_String_Name (Argument (3).all);
                        Dst_Process    := Get_String_Name (Argument (4).all);

                        Move_Thread (Thread_To_Move, Src_Process, Dst_Process);

                        Set_Current_Backend_Name ("aadl");
                        Generate_Code (AADL_Root);
                     end;

                  when Status =>
                     Print_Status;

                  when Version =>
                     Version;

                  when Quit =>
                     exit;
               end case;
            exception
               when Syntax_Error =>
                  Write_Line ("syntax error");

               when E : others =>
                  Write_Line ("raised " & Exception_Information (E));
                  Write_Line (Exception_Message (E));
            end;
         end if;
      end loop;
   end Ocarina_Shell;

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

      -------------------------------
      -- Extract_Referencial_Files --
      -------------------------------

      procedure Extract_Referencial_Files
        (Ref_Files :        List_Id;
         Ref_Map   : in out String_String_Maps.Map)
      is
         N : Node_Id;
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

         --  All scenario files have to be located in the same directory

         Exit_On_Error
           (Current_Scenario_Dirname /= Dirname,
            "Cannot locate scenario files in the directory");
         File_Name := Search_File (Sources.Table (F));
         Exit_On_Error
           ((File_Name = No_Name),
            "Cannot find file " & Get_Name_String (Sources.Table (F)));
         Buffer := Load_File (File_Name);
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

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line
     (Root_System_Name : out Name_Id;
      Success          : out Boolean)
   is
   begin
      Root_System_Name := No_Name;
      Success          := True;

      Initialize_Option_Scan;
      loop
         case Getopt
           ("* aadlv1 aadlv2 help o: c d g: " &
            "r: real_lib: real_theorem: boundt_process: " &
            "disable-annexes=: " &
            "i p q v V s x t?")
         is

            when 'a' =>
               if Full_Switch = "aadlv2" then
                  AADL_Version := AADL_V2;
               elsif Full_Switch = "aadlv1" then
                  AADL_Version := AADL_V1;
               else
                  raise Invalid_Switch;
               end if;

            when 'o' =>
               declare
                  D : constant String := Parameter;
               begin
                  if D'Length > 0 then
                     Output_Filename := Get_String_Name (D);
                  end if;
               end;

            when 'g' =>
               Set_Current_Action (Generate_Code);
               declare
                  G : constant String := Parameter;
               begin
                  if G'Length > 0 then
                     Set_Current_Backend_Name (G);
                  end if;
               end;

            when 'h' =>
               if Full_Switch /= "help" then
                  raise Invalid_Switch;
               end if;
               Set_Current_Action (Show_Help);

            when 'p' =>
               if Full_Switch = "p" then
                  After_Scenario_Action := Instantiate_Model;
               end if;

            when 'r' =>
               if Full_Switch = "r" then
                  declare
                     N : constant String := Parameter;
                  begin
                     if N'Length > 0 then
                        Root_System_Name := To_Lower (Get_String_Name (N));
                     end if;
                  end;
               end if;

            when 'i' =>
               Set_Current_Action (Instantiate_Model);

            when 'q' =>
               Quiet_Mode   := True;
               Verbose_Mode := False;

            when 'v' =>
               Quiet_Mode   := False;
               Verbose_Mode := True;

            when 'd' =>
               if Full_Switch = "disable-annexes=" then
                  Reset_Annex_Action;
                  Process_Annex_Action (Parameter);
               else
                  Debug_Mode := True;
               end if;

            when 'V' =>
               Set_Current_Action (Show_Version);
               exit;

            when 's' =>
               Set_Current_Action (Show_Libraries);

            --  Note: we continue parsing the command line
            --  parameters to know whether the user specified also
            --  an AADL version flag.

            when 'x' =>
               Use_Scenario_File := True;
               Set_Current_Action (Parse_Scenario_Files_First);

            when 't' =>
               Set_Current_Action (Shell);
               declare
                  N    : constant String := Get_Argument;
                  File : Ada.Text_IO.File_Type;
               begin
                  if N'Length > 0 then
                     Ada.Text_IO.Open (File, Ada.Text_IO.In_File, N);
                     Ada.Text_IO.Set_Input (File);
                     Standard_Input := False;
                  end if;
               end;

               exit;

            when ASCII.NUL =>
               exit;

            when others =>
               declare
                  S : constant String := Full_Switch;
               begin
                  if S (S'First) = '-' then
                     --  If there is a new switch, then the previous
                     --  files are discarded.
                     Sources.Init;

                  elsif S (S'First) = '@' then
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
               end;
         end case;
      end loop;

      --  If no file is given, print the usage function and exit

      if Sources.Last = 0
        and then Get_Current_Action /= Show_Version
        and then Get_Current_Action /= Show_Libraries
        and then Get_Current_Action /= Show_Help
        and then Get_Current_Action /= Shell
        and then Get_Current_Action /= Parse_Scenario_Files_First
      then
         Set_Current_Action (Show_Usage);

      elsif Sources.Last /= 0 and then Get_Current_Action = None then
         Set_Current_Action (Analyze_Model);
      end if;

   exception
      when Invalid_Options =>
         Write_Line
           (Base_Name (Command_Name) & ": invalid combination of options");
         Write_Eol;
         OS_Exit (1);

      when GNAT.Command_Line.Invalid_Switch =>
         Write_Line
           (Base_Name (Command_Name) &
            ": invalid switch " &
            Full_Switch &
            Parameter);
         Write_Eol;
         OS_Exit (1);

      when GNAT.Command_Line.Invalid_Parameter =>
         Write_Line
           (Base_Name (Command_Name) &
            ": invalid parameter for switch -" &
            Full_Switch);
         Write_Eol;
         OS_Exit (1);
   end Process_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      Exec_Suffix : String_Access := Get_Executable_Suffix;
   begin
      Set_Standard_Error;
      Write_Line ("Usage: ");
      Write_Line
        ("      " &
         Base_Name (Command_Name, Exec_Suffix.all) &
         " [options] files");
      Write_Line ("      OR");
      Write_Line
        ("      " & Base_Name (Command_Name, Exec_Suffix.all) & " -help");
      Write_Line ("  files are a non null sequence of AADL files");

      Write_Eol;
      Write_Line ("  General purpose options:");
      Write_Line ("   -V  Output Ocarina version, then exit");
      Write_Line ("   -s  Output Ocarina search directory, then exit");

      Write_Eol;
      Write_Line ("  Scenario file options:");
      Write_Line ("   -b  Generate and build code from the AADL model");
      Write_Line ("   -z  Clean code generated from the AADL model");
      Write_Line ("   -ec Execute the generated application code and");
      Write_Line ("       retrieve coverage information");
      Write_Line ("   -er Execute the generated application code and");
      Write_Line ("       verify that there is no regression");
      Write_Line ("   -p  Only parse and instantiate the application model");

      Write_Eol;
      Write_Line ("  Advanced user options:");
      Write_Line ("   -d  Debug mode for developpers");
      Write_Line ("   -q  Quiet mode (default)");
      Write_Line ("   -t  [script] Run Ocarina in terminal interactive mode.");
      Write_Line ("       If a script is given, interpret it then exit.");
      Write_Line ("   -v  Verbose mode for users");
      Write_Line ("   -x  Parse AADL file as an AADL scenario file");

      Ocarina.FE_AADL.Usage;
      Ocarina.FE_REAL.Usage;
      Ocarina.Backends.Usage;

      Write_Line
        ("   -disable-annexes={annexes}" & "  Desactive one or all annexes");
      Write_Line ("       Annexes :");
      Write_Line ("        all");
      Write_Line ("        behavior_specification");
      Write_Line ("        real");
      Write_Eol;

      Free (Exec_Suffix);
   end Usage;

   package RT renames Ocarina.ME_REAL.Tokens;

begin
   --  Init

   Ocarina.Initialize;
   Language             := Get_String_Name ("aadl");
   Default_AADL_Version := Get_Default_AADL_Version;
   AADL_Version         := Default_AADL_Version;

   --  Process the command line

   Process_Command_Line (Root_System_Name, Success);
   Exit_On_Error (not Success, "Cannot process command line");

   --  Initialization Modules

   Ocarina.Configuration.Init_Modules;

   if Verbose_Mode then
      Set_Standard_Error;
      Version;
   end if;

   case Get_Current_Action is
      when Show_Version =>
         Version;
         OS_Exit (0);

      when Show_Help =>
         Usage;
         OS_Exit (0);

      when Show_Libraries =>
         Write_Line (Get_Name_String (Default_Library_Path));
         OS_Exit (0);

      when Show_Usage =>
         Usage;
         OS_Exit (1);

      when Shell =>
         Ocarina_Shell;
         OS_Exit (0);

      when Parse_Scenario_Files_First =>
         Parse_Scenario_Files;
         Reset_Current_Action;
         Set_Current_Action (After_Scenario_Action);
      when others =>
         null;
   end case;

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
         AADL_Root := Parse (Language, AADL_Root, Buffer);
         Exit_On_Error (No (AADL_Root), "Cannot parse AADL specifications");
         exit when F = Sources.Last;
         F := F + 1;
      end loop;
   end;

   Success := Analyze (Language, AADL_Root);
   Exit_On_Error (not Success, "Cannot analyze AADL specifications");

   if Verbose_Mode then
      Write_Line ("Model parsing: completed");
      Write_Eol;
   end if;

   case Get_Current_Action is
      when Analyze_Model =>
         null;

      when Instantiate_Model =>
         AADL_Root := Instantiate_Model (AADL_Root);
         Exit_On_Error (No (AADL_Root), "Cannot instantiate AADL models");
         if Verbose_Mode then
            Set_Standard_Error;
            Write_Line ("Model instantiation: completed");
            Write_Eol;
            Set_Standard_Output;
         end if;

      when Generate_Code =>
         if Get_Current_Backend_Name = Get_String_Name ("real_theorem")
           or else Get_Current_Backend_Name = Get_String_Name ("real_pp")
         then

            AADL_Root := Instantiate_Model (AADL_Root);
            Exit_On_Error (No (AADL_Root), "Cannot instantiate AADL models");

            Success := Analyze (RT.REAL_Language, AADL_Root);
            Exit_On_Error (not Success, "Cannot analyze REAL specifications");
         end if;
         Generate_Code (AADL_Root);
         if Verbose_Mode then
            Set_Standard_Error;
            Write_Line ("Code generation: completed");
            Write_Eol;
            Set_Standard_Output;
         end if;

      when others =>
         null;
   end case;
end Ocarina_Cmd;
