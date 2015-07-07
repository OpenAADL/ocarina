------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                        O C A R I N A . U T I L S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2013-2015 ESA & ISAE.                    --
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

with Ada.Command_Line;           use Ada.Command_Line;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Errors;                     use Errors;
with Locations;                  use Locations;
with Ocarina.Namet;                      use Ocarina.Namet;
with Ocarina.Output;                     use Ocarina.Output;
with Utils;                      use Utils;

with Ocarina.Analyzer;           use Ocarina.Analyzer;
with Ocarina.Backends;           use Ocarina.Backends;
with Ocarina.Configuration;      use Ocarina.Configuration;
with Ocarina.FE_AADL;            use Ocarina.FE_AADL;
with Ocarina.FE_AADL.Parser;
with Ocarina.FE_REAL;            use Ocarina.FE_REAL;
with Ocarina.Instances;          use Ocarina.Instances;
with Ocarina.Parser;             use Ocarina.Parser;
with Ocarina.Options;            use Ocarina.Options;
with Ocarina.Files;              use Ocarina.Files;
with Ocarina.Backends.Properties.Utils;

package body Ocarina.Utils is

   AADL_Root             : Node_Id := No_Node;
   File_Name             : Name_Id := No_Name;
   Buffer                : Location;
   Language              : Name_Id := No_Name;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      --  Reset Ocarina

      Ocarina.Configuration.Reset_Modules;
      Ocarina.Reset;

      --  Initialize Ocarina

      Ocarina.AADL_Version := Get_Default_AADL_Version;
      AADL_Version         := Ocarina.AADL_V2;
      Ocarina.Initialize;
      Ocarina.Configuration.Init_Modules;

      --  Reset library

      AADL_Root := No_Node;
      File_Name := No_Name;
      Language := No_Name;
   end Reset;

   -------------
   -- Version --
   -------------

   procedure Version is
   begin
      Write_Line
        ("Ocarina " & Ocarina_Version
           & " (" & Ocarina_Revision & ")");

      if Ocarina_Last_Configure_Date /= "" then
         Write_Line ("Build date: " & Ocarina_Last_Configure_Date);
      end if;

      Write_Line
        ("Copyright (c) 2003-2009 Telecom ParisTech, 2010-"
           & Ocarina_Last_Configure_Year & " ESA & ISAE");
   end Version;

   ------------------
   -- Print_Status --
   ------------------

   procedure Print_Status is
   begin
      Write_Line ("AADL version: " & Ocarina.AADL_Version'Img);
      Write_Line ("Library Path: "
                    & Get_Name_String (Default_Library_Path));
      Write_Line ("Load predefined property sets: "
                    & Ocarina.FE_AADL.Parser.Add_Pre_Prop_Sets'Img);
   end Print_Status;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      Exec_Suffix : String_Access := Get_Executable_Suffix;
   begin
      Set_Standard_Error;
      Write_Line ("Usage: ");
      Write_Line ("      "
                    & Base_Name (Command_Name, Exec_Suffix.all)
                    & " [options] files");
      Write_Line ("      OR");
      Write_Line ("      "
                    & Base_Name (Command_Name, Exec_Suffix.all)
                    & " -help");
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
      Write_Line ("   -c  Only perform schedulability analysis");

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

      Write_Line ("   -disable-annexes={annexes}"
                    & "  Desactive one or all annexes");
      Write_Line ("       Annexes :");
      Write_Line ("        all");
      Write_Line ("        behavior_specification");
      Write_Line ("        real");
      Write_Eol;

      Free (Exec_Suffix);
   end Usage;

   --------------------
   -- Load_AADL_File --
   --------------------

   procedure Load_AADL_File (Filename : String) is
   begin
      Language := Get_String_Name ("aadl");
      Set_Str_To_Name_Buffer (Filename);

      File_Name := Search_File (Name_Find);
      if File_Name = No_Name then
         Write_Line ("Cannot find file " & Filename);
         return;
      end if;

      Buffer := Load_File (File_Name);
      if File_Name = No_Name then
         Write_Line ("Cannot read file " & Filename);
         return;
      end if;

      AADL_Root := Parse (Language, AADL_Root, Buffer);
      Exit_On_Error
        (No (AADL_Root),
         "Cannot parse AADL specifications");
      Write_Line
        ("File " & Filename
           & " loaded and parsed sucessfully");
   end Load_AADL_File;

   -------------
   -- Analyze --
   -------------

   function Analyze return Boolean is
      Success : Boolean;
   begin
      Success := Analyze (Language, AADL_Root);
      if not Success then
         Write_Line ("Cannot analyze AADL specifications");
      else
         Write_Line ("Model analyzed sucessfully");
      end if;
      return Success;
   end Analyze;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate (Root_System : String) return Boolean is
      Success : Boolean;
   begin
      if Root_System /= "" then
         Root_System_Name := To_Lower
           (Get_String_Name (Root_System));
      end if;
      AADL_Root := Instantiate_Model (AADL_Root);
      if Present (AADL_Root) then
         Write_Line ("Model instantiated sucessfully");
         Success := True;
      else
         Success := False;
      end if;

      return Success;
   end Instantiate;

   --------------
   -- Generate --
   --------------

   procedure Generate (Backend_Name : String) is
   begin
      Set_Current_Backend_Name (Backend_Name);
      Write_Line ("Generating code using backend " & Backend_Name);
      Generate_Code (AADL_Root);
   end Generate;

   -------------------
   -- Get_AADL_Root --
   -------------------

   function Get_AADL_Root return Node_Id is
   begin
      return AADL_Root;
   end Get_AADL_Root;

   ------------------------
   -- Get_Property_Value --
   ------------------------

   procedure Get_Property_Value (Data : in out Callback_Data'Class;
                                 PropId : String; PropName : String)
   is
      Result : constant String_List :=
        Ocarina.Backends.Properties.Utils.Check_And_Get_Property
        (Get_Node_Id_From_String (PropId),
         Get_Node_Id_From_String (PropName));
   begin
      Set_Return_Value_As_List (Data);

      for Elt of Result loop
         Set_Return_Value (Data, Elt.all);
      end loop;
   end Get_Property_Value;

   -----------------
   -- Get_Node_Id --
   -----------------

   procedure Get_Node_Id (Data : in out Callback_Data'Class;
      N : String) is
   begin
      Set_Return_Value (Data, Integer'Image (Integer
         (Namet.Get_String_Name (N))));
   end Get_Node_Id;

   -----------------------------
   -- Get_Node_Id_From_String --
   -----------------------------

   function Get_Node_Id_From_String (Name : String) return Node_Id is
   begin
      return Node_Id (Integer'Value (Name));
   end Get_Node_Id_From_String;

   -----------------------------
   -- Get_List_Id_From_String --
   -----------------------------

   function Get_List_Id_From_String (Name : String) return List_Id is
   begin
      return List_Id (Integer'Value (Name));
   end Get_List_Id_From_String;

   -----------------------------
   -- Get_Boolean_From_String --
   -----------------------------

   function Get_Boolean_From_String (Name : String) return Boolean is
   begin
      return Boolean'Value (Name);
   end Get_Boolean_From_String;

   --------------------------
   -- Get_Byte_From_String --
   --------------------------

   function Get_Byte_From_String (Name : String) return Byte is
   begin
      return Byte (Integer'Value (Name));
   end Get_Byte_From_String;

   -------------------------
   -- Get_Int_From_String --
   -------------------------

   function Get_Int_From_String (Name : String) return Int is
   begin
      return Int (Integer'Value (Name));
   end Get_Int_From_String;

   ------------------------------
   -- Get_Value_Id_From_String --
   ------------------------------

   function Get_Value_Id_From_String (Name : String) return Value_Id is
   begin
      return Value_Id (Integer'Value (Name));
   end Get_Value_Id_From_String;

   -----------------------------
   -- Get_Name_Id_From_String --
   -----------------------------

   function Get_Name_Id_From_String (Name : String) return Name_Id is
      val : Integer;
   begin
      val := Integer'Value (Name);
      if val > 300_000_000 and then val < 399_999_999 then
         return Name_Id (val);
      else
         return No_Name;
      end if;
   end Get_Name_Id_From_String;

end Ocarina.Utils;
