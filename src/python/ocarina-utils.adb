------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                        O C A R I N A . U T I L S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2013-2016 ESA & ISAE.                    --
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

with Errors;                     use Errors;
with Locations;                  use Locations;
with Ocarina.Namet;              use Ocarina.Namet;
with Ocarina.Output;             use Ocarina.Output;
with Utils;                      use Utils;

with Ocarina.Analyzer;           use Ocarina.Analyzer;
with Ocarina.Analyzer.REAL;      use Ocarina.Analyzer.REAL;
with Ocarina.Backends;           use Ocarina.Backends;
with Ocarina.Configuration;      use Ocarina.Configuration;
with Ocarina.FE_AADL;            use Ocarina.FE_AADL;
with Ocarina.FE_AADL.Parser;
with Ocarina.FE_REAL.Parser;     use Ocarina.FE_REAL.Parser;
with Ocarina.Instances;          use Ocarina.Instances;
with Ocarina.Parser;             use Ocarina.Parser;
with Ocarina.Options;            use Ocarina.Options;
with Ocarina.Files;              use Ocarina.Files;

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

   ----------------------
   -- Set_REAL_Theorem --
   ----------------------

   function Set_REAL_Theorem (Theorem_Name : String) return Boolean is
   begin
      if Theorem_Name /= "" then
         Main_Theorem := To_Lower (Get_String_Name (Theorem_Name));
      end if;

      return True;
   end Set_REAL_Theorem;

   ----------------------
   -- Add_REAL_Library --
   ----------------------

   function Add_REAL_Library (Library_Name : String) return Boolean is
   begin
      if Library_Name /= "" then
         Write_Line ("Adding: " & Library_Name);
         Load_REAL_Library (Get_String_Name (Library_Name));
      end if;
      return True;
   end Add_REAL_Library;

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
