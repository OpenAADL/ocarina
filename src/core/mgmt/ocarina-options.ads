------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                      O C A R I N A . O P T I O N S                       --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.Types; use Ocarina.Types;

with GNAT.Table;

package Ocarina.Options is

   type Action_Kind is
     (None,
      Analyze_Model,
      Instantiate_Model,
      Generate_Code,
      Show_Version,
      Show_Help,
      Show_Libraries,
      Show_Usage,
      Parse_Scenario_Files_First,
      Shell,
      Python_Shell);

   Root_System_Name       : Name_Id := No_Name;
   Installation_Directory : Name_Id := No_Name;
   Output_Filename        : Name_Id := No_Name;
   Boundt_Process         : Name_Id := No_Name;
   Quiet_Mode             : Boolean := False;
   Verbose_Mode           : Boolean := False;
   Debug_Mode             : Boolean := False;
   Use_Scenario_File      : Boolean := False;
   Auto_Load_AADL_Files   : Boolean := False;

   procedure Set_Current_Action (Action : Action_Kind);
   function Get_Current_Action return Action_Kind;
   procedure Reset_Current_Action;

   Invalid_Options : exception;

   function Default_Library_Path return Name_Id;
   --  Return the default AADL property set directory depending on the
   --  current AADL version.

   package Library_Paths is new GNAT.Table (Name_Id, Natural, 1, 5, 20);

   procedure Add_Library_Path (Path : String);
   --  Add a library path in Library_Paths

   type Annex_Action_Kind is (Disable_BA, Disable_REAL, Disable_ALL);

   type Annex_Action is array (Annex_Action_Kind) of Byte;

   procedure Set_Annex_Action (Action : Annex_Action_Kind);
   function Get_Annex_Action (Action : Annex_Action_Kind) return Byte;
   procedure Reset_Annex_Action;
   procedure Process_Annex_Action (Parameters : String);
   function Perform_Annex_Action (Language : Name_Id) return Boolean;

private

   Current_Action : Action_Kind := None;

   Current_Annex_Action : Annex_Action;

end Ocarina.Options;
