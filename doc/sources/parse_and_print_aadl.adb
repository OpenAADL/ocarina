------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 P A R S E _ A N D _ P R I N T _ A A D L                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2008, GET-Telecom Paris.                --
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

with Ada.Command_Line;
with GNAT.OS_Lib;

with Types;
with Namet;

with Ocarina.Configuration;
with Ocarina.Parser;
with Ocarina.Printers;
with Ocarina.Analyzer;

procedure Parse_And_Print_AADL is

   use Types;
   use Namet;

   Root              : Node_Id;
   Printer_Options   : Ocarina.Printers.Output_Options :=
     Ocarina.Printers.Default_Output_Options;
   Analysis_Options  : constant Ocarina.Analyzer.Analyzer_Options :=
     Ocarina.Analyzer.Default_Analyzer_Options;
   Success           : Boolean;

begin
   loop
      --  Initialization step

      Ocarina.Initialize;
      Ocarina.Configuration.Init_Modules;

      --  Parse the aadl source file, the right parser is selected
      --  automatically depending on the file suffix. It is important
      --  that the root node is set to No_Node at the very beginning
      --  or parsing.

      Root := No_Node;

      for J in 1 .. Ada.Command_Line.Argument_Count loop
         --  Parse the file corresponding to the Jth argument opf the
         --  commant line and enrich the global AADL tree.

         Root := Ocarina.Parser.Parse (Ada.Command_Line.Argument (J), Root);
      end loop;

      --  If something went wrong, Root = No_Node

      if Root /= No_Node then
         --  Analyze the tree

         Success := Ocarina.Analyzer.Analyze_Model (Root, Analysis_Options);

         if Success then
            --  Select the printer

            Set_Str_To_Name_Buffer ("aadl");
            Printer_Options.Printer_Name := Name_Find;

            --  Print to an AADL File

            Success := Ocarina.Printers.Print
              (Root    => Root,
               Options => Printer_Options);
         else
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      else
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      --  Pause for 1 second

      delay 1.0;

      --  Reset step

      Ocarina.Configuration.Reset_Modules;
      Ocarina.Reset;
   end loop;

end Parse_And_Print_AADL;
