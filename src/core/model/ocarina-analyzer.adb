------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                     O C A R I N A . A N A L Y Z E R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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

with Charset;
with Namet;
with Output;

with GNAT.Table;

with Ocarina.Analyzer.AADL;
with Ocarina.Analyzer.REAL;

package body Ocarina.Analyzer is

   use Charset;
   use Namet;
   use Output;

   type Analyzer_Record is record
      Language : Name_Id;
      Analyzer : Analyzer_Subprogram;
   end record;

   package Analyzers is new GNAT.Table (Analyzer_Record, Nat, 1, 5, 20);

   -------------
   -- Analyze --
   -------------

   function Analyze (Language : Name_Id; Root : Node_Id) return Boolean is
   begin
      for P in Analyzers.First .. Analyzers.Last loop
         if Language = Analyzers.Table (P).Language then
            return Analyzers.Table (P).Analyzer (Root);
         end if;
      end loop;

      Set_Standard_Error;
      Write_Line ("Analyzers : Cannot find language "
                    & Get_Name_String (Language));
      Set_Standard_Output;
      return False;
   end Analyze;

   --------------------
   -- Init_Analyzers --
   --------------------

   procedure Init_Analyzers is
   begin
      Ocarina.Analyzer.AADL.Init;
      Ocarina.Analyzer.REAL.Init;
   end Init_Analyzers;

   ---------------------
   -- Reset_Analyzers --
   ---------------------

   procedure Reset_Analyzers is
   begin
      Analyzers.Init;
      Ocarina.Analyzer.AADL.Reset;
      Ocarina.Analyzer.REAL.Reset;
   end Reset_Analyzers;

   -----------------------
   -- Register_Analyzer --
   -----------------------

   procedure Register_Analyzer
     (Language : String;
      Analyzer : Analyzer_Subprogram)
   is
      N : Name_Id;
   begin
      N := Get_String_Name (To_Lower (Language));
      for B in Analyzers.First .. Analyzers.Last loop
         if Analyzers.Table (B).Language = N then
            Set_Standard_Error;
            Write_Line ("Cannot register twice analyzer " & Language);
            Set_Standard_Output;
         end if;
      end loop;

      if Language'Length > 0 then
         Analyzers.Increment_Last;
         Set_Str_To_Name_Buffer (Language);
         Analyzers.Table (Analyzers.Last).Language := Name_Find;
         Analyzers.Table (Analyzers.Last).Analyzer := Analyzer;
      end if;
   end Register_Analyzer;

end Ocarina.Analyzer;
