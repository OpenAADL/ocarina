------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                     O C A R I N A . A N A L Y Z E R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

with Charset;
with Ocarina.Namet;

with GNAT.Table;

with Ocarina.Analyzer.AADL;
with Ocarina.Analyzer.REAL;
with Ocarina.Analyzer.AADL_EMA;
with Ocarina.Analyzer.AADL_BA;

package body Ocarina.Analyzer is

   use Charset;

   use Ocarina.Namet;

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

      raise Program_Error with
        "Analyzers : Cannot find language " & Get_Name_String (Language);
   end Analyze;

   --------------------
   -- Init_Analyzers --
   --------------------

   procedure Init_Analyzers is
   begin
      Ocarina.Analyzer.AADL.Init;
      Ocarina.Analyzer.REAL.Init;
      Ocarina.Analyzer.AADL_EMA.Init;
      Ocarina.Analyzer.AADL_BA.Init;
   end Init_Analyzers;

   ---------------------
   -- Reset_Analyzers --
   ---------------------

   procedure Reset_Analyzers is
   begin
      Analyzers.Init;
      Ocarina.Analyzer.AADL.Reset;
      Ocarina.Analyzer.REAL.Reset;
      Ocarina.Analyzer.AADL_EMA.Reset;
      Ocarina.Analyzer.AADL_BA.Reset;
   end Reset_Analyzers;

   -----------------------
   -- Register_Analyzer --
   -----------------------

   procedure Register_Analyzer
     (Language : String;
      Analyzer : Analyzer_Subprogram)
   is
      N : constant Name_Id := Get_String_Name (To_Lower (Language));

   begin
      for B in Analyzers.First .. Analyzers.Last loop
         if Analyzers.Table (B).Language = N then
            raise Program_Error with
              "Cannot register twice analyzer " & Language;
         end if;
      end loop;

      Analyzers.Increment_Last;
      Set_Str_To_Name_Buffer (Language);
      Analyzers.Table (Analyzers.Last).Language := Name_Find;
      Analyzers.Table (Analyzers.Last).Analyzer := Analyzer;
   end Register_Analyzer;

end Ocarina.Analyzer;
