------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . A N A L Y Z E R . R E A L                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

--  Analyze REAL theorems

package Ocarina.Analyzer.REAL is

   procedure Init;

   procedure Reset;

   procedure Build_Theorem_List (AADL_Root : Node_Id);
   --  Build the list of theorems to be run

   procedure Analyze_Theorem
     (Theorem   :     Node_Id;
      AADL_Root :     Node_Id;
      Success   : out Boolean);
   --  Root procedure of analysis

   function Analyze_Model (Root : Node_Id) return Boolean;
   --  Proceed to both REAL expansion and analysis on the whole
   --  AADL model

   procedure Register_Library_Theorems (REAL_Library : Node_Id);
   --  Append the content of a parsed REAL specification to
   --  the library theorem list (ie. REAL theorems that must *not*
   --  be called directly).

   Main_Theorem : Name_Id := No_Name;
   --  Name of the main theorem to be evaluated, by default evaluate
   --  all theorems.

   Continue_Evaluation : Boolean := False;
   --  In case of a theorem evaluates to false, continue the
   --  evaluation.

end Ocarina.Analyzer.REAL;
