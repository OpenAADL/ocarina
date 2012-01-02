------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . A N A L Y Z E R . R E A L                 --
--                                                                          --
--                                 S p e c                                  --
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

--  Analyze REAL theorems

package Ocarina.Analyzer.REAL is

   procedure Init;

   procedure Reset;

   procedure Build_Theorem_List (AADL_Root : Node_Id);
   --  Build the list of theorems to be run

   procedure Analyze_Theorem
     (Theorem   : Node_Id;
      AADL_Root : Node_Id;
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
