------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . A N A L Y Z E R . A A D L                 --
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

with Ocarina;

package Ocarina.Analyzer.AADL is

   procedure Init;

   procedure Reset;

   function Analyze_Model (Root : Node_Id) return Boolean;
   --  Create the links between the abstract syntax tree nodes and
   --  check the correctness of the tree. This function must be
   --  invoked after all the AADL elements have been integrated into
   --  the tree. Root must be the Node_Id of the tree root. Return
   --  True if everything in the tree is correct, else False.  The
   --  'Analyzer_Options' parameter specify the analyzer options
   --  Allwo_Automatic_Importation is set to True, then the analyzer
   --  will try to parse additional files if declarations are missing.

private

   function Have_Common_Statements
     (Node_1 : Node_Id;
      Node_2 : Node_Id)
     return Boolean;
   --  Return True iff the two statements Node_1 and Node_2 have
   --  common elements. Considered statements are 'in modes' and 'in
   --  bindings'.

end Ocarina.Analyzer.AADL;
