------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . A N A L Y Z E R . A A D L                 --
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
      Node_2 : Node_Id) return Boolean;
   --  Return True iff the two statements Node_1 and Node_2 have
   --  common elements. Considered statements are 'in modes' and 'in
   --  bindings'.

end Ocarina.Analyzer.AADL;
