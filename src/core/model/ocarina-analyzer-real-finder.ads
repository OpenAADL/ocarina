------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . A N A L Y Z E R . R E A L . F I N D E R          --
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

--  Allows to explore a REAL theorem

with Ocarina.ME_REAL.REAL_Tree.Nutils;

package Ocarina.Analyzer.REAL.Finder is
   use Ocarina.ME_REAL.REAL_Tree.Nutils;

   function Get_REAL_Annexes_List
     (AADL_Root : Node_Id)
      return Ocarina.ME_REAL.REAL_Tree.Nutils.Node_List.Instance;
   --  Returns the list of all REAL annexes

   function Compute_Expression_Type (E : Node_Id) return Value_Id;
   --  Compute the actual type returned by an expression
   --  Look in documentation for association rules

   function Get_Set_Type (S : Node_Id) return Value_Id;
   --  Returns a set identifier type,
   --  Takes a Set_Reference as parameter

   Is_Dependant : Boolean;
   --  Is the set currrently parsed dependant of the range variable ?

end Ocarina.Analyzer.REAL.Finder;
