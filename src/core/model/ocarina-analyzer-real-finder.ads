------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . A N A L Y Z E R . R E A L . F I N D E R          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2014 ESA & ISAE.        --
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
