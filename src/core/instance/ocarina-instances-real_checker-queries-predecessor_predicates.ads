------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      OCARINA.INSTANCES.REAL_CHECKER.QUERIES.PREDECESSOR_PREDICATES       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2012 ESA & ISAE.                       --
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

with Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
pragma Elaborate_All
  (Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates);

with Types;

package Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates is

   function Is_Predecessor_Of_Predicate
     (E      : Types.Node_Id;
      C      : Types.Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean;
   --  Check if the component instance E is a predecessor of the
   --  component instance C in any end-to-end flow

   package Predecessor_Query is
      new Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates
     (Is_Predecessor_Of_Predicate);

end Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates;
