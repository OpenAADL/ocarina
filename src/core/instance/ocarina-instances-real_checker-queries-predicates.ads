------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            OCARINA.INSTANCES.REAL_CHECKER.QUERIES.PREDICATES             --
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

with Ocarina.Types;

generic
   with function Predicate (E : Ocarina.Types.Node_Id) return Boolean;

package Ocarina.Instances.REAL_Checker.Queries.Predicates is

   function Get_Instances_Verifying_Predicate return Result_Set;
   --  search in Node_Id table components verifying the Predicate
   --  property

   function Get_Instances_Verifying_Predicate
     (Set : Result_Set) return Result_Set;
   --  search in a given Result_Set components verifying the
   --  Predicate property

end Ocarina.Instances.REAL_Checker.Queries.Predicates;
