------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      OCARINA.INSTANCES.REAL_CHECKER.QUERIES.PREDECESSOR_PREDICATES       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2012-2015 ESA & ISAE.                    --
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

with Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
with Ocarina.Types;

package Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates is

   function Is_Predecessor_Of_Predicate
     (E      : Ocarina.Types.Node_Id;
      C      : Ocarina.Types.Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean;
   --  Check if the component instance E is a predecessor of the
   --  component instance C in any end-to-end flow

   package Predecessor_Query is new Ocarina.Instances.REAL_Checker.Queries
     .Relational_Predicates
     (Is_Predecessor_Of_Predicate);

end Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates;
