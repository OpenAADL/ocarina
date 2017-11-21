------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       OCARINA.INSTANCES.REAL_CHECKER.QUERIES.CONNECTED_PREDICATES        --
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

--  This package defines a method in order to select instances connected
--  to a user-provided AADL instance

with Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
with Ocarina.Types;

package Ocarina.Instances.REAL_Checker.Queries.Connected_Predicates is

   function Is_Connected_Predicate
     (E      : Ocarina.Types.Node_Id;
      D      : Ocarina.Types.Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean;
   --  Check if the instance E is bound to the instance D
   --  with the relation actual_processor_binding (so D must be
   --  a processor instance).

   package Connected_Query is new Ocarina.Instances.REAL_Checker.Queries
     .Relational_Predicates
     (Is_Connected_Predicate);
   --  Allows to search for all instances connected to a given
   --  component instance in a Result_Set or in the global node
   --  table.

end Ocarina.Instances.REAL_Checker.Queries.Connected_Predicates;
