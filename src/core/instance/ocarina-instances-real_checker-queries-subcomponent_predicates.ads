------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     OCARINA.INSTANCES.REAL_CHECKER.QUERIES.SUBCOMPONENT_PREDICATES       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                  Copyright (C) 2009 Telecom ParisTech,                   --
--                 2010-2019 ESA & ISAE, 2019-2020 OpenAADL                 --
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
--                    Ocarina is maintained by OpenAADL team                --
--                              (info@openaadl.org)                         --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines a method in order to select instances which are
--  subcomponents of a user-provided AADL instance

with Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
with Ocarina.Types;

package Ocarina.Instances.REAL_Checker.Queries.Subcomponent_Predicates is

   function Is_Subcomponent_Predicate
     (E      : Ocarina.Types.Node_Id;
      D      : Ocarina.Types.Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean;
   --  Check if the instance E is a subcomponent of the instance D

   package Subcomponent_Query is new Ocarina.Instances.REAL_Checker.Queries
     .Relational_Predicates
     (Is_Subcomponent_Predicate);
   --  Allows to search for all instances which are subcomponents
   --  to a given processor instance in a Result_Set or in the
   --  global node table.

end Ocarina.Instances.REAL_Checker.Queries.Subcomponent_Predicates;
