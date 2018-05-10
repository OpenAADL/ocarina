------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      OCARINA.INSTANCES.REAL_CHECKER.QUERIES.RELATIONAL_PREDICATES        --
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

--  This generic package defines a method in order to select instances
--  withing a given set, accordingly to a user-provided selection
--  criterium, where criterium involves another AADL node.

with Ocarina.Types;

generic
   with function Predicate
     (E      : Ocarina.Types.Node_Id;
      D      : Ocarina.Types.Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean;

package Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates is
   use Ocarina.Types;

   function Get_Instances_Verifying_Predicate
     (D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Result_Set;
   --  search in Node_Id table components verifying the Predicate
   --  property, with respect to the D node

   function Get_Instances_Verifying_Predicate
     (Set    : Result_Set;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Result_Set;
   --  search in a given Result_Set components verifying the
   --  Predicate property

   function Apply
     (Set_1    : Result_Set;
      Set_2    : Result_Set;
      Reversed : Boolean                   := False;
      Distinct : Boolean                   := False;
      Option   : Predicates_Search_Options := PSO_Direct) return Result_Set;
   --  search in Set_1 components verifying the
   --  Predicate property with *any* element of Set_2
   --  * Reversed : all elements of set_2 which comply to the relation
   --  * Distinct : identical results are stored only one time
   --  * PSO_Direct : relation-dependant

end Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
