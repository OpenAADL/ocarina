------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            OCARINA.INSTANCES.REAL_CHECKER.QUERIES.PREDICATES             --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.Instances.Finder;

package body Ocarina.Instances.REAL_Checker.Queries.Predicates is
   use Ocarina.Instances.Finder;
   use Set;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   ---------------------------------------
   -- Get_Instances_Verifying_Predicate --
   ---------------------------------------

   function Get_Instances_Verifying_Predicate return Result_Set is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      Result : Result_Set;
      EL     : Node_List;
   begin
      Init (Result);

      Find_All_Instances
        (Root_Instance,
         (K_Component_Instance, K_Call_Instance, K_Call_Sequence_Instance),
         EL.First,
         EL.Last);
      while Present (EL.First) loop
         if Predicate (EL.First) then
            Append (Result, EL.First);
         end if;
         EL.First := AIN.Next_Entity (EL.First);
      end loop;

      Find_All_Flows (Root_Instance, EL.First, EL.Last);
      while Present (EL.First) loop
         if Predicate (EL.First) then
            Append (Result, EL.First);
         end if;
         EL.First := ATN.Next_Entity (EL.First);
      end loop;

      return Result;
   end Get_Instances_Verifying_Predicate;

   ---------------------------------------
   -- Get_Instances_Verifying_Predicate --
   ---------------------------------------

   function Get_Instances_Verifying_Predicate
     (Set : Result_Set) return Result_Set
   is
      Result : Result_Set;
   begin
      Init (Result);

      for N in First .. Last (Set) loop
         if Predicate (Set.Table (N)) then
            Append (Result, Set.Table (N));
         end if;
      end loop;

      return Result;
   end Get_Instances_Verifying_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Predicates;
