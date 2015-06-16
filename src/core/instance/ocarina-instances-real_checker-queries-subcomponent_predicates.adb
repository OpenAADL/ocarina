------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     OCARINA.INSTANCES.REAL_CHECKER.QUERIES.SUBCOMPONENT_PREDICATES       --
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

package body Ocarina.Instances.REAL_Checker.Queries.Subcomponent_Predicates is
   use Ocarina.ME_AADL.AADL_Instances.Nodes;

   -------------------------------
   -- Is_Subcomponent_Predicate --
   -------------------------------

   function Is_Subcomponent_Predicate
     (E      : Node_Id;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean
   is
      P : Node_Id;
   begin

      if Kind (E) = K_Component_Instance then
         P := Parent_Subcomponent (E);
         if Option = PSO_Direct then

            --  Returns true if the current node is a subcomponent of
            --  the parameter component...

            return (P /= No_Node and then Parent_Component (P) = D);
         else

            --  Recursively search for parents components

            while P /= No_Node loop
               if Parent_Component (P) = D then
                  return True;
               end if;
               P := Parent_Subcomponent (Parent_Component (E));
            end loop;

            return False;
         end if;
      end if;

      return False;
   end Is_Subcomponent_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Subcomponent_Predicates;
