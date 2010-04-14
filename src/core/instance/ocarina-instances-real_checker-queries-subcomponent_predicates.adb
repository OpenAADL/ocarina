------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     OCARINA.INSTANCES.REAL_CHECKER.QUERIES.SUBCOMPONENT_PREDICATES       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2009, GET-Telecom Paris.                   --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
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
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean
   is
      P : Node_Id;
   begin

      if  Kind (E) = K_Component_Instance then
         P := Parent_Subcomponent (E);
         if Option = PSO_Direct then

            --  Returns true if the current node is a subcomponent of
            --  the parameter component...

            return (P /= No_Node and then
                    Parent_Component (P) = D);
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
