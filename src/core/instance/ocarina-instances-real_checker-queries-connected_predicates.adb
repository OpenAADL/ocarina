------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       OCARINA.INSTANCES.REAL_CHECKER.QUERIES.CONNECTED_PREDICATES        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2012 ESA & ISAE.        --
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

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

package body Ocarina.Instances.REAL_Checker.Queries.Connected_Predicates is
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   function Is_Connected_Predicate
     (E      : Node_Id;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean
   is
      pragma Unreferenced (Option);

      N : Node_Id;
   begin
      if Kind (E) /= K_Component_Instance or else
        Kind (D) /= K_Component_Instance then
         return False;
      end if;

      if AINU.Is_Empty (Features (E)) then
         return False;
      end if;

      --  Returns true if the current node is connected to the parameter
      --  component.

      N := First_Node (Features (E));
      while Present (N) loop
         declare
            S : Node_Id := First_Node (Destinations (N));
            M : Node_Id;
            P : Node_Id;
         begin
            while Present (S) loop
               case Kind (Item (S)) is

                  when K_Port_Spec_Instance =>
                     M := S;
                     while Present (M) loop

                        P := Parent_Component (Item (M));
                        if P = D then
                           return True;
                        end if;

                        M := First_Node (Destinations (Item (M)));
                     end loop;

                  when others =>
                     null;
               end case;

               S := Next_Node (S);
            end loop;
         end;

         N := Next_Node (N);
      end loop;

      return False;

   end Is_Connected_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Connected_Predicates;
