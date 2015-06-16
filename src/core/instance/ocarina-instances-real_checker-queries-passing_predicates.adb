------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        OCARINA.INSTANCES.REAL_CHECKER.QUERIES.PASSING_PREDICATES         --
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
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Instances.REAL_Checker.Queries.Passing_Predicates is
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   --------------------------
   -- Is_Passing_Predicate --
   --------------------------

   function Is_Passing_Through_Predicate
     (F      : Node_Id;
      C      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean
   is
      use ATN;

      pragma Unreferenced (Option);

      P : Node_Id;
      E : Node_Id;
   begin
      if ATN.Kind (F) = K_End_To_End_Flow_Spec
        and then AIN.Kind (C) = K_Component_Instance
        and then not Is_Empty (ATN.Connections (F))
      then
         P := AIN.First_Node (ATN.Connections (F));

         --  Returns true if the current (end to end flow) node is
         --  passing through the parameter component C.

         while Present (P) loop
            pragma Assert (AIN.Kind (P) = AIN.K_Identifier);

            E := AIN.Corresponding_Entity (P);

            case AIN.Kind (E) is

               when K_Port_Spec_Instance | K_Parameter_Instance =>
                  if AIN.Parent_Component (E) = C then
                     return True;
                  end if;

               when K_Subcomponent_Access_Instance =>
                  if Corresponding_Instance (E) = C then
                     return True;
                  end if;
               --  XXX FIXME
               --  is this case still possible ?

               when K_Component_Instance =>
                  --  Buses
                  if E = C then
                     return True;
                  end if;

               when others =>
                  null;
            end case;

            P := AIN.Next_Node (P);
         end loop;
      end if;

      return False;
   end Is_Passing_Through_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Passing_Predicates;
