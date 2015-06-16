------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      OCARINA.INSTANCES.REAL_CHECKER.QUERIES.PREDECESSOR_PREDICATES       --
--                                                                          --
--                                 B o d y                                  --
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

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates is
   use Ocarina.ME_AADL.AADL_Instances.Nodes;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   ---------------------------------
   -- Is_Predecessor_Of_Predicate --
   ---------------------------------

   function Is_Predecessor_Of_Predicate
     (E      : Node_Id;
      C      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean
   is
      use ATN;

      pragma Unreferenced (Option);

      P, P2  : Node_Id;
      Flows  : constant Result_Set := Get_Instances_Of_End_To_End_Flows;
      F      : Node_Id;
      F1, F2 : Boolean;
   begin

      if AIN.Kind (E) /= K_Component_Instance
        or else AIN.Kind (C) /= K_Component_Instance
      then
         return False;
      end if;

      for I in 1 .. Cardinal (Flows) loop
         F  := Get (Flows, I);
         P  := AIN.First_Node (ATN.Connections (F));
         F1 := False;
         F2 := False;

         while Present (P) loop
            if AIN.Kind (P) = AIN.K_Identifier
              and then
              (AIN.Kind (AIN.Corresponding_Entity (P)) = K_Port_Spec_Instance
               or else
                 AIN.Kind (AIN.Corresponding_Entity (P)) =
                 K_Parameter_Instance)
            then
               if AIN.Parent_Component (AIN.Corresponding_Entity (P)) = E then
                  F1 := True;
                  exit;
               end if;

            elsif AIN.Kind (AIN.Corresponding_Entity (P)) =
              K_Subcomponent_Access_Instance
            then

               if Corresponding_Instance (AIN.Corresponding_Entity (P)) =
                 E
               then
                  F1 := True;
                  exit;
               end if;
               --  XXX FIXME
               --  is this case still possible ?

            end if;

            P := AIN.Next_Node (P);
         end loop;

         if F1 then
            P2 := AIN.Next_Node (P);

            while Present (P2) loop
               if AIN.Kind (P2) = AIN.K_Identifier
                 and then
                 (AIN.Kind (AIN.Corresponding_Entity (P2)) =
                  K_Port_Spec_Instance
                  or else
                    AIN.Kind (AIN.Corresponding_Entity (P2)) =
                    K_Parameter_Instance)
               then
                  if AIN.Parent_Component (AIN.Corresponding_Entity (P2)) =
                    C
                  then
                     F2 := True;
                     exit;
                  end if;

               elsif AIN.Kind (AIN.Corresponding_Entity (P2)) =
                 K_Subcomponent_Access_Instance
               then

                  if Corresponding_Instance (AIN.Corresponding_Entity (P2)) =
                    C
                  then
                     F2 := True;
                     exit;
                  end if;
                  --  XXX FIXME
                  --  is this case still possible ?

               end if;

               P2 := AIN.Next_Node (P2);
            end loop;
         end if;

         if F1 and then F2 then
            return True;
         end if;
      end loop;

      return False;
   end Is_Predecessor_Of_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates;
