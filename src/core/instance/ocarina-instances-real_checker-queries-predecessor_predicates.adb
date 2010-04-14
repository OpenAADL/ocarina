
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
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean
   is
      use ATN;

      pragma Unreferenced (Option);

      P, P2  : Node_Id;
      Flows  : constant Result_Set := Get_Instances_Of_End_To_End_Flows;
      F      : Node_Id;
      F1, F2 : Boolean;
   begin

      if AIN.Kind (E) /= K_Component_Instance or else
        AIN.Kind (C) /= K_Component_Instance then
         return False;
      end if;

      for I in 1 .. Cardinal (Flows) loop
         F := Get (Flows, I);
         P := AIN.First_Node (ATN.Connections (F));
         F1 := False;
         F2 := False;

         while Present (P) loop
            if AIN.Kind (P) = AIN.K_Identifier and then
              (AIN.Kind (AIN.Corresponding_Entity (P)) =
               K_Port_Spec_Instance or else
               AIN.Kind (AIN.Corresponding_Entity (P)) =
               K_Parameter_Instance) then
               if AIN.Parent_Component (AIN.Corresponding_Entity (P)) = E then
                  F1 := True;
                  exit;
               end if;

            elsif AIN.Kind (AIN.Corresponding_Entity (P)) =
              K_Subcomponent_Access_Instance then

               if Corresponding_Instance (AIN.Corresponding_Entity (P)) =
                 E then
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
               if AIN.Kind (P2) = AIN.K_Identifier and then
                 (AIN.Kind (AIN.Corresponding_Entity (P2)) =
                  K_Port_Spec_Instance or else
                  AIN.Kind (AIN.Corresponding_Entity (P2)) =
                  K_Parameter_Instance) then
                  if AIN.Parent_Component (AIN.Corresponding_Entity (P2)) =
                    C then
                     F2 := True;
                     exit;
                  end if;

               elsif AIN.Kind (AIN.Corresponding_Entity (P2)) =
                 K_Subcomponent_Access_Instance then

                  if Corresponding_Instance (AIN.Corresponding_Entity (P2)) =
                    C then
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
