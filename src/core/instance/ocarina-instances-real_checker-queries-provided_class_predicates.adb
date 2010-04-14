with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.Instances.Queries;
with Namet;

package body Ocarina.Instances.REAL_Checker.Queries.Provided_Class_Predicates
is
   use Ocarina.Instances.Queries;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   ---------------------------------
   -- Is_Provided_Class_Predicate --
   ---------------------------------

   function Is_Provided_Class_Predicate
     (E      : Node_Id;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean
   is
      pragma Unreferenced (Option);
      use Namet;

      Str_1  : constant Name_Id := Get_String_Name
        ("provided_virtual_bus_class");
   begin

      --  Returns true if the current node provides a component of
      --  same class than the parameter component.

      --  Test for virtual buses

      if Is_Defined_Classifier_Property (E, Str_1) then
         if AIN.Corresponding_Declaration
           (Get_Classifier_Property (E, Str_1)) =
           AIN.Corresponding_Declaration (D) then
            return True;
         end if;
      end if;

      return False;
   end Is_Provided_Class_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Provided_Class_Predicates;
