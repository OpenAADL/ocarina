with Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
with Types;

package Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates is

   function Is_Predecessor_Of_Predicate
     (E      : Types.Node_Id;
      C      : Types.Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean;
   --  Check if the component instance E is a predecessor of the
   --  component instance C in any end-to-end flow

   package Predecessor_Query is
      new Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates
     (Is_Predecessor_Of_Predicate);

end Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates;
