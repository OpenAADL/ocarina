--  This package defines a method in order to select instances which
--  have a provided component class of same type than user-provided AADL
--  instance

with Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
with Types;

package Ocarina.Instances.REAL_Checker.Queries.Provided_Class_Predicates is

   function Is_Provided_Class_Predicate
     (E      : Types.Node_Id;
      D      : Types.Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean;
   --  Check if a component of same class than the instance E is provided
   --  to the instance D
   --  with the relation Provided_Virtual_Bus_Class property

   package Provided_Class_Query is
      new Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates
     (Is_Provided_Class_Predicate);
   --  Allows to search for all instances bound to a given
   --  instance in a Result_Set or in the global node table.

end Ocarina.Instances.REAL_Checker.Queries.Provided_Class_Predicates;
