------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.INSTANCES.REAL_CHECKER.QUERIES                   --
--                                                                          --
--                                 S p e c                                  --
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

with Ocarina.Types;
with GNAT.Dynamic_Tables;

package Ocarina.Instances.REAL_Checker.Queries is

   type Result_Set is private;
   --  A Result set, on which all elementary set operation
   --  will be defined.

   type Instance_Type is
     (C_Data,
      C_Subprogram,
      C_Subprogram_Call,
      C_Sequence_Call,
      C_Thread,
      C_Thread_Group,
      C_Process,
      C_Memory,
      C_Processor,
      C_Bus,
      C_Virtual_Processor,
      C_Virtual_Bus,
      C_Connection,
      C_Device,
      C_System,
      C_Abstract,
      C_Unknown);

   type Predicates_Search_Options is (PSO_Direct,      --  Non-recursive search
   PSO_Recursive);  --  Recursive search

   procedure Init (Root : Node_Id);
   --  To call before any usage of the package

   --  Set builder procedures

   function Get_Instances_Of_End_To_End_Flows return Result_Set;
   --  Return all end to end flows

   function Get_Instances_Of_Component_Type
     (Component_T : Instance_Type) return Result_Set;
   --  Search in the Node table for instances a given type

   function Get_Instances_Of_Component_Type
     (E : Ocarina.Types.Node_Id) return Result_Set;
   --  Search in the Node table for instances a given type

   function Get_Instances_With_Property
     (Set           : Result_Set;
      Property_Name : String) return Result_Set;
   --  Search in the given set for instances with the
   --  property_name property

   --  Set manipulation procedures

   function Union
     (Set_1    : Result_Set;
      Set_2    : Result_Set;
      Distinct : Boolean := False) return Result_Set;
   --  Returns all distincts elements of set_1 and set_2
   --  If an element is present in the intersection, it
   --  will be present only one time in the result_set if
   --  'distinct' is set to true, two times otherwise.

   function Intersection
     (Set_1 : Result_Set;
      Set_2 : Result_Set) return Result_Set;
   --  Returns all elements that are in both of set_1 and set_2
   --  each element is present only one time in the result_set.

   function Exclusion
     (Set_1 : Result_Set;
      Set_2 : Result_Set) return Result_Set;
   --  Returns all elements that are in set_1 and NOT in set_2

   function Includes (Set_1 : Result_Set; Set_2 : Result_Set) return Boolean;
   --  Does Set_1 includes set_2 ?

   function Mutual_Inclusion
     (Set_1 : Result_Set;
      Set_2 : Result_Set) return Boolean;
   --  Does Set_1 includes set_2 AND set_2 includes set_1 ?

   function Empty_Set return Result_Set;
   --  returns the empty set

   function Is_Empty (Set : Result_Set) return Boolean;
   --  Is the set empty ?

   function Cardinal (Set : Result_Set) return Natural;
   --  Return set's cardinality

   function Get (Set : Result_Set; Index : Natural)
                return Ocarina.Types.Node_Id;

   procedure Add
     (Set      : in out Result_Set;
      E        :        Ocarina.Types.Node_Id;
      Distinct :        Boolean := False);
   --  Add the element E in the result set.
   --  If distint is set to true and if E is already
   --  present in the set, E won't be added

   function Is_In (E : Ocarina.Types.Node_Id; Set : Result_Set) return Boolean;
   --  Found whether E is present in the Set

   --  Testing procedures

   function Test_Dummy (C : Instance_Type) return Result_Set;

   function Test_Dummy_Sets return Result_Set;

   procedure Display_Set (Set : Result_Set);

   --  Misc functions

   function Get_Property_Value
     (E    : Ocarina.Types.Node_Id;
      Name : String) return Ocarina.Types.Node_Id;
   --  for a given set element, returns then Name property value
   --  if the property is absent, returns No_Node.

   Root_Instance : Node_Id;

private
   type Element is new Node_Id;

   package Set is new GNAT.Dynamic_Tables
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,  -- # of elements
      Table_Increment      => 50); -- % increase

   type Result_Set is new Set.Instance;

end Ocarina.Instances.REAL_Checker.Queries;
