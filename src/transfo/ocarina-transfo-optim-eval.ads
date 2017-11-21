------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . T R A N S F O . O P T I M . E V A L            --
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

with GNAT.Dynamic_Tables;
with Ocarina.Instances.REAL_Checker.Queries;

package Ocarina.Transfo.Optim.Eval is
   use Ocarina.Instances.REAL_Checker.Queries;

   type Thread_Unit is record
      Thread_Node : Node_Id;
   end record;

   package Set is new GNAT.Dynamic_Tables
     (Table_Component_Type => Thread_Unit,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 100, -- # of elements
      Table_Increment      => 50); -- % increase

   type Solution_Set is new Set.Instance;

   procedure Copy (Src : Solution_Set; Dst : out Solution_Set);

   Current_System_Value : Integer;
   Current_System_Cost  : Integer;
   --  FIXME :
   --  criteria should be separated rather than combined

   procedure Init (AADL_Instance : Node_Id);
   --  Perform needed initializations (REAL)

   procedure Compute_Relative_Cost
     (Solution :     Solution_Set;
      Result   : out Float;
      Success  : out Boolean);
   --  Evaluate the cost of the fusion of threads in Solution
   --  for the current system, using REAL theorems

   procedure Compute_Relative_Value
     (Solution :     Solution_Set;
      Result   : out Float;
      Success  : out Boolean);
   --  Evaluate the value of the fusion of threads in Solution
   --  for the current system, using REAL theorems

   procedure Compute_Relative_Move_Value
     (Solution :     Solution_Set;
      Result   : out Float;
      Success  : out Boolean);
   --  Evaluate the value of a move (of a thread within the
   --  solution set) from its process to the one specified in the
   --  solution set

   function Compute_Multi_Criteria_Move_Value (Memory : Float) return Float;

   procedure Compute_System_Cost (Result : out Float; Success : out Boolean);
   --  Evaluate the system cost with REAL theorems

   procedure Compute_System_Value (Result : out Float; Success : out Boolean);
   --  Evaluate the system cost with REAL theorems

   procedure Precise_System_Evaluation
     (Result  : out Float;
      Success : out Boolean);
   --  Compute precisely system value, using external tools
   --  such as Bound-T and Cheddar. As a side effect, will
   --  update the model non-functionnal properties (timing,
   --  memory...) if needed.
   --  NOTE : Very costy, should not be called frequently

   procedure Register_Current_Values (Memory : Float; WCET : Float);
   --  Set the values of all criteria for current system

   function Current_Cost (Candidate_Cost : Float) return Float;
   --  Return the current cost of the system

   function Get_Current_Value return Float;
   --  Return the current value of the system

   function Build_Domain (Solution : Solution_Set) return Result_Set;
   --  Create a domain that contains all the elements to be fusioned

end Ocarina.Transfo.Optim.Eval;
