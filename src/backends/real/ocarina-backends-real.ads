------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . B A C K E N D S . R E A L                 --
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

--  Resolves REAL theorems for a given AADL model

with Ocarina.Instances.REAL_Checker.Queries;

package Ocarina.Backends.REAL is
   use Ocarina.Instances.REAL_Checker.Queries;

   procedure Generate (AADL_Root : Node_Id);
   --  Display the result of all the REAL theorems execution on
   --  the specified AADL model

   procedure Init;
   --  Initialize the REAL module

   procedure Reset;
   --  Reset the REAL module

   procedure Compute_Theorem_Call
     (E         :     Node_Id;
      Local_Set :     Result_Set;
      Result    : out Value_Id;
      Success   : out Boolean);
   --  Allows to call directly returning theorems with user-specified
   --  domain, without using top level theorems

private

   --  Internal types declarations

   type Set_Table is
     array
       (Integer range <>) of Ocarina.Instances.REAL_Checker.Queries.Result_Set;

   type Set_Table_Access is access Set_Table;

   --  Global sets array
   --  all existing sets are declared within this array

   Set_Array : Set_Table_Access;

end Ocarina.Backends.REAL;
