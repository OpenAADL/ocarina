------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         OCARINA.INSTANCES.REAL_CHECKER.QUERIES.BOUND_PREDICATES          --
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

with Ocarina.Instances.Queries;
with Ocarina.Namet;

package body Ocarina.Instances.REAL_Checker.Queries.Bound_Predicates is
   use Ocarina.Instances.Queries;

   ------------------------
   -- Is_Bound_Predicate --
   ------------------------

   function Is_Bound_Predicate
     (E      : Node_Id;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean
   is
      pragma Unreferenced (Option);
      use Ocarina.Namet;

      Str_1 : constant Name_Id := Get_String_Name ("actual_processor_binding");
      Str_2 : constant Name_Id :=
        Get_String_Name ("actual_connection_binding");
      Str_3 : constant Name_Id := Get_String_Name ("actual_memory_binding");
   begin

      --  Returns true if the current node is bound to the parameter
      --  component...

      --  Test for processor and virtual processor binding

      if Is_Defined_Reference_Property (E, Str_1)
        and then Get_Reference_Property (E, Str_1) = D
      then
         return True;
      end if;

      --  Test for connection binding

      if Is_Defined_Reference_Property (E, Str_2)
        and then Get_Reference_Property (E, Str_2) = D
      then
         return True;
      end if;

      --  Test for memory binding

      if Is_Defined_Reference_Property (D, Str_3) then
         if Get_Reference_Property (D, Str_3) = E then
            return True;
         end if;
      end if;

      return False;

   end Is_Bound_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Bound_Predicates;
