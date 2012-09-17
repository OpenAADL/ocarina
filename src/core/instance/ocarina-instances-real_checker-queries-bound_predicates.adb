------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         OCARINA.INSTANCES.REAL_CHECKER.QUERIES.BOUND_PREDICATES          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2012 ESA & ISAE.        --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Ocarina.Instances.Queries;
with Namet;

package body Ocarina.Instances.REAL_Checker.Queries.Bound_Predicates is
   use Ocarina.Instances.Queries;

   ------------------------
   -- Is_Bound_Predicate --
   ------------------------

   function Is_Bound_Predicate
     (E      : Node_Id;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Boolean
   is
      pragma Unreferenced (Option);
      use Namet;

      Str_1  : constant Name_Id := Get_String_Name
        ("actual_processor_binding");
      Str_2  : constant Name_Id := Get_String_Name
        ("actual_connection_binding");
      Str_3  : constant Name_Id := Get_String_Name
        ("actual_memory_binding");
   begin

      --  Returns true if the current node is bound to the parameter
      --  component...

      --  Test for processor and virtual processor binding

      if Is_Defined_Reference_Property (E, Str_1) and then
        Get_Reference_Property (E, Str_1) = D then
         return True;
      end if;

      --  Test for connection binding

      if Is_Defined_Reference_Property (E, Str_2) and then
        Get_Reference_Property (E, Str_2) = D then
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
