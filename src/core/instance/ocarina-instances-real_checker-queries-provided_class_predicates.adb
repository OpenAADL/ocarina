------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    OCARINA.INSTANCES.REAL_CHECKER.QUERIES.PROVIDED_CLASS_PREDICATES      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2012-2014 ESA & ISAE.                    --
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
      Option : Predicates_Search_Options := PSO_Direct) return Boolean
   is
      pragma Unreferenced (Option);
      use Namet;

      Str_1 : constant Name_Id :=
        Get_String_Name ("provided_virtual_bus_class");
   begin

      --  Returns true if the current node provides a component of
      --  same class than the parameter component.

      --  Test for virtual buses

      if Is_Defined_Classifier_Property (E, Str_1) then
         if AIN.Corresponding_Declaration
             (Get_Classifier_Property (E, Str_1)) =
           AIN.Corresponding_Declaration (D)
         then
            return True;
         end if;
      end if;

      return False;
   end Is_Provided_Class_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Provided_Class_Predicates;
