------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      OCARINA.INSTANCES.REAL_CHECKER.QUERIES.RELATIONAL_PREDICATES        --
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

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.Instances.Finder;

package body Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates is
   use Ocarina.Instances.Finder;
   use Set;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   ---------------------------------------
   -- Get_Instances_Verifying_Predicate --
   ---------------------------------------

   function Get_Instances_Verifying_Predicate
     (D : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Result_Set
   is
      use Ocarina.ME_AADL.AADL_Instances.Nodes;
      Result : Result_Set;
      EL     : Node_List;
   begin
      Init (Result);
      Find_All_Instances
        (Root_Instance,
         (K_Component_Instance,
          K_Call_Instance,
          K_Call_Sequence_Instance),
         EL.First, EL.Last);
      while Present (EL.First) loop
         if Predicate (EL.First, D, Option) then
            Append (Result, EL.First);
         end if;
         EL.First := AIN.Next_Entity (EL.First);
      end loop;

      Find_All_Flows (Root_Instance, EL.First, EL.Last);
      while Present (EL.First) loop
         if Predicate (EL.First, D, Option) then
            Append (Result, EL.First);
         end if;
         EL.First := ATN.Next_Entity (EL.First);
      end loop;

      return Result;
   end Get_Instances_Verifying_Predicate;

   ---------------------------------------
   -- Get_Instances_Verifying_Predicate --
   ---------------------------------------

   function Get_Instances_Verifying_Predicate
     (Set    : Result_Set;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct)
     return Result_Set
   is
      Result : Result_Set;
   begin
      Init (Result);
      for N in First .. Last (Set) loop
         if Predicate (Set.Table (N), D, Option) then
            Append (Result, Set.Table (N));
         end if;
      end loop;

      return Result;
   end Get_Instances_Verifying_Predicate;

   -----------
   -- Apply --
   -----------

   function Apply
     (Set_1    : Result_Set;
      Set_2    : Result_Set;
      Reversed : Boolean := False;
      Distinct : Boolean := False;
      Option   : Predicates_Search_Options := PSO_Direct)
     return Result_Set
   is
      M      : Natural;
      Result : Result_Set;
      R1, R2 : Result_Set;
      Found  : Boolean;
   begin
      Init (Result);
      if Reversed then
         R1 := Set_2;
         R2 := Set_1;
      else
         R1 := Set_1;
         R2 := Set_2;
      end if;

      for N in First .. Last (R1) loop
         M := 1;
         Found := False;

         while M < (Last (R2) + 1)
           and then (not Distinct or else not found) loop
            if Predicate (R1.Table (N), R2.Table (M), Option) then
               if not Reversed then
                  Append (Result, R1.Table (N));
               else
                  Append (Result, R2.Table (N));
               end if;

               Found := True;
            end if;

            M := M + 1;
         end loop;
      end loop;

      return Result;
   end Apply;

end Ocarina.Instances.REAL_Checker.Queries.Relational_Predicates;
