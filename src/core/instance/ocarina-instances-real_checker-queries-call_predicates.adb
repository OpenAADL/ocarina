------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         OCARINA.INSTANCES.REAL_CHECKER.QUERIES.CALL_PREDICATES           --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

package body Ocarina.Instances.REAL_Checker.Queries.Call_Predicates is
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   use AINU;

   -------------------------
   -- Is_Called_Predicate --
   -------------------------

   function Is_Called_Predicate
     (E      : Node_Id;
      D      : Node_Id;
      Option : Predicates_Search_Options := PSO_Direct) return Boolean
   is
      function Find_Called_Subprograms (E : Node_Id) return Result_Set;
      --  Return the set of subprograms instances called by the
      --  component

      function Find_Calls (E : Node_Id) return Result_Set;
      --  Return the set of subprograms calls instances called by the
      --  component

      -----------------------------
      -- Find_Called_Subprograms --
      -----------------------------

      function Find_Called_Subprograms (E : Node_Id) return Result_Set is
         Set : Result_Set := Empty_Set;
      begin
         if E /= No_Node and then not AINU.Is_Empty (Calls (E)) then
            declare
               Call_Seq : Node_Id := First_Node (Calls (E));
               N        : Node_Id;
            begin
               while Present (Call_Seq) loop
                  if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                     N := First_Node (Subprogram_Calls (Call_Seq));
                     while Present (N) loop
                        Add
                          (Set,
                           Corresponding_Declaration
                             (Corresponding_Instance (N)));
                        N := Next_Node (N);
                     end loop;
                     Call_Seq := Next_Node (Call_Seq);
                  end if;
               end loop;
            end;
         end if;

         return Set;
      end Find_Called_Subprograms;

      ----------------
      -- Find_Calls --
      ----------------

      function Find_Calls (E : Node_Id) return Result_Set is
         Set : Result_Set := Empty_Set;
      begin
         if E /= No_Node and then not AINU.Is_Empty (Calls (E)) then
            declare
               Call_Seq : Node_Id := First_Node (Calls (E));
               N        : Node_Id;
            begin
               while Present (Call_Seq) loop
                  if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                     N := First_Node (Subprogram_Calls (Call_Seq));
                     while Present (N) loop
                        Add (Set, N);
                        N := Next_Node (N);
                     end loop;
                     Call_Seq := Next_Node (Call_Seq);
                  end if;
               end loop;
            end;
         end if;

         return Set;
      end Find_Calls;

      Direct_Calls_Set : Result_Set;
      Final_Set        : Result_Set := Empty_Set;
   begin
      if Kind (E) /= K_Call_Instance
        and then
        (Kind (E) /= K_Component_Instance
         or else Get_Category_Of_Component (E) /= CC_Subprogram)
      then
         return False;
      end if;

      if Option = PSO_Direct then

         --  Returns true if the current node is a called by
         --  the parameter component...

         if Kind (E) = K_Call_Instance then
            Direct_Calls_Set := Find_Calls (D);
            return Is_In (E, Direct_Calls_Set);
         else
            Direct_Calls_Set := Find_Called_Subprograms (D);
            return Is_In (Corresponding_Declaration (E), Direct_Calls_Set);
         end if;
      else
         --  Search called subprograms by generations

         --  Note that the fact we don't use 'distinct' option
         --  in unions allows us to search for recursive loops
         --  in subprograms calls.

         declare
            Tmp_Set : Result_Set;
         begin
            if Kind (E) = K_Call_Instance then
               Direct_Calls_Set := Find_Calls (D);
            else
               Direct_Calls_Set := Find_Called_Subprograms (D);
            end if;

            while not Is_Empty (Direct_Calls_Set) loop
               for I in 1 .. Cardinal (Direct_Calls_Set) loop
                  if Kind (E) = K_Call_Instance then
                     Tmp_Set := Find_Calls (Get (Direct_Calls_Set, I));
                  else
                     Tmp_Set :=
                       Find_Called_Subprograms
                         (Corresponding_Instance (Get (Direct_Calls_Set, I)));
                  end if;
                  Direct_Calls_Set := Union (Tmp_Set, Direct_Calls_Set);
               end loop;
               Final_Set        := Union (Final_Set, Direct_Calls_Set);
               Direct_Calls_Set := Empty_Set;
            end loop;
         end;

         return Is_In (E, Final_Set);
      end if;

   end Is_Called_Predicate;

end Ocarina.Instances.REAL_Checker.Queries.Call_Predicates;
