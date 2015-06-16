------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . R E A L _ E X P A N D E R                 --
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

with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.REAL_Tree.Utils;
with Ocarina.ME_REAL.REAL_Tree.Nutils;
with Ocarina.Builder.REAL;
with Ocarina.Analyzer.REAL.Finder;
with Ocarina.REAL_Values;
with Ocarina.Namet;

package body Ocarina.REAL_Expander is
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Utils;
   use Ocarina.ME_REAL.REAL_Tree.Nutils;
   use Ocarina.Builder.REAL;
   use Ocarina.Analyzer.REAL.Finder;
   use Ocarina.Namet;

   ------------
   -- Expand --
   ------------

   procedure Expand (Theorem : Node_Id; Success : out Boolean) is
      pragma Assert (Kind (Theorem) = K_Theorem);
   begin
      Build_Used_Sets_List (Theorem, Success);
   end Expand;

   --------------------------
   -- Build_Used_Sets_List --
   --------------------------

   procedure Build_Used_Sets_List (R : Node_Id; Success : out Boolean) is
      pragma Assert (Kind (R) = K_Theorem);

      Decl       : Node_Id := First_Node (Declarations (R));
      Expr_Value : Value_Id;
      Set_Name   : Name_Id;
      Set        : Node_Id;
      Interval   : Node_Id;
      Ref        : Node_Id;
      Range_Var  : Node_Id;
      Local_Var  : Node_Id;
   begin
      Success := True;

      --  We first add the global range set to the used set list

      Interval   := Range_Set (Range_Declaration (R));
      Expr_Value := Compute_Expression_Type (Interval);
      if Kind (Interval) /= K_Set_Reference then
         Set := Make_Anonymous_Set (Expr_Value);
         Set_Set_Expression (Set, Interval);
         Ref := Make_Set_Reference;
         Set_Referenced_Set (Ref, Set);
         Set_Set_Reference (Range_Variable (Range_Declaration (R)), Ref);
         Append_Node_To_List (Set, Used_Set (R));
      else
         Ref := Make_Set_Reference;
         Set_Referenced_Set (Ref, Referenced_Set (Interval));
         Set_Set_Reference (Range_Variable (Range_Declaration (R)), Ref);
      end if;

      --  We create a variable corresponding to the range variable

      Range_Var :=
        Make_Variable
          (Name (Identifier (Range_Variable (Range_Declaration (R)))));
      Set_Var_Type (Range_Var, RT_Element);
      Set_Referenced_Var (Variable_Ref (Range_Declaration (R)), Range_Var);
      Append_Node_To_List (Range_Var, Used_Var (R));

      while Present (Decl) loop

         if Kind (Decl) = K_Set_Declaration then

            --  Analyze a set declaration

            --  initialize dependance to false

            Is_Dependant := False;

            --  Get new set name

            Set_Name := Name (Identifier (Parametrized_Expr (Decl)));

            --  Analyze the variable declaration

            Local_Var := Make_Variable (Name (Local_Variable (Decl)));
            Set_Var_Type (Local_Var, RT_Element);
            Set_Referenced_Var (Local_Variable (Decl), Local_Var);

            --  In order to allows restricted-scope variables,
            --  we do not include the local variable in the
            --  global list. Instead, local objects should be
            --  searched in a local list.

            --  Compute expression set actual type

            Expr_Value :=
              Compute_Expression_Type (Local_Set_Expression (Decl));
            if Expr_Value = Value_Id (0) then
               Success := False;
               return;
            end if;

            --  Create and bind the local set

            Set := Make_Anonymous_Set (Expr_Value);
            Ref := Make_Set_Reference;
            Set_Referenced_Set (Ref, Set);
            Set_Local_Set (Decl, Ref);
            Append_Node_To_List (Set, Used_Set (R));

            --  Referenced set is the globally-visible set

            Set := Make_Set (Set_Name, Expr_Value);
            Set_Referenced_Set (Decl, Set);
            Append_Node_To_List (Set, Used_Set (R));

            --  If the set depends

            if Is_Dependant then
               Set_Dependant (Set, Value_Id (2));
            end if;
         else
            --  Analyze a variable declaration

            --  if the variable scope is local to the theorem
            if Is_Global (Decl) = Value_Id (0) then

               Set_Name  := Name (Var_Ref (Decl));
               Local_Var := Make_Variable (Set_Name);
               Set_Referenced_Var (Var_Ref (Decl), Local_Var);
               Append_Node_To_List (Local_Var, Used_Var (R));

            --  if the variable scope is global
            else
               Set_Name  := Name (Var_Ref (Decl));
               Local_Var := Make_Variable (Set_Name);
               Set_Referenced_Var (Var_Ref (Decl), Local_Var);
               Append_Variable_To_Global_Variables (Local_Var);
            end if;
         end if;

         Decl := Next_Node (Decl);
      end loop;

      --  Build environment access variable

      declare
         use Ocarina.REAL_Values;
         Arg_Name : Name_Id;
      begin
         for I in 0 .. 10 loop
            Set_Str_To_Name_Buffer ("argv_" & Image (I));
            Arg_Name  := Name_Find;
            Local_Var := Make_Variable (Arg_Name);
            Set_Var_Type (Local_Var, RT_Unknown);
            Append_Node_To_List (Local_Var, Used_Var (R));
         end loop;
      end;
   end Build_Used_Sets_List;

end Ocarina.REAL_Expander;
