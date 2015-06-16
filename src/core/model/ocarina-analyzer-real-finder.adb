------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--         O C A R I N A . A N A L Y Z E R . R E A L . F I N D E R          --
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

with Ocarina.Namet;
with Utils;
with Ocarina.Builder.REAL;
with Ocarina.Analyzer.AADL.Finder;
with Ocarina.Analyzer.Messages;
with Ocarina.ME_REAL.Tokens;
with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.REAL_Tree.Utils;
with Ocarina.ME_AADL.AADL_Tree.Nodes;

package body Ocarina.Analyzer.REAL.Finder is

   use Ocarina.Namet;
   use Ocarina.Builder.REAL;
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Utils;
   use Ocarina.Analyzer.Messages;

   package RNU renames Ocarina.ME_REAL.REAL_Tree.Nutils;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   ----------------------------
   -- Get_REAL_Theorems_List --
   ----------------------------

   function Get_REAL_Annexes_List
     (AADL_Root : Node_Id) return RNU.Node_List.Instance
   is
      use Ocarina.Analyzer.AADL.Finder;

      L1 : Ocarina.Types.Node_List;
      L2 : Ocarina.Types.Node_List;
      N1 : Node_Id;
      N2 : Node_Id;
      NA : Node;
      NL : RNU.Node_List.Instance;
   begin
      RNU.Node_List.Init (NL);

      L1 :=
        Find_All_Declarations
          (AADL_Root,
           (ATN.K_Component_Type, ATN.K_Component_Implementation));
      N1 := L1.First;
      while Present (N1) loop
         L2 := Find_All_Subclauses (N1, (1 => ATN.K_Annex_Subclause));

         N2 := L2.First;
         while Present (N2) loop
            if Get_Name_String
                (Utils.To_Lower (ATN.Name (ATN.Identifier (N2)))) =
              Ocarina.ME_REAL.Tokens.Language
              and then Present (ATN.Corresponding_Annex (N2))
            then
               NA.Node := N2;
               RNU.Node_List.Append (NL, NA);
            end if;
            N2 := ATN.Next_Entity (N2);
         end loop;

         N1 := ATN.Next_Entity (N1);
      end loop;

      return NL;
   end Get_REAL_Annexes_List;

   ------------------
   -- Get_Set_Type --
   ------------------

   function Get_Set_Type (S : Node_Id) return Value_Id is
      pragma Assert (Kind (S) = K_Set_Reference);

      R : constant Node_Id := REAL_Root;
      N : Node_Id;
      M : Name_Id;
      T : Value_Id;
   begin
      N := Find_Node_By_Name (Name (S), Used_Set (R));

      if Present (N) then
         --  Update reference

         Set_Referenced_Set (S, N);

         --  If the new set depends on a dependant set,
         --  then it is also dependant

         if Dependant (N) /= Value_Id (0) then
            Is_Dependant := True;
         end if;

         return Set_Type (N);
      else
         if Predefined_Type (S) /= SV_No_Type then

            T := Predefined_Type (S);
            Set_Str_To_Name_Buffer
              (Ocarina.ME_REAL.Tokens.Image (Translate_Predefined_Sets (T)));

         elsif Get_Name_String (Name (S)) = "local_set" then
            T := SV_Local_Set;
            Set_Str_To_Name_Buffer
              (Ocarina.ME_REAL.Tokens.Image (Translate_Predefined_Sets (T)));
         else
            Display_Analyzer_Error
              (No_Node,
               "unable to determine actual type of " &
               Get_Name_String (Name (S)),
               Loc => Loc (S));
            return Value_Id (0);
         end if;

         M := Name_Find;

         --  Create a set in order to register actual use of the
         --  predefined set

         N := Make_Set (M, T);
         Set_Predefined_Type (N, T);
         Set_Referenced_Set (S, N);
         Set_Set_Type (N, T);
         Append_Node_To_List (N, Used_Set (R));

         return T;
      end if;
   end Get_Set_Type;

   -----------------------------
   -- Compute_Expression_Type --
   -----------------------------

   function Compute_Expression_Type (E : Node_Id) return Value_Id is
      pragma Assert
        (Kind (E) = K_Set_Expression or else Kind (E) = K_Set_Reference);

      T1, T2 : Value_Id;
   begin
      --  Parameter is a set reference

      if Kind (E) = K_Set_Reference then
         return Get_Set_Type (E);
      end if;

      --  Parameter is a set expression

      if Present (Left_Expr (E)) and then Present (Right_Expr (E)) then
         T1 := Compute_Expression_Type (Left_Expr (E));
         T2 := Compute_Expression_Type (Right_Expr (E));
         if T1 = Value_Id (0) or else T2 = Value_Id (0) then
            return Value_Id (0);
         end if;

         if T1 = T2 then
            Set_Set_Type (E, T2);
            return T2;
         else
            case Operator (E) is
               when OV_Plus =>
                  Set_Set_Type (E, SV_Generic);
                  return SV_Generic;

               when OV_Star =>
                  Display_Analyzer_Error
                    (No_Node,
                     "can not perform an Intersection " &
                     "between two sets of different types",
                     Loc => Loc (E));
                  return Value_Id (0);

               when OV_Minus =>
                  Set_Set_Type (E, T1);
                  return T1;

               when others =>
                  Display_Analyzer_Error
                    (No_Node,
                     "Parsing error : Impossible operator for " &
                     "Set expressions",
                     Loc => Loc (E));
                  return Value_Id (0);
            end case;
         end if;

      elsif Present (Right_Expr (E)) then
         T1 := Compute_Expression_Type (Right_Expr (E));
         if T1 = Value_Id (0) then
            return Value_Id (0);
         end if;
         Set_Set_Type (E, T1);
         return T1;
      end if;

      Display_Analyzer_Error
        (No_Node,
         "Parsing error : An expression must have " &
         "at least a right operand",
         Loc => Loc (E));
      return Value_Id (0);

   end Compute_Expression_Type;

end Ocarina.Analyzer.REAL.Finder;
