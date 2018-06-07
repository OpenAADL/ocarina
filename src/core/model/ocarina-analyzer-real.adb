------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . A N A L Y Z E R . R E A L                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2018 ESA & ISAE.        --
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
with Errors;

with Ocarina.Analyzer.Messages;
with Ocarina.Analyzer.REAL.Finder;
with Ocarina.Builder.REAL;
with Ocarina.Instances.REAL_Checker.Queries;
with Ocarina.ME_REAL.Tokens;
with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.REAL_Tree.Utils;
with Ocarina.ME_REAL.REAL_Tree.Nutils;
with Ocarina.ME_REAL.REAL_Tree.Debug;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.REAL_Values;
with Ocarina.REAL_Expander;
with Ocarina.REAL_Expander.Flow_Analysis;

package body Ocarina.Analyzer.REAL is

   use Ocarina.Namet;
   use Errors;
   use Ocarina.Analyzer.REAL.Finder;
   use Ocarina.Analyzer.Messages;
   use Ocarina.Builder.REAL;

   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Utils;
   use Ocarina.ME_REAL.REAL_Tree.Nutils;
   use Ocarina.ME_REAL.REAL_Tree.Debug;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package RN renames Ocarina.ME_REAL.REAL_Tree.Nodes;
   package RT renames Ocarina.ME_REAL.Tokens;
   package RNU renames Ocarina.ME_REAL.REAL_Tree.Nutils;

   procedure Compute_Selection_Subprogram_Calls
     (R       :     Node_Id;
      Success : out Boolean);
   --  Check selection expression correctness

   procedure Compute_Verification_Subprogram_Calls
     (R       :     Node_Id;
      Success : out Boolean);
   --  Check verification expression correctness

   procedure Compute_Return_Subprogram_Calls
     (R       :     Node_Id;
      Success : out Boolean);
   --  Check return expression correctness

   procedure Check_Requirements_Existance (E : Node_Id; Success : out Boolean);
   --  Check weither the requirements are registered as theorems,
   --  and merge the trees.

   function Analyze_Sub_Theorem (R : Node_Id) return Boolean;
   --  Analyze a subtheorem tree

   procedure Analyze_Verification_Expression
     (E       :     Node_Id;
      Success : out Boolean);
   --  Check if the verification expression is well-formed,
   --  and then link all set references in parameters to existing sets.
   --  Check verification expression type consistency.

   procedure Analyze_Check_Subprogram_Call
     (S       :     Node_Id;
      Success : out Boolean);
   --  Bind sets reference to their related set
   --  Put each kind of parameter in the corresponding list
   --  Determine subprogram real return type

   procedure Get_Property_Type
     (E       :     Node_Id;
      Success : out Boolean;
      Result  : out Return_Type);
   --  Ask Ocarina for the property real type

   procedure Analyze_Subtheorem_Parameters
     (E       :     Node_Id;
      Success : out Boolean);
   --  Analyze subtheorem parameters

   function Find_Variable (E : Name_Id) return Node_Id;
   --  returns a variable, searched by name from both used_var list
   --  (theorem-scoped) and global_variables list (global scope).

   AADL_Tree     : Node_Id;
   AADL_Instance : Node_Id;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      AADL_Tree     := No_Node;
      AADL_Instance := No_Node;
      Ocarina.ME_REAL.REAL_Tree.Nutils.Init;
      Ocarina.Analyzer.Register_Analyzer (RT.Language, Analyze_Model'Access);
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      AADL_Tree := No_Node;
      Ocarina.ME_REAL.REAL_Tree.Nutils.Reset;
   end Reset;

   -------------------------------
   -- Register_Library_Theorems --
   -------------------------------

   procedure Register_Library_Theorems (REAL_Library : Node_Id) is
      pragma Assert (Kind (REAL_Library) = K_Root_Node);

      N : Node;
      T : Node_Id;
   begin
      if Is_Empty (Theorems (REAL_Library)) then
         return;
      end if;

      T := First_Node (Theorems (REAL_Library));
      while Present (T) loop
         N.Node := T;
         Set_Related_Entity (N.Node, No_Node);
         RNU.Node_List.Append (Library_Theorems, N);
         T := Next_Node (T);
      end loop;
   end Register_Library_Theorems;

   ------------------------
   -- Build_Theorem_List --
   ------------------------

   procedure Build_Theorem_List (AADL_Root : Node_Id) is
      NL : RNU.Node_List.Instance;
      N  : Node;
      A  : Node_Id;
      T  : Node_Id;
      It : Natural := RNU.Node_List.First;
   begin
      --  XXX The list of theorem to be checked should be computed
      --  from the instance tree instead

      if Main_Theorem = No_Name then
         --  We walk through all annex clauses to build the list of
         --  theorems to be checked.

         NL := Get_REAL_Annexes_List (AADL_Root);
         while It <= RNU.Node_List.Last (NL) loop
            A := ATN.Corresponding_Annex (NL.Table (It).Node);

            T := First_Node (Theorems (A));
            while Present (T) loop
               N.Node := T;

               --  Set link to the container AADL component for each
               --  theorem in the annex

               Set_Related_Entity
                 (N.Node,
                  ATN.Container_Component (NL.Table (It).Node));

               --  Append to the list of theorems to be run

               RNU.Node_List.Append (To_Run_Theorem_List, N);

               T := Next_Node (T);
            end loop;

            It := It + 1;
         end loop;

      else
         --  Otherwise, iterate over Library theorems and fetch the
         --  corresponding theorem.

         RNU.Node_List.Init (To_Run_Theorem_List); --  Reset list of theorems

         for J in RNU.Node_List.First .. RNU.Node_List.Last (Library_Theorems)
         loop
            A := Library_Theorems.Table (J).Node;

            if Main_Theorem = Name (Identifier (A)) then
               N.Node := A;
               Set_Related_Entity (N.Node, AADL_Root);

               --  Append to the list of theorems to be run

               RNU.Node_List.Append (To_Run_Theorem_List, N);
            end if;
         end loop;
      end if;
   end Build_Theorem_List;

   -------------------
   -- Analyze_Model --
   -------------------

   function Analyze_Model (Root : Node_Id) return Boolean is
      use AIN;
      use Ocarina.REAL_Expander;
      use Ocarina.REAL_Expander.Flow_Analysis;

      pragma Assert (AIN.Kind (Root) = AIN.K_Architecture_Instance);

      Node    : Node_Id;
      It      : Natural := RNU.Node_List.First;
      Success : Boolean;
   begin
      AADL_Tree :=
        ATN.Parent
          (ATN.Namespace (Corresponding_Declaration (Root_System (Root))));

      AADL_Instance := Root;

      --  Build the theorem list

      Build_Theorem_List (AADL_Tree);

      --  Build flows

      Explore_Flows;
      Store_Flows (Root);

      --  Init queries

      Ocarina.Instances.REAL_Checker.Queries.Init (Root);

      while It <= RNU.Node_List.Last (To_Run_Theorem_List) loop
         Node          := To_Run_Theorem_List.Table (It).Node;
         RNU.REAL_Root := Node;

         --  Expansion step

         Ocarina.REAL_Expander.Expand (Node, Success);
         if not Success then
            Display_Analyzer_Error
              (RN.Related_Entity (Node),
               "could not proceed to theorem expansion");
            return False;
         end if;

         --  Analyze step

         Ocarina.Analyzer.REAL.Analyze_Theorem (RNU.REAL_Root, Root, Success);
         if not Success then
            Display_Analyzer_Error
              (RN.Related_Entity (Node),
               "theorem analysis failed");
            return False;
         end if;

         It := It + 1;
      end loop;

      --  For non-used library theorems, we still analyze them,
      --  since they can be called directly through the API

      for J in RNU.Node_List.First ..
        RNU.Node_List.Last (Library_Theorems) loop
         Node := Library_Theorems.Table (J).Node;

         RNU.REAL_Root := Node;

         if not Analyze_Sub_Theorem (RNU.REAL_Root) then
            Display_Analyzer_Error
              (Root, "could not proceed to theorem analysis");
            return False;
         end if;
      end loop;

      return True;
   end Analyze_Model;

   ---------------------
   -- Analyze_Theorem --
   ---------------------

   procedure Analyze_Theorem
     (Theorem   :     Node_Id;
      AADL_Root :     Node_Id;
      Success   : out Boolean)
   is
      use AIN;

      pragma Assert (RN.Kind (Theorem) = RN.K_Theorem);
      pragma Assert (AIN.Kind (AADL_Root) = AIN.K_Architecture_Instance);
   begin
      AADL_Instance := AADL_Root;

      Compute_Selection_Subprogram_Calls (Theorem, Success);
      if not Success then
         return;
      end if;

      if Present (Return_Expression (Theorem)) then
         Compute_Return_Subprogram_Calls (Theorem, Success);
      else
         Compute_Verification_Subprogram_Calls (Theorem, Success);
      end if;
      if not Success then
         return;
      end if;

      Check_Requirements_Existance (Theorem, Success);
      if not Success then
         return;
      end if;
   end Analyze_Theorem;

   ----------------------------------
   -- Check_Requirements_Existance --
   ----------------------------------

   procedure Check_Requirements_Existance
     (E       :     Node_Id;
      Success : out Boolean)
   is
      pragma Assert (Kind (E) = K_Theorem);

      R      : constant List_Id := Required_Theorems (E);
      N, T   : Node_Id;
      Caller : constant Node_Id := RNU.REAL_Root;
   begin
      if not Is_Empty (R) then
         N := First_Node (R);
      else
         N := No_Node;
      end if;

      while Present (N) loop
         T := Find_Declared_Theorem (Theorem_Name (N));

         if No (T) then
            Display_Analyzer_Error
              (No_Node,
               Get_Name_String (Theorem_Name (N)) &
               " is not a declared theorem",
               Loc => Loc (N));
            Success := False;
            return;
         else
            RNU.REAL_Root := T;
            if not Analyze_Sub_Theorem (RNU.REAL_Root) then
               Display_Analyzer_Error
                 (RN.Related_Entity (E),
                  "could not proceed to sub-theorem analysis");
               Success := False;
               return;
            end if;
            Set_Related_Theorem (N, RNU.REAL_Root);
         end if;

         N := Next_Node (N);
      end loop;
      RNU.REAL_Root := Caller;

      Success := True;
   end Check_Requirements_Existance;

   -------------------------------------------
   -- Compute_Verification_Subprogram_Calls --
   -------------------------------------------

   procedure Compute_Verification_Subprogram_Calls
     (R       :     Node_Id;
      Success : out Boolean)
   is
      pragma Assert (Kind (R) = K_Theorem);
   begin
      Analyze_Verification_Expression (Check_Expression (R), Success);
      if Success
        and then Returned_Type (Check_Expression (R)) /= RT_Boolean
        and then Returned_Type (Check_Expression (R)) /= RT_Unknown
      then
         Display_Analyzer_Error
           (No_Node,
            "Top-level check expression must be a boolean value",
            Loc => Loc (Check_Expression (R)));
         Success := False;
      end if;

   end Compute_Verification_Subprogram_Calls;

   -------------------------------------
   -- Compute_Return_Subprogram_Calls --
   -------------------------------------

   procedure Compute_Return_Subprogram_Calls
     (R       :     Node_Id;
      Success : out Boolean)
   is
      pragma Assert (Kind (R) = K_Theorem);

      T : Return_Type;
      C : constant Node_Id := Check_Expression (Return_Expression (R));
   begin
      Analyze_Verification_Expression (C, Success);
      T := Returned_Type (C);

      case Range_Function (Return_Expression (R)) is
         when FC_MMax | FC_MMin | FC_MSum | FC_MProduct =>
            case T is
               when RT_Unknown =>
                  T             := RT_Float;
                  Error_Loc (1) := Loc (C);
                  DW ("Unable to determine actualy returned type.");

               when RT_Float | RT_Integer =>
                  null;

               when others =>
                  Display_Analyzer_Error
                    (No_Node,
                     "aggregate function expected a numeric value",
                     Loc => Loc (C));
                  Success := False;
            end case;

         when FC_MAll_Equals =>
            case T is
               when RT_Unknown =>
                  T             := RT_Boolean;
                  Error_Loc (1) := Loc (C);
                  DW ("Unable to determine actualy returned type.");

               when RT_Float | RT_Integer =>
                  T := RT_Boolean;

               when others =>
                  Display_Analyzer_Error
                    (No_Node,
                     "aggregate function expected a numeric value",
                     Loc => Loc (C));
                  Success := False;
            end case;

         when others =>
            Display_Analyzer_Error
              (No_Node,
               "aggregate function unknown",
               Loc => Loc (C));
            Success := False;
            T       := RT_Error;
      end case;

      if Success then
         case T is
            when RT_Float =>
               null;

            when RT_Integer =>
               Error_Loc (1) := Loc (Return_Expression (R));
               DW ("Returned integer value will be cast to a float " &
                  "at runtime");

            when RT_Boolean =>
               Error_Loc (1) := Loc (Return_Expression (R));
               DW ("Returned boolean value will be cast to a float " &
                  "at runtime");

            when others =>
               Display_Analyzer_Error
                 (No_Node,
                  "Top-level return expression must be a numeric value",
                  Loc => Loc (Return_Expression (R)));
               Success := False;
         end case;
      end if;
   end Compute_Return_Subprogram_Calls;

   -------------------------
   -- Analyze_Sub_Theorem --
   -------------------------

   function Analyze_Sub_Theorem (R : Node_Id) return Boolean is
      use Ocarina.REAL_Expander;

      pragma Assert (Kind (R) = K_Theorem);

      Success : Boolean := True;
   begin
      --  If the used set is not empty then that theorem has already
      --  been analyzed.

      if not Is_Empty (Used_Set (R)) then
         return True;
      end if;

      --  Proceed to expansion and analysis

      Expand (R, Success);
      if Success then
         Analyze_Theorem (R, AADL_Instance, Success);
      end if;

      return Success;
   end Analyze_Sub_Theorem;

   ----------------------------------------
   -- Compute_Selection_Subprogram_Calls --
   ----------------------------------------

   procedure Compute_Selection_Subprogram_Calls
     (R       :     Node_Id;
      Success : out Boolean)
   is
      pragma Assert (Kind (R) = K_Theorem);

      D : Node_Id := First_Node (Declarations (R));
   begin
      Success := True;
      while Present (D) loop
         case Kind (D) is

            when K_Set_Declaration =>
               Is_Dependant := False;

               --  Set local set to the local stack

               Append_Node_To_List
                 (Referenced_Var (Local_Variable (D)),
                  Local_Var (REAL_Root));

               --  Performs analysis

               Analyze_Verification_Expression
                 (Selection_Expression (D),
                  Success);
               if not Success then
                  Display_Analyzer_Error
                    (No_Node,
                     "Could not analyze set declaration",
                     Loc => Loc (D));
                  return;
               end if;

               --  remove local variable from the local stack

               Remove_Node_From_List
                 (Referenced_Var (Local_Variable (D)),
                  Local_Var (REAL_Root));

            when K_Variable_Decl_Expression =>
               --  Analyze variable declaration

               Analyze_Verification_Expression
                 (Check_Expression (Return_Expr (D)),
                  Success);
               if not Success then
                  Display_Analyzer_Error
                    (No_Node,
                     "Could not analyze variable declaration",
                     Loc => Loc (D));
                  return;
               end if;

               Set_Var_Type
                 (Referenced_Var (Var_Ref (D)),
                  Returned_Type (Check_Expression (Return_Expr (D))));

            when K_Variable_Decl_Compute =>
               declare
                  T      : constant Name_Id := Theorem_Name (D);
                  R      : constant Node_Id := Find_Declared_Theorem (T);
                  Stored : constant Node_Id := REAL_Root;
               begin
                  if No (R) then
                     W_Line
                       ("Error : Theorem " &
                        Get_Name_String (T) &
                        " not found");
                     Success := False;
                     return;
                  end if;

                  REAL_Root := R;
                  if not Analyze_Sub_Theorem (REAL_Root) then
                     W_Line
                       ("analyze of sub-theorem " &
                        Get_Name_String (Name (Identifier (REAL_Root))) &
                        " failed");
                     Success := False;
                     return;
                  end if;
                  REAL_Root := Stored;

                  Set_Related_Theorem (D, R);
                  Set_Var_Type (Referenced_Var (Var_Ref (D)), RT_Float);
                  if Parameters (D) /= No_List then
                     Analyze_Subtheorem_Parameters (D, Success);
                  end if;
               end;

            when others =>
               Display_Analyzer_Error
                 (No_Node,
                  "unexpected node kind in selection expression",
                  Loc => Loc (D));
               Success := False;
               return;

         end case;

         D := Next_Node (D);
      end loop;
   end Compute_Selection_Subprogram_Calls;

   -----------------------------------
   -- Analyze_Subtheorem_Parameters --
   -----------------------------------

   procedure Analyze_Subtheorem_Parameters
     (E       :     Node_Id;
      Success : out Boolean)
   is
      pragma Assert (Kind (E) = K_Variable_Decl_Compute);

      ------------------------
      -- Find_Set_Reference --
      ------------------------

      procedure Find_Set_Reference
        (N       :     Node_Id;
         S       :     Node_Id;
         Success : out Boolean)
      is
         M, D : Node_Id;
      begin
         Success := True;

         case Kind (N) is

            when K_Set_Reference =>
               declare
                  TV : Value_Id;
               begin
                  --  We register the predefined set

                  pragma Unreferenced (TV);
                  TV := Get_Set_Type (N);
                  Set_Domain (S, N);
               end;

            when K_Identifier =>
               D :=
                 Find_Node_By_Name (To_Lower (Name (N)), Used_Set (REAL_Root));
               if Present (D) then

                  --  Add to set reference list

                  M := Make_Set_Reference;
                  Set_Referenced_Set (M, D);
                  Set_Domain (S, M);
               else

                  --  Can be :
                  --  * a reference on "range" variable
                  --  * any element variable
                  --  * a predefined set (including local_set)

                  --  Search related variable in global list

                  D := Find_Variable (To_Lower (Name (N)));
                  if Present (D) then
                     if Var_Type (D) = RT_Element then
                        M := Make_Var_Reference (Name (N));
                        Set_Referenced_Var (M, D);
                        Set_Domain (S, M);
                     else
                        Display_Analyzer_Error
                          (No_Node,
                           Get_Name_String (Name (N)) &
                           " variable is not an element of a set",
                           Loc => Loc (N));
                        Success := False;
                        return;
                     end if;

                  elsif Get_Name_String (Name (N)) = "local_set" then
                     declare
                        T   : Value_Id;
                        NM  : Name_Id;
                        Set : Node_Id;
                     begin
                        M := Make_Set_Reference;

                        T := SV_Local_Set;
                        Set_Str_To_Name_Buffer
                          (Ocarina.ME_REAL.Tokens.Image
                             (Translate_Predefined_Sets (T)));
                        NM := Name_Find;

                        --  Create a set in order to register actual use
                        --  of the predefined set

                        Set := Make_Set (NM, T);
                        Set_Predefined_Type (Set, T);
                        Set_Referenced_Set (M, Set);
                        Set_Set_Type (Set, T);
                        Append_Node_To_List (Set, Used_Set (REAL_Root));

                        Set_Domain (S, M);
                     end;
                  else
                     Display_Analyzer_Error
                       (No_Node,
                        Get_Name_String (Name (N)) &
                        " is not a declared set or variable",
                        Loc => Loc (N));
                     Success := False;
                     return;
                  end if;
               end if;

            when others =>
               Display_Analyzer_Error
                 (No_Node,
                  "wrong parameter type",
                  Loc => Loc (N));
               Success := False;
         end case;
      end Find_Set_Reference;

      N : Node_Id := First_Node (Parameters (E));
      P : Node_Id;
      D : Node_Id;
   begin
      Success := True;

      if not Present (N) then
         Set_Domain (E, No_Node);
         return;
      end if;

      Find_Set_Reference (N, E, Success);
      Set_True_params (E, New_List (K_List_Id, Loc (E)));
      if not Success then
         Display_Analyzer_Error
           (No_Node,
            "First parameter of subprogram " & "call is not a set",
            Loc => Loc (N));
         return;
      end if;

      N := Next_Node (N);
      while Present (N) loop
         case Kind (N) is

            when K_Literal =>
               P := New_Node (K_Literal, Loc (N));
               Set_Value (P, Value (N));
               declare
                  use Ocarina.REAL_Values;
                  V : constant Value_Type := Get_Value_Type (Value (N));
               begin
                  case V.T is
                     when LT_Integer =>
                        Set_Returned_Type (P, RT_Integer);

                     when LT_Real =>
                        Set_Returned_Type (P, RT_Float);

                     when LT_String =>
                        Set_Returned_Type (P, RT_String);

                     when LT_Boolean =>
                        Set_Returned_Type (P, RT_Boolean);

                     when LT_Enumeration =>
                        Set_Returned_Type (P, RT_String);

                     when others =>  -- Can't happen
                        Display_Analyzer_Error
                          (No_Node,
                           "unexpected value type in literal",
                           Loc => Loc (N));
                        Success := False;
                        return;
                  end case;
               end;
               Append_Node_To_List (P, True_params (E));

            when K_Identifier =>
               D := Find_Variable (To_Lower (Name (N)));
               if Present (D) then
                  P := Make_Var_Reference (Name (N));
                  Set_Referenced_Var (P, D);
                  Append_Node_To_List (P, True_params (E));
               else
                  Display_Analyzer_Error
                    (No_Node,
                     "could not find variable " &
                     Get_Name_String (To_Lower (Name (N))),
                     Loc => Loc (N));
                  Success := False;
                  return;
               end if;

            when others =>
               Display_Analyzer_Error
                 (No_Node,
                  "subtheorem parameter " &
                  "must be a literal or an identifier",
                  Loc => Loc (N));
               Success := False;
               return;
         end case;

         N := Next_Node (N);
      end loop;
   end Analyze_Subtheorem_Parameters;

   -------------------------------------
   -- Analyze_Verification_Expression --
   -------------------------------------

   procedure Analyze_Verification_Expression
     (E       :     Node_Id;
      Success : out Boolean)
   is
      use Ocarina.REAL_Values;

      pragma Assert
        (Kind (E) = K_Check_Expression
         or else Kind (E) = K_Ternary_Expression
         or else Kind (E) = K_Literal
         or else Kind (E) = K_Var_Reference
         or else Kind (E) = K_Check_Subprogram_Call);

      T1, T2       : Return_Type;
      V            : Value_Type;
      Part_Unknown : Boolean;
   begin
      Success := True;

      case Kind (E) is
         when K_Var_Reference =>  --  Found a variable
            declare
               N : Node_Id;
            begin
               N := Find_Variable (To_Lower (Name (E)));

               --  The variable must have been previsously
               --  defined andd analyzed.

               if No (N) then
                  N :=
                    Find_Node_By_Name
                      (To_Lower (Name (E)),
                       Local_Var (REAL_Root));
                  if No (N) then
                     Display_Analyzer_Error
                       (No_Node,
                        "could not find variable " &
                        Get_Name_String (To_Lower (Name (E))),
                        Loc => Loc (E));
                     Success := False;
                     return;
                  end if;
               end if;

               if Var_Type (N) = No_Value then
                  Display_Analyzer_Error
                    (No_Node,
                     Get_Name_String (To_Lower (Name (E))) &
                     " is being used before being defined",
                     Loc => Loc (E));
                  Success := False;
                  return;
               end if;
               Set_Referenced_Var (E, N);
               Set_Returned_Type (E, Var_Type (N));
            end;

         when K_Check_Subprogram_Call =>
            Analyze_Check_Subprogram_Call (E, Success);

         when K_Literal =>
            V := Get_Value_Type (Value (E));
            case V.T is
               when LT_Integer =>
                  Set_Returned_Type (E, RT_Integer);

               when LT_Real =>
                  Set_Returned_Type (E, RT_Float);

               when LT_String =>
                  Set_Returned_Type (E, RT_String);

               when LT_Boolean =>
                  Set_Returned_Type (E, RT_Boolean);

               when LT_Enumeration =>
                  Set_Returned_Type (E, RT_String);

               when others =>
                  Display_Analyzer_Error
                    (No_Node,
                     "unexpected value type in literal",
                     Loc => Loc (E));
                  Success := False;
                  return;
            end case;

         when K_Ternary_Expression =>
            Analyze_Verification_Expression (Left_Expr (E), Success);
            if Success then
               Analyze_Verification_Expression (Right_Expr (E), Success);
            end if;

            if Success then
               Analyze_Verification_Expression (Third_Expr (E), Success);
            end if;
            if Success then
               T1 := Returned_Type (Left_Expr (E));
               if T1 /= RT_Boolean then
                  Display_Analyzer_Error
                    (No_Node,
                     "Ternary expression must begin by a boolean " &
                     "expression ",
                     Loc => Loc (E));
                  Success := False;
                  return;
               end if;
               T1 := Returned_Type (Right_Expr (E));
               T2 := Returned_Type (Third_Expr (E));
               if T1 = T2 then
                  Set_Returned_Type (E, T1);
               else
                  Set_Returned_Type (E, RT_Unknown);
               end if;
            end if;
            if not Success then
               return;
            end if;

         when K_Check_Expression =>
            if Present (Left_Expr (E)) and then Present (Right_Expr (E)) then
               Analyze_Verification_Expression (Left_Expr (E), Success);

               if Success then
                  Analyze_Verification_Expression (Right_Expr (E), Success);
               end if;

               if not Success then
                  return;
               end if;
               T1           := Returned_Type (Left_Expr (E));
               T2           := Returned_Type (Right_Expr (E));
               Part_Unknown := (T1 = RT_Unknown or else T2 = RT_Unknown);

               case Operator (E) is
                  when OV_And | OV_Or =>
                     if not Part_Unknown then
                        if T1 /= RT_Boolean or else T2 /= RT_Boolean then
                           Display_Analyzer_Error
                             (No_Node,
                              "Inconsistent types in expression " &
                              "<boolean_operator> : " &
                              "must be boolean sub-expressions",
                              Loc => Loc (E));
                           Success := False;
                           return;
                        else
                           Set_Returned_Type (E, RT_Boolean);
                        end if;
                     else
                        Set_Returned_Type (E, RT_Boolean);
                     end if;

                  when OV_Equal =>
                     if not Part_Unknown then
                        if T1 /= T2 then
                           Display_Analyzer_Error
                             (No_Node,
                              "Inconsistent types in expression '='",
                              Loc => Loc (E));
                           Success := False;
                           return;
                        else
                           Set_Returned_Type (E, RT_Boolean);
                        end if;
                     else
                        Set_Returned_Type (E, RT_Boolean);
                     end if;

                  when OV_Different =>
                     if not Part_Unknown then
                        if T1 /= T2 then
                           Display_Analyzer_Error
                             (No_Node,
                              "Inconsistent types in expression '<>'",
                              Loc => Loc (E));
                           Success := False;
                           return;
                        else
                           Set_Returned_Type (E, RT_Boolean);
                        end if;
                     else
                        Set_Returned_Type (E, RT_Boolean);
                     end if;

                  when OV_Greater    |
                    OV_Less          |
                    OV_Less_Equal    |
                    OV_Greater_Equal =>
                     if not Part_Unknown then
                        if (T2 /= RT_Float and then T2 /= RT_Integer)
                          or else (T1 /= RT_Float and then T1 /= RT_Integer)
                        then
                           Display_Analyzer_Error
                             (No_Node,
                              "Inconsistent types in expression " &
                              "<comparator>",
                              Loc => Loc (E));
                           Success := False;
                           return;
                        else
                           Set_Returned_Type (E, RT_Boolean);
                        end if;
                     else
                        Set_Returned_Type (E, RT_Boolean);
                     end if;

                  when OV_Plus =>
                     if not Part_Unknown
                       and then
                       ((T1 /= T2
                         and then
                         ((T1 /= RT_Float and then T1 /= RT_Integer)
                          or else (T2 /= RT_Float and then T2 /= RT_Integer)))
                        or else
                        (T1 = RT_Boolean
                         or else T1 = RT_Element
                         or else T1 = RT_Range
                         or else T1 = RT_String
                         or else T1 = RT_Error))
                     then
                        Display_Analyzer_Error
                          (No_Node,
                           "Inconsistent types in expression +|-",
                           Loc => Loc (E));
                        Success := False;
                        return;
                     else
                        if T1 = T2 and then T1 = RT_Integer then
                           Set_Returned_Type (E, RT_Integer);
                        elsif T1 = RT_Float or else T2 = RT_Float then
                           Set_Returned_Type (E, RT_Float);
                        else
                           Set_Returned_Type (E, T1);
                        end if;
                     end if;

                  when OV_Modulo =>
                     if not Part_Unknown
                       and then (T2 /= RT_Integer or else T1 /= RT_Integer)
                     then
                        Display_Analyzer_Error
                          (No_Node,
                           "Inconsistent types for operator ",
                           Loc => Loc (E));
                        Success := False;
                        return;
                     else
                        Set_Returned_Type (E, RT_Integer);
                     end if;

                  when OV_Minus | OV_Star | OV_Slash | OV_Power =>
                     if not Part_Unknown
                       and then
                       ((T2 /= RT_Float and then T2 /= RT_Integer)
                        or else (T1 /= RT_Float and then T1 /= RT_Integer))
                     then
                        Display_Analyzer_Error
                          (No_Node,
                           "Inconsistent types in expression +|-",
                           Loc => Loc (E));
                        Success := False;
                        return;
                     else
                        if T1 = T2 and then T1 = RT_Integer then
                           Set_Returned_Type (E, RT_Integer);
                        else
                           Set_Returned_Type (E, RT_Float);
                        end if;
                     end if;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected operator",
                        Loc => Loc (E));
                     Success := False;
                     return;
               end case;

            elsif Present (Right_Expr (E)) then
               Analyze_Verification_Expression (Right_Expr (E), Success);
               if not Success then
                  return;
               end if;
               T1 := Returned_Type (Right_Expr (E));

               case Operator (E) is

                  when OV_Not =>
                     if not Part_Unknown then
                        if T1 /= RT_Boolean then
                           Display_Analyzer_Error
                             (No_Node,
                              "Inconsistent type in expression, " &
                              "must be boolean",
                              Loc => Loc (E));
                           Success := False;
                           return;
                        else
                           Set_Returned_Type (E, RT_Boolean);
                        end if;
                     else
                        Set_Returned_Type (E, RT_Boolean);
                     end if;

                  when OV_Minus =>
                     if not Part_Unknown then
                        if T1 /= RT_Integer and then T1 /= RT_Float then
                           Display_Analyzer_Error
                             (No_Node,
                              "Inconsistent type in expression, " &
                              "must be numeric",
                              Loc => Loc (E));
                           Success := False;
                           return;
                        else
                           Set_Returned_Type (E, RT_Unknown);
                        end if;
                     else
                        Set_Returned_Type (E, RT_Float);
                     end if;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected operator",
                        Loc => Loc (E));
                     Success := False;
                     return;
               end case;
            end if;

         when others =>
            Display_Analyzer_Error
              (No_Node,
               "unexpected node kind",
               Loc => Loc (E));
            Success := False;
            return;
      end case;
   end Analyze_Verification_Expression;

   -----------------------------------
   -- Analyze_Check_Subprogram_Call --
   -----------------------------------

   procedure Analyze_Check_Subprogram_Call
     (S       :     Node_Id;
      Success : out Boolean)
   is
      pragma Assert (Kind (S) = K_Check_Subprogram_Call);

      N : Node_Id := First_Node (Parameters (S));
      M : Node_Id;
      P : Node_Id;
      D : Node_Id;
      T : Return_Type;

      ------------------------
      -- Find_Set_Reference --
      ------------------------

      procedure Find_Set_Reference
        (N       :     Node_Id;
         S       :     Node_Id;
         Is_Set  : out Boolean;
         Success : out Boolean)
      is
         M : Node_Id;
      begin
         Success := True;
         Is_Set  := True;
         case Kind (N) is

            when K_Set_Reference =>
               --  We register the predefined set

               declare
                  TV : Value_Id;
               begin
                  pragma Unreferenced (TV);
                  TV := Get_Set_Type (N);
                  Append_Node_To_List (N, Referenced_Sets (S));
               end;

            when K_Identifier =>
               D :=
                 Find_Node_By_Name (To_Lower (Name (N)), Used_Set (REAL_Root));
               if Present (D) then
                  --  Add to set reference list

                  M := Make_Set_Reference;
                  Set_Referenced_Set (M, D);
                  Append_Node_To_List (M, Referenced_Sets (S));
               else
                  --  Must be a reference on "range" variable
                  --  or a variable referencing an element of a
                  --  set.

                  if Get_Name_String
                      (Name
                         (Identifier
                            (Range_Variable
                               (Range_Declaration (REAL_Root))))) =
                    Get_Name_String (To_Lower (Name (N)))
                  then

                     M := Make_Set_Reference;
                     Set_Referenced_Set
                       (M,
                        Referenced_Set
                          (Set_Reference
                             (Range_Variable
                                (Range_Declaration (REAL_Root)))));
                     Append_Node_To_List (M, Referenced_Sets (S));
                     Is_Set := False;
                  else
                     --  Search related variable

                     declare
                        Found  : Boolean := False;
                        Ignore : Boolean := False;
                     begin
                        --  1/ In global and theorem lists

                        D := Find_Variable (To_Lower (Name (N)));
                        if Present (D) and then Var_Type (D) = RT_Element then
                           Is_Set := False;
                           M      := Make_Var_Reference (Name (N));
                           Set_Referenced_Var (M, D);
                           Append_Node_To_List (M, True_Parameters (S));
                           Found := True;
                        else
                           if Present (D)
                             and then Var_Type (D) = RT_Unknown
                           then
                              DW (Get_Name_String (Name (N)) &
                                 " variable cannot be typed.");
                              Ignore := True;
                           end if;
                        end if;

                        --  1/ In local list

                        if not Found and then not Ignore then
                           D :=
                             Find_Node_By_Name
                               (To_Lower (Name (N)),
                                Local_Var (REAL_Root));
                           if Present (D)
                             and then Var_Type (D) = RT_Element
                           then
                              --  Set the variable position

                              if Variable_Position (S) = Value_Id (0) then
                                 if N = First_Node (Parameters (S)) then
                                    Set_Variable_Position (S, Value_Id (1));
                                 else
                                    Set_Variable_Position (S, Value_Id (2));
                                 end if;
                              else
                                 Display_Analyzer_Error
                                   (No_Node,
                                    Get_Name_String (Name (N)) &
                                    " variable is already referenced",
                                    Loc => Loc (N));
                                 Success := False;
                                 return;
                              end if;

                              Is_Set := False;
                              M      := Make_Var_Reference (Name (N));
                              Set_Referenced_Var (M, D);
                              Append_Node_To_List (M, True_Parameters (S));
                              Found := True;
                           else
                              if Present (D)
                                and then Var_Type (D) = RT_Unknown
                              then
                                 DW (Get_Name_String (Name (N)) &
                                    " variable cannot be typed.");
                                 Ignore := True;
                              end if;
                           end if;
                        end if;

                        if not Found then
                           if not Ignore then
                              Display_Analyzer_Error
                                (No_Node,
                                 Get_Name_String (Name (N)) &
                                 " variable is not a " &
                                 "declared set or variable",
                                 Loc => Loc (N));
                              Success := False;
                              return;
                           else
                              Is_Set := False;
                              M      := Make_Var_Reference (Name (N));
                              Set_Referenced_Var (M, D);
                              Append_Node_To_List (M, True_Parameters (S));
                           end if;
                        end if;
                     end;
                  end if;
               end if;

            when others =>
               Display_Analyzer_Error
                 (No_Node,
                  "unexpected node kind as a parameter",
                  Loc => Loc (N));
               Success := False;
               return;
         end case;
      end Find_Set_Reference;

      Var : Node_Id;
   begin
      Success := True;
      Set_Referenced_Sets (S, New_List (K_List_Id, Loc (S)));
      Set_True_Parameters (S, New_List (K_List_Id, Loc (S)));

      --  There are three kinds of verification subprogram :

      --  Set-based subprogram, which always takes a  set or
      --  element as first parameter, and can have others
      --  parameters (usually literal). They always returns
      --  a value or a list of values. eg : Cardinal,
      --  Get_Property_Value, Property_Exists.

      --  Value manipulation subprograms (eg. Max, Sum, First,
      --  Last...), where the single parameter is a value or a
      --  list of values, and which returns another value.

      --  The Expr subprogram (iterative expression) compute an
      --  expression on all the elements of the parameter-passed
      --  set, and returns a list of values.

      case Code (S) is

         when FC_Expr =>
            if Present (N)
              and then Present (Next_Node (N))
              and then Present (Next_Node (Next_Node (N)))
            then

               --  The first parameter must be a set

               case Kind (N) is
                  when K_Set_Reference =>

                     --  We register the predefined set

                     declare
                        TV : Value_Id;
                     begin
                        pragma Unreferenced (TV);
                        TV := Get_Set_Type (N);
                        Append_Node_To_List (N, Referenced_Sets (S));
                     end;

                  when K_Identifier =>
                     D :=
                       Find_Node_By_Name
                         (To_Lower (Name (N)),
                          Used_Set (REAL_Root));
                     if Present (D) then

                        --  Add to set reference list

                        M := Make_Set_Reference;
                        Set_Referenced_Set (M, D);
                        Append_Node_To_List (M, Referenced_Sets (S));
                     else
                        Display_Analyzer_Error
                          (No_Node,
                           Get_Name_String (Name (N)) &
                           " is not a declared set or variable",
                           Loc => Loc (N));
                        Success := False;
                        return;
                     end if;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;

               --  The second parameter must be a new variable

               N := Next_Node (N);
               case Kind (N) is
                  when K_Identifier =>
                     --  Check weither the variable is already used

                     D := Find_Variable (To_Lower (Name (N)));
                     if No (D) then

                        --  Declare the new variable

                        P := Make_Variable (Name (N));
                        Set_Var_Type (P, RT_Element);
                        Append_Node_To_List (P, Used_Var (REAL_Root));

                        --  Bind the new variable to the expression

                        M := Make_Var_Reference (Name (N));
                        Set_Referenced_Var (M, P);
                        Append_Node_To_List (M, True_Parameters (S));
                     else
                        Display_Analyzer_Error
                          (No_Node,
                           Get_Name_String (Name (Identifier (N))) &
                           " is already declared",
                           Loc => Loc (N));
                        Success := False;
                        return;
                     end if;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;

               --  The last parameter must be an expression

               N := Next_Node (N);
               case Kind (N) is

                  when K_Check_Subprogram_Call |
                    K_Check_Expression         |
                    K_Ternary_Expression       =>
                     Analyze_Verification_Expression (N, Success);
                     if Success then
                        case Returned_Type (N) is
                           when RT_Float =>
                              Set_Returned_Type (S, RT_Float_List);
                           when RT_Integer =>
                              Set_Returned_Type (S, RT_Int_List);
                           when RT_String =>
                              Set_Returned_Type (S, RT_String_List);
                           when RT_Boolean =>
                              Set_Returned_Type (S, RT_Bool_List);
                           when RT_Range =>
                              Set_Returned_Type (S, RT_Range_List);
                           when others =>
                              Display_Analyzer_Error
                                (No_Node,
                                 "could not resolve expression type",
                                 Loc => Loc (N));
                              Success := False;
                              return;
                        end case;
                        Remove_Node_From_List (N, Parameters (S));
                        Append_Node_To_List (N, True_Parameters (S));
                     else
                        return;
                     end if;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected 3 parameters",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

         when FC_Get_Property_Value =>
            if Present (N) and then Present (Next_Node (N)) then
               declare
                  Is_Set : Boolean;
               begin
                  --  Search related set

                  if Kind (N) = K_Identifier then
                     Find_Set_Reference (N, S, Is_Set, Success);
                  else
                     Success := False;
                  end if;

                  if not Success then
                     Display_Analyzer_Error
                       (No_Node,
                        "first parameter must be a declared set",
                        Loc => Loc (N));
                     return;
                  end if;

                  N := Next_Node (N);
                  if Kind (N) = K_Literal then
                     Get_Property_Type (N, Success, T);
                     if not Success then
                        Display_Analyzer_Error
                          (No_Node,
                           "could not analyze second parameter type",
                           Loc => Loc (N));
                        return;
                     end if;
                     Set_Returned_Type (S, T);

                     P := New_Node (K_Literal, Loc (N));
                     Set_Value (P, Value (N));
                     Append_Node_To_List (P, True_Parameters (S));
                  else
                     Display_Analyzer_Error
                       (No_Node,
                        "second parameter must be a literal",
                        Loc => Loc (N));
                     Success := False;
                     return;
                  end if;

                  if Is_Set then
                     case Value_Id (T) is
                        when RT_Float =>
                           Set_Returned_Type (S, RT_Float_List);
                        when RT_Integer =>
                           Set_Returned_Type (S, RT_Int_List);
                        when RT_String =>
                           Set_Returned_Type (S, RT_String_List);
                        when RT_Boolean =>
                           Set_Returned_Type (S, RT_Bool_List);
                        when RT_Range =>
                           Set_Returned_Type (S, RT_Range_List);
                        when RT_String_List =>
                           Set_Returned_Type (S, RT_String_List);
                        when RT_Float_List =>
                           Set_Returned_Type (S, RT_Float_List);
                        when RT_Int_List =>
                           Set_Returned_Type (S, RT_Int_List);
                        when RT_Bool_List =>
                           Set_Returned_Type (S, RT_Bool_List);
                        when RT_Range_List =>
                           Set_Returned_Type (S, RT_Range_List);
                        when RT_Element_List =>
                           Set_Returned_Type (S, RT_Element_List);
                        when others =>
                           Display_Analyzer_Error
                             (No_Node,
                              "Could not resolve list type " &
                              Value_Id (T)'Img,
                              Loc => Loc (N));
                           Success := False;
                           return;
                     end case;
                  else
                     Set_Returned_Type (S, Value_Id (T));
                  end if;
               end;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

         when FC_Property_Exists =>
            if Present (N) and then Present (Next_Node (N)) then
               declare
                  Is_Set : Boolean;
               begin

                  Find_Set_Reference (N, S, Is_Set, Success);
                  if not Success then
                     Display_Analyzer_Error
                       (No_Node,
                        "Could not find back set related to " &
                        Get_Name_String (Name (N)),
                        Loc => Loc (N));
                     Success := False;
                     return;
                  end if;

                  N := Next_Node (N);
                  case Kind (N) is

                     when K_Literal =>
                        P := New_Node (K_Literal, Loc (N));
                        Set_Value (P, Value (N));
                        Append_Node_To_List (P, True_Parameters (S));

                     when others =>
                        Display_Analyzer_Error
                          (No_Node,
                           "unexpected node kind as a parameter",
                           Loc => Loc (N));
                        Success := False;
                        return;
                  end case;
                  Set_Returned_Type (S, RT_Boolean);
               end;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (N));
               Success := False;
               return;
            end if;

         when FC_First | FC_Last =>
            if Present (N) then
               case Kind (N) is
                  when K_Check_Subprogram_Call =>
                     Analyze_Check_Subprogram_Call (N, Success);
                     if not Success then
                        return;
                     end if;

                     case Returned_Type (N) is
                        when RT_Range =>
                           Set_Returned_Type (S, RT_Float);

                        when RT_Range_List =>
                           Set_Returned_Type (S, RT_Float_List);

                        when others =>
                           Display_Analyzer_Error
                             (No_Node,
                              "expected a range value for " &
                              "expression return",
                              Loc => Loc (N));
                           Success := False;
                           return;
                     end case;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

         when FC_Size =>
            if Present (N) then
               case Kind (N) is

                  when K_Check_Expression   |
                    K_Check_Subprogram_Call |
                    K_Ternary_Expression    =>
                     Analyze_Verification_Expression (N, Success);

                  when K_Identifier =>
                     Var := Find_Variable (Name (N));
                     if No (Var) then
                        Display_Analyzer_Error
                          (No_Node,
                           "Could not find variable " &
                           Get_Name_String (Name (N)),
                           Loc => Loc (N));
                        return;
                     end if;
                     P := Make_Var_Reference (Name (N));
                     Set_Referenced_Var (P, Var);
                     Replace_Node_To_List (Parameters (S), N, P);
                     N := P;
                     Analyze_Verification_Expression (N, Success);

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

            if not Success then
               return;
            end if;
            case Returned_Type (N) is
               when RT_Float_List |
                 RT_Int_List      |
                 RT_String_List   |
                 RT_Bool_List     |
                 RT_Range_List    |
                 RT_Element_List  =>
                  Set_Returned_Type (S, RT_Integer);

               when others =>
                  Display_Analyzer_Error
                    (No_Node,
                     "expected a list value for " & "expression return",
                     Loc => Loc (N));
                  Success := False;
                  return;
            end case;

         when FC_Queue =>
            if Present (N) then
               case Kind (N) is

                  when K_Check_Expression   |
                    K_Check_Subprogram_Call |
                    K_Ternary_Expression    =>
                     Analyze_Verification_Expression (N, Success);

                  when K_Identifier =>
                     Var := Find_Variable (Name (N));
                     if No (Var) then
                        Display_Analyzer_Error
                          (No_Node,
                           "Could not find variable " &
                           Get_Name_String (Name (N)),
                           Loc => Loc (N));
                        return;
                     end if;
                     P := Make_Var_Reference (Name (N));
                     Set_Referenced_Var (P, Var);
                     Replace_Node_To_List (Parameters (S), N, P);
                     N := P;
                     Analyze_Verification_Expression (N, Success);

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

            if not Success then
               return;
            end if;
            case Returned_Type (N) is
               when RT_Float_List |
                 RT_Int_List      |
                 RT_String_List   |
                 RT_Bool_List     |
                 RT_Range_List    |
                 RT_Element_List  =>
                  Set_Returned_Type (S, Returned_Type (N));

               when others =>
                  Display_Analyzer_Error
                    (No_Node,
                     "expected a list value for " & "expression return",
                     Loc => Loc (N));
                  Success := False;
                  return;
            end case;

         when FC_Head =>
            if Present (N) then
               case Kind (N) is

                  when K_Check_Expression   |
                    K_Check_Subprogram_Call |
                    K_Ternary_Expression    =>
                     Analyze_Verification_Expression (N, Success);

                  when K_Identifier =>
                     Var := Find_Variable (Name (N));
                     if No (Var) then
                        Display_Analyzer_Error
                          (No_Node,
                           "Could not find variable " &
                           Get_Name_String (Name (N)),
                           Loc => Loc (N));
                        return;
                     end if;
                     P := Make_Var_Reference (Name (N));
                     Set_Referenced_Var (P, Var);
                     Replace_Node_To_List (Parameters (S), N, P);
                     N := P;
                     Analyze_Verification_Expression (N, Success);

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

            if not Success then
               return;
            end if;
            case Returned_Type (N) is

               when RT_Float_List =>
                  Set_Returned_Type (S, RT_Float);

               when RT_Int_List =>
                  Set_Returned_Type (S, RT_Integer);

               when RT_String_List =>
                  Set_Returned_Type (S, RT_String);

               when RT_Bool_List =>
                  Set_Returned_Type (S, RT_Boolean);

               when RT_Range_List =>
                  Set_Returned_Type (S, RT_Range);

               when RT_Element_List =>
                  Set_Returned_Type (S, RT_Element);

               when others =>
                  Display_Analyzer_Error
                    (No_Node,
                     "expected a list value for " & "expression return",
                     Loc => Loc (N));
                  Success := False;
                  return;
            end case;

         when FC_Float | FC_Int =>
            if Code (S) = FC_Int then
               Set_Returned_Type (S, RT_Integer);
            else
               Set_Returned_Type (S, RT_Float);
            end if;
            if Present (N) then
               case Kind (N) is

                  when K_Check_Subprogram_Call |
                    K_Check_Expression         |
                    K_Ternary_Expression       =>
                     Analyze_Verification_Expression (N, Success);
                     if Success then
                        if Returned_Type (N) /= RT_Integer
                          and then Returned_Type (N) /= RT_Float
                        then
                           Display_Analyzer_Error
                             (No_Node,
                              "expected a numeric return for expression",
                              Loc => Loc (N));
                           Success := False;
                           return;
                        end if;
                     else
                        return;
                     end if;

                  when K_Identifier =>
                     D := Find_Variable (Name (N));
                     if No (D) then
                        Display_Analyzer_Error
                          (No_Node,
                           "Could not find variable " &
                           Get_Name_String (Name (N)),
                           Loc => Loc (N));
                        Success := False;
                        return;
                     end if;
                     P := Make_Var_Reference (Name (N));
                     Set_Referenced_Var (P, D);
                     Replace_Node_To_List (Parameters (S), N, P);
                     Analyze_Verification_Expression (P, Success);
                     if not Success
                       or else
                       (Returned_Type (P) /= RT_Float
                        and then Returned_Type (P) /= RT_Integer)
                     then
                        Success := False;
                        Display_Analyzer_Error (S, "expected a numeric value");
                        return;
                     end if;

                  when K_Literal =>
                     declare
                        use Ocarina.REAL_Values;

                        V : constant Value_Type := Get_Value_Type (Value (N));
                     begin
                        if V.T /= LT_Real and then V.T /= LT_Integer then
                           Display_Analyzer_Error
                             (No_Node,
                              "expected a numeric value");
                           Success := False;
                           return;
                        end if;
                     end;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "expected a numeric value",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

         when FC_List =>
            --  Turns a set into a list of elements or
            --  create a list from a collection of elements or
            --  create a list from a collection of values

            declare
               use Ocarina.REAL_Values;

               Is_List_Decl : Boolean  := False;
               V            : Value_Type;
               Last_Found   : Value_Id := Value_Id (RT_Unknown);
               Inconsistant : Boolean  := False;
               M, D, P      : Node_Id;
            begin
               while Present (N) loop
                  case Kind (N) is
                     when K_Set_Reference =>
                        if Is_List_Decl then
                           Display_Analyzer_Error
                             (No_Node,
                              "Could not put a set reference into " &
                              "a list of elements",
                              Loc => Loc (N));
                           return;
                        end if;
                        declare
                           TV : Value_Id;
                        begin
                           pragma Unreferenced (TV);
                           TV := Get_Set_Type (N);
                           Append_Node_To_List (N, Referenced_Sets (S));
                        end;

                     when K_Identifier =>
                        D :=
                          Find_Node_By_Name
                            (To_Lower (Name (N)),
                             Used_Set (REAL_Root));
                        if Present (D) then

                           --  Add to set reference list

                           M := Make_Set_Reference;
                           Set_Referenced_Set (M, D);
                           Append_Node_To_List (M, Referenced_Sets (S));
                        else
                           D := Find_Variable (Name (N));
                           if Present (D) then
                              Is_List_Decl := True;
                              P            := Make_Var_Reference (Name (N));
                              Set_Referenced_Var (P, D);
                              Replace_Node_To_List (Parameters (S), N, P);
                              Analyze_Verification_Expression (P, Success);
                              if Success then
                                 case Returned_Type (P) is
                                    when RT_Float =>
                                       if Last_Found = RT_Unknown then
                                          Last_Found := RT_Float_List;
                                       else
                                          Inconsistant :=
                                            (Last_Found /=
                                             Value_Id (RT_Float_List));
                                       end if;
                                    when RT_Integer =>
                                       if Last_Found = RT_Unknown then
                                          Last_Found := RT_Int_List;
                                       else
                                          Inconsistant :=
                                            (Last_Found /=
                                             Value_Id (RT_Int_List));
                                       end if;
                                    when RT_String =>
                                       if Last_Found = RT_Unknown then
                                          Last_Found := RT_String_List;
                                       else
                                          Inconsistant :=
                                            (Last_Found /=
                                             Value_Id (RT_String_List));
                                       end if;
                                    when RT_Boolean =>
                                       if Last_Found = RT_Unknown then
                                          Last_Found := RT_Bool_List;
                                       else
                                          Inconsistant :=
                                            (Last_Found /=
                                             Value_Id (RT_Bool_List));
                                       end if;
                                    when RT_Range =>
                                       if Last_Found = RT_Unknown then
                                          Last_Found := RT_Range_List;
                                       else
                                          Inconsistant :=
                                            (Last_Found /=
                                             Value_Id (RT_Range_List));
                                       end if;
                                    when others =>
                                       Display_Analyzer_Error
                                         (No_Node,
                                          "Could not resolve list type",
                                          Loc => Loc (N));
                                       Success := False;
                                       return;
                                 end case;
                              end if;
                           else
                              Display_Analyzer_Error
                                (No_Node,
                                 "Could not find set or variable " &
                                 Get_Name_String (Name (N)),
                                 Loc => Loc (N));
                              return;
                           end if;
                        end if;

                     when K_Literal =>
                        Is_List_Decl := True;
                        V            := Get_Value_Type (Value (N));
                        case V.T is
                           when LT_Real =>
                              if Last_Found = RT_Unknown then
                                 Last_Found := RT_Float_List;
                              else
                                 Inconsistant :=
                                   (Last_Found /= Value_Id (RT_Float_List));
                              end if;
                           when LT_Integer =>
                              if Last_Found = RT_Unknown then
                                 Last_Found := RT_Int_List;
                              else
                                 Inconsistant :=
                                   (Last_Found /= Value_Id (RT_Int_List));
                              end if;
                           when LT_String =>
                              if Last_Found = RT_Unknown then
                                 Last_Found := RT_String_List;
                              else
                                 Inconsistant :=
                                   (Last_Found /= Value_Id (RT_String_List));
                              end if;

                           when LT_Boolean =>
                              if Last_Found = RT_Unknown then
                                 Last_Found := RT_Bool_List;
                              else
                                 Inconsistant :=
                                   (Last_Found /= Value_Id (RT_Bool_List));
                              end if;

                           when LT_Range =>
                              if Last_Found = RT_Unknown then
                                 Last_Found := RT_Range_List;
                              else
                                 Inconsistant :=
                                   (Last_Found /= Value_Id (RT_Range_List));
                              end if;

                           when others =>
                              Display_Analyzer_Error
                                (No_Node,
                                 "Could not resolve list type ",
                                 Loc => Loc (N));
                              Success := False;
                              return;
                        end case;

                     when others =>
                        Display_Analyzer_Error
                          (No_Node,
                           "expected a set, a set identifier or a list " &
                           "of literal",
                           Loc => Loc (N));
                        Success := False;
                        return;
                  end case;

                  N := Next_Node (N);
               end loop;

               if Inconsistant then
                  Display_Analyzer_Error
                    (No_Node,
                     "Inconsistant elements type " & "in list declaration",
                     Loc => Loc (S));
                  Success := False;
                  return;
               end if;

               if Is_List_Decl then
                  Set_Returned_Type (S, Last_Found);
               else
                  Set_Returned_Type (S, Value_Id (RT_Element_List));
               end if;
            end;

         when FC_GCD | FC_LCM =>
            --  Takes as parameters :
            --  a couple of integer
            --  or a list of integer

            Set_Returned_Type (S, Value_Id (RT_Integer));

            declare
               Is_List : Boolean := False;
               Iter    : Int     := 0;
               P, Var  : Node_Id;
            begin
               Success := True;
               while Present (N) and then Success loop
                  case Kind (N) is
                     when K_Check_Subprogram_Call |
                       K_Check_Expression         |
                       K_Ternary_Expression       |
                       K_Var_Reference            =>

                        Analyze_Verification_Expression (N, Success);
                        if not Success then
                           return;
                        end if;
                        if Returned_Type (N) = RT_Integer then
                           null;
                        elsif Returned_Type (N) = RT_Int_List then
                           if Iter = 0 then
                              Is_List := True;
                           else
                              Success := False;
                           end if;
                        else
                           Success := False;
                           return;
                        end if;

                     when K_Literal =>
                        declare
                           use Ocarina.REAL_Values;

                           V : constant Value_Type :=
                             Get_Value_Type (Value (N));
                        begin
                           Success := (V.T = LT_Integer);
                        end;

                     when K_Identifier =>
                        Var := Find_Variable (Name (N));
                        if No (Var) then
                           Display_Analyzer_Error
                             (No_Node,
                              "Could not find variable " &
                              Get_Name_String (Name (Identifier (N))),
                              Loc => Loc (N));
                           Success := False;
                           exit;
                        end if;
                        P := Make_Var_Reference (Name (N));
                        Set_Referenced_Var (P, Var);
                        Replace_Node_To_List (Parameters (S), N, P);
                        Analyze_Verification_Expression (P, Success);
                        if Success then
                           if Returned_Type (P) = RT_Integer then
                              Success := not Is_List;
                           elsif Returned_Type (P) = RT_Int_List then
                              Is_List := True;
                           else
                              Success := False;
                           end if;
                        else
                           return;
                        end if;

                     when others =>
                        Success := False;
                  end case;

                  Iter := Iter + 1;
                  N    := Next_Node (N);
               end loop;

               if not Success or else (Iter < 1 and then not Is_List) then
                  Display_Analyzer_Error
                    (No_Node,
                     "expected a list of integers or an integer list" &
                     " as parameters",
                     Loc => Loc (S));
                  return;
               end if;
            end;

         when FC_Non_Null =>
            --  Takes as parameters an integer

            Set_Returned_Type (S, Value_Id (RT_Integer));

            declare
               Iter   : Int := 0;
               P, Var : Node_Id;
            begin
               while Present (N) and then Success loop

                  case Kind (N) is

                     when K_Check_Subprogram_Call |
                       K_Check_Expression         |
                       K_Ternary_Expression       |
                       K_Var_Reference            =>
                        Analyze_Verification_Expression (N, Success);
                        Success :=
                          Success and then (Returned_Type (N) = RT_Integer);
                        if not Success then
                           return;
                        end if;

                     when K_Literal =>
                        declare
                           use Ocarina.REAL_Values;

                           V : constant Value_Type :=
                             Get_Value_Type (Value (N));
                        begin
                           Success := (V.T = LT_Integer);
                        end;

                     when K_Identifier =>
                        Var := Find_Variable (Name (N));
                        if No (Var) then
                           Display_Analyzer_Error
                             (No_Node,
                              "Could not find variable " &
                              Get_Name_String (Name (Identifier (N))),
                              Loc => Loc (N));
                           Success := False;
                           exit;
                        end if;
                        P := Make_Var_Reference (Name (N));
                        Set_Referenced_Var (P, Var);
                        Replace_Node_To_List (Parameters (S), N, P);
                        Analyze_Verification_Expression (P, Success);
                        Success :=
                          Success and then (Returned_Type (P) = RT_Integer);

                     when others =>
                        Success := False;
                  end case;

                  Iter := Iter + 1;
                  N    := Next_Node (N);
               end loop;

               if not Success or else Iter /= 1 then
                  Display_Analyzer_Error
                    (No_Node,
                     "expected an integer as parameter",
                     Loc => Loc (S));
                  return;
               end if;
            end;

         when FC_Is_In =>
            --  Expects parameters which can be either a list, a set
            --  or a single element
            declare
               Nb_Params : Natural := 0;
               P, Var    : Node_Id;
               Is_Set    : Boolean;
               Find_Set  : Boolean := False;
            begin
               Set_Returned_Type (S, RT_Boolean);
               while Present (N) loop
                  case Kind (N) is
                     when K_Identifier =>
                        Var := Find_Variable (Name (N));
                        if No (Var) then
                           Find_Set_Reference (N, S, Is_Set, Success);
                           if not Success then
                              Display_Analyzer_Error
                                (No_Node,
                                 "Could not find back set or variable named " &
                                 Get_Name_String (Name (N)),
                                 Loc => Loc (N));
                              Success := False;
                              return;
                           end if;
                           Find_Set := True;
                           N        := Next_Node (N);
                        else
                           if Find_Set then
                              Display_Analyzer_Error
                                (No_Node,
                                 "cannot call is_in on heterogenous types",
                                 Loc => Loc (N));
                           end if;
                           Set_Variable_Position (S, Value_Id (1 - Nb_Params));
                           P := Make_Var_Reference (Name (N));
                           Set_Referenced_Var (P, Var);
                           Remove_Node_From_List (N, Parameters (S));
                           Append_Node_To_List (P, True_Parameters (S));
                           N := Next_Node (N);
                        end if;

                     when K_Check_Subprogram_Call =>
                        if Find_Set then
                           Display_Analyzer_Error
                             (No_Node,
                              "cannot call is_in on heterogenous types",
                              Loc => Loc (N));
                        end if;
                        Analyze_Check_Subprogram_Call (N, Success);
                        if not Success then
                           return;
                        end if;

                        if Returned_Type (N) /= RT_Int_List
                          and then Returned_Type (N) /= RT_Float_List
                          and then Returned_Type (N) /= RT_String_List
                          and then Returned_Type (N) /= RT_Element_List
                          and then Returned_Type (N) /= RT_Bool_List
                          and then Nb_Params = 1
                        then
                           if Returned_Type (N) = RT_Unknown then
                              Success := True;
                              return;
                           end if;

                           Display_Analyzer_Error
                             (No_Node,
                              "expected a list return expression ",
                              Loc => Loc (N));
                           Success := False;
                           return;
                        end if;
                        P := Next_Node (N);
                        Remove_Node_From_List (N, Parameters (S));
                        Set_Next_Node (N, No_Node);
                        Append_Node_To_List (N, True_Parameters (S));
                        N := P;
                        Set_Variable_Position (S, Value_Id (1 - Nb_Params));

                     when K_Set_Reference =>
                        Find_Set_Reference (N, S, Is_Set, Success);
                        if not Success then
                           Display_Analyzer_Error
                             (No_Node,
                              "Could not find back set or variable named " &
                              Get_Name_String (Name (N)),
                              Loc => Loc (N));
                           Success := False;
                           return;
                        end if;
                        N := Next_Node (N);

                     when others =>
                        Display_Analyzer_Error
                          (No_Node,
                           "expected a list return expression",
                           Loc => Loc (N));
                        Success := False;
                        return;
                  end case;
                  Nb_Params := Nb_Params + 1;
               end loop;

               if Nb_Params /= 2 then
                  Display_Analyzer_Error
                    (No_Node,
                     "expected two parameters for 'is_in' function",
                     Loc => Loc (S));
                  Success := False;
                  return;
               end if;
            end;

         when FC_Max | FC_Min | FC_Sum | FC_Product =>
            Set_Returned_Type (S, Get_Returned_Type (Code (S)));
            declare
               Var : Node_Id;
            begin
               if Present (N) then
                  case Kind (N) is
                     when K_Check_Expression   |
                       K_Check_Subprogram_Call |
                       K_Ternary_Expression    =>
                        Analyze_Verification_Expression (N, Success);
                        if not Success then
                           return;
                        end if;

                     when K_Identifier =>
                        Var := Find_Variable (Name (N));
                        if No (Var) then
                           Display_Analyzer_Error
                             (No_Node,
                              "Could not find variable " &
                              Get_Name_String (Name (N)),
                              Loc => Loc (N));
                           return;
                        end if;
                        P := Make_Var_Reference (Name (N));
                        Set_Referenced_Var (P, Var);
                        Replace_Node_To_List (Parameters (S), N, P);
                        N := P;
                        Analyze_Verification_Expression (N, Success);

                     when others =>
                        Display_Analyzer_Error
                          (No_Node,
                           "expected a numeric list return expression",
                           Loc => Loc (N));
                        Success := False;
                        return;
                  end case;

                  if Returned_Type (N) /= RT_Int_List
                    and then Returned_Type (N) /= RT_Float_List
                  then
                     if Returned_Type (N) = RT_Unknown then
                        Success := True;
                        return;
                     end if;

                     Display_Analyzer_Error
                       (No_Node,
                        "expected a numeric list return expression",
                        Loc => Loc (N));
                     Success := False;
                     return;
                  end if;
               else
                  Display_Analyzer_Error
                    (No_Node,
                     "expected a parameter",
                     Loc => Loc (S));
                  Success := False;
                  return;
               end if;
            end;

         when FC_All_Equals =>
            Set_Returned_Type (S, RT_Boolean);
            if Present (N) then
               case Kind (N) is
                  when K_Check_Subprogram_Call =>
                     Analyze_Check_Subprogram_Call (N, Success);
                     if not Success then
                        return;
                     end if;
                     if Returned_Type (N) /= RT_Int_List
                       and then Returned_Type (N) /= RT_Float_List
                     then
                        if Returned_Type (N) = RT_Unknown then
                           Success := True;
                           return;
                        end if;

                        Display_Analyzer_Error
                          (No_Node,
                           "expected a numeric list return expression",
                           Loc => Loc (N));
                        Success := False;
                        return;
                     end if;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "expected a numeric list return expression",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

         when FC_Get_System_Property_Value =>
            if Present (N) then
               case Kind (N) is

                  when K_Literal =>
                     Get_Property_Type (N, Success, T);
                     if not Success then
                        Display_Analyzer_Error
                          (No_Node,
                           "could not resolve property type",
                           Loc => Loc (N));
                        Success := False;
                        return;
                     end if;
                     Set_Returned_Type (S, T);

                     P := New_Node (K_Literal, Loc (N));
                     Set_Value (P, Value (N));
                     Append_Node_To_List (P, True_Parameters (S));

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "expected a literal",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
               Set_Returned_Type (S, Value_Id (T));
            else
               Display_Analyzer_Error
                 (No_Node,
                  "expected a parameter",
                  Loc => Loc (S));
               Success := False;
               return;
            end if;

         when FC_Cos |
           FC_Sin    |
           FC_Tan    |
           FC_Cosh   |
           FC_Sinh   |
           FC_Tanh   |
           FC_Ln     |
           FC_Exp    |
           FC_Sqrt   |
           FC_Floor  |
           FC_Ceil   =>
            Set_Returned_Type (S, RT_Float);

            declare
               Is_List : Boolean := False;
               Iter    : Int     := 0;
               P, Var  : Node_Id;
            begin
               while Present (N) and then Success loop
                  case Kind (N) is
                     when K_Check_Subprogram_Call |
                       K_Check_Expression         |
                       K_Ternary_Expression       |
                       K_Var_Reference            =>

                        Analyze_Verification_Expression (N, Success);
                        if not Success then
                           return;
                        end if;
                        if Returned_Type (N) = RT_Float then
                           null;
                        elsif Returned_Type (N) = RT_Float_List then
                           if Iter = 0 then
                              Is_List := True;
                           else
                              Success := False;
                           end if;
                        else
                           Success := False;
                           return;
                        end if;

                     when K_Literal =>
                        declare
                           use Ocarina.REAL_Values;

                           V : constant Value_Type :=
                             Get_Value_Type (Value (N));
                        begin
                           Success := (V.T = LT_Real);
                        end;

                     when K_Identifier =>
                        Var := Find_Variable (Name (N));
                        if No (Var) then
                           Display_Analyzer_Error
                             (No_Node,
                              "Could not find variable " &
                              Get_Name_String (Name (Identifier (N))),
                              Loc => Loc (N));
                           Success := False;
                           exit;
                        end if;
                        P := Make_Var_Reference (Name (N));
                        Set_Referenced_Var (P, Var);
                        Replace_Node_To_List (Parameters (S), N, P);
                        Analyze_Verification_Expression (P, Success);
                        if Success then
                           if Returned_Type (P) = RT_Float then
                              Success := not Is_List;
                           elsif Returned_Type (P) = RT_Float_List then
                              Is_List := True;
                           else
                              Success := False;
                           end if;
                        else
                           return;
                        end if;

                     when others =>
                        Success := False;
                  end case;

                  Iter := Iter + 1;
                  N    := Next_Node (N);
               end loop;

               if not Success or else (Iter >= 1 and then Is_List) then
                  Display_Analyzer_Error
                    (No_Node,
                     "expected a float as parameter",
                     Loc => Loc (S));
                  return;
               end if;
            end;

         when others =>
            Set_Returned_Type (S, Get_Returned_Type (Code (S)));

            while Present (N) loop
               case Kind (N) is
                  when K_Set_Reference =>
                     --  We register the predefined set

                     declare
                        TV : Value_Id;
                     begin
                        pragma Unreferenced (TV);
                        TV := Get_Set_Type (N);
                        Append_Node_To_List (N, Referenced_Sets (S));
                     end;

                  when K_Identifier =>
                     declare
                        Is_Set : Boolean := True;
                     begin
                        Find_Set_Reference (N, S, Is_Set, Success);
                        if not Success then
                           Display_Analyzer_Error
                             (No_Node,
                              "could not find back set " &
                              Get_Name_String (Name (N)),
                              Loc => Loc (N));
                           return;
                        end if;
                     end;

                  when others =>
                     Display_Analyzer_Error
                       (No_Node,
                        "unexpected node kind as a parameter",
                        Loc => Loc (N));
                     Success := False;
                     return;
               end case;
               N := Next_Node (N);
            end loop;
      end case;
   end Analyze_Check_Subprogram_Call;

   -----------------------
   -- Get_Property_Type --
   -----------------------

   procedure Get_Property_Type
     (E       :     Node_Id;
      Success : out Boolean;
      Result  : out Return_Type)
   is
      use ATN;

      Packages : Node_Id := ATN.First_Node (ATN.Declarations (AADL_Tree));
      Fully_Qualified_Property_Name : Name_Id;
      Property_Name                 : Name_Id;
      List                          : Boolean;
      Property_Designator           : Node_Id;
   --  Stands for property type designator, actually the property
   --  type, yet does not say wheither it is a list or not.

   begin
      Success := True;
      Set_Str_To_Name_Buffer (Ocarina.REAL_Values.Image (RN.Value (E), False));
      Fully_Qualified_Property_Name := Name_Find;
      Property_Name := Remove_Prefix (Fully_Qualified_Property_Name);

      while Present (Packages) loop
         if ATN.Kind (Packages) = ATN.K_Property_Set then
            declare
               Decl : Node_Id := ATN.First_Node (ATN.Declarations (Packages));
               Good_Package : Boolean;
            begin
               while Present (Decl) loop
                  if ATN.Kind (Decl) =
                    ATN.K_Property_Definition_Declaration
                  then

                     declare
                        Package_S : constant String :=
                          Get_Name_String
                            (ATN.Name (ATN.Identifier (Packages)));
                        Package_Length : constant Natural := Package_S'Length;
                        FQN_S          : constant String  :=
                          Get_Name_String (Fully_Qualified_Property_Name);
                        Property_Name_Length : constant Natural :=
                          Get_Name_String (Property_Name)'Length;
                     begin
                        Good_Package :=
                          (Package_Length =
                           FQN_S'Length - Property_Name_Length - 2
                           and then FQN_S (1 .. Package_Length) = Package_S)
                          or else
                            Property_Name =
                            Fully_Qualified_Property_Name;
                     end;

                     if Property_Name = ATN.Name (ATN.Identifier (Decl))
                       and then Good_Package
                     then
                        Property_Designator :=
                          Expanded_Type_Designator (Property_Name_Type (Decl));
                        if No (Property_Designator) then
                           Property_Designator :=
                             Property_Type_Designator
                               (Property_Name_Type (Decl));
                        end if;
                        List := Is_List (Property_Name_Type (Decl));

                        case ATN.Kind (Property_Designator) is
                           when K_Integer_Type =>
                              if List then
                                 Result := RT_Int_List;
                              else
                                 Result := RT_Integer;
                              end if;
                              return;

                           when K_Boolean_Type =>
                              if List then
                                 Result := RT_Bool_List;
                              else
                                 Result := RT_Boolean;
                              end if;
                              return;

                           when K_String_Type =>
                              if List then
                                 Result := RT_String_List;
                              else
                                 Result := RT_String;
                              end if;
                              return;

                           when K_Real_Type =>
                              if List then
                                 Result := RT_Float_List;
                              else
                                 Result := RT_Float;
                              end if;
                              return;

                           when K_Enumeration_Type =>
                              if List then
                                 Result := RT_String_List;
                              else
                                 Result := RT_String;
                              end if;
                              return;

                           when K_Range_Type =>
                              if List then
                                 Result := RT_Range_List;
                              else
                                 Result := RT_Range;
                              end if;
                              return;

                           when K_Reference_Type =>
                              if List then
                                 Result := RT_Element_List;
                              else
                                 Result := RT_Element;
                              end if;
                              return;

                           when K_Unique_Property_Type_Identifier =>
                              case ATN.Kind
                                (Property_Type_Designator
                                   (Entity (Property_Designator)))
                              is

                                 when K_Integer_Type =>
                                    if List then
                                       Result := RT_Int_List;
                                    else
                                       Result := RT_Integer;
                                    end if;
                                    return;

                                 when K_String_Type =>
                                    if List then
                                       Result := RT_String_List;
                                    else
                                       Result := RT_String;
                                    end if;
                                    return;

                                 when K_Real_Type =>
                                    if List then
                                       Result := RT_Float_List;
                                    else
                                       Result := RT_Float;
                                    end if;
                                    return;

                                 when K_Enumeration_Type =>
                                    --  Treated as string
                                    if List then
                                       Result := RT_String_List;
                                    else
                                       Result := RT_String;
                                    end if;
                                    return;

                                 when K_Range_Type =>
                                    if List then
                                       Result := RT_Range_List;
                                    else
                                       Result := RT_Range;
                                    end if;
                                    return;

                                 when K_Reference_Type =>
                                    if List then
                                       Result := RT_Element_List;
                                    else
                                       Result := RT_Element;
                                    end if;
                                    return;

                                 when others =>
                                    Display_Analyzer_Error
                                      (No_Node,
                                       "could not resolve property type",
                                       Loc => ATN.Loc (Property_Designator));
                                    Success := False;
                                    return;
                              end case;

                           when others =>
                              Display_Analyzer_Error
                                (No_Node,
                                 "could not resolve property type",
                                 Loc => ATN.Loc (Property_Designator));
                              Success := False;
                              return;
                        end case;
                     end if;
                  end if;

                  Decl := ATN.Next_Node (Decl);
               end loop;
            end;
         end if;

         Packages := ATN.Next_Node (Packages);
      end loop;

      Result  := RT_Unknown;
      Success := True; -- Here, we return true to avoid false positive
      return;
   end Get_Property_Type;

   -------------------
   -- Find_Variable --
   -------------------

   function Find_Variable (E : Name_Id) return Node_Id is
      D : Node_Id := Find_Node_By_Name (To_Lower (E), Used_Var (REAL_Root));

   begin
      if No (D) then
         D := Find_Global_Variable (To_Lower (E));
      end if;
      return D;
   end Find_Variable;

end Ocarina.Analyzer.REAL;
