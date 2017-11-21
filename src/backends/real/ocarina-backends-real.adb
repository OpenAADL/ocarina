------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                O C A R I N A . B A C K E N D S . R E A L                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2016 ESA & ISAE.        --
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

with Ada.Numerics.Generic_Elementary_Functions;
with Ocarina.Namet;
with Ocarina.Output;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ocarina.Options; use Ocarina.Options;
with Outfiles; use Outfiles;

with Errors; use Errors;
with Locations; use Locations;

with Ocarina.Analyzer.REAL;
with Ocarina.ME_REAL.Tokens;  use Ocarina.ME_REAL.Tokens;
with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.REAL_Tree.Nutils;
with Ocarina.ME_REAL.REAL_Tree.Utils;
with Ocarina.REAL_Values;
with Ocarina.Backends.Messages;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

with Ocarina.Analyzer;
with Ocarina.Instances;
with Ocarina.Instances.REAL_Finder;
with Ocarina.Instances.REAL_Checker.Queries.Subcomponent_Predicates;
with Ocarina.Instances.REAL_Checker.Queries.Bound_Predicates;
with Ocarina.Instances.REAL_Checker.Queries.Connected_Predicates;
with Ocarina.Instances.REAL_Checker.Queries.Call_Predicates;
with Ocarina.Instances.REAL_Checker.Queries.Access_Predicates;
with Ocarina.Instances.REAL_Checker.Queries.Passing_Predicates;
with Ocarina.Instances.REAL_Checker.Queries.Predecessor_Predicates;
with Ocarina.Instances.REAL_Checker.Queries.Provided_Class_Predicates;

with Unchecked_Deallocation;

package body Ocarina.Backends.REAL is

   package Numerics is new Ada.Numerics.Generic_Elementary_Functions
     (Long_Long_Float);
   use Numerics;

   use Ocarina.Analyzer;
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Nutils;
   use Ocarina.ME_REAL.REAL_Tree.Utils;
   use Ocarina.REAL_Values;
   use Ocarina.Instances;
   use Ocarina.Instances.REAL_Finder;
   use Ocarina.Backends.Messages;
   use Ocarina.Namet;
   use Ocarina.Output;

   package RN renames Ocarina.ME_REAL.REAL_Tree.Nodes;
   package RNU renames Ocarina.ME_REAL.REAL_Tree.Nutils;
   --  package AIEP renames Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;

   --  This variable refers to the current element of the range set

   Current_Range_Variable : Node_Id;

   Root_System : Node_Id;

   --  Buffer for runtime instance

   type Runtime_Instance is record
      Set_Array              : Set_Table_Access;
      Current_Range_Variable : Node_Id;
      REAL_Root_Node         : Node_Id;
   end record;
   type Runtime_Instance_Access is access Runtime_Instance;

   procedure Initialize_Sets_Table (R : Node_Id);
   --  Initialization procedure

   procedure Build_All_Objects (R : Node_Id; Success : in out Boolean);
   --  Builds all sets (independants or not) and variables

   procedure Apply_To_All_Elements (R : Node_Id);
   --  For each element of the range set :
   --  1/ build all dependant sets
   --  2/ evaluate the check expression

   procedure Compute_Value
     (R       :     Node_Id;
      Success : out Boolean;
      Res     : out Float);
   --  We returns the value computed for the range set if
   --  it contains only one element, otherwise we compute the
   --  expected range function value.
   --  If there is a function value called on a single element,
   --  it is ignored.
   --  If they are multiples elements without a range fucntion call,
   --  we return an error value

   function Check_Requirements (R : Node_Id) return Boolean;
   --  Execute all requirements that are in "requires" field
   --  importing current environment (ie. Owner_Node variable)

   procedure Clean_Runtime;
   --  Clean array table

   function Save_Instance return Runtime_Instance_Access;
   --  Save the current runtime instance

   procedure Load_Instance (Instance : Runtime_Instance_Access);
   --  Load a previously-saved runtime instance

   --  Internal functions

   function Compute_Set_Expression (E : Node_Id) return Result_Set;

   procedure Compute_Check_Expression
     (E      :     Node_Id;
      Ret    : out Return_Type;
      Result : out Value_Id);

   procedure Compute_Check_Subprogram_Call
     (E      :     Node_Id;
      T      : out Return_Type;
      Result : out Value_Id);

   function Apply_To_All_Elements
     (R                     : Node_Id;
      Force_Result_To_False : Boolean := False) return Boolean;
   --  Test the theorem on all elements of the range set;
   --  return false if at least one of them fail

   procedure Manage_Set_Declaration (E : Node_Id);
   --  Compute the value (ie elements) of a set

   procedure Manage_Variable_Declaration
     (E       :        Node_Id;
      Success : in out Boolean);
   --  Compute the value of a variable with an expression

   procedure Manage_Variable_Theorem_Call
     (E       :        Node_Id;
      Success : in out Boolean);
   --  Compute the value of a variable with an extern theorem call

   function Build_Predefined_Set (T : Value_Id) return Result_Set;
   --  Build a predefined set according to the value given as parameter

   function Manage_Check_Expression (E : Node_Id) return Boolean;
   --  Evaluate the check expression for the current value of
   --  the range variable element

   function Manage_Return_Expression (E : Node_Id) return Float;
   --  Evaluate the return expression for the current value of
   --  the range variable element

   procedure Compute_Range_Function_Value
     (R       :     Node_Id;
      Success : out Boolean;
      Res     : out Float);
   --  Compute the value of a range (i.e. multi-element based) function;
   --  return an error if there is no element.

   procedure Load_Environment;
   --  Load the parameter-passed variables

   procedure Load_Environment is
      N, P : Node_Id;
      Cpt  : Natural := 0;
      M    : Name_Id;
      V    : Value_Id;
      RT   : Return_Type;

   begin
      if Environment /= No_List then
         N := First_Node (Environment);
         while Present (N) loop
            Set_Str_To_Name_Buffer ("argv_" & Image (Cpt));
            M := Name_Find;
            P := Find_Node_By_Name (M, Used_Var (RNU.REAL_Root));
            case Kind (N) is

               when K_Literal =>
                  V  := Value (N);
                  RT := Returned_Type (N);

               when K_Var_Reference =>
                  V  := Var_Value (Referenced_Var (N));
                  RT := Var_Type (Referenced_Var (N));

               when others =>
                  Display_Located_Error
                    (Loc (N),
                     "Could not analyze parameter " & Get_Name_String (M),
                     Fatal => True);
            end case;
            Set_Var_Type (P, RT);
            Set_Var_Value (P, V);
            Cpt := Cpt + 1;
            N   := Next_Node (N);
         end loop;
      end if;
   end Load_Environment;

   ---------------------------
   -- Initialize_Sets_Table --
   ---------------------------

   procedure Initialize_Sets_Table (R : Node_Id) is
      pragma Assert (Kind (R) = K_Theorem);

      Cpt : Natural := 0;
      N   : Node_Id := First_Node (Used_Set (R));
      A   : Node_Id;

   begin
      --  We compute the actual number of used sets, both implicit or
      --  explicit (complex set expressions)

      while Present (N) loop
         Cpt := Cpt + 1;

         --  Put annotations to find back the corresponding index in
         --  set table from the tree

         A := New_Node (K_Annotation, Loc (N));
         Set_Index (A, Value_Id (Cpt));
         Set_Annotation (N, A);

         N := Next_Node (N);
      end loop;

      --  We allocate the set table
      Set_Array := new Set_Table (1 .. Cpt);

      --  Build used predefined sets

      declare
         Res : Result_Set;
      begin

         N := First_Node (Used_Set (R));
         while Present (N) loop
            --  No expression can refer to non-previously declared
            --  sets, so there is no order issue

            if Predefined_Type (N) /= SV_No_Type then
               Res := Build_Predefined_Set (Predefined_Type (N));
               Set_Array (Integer (Index (Annotation (N)))) := Res;
            end if;

            N := Next_Node (N);
         end loop;
      end;

      if RNU.Environment /= No_List then
         Load_Environment;
      end if;
   end Initialize_Sets_Table;

   -------------------
   -- Clean_Runtime --
   -------------------

   procedure Clean_Runtime is
      procedure Free is new Unchecked_Deallocation
        (Set_Table,
         Set_Table_Access);
   begin
      Free (Set_Array);
   end Clean_Runtime;

   ---------------------------
   -- Apply_To_All_Elements --
   ---------------------------

   procedure Apply_To_All_Elements (R : Node_Id) is

      --  Actually, Dummy can be false since in some cases we want to
      --  handle an erroneous theorem more precisely than leaving an
      --  exception.
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Dummy := Apply_To_All_Elements (R);
   end Apply_To_All_Elements;

   function Apply_To_All_Elements
     (R                     : Node_Id;
      Force_Result_To_False : Boolean := False) return Boolean
   is
      use Ocarina.ME_AADL.AADL_Instances.Nutils;
      pragma Assert (Kind (R) = K_Theorem);

      Range_Set : constant Result_Set :=
        Set_Array
          (Integer
             (Index
                (Annotation
                   (Referenced_Set
                      (Set_Reference
                         (Range_Variable (Range_Declaration (R))))))));
      Success : Boolean := True;
   begin
      --  For each element of the global ("range") set, we build the
      --  dependant sets and then we check the verification expression

      Write_Line ("-------------------------------------");
      Write_Line
        ("Evaluating theorem " & Get_Name_String (Name (Identifier (R))));
      Write_Line ("");

      if Cardinal (Range_Set) = 0 then
         Write_Line ("Warning: evaluating theorem on an empty set");
      end if;

      for J in 1 .. Cardinal (Range_Set) loop
         Current_Range_Variable := Get (Range_Set, J);
         begin
            Write_Line
              (" * Iterate for variable: " &
               Get_Name_String
                 (Compute_Full_Name_Of_Instance (Current_Range_Variable)));
         exception
            when others =>
               null;
         end;
         Set_Var_Value
           (Referenced_Var (Variable_Ref (Range_Declaration (R))),
            New_Elem_Value (Current_Range_Variable));
         Build_All_Objects (R, Success);
         if Success then
            Success := Manage_Check_Expression (R);
         end if;
         Write_Line (" => Result: " & Success'Img);
         Write_Line ("");
         exit when not Success;
      end loop;

      if Force_Result_To_False then
         Success := False;
      end if;

      if not Success then
         N_Errors := N_Errors + 1;
      end if;

      Write_Line
        ("theorem " &
         Get_Name_String (Name (Identifier (R))) &
         " is: " &
         Boolean'Image (Success));
      Write_Line ("");
      return Success;
   end Apply_To_All_Elements;

   -------------------
   -- Compute_Value --
   -------------------

   procedure Compute_Value
     (R       :     Node_Id;
      Success : out Boolean;
      Res     : out Float)
   is
      pragma Assert (Kind (R) = K_Theorem);

      Range_Set : constant Result_Set :=
        Set_Array
          (Integer
             (Index
                (Annotation
                   (Referenced_Set
                      (Set_Reference
                         (Range_Variable (Range_Declaration (R))))))));
   begin
      Success := True;

      --  We returns the value computed for the range set if it
      --  contains only one element, otherwise we compute the expected
      --  range function value.
      --  If there is a function value called on a single element,
      --  it is ignored.
      --  If there are multiples elements without a range function call,
      --  we return an error value

      if Cardinal (Range_Set) = 0 then
         Display_Located_Error
           (Loc (R),
            "Empty range set, returned value is 0.0",
            Fatal   => False,
            Warning => True);
         Res := 0.0;

      elsif Return_Expression (R) /= No_Node
        and then Range_Function (Return_Expression (R)) /= No_Value
      then
         Compute_Range_Function_Value (R, Success, Res);

      elsif Cardinal (Range_Set) = 1 then
         Current_Range_Variable := Get (Range_Set, 1);
         Set_Var_Value
           (Referenced_Var (Variable_Ref (Range_Declaration (R))),
            New_Elem_Value (Current_Range_Variable));
         Build_All_Objects (R, Success);

         if Success then
            Res := Manage_Return_Expression (R);
         end if;

      else
         Display_Located_Error
           (Loc (R),
            "returns value cannot be valuated " &
            "for multi-element range sets",
            Fatal => True);
      end if;
   end Compute_Value;

   ----------------------------------
   -- Compute_Range_Function_Value --
   ----------------------------------

   procedure Compute_Range_Function_Value
     (R       :     Node_Id;
      Success : out Boolean;
      Res     : out Float)
   is
      pragma Assert (Kind (R) = K_Theorem);

      Range_Set : constant Result_Set :=
        Set_Array
          (Integer
             (Index
                (Annotation
                   (Referenced_Set
                      (Set_Reference
                         (Range_Variable (Range_Declaration (R))))))));
      Tmp   : Float;
      Buf   : Float;
      Found : Boolean := False;
   begin
      Success := True;

      case Range_Function (Return_Expression (R)) is
         when FC_MMax =>
            Buf := 0.0;

         when FC_MProduct =>
            Buf := 1.0;

         when FC_MSum =>
            Buf := 0.0;

         when FC_MMin =>
            Buf := 0.0;

         when FC_MAll_Equals =>
            null;

         when others =>
            Display_Located_Error
              (Loc (R),
               "expected range-level function",
               Fatal => True);
      end case;

      for I in 1 .. Cardinal (Range_Set) loop
         Current_Range_Variable := Get (Range_Set, I);

         Set_Var_Value
           (Referenced_Var (Variable_Ref (Range_Declaration (R))),
            New_Elem_Value (Current_Range_Variable));

         Build_All_Objects (R, Success);
         if not Success then
            return;
         end if;
         Tmp := Manage_Return_Expression (R);

         case Range_Function (Return_Expression (R)) is

            when FC_MMax =>
               if not Found then
                  Buf := Tmp;
               elsif Tmp > Buf then
                  Buf := Tmp;
               end if;

            when FC_MMin =>
               if not Found then
                  Buf := Tmp;
               elsif Tmp < Buf then
                  Buf := Tmp;
               end if;

            when FC_MAll_Equals =>
               if not Found then
                  Buf := Tmp;
                  Res := 1.0;
               elsif Tmp /= Buf then
                  Res := 0.0;
                  exit;
               end if;

            when FC_MProduct =>
               Buf := Buf * Tmp;

            when FC_MSum =>
               Buf := Buf + Tmp;

            when others =>
               Display_Located_Error
                 (Loc (R),
                  "expected range-level function",
                  Fatal => True);
         end case;

         Found := True;
      end loop;

      if Range_Function (Return_Expression (R)) /= FC_MAll_Equals then
         Success := Found;
         Res     := Buf;
      end if;
   end Compute_Range_Function_Value;

   ------------------------
   -- Check_Requirements --
   ------------------------

   function Check_Requirements (R : Node_Id) return Boolean is
      pragma Assert (Kind (R) = K_Theorem);

      Stored_Root   : constant Node_Id := R;
      N             : Node_Id;
      Success       : Boolean          := True;
      Local_Success : Boolean;
   begin
      N := First_Node (Required_Theorems (R));

      while Present (N) loop
         Local_Success := True;
         RNU.REAL_Root := Related_Theorem (N);

         Write_Line
           ("Processing requirement : " &
            Get_Name_String (Name (Identifier (RNU.REAL_Root))));

         --  Requirements can also have requirements

         if not Check_Requirements (RNU.REAL_Root) then
            Display_Located_Error
              (Loc (RNU.REAL_Root),
               "requirements of " &
               Get_Name_String (Name (Identifier (RNU.REAL_Root))) &
               " are not fulfilled",
               Fatal => not Ocarina.Analyzer.REAL.Continue_Evaluation);
            Local_Success := False;
         end if;

         --  Library theorems are already analyzed, so we proceed
         --  directly to execution

         Initialize_Sets_Table (RNU.REAL_Root);

         --  Note: if the requirements are not fulfilled, we flag the
         --  theorem as wrong in all cases.
         Success :=
           Apply_To_All_Elements (RNU.REAL_Root, not Local_Success)
           and then Success;

         Clean_Runtime;
         exit when (not Success)
           and then (not Ocarina.Analyzer.REAL.Continue_Evaluation);

         N := Next_Node (N);
      end loop;

      RNU.REAL_Root := Stored_Root;

      return Success;
   end Check_Requirements;

   -----------------------
   -- Build_All_Objects --
   -----------------------

   procedure Build_All_Objects (R : Node_Id; Success : in out Boolean) is
      pragma Assert (Kind (R) = K_Theorem);

      N   : Node_Id := First_Node (Used_Set (R));
      Res : Result_Set;

   begin
      Success := True;

      --  Build used predefined sets

      while Present (N) loop
         --  No expression can refer to non-previously declared sets,
         --  so there is no order issue

         if Predefined_Type (N) /= SV_No_Type then
            Res := Build_Predefined_Set (Predefined_Type (N));
            Set_Array (Integer (Index (Annotation (N)))) := Res;
         end if;

         N := Next_Node (N);
      end loop;

      --  Independant set declarations

      N := First_Node (Declarations (R));

      while Present (N) and then Success loop
         case Kind (N) is
            when K_Set_Declaration =>  --  Build a set
               Manage_Set_Declaration (N);
               if Returned_Type (Selection_Expression (N)) = RT_Error then
                  Success := False;
               end if;

            when K_Variable_Decl_Expression =>  --  Build a variable
               Manage_Variable_Declaration (N, Success);

            when K_Variable_Decl_Compute =>  --  Build a variable
               Manage_Variable_Theorem_Call (N, Success);

            when others =>
               --  Parsing error undetected !
               Display_Located_Error
                 (Loc (N),
                  "expected set or variable declaration",
                  Fatal => True);
         end case;

         N := Next_Node (N);
      end loop;
   end Build_All_Objects;

   ----------------------------------
   -- Manage_Variable_Theorem_Call --
   ----------------------------------

   procedure Manage_Variable_Theorem_Call
     (E       :        Node_Id;
      Success : in out Boolean)
   is
      pragma Assert (Kind (E) = K_Variable_Decl_Compute);

      procedure Export_Domain (E : Node_Id; Success : in out Boolean);

      -------------------
      -- Export_Domain --
      -------------------

      procedure Export_Domain (E : Node_Id; Success : in out Boolean) is
      begin
         --  1/ extract the domain

         case Kind (E) is

            when K_Set_Reference =>
               RNU.Domain :=
                 Set_Array (Integer (Index (Annotation (Referenced_Set (E)))));

            when K_Var_Reference =>
               declare
                  VT : constant Value_Type :=
                    Get_Value_Type (Var_Value (Referenced_Var (E)));
               begin
                  RNU.Domain := Empty_Set;
                  Add (RNU.Domain, VT.ELVal);
               end;

            when others =>
               Success := False;
         end case;
      end Export_Domain;

      Result               : Value_Id;
      R                    : Float;
      S                    : Runtime_Instance_Access;
      Ref                  : constant Node_Id := Referenced_Var (Var_Ref (E));
      Local_Domain         : constant Boolean    := RNU.Is_Domain;
      Local_Domain_value   : constant Result_Set := RNU.Domain;
      Requirements_Checked : Boolean;
   begin
      Write_Line
        ("   Evaluating " &
         Get_Name_String (Name (Identifier (Referenced_Var (Var_Ref (E))))));

      --  1/ Save the current runtime state

      S := Save_Instance;

      --  2/ Export the user-specified parameters to the environment

      if RN.Domain (E) /= No_Node then
         RNU.Is_Domain := True;
         Export_Domain (RN.Domain (E), Success);
         RNU.Environment := True_params (E);
         if not Success then
            Display_Located_Error
              (Loc (RNU.REAL_Root),
               "could not find " &
               Get_Name_String (Theorem_Name (E)) &
               " domain ",
               Fatal => True);
         end if;
      else
         RNU.Is_Domain := Local_Domain;
         RNU.Domain    := Local_Domain_value;
      end if;

      --  3/ Initialize the runtime space with new values
      --  Library theorems are already analyzed
      --  so we proceed directly to execution

      Clean_Runtime;
      REAL_Root := Related_Theorem (E);

      Requirements_Checked := Check_Requirements (RNU.REAL_Root);

      if not Requirements_Checked then
         Display_Located_Error
           (Loc (RNU.REAL_Root),
            "requirements are not fulfilled",
            Fatal => not Ocarina.Analyzer.REAL.Continue_Evaluation);
      end if;

      Initialize_Sets_Table (RNU.REAL_Root);

      --  4/ Launch the external theorem

      Compute_Value (RNU.REAL_Root, Success, R);
      if not Success then
         Display_Located_Error
           (Loc (RNU.REAL_Root),
            "Could not compute " &
            Get_Name_String (Theorem_Name (E)) &
            " value",
            Fatal => True);
      end if;

      Write_Line
        ("   value for " &
         Get_Name_String (Name (Identifier (Referenced_Var (Var_Ref (E))))) &
         " after evaluating " &
         Get_Name_String (Theorem_Name (E)) &
         " is" &
         Float'Image (R));

      --  5/ Restore current runtime state

      Clean_Runtime;
      Load_Instance (S);
      RNU.Environment := No_List;
      RNU.Is_Domain   := Local_Domain;
      RNU.Domain      := Local_Domain_value;
      Result          := New_Real_Value (Long_Long_Float (R));
      Set_Var_Type (Ref, RT_Float);
      Set_Var_Value (Ref, Result);
   end Manage_Variable_Theorem_Call;

   ---------------------------------
   -- Manage_Variable_Declaration --
   ---------------------------------

   procedure Manage_Variable_Declaration
     (E       :        Node_Id;
      Success : in out Boolean)
   is
      pragma Assert (Kind (E) = K_Variable_Decl_Expression);

      Result : Value_Id         := No_Value;
      Ret    : Return_Type      := RT_Unknown;
      D      : constant Node_Id := Check_Expression (Return_Expr (E));
   begin
      Compute_Check_Expression (D, Ret, Result);

      if Ret = RT_Error then
         Display_Located_Error
           (Loc (D),
            "Could not resolve expression value",
            Fatal => True);
         Success := False;
         return;
      end if;

      Write_Line
        ("     -> value for " &
         Get_Name_String (Name (Identifier (Referenced_Var (Var_Ref (E))))) &
         " is " &
         Image (Result));

      Set_Var_Value (Referenced_Var (Var_Ref (E)), Result);
   end Manage_Variable_Declaration;

   ----------------------------
   -- Manage_Set_Declaration --
   ----------------------------

   procedure Manage_Set_Declaration (E : Node_Id) is
      pragma Assert (Kind (E) = K_Set_Declaration);

      R  : Result_Set;
      R2 : Result_Set := Empty_Set;
      S  : Node_Id;
      G  : Node_Id;
   begin
      --  First we compute effective "local" (anonymous) set

      S := Referenced_Set (Local_Set (E));

      --  Compute expression actual value

      R := Compute_Set_Expression (Local_Set_Expression (E));

      --  Bind it in the set table

      Set_Array (Integer (Index (Annotation (S)))) := R;

      --  Find back global set declaration

      G := Referenced_Set (E);

      --  Append the local variable into the local stack

      RNU.Append_Node_To_List
        (Referenced_Var (Local_Variable (E)),
         Local_Var (RNU.REAL_Root));

      --  Iterate on the local set

      for J in 1 .. Cardinal (R) loop
         declare
            V      : constant Value_Id := New_Elem_Value (Get (R, J));
            Ret    : Return_Type       := RT_Error;
            Result : Value_Id          := No_Value;
         begin
            --  Set the local variable value to the current element of
            --  the local set

            Set_Var_Value (Referenced_Var (Local_Variable (E)), V);

            --  Compute selection expression result
            Compute_Check_Expression (Selection_Expression (E), Ret, Result);
            if Ret /= RT_Boolean then
               Display_Located_Error
                 (Loc (E),
                  "selection expression must return a boolean",
                  Fatal => True);
               Set_Returned_Type (Selection_Expression (E), Ret);
               return;
            end if;

            if Get_Value_Type (Result).BVal then
               Add (R2, Get (R, J));
            end if;
         end;
      end loop;

      --  Append the local variable into the local stack

      RNU.Remove_Node_From_List
        (Referenced_Var (Local_Variable (E)),
         Local_Var (RNU.REAL_Root));

      --  Bind it in the set table,

      Set_Array (Integer (Index (Annotation (G)))) := R2;

      Write_Line
        ("Content of set " &
         Get_Name_String (Name (Identifier (Parametrized_Expr (E)))) &
         " (" &
         Image (Loc (E)) &
         ") is ");
      Increment_Indentation;
      Display_Set (R2);
      Decrement_Indentation;
   end Manage_Set_Declaration;

   -----------------------------
   -- Manage_Check_Expression --
   -----------------------------

   function Manage_Check_Expression (E : Node_Id) return Boolean is
      pragma Assert (Kind (E) = K_Theorem);

      use Ocarina.ME_AADL.AADL_Instances.Nutils;

      D      : constant Node_Id := Check_Expression (E);
      Result : Value_Id         := No_Value;
      Ret    : Return_Type      := RT_Unknown;
   begin
      if No (D) then
         return True;
      end if;

      Compute_Check_Expression (D, Ret, Result);

      if Ret = RT_Error then
         return False;

      elsif Ret = RT_Boolean then
         if Get_Value_Type (Result).BVal = False then
            Display_Located_Error
              (Loc (D),
               "Property is false for instance " &
               Image (Current_Range_Variable) &
               " (" &
               Get_Name_String
                 (Compute_Full_Name_Of_Instance (Current_Range_Variable)) &
               ")",
               Fatal => False);
            return False;
         end if;
         return True;

      else
         Display_Located_Error
           (Loc (D),
            "Check expression must return a boolean",
            Fatal => True);
         return False;
      end if;
   end Manage_Check_Expression;

   ------------------------------
   -- Manage_Return_Expression --
   ------------------------------

   function Manage_Return_Expression (E : Node_Id) return Float is
      pragma Assert (Kind (E) = K_Theorem);

      D      : constant Node_Id := Check_Expression (Return_Expression (E));
      Result : Value_Id         := No_Value;
      Ret    : Return_Type      := RT_Unknown;
   begin
      Compute_Check_Expression (D, Ret, Result);

      case Ret is
         when RT_Float | RT_Integer =>
            if Get_Value_Type (Result).T = LT_Integer then
               if Get_Value_Type (Result).ISign then
                  return -(Float (Get_Value_Type (Result).IVal));
               else
                  return Float (Get_Value_Type (Result).IVal);
               end if;
            else
               if Get_Value_Type (Result).RSign then
                  return -(Float (Get_Value_Type (Result).RVal));
               else
                  return Float (Get_Value_Type (Result).RVal);
               end if;
            end if;

         when RT_Error =>
            Display_Located_Error (Loc (D), "theorem invalid", Fatal => True);
            return 0.0;

         when others =>
            Display_Located_Error
              (Loc (D),
               "return expression must return a numeric value",
               Fatal => True);
            return 0.0;
      end case;
   end Manage_Return_Expression;

   ------------------------------
   -- Compute_Check_Expression --
   ------------------------------

   procedure Compute_Check_Expression
     (E      :     Node_Id;
      Ret    : out Return_Type;
      Result : out Value_Id)
   is
      pragma Assert
        (Kind (E) = K_Check_Expression
         or else Kind (E) = K_Ternary_Expression
         or else Kind (E) = K_Literal
         or else Kind (E) = K_Var_Reference
         or else Kind (E) = K_Check_Subprogram_Call);

      T1, T2 : Return_Type := RT_Unknown;
      R1, R2 : Value_Id    := No_Value;
      V, V2  : Value_Type;

   begin
      case Kind (E) is
         when K_Identifier =>
            Write_Line (Get_Name_String (Name (E)));
            raise Program_Error;

         when K_Var_Reference =>
            Ret    := Var_Type (Referenced_Var (E));
            Result := Var_Value (Referenced_Var (E));

         when K_Check_Subprogram_Call =>
            Compute_Check_Subprogram_Call (E, Ret, Result);
            if Ret = RT_Error then
               return;
            end if;

         when K_Literal =>
            V := Get_Value_Type (Value (E));
            case V.T is
               when LT_Integer =>
                  Ret    := RT_Integer;
                  Result := New_Integer_Value (V.IVal);

               when LT_Real =>
                  Ret    := RT_Float;
                  Result := New_Real_Value (V.RVal);

               when LT_String =>
                  Ret    := RT_String;
                  Result := New_String_Value (V.SVal);

               when LT_Boolean =>
                  Ret    := RT_Boolean;
                  Result := New_Boolean_Value (V.BVal);

               when LT_Enumeration =>
                  Display_Located_Error
                    (Loc (E),
                     "could not compute expression value",
                     Fatal => True);

               when others =>
                  Display_Located_Error
                    (Loc (E),
                     "could not compute expression value",
                     Fatal => True);
            end case;

         when K_Ternary_Expression =>
            declare
               Cond  : constant Node_Id := Left_Expr (E);
               Expr1 : constant Node_Id := Right_Expr (E);
               Expr2 : constant Node_Id := Third_Expr (E);
            begin
               case Operator (E) is
                  when OV_If_Then_Else =>
                     --  Choose between expressions evaluation according
                     --  to the condition evaluation result
                     declare
                        R  : Value_Id;
                        T2 : Return_Type;

                     begin
                        Compute_Check_Expression (Cond, T2, R);
                        if T2 /= RT_Boolean then
                           Ret := RT_Error;
                           return;
                        end if;

                        if Get_Value_Type (R).BVal then
                           Compute_Check_Expression (Expr1, Ret, Result);
                        else
                           Compute_Check_Expression (Expr2, Ret, Result);
                        end if;
                     end;

                  when others =>
                     Display_Located_Error
                       (Loc (E),
                        "unknown operator found",
                        Fatal => True);
               end case;
            end;

         when K_Check_Expression =>
            if Present (Left_Expr (E)) and then Present (Right_Expr (E)) then
               Compute_Check_Expression (Left_Expr (E), T1, R1);
               if T1 = RT_Unknown then
                  Ret    := RT_Boolean;
                  Result := New_Boolean_Value (False);
                  return;
               elsif T1 = RT_Error then
                  Ret := RT_Error;
                  return;
               end if;

               case Operator (E) is
                  when OV_And =>
                     --  Cautious : Lazy evaluation !

                     Ret := RT_Boolean;
                     V   := Get_Value_Type (R1);
                     if V.BVal then
                        Compute_Check_Expression (Right_Expr (E), T2, R2);
                        if T2 = RT_Error then
                           Ret := RT_Error;
                           return;
                        end if;

                        V2     := Get_Value_Type (R2);
                        Result := New_Boolean_Value (V.BVal and then V2.BVal);
                     else
                        Result := New_Boolean_Value (V.BVal);
                     end if;

                  when OV_Or =>
                     --  Cautious : Lazy evaluation !
                     Ret := RT_Boolean;
                     V   := Get_Value_Type (R1);
                     if V.BVal = False then
                        Compute_Check_Expression (Right_Expr (E), T2, R2);
                        if T2 = RT_Error then
                           Ret := RT_Error;
                           return;
                        end if;

                        V2     := Get_Value_Type (R2);
                        Result := New_Boolean_Value (V.BVal or else V2.BVal);
                     else
                        Result := New_Boolean_Value (V.BVal);
                     end if;

                  when OV_Equal =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     Ret    := RT_Boolean;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     Result := New_Boolean_Value (V = V2);

                  when OV_Different =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     Ret    := RT_Boolean;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     Result := New_Boolean_Value (V /= V2);

                  when OV_Greater =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     Ret    := RT_Boolean;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     Result :=
                       New_Boolean_Value
                         ((not (V < V2)) and then not (V = V2));

                  when OV_Less =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     Ret    := RT_Boolean;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     Result := New_Boolean_Value (V < V2);

                  when OV_Less_Equal =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     Ret    := RT_Boolean;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     Result := New_Boolean_Value (V < V2 or else V = V2);

                  when OV_Greater_Equal =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Unknown then
                        Ret    := RT_Boolean;
                        Result := New_Boolean_Value (True);
                        return;
                     elsif T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     Ret    := RT_Boolean;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     Result := New_Boolean_Value (not (V < V2));

                  when OV_Plus =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     V      := V + V2;
                     Result := New_Value (V);
                     case V.T is
                        when LT_Integer =>
                           Ret := RT_Integer;

                        when LT_Real =>
                           Ret := RT_Float;

                        when LT_List =>
                           Ret := Returned_Type (E);

                        when others =>
                           Display_Located_Error
                             (Loc (E),
                              "expected numeric value",
                              Fatal => True);
                     end case;

                  when OV_Minus =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     declare
                        V3 : Value_Type;
                     begin
                        V      := Get_Value_Type (R1);
                        V2     := Get_Value_Type (R2);
                        V3     := V - V2;
                        Result := New_Value (V3);
                        case V.T is
                           when LT_Integer =>
                              Ret := RT_Integer;

                           when LT_Real =>
                              Ret := RT_Float;

                           when others =>
                              Display_Located_Error
                                (Loc (E),
                                 "expected numeric value",
                                 Fatal => True);
                        end case;
                     end;

                  when OV_Star =>
                     declare
                        V3 : Value_Type;
                     begin
                        Compute_Check_Expression (Right_Expr (E), T2, R2);
                        if T2 = RT_Error then
                           Ret := RT_Error;
                           return;
                        end if;
                        V      := Get_Value_Type (R1);
                        V2     := Get_Value_Type (R2);
                        V3     := V * V2;
                        Result := New_Value (V3);
                        case V3.T is
                           when LT_Integer =>
                              Ret := RT_Integer;

                           when LT_Real =>
                              Ret := RT_Float;

                           when others =>
                              Display_Located_Error
                                (Loc (E),
                                 "expected numeric value",
                                 Fatal => True);
                        end case;
                     end;

                  when OV_Modulo =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     V      := V mod V2;
                     Result := New_Value (V);
                     if V.T = LT_Integer then
                        Ret := RT_Integer;
                     else
                        Display_Located_Error
                          (Loc (E),
                           "expected integer value",
                           Fatal => True);
                     end if;

                  when OV_Slash =>
                     declare
                        V3 : Value_Type;
                     begin
                        Compute_Check_Expression (Right_Expr (E), T2, R2);
                        if T2 = RT_Error then
                           Ret := RT_Error;
                           return;
                        end if;
                        V      := Get_Value_Type (R1);
                        V2     := Get_Value_Type (R2);
                        V3     := V / V2;
                        Result := New_Value (V3);
                        case V.T is
                           when LT_Integer =>
                              Ret := RT_Integer;

                           when LT_Real =>
                              Ret := RT_Float;

                           when others =>
                              Display_Located_Error
                                (Loc (E),
                                 "expected numeric value",
                                 Fatal => True);
                        end case;
                     end;

                  when OV_Power =>
                     Compute_Check_Expression (Right_Expr (E), T2, R2);
                     if T2 = RT_Error then
                        Ret := RT_Error;
                        return;
                     end if;
                     V      := Get_Value_Type (R1);
                     V2     := Get_Value_Type (R2);
                     V      := Power (V, V2);
                     Result := New_Value (V);

                     case V.T is
                        when LT_Integer =>
                           Ret := RT_Integer;

                        when LT_Real =>
                           Ret := RT_Float;

                        when others =>
                           Display_Located_Error
                             (Loc (E),
                              "expected numeric value",
                              Fatal => True);
                     end case;

                  when others =>
                     Display_Located_Error
                       (Loc (E),
                        "unknown operator found",
                        Fatal => True);
               end case;

            elsif Present (Right_Expr (E)) then
               Compute_Check_Expression (Right_Expr (E), T1, R1);
               if T1 = RT_Error then
                  Ret := RT_Error;
                  return;
               end if;

               case Operator (E) is
                  when OV_Not =>
                     Ret    := RT_Boolean;
                     Result :=
                       New_Boolean_Value (not Get_Value_Type (R1).BVal);

                  when OV_Minus =>
                     V := Get_Value_Type (R1);
                     case V.T is
                        when LT_Integer =>
                           V.ISign := not V.ISign;
                           Result  := New_Value (V);
                           Ret     := RT_Integer;

                        when LT_Real =>
                           V.RSign := not V.RSign;
                           Result  := New_Value (V);
                           Ret     := RT_Float;

                        when others =>
                           Display_Located_Error
                             (Loc (E),
                              "expected numeric value",
                              Fatal => True);
                     end case;

                  when others =>
                     Display_Located_Error
                       (Loc (E),
                        "unknown operator found",
                        Fatal => True);
               end case;
            end if;

         when others =>
            Display_Located_Error
              (Loc (E),
               "unexpected node kind",
               Fatal => True);
      end case;
   end Compute_Check_Expression;

   ----------------------------
   -- Compute_Set_Expression --
   ----------------------------

   function Compute_Set_Expression (E : Node_Id) return Result_Set is
      pragma Assert
        (Kind (E) = K_Set_Expression or else Kind (E) = K_Set_Reference);

      R1 : Result_Set;
      R2 : Result_Set;
   begin
      if Kind (E) = K_Set_Reference then
         return Set_Array (Integer (Index (Annotation (Referenced_Set (E)))));

      else
         if Present (Left_Expr (E)) and then Present (Right_Expr (E)) then
            R1 := Compute_Set_Expression (Left_Expr (E));
            R2 := Compute_Set_Expression (Right_Expr (E));

            case Operator (E) is
               when OV_Plus =>
                  return Union (R1, R2, Distinct => True);

               when OV_Star =>
                  return Intersection (R1, R2);

               when OV_Minus =>
                  return Exclusion (R1, R2);

               when others =>
                  Display_Located_Error
                    (Loc (E),
                     "not a set operator",
                     Fatal => True);
                  return R1;
            end case;
         else
            Display_Located_Error
              (Loc (E),
               "missing an operand",
               Fatal => True);
            return R1;
         end if;
      end if;
   end Compute_Set_Expression;

   -----------------------------------
   -- Compute_Check_Subprogram_Call --
   -----------------------------------

   procedure Compute_Check_Subprogram_Call
     (E      :     Node_Id;
      T      : out Return_Type;
      Result : out Value_Id)
   is
      pragma Assert (Kind (E) = K_Check_Subprogram_Call);

      procedure Extract_Parameters_Sets
        (E       :     Node_Id;
         R1, R2  : out Result_Set;
         Success : out Boolean);

      -----------------------------
      -- Extract_Parameters_Sets --
      -----------------------------

      procedure Extract_Parameters_Sets
        (E       :     Node_Id;
         R1, R2  : out Result_Set;
         Success : out Boolean)
      is
         N         : Node_Id;
         Range_Set : constant Node_Id :=
           Referenced_Set
             (Set_Reference
                (Range_Variable (Range_Declaration (RNU.REAL_Root))));
         T1, T2 : Result_Set;
      begin
         Success := True;
         T1      := Empty_Set;
         T2      := Empty_Set;

         if not Is_Empty (True_Parameters (E)) then
            N := First_Node (True_Parameters (E));
         else
            Success := False;
            return;
         end if;

         if Kind (N) = K_Var_Reference then
            declare
               N2 : constant Node_Id    := First_Node (Referenced_Sets (E));
               VT : constant Value_Type :=
                 Get_Value_Type (Var_Value (Referenced_Var (N)));
            begin
               Add (T1, VT.ELVal);
               if Referenced_Set (N2) = Range_Set then
                  Add (T2, Current_Range_Variable);
               else
                  T2 :=
                    Set_Array
                      (Integer (Index (Annotation (Referenced_Set (N2)))));
               end if;
            end;
         else
            Display_Located_Error
              (Loc (E),
               "Expected a variable reference as first parameter",
               Fatal => True);
         end if;

         if Variable_Position (E) = Value_Id (1) then
            R1 := T1;
            R2 := T2;
         else
            R1 := T2;
            R2 := T1;
         end if;
      end Extract_Parameters_Sets;

      package OBCQ renames Ocarina.Instances.REAL_Checker.Queries;

      package Is_Bound renames OBCQ.Bound_Predicates.Bound_Query;
      package Is_Subcomponent renames
        OBCQ.Subcomponent_Predicates.Subcomponent_Query;
      package Is_Connected renames OBCQ.Connected_Predicates.Connected_Query;
      package Is_Called renames OBCQ.Call_Predicates.Call_Query;
      package Is_Accessed renames OBCQ.Access_Predicates.Access_Query;
      package Is_Passing renames OBCQ.Passing_Predicates.Passing_Query;
      package Is_Provided_Class renames
        OBCQ.Provided_Class_Predicates.Provided_Class_Query;
      package Is_Predecessor renames
        OBCQ.Predecessor_Predicates.Predecessor_Query;

      N       : Node_Id;
      R1, R2  : Result_Set;
      RS      : Result_Set;
      Success : Boolean;

   begin
      case Code (E) is
         when FC_Is_Called_By =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T      := RT_Error;
               Result := New_Boolean_Value (False);
               return;
            end if;
            RS := Is_Called.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Calling =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Called.Apply (R1, R2, Reversed => True);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Predecessor_Of =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Predecessor.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Passing_Through =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Passing.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Accessed_By =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Accessed.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Accessing_To =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Accessed.Apply (R1, R2, Reversed => True);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Connected_To =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Connected.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Connecting_To =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Connected.Apply (R1, R2, Reversed => True);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Bound_To =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Bound.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Subcomponent_Of =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Subcomponent.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Is_Provided_Class =>
            Extract_Parameters_Sets (E, R1, R2, Success);
            if not Success then
               T := RT_Error;
               return;
            end if;
            RS := Is_Provided_Class.Apply (R1, R2);
            T  := RT_Boolean;
            if Cardinal (RS) = 0 then
               Result := New_Boolean_Value (False);
            else
               Result := New_Boolean_Value (True);
            end if;

         when FC_Expr =>
            declare
               Var : constant Node_Id    := First_Node (True_Parameters (E));
               Set : constant Result_Set :=
                 Set_Array
                   (Integer
                      (Index
                         (Annotation
                            (Referenced_Set
                               (First_Node (Referenced_Sets (E)))))));
               L  : constant List_Id := New_List (K_List_Id, Loc (E));
               F  : Node_Id;
               T1 : Return_Type      := RT_Unknown;
               R  : Value_Id         := No_Value;
            begin
               for I in 1 .. Cardinal (Set) loop

                  --  Set the current value of the variable to the
                  --  value of the element

                  Set_Var_Value
                    (Referenced_Var (Var),
                     New_Elem_Value (Get (Set, I)));

                  --  Compute the iterative expression value for
                  --  the current variable value

                  Compute_Check_Expression (Next_Node (Var), T1, R);

                  if T1 = RT_Error then
                     Display_Located_Error
                       (Loc (Next_Node (Var)),
                        "could not compute expression value",
                        Fatal => True);
                  else
                     --  Add the computed value to the list of results

                     F := New_Node (K_Value_Node, Loc (E));
                     Set_Item_Val (F, R);
                     RNU.Append_Node_To_List (F, L);
                  end if;
               end loop;

               --  Since we leave the variable's scope,
               --  we delete it.

               RNU.Remove_Node_From_List
                 (Referenced_Var (Var),
                  Used_Var (RNU.REAL_Root));

               Result := New_List_Value (L);
               T      := Returned_Type (E);
            end;

         when FC_Get_System_Property_Value =>
            --  Takes 1 parameter :
            --  * a string literal (with property name)

            declare
               RS : constant Result_Set :=
                 Get_Instances_Of_Component_Type (C_System);
               Root_System : Node_Id;
            begin
               --  1/ Find the root system

               Root_System := Get (RS, 1);
               if Cardinal (RS) > 1 then
                  while AIN.Parent_Subcomponent (Root_System) /= No_Node loop
                     Root_System :=
                       AIN.Parent_Component
                         (AIN.Parent_Subcomponent (Root_System));
                  end loop;
               end if;

               --  2/ Search the property

               Result :=
                 Get_Property_Value_Function
                   (Value (First_Node (True_Parameters (E))),
                    Returned_Type (E),
                    Root_System);
               if Result = No_Value then
                  T := RT_Error;
                  Display_Located_Error
                    (Loc (First_Node (True_Parameters (E))),
                     "property " &
                     Image (Value (First_Node (True_Parameters (E)))) &
                     " is not defined on the root system.",
                     Fatal => True);
               else
                  T := Returned_Type (E);
               end if;
            end;

         when FC_Get_Property_Value =>
            --  Takes 2 parameters :
            --  * an element, a variable reference or a set name
            --  * a string literal (with property name)

            --  If the related set is the range set, then it
            --  actually refers to the range variable.

            --  If the first parameter is an element, then
            --  the property *must* be defined on it.

            N := First_Node (Referenced_Sets (E));
            if Present (N) then
               N := Referenced_Set (N);
            end if;

            if N =
              Referenced_Set
                (Set_Reference
                   (Range_Variable (Range_Declaration (RNU.REAL_Root))))
            then
               Result :=
                 Get_Property_Value_Function
                   (Value (First_Node (True_Parameters (E))),
                    Returned_Type (E),
                    Current_Range_Variable);

               if Result = No_Value then
                  T := RT_Error;
                  Display_Located_Error
                    (Loc (First_Node (True_Parameters (E))),
                     "property " &
                     Image (Value (First_Node (True_Parameters (E)))) &
                     " is not defined on element " &
                     Image (Current_Range_Variable) &
                     " (" &
                     Get_Name_String
                       (Ocarina.ME_AADL.AADL_Instances.Nutils
                          .Compute_Full_Name_Of_Instance
                          (Current_Range_Variable)) &
                     ") " &
                     Image (AIN.Loc (Current_Range_Variable)) &
                     Get_Name_String (Name (Identifier (N))),
                     Fatal   => False,
                     Warning => True);

                  Result := No_Value;
                  T      := RT_Unknown;
               else
                  T := Returned_Type (E);
               end if;
            else
               if Present (First_Node (Referenced_Sets (E))) then
                  --  The first parameter is a set
                  declare
                     L  : constant List_Id    := New_List (K_List_Id, Loc (E));
                     F  : Node_Id;
                     V  : Value_Id;
                     VT : Value_Type;
                     R1 : constant Result_Set :=
                       Set_Array
                         (Integer
                            (Index
                               (Annotation
                                  (Referenced_Set
                                     (First_Node (Referenced_Sets (E)))))));
                  begin
                     if Cardinal (R1) > 0 then
                        for J in 1 .. Cardinal (R1) loop
                           V :=
                             Get_Property_Value_Function
                               (Value (First_Node (True_Parameters (E))),
                                Returned_Type (E),
                                Get (R1, J));
                           if V /= No_Value then
                              VT := Get_Value_Type (V);
                              if VT.T /= LT_List then
                                 F := New_Node (K_Value_Node, Loc (E));
                                 Set_Item_Val (F, V);
                                 RNU.Append_Node_To_List (F, L);
                              else
                                 --  If the result is a list, we flatten
                                 --  the list of lists into a single list
                                 declare
                                    P : Node_Id := First_Node (VT.LVal);
                                 begin
                                    while Present (P) loop
                                       F := New_Node (K_Value_Node, Loc (E));
                                       Set_Item_Val (F, Item_Val (P));
                                       RNU.Append_Node_To_List (F, L);
                                       P := Next_Node (P);
                                    end loop;
                                 end;
                              end if;
                           end if;
                        end loop;
                        Result := New_List_Value (L);
                        T      := Returned_Type (E);
                     else
                        Display_Located_Error
                          (Loc (E),
                           "cardinal of set " &
                           Get_Name_String
                             (Name
                                (Identifier
                                   (Referenced_Set
                                      (First_Node (Referenced_Sets (E)))))) &
                           " is null",
                           Fatal   => False,
                           Warning => True);
                        Result := No_Value;
                        T      := RT_Unknown;
                     end if;
                  end;
               else
                  --  The first parameter is a variable
                  --  (hence it refers to an element)

                  declare
                     P  : constant Node_Id := First_Node (True_Parameters (E));
                     VT : constant Value_Type :=
                       Get_Value_Type (Var_Value (Referenced_Var (P)));
                  begin
                     Result :=
                       Get_Property_Value_Function
                         (Value (Next_Node (P)),
                          Returned_Type (E),
                          VT.ELVal);
                     if Result = No_Value then
                        T := RT_Error;
                        Display_Located_Error
                          (Loc (Next_Node (P)),
                           "property " &
                           Image (Value (Next_Node (P))) &
                           " is not defined on this single element. " &
                           "Try using 'Property_Exists' before.",
                           Fatal => True);
                     else
                        T := Returned_Type (E);
                     end if;
                  end;
               end if;
            end if;

         when FC_Property_Exists =>
            T := Returned_Type (E);
            N := First_Node (Referenced_Sets (E));
            if Present (N) then
               N := Referenced_Set (N);
            end if;
            if N =
              Referenced_Set
                (Set_Reference
                   (Range_Variable (Range_Declaration (RNU.REAL_Root))))
            then
               Result :=
                 Get_Property_Value_Function
                   (Value (First_Node (True_Parameters (E))),
                    Returned_Type (E),
                    Current_Range_Variable);
               if Result /= No_Value then
                  Result := New_Boolean_Value (True);
               else
                  Result := New_Boolean_Value (False);
               end if;

            else
               if Present (First_Node (Referenced_Sets (E))) then
                  --  The first parameter is a set

                  declare
                     Found : Boolean             := False;
                     V     : Value_Id;
                     R1    : constant Result_Set :=
                       Set_Array
                         (Integer
                            (Index
                               (Annotation
                                  (Referenced_Set
                                     (First_Node (Referenced_Sets (E)))))));
                  begin
                     for I in 1 .. Cardinal (R1) loop
                        V :=
                          Get_Property_Value_Function
                            (Value (First_Node (True_Parameters (E))),
                             Returned_Type (E),
                             Get (R1, I));
                        if V = No_Value then
                           Found := False;
                           exit;
                        else
                           Found := True;
                        end if;
                     end loop;

                     Result := New_Boolean_Value (Found);
                     T      := RT_Boolean;
                  end;
               else
                  --  The first parameter is a variable
                  --  (hence it refers to an element)

                  declare
                     P  : constant Node_Id := First_Node (True_Parameters (E));
                     VT : constant Value_Type :=
                       Get_Value_Type (Var_Value (Referenced_Var (P)));
                  begin
                     Result :=
                       Get_Property_Value_Function
                         (Value (Next_Node (P)),
                          Returned_Type (E),
                          VT.ELVal);
                     if Result = No_Value then
                        Result := New_Boolean_Value (False);
                     else
                        Result := New_Boolean_Value (True);
                     end if;
                  end;
               end if;
            end if;

         when FC_Cardinal =>
            --  Takes a set as parameter

            declare
               R : constant Result_Set :=
                 Set_Array
                   (Integer
                      (Index
                         (Annotation
                            (Referenced_Set
                               (First_Node (Referenced_Sets (E)))))));
            begin
               Result := New_Integer_Value (Unsigned_Long_Long (Cardinal (R)));
               T      := RT_Integer;
            end;

         when FC_First =>
            declare
               VT : Value_Type;
               N  : Node_Id;
               R  : Value_Id;
               T2 : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 /= RT_Error then
                  VT := Get_Value_Type (R);
               else
                  T := RT_Error;
                  return;
               end if;

               if T2 = RT_Range_List then
                  declare
                     L   : constant List_Id := New_List (K_List_Id, Loc (E));
                     F   : Node_Id;
                     VT2 : Value_Type;
                     V   : Value_Id;
                  begin
                     T := RT_Float_List;
                     N := First_Node (VT.LVal);
                     while Present (N) loop
                        F   := New_Node (K_Value_Node, Loc (E));
                        VT2 := Get_Value_Type (Item_Val (N));
                        V   :=
                          New_Real_Value
                            (VT2.RVal_Left,
                             VT2.RSign_Left,
                             VT2.RVBase,
                             VT2.RVExp);
                        Set_Item_Val (F, V);
                        RNU.Append_Node_To_List (F, L);
                        N := Next_Node (N);
                     end loop;
                     Result := New_List_Value (L);
                  end;
               else
                  T      := RT_Float;
                  Result :=
                    New_Real_Value
                      (VT.RVal_Left,
                       VT.RSign_Left,
                       VT.RVBase,
                       VT.RVExp);
               end if;
            end;

         when FC_Last =>
            declare
               VT : Value_Type;
               N  : Node_Id;
               R  : Value_Id;
               T2 : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Unknown then
                  Display_Located_Error
                    (Loc (E),
                     "use default float value of 0.0 for " & "operator Last",
                     Fatal   => False,
                     Warning => True);
                  T      := RT_Float;
                  Result := New_Real_Value (0.0);
                  return;

               elsif T2 /= RT_Error then
                  VT := Get_Value_Type (R);

               else
                  T := RT_Error;
                  return;
               end if;

               if T2 = RT_Range_List then
                  declare
                     L   : constant List_Id := New_List (K_List_Id, Loc (E));
                     F   : Node_Id;
                     VT2 : Value_Type;
                     V   : Value_Id;
                  begin
                     T := RT_Float_List;
                     N := First_Node (VT.LVal);
                     while Present (N) loop
                        F   := New_Node (K_Value_Node, Loc (E));
                        VT2 := Get_Value_Type (Item_Val (N));
                        V   :=
                          New_Real_Value
                            (VT2.RVal_Right,
                             VT2.RSign_Right,
                             VT2.RVBase,
                             VT2.RVExp);
                        Set_Item_Val (F, V);
                        RNU.Append_Node_To_List (F, L);
                        N := Next_Node (N);
                     end loop;
                     Result := New_List_Value (L);
                  end;
               else
                  T      := RT_Float;
                  Result :=
                    New_Real_Value
                      (VT.RVal_Right,
                       VT.RSign_Right,
                       VT.RVBase,
                       VT.RVExp);
               end if;
            end;

         when FC_Max =>  --  takes a list as parameter
            declare
               V           : Value_Type;
               VT          : Value_Type;
               N           : Node_Id;
               R           : Value_Id;
               T2          : Return_Type;
               Current_Max : Value_Type;
               Min_Value   : constant Value_Id :=
                 New_Real_Value (Long_Long_Float'Last, True);
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);

               if T2 = RT_Unknown then
                  Display_Located_Error
                    (Loc (E),
                     "use default float value of 0.0 for operator" & " Max",
                     Fatal   => False,
                     Warning => True);
                  T      := RT_Float;
                  Result := New_Real_Value (0.0);
                  return;

               elsif T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               VT := Get_Value_Type (R);
               N  := First_Node (VT.LVal);

               case T2 is
                  when RT_Float_List =>
                     T := RT_Float;
                  when RT_Int_List =>
                     T := RT_Integer;
                  when others =>
                     Display_Located_Error
                       (Loc (E),
                        "'Max' subprogram cannot be " &
                        "run on non-numeric properties",
                        Fatal => True);
               end case;

               Current_Max := Get_Value_Type (Min_Value);
               while Present (N) loop
                  V := Get_Value_Type (Item_Val (N));

                  if not (V < Current_Max) and then not (V = Current_Max) then
                     Current_Max := V;
                  end if;

                  N := Next_Node (N);
               end loop;

               Result := New_Value (Current_Max);
            end;

         when FC_All_Equals =>  --  takes a list as parameter
            declare
               V        : Value_Id;
               VT       : Value_Type;
               Cpt      : Integer := 0;
               Real_Cpt : Float   := 0.0;
               Found    : Boolean := False;
               Equals   : Boolean := True;
               N        : Node_Id;
               Is_Int   : Boolean;
               R        : Value_Id;
               T2       : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Unknown then
                  Display_Located_Error
                    (Loc (E),
                     "use default boolean value of true for " & "operator '='",
                     Fatal   => False,
                     Warning => True);
                  Result := New_Boolean_Value (True);
                  T      := RT_Boolean;
                  return;

               elsif T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               Result := New_Real_Value (0.0);
               VT     := Get_Value_Type (R);
               N      := First_Node (VT.LVal);

               case T2 is
                  when RT_Float_List =>
                     Is_Int := False;

                  when RT_Int_List =>
                     Is_Int := True;

                  when others =>
                     Display_Located_Error
                       (Loc (First_Node (Parameters (E))),
                        "'Max' subprogram cannot be " &
                        "run on non-numeric properties",
                        Fatal => True);
               end case;

               while Equals and then Present (N) loop
                  V := Item_Val (N);
                  if Is_Int then
                     if not Found then
                        Cpt   := Integer (Get_Value_Type (V).IVal);
                        Found := True;
                     else
                        if Integer (Get_Value_Type (V).IVal) /= Cpt then
                           Equals := False;
                        end if;
                     end if;
                  else
                     if not Found then
                        Real_Cpt := Float (Get_Value_Type (V).RVal);
                        Found    := True;
                     else
                        if Float (Get_Value_Type (V).RVal) /= Real_Cpt then
                           Equals := False;
                        end if;
                     end if;
                  end if;
                  N := Next_Node (N);
               end loop;

               Result := New_Boolean_Value (Equals);
               T      := RT_Boolean;
            end;

         when FC_Size =>  --  takes a list as parameter
            declare
               VT  : Value_Type;
               Cpt : Unsigned_Long_Long := 0;
               N   : Node_Id;
               R   : Value_Id;
               T2  : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               VT := Get_Value_Type (R);
               N  := First_Node (VT.LVal);

               case T2 is
                  when RT_Int_List  |
                    RT_Float_List   |
                    RT_String_List  |
                    RT_Bool_List    |
                    RT_Range_List   |
                    RT_Element_List =>
                     T := RT_Integer;

                  when others =>
                     Display_Located_Error
                       (Loc (First_Node (Parameters (E))),
                        "'Size' subprogram cannot be " &
                        "run on non-list values",
                        Fatal => True);
               end case;

               while Present (N) loop
                  Cpt := Cpt + 1;
                  N   := Next_Node (N);
               end loop;

               Result := New_Integer_Value (Cpt);
            end;

         when FC_Min =>  --  takes a list as parameter
            declare
               V           : Value_Type;
               VT          : Value_Type;
               N           : Node_Id;
               R           : Value_Id;
               T2          : Return_Type;
               Current_Min : Value_Type;
               Max_Value   : constant Value_Id :=
                 New_Real_Value (Long_Long_Float'Last, False);
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               if T2 = RT_Unknown then
                  Display_Located_Error
                    (Loc (E),
                     "unknown value, use default value of 0.0 for operator" &
                     " Min",
                     Fatal   => False,
                     Warning => True);

                  Result := New_Real_Value (0.0, False);

               else
                  VT := Get_Value_Type (R);
                  N  := First_Node (VT.LVal);

                  case T2 is
                     when RT_Float_List =>
                        T := RT_Float;
                     when RT_Int_List =>
                        T := RT_Integer;
                     when others =>
                        Display_Located_Error
                          (Loc (E),
                           "'Min' subprogram cannot be " &
                           "run on non-numeric properties",
                           Fatal => True);
                  end case;

                  Current_Min := Get_Value_Type (Max_Value);
                  while Present (N) loop
                     V := Get_Value_Type (Item_Val (N));

                     if V < Current_Min then
                        Current_Min := V;
                     end if;

                     N := Next_Node (N);
                  end loop;

                  Result := New_Value (Current_Min);
               end if;
            end;

         when FC_Head =>  --  takes a list as parameter
            declare
               VT : Value_Type;
               N  : Node_Id;
               R  : Value_Id;
               T2 : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               VT := Get_Value_Type (R);
               N  := First_Node (VT.LVal);

               case T2 is
                  when RT_Int_List =>
                     T := RT_Integer;
                  when RT_Float_List =>
                     T := RT_Float;
                  when RT_String_List =>
                     T := RT_String;
                  when RT_Bool_List =>
                     T := RT_Boolean;
                  when RT_Range_List =>
                     T := RT_Range;
                  when RT_Element_List =>
                     T := RT_Element;
                  when others =>
                     Display_Located_Error
                       (Loc (E),
                        "'head' subprogram cannot be " &
                        "run on a non-list parameter",
                        Fatal => True);
               end case;

               Result := New_Value (Get_Value_Type (Item_Val (N)));
            end;

         when FC_Queue =>  --  takes a list as parameter
            declare
               V  : Value_Id;
               VT : Value_Type;
               N  : Node_Id;
               L  : constant List_Id := New_List (K_List_Id, Loc (E));
               F  : Node_Id;
               R  : Value_Id;
               T2 : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               VT := Get_Value_Type (R);
               N  := First_Node (VT.LVal);

               case T2 is
                  when RT_Int_List  |
                    RT_Float_List   |
                    RT_String_List  |
                    RT_Bool_List    |
                    RT_Range_List   |
                    RT_Element_List =>
                     T := T2;

                  when others =>
                     Display_Located_Error
                       (Loc (E),
                        "'queue' subprogram cannot be " &
                        "run on a non-list parameter",
                        Fatal => True);
               end case;

               if Present (N) then
                  N := Next_Node (N);
               end if;

               while Present (N) loop
                  V := Item_Val (N);
                  F := New_Node (K_Value_Node, Loc (E));
                  Set_Item_Val (F, V);
                  RNU.Append_Node_To_List (F, L);
                  N := Next_Node (N);
               end loop;

               Result := New_List_Value (L);
            end;

         when FC_Int =>  --  takes a float or an integer as parameter
            declare
               VT : Value_Type;
               R  : Value_Id;
               T2 : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               VT := Get_Value_Type (R);
               if VT.T = LT_Real then
                  Result := New_Integer_Value (Unsigned_Long_Long (VT.RVal));
               else
                  Result := New_Integer_Value (VT.IVal);
               end if;
               T := RT_Integer;
            end;

         when FC_List =>
            --  takes a set or a list of literal as parameters
            declare
               R           : Result_Set;
               Result_List : constant List_Id := New_List (K_List_Id, Loc (E));
               V           : Value_Id;
               F, P        : Node_Id;
            begin
               if not Is_Empty (Referenced_Sets (E)) then
                  R :=
                    Set_Array
                      (Integer
                         (Index
                            (Annotation
                               (Referenced_Set
                                  (First_Node (Referenced_Sets (E)))))));
                  for I in 1 .. Cardinal (R) loop
                     V := New_Elem_Value (Get (R, I));
                     F := New_Node (K_Value_Node, Loc (E));
                     Set_Item_Val (F, V);
                     RNU.Append_Node_To_List (F, Result_List);
                  end loop;
                  Result := New_List_Value (Result_List);
                  T      := RT_Element_List;
               elsif not Is_Empty (Parameters (E)) then
                  P := First_Node (Parameters (E));
                  while Present (P) loop
                     Compute_Check_Expression (P, T, Result);
                     F := New_Node (K_Value_Node, Loc (E));
                     Set_Item_Val (F, Result);
                     RNU.Append_Node_To_List (F, Result_List);
                     P := Next_Node (P);
                  end loop;
                  Result := New_List_Value (Result_List);
                  T      := Returned_Type (E);
               end if;
            end;

         when FC_Float =>  --  takes a float or an integer as parameter
            declare
               VT : Value_Type;
               R  : Value_Id;
               T2 : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               VT := Get_Value_Type (R);
               if VT.T = LT_Integer then
                  Result := New_Real_Value (Long_Long_Float (VT.IVal));
               else
                  Result := New_Real_Value (VT.RVal);
               end if;
               T := RT_Float;
            end;

         when FC_Product =>  --  takes a list as parameter
            declare
               V        : Value_Id;
               VT       : Value_Type;
               Cpt      : Long_Long       := 1;
               Real_Cpt : Long_Long_Float := 1.0;
               N        : Node_Id;
               Is_Int   : Boolean;
               R        : Value_Id;
               T2       : Return_Type;
            begin
               Compute_Check_Subprogram_Call
                 (First_Node (Parameters (E)),
                  T2,
                  R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               Result := New_Real_Value (0.0);
               VT     := Get_Value_Type (R);
               N      := First_Node (VT.LVal);

               case T2 is

                  when RT_Float_List =>
                     Is_Int := False;

                  when RT_Int_List =>
                     Is_Int := True;

                  when others =>
                     Display_Located_Error
                       (Loc (First_Node (Parameters (E))),
                        "'Product' subprogram cannot be " &
                        "run on non-numeric properties",
                        Fatal => True);
               end case;

               while Present (N) loop
                  V := Item_Val (N);
                  --  XXX dubious
                  if Is_Int then
                     Cpt := Cpt * Long_Long (Get_Value_Type (V).IVal);
                  else
                     Real_Cpt := Real_Cpt * Get_Value_Type (V).RVal;
                  end if;
                  N := Next_Node (N);
               end loop;

               if Is_Int then
                  Result := New_Integer_Value (Unsigned_Long_Long (Cpt));
                  T      := RT_Integer;
               else
                  Result := New_Real_Value (Real_Cpt);
                  T      := RT_Float;
               end if;
            end;

         when FC_Sum =>  --  takes a list as parameter
            declare
               V        : Value_Id;
               VT       : Value_Type;
               Cpt      : Long_Long       := 0;
               Real_Cpt : Long_Long_Float := 0.0;
               N        : Node_Id;
               Is_Int   : Boolean;
               R        : Value_Id;
               T2       : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);

               if T2 = RT_Unknown then
                  Display_Located_Error
                    (Loc (E),
                     "use default float value of 0.0 for operator" & " Sum",
                     Fatal   => False,
                     Warning => True);
                  T      := RT_Float;
                  Result := New_Real_Value (0.0);
                  return;

               elsif T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               Result := New_Real_Value (0.0);
               VT     := Get_Value_Type (R);
               N      := First_Node (VT.LVal);
               case T2 is
                  when RT_Float_List =>
                     Is_Int := False;

                  when RT_Int_List =>
                     Is_Int := True;

                  when others =>
                     Display_Located_Error
                       (Loc (First_Node (Parameters (E))),
                        "'Sum' subprogram cannot be " &
                        "run on non-numeric properties",
                        Fatal => True);
               end case;

               while Present (N) loop
                  V := Item_Val (N);
                  if Is_Int then
                     Cpt := Cpt + Long_Long (Get_Value_Type (V).IVal);
                  --  XXX Dubious
                  else
                     if Get_Value_Type (V).T = LT_Real then
                        Real_Cpt := Real_Cpt + Get_Value_Type (V).RVal;
                     elsif Get_Value_Type (V).T = LT_Integer then
                        Real_Cpt :=
                          Real_Cpt + Long_Long_Float (Get_Value_Type (V).IVal);
                     else
                        Display_Located_Error
                          (Loc (First_Node (Parameters (E))),
                           "Incompatible types" & Get_Value_Type (V).T'Img,
                           Fatal => True);
                     end if;

                  end if;

                  N := Next_Node (N);
               end loop;

               if Is_Int then
                  Result := New_Integer_Value (Unsigned_Long_Long (Cpt));
                  T      := RT_Integer;
               else
                  Result := New_Real_Value (Real_Cpt);
                  T      := RT_Float;
               end if;
            end;

         when FC_Non_Null =>
            --  Takes an integer as parameter
            --  returns 0 if the parameter is 0
            --  returns 1 in the other cases

            declare
               Cpt : Integer;
               R   : Value_Id;
               T2  : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Integer then
                  Cpt := Integer (Get_Value_Type (R).IVal);
                  if Cpt /= 0 then
                     Cpt := 1;
                  end if;
                  Result := New_Integer_Value (Unsigned_Long_Long (Cpt));
                  T      := RT_Integer;
               else
                  Display_Located_Error
                    (Loc (First_Node (Parameters (E))),
                     "unexpected type",
                     Fatal => True);
               end if;
            end;

         when FC_LCM | FC_GCD =>
            --  takes a list or an list of integers as parameters

            declare
               function Lcm (A, B : Integer) return Integer;
               --  Lowest common multiple

               function Gcd (A, B : Integer) return Integer;
               --  Greatest common divisor

               ---------
               -- Lcm --
               ---------

               function Lcm (A, B : Integer) return Integer is
               begin
                  return (A * B) / Gcd (A, B);
               end Lcm;

               ---------
               -- Gcd --
               ---------

               function Gcd (A, B : Integer) return Integer is
               begin
                  if B = 0 then
                     return A;
                  else
                     return Gcd (B, A mod B);
                  end if;
               end Gcd;

               Is_Lcm   : constant Boolean := ((Code (E)) = FC_LCM);
               V        : Value_Id         := No_Value;
               VT       : Value_Type;
               Cpt      : Integer;
               Last_Cpt : Integer          := 0;
               N        : Node_Id          := First_Node (Parameters (E));
               R        : Value_Id;
               T2       : Return_Type;
            begin
               Compute_Check_Expression (N, T2, R);
               case T2 is
                  when RT_Int_List =>
                     Result := New_Integer_Value (0);
                     VT     := Get_Value_Type (R);
                     N      := First_Node (VT.LVal);

                     while Present (N) loop
                        V  := Item_Val (N);
                        VT := Get_Value_Type (V);
                        if VT.T /= LT_Integer then
                           Display_Located_Error
                             (Loc (N),
                              "unexpected type",
                              Fatal => True);
                        end if;
                        Cpt := Integer (VT.IVal);
                        if Cpt <= 0 then
                           Display_Located_Error
                             (Loc (N),
                              "LCM and GCD must be called with " &
                              "strictly positive parameters",
                              Fatal => True);
                        end if;

                        if Last_Cpt = 0 then
                           Last_Cpt := Cpt;
                        else
                           if Is_Lcm then
                              Last_Cpt := Lcm (Last_Cpt, Cpt);
                           else
                              Last_Cpt := Gcd (Last_Cpt, Cpt);
                           end if;
                        end if;
                        N := Next_Node (N);
                     end loop;
                     if V = No_Value then
                        Display_Error
                          ("LCM and GCD cannot be called on empty lists ",
                           Fatal => True);
                     end if;

                  when RT_Integer =>
                     Last_Cpt := Integer (Get_Value_Type (R).IVal);
                     if Last_Cpt <= 0 then
                        Display_Located_Error
                          (Loc (N),
                           "LCM and GCD must be called with " &
                           "strictly positive parameters",
                           Fatal => True);
                     end if;
                     N := Next_Node (N);

                     while Present (N) loop
                        Compute_Check_Expression (N, T2, R);
                        if T2 = RT_Integer then
                           Cpt := Integer (Get_Value_Type (R).IVal);
                           if Cpt <= 0 then
                              Display_Located_Error
                                (Loc (N),
                                 "LCM and GCD must be called with " &
                                 "strictly positive parameters",
                                 Fatal => True);
                           end if;
                           if Is_Lcm then
                              Last_Cpt := Lcm (Last_Cpt, Cpt);
                           else
                              Last_Cpt := Gcd (Last_Cpt, Cpt);
                           end if;
                        else
                           Display_Located_Error
                             (Loc (N),
                              "unexpected type",
                              Fatal => True);
                        end if;

                        N := Next_Node (N);
                     end loop;

                  when RT_Error =>
                     T := RT_Error;
                     return;

                  when others =>
                     Display_Located_Error
                       (Loc (N),
                        "unexpected type",
                        Fatal => True);
               end case;

               Result := New_Integer_Value (Unsigned_Long_Long (Last_Cpt));
               T      := RT_Integer;
            end;

         when FC_Is_In =>
            --  Takes two parameters that can be either
            --  a list, a set or a variable
            declare
               V           : Value_Id;
               V2          : Value_Id;
               VT          : Value_Type;
               VT2         : Value_Type;
               I           : Node_Id;
               J, K        : Node_Id;
               L_List      : Value_Id;
               R_List      : Value_Id;
               P : constant Node_Id := First_Node (True_Parameters (E));
               Local_Is_In : Boolean          := True;
               T1          : Return_Type;
               T2          : Return_Type;
            begin
               if Is_Empty (Referenced_Sets (E)) then
                  Compute_Check_Expression (P, T1, L_List);
                  if T1 = RT_Error then
                     T := RT_Error;
                     return;
                  end if;
                  Compute_Check_Expression (Next_Node (P), T2, R_List);
                  if T2 = RT_Error then
                     T := RT_Error;
                     return;
                  end if;
                  if T1 /= T2 then
                     Display_Located_Error
                       (Loc (P),
                        "Is_In must be called on lists " &
                        "of same type" &
                        T1'Img &
                        " " &
                        T2'Img,
                        Fatal => True);
                  end if;

                  VT  := Get_Value_Type (L_List);
                  VT2 := Get_Value_Type (R_List);
                  I   := First_Node (VT.LVal);
                  while Present (I) and then Local_Is_In loop
                     V           := Item_Val (I);
                     J           := First_Node (VT2.LVal);
                     Local_Is_In := False;
                     while Present (J) and then not Local_Is_In loop
                        V2          := Item_Val (J);
                        Local_Is_In :=
                          (Get_Value_Type (V) = Get_Value_Type (V2));
                        J := Next_Node (J);
                     end loop;
                     I := Next_Node (I);
                  end loop;
               else
                  --  Contains either a couple of set or an element
                  --  and a set or a list and a set
                  begin
                     if not Present (P) or else Kind (P) = K_Var_Reference then
                        if not Present (P) then
                           R1 :=
                             Set_Array
                               (Integer
                                  (Index
                                     (Annotation
                                        (Referenced_Set
                                           (First_Node
                                              (Referenced_Sets (E)))))));
                           R2 :=
                             Set_Array
                               (Integer
                                  (Index
                                     (Annotation
                                        (Referenced_Set
                                           (Next_Node
                                              (First_Node
                                                 (Referenced_Sets (E))))))));
                        else
                           Extract_Parameters_Sets (E, R1, R2, Success);
                           if not Success then
                              Display_Located_Error
                                (Loc (P),
                                 "Failed to extract parameters",
                                 Fatal => True);
                           end if;
                        end if;
                        for I in 1 .. Cardinal (R1) loop
                           Local_Is_In := False;
                           for J in 1 .. Cardinal (R2) loop
                              if Get (R1, I) = Get (R2, J) then
                                 Local_Is_In := True;
                              end if;
                           end loop;
                           exit when not Local_Is_In;
                        end loop;
                     else
                        R1 :=
                          Set_Array
                            (Integer
                               (Index
                                  (Annotation
                                     (Referenced_Set
                                        (First_Node (Referenced_Sets (E)))))));
                        Compute_Check_Expression (P, T1, L_List);
                        if T1 = RT_Error then
                           T := RT_Error;
                           return;
                        end if;
                        VT := Get_Value_Type (L_List);

                        if Variable_Position (E) = Value_Id (1) then
                           I := First_Node (VT.LVal);
                           while Present (I) and then Local_Is_In loop
                              Local_Is_In := False;
                              K := Get_Value_Type (Item_Val (I)).ELVal;
                              for J in 1 .. Cardinal (R1) loop
                                 if Get (R1, J) = K then
                                    Local_Is_In := True;
                                 end if;
                              end loop;
                              exit when not Local_Is_In;
                              I := Next_Node (I);
                           end loop;
                        else
                           for J in 1 .. Cardinal (R1) loop
                              Local_Is_In := False;
                              I           := First_Node (VT.LVal);
                              while Present (I) and then Local_Is_In loop
                                 K := Get_Value_Type (Item_Val (I)).ELVal;
                                 if Get (R1, J) = K then
                                    Local_Is_In := True;
                                 end if;
                                 I := Next_Node (I);
                              end loop;
                              exit when not Local_Is_In;
                           end loop;
                        end if;
                     end if;
                  end;
               end if;

               T      := RT_Boolean;
               Result := New_Boolean_Value (Local_Is_In);
            end;

         when FC_Cos |
           FC_Sin    |
           FC_Tan    |
           FC_Cosh   |
           FC_Sinh   |
           FC_Tanh   |
           FC_Ln     |
           FC_Exp    |
           FC_Sqrt   |
           FC_Ceil   |
           FC_Floor  =>
            declare
               VT : Value_Type;
               R  : Value_Id;
               T2 : Return_Type;
            begin
               Compute_Check_Expression (First_Node (Parameters (E)), T2, R);
               if T2 = RT_Error then
                  T := RT_Error;
                  return;
               end if;

               VT := Get_Value_Type (R);
               case Code (E) is
                  when FC_Cos =>
                     Result := New_Real_Value (Cos (VT.RVal));
                  when FC_Sin =>
                     Result := New_Real_Value (Sin (VT.RVal));
                  when FC_Tan =>
                     Result := New_Real_Value (Tan (VT.RVal));
                  when FC_Cosh =>
                     Result := New_Real_Value (Cosh (VT.RVal));
                  when FC_Sinh =>
                     Result := New_Real_Value (Sinh (VT.RVal));
                  when FC_Tanh =>
                     Result := New_Real_Value (Tanh (VT.RVal));
                  when FC_Ln =>
                     Result := New_Real_Value (Log (VT.RVal));
                  when FC_Exp =>
                     Result := New_Real_Value (Exp (VT.RVal));
                  when FC_Sqrt =>
                     Result := New_Real_Value (Sqrt (VT.RVal));
                  when FC_Floor =>
                     Result :=
                       New_Real_Value (Long_Long_Float'Floor (VT.RVal));
                  when FC_Ceil =>
                     Result :=
                       New_Real_Value (Long_Long_Float'Ceiling (VT.RVal));
                  when others =>
                     raise Program_Error;
               end case;
               T := RT_Float;
            end;

         when others =>
            Display_Located_Error
              (Loc (E),
               "Unknown or not implemented verification function",
               Fatal => True);
      end case;
   end Compute_Check_Subprogram_Call;

   --------------------------
   -- Build_Predefined_Set --
   --------------------------

   function Build_Predefined_Set (T : Value_Id) return Result_Set is
   begin
      case T is
         when SV_Processor_Set =>
            return Get_Instances_Of_Component_Type (C_Processor);

         when SV_Virtual_Processor_Set =>
            return Get_Instances_Of_Component_Type (C_Virtual_Processor);

         when SV_Process_Set =>
            return Get_Instances_Of_Component_Type (C_Process);

         when SV_Thread_Set =>
            return Get_Instances_Of_Component_Type (C_Thread);

         when SV_Threadgroup_Set =>
            return Get_Instances_Of_Component_Type (C_Thread_Group);

         when SV_Subprogram_Call_Set =>
            return Get_Instances_Of_Component_Type (C_Subprogram_Call);

         when SV_Sequence_Call_Set =>
            return Get_Instances_Of_Component_Type (C_Sequence_Call);

         when SV_Subprogram_Set =>
            return Get_Instances_Of_Component_Type (C_Subprogram);

         when SV_Data_Set =>
            return Get_Instances_Of_Component_Type (C_Data);

         when SV_Memory_Set =>
            return Get_Instances_Of_Component_Type (C_Memory);

         when SV_Bus_Set =>
            return Get_Instances_Of_Component_Type (C_Bus);

         when SV_Virtual_Bus_Set =>
            return Get_Instances_Of_Component_Type (C_Virtual_Bus);

         when SV_Connection_Set =>
            return Get_Instances_Of_Component_Type (C_Connection);

         when SV_Device_Set =>
            return Get_Instances_Of_Component_Type (C_Device);

         when SV_System_Set =>
            return Get_Instances_Of_Component_Type (C_System);

         when SV_Abstract_Set =>
            return Get_Instances_Of_Component_Type (C_Abstract);

         when SV_End_To_End_Flows_Set =>
            return Get_Instances_Of_End_To_End_Flows;

         when SV_Root_System_Set =>
            declare
               Result : Result_Set;
            begin
               Result := Empty_Set;
               Add (Result, AIN.Root_System (Root_System), Distinct => False);
               return Result;
            end;

         when SV_Local_Set =>
            --  Local set is either the AADL component where the theorem
            --  has been declared or the parameter-passed domain

            if RNU.Is_Domain = False then
               return Get_Instances_Of_Component_Type (RNU.Owner_Node);
            else
               return RNU.Domain;
            end if;

         when others =>
            Display_Error ("no such predefined set found", Fatal => True);
            return RNU.Domain;
      end case;
   end Build_Predefined_Set;

   -------------------
   -- Save_Instance --
   -------------------

   function Save_Instance return Runtime_Instance_Access is
      S : constant Runtime_Instance_Access := new Runtime_Instance;

   begin
      S.Set_Array                  := new Set_Table'(Set_Array.all);
      S.all.Current_Range_Variable := Current_Range_Variable;
      S.all.REAL_Root_Node         := RNU.REAL_Root;
      return S;
   end Save_Instance;

   -------------------
   -- Load_Instance --
   -------------------

   procedure Load_Instance (Instance : Runtime_Instance_Access) is
   begin
      Set_Array              := Instance.Set_Array;
      Current_Range_Variable := Instance.all.Current_Range_Variable;
      RNU.REAL_Root          := Instance.all.REAL_Root_Node;
   end Load_Instance;

   --------------
   -- Generate --
   --------------

   procedure Generate (AADL_Root : Node_Id) is
      use RNU.Node_List;

      It   : Natural := First;
      Node : Node_Id;
      Success : Boolean;
      Fd : File_Descriptor;

   begin
      Root_System := Instantiate_Model (AADL_Root);
      Exit_On_Error (No (AADL_Root), "Cannot instantiate AADL models");

      Success := Analyze (REAL_Language, Root_System);
      Exit_On_Error (not Success, "Cannot analyze REAL specifications");

      if Output_Filename /= No_Name then
         Fd := Set_Output (Output_Filename);
      end if;

      --  Runtime

      while It <= Last (To_Run_Theorem_List) loop
         Node := To_Run_Theorem_List.Table (It).Node;

         --  Set local context

         RNU.REAL_Root  := Node;
         RNU.Owner_Node := RN.Related_Entity (Node);

         --  Runtime step

         Write_Line
           (Get_Name_String (RN.Name (RN.Identifier (RNU.REAL_Root))) &
            " execution");

         if not Check_Requirements (RNU.REAL_Root) then
            Display_Located_Error
              (Loc (RNU.REAL_Root),
               "requirements are not fulfilled for theorem " &
               Get_Name_String (RN.Name (RN.Identifier (RNU.REAL_Root))),
               Fatal => not Ocarina.Analyzer.REAL.Continue_Evaluation);
            Write_Line ("");
            Write_Line
              ("theorem " &
               Get_Name_String (RN.Name (RN.Identifier (RNU.REAL_Root))) &
               " is: FALSE");

         else
            Initialize_Sets_Table (RNU.REAL_Root);
            Apply_To_All_Elements (RNU.REAL_Root);
            Clean_Runtime;
         end if;
         Write_Line ("");

         It := It + 1;
      end loop;

      Set_Standard_Output;
      Release_Output (Fd);
   end Generate;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Register_Backend ("real_theorem", Generate'Access, REAL_Theorem);
      Current_Range_Variable := No_Node;
   end Init;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Clean_Runtime;
      Current_Range_Variable := No_Node;
   end Reset;

   --------------------------
   -- Compute_Theorem_Call --
   --------------------------

   procedure Compute_Theorem_Call
     (E         :     Node_Id;
      Local_Set :     Result_Set;
      Result    : out Value_Id;
      Success   : out Boolean)
   is
      pragma Assert (Kind (E) = K_Theorem);

      R : Float;
   begin
      --  1/ Export the user-specified parameters to the environment

      RNU.Is_Domain := True;
      RNU.Domain    := Local_Set;

      --  2/ Initialize the runtime space with new values
      --  Library theorems are already analyzed
      --  so we proceed directly to execution

      Clean_Runtime;
      RNU.REAL_Root := E;
      Initialize_Sets_Table (RNU.REAL_Root);

      --  3/ Launch the specified theorem

      Compute_Value (RNU.REAL_Root, Success, R);
      if not Success then
         Display_Located_Error
           (Loc (RNU.REAL_Root),
            "Could not compute " &
            Get_Name_String (Name (Identifier (E))) &
            " value",
            Fatal => True);
      end if;

      --  FIXME
      --  Temporary message
      --  Should be deleted after usage

      Write_Line ("   returned value : " & Float'Image (R));

      --  4/ Restore current runtime state

      Clean_Runtime;
      RNU.Environment := No_List;
      RNU.Is_Domain   := False;
      RNU.Domain      := Empty_Set;

      Result := New_Real_Value (Long_Long_Float (R));
   end Compute_Theorem_Call;

end Ocarina.Backends.REAL;
