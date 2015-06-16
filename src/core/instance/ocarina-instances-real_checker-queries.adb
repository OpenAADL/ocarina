------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.INSTANCES.REAL_CHECKER.QUERIES                   --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Debug;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Instances.Queries;
with Ocarina.Instances.Finder;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Debug;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Namet;  use Ocarina.Namet;
with Ocarina.Output; use Ocarina.Output;

package body Ocarina.Instances.REAL_Checker.Queries is

   use Ocarina.ME_AADL.AADL_Instances.Debug;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
   use Ocarina.Instances.Queries;
   use Ocarina.Instances.Finder;

   use Set;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package ATD renames Ocarina.ME_AADL.AADL_Tree.Debug;
   use AIN;
   use ATN;

   function Is_Component
     (E           : Node_Id;
      Component_T : Instance_Type) return Boolean;

   ------------------
   -- Is_Component --
   ------------------

   function Is_Component
     (E           : Node_Id;
      Component_T : Instance_Type) return Boolean
   is
      use Ocarina.ME_AADL;
   begin
      case AIN.Kind (E) is
         when K_Call_Sequence_Instance =>
            return (Component_T = C_Sequence_Call);

         when K_Call_Instance =>
            return (Component_T = C_Subprogram_Call);

         when K_Connection_Instance =>
            return (Component_T = C_Connection);

         when K_Component_Instance =>
            case Get_Category_Of_Component (E) is
               when CC_Data =>
                  return (Component_T = C_Data);
               when CC_Subprogram =>
                  return (Component_T = C_Subprogram);
               when CC_Thread =>
                  return (Component_T = C_Thread);
               when CC_Thread_Group =>
                  return (Component_T = C_Thread_Group);
               when CC_Process =>
                  return (Component_T = C_Process);
               when CC_Memory =>
                  return (Component_T = C_Memory);
               when CC_Processor =>
                  return (Component_T = C_Processor);
               when CC_Bus =>
                  return (Component_T = C_Bus);
               when CC_Virtual_Processor =>
                  return (Component_T = C_Virtual_Processor);
               when CC_Virtual_Bus =>
                  return (Component_T = C_Virtual_Bus);
               when CC_Device =>
                  return (Component_T = C_Device);
               when CC_Abstract =>
                  return (Component_T = C_Abstract);
               when CC_System =>
                  return (Component_T = C_System);
               when others =>
                  return (Component_T = C_Unknown);
            end case;

         when others =>
            return False;
      end case;
   end Is_Component;

   ---------------------------------------
   -- Get_Instances_Of_End_To_End_Flows --
   ---------------------------------------

   function Get_Instances_Of_End_To_End_Flows return Result_Set is
      Results : Result_Set;
      EL      : Node_List;
   begin
      Init (Results);
      Find_All_Flows (Root_Instance, EL.First, EL.Last);

      while Present (EL.First) loop
         Append (Results, EL.First);
         EL.First := ATN.Next_Entity (EL.First);
      end loop;

      return Results;
   end Get_Instances_Of_End_To_End_Flows;

   -------------------------------------
   -- Get_Instances_Of_Component_Type --
   -------------------------------------

   function Get_Instances_Of_Component_Type
     (Component_T : Instance_Type) return Result_Set
   is

      function Find_Subprogram_Declaration
        (E   : Node_Id;
         Set : Result_Set) return Boolean;

      ---------------------------------
      -- Find_Subprogram_Declaration --
      ---------------------------------

      function Find_Subprogram_Declaration
        (E   : Node_Id;
         Set : Result_Set) return Boolean
      is
      begin
         for N in First .. Last (Set) loop
            if Corresponding_Declaration (Set.Table (N)) = E then
               return True;
            end if;
         end loop;

         return False;
      end Find_Subprogram_Declaration;

      Results : Result_Set;
      EL      : Node_List;
   begin
      Init (Results);
      case Component_T is
         when C_Sequence_Call =>
            Find_All_Instances
              (Root_Instance,
               (1 => K_Call_Sequence_Instance),
               EL.First,
               EL.Last);

         when C_Subprogram_Call =>
            Find_All_Instances
              (Root_Instance,
               (1 => K_Call_Instance),
               EL.First,
               EL.Last);

         when C_Connection =>
            Find_All_Instances
              (Root_Instance,
               (1 => K_Connection_Instance),
               EL.First,
               EL.Last);

         when others =>
            Find_All_Instances
              (Root_Instance,
               (1 => K_Component_Instance),
               EL.First,
               EL.Last);
      end case;

      while Present (EL.First) loop
         if Is_Component (EL.First, Component_T)
           and then
           (Component_T /= C_Subprogram
            or else not Find_Subprogram_Declaration
              (Corresponding_Declaration (EL.First),
               Results))
         then
            Add (Results, EL.First, Distinct => True);
         end if;
         EL.First := AIN.Next_Entity (EL.First);
      end loop;

      return Results;
   end Get_Instances_Of_Component_Type;

   -------------------------------------
   -- Get_Instances_Of_Component_Type --
   -------------------------------------

   function Get_Instances_Of_Component_Type (E : Node_Id) return Result_Set is
      pragma Assert
        (Kind (E) = K_Component_Type
         or else Kind (E) = K_Component_Implementation
         or else Kind (E) = K_Feature_Group_Type);

      Results : Result_Set;
      EL      : Node_List;
   begin
      Init (Results);

      Find_All_Instances
        (Root_Instance,
         (1 => K_Component_Instance),
         EL.First,
         EL.Last);

      while Present (EL.First) loop
         if Corresponding_Declaration (EL.First) = E then
            Append (Results, EL.First);
         end if;
         EL.First := AIN.Next_Entity (EL.First);
      end loop;

      return Results;
   end Get_Instances_Of_Component_Type;

   ---------------------------------
   -- Get_Instances_With_Property --
   ---------------------------------

   function Get_Instances_With_Property
     (Set           : Result_Set;
      Property_Name : String) return Result_Set
   is
      Result : Result_Set;
   begin
      Init (Result);

      for N in First .. Last (Set) loop
         if Find_Property_Association_From_Name
             (AIN.Properties (Set.Table (N)),
              Property_Name) /=
           No_Node
         then
            Append (Result, Set.Table (N));
         end if;
      end loop;

      return Result;
   end Get_Instances_With_Property;

   --  Set manipulation

   -----------
   -- Is_In --
   -----------

   function Is_In (E : Node_Id; Set : Result_Set) return Boolean is
   begin
      for N in First .. Last (Set) loop
         if Set.Table (N) = E then
            return True;
         end if;
      end loop;

      return False;
   end Is_In;

   ---------
   -- Add --
   ---------

   procedure Add
     (Set      : in out Result_Set;
      E        :        Node_Id;
      Distinct :        Boolean := False)
   is
   begin
      if Distinct then
         if not Is_In (E, Set) then
            Append (Set, E);
         end if;
      else
         Append (Set, E);
      end if;
   end Add;

   -----------
   -- Union --
   -----------

   function Union
     (Set_1    : Result_Set;
      Set_2    : Result_Set;
      Distinct : Boolean := False) return Result_Set
   is
      Result : Result_Set;
   begin

      Init (Result);

      --  Add Set_1

      for N in First .. Last (Set_1) loop
         Result.Table (N) := Set_1.Table (N);
      end loop;

      if Distinct then
         --  Add elements of Set_2 which are not already in the result_set

         for N in First .. Last (Set_2) loop
            if not (Is_In (Set_2.Table (N), Result)) then
               Append (Result, Set_2.Table (N));
            end if;
         end loop;

      else
         --  Add all elements of Set_2 in the result_set

         for N in First .. Last (Set_2) loop
            Append (Result, Set_2.Table (N));
         end loop;
      end if;

      return Result;
   end Union;

   ------------------
   -- Intersection --
   ------------------

   function Intersection
     (Set_1 : Result_Set;
      Set_2 : Result_Set) return Result_Set
   is
      Result : Result_Set;
   begin
      Init (Result);

      --  Add elements of Set_1 which are in the Set_2

      for N in First .. Last (Set_1) loop
         if Is_In (Set_1.Table (N), Set_2) then
            Append (Result, Set_1.Table (N));
         end if;
      end loop;

      return Result;
   end Intersection;

   ---------------
   -- Exclusion --
   ---------------

   function Exclusion
     (Set_1 : Result_Set;
      Set_2 : Result_Set) return Result_Set
   is
      Result : Result_Set;
   begin
      Init (Result);
      --  Add elements of Set_1 which are *not* in the Set_2

      for N in First .. Last (Set_1) loop
         if not Is_In (Set_1.Table (N), Set_2) then
            Append (Result, Set_1.Table (N));
         end if;
      end loop;

      return Result;
   end Exclusion;

   --------------
   -- Includes --
   --------------

   function Includes (Set_1 : Result_Set; Set_2 : Result_Set) return Boolean is
   begin
      for N in First .. Last (Set_2) loop
         if not Is_In (Set_2.Table (N), Set_1) then
            return False;
         end if;
      end loop;

      return True;
   end Includes;

   ----------------------
   -- Mutual_Inclusion --
   ----------------------

   function Mutual_Inclusion
     (Set_1 : Result_Set;
      Set_2 : Result_Set) return Boolean
   is
   begin
      return (Includes (Set_1, Set_2) and then Last (Set_1) = Last (Set_2));
   end Mutual_Inclusion;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Set : Result_Set) return Boolean is
   begin
      return (Last (Set) < First);
   end Is_Empty;

   --------------
   -- Cardinal --
   --------------

   function Cardinal (Set : Result_Set) return Natural is
   begin
      return Natural (Last (Set) - First + 1);
   end Cardinal;

   ---------------
   -- Empty_Set --
   ---------------

   function Empty_Set return Result_Set is
      R : Result_Set;
   begin
      Init (R);
      return R;
   end Empty_Set;

   ------------------------
   -- Get_Property_Value --
   ------------------------

   function Get_Property_Value
     (E    : Ocarina.Types.Node_Id;
      Name : String) return Ocarina.Types.Node_Id
   is
      N : Ocarina.Types.Node_Id;
   begin
      case AIN.Kind (E) is
         when K_Call_Instance | K_Call_Sequence_Instance =>
            N :=
              Get_Value_Of_Property_Association
                (Corresponding_Instance (E),
                 Get_String_Name (Name));

         when others =>
            N := Get_Value_Of_Property_Association (E, Get_String_Name (Name));
      end case;

      return N;
   end Get_Property_Value;

   ---------
   -- Get --
   ---------

   function Get (Set : Result_Set; Index : Natural) return Node_Id is
   begin
      return Set.Table (Index);
   end Get;

   ----------------
   -- Test_Dummy --
   ----------------

   function Test_Dummy (C : Instance_Type) return Result_Set is
      Results : Result_Set;
   begin
      Results := Get_Instances_Of_Component_Type (C);

      return Results;
   end Test_Dummy;

   ---------------------
   -- Test_Dummy_Sets --
   ---------------------

   function Test_Dummy_Sets return Result_Set is
      C1 : constant Instance_Type := C_Subprogram_Call;
      C2 : constant Instance_Type := C_Thread;
      R1 : Result_Set;
      R2 : Result_Set;
      R3 : Result_Set;
   begin
      R1 := Test_Dummy (C1);
      Display_Set (R1);
      W_Line ("------");
      R2 := Test_Dummy (C2);

      R3 := Union (R1, R2);

      return R3;
   end Test_Dummy_Sets;

   -----------------
   -- Display_Set --
   -----------------

   procedure Display_Set (Set : Result_Set) is
   begin
      for N in First .. Last (Set) loop

         --  XXX FIXME
         --  Very dangerous, but unavoidable yet : end to end flows
         --  instances should be defined

         if ATN.Kind (Set.Table (N)) /= K_End_To_End_Flow_Spec then
            Write_Indentation;
            Write_Name (Compute_Full_Name_Of_Instance (Set.Table (N)));
            W_Str (": ");
            W_Node_Header (Set.Table (N));
         else
            W_Str ("anonymous end to end flow :");
            ATD.W_Node_Header (Set.Table (N));
         end if;
      end loop;
   end Display_Set;

   ----------
   -- Init --
   ----------

   procedure Init (Root : Node_Id) is
   begin
      Root_Instance := Root;
   end Init;

end Ocarina.Instances.REAL_Checker.Queries;
