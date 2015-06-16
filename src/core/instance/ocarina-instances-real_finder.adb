------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . I N S T A N C E S . R E A L _ F I N D E R         --
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

with Locations;

with Ocarina.AADL_Values;

with Ocarina.Instances.Queries;

with Ocarina.ME_AADL.AADL_Tree.Nodes;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Entities.Properties;

with Ocarina.ME_REAL.REAL_Tree.Nodes;
with Ocarina.ME_REAL.REAL_Tree.Nutils;

with Ocarina.Processor.Properties;

with Ocarina.REAL_Values;

package body Ocarina.Instances.REAL_Finder is

   use Ocarina.Instances.Queries;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Nodes;
   use Ocarina.ME_REAL.REAL_Tree.Nutils;
   use Ocarina.REAL_Values;
   use Ocarina.Processor.Properties;

   package OV renames Ocarina.AADL_Values;
   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AIEP renames Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
   package RNU renames Ocarina.ME_REAL.REAL_Tree.Nutils;
   package RV renames Ocarina.REAL_Values;

   ------------------------
   -- Get_Property_Value --
   ------------------------

   function Get_Property_Value
     (Property_Name : Name_Id;
      Node          : Node_Id) return Value_Id
   is
      V : Value_Id;
   begin
      V := RV.New_String_Value (Property_Name);
      return Get_Property_Value_Function (V, RT_Unknown, Node);
   end Get_Property_Value;

   ---------------------------------
   -- Get_Property_Value_Function --
   ---------------------------------

   function Get_Property_Value_Function
     (Property : Value_Id;
      T        : Return_Type;
      Var      : Node_Id) return Value_Id
   is
      use Locations;

      use OV;
      use type ATN.Node_Kind;

      pragma Assert
        (AIN.Kind (Var) = AIN.K_Component_Instance
         or else AIN.Kind (Var) = AIN.K_Call_Instance);

      VT2           : constant RV.Value_Type := RV.Get_Value_Type (Property);
      Property_Name : constant Name_Id       := To_Lower (VT2.SVal);
      VT            : OV.Value_Type;
      Result        : Value_Id               := REAL_Values.No_Value;
      N             : Node_Id;
      Is_List       : Boolean                := False;
      Resolved_Var  : Node_Id;
      Val           : Node_Id;
      Result_List : constant List_Id := RNU.New_List (K_List_Id, No_Location);

   begin
      if AIN.Kind (Var) = AIN.K_Call_Instance then
         Resolved_Var := AIN.Corresponding_Instance (Var);
      else
         Resolved_Var := Var;
      end if;

      --  XXX Expanded_Multi_Value seems bugged. Fix it, and use
      --  Get_Value_Of_Property_Association instead

      N :=
        AIEP.Find_Property_Association_From_Name
          (Property_List => AIN.Properties (Resolved_Var),
           Property_Name => Property_Name);

      if Present (N) then
         N := AIN.Property_Association_Value (N);
         if ATN.Expanded_Single_Value (N) /= No_Node then
            N := ATN.Expanded_Single_Value (N);
         elsif ATN.Single_Value (N) /= No_Node then
            N := ATN.Single_Value (N);
         elsif ATN.Multi_Value (N) /= No_List then
            N       := ATN.First_Node (ATN.Multi_Value (N));
            Is_List := True;
         else
            N := No_Node;
         end if;

      else
         --  In the specific case of lists, we always returns an empty list

         case T is
            when RT_String_List |
              RT_Float_List     |
              RT_Int_List       |
              RT_Bool_List      |
              RT_Range_List     |
              RT_Element_List   =>
               Result := RV.New_List_Value (Result_List);
               return Result;

            when others =>
               Result := RV.No_Value;
         end case;
      end if;

      while Present (N) loop
         if not Is_List then
            Is_List := Present (ATN.Next_Node (N));
         end if;

         case ATN.Kind (N) is
            when ATN.K_Signed_AADLNumber =>
               declare
                  Base_Value : Node_Id;
               begin
                  if Present (ATN.Unit_Identifier (N))
                    and then Present
                      (ATN.Corresponding_Entity (ATN.Unit_Identifier (N)))
                  then
                     Base_Value :=
                       Convert_To_Base
                         (ATN.Number_Value (N),
                          ATN.Corresponding_Entity (ATN.Unit_Identifier (N)));
                     Result := AADL_Value (ATN.Value (Base_Value));
                  else
                     Result := AADL_Value (ATN.Value (ATN.Number_Value (N)));
                  end if;
               end;

            when ATN.K_Literal =>
               VT := OV.Get_Value_Type (ATN.Value (N));

               --  Enum are turned into strings

               if VT.T = OV.LT_Enumeration then
                  Result := RV.New_String_Value (VT.EVal);
               else
                  Result := AADL_Value (ATN.Value (N));
               end if;

            when ATN.K_Number_Range_Term =>
               declare
                  L, R       : Value_Id;
                  LT, RT     : RV.Value_Type;
                  Base_Value : Node_Id;
               begin
                  if Present (ATN.Unit_Identifier (ATN.Lower_Bound (N)))
                    and then Present
                      (ATN.Corresponding_Entity
                         (ATN.Unit_Identifier (ATN.Lower_Bound (N))))
                  then
                     Base_Value :=
                       Convert_To_Base
                         (ATN.Number_Value (ATN.Lower_Bound (N)),
                          ATN.Corresponding_Entity
                            (ATN.Unit_Identifier (ATN.Lower_Bound (N))));
                     L := AADL_Value (ATN.Value (Base_Value));
                  else
                     L :=
                       AADL_Value
                         (ATN.Value (ATN.Number_Value (ATN.Lower_Bound (N))));
                  end if;
                  LT := Get_Value_Type (L);

                  if Present (ATN.Unit_Identifier (ATN.Upper_Bound (N)))
                    and then Present
                      (ATN.Corresponding_Entity
                         (ATN.Unit_Identifier (ATN.Upper_Bound (N))))
                  then
                     Base_Value :=
                       Convert_To_Base
                         (ATN.Number_Value (ATN.Upper_Bound (N)),
                          ATN.Corresponding_Entity
                            (ATN.Unit_Identifier (ATN.Upper_Bound (N))));
                     R := AADL_Value (ATN.Value (Base_Value));
                  else
                     R :=
                       AADL_Value
                         (ATN.Value (ATN.Number_Value (ATN.Upper_Bound (N))));
                  end if;
                  RT := Get_Value_Type (R);

                  if LT.T = LT_Integer then
                     Result :=
                       New_Range_Value
                         (Long_Long_Float (LT.IVal),
                          Long_Long_Float (RT.IVal),
                          LT.ISign,
                          RT.ISign,
                          RT.IBase,
                          RT.IExp);
                  else
                     Result :=
                       New_Range_Value
                         (LT.RVal,
                          RT.RVal,
                          LT.RSign,
                          RT.RSign,
                          RT.RBase,
                          RT.RExp);
                  end if;
               end;

            when ATN.K_Enumeration_Term =>
               Result := RV.New_String_Value (ATN.Name (ATN.Identifier (N)));

            when ATN.K_Reference_Term =>
               --  In the case of reference term, we change the logic:
               --  we use Get_List_Property to fetch all elements, and
               --  then enqueue them in the Result.

               if not Is_List then
                  Result :=
                    RV.New_Elem_Value
                      (Get_Reference_Property (Resolved_Var, Property_Name));
               else
                  declare
                     A_List : constant List_Id :=
                       Get_List_Property (Resolved_Var, Property_Name);
                     A_Node : Node_Id;
                  begin
                     A_Node := ATN.First_Node (A_List);
                     while Present (A_Node) loop
                        Result :=
                          RV.New_Elem_Value
                            (ATN.Entity (ATN.Reference_Term (A_Node)));
                        Val := New_Node (K_Value_Node, No_Location);
                        Set_Item_Val (Val, Result);
                        RNU.Append_Node_To_List (Val, Result_List);
                        A_Node := ATN.Next_Node (A_Node);
                     end loop;

                     return RV.New_List_Value (Result_List);
                  end;
               end if;

            when ATN.K_Component_Classifier_Term =>
               Result :=
                 RV.New_Elem_Value
                   (Get_Classifier_Property (Resolved_Var, Property_Name));

            when others =>
               return RV.No_Value;
         end case;

         if Is_List and then ATN.Kind (N) /= ATN.K_Reference_Term then
            Val := New_Node (K_Value_Node, No_Location);
            Set_Item_Val (Val, Result);
            RNU.Append_Node_To_List (Val, Result_List);
         end if;

         N := ATN.Next_Node (N);
      end loop;

      if Is_List then
         Result := RV.New_List_Value (Result_List);
      end if;

      return Result;
   end Get_Property_Value_Function;

end Ocarina.Instances.REAL_Finder;
