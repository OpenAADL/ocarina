------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--              OCARINA.ME_AADL.AADL_TREE.ENTITIES.PROPERTIES               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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
with Charset;
with Utils;

with Ocarina.ME_AADL.AADL_Tree.Nutils;

package body Ocarina.ME_AADL.AADL_Tree.Entities.Properties is

   use Ocarina.Namet;
   use Charset;
   use Utils;

   use Ocarina.ME_AADL.AADL_Tree.Nutils;

   function Get_Type_Of_Literal (Literal : Node_Id) return Property_Type;

   -------------------------
   -- Get_Type_Of_Literal --
   -------------------------

   function Get_Type_Of_Literal (Literal : Node_Id) return Property_Type is
      pragma Assert (Literal /= No_Node and then Kind (Literal) = K_Literal);

      Literal_Value : constant Value_Type := Get_Value_Type (Value (Literal));
   begin
      case (Literal_Value.T) is
         when LT_Boolean =>
            return PT_Boolean;

         when LT_String =>
            return PT_String;

         when LT_Real =>
            return PT_Float;

         when LT_Integer =>
            return PT_Integer;

         when LT_Enumeration =>
            return PT_Enumeration;
      end case;
   end Get_Type_Of_Literal;

   ------------------------------------------------
   -- Value_Of_Property_Association_Is_Undefined --
   ------------------------------------------------

   function Value_Of_Property_Association_Is_Undefined
     (Property : Node_Id) return Boolean
   is
      pragma Assert (Kind (Property) = K_Property_Association);
      pragma Assert
        (Single_Value (Property_Association_Value (Property)) /= No_Node
         or else
           Multi_Value (Property_Association_Value (Property)) /=
           No_List);
   begin
      --  the value of a property association is undefined if there is
      --  no expanded value, and the raw values either are empty, or
      --  if the expanded values are set to an empty list

      return Expanded_Single_Value (Property_Association_Value (Property)) =
        No_Node
        and then
        (Expanded_Multi_Value (Property_Association_Value (Property)) =
         No_List);
   end Value_Of_Property_Association_Is_Undefined;

   --------------------------------
   -- Type_Of_Property_Is_A_List --
   --------------------------------

   function Type_Of_Property_Is_A_List (Property : Node_Id) return Boolean is
      pragma Assert
        (Kind (Property) = K_Property_Association
         or else Kind (Property) = K_Property_Definition_Declaration
         or else Kind (Property) = K_Property_Type_Declaration);
   begin
      case Kind (Property) is
         when K_Property_Association =>
            if Expanded_Single_Value (Property_Association_Value (Property)) =
              No_Node
              and then
                Expanded_Multi_Value (Property_Association_Value (Property)) =
                No_List
            then
               return Multi_Value (Property_Association_Value (Property)) /=
                 No_List;

            --  If the property value has not been expanded yet, we
            --  use the raw property value.
            else
               return Expanded_Multi_Value
                   (Property_Association_Value (Property)) /=
                 No_List;
            end if;

         when K_Property_Definition_Declaration =>
            return Is_List (Property_Name_Type (Property));

         when K_Property_Type_Declaration =>
            return False;

         when others =>
            return False;
      end case;
   end Type_Of_Property_Is_A_List;

   --------------------------
   -- Get_Type_Of_Property --
   --------------------------

   function Get_Type_Of_Property
     (Property             : Node_Id;
      Use_Evaluated_Values : Boolean := True) return Property_Type
   is
      pragma Assert
        (Kind (Property) = K_Property_Association
         or else Kind (Property) = K_Property_Definition_Declaration
         or else Kind (Property) = K_Property_Type_Declaration);

   begin
      case Kind (Property) is
         when K_Property_Association =>
            return Get_Type_Of_Property_Value
                (Property_Association_Value (Property),
                 Use_Evaluated_Values);

         when K_Property_Definition_Declaration =>
            declare
               Associated_Type : Node_Id;
            begin
               if Use_Evaluated_Values
                 and then
                   Expanded_Type_Designator (Property_Name_Type (Property)) /=
                   No_Node
               then
                  Associated_Type :=
                    Expanded_Type_Designator (Property_Name_Type (Property));
               else
                  Associated_Type :=
                    Property_Type_Designator (Property_Name_Type (Property));
               end if;

               case Kind (Associated_Type) is
                  when K_Unique_Property_Type_Identifier =>
                     return PT_Other;

                  when K_String_Type =>
                     return PT_String;

                  when K_Boolean_Type =>
                     return PT_Boolean;

                  when K_Real_Type =>
                     return PT_Float;

                  when K_Integer_Type =>
                     return PT_Integer;

                  when K_Range_Type =>
                     return PT_Range;

                  when K_Enumeration_Type =>
                     return PT_Enumeration;

                  when K_Reference_Type =>
                     return PT_Reference;

                  when K_Classifier_Type =>
                     return PT_Classifier;

                  when K_Record_Type =>
                     return PT_Record;

                  when others =>
                     return PT_Other;
               end case;
            end;

         when K_Property_Type_Declaration =>
            case Kind (Property_Type_Designator (Property)) is
               when K_Integer_Type =>
                  return PT_Integer;

               when K_Real_Type =>
                  return PT_Float;

               when K_Boolean_Type =>
                  return PT_Boolean;

               when K_String_Type =>
                  return PT_String;

               when K_Unique_Property_Type_Identifier =>
                  return PT_Other;

               when K_Range_Type =>
                  return PT_Range;

               when K_Enumeration_Type =>
                  return PT_Enumeration;

               when K_Reference_Type =>
                  return PT_Reference;

               when K_Classifier_Type =>
                  return PT_Classifier;

               when K_Units_Type =>
                  return PT_Other;

               when others =>
                  return PT_Other;
            end case;

         when others =>
            raise Program_Error;
      end case;
   end Get_Type_Of_Property;

   --------------------------------
   -- Get_Type_Of_Property_Value --
   --------------------------------

   function Get_Type_Of_Property_Value
     (Property_Value       : Node_Id;
      Use_Evaluated_Values : Boolean := True) return Property_Type
   is
      pragma Assert
        (No (Property_Value)
         or else Kind (Property_Value) = K_Property_Value
         or else Kind (Property_Value) = K_Literal
         or else Kind (Property_Value) = K_Minus_Numeric_Term
         or else Kind (Property_Value) = K_Signed_AADLNumber
         or else Kind (Property_Value) = K_Number_Range_Term
         or else Kind (Property_Value) = K_Not_Boolean_Term
         or else Kind (Property_Value) = K_And_Boolean_Term
         or else Kind (Property_Value) = K_Or_Boolean_Term
         or else Kind (Property_Value) = K_Parenthesis_Boolean_Term
         or else Kind (Property_Value) = K_Reference_Term
         or else Kind (Property_Value) = K_Record_Term
         or else Kind (Property_Value) = K_Property_Term
         or else Kind (Property_Value) = K_Enumeration_Term
         or else Kind (Property_Value) = K_Component_Classifier_Term);

      Value_Type : Property_Type;
      Value_Node : Node_Id;
   begin
      if Property_Value = No_Node then
         Value_Node := No_Node;

      elsif Kind (Property_Value) = K_Property_Value then
         if Use_Evaluated_Values then
            if Expanded_Single_Value (Property_Value) /= No_Node then
               Value_Node := Expanded_Single_Value (Property_Value);

            elsif Expanded_Multi_Value (Property_Value) /= No_List then
               Value_Node :=
                 First_Node (Expanded_Multi_Value (Property_Value));

               --  If we are dealing with a list of values, only
               --  consider the first value, assuming the other ones
               --  are of the same type.
            else
               Value_Node := No_Node;
            end if;

            --  XXX todo : fix why Property_Association with
            --  Range_Term _and_ Record_Term Property_Value doesn't
            --  have Expanded_Value.

            if No (Value_Node) then
               if Single_Value (Property_Value) /= No_Node then
                  Value_Node := Single_Value (Property_Value);

               elsif Multi_Value (Property_Value) /= No_List then
                  Value_Node := First_Node (Multi_Value (Property_Value));
               end if;
            end if;

         else
            if Single_Value (Property_Value) /= No_Node then
               Value_Node := Single_Value (Property_Value);
            elsif Multi_Value (Property_Value) /= No_List then
               Value_Node := First_Node (Multi_Value (Property_Value));
            else
               Value_Node := No_Node;
            end if;
         end if;

      else
         Value_Node := Property_Value;
      end if;

      if Value_Node /= No_Node then
         case Kind (Value_Node) is
            when K_Literal =>
               Value_Type := Get_Type_Of_Literal (Value_Node);

            when K_Minus_Numeric_Term =>
               Value_Type :=
                 Get_Type_Of_Property_Value
                   (Numeric_Term (Value_Node),
                    Use_Evaluated_Values);

            when K_Signed_AADLNumber =>
               Value_Type :=
                 Get_Type_Of_Property_Value
                   (Number_Value (Value_Node),
                    Use_Evaluated_Values);

            when K_Number_Range_Term =>
               Value_Type := PT_Range;

            when K_Not_Boolean_Term      |
              K_And_Boolean_Term         |
              K_Or_Boolean_Term          |
              K_Parenthesis_Boolean_Term =>
               Value_Type := PT_Boolean_Expression;

            when K_Reference_Term =>
               Value_Type := PT_Reference;

            when K_Enumeration_Term =>
               Value_Type := PT_Enumeration;

            when K_Component_Classifier_Term =>
               Value_Type := PT_Classifier;

            when K_Record_Term =>
               Value_Type := PT_Record;

            --  XXX add here unit_term and record_term case

            when others =>
               Value_Type := PT_Other;
         end case;

      else
         Value_Type := PT_Other;
      end if;

      return Value_Type;
   end Get_Type_Of_Property_Value;

   -----------------------------------
   -- Get_Integer_Of_Property_Value --
   -----------------------------------

   function Get_Integer_Of_Property_Value
     (Property_Value : Node_Id) return Unsigned_Long_Long
   is
   begin
      case Kind (Property_Value) is
         when K_Literal =>
            return Get_Value_Type (Value (Property_Value)).IVal;
         when K_Signed_AADLNumber =>
            return Get_Value_Type (Value (Number_Value (Property_Value))).IVal;
         when others =>
            raise Program_Error;
      end case;
   end Get_Integer_Of_Property_Value;

   ---------------------------------
   -- Get_Float_Of_Property_Value --
   ---------------------------------

   function Get_Float_Of_Property_Value
     (Property_Value : Node_Id) return Long_Long_Float
   is
      pragma Assert (Kind (Number_Value (Property_Value)) = K_Literal);
   begin
      return Get_Value_Type (Value (Number_Value (Property_Value))).RVal;
   end Get_Float_Of_Property_Value;

   ----------------------------------
   -- Get_String_Of_Property_Value --
   ----------------------------------

   function Get_String_Of_Property_Value
     (Property_Value : Node_Id) return Name_Id
   is
      pragma Assert (Kind (Property_Value) = K_Literal);
   begin
      return Get_Value_Type (Value (Property_Value)).SVal;
   end Get_String_Of_Property_Value;

   function Get_String_Of_Property_Value
     (Property_Value : Node_Id) return String
   is
      pragma Assert (Kind (Property_Value) = K_Literal);
   begin
      return Get_Name_String (Get_String_Of_Property_Value (Property_Value));
   end Get_String_Of_Property_Value;

   ---------------------------------------
   -- Get_Enumeration_Of_Property_Value --
   ---------------------------------------

   function Get_Enumeration_Of_Property_Value
     (Property_Value : Node_Id) return Name_Id
   is
      pragma Assert
        (Kind (Property_Value) = K_Enumeration_Term
         or else
         (Kind (Property_Value) = K_Literal
          and then
            Get_Value_Type (Value (Property_Value)).T =
            LT_Enumeration));
   begin
      case Kind (Property_Value) is
         when K_Literal =>
            return Get_Value_Type (Value (Property_Value)).EVal;

         when K_Enumeration_Term =>
            return Name (Identifier (Property_Value));

         when others =>
            raise Program_Error;
      end case;
   end Get_Enumeration_Of_Property_Value;

   function Get_Enumeration_Of_Property_Value
     (Property_Value : Node_Id) return String
   is
      pragma Assert
        (Kind (Property_Value) = K_Enumeration_Term
         or else
         (Kind (Property_Value) = K_Literal
          and then
            Get_Value_Type (Value (Property_Value)).T =
            LT_Enumeration));
   begin
      case Kind (Property_Value) is
         when K_Literal =>
            return Get_Name_String
                (Get_Enumeration_Of_Property_Value (Property_Value));
         when K_Enumeration_Term =>
            return Get_Name_String (Name (Identifier (Property_Value)));

         when others =>
            raise Program_Error;
      end case;

   end Get_Enumeration_Of_Property_Value;

   -----------------------------------
   -- Get_Boolean_Of_Property_Value --
   -----------------------------------

   function Get_Boolean_Of_Property_Value
     (Property_Value : Node_Id) return Boolean
   is
      pragma Assert (Kind (Property_Value) = K_Literal);
   begin
      return Get_Value_Type (Value (Property_Value)).BVal;
   end Get_Boolean_Of_Property_Value;

   -------------------------------------
   -- Get_Reference_Of_Property_Value --
   -------------------------------------

   function Get_Reference_Of_Property_Value
     (Property_Value : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Property_Value) = K_Reference_Term);
   begin
      return Get_Referenced_Entity (Property_Value);
   end Get_Reference_Of_Property_Value;

   --------------------------------------
   -- Get_Classifier_Of_Property_Value --
   --------------------------------------

   function Get_Classifier_Of_Property_Value
     (Property_Value : Node_Id) return Node_Id
   is
      pragma Assert (Kind (Property_Value) = K_Component_Classifier_Term);
   begin
      return Get_Referenced_Entity (Property_Value);
   end Get_Classifier_Of_Property_Value;

   ----------------------------------
   -- Get_Record_Of_Property_Value --
   ----------------------------------

   function Get_Record_Of_Property_Value
     (Property_Value : Node_Id) return List_Id
   is
      pragma Unreferenced (Property_Value);
   begin
      return No_List;
   end Get_Record_Of_Property_Value;

   ---------------------------------------
   -- Get_Value_Of_Property_Association --
   ---------------------------------------

   function Get_Value_Of_Property_Association
     (Property : Node_Id) return Value_Type
   is
      pragma Assert (Kind (Property) = K_Property_Association);
      pragma Assert
        (Kind (Single_Value (Property_Association_Value (Property))) =
         K_Literal
         or else
           Kind
             (Number_Value
                (Single_Value (Property_Association_Value (Property)))) =
           K_Literal);
   begin
      if Kind (Single_Value (Property_Association_Value (Property))) =
        K_Literal
      then
         return Get_Value_Type
             (Value (Single_Value (Property_Association_Value (Property))));
      else
         return Get_Value_Type
             (Value
                (Number_Value
                   (Single_Value (Property_Association_Value (Property)))));
      end if;
   end Get_Value_Of_Property_Association;

   -----------------------------------------
   -- Find_Property_Association_From_Name --
   -----------------------------------------

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : Name_Id;
      In_Mode       : Name_Id := No_Name) return Node_Id
   is
      List_Node     : Node_Id;
      M             : Node_Id;
      Mode_Name     : Name_Id;
      Lower_In_Mode : constant Name_Id := To_Lower (In_Mode);
   begin
      if Property_List /= No_List then
         List_Node := First_Node (Property_List);

         while List_Node /= No_Node loop
            if Name (Identifier (List_Node)) = Property_Name then
               --  Verify if the 'in mode' clause of this property
               --  matches with the In_Mode parameter value. There is
               --  a match if:

               --  1 - The 'in modes' list of the property association
               --      is empty.

               --  2 - The 'in_mode' parameter is invalid (No_Name)

               --  3 - The 'in modes' list of the property association
               --      is not empty, the 'in_mode' parameter is valid
               --      and its value corresponds to the name of one
               --      element of the list.

               if No (In_Modes (List_Node))
                 or else Is_Empty (Modes (In_Modes (List_Node)))
                 or else In_Mode = No_Name
               then
                  return List_Node;
               else
                  M := First_Node (Modes (In_Modes (List_Node)));

                  while Present (M) loop
                     --  Depending on the nature of the traversed tree
                     --  (model tree or instance tree), the structure
                     --  of the mode list of a property is not the
                     --  same.

                     case Kind (M) is
                        when K_Entity_Reference =>
                           Mode_Name := Name (Identifier (M));

                        when K_Node_Container =>
                           Mode_Name := Name (Identifier (Item (M)));

                        when others =>
                           raise Program_Error;
                     end case;

                     if Mode_Name = Lower_In_Mode then
                        return List_Node;
                     end if;

                     M := Next_Node (M);
                  end loop;
               end if;
            end if;

            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return No_Node;
   end Find_Property_Association_From_Name;

   -----------------------------------------
   -- Find_Property_Association_From_Name --
   -----------------------------------------

   function Find_Property_Association_From_Name
     (Property_List : List_Id;
      Property_Name : String;
      In_Mode       : Name_Id := No_Name) return Node_Id
   is
      Name : Name_Id;
   begin
      Set_Str_To_Name_Buffer (To_Lower (Property_Name));
      Name := Name_Find;

      return Find_Property_Association_From_Name
          (Property_List,
           Name,
           In_Mode);
   end Find_Property_Association_From_Name;

   ------------------------------
   -- Resolve_Term_In_Property --
   ------------------------------

   procedure Resolve_Term_In_Property
     (Property  : Node_Id;
      Value     : Node_Id;
      Kind_Node : Node_Kind)
   is
      pragma Assert (Present (Value));
      pragma Assert
        (Kind (Property) = K_Property_Association
         or else Kind (Property) = K_Constant_Property_Declaration
         or else Kind (Property) = K_Property_Definition_Declaration
         or else Kind (Property) = K_Record_Term_Element
         or else Kind (Property) = K_Record_Type_Element);
      pragma Assert
        (Kind_Node = K_Enumeration_Term or else Kind_Node = K_Unit_Term);

      Node : Node_Id := No_Node;
   begin

      if Kind_Node = K_Enumeration_Term then
         Node := New_Node (Kind_Node, Loc (Value));

         Set_Identifier (Node, Identifier (Value));
         Set_Property_Set_Identifier (Node, Property_Set_Identifier (Value));
         Set_Entity (Node, Entity (Value));

         case Kind (Property) is
            when K_Property_Association =>
               if Single_Value (Property_Association_Value (Property)) /=
                 No_Node
               then
                  Set_Single_Value
                    (Property_Association_Value (Property),
                     Node);
               elsif Multi_Value (Property_Association_Value (Property)) /=
                 No_List
               then
                  Replace_Node_To_List
                    (Multi_Value (Property_Association_Value (Property)),
                     Value,
                     Node);
               end if;

            when K_Property_Definition_Declaration =>
               if Multi_Value (Default_Value (Property)) /= No_List then
                  Replace_Node_To_List
                    (Multi_Value (Default_Value (Property)),
                     Value,
                     Node);
               else
                  Set_Single_Value (Default_Value (Property), Node);
               end if;

            when K_Constant_Property_Declaration =>
               if Single_Value (Constant_Value (Property)) /= No_Node then
                  Set_Single_Value (Constant_Value (Property), Node);
               else
                  Replace_Node_To_List
                    (Multi_Value (Constant_Value (Property)),
                     Value,
                     Node);
               end if;

            when others =>
               null;
         end case;
      end if;

   end Resolve_Term_In_Property;

end Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
