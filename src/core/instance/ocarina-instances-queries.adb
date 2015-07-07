------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            O C A R I N A . I N S T A N C E S . Q U E R I E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

package body Ocarina.Instances.Queries is

   use Ocarina.Namet;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AIEP renames Ocarina.ME_AADL.AADL_Instances.Entities.Properties;
   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;

   -------------------------------------
   -- Compute_Absolute_Name_Of_Entity --
   -------------------------------------

   function Compute_Absolute_Name_Of_Entity
     (Entity    : Node_Id;
      Separator : Name_Id := No_Name) return Name_Id
   is
      pragma Assert (Present (Entity));

      Parent_Name      : Name_Id;
      Entity_Name      : Name_Id;
      Actual_Separator : Name_Id := Separator;
   begin
      if Actual_Separator = No_Name then
         Set_Str_To_Name_Buffer (".");
         Actual_Separator := Name_Find;
      end if;

      case AIN.Kind (Entity) is
         when K_Component_Instance =>
            if Parent_Subcomponent (Entity) = No_Node then
               --  If we are processing the top level system

               Entity_Name := No_Name;
            else
               Entity_Name :=
                 Compute_Absolute_Name_Of_Entity
                   (Parent_Subcomponent (Entity),
                    Actual_Separator);
            end if;

         when K_Subcomponent_Instance     |
           K_Feature_Instance             |
           K_Port_Spec_Instance           |
           K_Subprogram_Spec_Instance     |
           K_Parameter_Instance           |
           K_Subcomponent_Access_Instance |
           K_Connection_Instance          |
           K_Call_Sequence_Instance       =>
            Parent_Name :=
              Compute_Absolute_Name_Of_Entity
                (Parent_Component (Entity),
                 Actual_Separator);

            if Parent_Name /= No_Name then
               Get_Name_String (Parent_Name);
               Get_Name_String_And_Append (Actual_Separator);
               Get_Name_String_And_Append (Get_Name_Of_Entity (Entity));
            else
               Get_Name_String (Get_Name_Of_Entity (Entity));
            end if;

            Entity_Name := Name_Find;

         when others =>
            Entity_Name := No_Name;
      end case;

      return Entity_Name;
   end Compute_Absolute_Name_Of_Entity;

   -------------------------
   -- Is_Defined_Property --
   -------------------------

   function Is_Defined_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
   begin
      return Present
          (AIEP.Find_Property_Association_From_Name
             (Property_List => AIN.Properties (Entity),
              Property_Name => Name,
              In_Mode       => In_Mode));
   end Is_Defined_Property;

   function Is_Defined_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Name2 : Name_Id;
   begin
      Set_Str_To_Name_Buffer (Name);
      Name2 := Name_Find;

      return Is_Defined_Property (Entity, Name2, In_Mode);
   end Is_Defined_Property;

   ----------------------------
   -- Compute_Property_Value --
   ----------------------------

   function Compute_Property_Value (Property_Value : Node_Id) return Node_Id is
      pragma Assert (ATN.Kind (Property_Value) = K_Property_Value);

      Property_Expression : Node_Id;

   begin
      if Expanded_Single_Value (Property_Value) /= No_Node then
         Property_Expression := Expanded_Single_Value (Property_Value);

      elsif Expanded_Multi_Value (Property_Value) /= No_List then
         Property_Expression :=
           ATN.First_Node (Expanded_Multi_Value (Property_Value));

      elsif Single_Value (Property_Value) /= No_Node then
         Property_Expression := Single_Value (Property_Value);

      elsif Multi_Value (Property_Value) /= No_List then
         Property_Expression := ATN.First_Node (Multi_Value (Property_Value));

      else
         Property_Expression := No_Node;
      end if;

      return Property_Expression;
   end Compute_Property_Value;

   ------------------------------
   -- Get_Property_Association --
   ------------------------------

   function Get_Property_Association
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
   begin
      return AIEP.Find_Property_Association_From_Name
          (Property_List => AIN.Properties (Entity),
           Property_Name => Name,
           In_Mode       => In_Mode);
   end Get_Property_Association;

   --------------------------
   -- Get_Boolean_Property --
   --------------------------

   function Get_Boolean_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) = PT_Boolean then
            return Get_Boolean_Of_Property_Value (Property_Value);
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Get_Boolean_Property;

   ------------------------------
   -- Get_Enumeration_Property --
   ------------------------------

   function Get_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return String
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) =
           PT_Enumeration
         then
            return Get_Enumeration_Of_Property_Value (Property_Value);
         else
            return "";
         end if;
      else
         return "";
      end if;
   end Get_Enumeration_Property;

   function Get_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Name_Id
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) =
           PT_Enumeration
         then
            return Get_Enumeration_Of_Property_Value (Property_Value);
         else
            return No_Name;
         end if;
      else
         return No_Name;
      end if;
   end Get_Enumeration_Property;

   ------------------------
   -- Get_Float_Property --
   ------------------------

   function Get_Float_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Long_Long_Float
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) = PT_Float
           or else
             Get_Type_Of_Property_Value (Property_Value, True) =
             PT_Unsigned_Float
         then
            return Get_Float_Of_Property_Value (Property_Value);
         else
            return 0.0;
         end if;
      else
         return 0.0;
      end if;
   end Get_Float_Property;

   --------------------------
   -- Get_Integer_Property --
   --------------------------

   function Get_Integer_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Unsigned_Long_Long
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) = PT_Integer
           or else
             Get_Type_Of_Property_Value (Property_Value, True) =
             PT_Unsigned_Integer
         then
            return Get_Integer_Of_Property_Value (Property_Value);
         else
            return 0;
         end if;
      else
         return 0;
      end if;
   end Get_Integer_Property;

   --------------------------
   -- Get_Integer_Property --
   --------------------------

   function Get_Integer_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Unsigned_Long_Long
   is
      Property_Name : Name_Id;
   begin
      Property_Name := Get_String_Name (Name);
      return Get_Integer_Property (Entity, Property_Name, In_Mode);
   end Get_Integer_Property;

   ----------------------------
   -- Get_Reference_Property --
   ----------------------------

   function Get_Reference_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) =
           PT_Reference
         then
            return Get_Reference_Of_Property_Value (Property_Value);
         else
            return No_Node;
         end if;
      else
         return No_Node;
      end if;
   end Get_Reference_Property;

   -----------------------------
   -- Get_Classifier_Property --
   -----------------------------

   function Get_Classifier_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) =
           PT_Classifier
         then
            return Get_Classifier_Of_Property_Value (Property_Value);
         else
            return No_Node;
         end if;
      else
         return No_Node;
      end if;
   end Get_Classifier_Property;

   function Get_Classifier_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
      N : Name_Id;
   begin
      N := Get_String_Name (Name);
      return Get_Classifier_Property (Entity, N, In_Mode);
   end Get_Classifier_Property;

   ------------------------
   -- Get_Range_Property --
   ------------------------

   function Get_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
      Property : constant Node_Id :=
        Get_Property_Association (Entity, Name, In_Mode);

   begin
      if No (Property)
        or else Get_Type_Of_Property
          (ATE.Get_Referenced_Entity
             (AIN.Property_Name (Property))) /= PT_Range
      then
         return No_Node;
      end if;

      return Single_Value (AIN.Property_Association_Value (Property));
   end Get_Range_Property;

   -----------------------
   -- Get_List_Property --
   -----------------------

   function Get_List_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return List_Id
   is
      Property : constant Node_Id :=
        Get_Property_Association (Entity, Name, In_Mode);
   begin
      if No (Property)
        or else not Type_Of_Property_Is_A_List
          (ATE.Get_Referenced_Entity (AIN.Property_Name (Property)))
      then
         return No_List;
      end if;

      if No (Expanded_Multi_Value (AIN.Property_Association_Value (Property)))
        or else
        (Present
           (Expanded_Multi_Value (AIN.Property_Association_Value (Property)))
           and then No
           (ATN.First_Node
              (Expanded_Multi_Value
                 (AIN.Property_Association_Value (Property)))))
      then
         return Multi_Value (AIN.Property_Association_Value (Property));
      end if;

      return Expanded_Multi_Value (AIN.Property_Association_Value (Property));
   end Get_List_Property;

   function Get_List_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return List_Id
   is
   begin
      return Get_List_Property (Entity, Get_String_Name (Name), In_Mode);
   end Get_List_Property;

   -------------------------
   -- Get_String_Property --
   -------------------------

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return String
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) = PT_String then
            return Get_String_Of_Property_Value (Property_Value);
         else
            return "";
         end if;
      else
         return "";
      end if;
   end Get_String_Property;

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Name_Id
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      if Property_Value /= No_Node then
         if Get_Type_Of_Property_Value (Property_Value, True) = PT_String then
            return Get_String_Of_Property_Value (Property_Value);
         else
            return No_Name;
         end if;
      else
         return No_Name;
      end if;
   end Get_String_Property;

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Name_Id
   is
      Name2 : Name_Id;
   begin
      Set_Str_To_Name_Buffer (Name);
      Name2 := Name_Find;

      return Get_String_Property (Entity, Name2, In_Mode);
   end Get_String_Property;

   ---------------------------------------
   -- Get_Value_Of_Property_Association --
   ---------------------------------------

   function Get_Value_Of_Property_Association
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
      Property : constant Node_Id :=
        Get_Property_Association (Entity, Name, In_Mode);
      Value : Node_Id := No_Node;

   begin
      if Present (Property) then
         Value :=
           Compute_Property_Value (AIN.Property_Association_Value (Property));
      end if;
      return Value;
   end Get_Value_Of_Property_Association;

   ---------------------------------
   -- Is_Defined_Boolean_Property --
   ---------------------------------

   function Is_Defined_Boolean_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property_Value)
        and then
          Get_Type_Of_Property_Value (Property_Value, True) =
          PT_Boolean;
   end Is_Defined_Boolean_Property;

   -------------------------------------
   -- Is_Defined_Enumeration_Property --
   -------------------------------------

   function Is_Defined_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property_Value)
        and then
          Get_Type_Of_Property_Value (Property_Value, True) =
          PT_Enumeration;
   end Is_Defined_Enumeration_Property;

   -------------------------------
   -- Is_Defined_Float_Property --
   -------------------------------

   function Is_Defined_Float_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property_Value)
        and then
        (Get_Type_Of_Property_Value (Property_Value, True) = PT_Float
         or else
           Get_Type_Of_Property_Value (Property_Value, True) =
           PT_Unsigned_Float);
   end Is_Defined_Float_Property;

   ---------------------------------
   -- Is_Defined_Integer_Property --
   ---------------------------------

   function Is_Defined_Integer_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property_Value)
        and then
        (Get_Type_Of_Property_Value (Property_Value, True) = PT_Integer
         or else
           Get_Type_Of_Property_Value (Property_Value, True) =
           PT_Unsigned_Integer);
   end Is_Defined_Integer_Property;

   -----------------------------------
   -- Is_Defined_Reference_Property --
   -----------------------------------

   function Is_Defined_Reference_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property_Value)
        and then
          Get_Type_Of_Property_Value (Property_Value, True) =
          PT_Reference;
   end Is_Defined_Reference_Property;

   -------------------------------
   -- Is_Defined_Range_Property --
   -------------------------------

   function Is_Defined_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property : constant Node_Id :=
        Get_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property)
        and then
          Get_Type_Of_Property_Value
            (AIN.Property_Association_Value (Property),
             True) =
          PT_Range;
   end Is_Defined_Range_Property;

   --------------------------------
   -- Is_Defined_Record_Property --
   --------------------------------

   function Is_Defined_Record_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property : constant Node_Id
        := Get_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property)
        and then
        Get_Type_Of_Property_Value
        (ATN.Property_Association_Value
           (AIN.Corresponding_Declaration (Property)),
         True) = PT_Record;
   end Is_Defined_Record_Property;

   ------------------------------------
   -- Is_Defined_Classifier_Property --
   ------------------------------------

   function Is_Defined_Classifier_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

   begin
      return Present (Property_Value)
        and then
          Get_Type_Of_Property_Value (Property_Value, True) =
          PT_Classifier;
   end Is_Defined_Classifier_Property;

   ------------------------------
   -- Is_Defined_List_Property --
   ------------------------------

   function Is_Defined_List_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property : constant Node_Id :=
        Get_Property_Association (Entity, Name, In_Mode);
   begin
      return Present (Property)
        and then Type_Of_Property_Is_A_List
          (ATE.Get_Referenced_Entity (AIN.Property_Name (Property)));
   end Is_Defined_List_Property;

   function Is_Defined_List_Property
     (Entity  : Node_Id;
      Name    : String;
      In_Mode : Name_Id := No_Name) return Boolean
   is
   begin
      return Is_Defined_Property (Entity, Get_String_Name (Name), In_Mode);
   end Is_Defined_List_Property;

   --------------------------------
   -- Is_Defined_String_Property --
   --------------------------------

   function Is_Defined_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : constant Node_Id :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);
   begin
      return Present (Property_Value)
        and then Get_Type_Of_Property_Value (Property_Value, True) = PT_String;
   end Is_Defined_String_Property;

end Ocarina.Instances.Queries;
