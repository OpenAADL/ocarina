------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.REPLICATION_PROPERTIES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2016-2018 ESA & ISAE.                    --
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

with Ocarina.ME_AADL;
with Ocarina.AADL_Values;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Nutils;
with Ocarina.Analyzer.AADL.Queries;
with Ocarina.Me_AADL.AADL_Tree.Entities.Properties;

package body Ocarina.Backends.Replication_Properties is

   use Ocarina.Namet;
   use Utils;

   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Nutils;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.Analyzer.AADL.Queries;
   use Ocarina.Me_AADL.AADL_Tree.Entities.Properties;

   ----------------------------
   -- Replication properties --
   ----------------------------

   Replication_Description                          : Name_Id;
   Replication_Replica_Number                       : Name_Id;
   Replication_Min_Nbr_Replica                      : Name_Id;
   Replication_Max_Nbr_Replica                      : Name_Id;
   Replication_Replica_Identifiers                  : Name_Id;
   Replication_Replica_Type                         : Name_Id;
   Replication_Active_Name                          : Name_Id;
   Replication_Passive_Name                         : Name_Id;
   Replication_Consensus_Algorithm_Source_Text_Name : Name_Id;
   Replication_Consensus_Algorithm_Ref_Name         : Name_Id;
   Replication_Consensus_Algorithm_Class_Name       : Name_Id;

   ----------
   -- Init --
   ----------

   procedure Init is

   begin

      Replication_Description := Get_String_Name
                              ("Replication_Properties::Description");
      Replication_Replica_Number := Get_String_Name
                              ("Replication_Properties::Replica_Number");
      Replication_Min_Nbr_Replica := Get_String_Name
                              ("Replication_Properties::Min_Nbr_Replica");
      Replication_Max_Nbr_Replica := Get_String_Name
                              ("Replication_Properties::Max_Nbr_Replica");
      Replication_Replica_Identifiers := Get_String_Name
                              ("Replication_Properties::Replica_Identifiers");
      Replication_Active_Name := Get_String_Name ("active");
      Replication_Passive_Name := Get_String_Name ("passive");
      Replication_Replica_Type := Get_String_Name ("replica_type");
      Replication_Consensus_Algorithm_Source_Text_Name := Get_String_Name
              ("consensus_algorithm_source_text");
      Replication_Consensus_Algorithm_Class_Name := Get_String_Name
              ("consensus_algorithm_class");
      Replication_Consensus_Algorithm_Ref_Name := Get_String_Name
              ("consensus_algorithm_ref");
   end Init;

   ----------------------------
   -- Replication_Properties --
   ----------------------------
   ---------------------------------
   -- Get_Replication_Description --
   ---------------------------------

   function Get_Replication_Description (C : Node_Id)
      return Name_Id
   is
   begin
      if Is_Defined_String_Property (C, Replication_Description) then
         return Get_String_Property (C, Replication_Description);
      else
         return No_Name;
      end if;
   end Get_Replication_Description;

   function Get_Replication_Description
     (Property_Node    : Node_Id;
      Applies_To_Node  : Node_Id)
      return String
   is
      pragma Assert (Kind (Property_Node)   = K_Property_association);
      pragma Assert (Kind (Applies_To_Node) = K_SUBCOMPONENT);
      Property_Expression : Node_Id;
      Property_Name       : Name_Id;
      Property_Value      : Node_Id;
      Applies_To_List     : List_Id;
      Entity_Of_Property  : Node_Id;
      Pointed_Node        : Node_Id := No_Node;
      Prefix_Name         : constant Name_Id :=
        Get_String_Name ("Replication_Properties::Description");
   begin
      Property_Name := Get_Name_Of_Entity (Property_Node, true);
      Property_Value :=  Property_Association_Value (Property_Node);
      Property_Expression := Compute_Property_Value (Property_Value);
      Applies_To_List := List_Items (First_Node
                      (Applies_To_Prop (Property_Node)));
      if not Is_Empty (Applies_To_List) then
         Pointed_Node := First_Node (Applies_To_List);
         if Kind (Pointed_Node) = K_Array_Selection then
            Entity_Of_Property := Corresponding_Entity
                     (Identifier (Pointed_Node));
         else
            Entity_Of_Property := Corresponding_Entity
                                  (Pointed_Node);
         end if;
      end if;
      if kind (Property_Expression) = K_LITERAL and then
         Is_Prefix (Prefix_Name, Property_Name) and then
        Entity_Of_Property = Applies_To_Node
      then
         return Ocarina.AADL_Values.Image
                (Value (Property_Expression));
      else
         return "";
      end if;
   end Get_Replication_Description;

   ------------------------------------
   -- Get_Replication_Replica_Number --
   ------------------------------------

   function Get_Replication_Replica_Number (C : Node_Id)
      return Unsigned_Long_Long is
   begin
      if Is_Defined_Integer_Property (C, Replication_Replica_Number) then
         return Get_Integer_Property (C, Replication_Replica_Number);
      else
         return 0;
      end if;
   end Get_Replication_Replica_Number;

   ------------------------------------
   -- Get_Replication_Replica_Number --
   ------------------------------------

   function Get_Replication_Replica_Number
      (Property_Node   : Node_Id;
       Applies_To_Node : Node_Id)
      return Unsigned_Long_Long
   is
      pragma Assert (Kind (Property_Node)   = K_Property_association);
      pragma Assert (Kind (Applies_To_Node) = K_SUBCOMPONENT);
      Property_Expression : Node_Id;
      Property_Name       : Name_Id;
      Property_Value      : Node_Id;
      Applies_To_List     : List_Id;
      Entity_Of_Property  : Node_Id;
      Pointed_Node        : Node_Id := No_Node;
      Prefix_Name         : constant Name_Id :=
        Get_String_Name ("Replication_Properties::Replica_Number");
   begin
      Property_Name := Get_Name_Of_Entity (Property_Node, true);
      Property_Value :=  Property_Association_Value (Property_Node);
      Property_Expression := Compute_Property_Value (Property_Value);
      Applies_To_List := List_Items (First_Node
                      (Applies_To_Prop (Property_Node)));
      if not Is_Empty (Applies_To_List) then
         Pointed_Node := First_Node (Applies_To_List);
         if Kind (Pointed_Node) = K_Array_Selection then
            Entity_Of_Property := Corresponding_Entity
                     (Identifier (Pointed_Node));
         else
            Entity_Of_Property := Corresponding_Entity
                                  (Pointed_Node);
         end if;
      end if;
      if kind (Property_Expression) = K_Signed_AADLNUMBER and then
         Is_Prefix (Prefix_Name, Property_Name) and then
        Entity_Of_Property = Applies_To_Node
      then
         return Get_Integer_Of_Property_Value (Property_Expression);
      else
         return 0;
      end if;
   end Get_Replication_Replica_Number;

   -------------------------------------
   -- Get_Replication_Min_Nbr_Replica --
   -------------------------------------

   function Get_Replication_Min_Nbr_Replica (C : Node_Id)
      return Unsigned_Long_Long is
   begin
      if Is_Defined_Integer_Property (C, Replication_Min_Nbr_Replica) then
         return Get_Integer_Property (C, Replication_Min_Nbr_Replica);
      else
         return 0;
      end if;
   end Get_Replication_Min_Nbr_Replica;

   -------------------------------------
   -- Get_Replication_Max_Nbr_Replica --
   -------------------------------------

   function Get_Replication_Max_Nbr_Replica (C : Node_Id)
      return Unsigned_Long_Long is
   begin
      if Is_Defined_Integer_Property (C, Replication_Max_Nbr_Replica) then
         return Get_Integer_Property (C, Replication_Max_Nbr_Replica);
      else
         return 0;
      end if;
   end Get_Replication_Max_Nbr_Replica;

   -----------------------------------------
   -- Get_Replication_Replica_Identifiers --
   -----------------------------------------

   function Get_Replication_Replica_Identifiers (C : Node_Id)
      return List_Id is
      L : List_Id;
   begin
      if Is_Defined_Property (C, Replication_Replica_Identifiers) then
         L := Get_List_Property (C, Replication_Replica_Identifiers);
         if L /= No_List then
            return L;
         else
            return No_List;
         end if;
      else
            return No_List;
      end if;
   end Get_Replication_Replica_Identifiers;

   -----------------------------------------
   -- Get_Replication_Replica_Identifiers --
   -----------------------------------------

   function Get_Replication_Replica_Identifiers
   (Property_Node   : Node_Id;
    Applies_To_Node : Node_Id)
   return List_Id
   is
      pragma Assert (Kind (Property_Node)   = K_Property_association);
      pragma Assert (Kind (Applies_To_Node) = K_SUBCOMPONENT);
      Property_Name       : Name_Id;
      Property_Value      : Node_Id;
      Applies_To_List     : List_Id;
      Entity_Of_Property  : Node_Id;
      Pointed_Node        : Node_Id := No_Node;
      Prefix_Name         : constant Name_Id :=
        Get_String_Name ("Replication_Properties::Replica_Identifiers");
      L : List_Id := No_List;
   begin
      Property_Name := Get_Name_Of_Entity (Property_Node, true);
      Property_Value :=  Property_Association_Value (Property_Node);
      L :=  Expanded_Multi_Value (Property_Value);
      --  to determine the node to each the property is applied.
      Applies_To_List := List_Items (First_Node
                      (Applies_To_Prop (Property_Node)));
      if not Is_Empty (Applies_To_List) then
         Pointed_Node := First_Node (Applies_To_List);
         if Kind (Pointed_Node) = K_Array_Selection then
            Entity_Of_Property := Corresponding_Entity
                     (Identifier (Pointed_Node));
         else
            Entity_Of_Property := Corresponding_Entity
                                  (Pointed_Node);
         end if;
      end if;

      if Is_Prefix (Prefix_Name, Property_Name) and then
        Entity_Of_Property = Applies_To_Node
      then
         return L;
      else
         return No_list;
      end if;
   end Get_Replication_Replica_Identifiers;

   ----------------------------------
   -- Get_Replication_Replica_Type --
   ----------------------------------

   function Get_Replication_Replica_Type (C : Node_Id)
      return Replication_Types is
      Type_Name : Name_Id;
   begin
      if Is_Defined_Enumeration_Property (C, Replication_Replica_Type) then
         Type_Name := Get_Enumeration_Property (C, Replication_Replica_Type);

         if Type_Name = Replication_Active_Name then
            return Replication_Active;

         elsif Type_Name = Replication_Passive_Name then
            return Replication_Passive;
         else
            return Replication_None;
         end if;
      else
         return Replication_None;
      end if;
   end Get_Replication_Replica_Type;

   ----------------------------------
   -- Get_Replication_Replica_Type --
   ----------------------------------

   function Get_Replication_Replica_Type
   (Property_Node   : Node_Id;
    Applies_To_Node : Node_Id)
   return Replication_Types is
   pragma Assert (Kind (Property_Node)   = K_Property_association);
      pragma Assert (Kind (Applies_To_Node) = K_SUBCOMPONENT);
      Property_Name       : Name_Id;
      Property_Value      : Node_Id;
      Applies_To_List     : List_Id;
      Entity_Of_Property  : Node_Id;
      Pointed_Node        : Node_Id := No_Node;
      Prefix_Name         : constant Name_Id :=
        Get_String_Name ("Replication_Properties::Replica_Type");
   begin
      Property_Name := Get_Name_Of_Entity (Property_Node, true);
      --  the kind of Property_Value = K_Enumeration_Term
      Property_Value := Single_Value (Property_Association_Value
                        (Property_Node));
      --  to determine the node to each the property is applied.
      Applies_To_List := List_Items (First_Node
                      (Applies_To_Prop (Property_Node)));
      if not Is_Empty (Applies_To_List) then
         Pointed_Node := First_Node (Applies_To_List);
         if Kind (Pointed_Node) = K_Array_Selection then
            Entity_Of_Property := Corresponding_Entity
                     (Identifier (Pointed_Node));
         else
            Entity_Of_Property := Corresponding_Entity
                                  (Pointed_Node);
         end if;
      end if;

      if Is_Prefix (Prefix_Name, Property_Name) and then
        Entity_Of_Property = Applies_To_Node
      then
         if Get_Enumeration_Of_Property_Value (Property_Value) = "active"
         then
            return Replication_Active;
         elsif Get_Enumeration_Of_Property_Value (Property_Value) = "passive"
         then
            return Replication_Passive;
         else
            return Replication_None;
         end if;
      else
         return Replication_None;
      end if;
   end Get_Replication_Replica_Type;

   -----------------------------------------
   -- Get_Replication_Consensus_Algorithm --
   -----------------------------------------

   function Get_Replication_Consensus_Algorithm
     (C       : Node_Id;
      In_Mode : Name_Id := No_Name)
     return Name_Id
   is
   begin
      if Is_Defined_String_Property
        (C, Replication_Consensus_Algorithm_Source_Text_Name, In_Mode)
      then
         return Get_String_Property
           (C, Replication_Consensus_Algorithm_Source_Text_Name, In_Mode);
      else
         return No_Name;
      end if;
   end Get_Replication_Consensus_Algorithm;

   -----------------------------------------
   -- Get_Replication_Consensus_Algorithm --
   -----------------------------------------

   procedure Get_Replication_Consensus_Algorithm
   (Property_Node   : Node_Id;
    Consensus_Node_List : in out Node_List) is

      pragma Assert (Kind (Property_Node)   = K_Property_association);
      Property_Name       : Name_Id;
      Prefix_Name : constant Name_Id := Get_String_Name
      ("Replication_Properties::Consensus_Algorithm");
   begin
      Property_Name := Get_Name_Of_Entity (Property_Node, true);
      --  to determine the node to each the property is applied.
      if Is_Prefix (Prefix_Name, Property_Name) then
         Append_Node_To_Node_List (Property_Node,
                                  Consensus_Node_List, True);
      else
         null;
      end if;
   end Get_Replication_Consensus_Algorithm;

   -----------------------------------------
   -- Get_Replication_Consensus_Algorithm --
   -----------------------------------------

   function Get_Replication_Consensus_Algorithm
     (C       : Node_Id;
      In_Mode : Name_Id := No_Name)
     return Node_Id
   is
   begin
      --  if Kind (C) = K_Port_Spec_Instance and then not AIN.Is_Out (C) then
         --  Display_Located_Error
         --  (AIN.Loc (C),
         --  "Consensus algorithm cannot be specified for In-only ports",
         --  Fatal => True);
      --  end if;

      if Is_Defined_String_Property
        (C, Replication_Consensus_Algorithm_Source_Text_Name, In_Mode)
      then
         return Get_Property_Association
           (C, Replication_Consensus_Algorithm_Source_Text_Name, In_Mode);
      elsif Is_Defined_Reference_Property
        (C, Replication_Consensus_Algorithm_Ref_Name, In_Mode)
      then
         return Get_Property_Association
           (C, Replication_Consensus_Algorithm_Ref_Name, In_Mode);
      elsif Is_Defined_Property
        (C, Replication_Consensus_Algorithm_Class_Name, In_Mode)
      then
         return Get_Classifier_Property
            (C, Replication_Consensus_Algorithm_Class_Name, In_Mode);
      else
         return No_Node;
      end if;

   end Get_Replication_Consensus_Algorithm;

   ---------------------------------------------
   -- Is_Defined_Consensus_Algorithm_Property --
   ---------------------------------------------

   function Is_Defined_Consensus_Algorithm_Property
     (C       : Node_Id;
      In_Mode : Name_Id := No_Name)
     return Boolean
   is
   begin
      if Is_Defined_String_Property
        (C, Replication_Consensus_Algorithm_Source_Text_Name, In_Mode)
      then
         return True;
      elsif Is_Defined_Reference_Property
        (C, Replication_Consensus_Algorithm_Ref_Name, In_Mode)
      then
         return True;
      elsif Is_Defined_Property
        (C, Replication_Consensus_Algorithm_Class_Name, In_Mode)
      then
         return True;
      else
         return False;
      end if;
   end Is_Defined_Consensus_Algorithm_Property;

end Ocarina.Backends.Replication_Properties;
