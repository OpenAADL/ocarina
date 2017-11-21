------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . A N A L Y Z E R . A A D L . Q U E R I E S         --
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

with GNAT.Table;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
with Locations;
with Ocarina.Annotations;

package body Ocarina.Analyzer.AADL.Queries is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Tree.Nodes;
   use Ocarina.ME_AADL.AADL_Tree.Entities;
   use Ocarina.ME_AADL.AADL_Tree.Entities.Properties;
   use Ocarina.Annotations;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;

   function Get_Category_Of_Entity (Entity : Node_Id) return Named_Element;

   ----------------------------
   -- Compute_Property_Value --
   ----------------------------

   function Compute_Property_Value (Property_Value : Node_Id) return Node_Id is

      pragma Assert (Kind (Property_Value) = K_Property_Value);

      Property_Expression : Node_Id;
   begin
      if Expanded_Single_Value (Property_Value) /= No_Node then
         Property_Expression := Expanded_Single_Value (Property_Value);
      elsif Expanded_Multi_Value (Property_Value) /= No_List then
         Property_Expression :=
           First_Node (Expanded_Multi_Value (Property_Value));
      elsif Single_Value (Property_Value) /= No_Node then
         Property_Expression := Single_Value (Property_Value);
      elsif Multi_Value (Property_Value) /= No_List then
         Property_Expression := First_Node (Multi_Value (Property_Value));
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
      return Find_Property_Association_From_Name
          (Property_List => ATN.Properties (Entity),
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

   ----------------------------
   -- Get_Category_Of_Entity --
   ----------------------------

   function Get_Category_Of_Entity (Entity : Node_Id) return Named_Element is
      pragma Assert (Present (Entity));

   begin
      case Kind (Entity) is
         when K_Component_Type        |
           K_Component_Implementation |
           K_Subcomponent             |
           K_Subcomponent_Access      =>
            return PO_Component_Category;

         when K_Mode =>
            return PO_Mode;

         when K_Feature_Group_Type =>
            if AADL_Version = AADL_V1 then
               return PO_Port_Group;
            else
               return PO_Feature_Group;
            end if;

         when K_Flow_Spec                   |
           K_Flow_Implementation            |
           K_End_To_End_Flow_Spec           |
           K_Flow_Implementation_Refinement |
           K_End_To_End_Flow_Refinement     =>
            return PO_Flow;

         when K_Port_Spec =>
            if Is_Event (Entity) and then Is_Data (Entity) then
               return PO_Event_Data_Port;
            elsif Is_Event (Entity) then
               return PO_Event_Port;
            else
               return PO_Data_Port;
            end if;

         when K_Parameter =>
            return PO_Parameter;

         when K_Subprogram_Spec =>
            if Is_Server (Entity) then
               return PO_Server_Subprogram;
            else
               return PO_Component_Category;
            end if;

         when K_Subprogram_Call =>
            return PO_Component_Category;

         when K_Connection =>
            case Get_Category_Of_Connection (Entity) is
               when CT_Error =>
                  raise Program_Error;
               when CT_Port_Connection =>
                  return PO_Data_Port_Connections;
               --  XXX Incomplete TODO
               when CT_Data | CT_Data_Delayed =>
                  return PO_Data_Port_Connections;
               when CT_Event =>
                  return PO_Event_Port_Connections;
               when CT_Event_Data =>
                  return PO_Event_Data_Port_Connections;
               when CT_Parameter =>
                  return PO_Parameter_Connections;
               when CT_Feature =>
                  return PO_Feature;
               when CT_Feature_Group =>
                  case AADL_Version is
                     when AADL_V1 =>
                        return PO_Port_Group_Connections;
                     when AADL_V2 =>
                        return PO_Feature_Group_Connection;
                  end case;
               when CT_Access_Bus           |
                 CT_Access_Data             |
                 CT_Access_Subprogram       |
                 CT_Access_Subprogram_Group |
                 CT_Access_Virtual_Bus      |
                 CT_Access                  =>
                  return PO_Access_Connection;
            end case;

         when others =>
            raise Program_Error
              with "Invalid kind " &
              Kind (Entity)'Img &
              " at " &
              Locations.Image (Loc (Entity));
      end case;
   end Get_Category_Of_Entity;

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
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

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
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

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
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

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

   ------------------------
   -- Get_Range_Property --
   ------------------------

   function Get_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
      Property : constant Node_Id :=
        Find_Property_Association_From_Name
          (Property_List => ATN.Properties (Entity),
           Property_Name => Name,
           In_Mode       => In_Mode);
   begin

      if Property = No_Node then
         return No_Node;
      end if;

      if Get_Type_Of_Property (Property) /= PT_Range then
         return No_Node;
      end if;

      return Expanded_Single_Value (Property_Association_Value (Property));
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
        Find_Property_Association_From_Name
          (Property_List => ATN.Properties (Entity),
           Property_Name => Name,
           In_Mode       => In_Mode);
   begin
      if No (Property)
        or else not Type_Of_Property_Is_A_List
          (Get_Referenced_Entity (Property_Name (Property)))
      then
         return No_List;
      end if;

      return Expanded_Multi_Value (Property_Association_Value (Property));
   end Get_List_Property;

   -------------------------
   -- Get_String_Property --
   -------------------------

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return String
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

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

   -------------------------
   -- Get_String_Property --
   -------------------------

   function Get_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Name_Id
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

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

   ---------------------------------------
   -- Get_Value_Of_Property_Association --
   ---------------------------------------

   function Get_Value_Of_Property_Association
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Node_Id
   is
      Property : constant Node_Id :=
        Find_Property_Association_From_Name
          (Property_List => ATN.Properties (Entity),
           Property_Name => Name,
           In_Mode       => In_Mode);
   begin
      if Property /= No_Node then
         return Compute_Property_Value (Property_Association_Value (Property));
      else
         return No_Node;
      end if;
   end Get_Value_Of_Property_Association;

   ---------------------
   -- Is_An_Extension --
   ---------------------

   function Is_An_Extension
     (Component : Node_Id;
      Ancestor  : Node_Id) return Boolean
   is
      pragma Assert
        (Kind (Component) = K_Component_Implementation
         or else Kind (Component) = K_Component_Type
         or else Kind (Component) = K_Feature_Group_Type);

      pragma Assert
        (No (Ancestor)
         or else Kind (Ancestor) = K_Component_Implementation
         or else Kind (Ancestor) = K_Component_Type
         or else Kind (Ancestor) = K_Feature_Group_Type);

      Temp_Node : Node_Id := Component;
      Type_Node : Node_Id := Component;
   begin
      if Ancestor = No_Node then
         return False;
      end if;

      while Temp_Node /= No_Node loop
         if Temp_Node = Ancestor then
            return True;

         elsif Kind (Component) = K_Component_Implementation then
            Type_Node :=
              Corresponding_Entity (Component_Type_Identifier (Temp_Node));

            while Type_Node /= No_Node loop
               if Type_Node = Ancestor then
                  return True;
               end if;

               if Parent (Type_Node) /= No_Node then
                  Type_Node := Get_Referenced_Entity (Parent (Type_Node));
               else
                  Type_Node := No_Node;
               end if;
            end loop;
         end if;

         if Parent (Temp_Node) /= No_Node then
            Temp_Node := Get_Referenced_Entity (Parent (Temp_Node));
         else
            Temp_Node := No_Node;
         end if;
      end loop;

      return False;
   end Is_An_Extension;

   ---------------------------------
   -- Is_Defined_Boolean_Property --
   ---------------------------------

   function Is_Defined_Boolean_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      return Present (Property_Value)
        and then
        (Get_Type_Of_Property_Value (Property_Value, True) = PT_Boolean);
   end Is_Defined_Boolean_Property;

   -------------------------------------
   -- Is_Defined_Enumeration_Property --
   -------------------------------------

   function Is_Defined_Enumeration_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

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
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

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
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      return Present (Property_Value)
        and then
        (Get_Type_Of_Property_Value (Property_Value, True) = PT_Integer
         or else
           Get_Type_Of_Property_Value (Property_Value, True) =
           PT_Unsigned_Integer);
   end Is_Defined_Integer_Property;

   -------------------------
   -- Is_Defined_Property --
   -------------------------

   function Is_Defined_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
   begin
      --  A property is defined if it exists and has a value

      return Get_Value_Of_Property_Association (Entity, Name, In_Mode) /=
        No_Node;
   end Is_Defined_Property;

   -----------------------------------
   -- Is_Defined_Reference_Property --
   -----------------------------------

   function Is_Defined_Reference_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      return Present (Property_Value)
        and then
          Get_Type_Of_Property_Value (Property_Value, True) =
          PT_Reference;
   end Is_Defined_Reference_Property;

   function Is_Defined_Classifier_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      return Present (Property_Value)
        and then
          Get_Type_Of_Property_Value (Property_Value, True) =
          PT_Classifier;
   end Is_Defined_Classifier_Property;

   -------------------------------
   -- Is_Defined_Range_Property --
   -------------------------------

   function Is_Defined_Range_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property : constant Node_Id :=
        Find_Property_Association_From_Name
          (Property_List => ATN.Properties (Entity),
           Property_Name => Name,
           In_Mode       => In_Mode);
   begin
      return Present (Property)
        and then
          Get_Type_Of_Property_Value
            (Property_Association_Value (Property),
             True) =
          PT_Range;
   end Is_Defined_Range_Property;

   ------------------------------
   -- Is_Defined_List_Property --
   ------------------------------

   function Is_Defined_List_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      Property : constant Node_Id :=
        Find_Property_Association_From_Name
          (Property_List => ATN.Properties (Entity),
           Property_Name => Name,
           In_Mode       => In_Mode);
   begin
      return Present (Property)
        and then Type_Of_Property_Is_A_List
          (Get_Referenced_Entity (Property_Name (Property)));
   end Is_Defined_List_Property;

   --------------------------------
   -- Is_Defined_String_Property --
   --------------------------------

   function Is_Defined_String_Property
     (Entity  : Node_Id;
      Name    : Name_Id;
      In_Mode : Name_Id := No_Name) return Boolean
   is
      pragma Assert (Present (Entity));

      Property_Value : Node_Id;
   begin
      Property_Value :=
        Get_Value_Of_Property_Association (Entity, Name, In_Mode);

      return Present (Property_Value)
        and then Get_Type_Of_Property_Value (Property_Value, True) = PT_String;
   end Is_Defined_String_Property;

   ---------------
   -- Needed_By --
   ---------------

   function Needed_By (N : Node_Id; Entity : Node_Id) return Boolean is
      --  During the expansion phase, each component or property
      --  declaration is annotated with the components that need
      --  it. For exemple each AADL component C corresponding to a
      --  subcomponent S of another component D will be annotated with
      --  D. Therefore, D "needs" C.

      --  To see whether N is needed by Entity, we loop through all
      --  the annotations of N and see whether one of them is eqaul to
      --  Entity or if it is (recursively) "needed" by Entity.

      --  To avoid infinite recursion (that may occurs between data
      --  having a subprogram as a feature and a being access-required
      --  by the same subprogram), we build a list of the visited
      --  nodes to guarantee they are checked only once.

      package Checked_Nodes is new GNAT.Table (Node_Id, Nat, 1, 10, 50);
      --  This is the list of the already checked "nodes"

      function Is_Checked (E : Node_Id) return Boolean;
      --  See whether E has already been ckecked or not

      procedure Set_Checked (E : Node_Id);
      --  Append E to the checked node table

      function Need (E : Node_Id) return Boolean;
      --  This is the Actual recursive routine. It returns True when
      --  Entity is an annotation of E or else whether it is "needs"
      --  one of the annotations of E.

      ----------------
      -- Is_Checked --
      ----------------

      function Is_Checked (E : Node_Id) return Boolean is
         use Checked_Nodes;
      begin
         for J in First .. Last loop
            if Table (J) = E then
               return True;
            end if;
         end loop;

         return False;
      end Is_Checked;

      -----------------
      -- Set_Checked --
      -----------------

      procedure Set_Checked (E : Node_Id) is
         use Checked_Nodes;
      begin
         Append (E);
      end Set_Checked;

      ----------
      -- Need --
      ----------

      function Need (E : Node_Id) return Boolean is
         A      : Annotation_Id;
         Result : Boolean;

      begin
         Set_Checked (E);

         A := First_Annotation (E);
         if A = No_Annotation then
            Result := False;

         elsif Annotation_Index (E, Entity) /= 0 then
            Result := True;

         else
            Result := False;
            while A /= No_Annotation loop
               if not Is_Checked (Annotation_Node (A)) then
                  Result := Need (Annotation_Node (A));
                  exit when Result;
               end if;

               A := Next_Annotation (A);
            end loop;
         end if;

         return Result;
      end Need;

      Result : Boolean;
   begin
      --  Always return true when asked for a dependency upon a
      --  property declaration.

      Result :=
        Kind (N) = K_Property_Definition_Declaration
        or else Kind (N) = K_Property_Type_Declaration
        or else Kind (N) = K_Constant_Property_Declaration
        or else Need (N);

      --  Deallocate the checked nodes list

      Checked_Nodes.Free;

      return Result;
   end Needed_By;

   ----------------------------------
   -- Property_Can_Apply_To_Entity --
   ----------------------------------

   function Property_Can_Apply_To_Entity
     (Property : Node_Id;
      Entity   : Node_Id) return Boolean
   is
      pragma Assert (Kind (Property) = K_Property_Association);
      pragma Assert (Present (Entity));

      Property_Name : constant Node_Id :=
        Get_Referenced_Entity
          (Ocarina.ME_AADL.AADL_Tree.Nodes.Property_Name (Property));

      pragma Assert
        (Is_All (Applies_To (Property_Name))
         or else Owner_Categories (Applies_To (Property_Name)) /= No_List);

      List_Node                  : Node_Id := No_Node;
      Corresponding_Component    : Node_Id := No_Node;
      Category_Of_Property_Owner : Named_Element;
      Category_Of_Component      : Component_Category;
      Success                    : Boolean;
      Can_Apply                  : Boolean;
   begin
      if Is_All (Applies_To (Property_Name)) then
         Success := True;

      else
         Success                    := False;
         Category_Of_Property_Owner := Get_Category_Of_Entity (Entity);

         --  Get the corresponding component, for some tests

         if Kind (Entity) = K_Subcomponent then
            Corresponding_Component := Get_Corresponding_Component (Entity);

         elsif Kind (Entity) = K_Subprogram_Call then
            Corresponding_Component := Get_Corresponding_Subprogram (Entity);

         else
            Corresponding_Component := Entity;
         end if;

         --  Get the category of the entity

         if Kind (Entity) = K_Component_Type
           or else Kind (Entity) = K_Component_Implementation
         then
            Category_Of_Component := Get_Category_Of_Component (Entity);

         elsif Kind (Entity) = K_Subprogram_Spec
           and then not Is_Server (Entity)
         then
            Category_Of_Component := CC_Subprogram;

         elsif Kind (Entity) = K_Subprogram_Call then
            Category_Of_Component := CC_Subprogram;

         elsif Kind (Entity) = K_Subcomponent then
            Category_Of_Component := Get_Category_Of_Subcomponent (Entity);

         elsif Kind (Entity) = K_Subcomponent_Access then
            Category_Of_Component :=
              Component_Category'Val (Subcomponent_Category (Entity));
         end if;

         if AADL_Version = AADL_V1 then
            --  AADLV1 only: access property names can be applied to
            --  subcomponent accesses.

            if Kind (Entity) = K_Subcomponent_Access
              and then not Is_Access (Property_Name)
            then
               --  No need to go further, it cannot apply

               return False;
            end if;
         end if;

         --  Check if the property can be applied to the entity

         List_Node :=
           First_Node (Owner_Categories (Applies_To (Property_Name)));

         while List_Node /= No_Node and then not Success loop
            case Category_Of_Property_Owner is
               when PO_Component_Category =>
                  --  Here, we check that the component (or an access
                  --  to this component) matches one of the
                  --  corresponding component types or meta model
                  --  elements in the 'applies to'

                  Can_Apply :=
                    ((Named_Element'Val (Category (List_Node)) =
                      PO_Component_Category
                      or else
                        Named_Element'Val (Category (List_Node)) =
                        PO_Component_Access
                        or else
                        Named_Element'Val (Category (List_Node)) =
                        PO_Component_Implementation)
                     and then
                     (Category_Of_Component =
                      Component_Category'Val (Component_Cat (List_Node))))
                    or else Category_Of_Component = CC_Abstract;

                  --  if not, then we check that the kind of entity
                  --  matches one particular meta-model element

                  if not Can_Apply then
                     Can_Apply :=
                       Kind (Entity) = K_Subcomponent
                       and then
                         Named_Element'Val (Category (List_Node)) =
                         PO_Subcomponent;
                  end if;

                  --  XXX dubious, here we erase the previously
                  --  computed value of Can_Apply, to be investigated.

                  if Present (Classifier_Ref (List_Node)) then
                     if Present (Corresponding_Component) then
                        Can_Apply :=
                          Is_An_Extension
                            (Component => Corresponding_Component,
                             Ancestor  =>
                               Get_Referenced_Entity
                                 (Classifier_Ref (List_Node)));
                     else
                        Can_Apply := False;
                     end if;
                  end if;

               when PO_Event_Data_Port |
                 PO_Event_Port         |
                 PO_Data_Port          |
                 PO_Port               =>
                  Can_Apply :=
                    Named_Element'Val (Category (List_Node)) =
                    Category_Of_Property_Owner
                    or else Named_Element'Val (Category (List_Node)) = PO_Port
                    or else Named_Element'Val (Category (List_Node))
                    = PO_Feature;

               when PO_Data_Port_Connections    |
                 PO_Event_Port_Connections      |
                 PO_Event_Data_Port_Connections =>

                  Can_Apply :=
                    Named_Element'Val (Category (List_Node)) =
                    Category_Of_Property_Owner
                    or else
                      Named_Element'Val (Category (List_Node)) =
                      PO_Port_Connections
                    or else
                      Named_Element'Val (Category (List_Node)) =
                      PO_Port_Connection
                    or else
                      Named_Element'Val (Category (List_Node)) =
                      PO_Connection
                    or else
                      Named_Element'Val (Category (List_Node)) =
                      PO_Connections;

               when PO_Parameter_Connections |
                 PO_Feature_Group_Connection |
                 PO_Port_Group_Connections   |
                 PO_Access_Connection        |
                 PO_Connection               |
                 PO_Connections              =>
                  Can_Apply :=
                    Named_Element'Val (Category (List_Node)) =
                    Category_Of_Property_Owner
                    or else
                      Named_Element'Val (Category (List_Node)) =
                      PO_Connection
                    or else
                      Named_Element'Val (Category (List_Node)) =
                      PO_Connections;

               when others =>
                  Can_Apply :=
                    Named_Element'Val (Category (List_Node)) =
                    Category_Of_Property_Owner;
            end case;

            Success   := Success or else Can_Apply;
            List_Node := Next_Node (List_Node);
         end loop;
      end if;

      return Success;
   end Property_Can_Apply_To_Entity;

end Ocarina.Analyzer.AADL.Queries;
