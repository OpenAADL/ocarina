pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.ME_AADL.AADL_Tree.Debug; use Ocarina.ME_AADL.AADL_Tree.Debug;

package body Ocarina.ME_AADL.AADL_Tree.Nodes is

   pragma Warnings (Off);
   use Entries;

   function Kind (N : Node_Id) return Node_Kind is
   begin
      return Table (Types.Node_Id (N)).Kind;
   end Kind;

   procedure Set_Kind (N : Node_Id; V : Node_Kind) is
   begin
      Table (Types.Node_Id (N)).Kind := V;
   end Set_Kind;

   function Loc (N : Node_Id) return Location is
   begin
      return Table (Types.Node_Id (N)).Loc;
   end Loc;

   procedure Set_Loc (N : Node_Id; V : Location) is
   begin
      Table (Types.Node_Id (N)).Loc := V;
   end Set_Loc;

   function Next_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Invalid_Node
        or else Table (Types.Node_Id (N)).Kind = K_Scope_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Pair_Of_Entity_References
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Name
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Category
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_In_Modes
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_In_Binding
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Signed_AADLNumber
        or else Table (Types.Node_Id (N)).Kind = K_Not_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_And_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Or_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesis_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Minus_Numeric_Term
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Term
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term_Element
        or else Table (Types.Node_Id (N)).Kind = K_Computed_Term
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Type
        or else Table (Types.Node_Id (N)).Kind = K_String_Type
        or else Table (Types.Node_Id (N)).Kind = K_Real_Type
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Type
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Type
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Type
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Applies_To
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Content
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimensions
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Range_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Invalid_Node
        or else Table (Types.Node_Id (N)).Kind = K_Scope_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Pair_Of_Entity_References
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Name
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Category
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_In_Modes
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_In_Binding
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Signed_AADLNumber
        or else Table (Types.Node_Id (N)).Kind = K_Not_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_And_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Or_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesis_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Minus_Numeric_Term
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Term
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term_Element
        or else Table (Types.Node_Id (N)).Kind = K_Computed_Term
        or else Table (Types.Node_Id (N)).Kind = K_Boolean_Type
        or else Table (Types.Node_Id (N)).Kind = K_String_Type
        or else Table (Types.Node_Id (N)).Kind = K_Real_Type
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Type
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Type
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Type
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Applies_To
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Content
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimensions
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Range_Selection
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Next_Node;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifiers_List
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declarations_List
        or else Table (Types.Node_Id (N)).Kind = K_Property_List_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifiers_List
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declarations_List
        or else Table (Types.Node_Id (N)).Kind = K_Property_List_Value);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifiers_List
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declarations_List
        or else Table (Types.Node_Id (N)).Kind = K_Property_List_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Identifiers_List
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declarations_List
        or else Table (Types.Node_Id (N)).Kind = K_Property_List_Value);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Corresponding_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scope_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Corresponding_Entity;

   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scope_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Corresponding_Entity;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (3));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Name;

   function Display_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (4));
   end Display_Name;

   procedure Set_Display_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Display_Name;

   function Scope_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Scope_Entity;

   procedure Set_Scope_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Scope_Entity;

   function Homonym (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Homonym;

   procedure Set_Homonym (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Homonym;

   function Visible (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Visible;

   procedure Set_Visible (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Visible;

   function Backend_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Backend_Node;

   procedure Set_Backend_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Identifier);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Backend_Node;

   function Next_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Package_Name
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimensions
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Next_Entity;

   procedure Set_Next_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Package_Name
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimensions
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Next_Entity;

   function Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term_Element
        or else Table (Types.Node_Id (N)).Kind = K_Computed_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Identifier;

   procedure Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_AADL_Entity
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term_Element
        or else Table (Types.Node_Id (N)).Kind = K_Computed_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Identifier;

   function Entity_Scope (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Entity_Scope;

   procedure Set_Entity_Scope (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Entity_Scope;

   function Property_Scope (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Property_Scope;

   procedure Set_Property_Scope (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Property_Scope;

   function Is_Private (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Private;

   procedure Set_Is_Private (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Private;

   function First_Visited_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end First_Visited_Node;

   procedure Set_First_Visited_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_First_Visited_Node;

   function Namespace (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Namespace;

   procedure Set_Namespace (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Namespace;

   function Default_Instance (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Default_Instance;

   procedure Set_Default_Instance (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Default_Instance;

   function Path (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Path;

   procedure Set_Path (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Path;

   function Namespace_Path (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Namespace_Path;

   procedure Set_Namespace_Path (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Namespace_Path;

   function Namespace_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Namespace_Identifier;

   procedure Set_Namespace_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Namespace_Identifier;

   function Full_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Full_Identifier;

   procedure Set_Full_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Full_Identifier;

   function Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Entity;

   procedure Set_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Entity_Reference
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Entity;

   function First_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pair_Of_Entity_References);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Reference;

   procedure Set_First_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pair_Of_Entity_References);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Reference;

   function Second_Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pair_Of_Entity_References);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Second_Reference;

   procedure Set_Second_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pair_Of_Entity_References);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Second_Reference;

   function Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Declarations;

   procedure Set_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_AADL_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Declarations;

   function Identifiers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Name
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Identifiers;

   procedure Set_Identifiers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Name
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Identifiers;

   function Has_Private_Part (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Has_Private_Part;

   procedure Set_Has_Private_Part (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Has_Private_Part;

   function Has_Public_Part (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Has_Public_Part;

   procedure Set_Has_Public_Part (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Has_Public_Part;

   function Properties (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      return List_Id (Table (Types.Node_Id (N)).L (12));
   end Properties;

   procedure Set_Properties (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Properties;

   function Package_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Package_Name;

   procedure Set_Package_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Package_Name;

   function Parent (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Parent;

   procedure Set_Parent (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Parent;

   function Annexes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return List_Id (Table (Types.Node_Id (N)).L (13));
   end Annexes;

   procedure Set_Annexes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Annexes;

   function List_Items (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Type
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Type
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end List_Items;

   procedure Set_List_Items (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Name_Visibility_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Import_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Type
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Type
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_List_Items;

   function Is_All (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Applies_To);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_All;

   procedure Set_Is_All (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Applies_To);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_All;

   function Category (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Category
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category);

      return Byte (Table (Types.Node_Id (N)).O (3));
   end Category;

   procedure Set_Category (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Component_Category
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category);

      Table (Types.Node_Id (N)).O (3) := Byte (V);
   end Set_Category;

   function Entity_Category (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      return Byte (Table (Types.Node_Id (N)).O (4));
   end Entity_Category;

   procedure Set_Entity_Category (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      Table (Types.Node_Id (N)).O (4) := Byte (V);
   end Set_Entity_Category;

   function Reference (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Reference;

   procedure Set_Reference (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Reference;

   function Renamed_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Renamed_Entity;

   procedure Set_Renamed_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Alias_Declaration);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Renamed_Entity;

   function Features (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return List_Id (Table (Types.Node_Id (N)).L (14));
   end Features;

   procedure Set_Features (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Features;

   function Flows (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (15));
   end Flows;

   procedure Set_Flows (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Flows;

   function Instances (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (16));
   end Instances;

   procedure Set_Instances (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Instances;

   function Modes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_In_Modes);

      return List_Id (Table (Types.Node_Id (N)).L (17));
   end Modes;

   procedure Set_Modes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_In_Modes);

      Table (Types.Node_Id (N)).L (17) := Int (V);
   end Set_Modes;

   function Prototypes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return List_Id (Table (Types.Node_Id (N)).L (18));
   end Prototypes;

   procedure Set_Prototypes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (18) := Int (V);
   end Set_Prototypes;

   function Prototype_Bindings (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return List_Id (Table (Types.Node_Id (N)).L (19));
   end Prototype_Bindings;

   procedure Set_Prototype_Bindings (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Type
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (19) := Int (V);
   end Set_Prototype_Bindings;

   function Component_Type_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end Component_Type_Identifier;

   procedure Set_Component_Type_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Component_Type_Identifier;

   function Refines_Type (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (20));
   end Refines_Type;

   procedure Set_Refines_Type (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      Table (Types.Node_Id (N)).L (20) := Int (V);
   end Set_Refines_Type;

   function Subcomponents (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (21));
   end Subcomponents;

   procedure Set_Subcomponents (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      Table (Types.Node_Id (N)).L (21) := Int (V);
   end Set_Subcomponents;

   function Calls (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (22));
   end Calls;

   procedure Set_Calls (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation);

      Table (Types.Node_Id (N)).L (22) := Int (V);
   end Set_Calls;

   function Connections (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec);

      return List_Id (Table (Types.Node_Id (N)).L (23));
   end Connections;

   procedure Set_Connections (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec);

      Table (Types.Node_Id (N)).L (23) := Int (V);
   end Set_Connections;

   function Container_Component (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end Container_Component;

   procedure Set_Container_Component (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Entity
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Path);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Container_Component;

   function Entity_Ref (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end Entity_Ref;

   procedure Set_Entity_Ref (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Binding_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Entity_Ref;

   function Is_Refinement (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Refinement;

   procedure Set_Is_Refinement (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Prototype
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Refinement;

   function Is_Implicit_Inverse (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Implicit_Inverse;

   procedure Set_Is_Implicit_Inverse (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Implicit_Inverse;

   function Inversed_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Inversed_Entity;

   procedure Set_Inversed_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Refinable_Feature
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Mode
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Inversed_Entity;

   function Is_In (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_In;

   procedure Set_Is_In (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_In;

   function Is_Out (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Out;

   procedure Set_Is_Out (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Parameter);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Out;

   function Is_Feature (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec);

      return Boolean (Table (Types.Node_Id (N)).B (5));
   end Is_Feature;

   procedure Set_Is_Feature (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec);

      Table (Types.Node_Id (N)).B (5) := Boolean (V);
   end Set_Is_Feature;

   function Is_Event (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec);

      return Boolean (Table (Types.Node_Id (N)).B (6));
   end Is_Event;

   procedure Set_Is_Event (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec);

      Table (Types.Node_Id (N)).B (6) := Boolean (V);
   end Set_Is_Event;

   function Is_Data (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec);

      return Boolean (Table (Types.Node_Id (N)).B (7));
   end Is_Data;

   procedure Set_Is_Data (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec);

      Table (Types.Node_Id (N)).B (7) := Boolean (V);
   end Set_Is_Data;

   function Array_Dimensions (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Array_Dimensions;

   procedure Set_Array_Dimensions (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Port_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Array_Dimensions;

   function Inverse_Of (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Inverse_Of;

   procedure Set_Inverse_Of (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Feature_Group_Type);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Inverse_Of;

   function Is_Server (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Server;

   procedure Set_Is_Server (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Spec);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Server;

   function Is_Provided (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Provided;

   procedure Set_Is_Provided (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Provided;

   function Subcomponent_Category (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access);

      return Byte (Table (Types.Node_Id (N)).O (4));
   end Subcomponent_Category;

   procedure Set_Subcomponent_Category (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent_Access);

      Table (Types.Node_Id (N)).O (4) := Byte (V);
   end Set_Subcomponent_Category;

   function Source_Flow (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Source_Flow;

   procedure Set_Source_Flow (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Source_Flow;

   function Sink_Flow (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Sink_Flow;

   procedure Set_Sink_Flow (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Sink_Flow;

   function In_Modes (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end In_Modes;

   procedure Set_In_Modes (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_End_To_End_Flow_Refinement
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Subcomponent
        or else Table (Types.Node_Id (N)).Kind = K_Connection
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_In_Modes;

   function Is_Requires (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Requires;

   procedure Set_Is_Requires (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Requires;

   function Is_Initial (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Initial;

   procedure Set_Is_Initial (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Initial;

   function Source_Modes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Source_Modes;

   procedure Set_Source_Modes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Source_Modes;

   function Triggers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Triggers;

   procedure Set_Triggers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Triggers;

   function Destination_Mode (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Destination_Mode;

   procedure Set_Destination_Mode (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Destination_Mode;

   function Is_Self (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Self;

   procedure Set_Is_Self (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Self;

   function Is_Processor (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Processor;

   procedure Set_Is_Processor (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Mode_Transition_Trigger);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Processor;

   function Corresponding_Flow_Spec (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Corresponding_Flow_Spec;

   procedure Set_Corresponding_Flow_Spec (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Flow_Implementation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Corresponding_Flow_Spec;

   function Parent_Sequence (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Parent_Sequence;

   procedure Set_Parent_Sequence (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Parent_Sequence;

   function Subprogram_Calls (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Subprogram_Calls;

   procedure Set_Subprogram_Calls (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call_Sequence);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Subprogram_Calls;

   function Is_Bidirectional (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Bidirectional;

   procedure Set_Is_Bidirectional (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Bidirectional;

   function Source (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Source;

   procedure Set_Source (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Source;

   function Destination (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Destination;

   procedure Set_Destination (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Connection);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Destination;

   function Property_Set_Context (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Set_Context;

   procedure Set_Property_Set_Context (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Set_Context;

   function Imports_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Imports_List;

   procedure Set_Imports_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Set);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Imports_List;

   function Annex_Path (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Annex_Path;

   procedure Set_Annex_Path (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Contained_Element_Path);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Annex_Path;

   function Is_List (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_List;

   procedure Set_Is_List (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_List;

   function Multiplicity (N : Node_Id) return Int is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      return Int (Table (Types.Node_Id (N)).L (2));
   end Multiplicity;

   procedure Set_Multiplicity (N : Node_Id; V : Int) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Multiplicity;

   function Property_Type_Designator (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Property_Type_Designator;

   procedure Set_Property_Type_Designator (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Element);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Property_Type_Designator;

   function Expanded_Type_Designator (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Expanded_Type_Designator;

   procedure Set_Expanded_Type_Designator (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Type);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Expanded_Type_Designator;

   function Property_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term_Element);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Expression;

   procedure Set_Property_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Single_Valued_Property
        or else Table (Types.Node_Id (N)).Kind = K_Record_Term_Element);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Expression;

   function Property_Expressions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Expressions;

   procedure Set_Property_Expressions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Multi_Valued_Property);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Expressions;

   function Constant_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Constant_Type;

   procedure Set_Constant_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Constant_Type;

   function Unique_Unit_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Unique_Unit_Identifier;

   procedure Set_Unique_Unit_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Unique_Unit_Identifier;

   function Constant_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Constant_Value;

   procedure Set_Constant_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Constant_Property_Declaration);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Constant_Value;

   function Value_Container (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Value_Container;

   procedure Set_Value_Container (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value_Container;

   function Single_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Single_Value;

   procedure Set_Single_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Single_Value;

   function Multi_Value (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Multi_Value;

   procedure Set_Multi_Value (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Multi_Value;

   function Expanded_Single_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Expanded_Single_Value;

   procedure Set_Expanded_Single_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Expanded_Single_Value;

   function Expanded_Multi_Value (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Expanded_Multi_Value;

   procedure Set_Expanded_Multi_Value (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Value);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Expanded_Multi_Value;

   function Is_Access (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Access;

   procedure Set_Is_Access (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Access;

   function Is_Inherit (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Inherit;

   procedure Set_Is_Inherit (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Inherit;

   function Property_Name_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Property_Name_Type;

   procedure Set_Property_Name_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Property_Name_Type;

   function Default_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Default_Value;

   procedure Set_Default_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Default_Value;

   function Applies_To (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Applies_To;

   procedure Set_Applies_To (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Definition_Declaration);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Applies_To;

   function Binding (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_In_Binding);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Binding;

   procedure Set_Binding (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_In_Binding);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Binding;

   function Property_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Property_Name;

   procedure Set_Property_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Property_Name;

   function Is_Additive_Association (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Additive_Association;

   procedure Set_Is_Additive_Association (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Additive_Association;

   function Is_Constant (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Constant;

   procedure Set_Is_Constant (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Constant;

   function Property_Association_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Property_Association_Type;

   procedure Set_Property_Association_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Property_Association_Type;

   function Property_Association_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Property_Association_Value;

   procedure Set_Property_Association_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Property_Association_Value;

   function Applies_To_Prop (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return List_Id (Table (Types.Node_Id (N)).L (12));
   end Applies_To_Prop;

   procedure Set_Applies_To_Prop (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Applies_To_Prop;

   function In_Binding (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end In_Binding;

   procedure Set_In_Binding (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Association);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_In_Binding;

   function Component_Cat (N : Node_Id) return Byte is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category);

      return Byte (Table (Types.Node_Id (N)).O (1));
   end Component_Cat;

   procedure Set_Component_Cat (N : Node_Id; V : Byte) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Component_Classifier_Term
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Referable_Element_Category
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Component_Cat;

   function Classifier_Ref (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Classifier_Ref;

   procedure Set_Classifier_Ref (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Named_Element
        or else Table (Types.Node_Id (N)).Kind = K_Classifier_Category_Ref
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Category);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Classifier_Ref;

   function Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Value;

   procedure Set_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value;

   function Number_Value (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Signed_AADLNumber);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Number_Value;

   procedure Set_Number_Value (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Signed_AADLNumber);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Number_Value;

   function Unit_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Signed_AADLNumber
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Unit_Identifier;

   procedure Set_Unit_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Signed_AADLNumber
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Unit_Identifier;

   function Boolean_Term (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Not_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesis_Boolean_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Boolean_Term;

   procedure Set_Boolean_Term (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Not_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Parenthesis_Boolean_Term);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Boolean_Term;

   function First_Term (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_And_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Or_Boolean_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Term;

   procedure Set_First_Term (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_And_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Or_Boolean_Term);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Term;

   function Second_Term (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_And_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Or_Boolean_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Second_Term;

   procedure Set_Second_Term (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_And_Boolean_Term
        or else Table (Types.Node_Id (N)).Kind = K_Or_Boolean_Term);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Second_Term;

   function Numeric_Term (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Minus_Numeric_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Numeric_Term;

   procedure Set_Numeric_Term (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Minus_Numeric_Term);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Numeric_Term;

   function Property_Set_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Property_Set_Identifier;

   procedure Set_Property_Set_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Property_Term
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Term
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Type_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Unique_Property_Const_Identifier);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Property_Set_Identifier;

   function Lower_Bound (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range
        or else Table (Types.Node_Id (N)).Kind = K_Range_Selection);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Lower_Bound;

   procedure Set_Lower_Bound (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range
        or else Table (Types.Node_Id (N)).Kind = K_Range_Selection);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Lower_Bound;

   function Upper_Bound (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range
        or else Table (Types.Node_Id (N)).Kind = K_Range_Selection);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Upper_Bound;

   procedure Set_Upper_Bound (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range
        or else Table (Types.Node_Id (N)).Kind = K_Range_Selection);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Upper_Bound;

   function Delta_Term (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Delta_Term;

   procedure Set_Delta_Term (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Number_Range_Term);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Delta_Term;

   function Reference_Term (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Term);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Reference_Term;

   procedure Set_Reference_Term (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Reference_Term);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Reference_Term;

   function Type_Range (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Real_Type
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Type_Range;

   procedure Set_Type_Range (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Real_Type
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Type);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Type_Range;

   function Unit_Designator (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Real_Type
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Unit_Designator;

   procedure Set_Unit_Designator (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Real_Type
        or else Table (Types.Node_Id (N)).Kind = K_Integer_Type);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Unit_Designator;

   function Enumeration_Context (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Enumeration_Context;

   procedure Set_Enumeration_Context (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Enumeration_Context;

   function Numeric_Literal (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Numeric_Literal;

   procedure Set_Numeric_Literal (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Unit_Definition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Numeric_Literal;

   function Base_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Base_Identifier;

   procedure Set_Base_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Base_Identifier;

   function Unit_Definitions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Unit_Definitions;

   procedure Set_Unit_Definitions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Unit_Definitions;

   function Units_Context (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Units_Context;

   procedure Set_Units_Context (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Units_Type);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Units_Context;

   function Number_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Number_Type;

   procedure Set_Number_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Type);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Number_Type;

   function Owner_Categories (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Applies_To);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Owner_Categories;

   procedure Set_Owner_Categories (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Applies_To);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Owner_Categories;

   function Raw_Text (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Content);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Raw_Text;

   procedure Set_Raw_Text (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Content);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Raw_Text;

   function Annex_Content (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Annex_Content;

   procedure Set_Annex_Content (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Annex_Content;

   function Corresponding_Annex (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Corresponding_Annex;

   procedure Set_Corresponding_Annex (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Subclause
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Corresponding_Annex;

   function Container_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Container_Package;

   procedure Set_Container_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Annex_Library);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Container_Package;

   function Array_List_Dim (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimensions);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Array_List_Dim;

   procedure Set_Array_List_Dim (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimensions);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Array_List_Dim;

   function Size (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Size;

   procedure Set_Size (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Dimension_Size);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Size;

   function Range_Selections (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Range_Selections;

   procedure Set_Range_Selections (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Selection);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Range_Selections;

   function Item (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Item;

   procedure Set_Item (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Item;

   function Extra_Item (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Extra_Item;

   procedure Set_Extra_Item (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Container);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Extra_Item;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Invalid_Node =>
            W_Invalid_Node
              (Node_Id (N));
         when K_Scope_Definition =>
            W_Scope_Definition
              (Node_Id (N));
         when K_Identifier =>
            W_Identifier
              (Node_Id (N));
         when K_AADL_Entity =>
            W_AADL_Entity
              (Node_Id (N));
         when K_Named_AADL_Entity =>
            W_Named_AADL_Entity
              (Node_Id (N));
         when K_AADL_Declaration =>
            W_AADL_Declaration
              (Node_Id (N));
         when K_Entity_Reference =>
            W_Entity_Reference
              (Node_Id (N));
         when K_Pair_Of_Entity_References =>
            W_Pair_Of_Entity_References
              (Node_Id (N));
         when K_Identifiers_List =>
            W_Identifiers_List
              (List_Id (N));
         when K_AADL_Specification =>
            W_AADL_Specification
              (Node_Id (N));
         when K_AADL_Declarations_List =>
            W_AADL_Declarations_List
              (List_Id (N));
         when K_Package_Name =>
            W_Package_Name
              (Node_Id (N));
         when K_Package_Specification =>
            W_Package_Specification
              (Node_Id (N));
         when K_Name_Visibility_Declaration =>
            W_Name_Visibility_Declaration
              (Node_Id (N));
         when K_Import_Declaration =>
            W_Import_Declaration
              (Node_Id (N));
         when K_Alias_Declaration =>
            W_Alias_Declaration
              (Node_Id (N));
         when K_Component_Category =>
            W_Component_Category
              (Node_Id (N));
         when K_Component_Type =>
            W_Component_Type
              (Node_Id (N));
         when K_Component_Implementation =>
            W_Component_Implementation
              (Node_Id (N));
         when K_Contained_Entity =>
            W_Contained_Entity
              (Node_Id (N));
         when K_Subclause =>
            W_Subclause
              (Node_Id (N));
         when K_Prototype =>
            W_Prototype
              (Node_Id (N));
         when K_Binding_Prototype =>
            W_Binding_Prototype
              (Node_Id (N));
         when K_Feature =>
            W_Feature
              (Node_Id (N));
         when K_Refinable_Feature =>
            W_Refinable_Feature
              (Node_Id (N));
         when K_Port_Spec =>
            W_Port_Spec
              (Node_Id (N));
         when K_Feature_Group_Spec =>
            W_Feature_Group_Spec
              (Node_Id (N));
         when K_Subprogram_Spec =>
            W_Subprogram_Spec
              (Node_Id (N));
         when K_Parameter =>
            W_Parameter
              (Node_Id (N));
         when K_Subcomponent_Access =>
            W_Subcomponent_Access
              (Node_Id (N));
         when K_Flow_Spec =>
            W_Flow_Spec
              (Node_Id (N));
         when K_Mode =>
            W_Mode
              (Node_Id (N));
         when K_Mode_Transition =>
            W_Mode_Transition
              (Node_Id (N));
         when K_In_Modes =>
            W_In_Modes
              (Node_Id (N));
         when K_Mode_Transition_Trigger =>
            W_Mode_Transition_Trigger
              (Node_Id (N));
         when K_Flow_Implementation =>
            W_Flow_Implementation
              (Node_Id (N));
         when K_End_To_End_Flow_Spec =>
            W_End_To_End_Flow_Spec
              (Node_Id (N));
         when K_Flow_Implementation_Refinement =>
            W_Flow_Implementation_Refinement
              (Node_Id (N));
         when K_End_To_End_Flow_Refinement =>
            W_End_To_End_Flow_Refinement
              (Node_Id (N));
         when K_Subprogram_Call =>
            W_Subprogram_Call
              (Node_Id (N));
         when K_Subprogram_Call_Sequence =>
            W_Subprogram_Call_Sequence
              (Node_Id (N));
         when K_Subcomponent =>
            W_Subcomponent
              (Node_Id (N));
         when K_Feature_Group_Type =>
            W_Feature_Group_Type
              (Node_Id (N));
         when K_Connection =>
            W_Connection
              (Node_Id (N));
         when K_Property_Set =>
            W_Property_Set
              (Node_Id (N));
         when K_Contained_Element_Path =>
            W_Contained_Element_Path
              (Node_Id (N));
         when K_Property_Type =>
            W_Property_Type
              (Node_Id (N));
         when K_Property_Type_Declaration =>
            W_Property_Type_Declaration
              (Node_Id (N));
         when K_Single_Valued_Property =>
            W_Single_Valued_Property
              (Node_Id (N));
         when K_Multi_Valued_Property =>
            W_Multi_Valued_Property
              (Node_Id (N));
         when K_Constant_Property_Declaration =>
            W_Constant_Property_Declaration
              (Node_Id (N));
         when K_Property_Value =>
            W_Property_Value
              (Node_Id (N));
         when K_Property_Definition_Declaration =>
            W_Property_Definition_Declaration
              (Node_Id (N));
         when K_Property_List_Value =>
            W_Property_List_Value
              (List_Id (N));
         when K_In_Binding =>
            W_In_Binding
              (Node_Id (N));
         when K_Property_Association =>
            W_Property_Association
              (Node_Id (N));
         when K_Named_Element =>
            W_Named_Element
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_Signed_AADLNumber =>
            W_Signed_AADLNumber
              (Node_Id (N));
         when K_Not_Boolean_Term =>
            W_Not_Boolean_Term
              (Node_Id (N));
         when K_And_Boolean_Term =>
            W_And_Boolean_Term
              (Node_Id (N));
         when K_Or_Boolean_Term =>
            W_Or_Boolean_Term
              (Node_Id (N));
         when K_Parenthesis_Boolean_Term =>
            W_Parenthesis_Boolean_Term
              (Node_Id (N));
         when K_Minus_Numeric_Term =>
            W_Minus_Numeric_Term
              (Node_Id (N));
         when K_Property_Term =>
            W_Property_Term
              (Node_Id (N));
         when K_Enumeration_Term =>
            W_Enumeration_Term
              (Node_Id (N));
         when K_Unit_Term =>
            W_Unit_Term
              (Node_Id (N));
         when K_Number_Range_Term =>
            W_Number_Range_Term
              (Node_Id (N));
         when K_Component_Classifier_Term =>
            W_Component_Classifier_Term
              (Node_Id (N));
         when K_Reference_Term =>
            W_Reference_Term
              (Node_Id (N));
         when K_Record_Term =>
            W_Record_Term
              (Node_Id (N));
         when K_Record_Term_Element =>
            W_Record_Term_Element
              (Node_Id (N));
         when K_Computed_Term =>
            W_Computed_Term
              (Node_Id (N));
         when K_Boolean_Type =>
            W_Boolean_Type
              (Node_Id (N));
         when K_String_Type =>
            W_String_Type
              (Node_Id (N));
         when K_Real_Type =>
            W_Real_Type
              (Node_Id (N));
         when K_Integer_Type =>
            W_Integer_Type
              (Node_Id (N));
         when K_Enumeration_Type =>
            W_Enumeration_Type
              (Node_Id (N));
         when K_Number_Range =>
            W_Number_Range
              (Node_Id (N));
         when K_Unit_Definition =>
            W_Unit_Definition
              (Node_Id (N));
         when K_Units_Type =>
            W_Units_Type
              (Node_Id (N));
         when K_Range_Type =>
            W_Range_Type
              (Node_Id (N));
         when K_Classifier_Type =>
            W_Classifier_Type
              (Node_Id (N));
         when K_Classifier_Category_Ref =>
            W_Classifier_Category_Ref
              (Node_Id (N));
         when K_Referable_Element_Category =>
            W_Referable_Element_Category
              (Node_Id (N));
         when K_Reference_Type =>
            W_Reference_Type
              (Node_Id (N));
         when K_Reference_Category =>
            W_Reference_Category
              (Node_Id (N));
         when K_Record_Type =>
            W_Record_Type
              (Node_Id (N));
         when K_Record_Type_Element =>
            W_Record_Type_Element
              (Node_Id (N));
         when K_Unique_Property_Type_Identifier =>
            W_Unique_Property_Type_Identifier
              (Node_Id (N));
         when K_Applies_To =>
            W_Applies_To
              (Node_Id (N));
         when K_Unique_Property_Const_Identifier =>
            W_Unique_Property_Const_Identifier
              (Node_Id (N));
         when K_Annex_Content =>
            W_Annex_Content
              (Node_Id (N));
         when K_Annex_Subclause =>
            W_Annex_Subclause
              (Node_Id (N));
         when K_Annex_Library =>
            W_Annex_Library
              (Node_Id (N));
         when K_Annex_Path =>
            W_Annex_Path
              (Node_Id (N));
         when K_Array_Dimensions =>
            W_Array_Dimensions
              (Node_Id (N));
         when K_Array_Dimension_Size =>
            W_Array_Dimension_Size
              (Node_Id (N));
         when K_Array_Selection =>
            W_Array_Selection
              (Node_Id (N));
         when K_Range_Selection =>
            W_Range_Selection
              (Node_Id (N));
         when K_Node_Container =>
            W_Node_Container
              (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

   procedure W_Invalid_Node (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Invalid_Node;

   procedure W_Scope_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Corresponding_Entity",
         "Node_Id",
         Image (Corresponding_Entity (N)),
         Int (Corresponding_Entity (N)));
   end W_Scope_Definition;

   procedure W_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Corresponding_Entity",
         "Node_Id",
         Image (Corresponding_Entity (N)),
         Int (Corresponding_Entity (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Display_Name",
         "Name_Id",
         Image (Display_Name (N)));
      W_Node_Attribute
        ("Scope_Entity",
         "Node_Id",
         Image (Scope_Entity (N)),
         Int (Scope_Entity (N)));
      W_Node_Attribute
        ("Homonym",
         "Node_Id",
         Image (Homonym (N)),
         Int (Homonym (N)));
      W_Node_Attribute
        ("Visible",
         "Boolean",
         Image (Visible (N)));
      W_Node_Attribute
        ("Backend_Node",
         "Node_Id",
         Image (Backend_Node (N)),
         Int (Backend_Node (N)));
   end W_Identifier;

   procedure W_AADL_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
   end W_AADL_Entity;

   procedure W_Named_AADL_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
   end W_Named_AADL_Entity;

   procedure W_AADL_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity_Scope",
         "Node_Id",
         Image (Entity_Scope (N)),
         Int (Entity_Scope (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("First_Visited_Node",
         "Node_Id",
         Image (First_Visited_Node (N)),
         Int (First_Visited_Node (N)));
      W_Node_Attribute
        ("Namespace",
         "Node_Id",
         Image (Namespace (N)),
         Int (Namespace (N)));
      W_Node_Attribute
        ("Default_Instance",
         "Node_Id",
         Image (Default_Instance (N)),
         Int (Default_Instance (N)));
   end W_AADL_Declaration;

   procedure W_Entity_Reference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Path",
         "List_Id",
         Image (Path (N)),
         Int (Path (N)));
      W_Node_Attribute
        ("Namespace_Path",
         "List_Id",
         Image (Namespace_Path (N)),
         Int (Namespace_Path (N)));
      W_Node_Attribute
        ("Namespace_Identifier",
         "Node_Id",
         Image (Namespace_Identifier (N)),
         Int (Namespace_Identifier (N)));
      W_Node_Attribute
        ("Full_Identifier",
         "Node_Id",
         Image (Full_Identifier (N)),
         Int (Full_Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
   end W_Entity_Reference;

   procedure W_Pair_Of_Entity_References (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("First_Reference",
         "Node_Id",
         Image (First_Reference (N)),
         Int (First_Reference (N)));
      W_Node_Attribute
        ("Second_Reference",
         "Node_Id",
         Image (Second_Reference (N)),
         Int (Second_Reference (N)));
   end W_Pair_Of_Entity_References;

   procedure W_Identifiers_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Identifiers_List;

   procedure W_AADL_Specification (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Entity_Scope",
         "Node_Id",
         Image (Entity_Scope (N)),
         Int (Entity_Scope (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
   end W_AADL_Specification;

   procedure W_AADL_Declarations_List (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_AADL_Declarations_List;

   procedure W_Package_Name (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
   end W_Package_Name;

   procedure W_Package_Specification (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity_Scope",
         "Node_Id",
         Image (Entity_Scope (N)),
         Int (Entity_Scope (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
      W_Node_Attribute
        ("Has_Private_Part",
         "Boolean",
         Image (Has_Private_Part (N)));
      W_Node_Attribute
        ("Has_Public_Part",
         "Boolean",
         Image (Has_Public_Part (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Package_Name",
         "Node_Id",
         Image (Package_Name (N)),
         Int (Package_Name (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Annexes",
         "List_Id",
         Image (Annexes (N)),
         Int (Annexes (N)));
   end W_Package_Specification;

   procedure W_Name_Visibility_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("List_Items",
         "List_Id",
         Image (List_Items (N)),
         Int (List_Items (N)));
   end W_Name_Visibility_Declaration;

   procedure W_Import_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("List_Items",
         "List_Id",
         Image (List_Items (N)),
         Int (List_Items (N)));
   end W_Import_Declaration;

   procedure W_Alias_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("Package_Name",
         "Node_Id",
         Image (Package_Name (N)),
         Int (Package_Name (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Is_All",
         "Boolean",
         Image (Is_All (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Entity_Category",
         "Byte",
         Image (Entity_Category (N)));
      W_Node_Attribute
        ("Reference",
         "Node_Id",
         Image (Reference (N)),
         Int (Reference (N)));
      W_Node_Attribute
        ("Renamed_Entity",
         "Node_Id",
         Image (Renamed_Entity (N)),
         Int (Renamed_Entity (N)));
   end W_Alias_Declaration;

   procedure W_Component_Category (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
   end W_Component_Category;

   procedure W_Component_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity_Scope",
         "Node_Id",
         Image (Entity_Scope (N)),
         Int (Entity_Scope (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("First_Visited_Node",
         "Node_Id",
         Image (First_Visited_Node (N)),
         Int (First_Visited_Node (N)));
      W_Node_Attribute
        ("Namespace",
         "Node_Id",
         Image (Namespace (N)),
         Int (Namespace (N)));
      W_Node_Attribute
        ("Default_Instance",
         "Node_Id",
         Image (Default_Instance (N)),
         Int (Default_Instance (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Annexes",
         "List_Id",
         Image (Annexes (N)),
         Int (Annexes (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Features",
         "List_Id",
         Image (Features (N)),
         Int (Features (N)));
      W_Node_Attribute
        ("Flows",
         "List_Id",
         Image (Flows (N)),
         Int (Flows (N)));
      W_Node_Attribute
        ("Instances",
         "List_Id",
         Image (Instances (N)),
         Int (Instances (N)));
      W_Node_Attribute
        ("Modes",
         "List_Id",
         Image (Modes (N)),
         Int (Modes (N)));
      W_Node_Attribute
        ("Prototypes",
         "List_Id",
         Image (Prototypes (N)),
         Int (Prototypes (N)));
      W_Node_Attribute
        ("Prototype_Bindings",
         "List_Id",
         Image (Prototype_Bindings (N)),
         Int (Prototype_Bindings (N)));
   end W_Component_Type;

   procedure W_Component_Implementation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity_Scope",
         "Node_Id",
         Image (Entity_Scope (N)),
         Int (Entity_Scope (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("First_Visited_Node",
         "Node_Id",
         Image (First_Visited_Node (N)),
         Int (First_Visited_Node (N)));
      W_Node_Attribute
        ("Namespace",
         "Node_Id",
         Image (Namespace (N)),
         Int (Namespace (N)));
      W_Node_Attribute
        ("Default_Instance",
         "Node_Id",
         Image (Default_Instance (N)),
         Int (Default_Instance (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Annexes",
         "List_Id",
         Image (Annexes (N)),
         Int (Annexes (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Flows",
         "List_Id",
         Image (Flows (N)),
         Int (Flows (N)));
      W_Node_Attribute
        ("Instances",
         "List_Id",
         Image (Instances (N)),
         Int (Instances (N)));
      W_Node_Attribute
        ("Modes",
         "List_Id",
         Image (Modes (N)),
         Int (Modes (N)));
      W_Node_Attribute
        ("Prototypes",
         "List_Id",
         Image (Prototypes (N)),
         Int (Prototypes (N)));
      W_Node_Attribute
        ("Prototype_Bindings",
         "List_Id",
         Image (Prototype_Bindings (N)),
         Int (Prototype_Bindings (N)));
      W_Node_Attribute
        ("Component_Type_Identifier",
         "Node_Id",
         Image (Component_Type_Identifier (N)),
         Int (Component_Type_Identifier (N)));
      W_Node_Attribute
        ("Refines_Type",
         "List_Id",
         Image (Refines_Type (N)),
         Int (Refines_Type (N)));
      W_Node_Attribute
        ("Subcomponents",
         "List_Id",
         Image (Subcomponents (N)),
         Int (Subcomponents (N)));
      W_Node_Attribute
        ("Calls",
         "List_Id",
         Image (Calls (N)),
         Int (Calls (N)));
      W_Node_Attribute
        ("Connections",
         "List_Id",
         Image (Connections (N)),
         Int (Connections (N)));
   end W_Component_Implementation;

   procedure W_Contained_Entity (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
   end W_Contained_Entity;

   procedure W_Subclause (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
   end W_Subclause;

   procedure W_Prototype (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
   end W_Prototype;

   procedure W_Binding_Prototype (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
   end W_Binding_Prototype;

   procedure W_Feature (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
   end W_Feature;

   procedure W_Refinable_Feature (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
   end W_Refinable_Feature;

   procedure W_Port_Spec (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Is_In",
         "Boolean",
         Image (Is_In (N)));
      W_Node_Attribute
        ("Is_Out",
         "Boolean",
         Image (Is_Out (N)));
      W_Node_Attribute
        ("Is_Feature",
         "Boolean",
         Image (Is_Feature (N)));
      W_Node_Attribute
        ("Is_Event",
         "Boolean",
         Image (Is_Event (N)));
      W_Node_Attribute
        ("Is_Data",
         "Boolean",
         Image (Is_Data (N)));
      W_Node_Attribute
        ("Array_Dimensions",
         "Node_Id",
         Image (Array_Dimensions (N)),
         Int (Array_Dimensions (N)));
   end W_Port_Spec;

   procedure W_Feature_Group_Spec (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Inverse_Of",
         "Node_Id",
         Image (Inverse_Of (N)),
         Int (Inverse_Of (N)));
   end W_Feature_Group_Spec;

   procedure W_Subprogram_Spec (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Is_Server",
         "Boolean",
         Image (Is_Server (N)));
   end W_Subprogram_Spec;

   procedure W_Parameter (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Is_In",
         "Boolean",
         Image (Is_In (N)));
      W_Node_Attribute
        ("Is_Out",
         "Boolean",
         Image (Is_Out (N)));
   end W_Parameter;

   procedure W_Subcomponent_Access (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Is_Provided",
         "Boolean",
         Image (Is_Provided (N)));
      W_Node_Attribute
        ("Subcomponent_Category",
         "Byte",
         Image (Subcomponent_Category (N)));
   end W_Subcomponent_Access;

   procedure W_Flow_Spec (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Source_Flow",
         "Node_Id",
         Image (Source_Flow (N)),
         Int (Source_Flow (N)));
      W_Node_Attribute
        ("Sink_Flow",
         "Node_Id",
         Image (Sink_Flow (N)),
         Int (Sink_Flow (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
   end W_Flow_Spec;

   procedure W_Mode (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Is_Requires",
         "Boolean",
         Image (Is_Requires (N)));
      W_Node_Attribute
        ("Is_Initial",
         "Boolean",
         Image (Is_Initial (N)));
   end W_Mode;

   procedure W_Mode_Transition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Source_Modes",
         "List_Id",
         Image (Source_Modes (N)),
         Int (Source_Modes (N)));
      W_Node_Attribute
        ("Triggers",
         "List_Id",
         Image (Triggers (N)),
         Int (Triggers (N)));
      W_Node_Attribute
        ("Destination_Mode",
         "Node_Id",
         Image (Destination_Mode (N)),
         Int (Destination_Mode (N)));
   end W_Mode_Transition;

   procedure W_In_Modes (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Modes",
         "List_Id",
         Image (Modes (N)),
         Int (Modes (N)));
   end W_In_Modes;

   procedure W_Mode_Transition_Trigger (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_Self",
         "Boolean",
         Image (Is_Self (N)));
      W_Node_Attribute
        ("Is_Processor",
         "Boolean",
         Image (Is_Processor (N)));
   end W_Mode_Transition_Trigger;

   procedure W_Flow_Implementation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Connections",
         "List_Id",
         Image (Connections (N)),
         Int (Connections (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Source_Flow",
         "Node_Id",
         Image (Source_Flow (N)),
         Int (Source_Flow (N)));
      W_Node_Attribute
        ("Sink_Flow",
         "Node_Id",
         Image (Sink_Flow (N)),
         Int (Sink_Flow (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Corresponding_Flow_Spec",
         "Node_Id",
         Image (Corresponding_Flow_Spec (N)),
         Int (Corresponding_Flow_Spec (N)));
   end W_Flow_Implementation;

   procedure W_End_To_End_Flow_Spec (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Connections",
         "List_Id",
         Image (Connections (N)),
         Int (Connections (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Source_Flow",
         "Node_Id",
         Image (Source_Flow (N)),
         Int (Source_Flow (N)));
      W_Node_Attribute
        ("Sink_Flow",
         "Node_Id",
         Image (Sink_Flow (N)),
         Int (Sink_Flow (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
   end W_End_To_End_Flow_Spec;

   procedure W_Flow_Implementation_Refinement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
   end W_Flow_Implementation_Refinement;

   procedure W_End_To_End_Flow_Refinement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
   end W_End_To_End_Flow_Refinement;

   procedure W_Subprogram_Call (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Parent_Sequence",
         "Node_Id",
         Image (Parent_Sequence (N)),
         Int (Parent_Sequence (N)));
   end W_Subprogram_Call;

   procedure W_Subprogram_Call_Sequence (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Subprogram_Calls",
         "List_Id",
         Image (Subprogram_Calls (N)),
         Int (Subprogram_Calls (N)));
   end W_Subprogram_Call_Sequence;

   procedure W_Subcomponent (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Prototype_Bindings",
         "List_Id",
         Image (Prototype_Bindings (N)),
         Int (Prototype_Bindings (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("Is_Implicit_Inverse",
         "Boolean",
         Image (Is_Implicit_Inverse (N)));
      W_Node_Attribute
        ("Inversed_Entity",
         "Node_Id",
         Image (Inversed_Entity (N)),
         Int (Inversed_Entity (N)));
      W_Node_Attribute
        ("Array_Dimensions",
         "Node_Id",
         Image (Array_Dimensions (N)),
         Int (Array_Dimensions (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
   end W_Subcomponent;

   procedure W_Feature_Group_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity_Scope",
         "Node_Id",
         Image (Entity_Scope (N)),
         Int (Entity_Scope (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("First_Visited_Node",
         "Node_Id",
         Image (First_Visited_Node (N)),
         Int (First_Visited_Node (N)));
      W_Node_Attribute
        ("Namespace",
         "Node_Id",
         Image (Namespace (N)),
         Int (Namespace (N)));
      W_Node_Attribute
        ("Default_Instance",
         "Node_Id",
         Image (Default_Instance (N)),
         Int (Default_Instance (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Annexes",
         "List_Id",
         Image (Annexes (N)),
         Int (Annexes (N)));
      W_Node_Attribute
        ("Features",
         "List_Id",
         Image (Features (N)),
         Int (Features (N)));
      W_Node_Attribute
        ("Prototypes",
         "List_Id",
         Image (Prototypes (N)),
         Int (Prototypes (N)));
      W_Node_Attribute
        ("Prototype_Bindings",
         "List_Id",
         Image (Prototype_Bindings (N)),
         Int (Prototype_Bindings (N)));
      W_Node_Attribute
        ("Inverse_Of",
         "Node_Id",
         Image (Inverse_Of (N)),
         Int (Inverse_Of (N)));
   end W_Feature_Group_Type;

   procedure W_Connection (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Scope",
         "Node_Id",
         Image (Property_Scope (N)),
         Int (Property_Scope (N)));
      W_Node_Attribute
        ("Properties",
         "List_Id",
         Image (Properties (N)),
         Int (Properties (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Entity_Ref",
         "Node_Id",
         Image (Entity_Ref (N)),
         Int (Entity_Ref (N)));
      W_Node_Attribute
        ("Is_Refinement",
         "Boolean",
         Image (Is_Refinement (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Is_Bidirectional",
         "Boolean",
         Image (Is_Bidirectional (N)));
      W_Node_Attribute
        ("Source",
         "Node_Id",
         Image (Source (N)),
         Int (Source (N)));
      W_Node_Attribute
        ("Destination",
         "Node_Id",
         Image (Destination (N)),
         Int (Destination (N)));
   end W_Connection;

   procedure W_Property_Set (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity_Scope",
         "Node_Id",
         Image (Entity_Scope (N)),
         Int (Entity_Scope (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
      W_Node_Attribute
        ("Property_Set_Context",
         "Node_Id",
         Image (Property_Set_Context (N)),
         Int (Property_Set_Context (N)));
      W_Node_Attribute
        ("Imports_List",
         "List_Id",
         Image (Imports_List (N)),
         Int (Imports_List (N)));
   end W_Property_Set;

   procedure W_Contained_Element_Path (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("List_Items",
         "List_Id",
         Image (List_Items (N)),
         Int (List_Items (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("Annex_Path",
         "Node_Id",
         Image (Annex_Path (N)),
         Int (Annex_Path (N)));
   end W_Contained_Element_Path;

   procedure W_Property_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Is_List",
         "Boolean",
         Image (Is_List (N)));
      W_Node_Attribute
        ("Multiplicity",
         "Int",
         Image (Multiplicity (N)));
      W_Node_Attribute
        ("Property_Type_Designator",
         "Node_Id",
         Image (Property_Type_Designator (N)),
         Int (Property_Type_Designator (N)));
      W_Node_Attribute
        ("Expanded_Type_Designator",
         "Node_Id",
         Image (Expanded_Type_Designator (N)),
         Int (Expanded_Type_Designator (N)));
   end W_Property_Type;

   procedure W_Property_Type_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Type_Designator",
         "Node_Id",
         Image (Property_Type_Designator (N)),
         Int (Property_Type_Designator (N)));
   end W_Property_Type_Declaration;

   procedure W_Single_Valued_Property (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Property_Type_Designator",
         "Node_Id",
         Image (Property_Type_Designator (N)),
         Int (Property_Type_Designator (N)));
      W_Node_Attribute
        ("Property_Expression",
         "Node_Id",
         Image (Property_Expression (N)),
         Int (Property_Expression (N)));
   end W_Single_Valued_Property;

   procedure W_Multi_Valued_Property (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Multiplicity",
         "Int",
         Image (Multiplicity (N)));
      W_Node_Attribute
        ("Property_Type_Designator",
         "Node_Id",
         Image (Property_Type_Designator (N)),
         Int (Property_Type_Designator (N)));
      W_Node_Attribute
        ("Property_Expressions",
         "List_Id",
         Image (Property_Expressions (N)),
         Int (Property_Expressions (N)));
   end W_Multi_Valued_Property;

   procedure W_Constant_Property_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Multiplicity",
         "Int",
         Image (Multiplicity (N)));
      W_Node_Attribute
        ("Constant_Type",
         "Node_Id",
         Image (Constant_Type (N)),
         Int (Constant_Type (N)));
      W_Node_Attribute
        ("Unique_Unit_Identifier",
         "Node_Id",
         Image (Unique_Unit_Identifier (N)),
         Int (Unique_Unit_Identifier (N)));
      W_Node_Attribute
        ("Constant_Value",
         "Node_Id",
         Image (Constant_Value (N)),
         Int (Constant_Value (N)));
   end W_Constant_Property_Declaration;

   procedure W_Property_Value (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Value_Container",
         "Node_Id",
         Image (Value_Container (N)),
         Int (Value_Container (N)));
      W_Node_Attribute
        ("Single_Value",
         "Node_Id",
         Image (Single_Value (N)),
         Int (Single_Value (N)));
      W_Node_Attribute
        ("Multi_Value",
         "List_Id",
         Image (Multi_Value (N)),
         Int (Multi_Value (N)));
      W_Node_Attribute
        ("Expanded_Single_Value",
         "Node_Id",
         Image (Expanded_Single_Value (N)),
         Int (Expanded_Single_Value (N)));
      W_Node_Attribute
        ("Expanded_Multi_Value",
         "List_Id",
         Image (Expanded_Multi_Value (N)),
         Int (Expanded_Multi_Value (N)));
   end W_Property_Value;

   procedure W_Property_Definition_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_Access",
         "Boolean",
         Image (Is_Access (N)));
      W_Node_Attribute
        ("Is_Inherit",
         "Boolean",
         Image (Is_Inherit (N)));
      W_Node_Attribute
        ("Property_Name_Type",
         "Node_Id",
         Image (Property_Name_Type (N)),
         Int (Property_Name_Type (N)));
      W_Node_Attribute
        ("Default_Value",
         "Node_Id",
         Image (Default_Value (N)),
         Int (Default_Value (N)));
      W_Node_Attribute
        ("Applies_To",
         "Node_Id",
         Image (Applies_To (N)),
         Int (Applies_To (N)));
   end W_Property_Definition_Declaration;

   procedure W_Property_List_Value (N : List_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("First_Node",
         "Node_Id",
         Image (First_Node (N)),
         Int (First_Node (N)));
      W_Node_Attribute
        ("Last_Node",
         "Node_Id",
         Image (Last_Node (N)),
         Int (Last_Node (N)));
   end W_Property_List_Value;

   procedure W_In_Binding (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Binding",
         "List_Id",
         Image (Binding (N)),
         Int (Binding (N)));
   end W_In_Binding;

   procedure W_Property_Association (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Is_Access",
         "Boolean",
         Image (Is_Access (N)));
      W_Node_Attribute
        ("Property_Name",
         "Node_Id",
         Image (Property_Name (N)),
         Int (Property_Name (N)));
      W_Node_Attribute
        ("Is_Additive_Association",
         "Boolean",
         Image (Is_Additive_Association (N)));
      W_Node_Attribute
        ("Is_Constant",
         "Boolean",
         Image (Is_Constant (N)));
      W_Node_Attribute
        ("Property_Association_Type",
         "Node_Id",
         Image (Property_Association_Type (N)),
         Int (Property_Association_Type (N)));
      W_Node_Attribute
        ("Property_Association_Value",
         "Node_Id",
         Image (Property_Association_Value (N)),
         Int (Property_Association_Value (N)));
      W_Node_Attribute
        ("Applies_To_Prop",
         "List_Id",
         Image (Applies_To_Prop (N)),
         Int (Applies_To_Prop (N)));
      W_Node_Attribute
        ("In_Binding",
         "Node_Id",
         Image (In_Binding (N)),
         Int (In_Binding (N)));
   end W_Property_Association;

   procedure W_Named_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Component_Cat",
         "Byte",
         Image (Component_Cat (N)));
      W_Node_Attribute
        ("Classifier_Ref",
         "Node_Id",
         Image (Classifier_Ref (N)),
         Int (Classifier_Ref (N)));
   end W_Named_Element;

   procedure W_Literal (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Value",
         "Value_Id",
         Image (Value (N)));
   end W_Literal;

   procedure W_Signed_AADLNumber (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Number_Value",
         "Node_Id",
         Image (Number_Value (N)),
         Int (Number_Value (N)));
      W_Node_Attribute
        ("Unit_Identifier",
         "Node_Id",
         Image (Unit_Identifier (N)),
         Int (Unit_Identifier (N)));
   end W_Signed_AADLNumber;

   procedure W_Not_Boolean_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Boolean_Term",
         "Node_Id",
         Image (Boolean_Term (N)),
         Int (Boolean_Term (N)));
   end W_Not_Boolean_Term;

   procedure W_And_Boolean_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("First_Term",
         "Node_Id",
         Image (First_Term (N)),
         Int (First_Term (N)));
      W_Node_Attribute
        ("Second_Term",
         "Node_Id",
         Image (Second_Term (N)),
         Int (Second_Term (N)));
   end W_And_Boolean_Term;

   procedure W_Or_Boolean_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("First_Term",
         "Node_Id",
         Image (First_Term (N)),
         Int (First_Term (N)));
      W_Node_Attribute
        ("Second_Term",
         "Node_Id",
         Image (Second_Term (N)),
         Int (Second_Term (N)));
   end W_Or_Boolean_Term;

   procedure W_Parenthesis_Boolean_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Boolean_Term",
         "Node_Id",
         Image (Boolean_Term (N)),
         Int (Boolean_Term (N)));
   end W_Parenthesis_Boolean_Term;

   procedure W_Minus_Numeric_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Numeric_Term",
         "Node_Id",
         Image (Numeric_Term (N)),
         Int (Numeric_Term (N)));
   end W_Minus_Numeric_Term;

   procedure W_Property_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Property_Set_Identifier",
         "Node_Id",
         Image (Property_Set_Identifier (N)),
         Int (Property_Set_Identifier (N)));
   end W_Property_Term;

   procedure W_Enumeration_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Property_Set_Identifier",
         "Node_Id",
         Image (Property_Set_Identifier (N)),
         Int (Property_Set_Identifier (N)));
   end W_Enumeration_Term;

   procedure W_Unit_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Property_Set_Identifier",
         "Node_Id",
         Image (Property_Set_Identifier (N)),
         Int (Property_Set_Identifier (N)));
   end W_Unit_Term;

   procedure W_Number_Range_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Lower_Bound",
         "Node_Id",
         Image (Lower_Bound (N)),
         Int (Lower_Bound (N)));
      W_Node_Attribute
        ("Upper_Bound",
         "Node_Id",
         Image (Upper_Bound (N)),
         Int (Upper_Bound (N)));
      W_Node_Attribute
        ("Delta_Term",
         "Node_Id",
         Image (Delta_Term (N)),
         Int (Delta_Term (N)));
   end W_Number_Range_Term;

   procedure W_Component_Classifier_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Path",
         "List_Id",
         Image (Path (N)),
         Int (Path (N)));
      W_Node_Attribute
        ("Namespace_Path",
         "List_Id",
         Image (Namespace_Path (N)),
         Int (Namespace_Path (N)));
      W_Node_Attribute
        ("Namespace_Identifier",
         "Node_Id",
         Image (Namespace_Identifier (N)),
         Int (Namespace_Identifier (N)));
      W_Node_Attribute
        ("Full_Identifier",
         "Node_Id",
         Image (Full_Identifier (N)),
         Int (Full_Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Component_Cat",
         "Byte",
         Image (Component_Cat (N)));
   end W_Component_Classifier_Term;

   procedure W_Reference_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Reference_Term",
         "Node_Id",
         Image (Reference_Term (N)),
         Int (Reference_Term (N)));
   end W_Reference_Term;

   procedure W_Record_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("List_Items",
         "List_Id",
         Image (List_Items (N)),
         Int (List_Items (N)));
   end W_Record_Term;

   procedure W_Record_Term_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Property_Expression",
         "Node_Id",
         Image (Property_Expression (N)),
         Int (Property_Expression (N)));
   end W_Record_Term_Element;

   procedure W_Computed_Term (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
   end W_Computed_Term;

   procedure W_Boolean_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_Boolean_Type;

   procedure W_String_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
   end W_String_Type;

   procedure W_Real_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Range",
         "Node_Id",
         Image (Type_Range (N)),
         Int (Type_Range (N)));
      W_Node_Attribute
        ("Unit_Designator",
         "Node_Id",
         Image (Unit_Designator (N)),
         Int (Unit_Designator (N)));
   end W_Real_Type;

   procedure W_Integer_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Type_Range",
         "Node_Id",
         Image (Type_Range (N)),
         Int (Type_Range (N)));
      W_Node_Attribute
        ("Unit_Designator",
         "Node_Id",
         Image (Unit_Designator (N)),
         Int (Unit_Designator (N)));
   end W_Integer_Type;

   procedure W_Enumeration_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
      W_Node_Attribute
        ("Enumeration_Context",
         "Node_Id",
         Image (Enumeration_Context (N)),
         Int (Enumeration_Context (N)));
   end W_Enumeration_Type;

   procedure W_Number_Range (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Lower_Bound",
         "Node_Id",
         Image (Lower_Bound (N)),
         Int (Lower_Bound (N)));
      W_Node_Attribute
        ("Upper_Bound",
         "Node_Id",
         Image (Upper_Bound (N)),
         Int (Upper_Bound (N)));
   end W_Number_Range;

   procedure W_Unit_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Unit_Identifier",
         "Node_Id",
         Image (Unit_Identifier (N)),
         Int (Unit_Identifier (N)));
      W_Node_Attribute
        ("Numeric_Literal",
         "Node_Id",
         Image (Numeric_Literal (N)),
         Int (Numeric_Literal (N)));
   end W_Unit_Definition;

   procedure W_Units_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Base_Identifier",
         "Node_Id",
         Image (Base_Identifier (N)),
         Int (Base_Identifier (N)));
      W_Node_Attribute
        ("Unit_Definitions",
         "List_Id",
         Image (Unit_Definitions (N)),
         Int (Unit_Definitions (N)));
      W_Node_Attribute
        ("Units_Context",
         "Node_Id",
         Image (Units_Context (N)),
         Int (Units_Context (N)));
   end W_Units_Type;

   procedure W_Range_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Number_Type",
         "Node_Id",
         Image (Number_Type (N)),
         Int (Number_Type (N)));
   end W_Range_Type;

   procedure W_Classifier_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("List_Items",
         "List_Id",
         Image (List_Items (N)),
         Int (List_Items (N)));
   end W_Classifier_Type;

   procedure W_Classifier_Category_Ref (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Component_Cat",
         "Byte",
         Image (Component_Cat (N)));
      W_Node_Attribute
        ("Classifier_Ref",
         "Node_Id",
         Image (Classifier_Ref (N)),
         Int (Classifier_Ref (N)));
   end W_Classifier_Category_Ref;

   procedure W_Referable_Element_Category (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Component_Cat",
         "Byte",
         Image (Component_Cat (N)));
   end W_Referable_Element_Category;

   procedure W_Reference_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("List_Items",
         "List_Id",
         Image (List_Items (N)),
         Int (List_Items (N)));
   end W_Reference_Type;

   procedure W_Reference_Category (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Category",
         "Byte",
         Image (Category (N)));
      W_Node_Attribute
        ("Component_Cat",
         "Byte",
         Image (Component_Cat (N)));
      W_Node_Attribute
        ("Classifier_Ref",
         "Node_Id",
         Image (Classifier_Ref (N)),
         Int (Classifier_Ref (N)));
   end W_Reference_Category;

   procedure W_Record_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("List_Items",
         "List_Id",
         Image (List_Items (N)),
         Int (List_Items (N)));
   end W_Record_Type;

   procedure W_Record_Type_Element (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_List",
         "Boolean",
         Image (Is_List (N)));
      W_Node_Attribute
        ("Property_Type_Designator",
         "Node_Id",
         Image (Property_Type_Designator (N)),
         Int (Property_Type_Designator (N)));
   end W_Record_Type_Element;

   procedure W_Unique_Property_Type_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Property_Set_Identifier",
         "Node_Id",
         Image (Property_Set_Identifier (N)),
         Int (Property_Set_Identifier (N)));
   end W_Unique_Property_Type_Identifier;

   procedure W_Applies_To (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Is_All",
         "Boolean",
         Image (Is_All (N)));
      W_Node_Attribute
        ("Owner_Categories",
         "List_Id",
         Image (Owner_Categories (N)),
         Int (Owner_Categories (N)));
   end W_Applies_To;

   procedure W_Unique_Property_Const_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Property_Set_Identifier",
         "Node_Id",
         Image (Property_Set_Identifier (N)),
         Int (Property_Set_Identifier (N)));
   end W_Unique_Property_Const_Identifier;

   procedure W_Annex_Content (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Raw_Text",
         "Name_Id",
         Image (Raw_Text (N)));
   end W_Annex_Content;

   procedure W_Annex_Subclause (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
      W_Node_Attribute
        ("In_Modes",
         "Node_Id",
         Image (In_Modes (N)),
         Int (In_Modes (N)));
      W_Node_Attribute
        ("Annex_Content",
         "Node_Id",
         Image (Annex_Content (N)),
         Int (Annex_Content (N)));
      W_Node_Attribute
        ("Corresponding_Annex",
         "Node_Id",
         Image (Corresponding_Annex (N)),
         Int (Corresponding_Annex (N)));
   end W_Annex_Subclause;

   procedure W_Annex_Library (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Is_Private",
         "Boolean",
         Image (Is_Private (N)));
      W_Node_Attribute
        ("Annex_Content",
         "Node_Id",
         Image (Annex_Content (N)),
         Int (Annex_Content (N)));
      W_Node_Attribute
        ("Corresponding_Annex",
         "Node_Id",
         Image (Corresponding_Annex (N)),
         Int (Corresponding_Annex (N)));
      W_Node_Attribute
        ("Container_Package",
         "Node_Id",
         Image (Container_Package (N)),
         Int (Container_Package (N)));
   end W_Annex_Library;

   procedure W_Annex_Path (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Identifiers",
         "List_Id",
         Image (Identifiers (N)),
         Int (Identifiers (N)));
      W_Node_Attribute
        ("Container_Component",
         "Node_Id",
         Image (Container_Component (N)),
         Int (Container_Component (N)));
   end W_Annex_Path;

   procedure W_Array_Dimensions (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Array_List_Dim",
         "List_Id",
         Image (Array_List_Dim (N)),
         Int (Array_List_Dim (N)));
   end W_Array_Dimensions;

   procedure W_Array_Dimension_Size (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Size",
         "Node_Id",
         Image (Size (N)),
         Int (Size (N)));
   end W_Array_Dimension_Size;

   procedure W_Array_Selection (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Next_Entity",
         "Node_Id",
         Image (Next_Entity (N)),
         Int (Next_Entity (N)));
      W_Node_Attribute
        ("Identifier",
         "Node_Id",
         Image (Identifier (N)),
         Int (Identifier (N)));
      W_Node_Attribute
        ("Range_Selections",
         "List_Id",
         Image (Range_Selections (N)),
         Int (Range_Selections (N)));
   end W_Array_Selection;

   procedure W_Range_Selection (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Lower_Bound",
         "Node_Id",
         Image (Lower_Bound (N)),
         Int (Lower_Bound (N)));
      W_Node_Attribute
        ("Upper_Bound",
         "Node_Id",
         Image (Upper_Bound (N)),
         Int (Upper_Bound (N)));
   end W_Range_Selection;

   procedure W_Node_Container (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Item",
         "Node_Id",
         Image (Item (N)),
         Int (Item (N)));
      W_Node_Attribute
        ("Extra_Item",
         "Node_Id",
         Image (Extra_Item (N)),
         Int (Extra_Item (N)));
   end W_Node_Container;

end Ocarina.ME_AADL.AADL_Tree.Nodes;
