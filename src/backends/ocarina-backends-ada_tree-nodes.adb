pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.Backends.Ada_Tree.Debug; use Ocarina.Backends.Ada_Tree.Debug;

package body Ocarina.Backends.Ada_Tree.Nodes is

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
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Explicit_Dereference
        or else Table (Types.Node_Id (N)).Kind = K_Used_Type
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Used_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Exit_When_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Null_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Raise_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Private_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Record_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part
        or else Table (Types.Node_Id (N)).Kind = K_Variant
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Object_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Explicit_Dereference
        or else Table (Types.Node_Id (N)).Kind = K_Used_Type
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Used_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Exit_When_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Null_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Raise_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Private_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Record_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part
        or else Table (Types.Node_Id (N)).Kind = K_Variant
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Object_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Next_Node;

   function Frontend_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Explicit_Dereference
        or else Table (Types.Node_Id (N)).Kind = K_Used_Type
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Used_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Exit_When_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Null_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Raise_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Private_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Record_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part
        or else Table (Types.Node_Id (N)).Kind = K_Variant
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Object_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Frontend_Node;

   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Explicit_Dereference
        or else Table (Types.Node_Id (N)).Kind = K_Used_Type
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Used_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Exit_When_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Null_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Raise_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Private_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Record_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part
        or else Table (Types.Node_Id (N)).Kind = K_Variant
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Array_Aggregate
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion
        or else Table (Types.Node_Id (N)).Kind = K_Object_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Frontend_Node;

   function Defining_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Defining_Identifier;

   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Defining_Identifier;

   function Parent (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Parent;

   procedure Set_Parent (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Parent;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Package_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Component_List
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraints
        or else Table (Types.Node_Id (N)).Kind = K_Variant_List
        or else Table (Types.Node_Id (N)).Kind = K_Discrete_Choice_List
        or else Table (Types.Node_Id (N)).Kind = K_Element_List);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Package_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Component_List
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraints
        or else Table (Types.Node_Id (N)).Kind = K_Variant_List
        or else Table (Types.Node_Id (N)).Kind = K_Discrete_Choice_List
        or else Table (Types.Node_Id (N)).Kind = K_Element_List);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Package_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Component_List
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraints
        or else Table (Types.Node_Id (N)).Kind = K_Variant_List
        or else Table (Types.Node_Id (N)).Kind = K_Discrete_Choice_List
        or else Table (Types.Node_Id (N)).Kind = K_Element_List);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Declaration_List
        or else Table (Types.Node_Id (N)).Kind = K_Statement_List
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Package_Headers
        or else Table (Types.Node_Id (N)).Kind = K_Packages
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Profile
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Literals
        or else Table (Types.Node_Id (N)).Kind = K_Component_List
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraints
        or else Table (Types.Node_Id (N)).Kind = K_Variant_List
        or else Table (Types.Node_Id (N)).Kind = K_Discrete_Choice_List
        or else Table (Types.Node_Id (N)).Kind = K_Element_List);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Name;

   function Corresponding_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Corresponding_Node;

   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Corresponding_Node;

   function Parent_Unit_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Parent_Unit_Name;

   procedure Set_Parent_Unit_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Parent_Unit_Name;

   function Is_All (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_All;

   procedure Set_Is_All (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_All;

   function Prefix (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Explicit_Dereference
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Prefix;

   procedure Set_Prefix (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Explicit_Dereference
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Prefix;

   function The_Used_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Used_Type
        or else Table (Types.Node_Id (N)).Kind = K_Used_Package);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end The_Used_Entity;

   procedure Set_The_Used_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Used_Type
        or else Table (Types.Node_Id (N)).Kind = K_Used_Package);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_The_Used_Entity;

   function Used (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Used;

   procedure Set_Used (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Used;

   function Warnings_Off (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Warnings_Off;

   procedure Set_Warnings_Off (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Warnings_Off;

   function Elaborated (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Elaborated;

   procedure Set_Elaborated (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Withed_Package);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Elaborated;

   function Package_Declaration (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Package_Declaration;

   procedure Set_Package_Declaration (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Package_Declaration;

   function Withed_Packages (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (8));
   end Withed_Packages;

   procedure Set_Withed_Packages (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Withed_Packages;

   function Visible_Part (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec);

      return List_Id (Table (Types.Node_Id (N)).L (9));
   end Visible_Part;

   procedure Set_Visible_Part (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Visible_Part;

   function Private_Part (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec);

      return List_Id (Table (Types.Node_Id (N)).L (10));
   end Private_Part;

   procedure Set_Private_Part (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Private_Part;

   function Is_Runtime_Package (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Runtime_Package;

   procedure Set_Is_Runtime_Package (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Runtime_Package;

   function Is_Subunit_Package (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Subunit_Package;

   procedure Set_Is_Subunit_Package (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Subunit_Package;

   function Is_Instantiated_Package (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Instantiated_Package;

   procedure Set_Is_Instantiated_Package (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Instantiated_Package;

   function Package_Instantiation (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Package_Instantiation;

   procedure Set_Package_Instantiation (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Package_Instantiation;

   function Package_Headers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (11));
   end Package_Headers;

   procedure Set_Package_Headers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Package_Headers;

   function Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Declarations;

   procedure Set_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Declarations;

   function Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Statements;

   procedure Set_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Body
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Loop_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Statements;

   function Package_Initialization (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Package_Initialization;

   procedure Set_Package_Initialization (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Implementation);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Package_Initialization;

   function Distributed_Application_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Distributed_Application_Unit;

   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Distributed_Application_Unit;

   function Package_Specification (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Package_Specification;

   procedure Set_Package_Specification (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Package_Specification;

   function Package_Implementation (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Package_Implementation;

   procedure Set_Package_Implementation (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Package_Implementation;

   function Has_Custom_File_Name (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Has_Custom_File_Name;

   procedure Set_Has_Custom_File_Name (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Has_Custom_File_Name;

   function File_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      return Name_Id (Table (Types.Node_Id (N)).L (9));
   end File_Name;

   procedure Set_File_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_File_Name;

   function Subprogram_Specification (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Subprogram_Specification;

   procedure Set_Subprogram_Specification (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Subprogram_Specification;

   function Subprogram_Implementation (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Subprogram_Implementation;

   procedure Set_Subprogram_Implementation (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Main_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Subprogram_Implementation;

   function QoS_Nodes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end QoS_Nodes;

   procedure Set_QoS_Nodes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Distributed_Application);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_QoS_Nodes;

   function Units (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Units;

   procedure Set_Units (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Units;

   function Distributed_Application (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Distributed_Application;

   procedure Set_Distributed_Application (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Node
        or else Table (Types.Node_Id (N)).Kind = K_HI_Node);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Distributed_Application;

   function HI_Nodes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end HI_Nodes;

   procedure Set_HI_Nodes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Distributed_Application);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_HI_Nodes;

   function Main_Subprogram (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Main_Subprogram;

   procedure Set_Main_Subprogram (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Main_Subprogram;

   function Packages (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Packages;

   procedure Set_Packages (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Packages;

   function Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Entity;

   procedure Set_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_API_Unit
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Entity;

   function Helpers_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Helpers_Package;

   procedure Set_Helpers_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Helpers_Package;

   function Servants_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Servants_Package;

   procedure Set_Servants_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Servants_Package;

   function Parameters_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Parameters_Package;

   procedure Set_Parameters_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Parameters_Package;

   function Obj_Adapters_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Obj_Adapters_Package;

   procedure Set_Obj_Adapters_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Obj_Adapters_Package;

   function Setup_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Setup_Package;

   procedure Set_Setup_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Setup_Package;

   function Namespaces_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Namespaces_Package;

   procedure Set_Namespaces_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Unit);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Namespaces_Package;

   function Marshallers_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Marshallers_Package;

   procedure Set_Marshallers_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Marshallers_Package;

   function Activity_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Activity_Package;

   procedure Set_Activity_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Activity_Package;

   function Subprograms_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Subprograms_Package;

   procedure Set_Subprograms_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Subprograms_Package;

   function Transport_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Transport_Package;

   procedure Set_Transport_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Transport_Package;

   function Types_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Types_Package;

   procedure Set_Types_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Types_Package;

   function Deployment_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Deployment_Package;

   procedure Set_Deployment_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Deployment_Package;

   function Naming_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Naming_Package;

   procedure Set_Naming_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Unit);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Naming_Package;

   function Parameter_Mode (N : Node_Id) return Mode_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      return Mode_Id (Table (Types.Node_Id (N)).O (1));
   end Parameter_Mode;

   procedure Set_Parameter_Mode (N : Node_Id; V : Mode_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Parameter_Mode;

   function Parameter_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Parameter_Type;

   procedure Set_Parameter_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Parameter_Type;

   function Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Expression;

   procedure Set_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Component_Association
        or else Table (Types.Node_Id (N)).Kind = K_Assignment_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Return_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Expression;

   function Parameter_Profile (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Parameter_Profile;

   procedure Set_Parameter_Profile (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Parameter_Profile;

   function Return_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Return_Type;

   procedure Set_Return_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Return_Type;

   function Renamed_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Renamed_Entity;

   procedure Set_Renamed_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Exception_Declaration);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Renamed_Entity;

   function Instantiated_Entity (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Instantiated_Entity;

   procedure Set_Instantiated_Entity (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Instantiated_Entity;

   function Main_Subprogram_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Main_Subprogram_Unit;

   procedure Set_Main_Subprogram_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Specification
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Main_Subprogram_Unit;

   function Specification (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Specification;

   procedure Set_Specification (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Implementation);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Specification;

   function Actual_Parameter_Part (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Actual_Parameter_Part;

   procedure Set_Actual_Parameter_Part (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Subprogram_Call);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Actual_Parameter_Part;

   function Selector_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Selector_Name;

   procedure Set_Selector_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association
        or else Table (Types.Node_Id (N)).Kind = K_Selected_Component);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Selector_Name;

   function Actual_Parameter (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Actual_Parameter;

   procedure Set_Actual_Parameter (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Parameter_Association);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Actual_Parameter;

   function Type_Definition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Type_Definition;

   procedure Set_Type_Definition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Type_Definition;

   function Discriminant_Spec (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Discriminant_Spec;

   procedure Set_Discriminant_Spec (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Discriminant_Spec;

   function Is_Subtype (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Subtype;

   procedure Set_Is_Subtype (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Full_Type_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Subtype;

   function Attribute_Designator (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Attribute_Designator;

   procedure Set_Attribute_Designator (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Attribute_Definition_Clause);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Attribute_Designator;

   function Enumeration_Literals (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Enumeration_Literals;

   procedure Set_Enumeration_Literals (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Type_Definition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Enumeration_Literals;

   function Array_Aggregate (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Array_Aggregate;

   procedure Set_Array_Aggregate (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumeration_Representation_Clause);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Array_Aggregate;

   function Scale (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Scale;

   procedure Set_Scale (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Scale;

   function Total (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition);

      return Value_Id (Table (Types.Node_Id (N)).L (2));
   end Total;

   procedure Set_Total (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Decimal_Type_Definition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Total;

   function Component_Association_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Aggregate);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Component_Association_List;

   procedure Set_Component_Association_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Aggregate);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Component_Association_List;

   function Is_Type (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Type;

   procedure Set_Is_Type (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Protected_Object_Spec);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Type;

   function Declarative_Part (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Declarative_Part;

   procedure Set_Declarative_Part (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Declarative_Part;

   function Exception_Handler (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Exception_Handler;

   procedure Set_Exception_Handler (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Block_Statement);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Exception_Handler;

   function Condition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Exit_When_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Condition;

   procedure Set_Condition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement
        or else Table (Types.Node_Id (N)).Kind = K_Exit_When_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Condition;

   function Then_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Then_Statements;

   procedure Set_Then_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Elsif_Statement
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Then_Statements;

   function Elsif_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Elsif_Statements;

   procedure Set_Elsif_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Elsif_Statements;

   function Else_Statements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Else_Statements;

   procedure Set_Else_Statements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_If_Statement);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Else_Statements;

   function Is_Until (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Until;

   procedure Set_Is_Until (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Delay_Statement);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Until;

   function Range_Constraint (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Range_Constraint;

   procedure Set_Range_Constraint (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_For_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Range_Constraint;

   function Discret_Choice_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Discret_Choice_List;

   procedure Set_Discret_Choice_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement_Alternative);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Discret_Choice_List;

   function Case_Statement_Alternatives (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Case_Statement_Alternatives;

   procedure Set_Case_Statement_Alternatives (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Case_Statement_Alternatives;

   function Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Value;

   procedure Set_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Case_Label
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value;

   function Argument_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Argument_List;

   procedure Set_Argument_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Pragma_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Argument_List;

   function Generic_Package (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Generic_Package;

   procedure Set_Generic_Package (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Generic_Package;

   function Parameter_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Parameter_List;

   procedure Set_Parameter_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Package_Instantiation);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Parameter_List;

   function Raised_Error (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Raise_Statement);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Raised_Error;

   procedure Set_Raised_Error (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Raise_Statement);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Raised_Error;

   function Has_Header_Spaces (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Has_Header_Spaces;

   procedure Set_Has_Header_Spaces (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Ada_Comment);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Has_Header_Spaces;

   function Is_Constant (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Constant;

   procedure Set_Is_Constant (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Constant;

   function Is_Not_Null (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Not_Null;

   procedure Set_Is_Not_Null (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Not_Null;

   function Subtype_Indication (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Subtype_Indication;

   procedure Set_Subtype_Indication (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Access_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Subtype_Indication;

   function Is_Private_Extention (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Private_Extention;

   procedure Set_Is_Private_Extention (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Private_Extention;

   function Is_Abstract_Type (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Abstract_Type;

   procedure Set_Is_Abstract_Type (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Abstract_Type;

   function Record_Extension_Part (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Record_Extension_Part;

   procedure Set_Record_Extension_Part (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Derived_Type_Definition);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Record_Extension_Part;

   function Is_Tagged_Type (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Tagged_Type;

   procedure Set_Is_Tagged_Type (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Tagged_Type;

   function Is_Limited_Type (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Limited_Type;

   procedure Set_Is_Limited_Type (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Limited_Type;

   function Record_Definition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Record_Definition;

   procedure Set_Record_Definition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Type_Definition);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Record_Definition;

   function Aliased_Present (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Aliased_Present;

   procedure Set_Aliased_Present (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Component_Declaration
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Aliased_Present;

   function Component_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Variant);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Component_List;

   procedure Set_Component_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Record_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Variant);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Component_List;

   function Range_Constraints (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Range_Constraints;

   procedure Set_Range_Constraints (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Range_Constraints;

   function Component_Definition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Component_Definition;

   procedure Set_Component_Definition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Type_Definition);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Component_Definition;

   function First (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First;

   procedure Set_First (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First;

   function Last (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last;

   procedure Set_Last (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last;

   function Index_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Index_Type;

   procedure Set_Index_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Range_Constraint);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Index_Type;

   function Variants (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Variants;

   procedure Set_Variants (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Variants;

   function Discriminant (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Discriminant;

   procedure Set_Discriminant (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variant_Part);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Discriminant;

   function Discrete_Choices (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variant);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end Discrete_Choices;

   procedure Set_Discrete_Choices (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Variant);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Discrete_Choices;

   function Constant_Present (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Constant_Present;

   procedure Set_Constant_Present (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Constant_Present;

   function Object_Definition (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Object_Definition;

   procedure Set_Object_Definition (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Object_Declaration);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Object_Definition;

   function Parent_Designator (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Parent_Designator;

   procedure Set_Parent_Designator (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Parent_Designator;

   function Index (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Index;

   procedure Set_Index (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Element_Association);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Index;

   function Elements (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Aggregate);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Elements;

   procedure Set_Elements (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Array_Aggregate);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Elements;

   function Expressions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Expressions;

   procedure Set_Expressions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Indexed_Component);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Expressions;

   function Operator (N : Node_Id) return Operator_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      return Operator_Id (Table (Types.Node_Id (N)).O (1));
   end Operator;

   procedure Set_Operator (N : Node_Id; V : Operator_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      Table (Types.Node_Id (N)).O (1) := Byte (V);
   end Set_Operator;

   function Left_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Left_Expr;

   procedure Set_Left_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Left_Expr;

   function Right_Expr (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Right_Expr;

   procedure Set_Right_Expr (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Expression);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Right_Expr;

   function Subtype_Mark (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Subtype_Mark;

   procedure Set_Subtype_Mark (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression
        or else Table (Types.Node_Id (N)).Kind = K_Type_Conversion);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Subtype_Mark;

   function Aggregate (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Aggregate;

   procedure Set_Aggregate (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Qualified_Expression);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Aggregate;

   function Qualified_Expression (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Object_Instantiation);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Qualified_Expression;

   procedure Set_Qualified_Expression (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Object_Instantiation);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Qualified_Expression;

   function Image (N : Base_Type) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Boolean
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Integer
        or else Table (Types.Node_Id (N)).Kind = K_String
        or else Table (Types.Node_Id (N)).Kind = K_Wide_String
        or else Table (Types.Node_Id (N)).Kind = K_Character
        or else Table (Types.Node_Id (N)).Kind = K_Wide_Character);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Image;

   procedure Set_Image (N : Base_Type; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Boolean
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Integer
        or else Table (Types.Node_Id (N)).Kind = K_String
        or else Table (Types.Node_Id (N)).Kind = K_Wide_String
        or else Table (Types.Node_Id (N)).Kind = K_Character
        or else Table (Types.Node_Id (N)).Kind = K_Wide_Character);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Image;

   function Main_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Main_Node;

   procedure Set_Main_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Main_Node;

   function Type_Definition_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Type_Definition_Node;

   procedure Set_Type_Definition_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Type_Definition_Node;

   function Feature_Subprogram_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Feature_Subprogram_Node;

   procedure Set_Feature_Subprogram_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Feature_Subprogram_Node;

   function Subprogram_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Subprogram_Node;

   procedure Set_Subprogram_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Subprogram_Node;

   function Helpers_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Helpers_Node;

   procedure Set_Helpers_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Helpers_Node;

   function Servants_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Servants_Node;

   procedure Set_Servants_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Servants_Node;

   function Parameters_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Parameters_Node;

   procedure Set_Parameters_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Parameters_Node;

   function Obj_Adapters_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Obj_Adapters_Node;

   procedure Set_Obj_Adapters_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Obj_Adapters_Node;

   function Setup_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Setup_Node;

   procedure Set_Setup_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Setup_Node;

   function Namespaces_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Namespaces_Node;

   procedure Set_Namespaces_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Namespaces_Node;

   function TypeCode_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end TypeCode_Node;

   procedure Set_TypeCode_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_TypeCode_Node;

   function From_Any_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end From_Any_Node;

   procedure Set_From_Any_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_From_Any_Node;

   function To_Any_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end To_Any_Node;

   procedure Set_To_Any_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_To_Any_Node;

   function Initialize_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Initialize_Node;

   procedure Set_Initialize_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Initialize_Node;

   function Thread_Controller_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (17));
   end Thread_Controller_Node;

   procedure Set_Thread_Controller_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (17) := Int (V);
   end Set_Thread_Controller_Node;

   function Execute_Servant_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (18));
   end Execute_Servant_Node;

   procedure Set_Execute_Servant_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (18) := Int (V);
   end Set_Execute_Servant_Node;

   function Put_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (19));
   end Put_Node;

   procedure Set_Put_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (19) := Int (V);
   end Set_Put_Node;

   function Push_Back_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (20));
   end Push_Back_Node;

   procedure Set_Push_Back_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (20) := Int (V);
   end Set_Push_Back_Node;

   function Get_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (21));
   end Get_Node;

   procedure Set_Get_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (21) := Int (V);
   end Set_Get_Node;

   function Package_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (22));
   end Package_Node;

   procedure Set_Package_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (22) := Int (V);
   end Set_Package_Node;

   function Reference_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (23));
   end Reference_Node;

   procedure Set_Reference_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (23) := Int (V);
   end Set_Reference_Node;

   function Set_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (24));
   end Set_Node;

   procedure Set_Set_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (24) := Int (V);
   end Set_Set_Node;

   function Build_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (25));
   end Build_Node;

   procedure Set_Build_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_QoS_Tree_Bindings);

      Table (Types.Node_Id (N)).L (25) := Int (V);
   end Set_Build_Node;

   function Marshallers_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Marshallers_Node;

   procedure Set_Marshallers_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Marshallers_Node;

   function Activity_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Activity_Node;

   procedure Set_Activity_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Activity_Node;

   function Subprograms_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (9));
   end Subprograms_Node;

   procedure Set_Subprograms_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Subprograms_Node;

   function Types_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (10));
   end Types_Node;

   procedure Set_Types_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Types_Node;

   function Transport_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Transport_Node;

   procedure Set_Transport_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Transport_Node;

   function Deployment_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Deployment_Node;

   procedure Set_Deployment_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Deployment_Node;

   function Naming_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end Naming_Node;

   procedure Set_Naming_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Naming_Node;

   function Job_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end Job_Node;

   procedure Set_Job_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Job_Node;

   function Enumerator_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Enumerator_Node;

   procedure Set_Enumerator_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Enumerator_Node;

   function Marshall_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Marshall_Node;

   procedure Set_Marshall_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Marshall_Node;

   function Unmarshall_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (17));
   end Unmarshall_Node;

   procedure Set_Unmarshall_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (17) := Int (V);
   end Set_Unmarshall_Node;

   function Port_Interface_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (18));
   end Port_Interface_Node;

   procedure Set_Port_Interface_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (18) := Int (V);
   end Set_Port_Interface_Node;

   function Port_Enumeration_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (19));
   end Port_Enumeration_Node;

   procedure Set_Port_Enumeration_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (19) := Int (V);
   end Set_Port_Enumeration_Node;

   function Deliver_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (20));
   end Deliver_Node;

   procedure Set_Deliver_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (20) := Int (V);
   end Set_Deliver_Node;

   function Send_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (21));
   end Send_Node;

   procedure Set_Send_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (21) := Int (V);
   end Set_Send_Node;

   function Put_Value_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (22));
   end Put_Value_Node;

   procedure Set_Put_Value_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (22) := Int (V);
   end Set_Put_Value_Node;

   function Get_Value_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (23));
   end Get_Value_Node;

   procedure Set_Get_Value_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (23) := Int (V);
   end Set_Get_Value_Node;

   function Get_Count_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (24));
   end Get_Count_Node;

   procedure Set_Get_Count_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (24) := Int (V);
   end Set_Get_Count_Node;

   function Next_Value_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (25));
   end Next_Value_Node;

   procedure Set_Next_Value_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (25) := Int (V);
   end Set_Next_Value_Node;

   function Store_Received_Message_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (26));
   end Store_Received_Message_Node;

   procedure Set_Store_Received_Message_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (26) := Int (V);
   end Set_Store_Received_Message_Node;

   function Default_Value_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (27));
   end Default_Value_Node;

   procedure Set_Default_Value_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (27) := Int (V);
   end Set_Default_Value_Node;

   function Object_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (28));
   end Object_Node;

   procedure Set_Object_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (28) := Int (V);
   end Set_Object_Node;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Definition =>
            W_Definition
              (Node_Id (N));
         when K_Defining_Identifier =>
            W_Defining_Identifier
              (Node_Id (N));
         when K_Designator =>
            W_Designator
              (Node_Id (N));
         when K_Attribute_Designator =>
            W_Attribute_Designator
              (Node_Id (N));
         when K_Explicit_Dereference =>
            W_Explicit_Dereference
              (Node_Id (N));
         when K_Used_Type =>
            W_Used_Type
              (Node_Id (N));
         when K_Declaration_List =>
            W_Declaration_List
              (List_Id (N));
         when K_Statement_List =>
            W_Statement_List
              (List_Id (N));
         when K_Withed_Package =>
            W_Withed_Package
              (Node_Id (N));
         when K_Used_Package =>
            W_Used_Package
              (Node_Id (N));
         when K_Withed_Packages =>
            W_Withed_Packages
              (List_Id (N));
         when K_Package_Headers =>
            W_Package_Headers
              (List_Id (N));
         when K_Package_Specification =>
            W_Package_Specification
              (Node_Id (N));
         when K_Package_Implementation =>
            W_Package_Implementation
              (Node_Id (N));
         when K_Package_Declaration =>
            W_Package_Declaration
              (Node_Id (N));
         when K_Main_Subprogram_Implementation =>
            W_Main_Subprogram_Implementation
              (Node_Id (N));
         when K_Packages =>
            W_Packages
              (List_Id (N));
         when K_QoS_Distributed_Application =>
            W_QoS_Distributed_Application
              (Node_Id (N));
         when K_QoS_Node =>
            W_QoS_Node
              (Node_Id (N));
         when K_HI_Distributed_Application =>
            W_HI_Distributed_Application
              (Node_Id (N));
         when K_HI_Node =>
            W_HI_Node
              (Node_Id (N));
         when K_API_Unit =>
            W_API_Unit
              (Node_Id (N));
         when K_QoS_Unit =>
            W_QoS_Unit
              (Node_Id (N));
         when K_HI_Unit =>
            W_HI_Unit
              (Node_Id (N));
         when K_Parameter_Specification =>
            W_Parameter_Specification
              (Node_Id (N));
         when K_Parameter_Profile =>
            W_Parameter_Profile
              (List_Id (N));
         when K_Subprogram_Specification =>
            W_Subprogram_Specification
              (Node_Id (N));
         when K_Subprogram_Implementation =>
            W_Subprogram_Implementation
              (Node_Id (N));
         when K_Subprogram_Call =>
            W_Subprogram_Call
              (Node_Id (N));
         when K_Parameter_Association =>
            W_Parameter_Association
              (Node_Id (N));
         when K_Selected_Component =>
            W_Selected_Component
              (Node_Id (N));
         when K_Full_Type_Declaration =>
            W_Full_Type_Declaration
              (Node_Id (N));
         when K_Attribute_Definition_Clause =>
            W_Attribute_Definition_Clause
              (Node_Id (N));
         when K_Enumeration_Literals =>
            W_Enumeration_Literals
              (List_Id (N));
         when K_Enumeration_Type_Definition =>
            W_Enumeration_Type_Definition
              (Node_Id (N));
         when K_Enumeration_Representation_Clause =>
            W_Enumeration_Representation_Clause
              (Node_Id (N));
         when K_Decimal_Type_Definition =>
            W_Decimal_Type_Definition
              (Node_Id (N));
         when K_Record_Aggregate =>
            W_Record_Aggregate
              (Node_Id (N));
         when K_Component_Association =>
            W_Component_Association
              (Node_Id (N));
         when K_Protected_Object_Spec =>
            W_Protected_Object_Spec
              (Node_Id (N));
         when K_Protected_Object_Body =>
            W_Protected_Object_Body
              (Node_Id (N));
         when K_Block_Statement =>
            W_Block_Statement
              (Node_Id (N));
         when K_Elsif_Statement =>
            W_Elsif_Statement
              (Node_Id (N));
         when K_If_Statement =>
            W_If_Statement
              (Node_Id (N));
         when K_Exit_When_Statement =>
            W_Exit_When_Statement
              (Node_Id (N));
         when K_Assignment_Statement =>
            W_Assignment_Statement
              (Node_Id (N));
         when K_Delay_Statement =>
            W_Delay_Statement
              (Node_Id (N));
         when K_Return_Statement =>
            W_Return_Statement
              (Node_Id (N));
         when K_For_Statement =>
            W_For_Statement
              (Node_Id (N));
         when K_Loop_Statement =>
            W_Loop_Statement
              (Node_Id (N));
         when K_Case_Statement_Alternative =>
            W_Case_Statement_Alternative
              (Node_Id (N));
         when K_Case_Statement =>
            W_Case_Statement
              (Node_Id (N));
         when K_Case_Label =>
            W_Case_Label
              (Node_Id (N));
         when K_Pragma_Statement =>
            W_Pragma_Statement
              (Node_Id (N));
         when K_Null_Statement =>
            W_Null_Statement
              (Node_Id (N));
         when K_Package_Instantiation =>
            W_Package_Instantiation
              (Node_Id (N));
         when K_Raise_Statement =>
            W_Raise_Statement
              (Node_Id (N));
         when K_Ada_Comment =>
            W_Ada_Comment
              (Node_Id (N));
         when K_Access_Type_Definition =>
            W_Access_Type_Definition
              (Node_Id (N));
         when K_Derived_Type_Definition =>
            W_Derived_Type_Definition
              (Node_Id (N));
         when K_Record_Type_Definition =>
            W_Record_Type_Definition
              (Node_Id (N));
         when K_Private_Type_Definition =>
            W_Private_Type_Definition
              (Node_Id (N));
         when K_Component_Declaration =>
            W_Component_Declaration
              (Node_Id (N));
         when K_Component_List =>
            W_Component_List
              (List_Id (N));
         when K_Record_Definition =>
            W_Record_Definition
              (Node_Id (N));
         when K_Range_Constraints =>
            W_Range_Constraints
              (List_Id (N));
         when K_Array_Type_Definition =>
            W_Array_Type_Definition
              (Node_Id (N));
         when K_Range_Constraint =>
            W_Range_Constraint
              (Node_Id (N));
         when K_Variant_List =>
            W_Variant_List
              (List_Id (N));
         when K_Variant_Part =>
            W_Variant_Part
              (Node_Id (N));
         when K_Discrete_Choice_List =>
            W_Discrete_Choice_List
              (List_Id (N));
         when K_Variant =>
            W_Variant
              (Node_Id (N));
         when K_Object_Declaration =>
            W_Object_Declaration
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_Element_Association =>
            W_Element_Association
              (Node_Id (N));
         when K_Element_List =>
            W_Element_List
              (List_Id (N));
         when K_Array_Aggregate =>
            W_Array_Aggregate
              (Node_Id (N));
         when K_Indexed_Component =>
            W_Indexed_Component
              (Node_Id (N));
         when K_Exception_Declaration =>
            W_Exception_Declaration
              (Node_Id (N));
         when K_Expression =>
            W_Expression
              (Node_Id (N));
         when K_Qualified_Expression =>
            W_Qualified_Expression
              (Node_Id (N));
         when K_Type_Conversion =>
            W_Type_Conversion
              (Node_Id (N));
         when K_Object_Instantiation =>
            W_Object_Instantiation
              (Node_Id (N));
         when K_Boolean =>
            W_Boolean
              (Base_Type (N));
         when K_Float =>
            W_Float
              (Base_Type (N));
         when K_Integer =>
            W_Integer
              (Base_Type (N));
         when K_String =>
            W_String
              (Base_Type (N));
         when K_Wide_String =>
            W_Wide_String
              (Base_Type (N));
         when K_Character =>
            W_Character
              (Base_Type (N));
         when K_Wide_Character =>
            W_Wide_Character
              (Base_Type (N));
         when K_Tree_Bindings =>
            W_Tree_Bindings
              (Node_Id (N));
         when K_QoS_Tree_Bindings =>
            W_QoS_Tree_Bindings
              (Node_Id (N));
         when K_HI_Tree_Bindings =>
            W_HI_Tree_Bindings
              (Node_Id (N));
         when others =>
            null;
      end case;
   end W_Node;

   procedure W_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
   end W_Definition;

   procedure W_Defining_Identifier (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Corresponding_Node",
         "Node_Id",
         Image (Corresponding_Node (N)),
         Int (Corresponding_Node (N)));
      W_Node_Attribute
        ("Parent_Unit_Name",
         "Node_Id",
         Image (Parent_Unit_Name (N)),
         Int (Parent_Unit_Name (N)));
   end W_Defining_Identifier;

   procedure W_Designator (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent_Unit_Name",
         "Node_Id",
         Image (Parent_Unit_Name (N)),
         Int (Parent_Unit_Name (N)));
      W_Node_Attribute
        ("Is_All",
         "Boolean",
         Image (Is_All (N)));
   end W_Designator;

   procedure W_Attribute_Designator (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Corresponding_Node",
         "Node_Id",
         Image (Corresponding_Node (N)),
         Int (Corresponding_Node (N)));
      W_Node_Attribute
        ("Parent_Unit_Name",
         "Node_Id",
         Image (Parent_Unit_Name (N)),
         Int (Parent_Unit_Name (N)));
      W_Node_Attribute
        ("Prefix",
         "Node_Id",
         Image (Prefix (N)),
         Int (Prefix (N)));
   end W_Attribute_Designator;

   procedure W_Explicit_Dereference (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Prefix",
         "Node_Id",
         Image (Prefix (N)),
         Int (Prefix (N)));
   end W_Explicit_Dereference;

   procedure W_Used_Type (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("The_Used_Entity",
         "Node_Id",
         Image (The_Used_Entity (N)),
         Int (The_Used_Entity (N)));
   end W_Used_Type;

   procedure W_Declaration_List (N : List_Id) is
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
   end W_Declaration_List;

   procedure W_Statement_List (N : List_Id) is
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
   end W_Statement_List;

   procedure W_Withed_Package (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Used",
         "Boolean",
         Image (Used (N)));
      W_Node_Attribute
        ("Warnings_Off",
         "Boolean",
         Image (Warnings_Off (N)));
      W_Node_Attribute
        ("Elaborated",
         "Boolean",
         Image (Elaborated (N)));
   end W_Withed_Package;

   procedure W_Used_Package (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("The_Used_Entity",
         "Node_Id",
         Image (The_Used_Entity (N)),
         Int (The_Used_Entity (N)));
   end W_Used_Package;

   procedure W_Withed_Packages (N : List_Id) is
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
   end W_Withed_Packages;

   procedure W_Package_Headers (N : List_Id) is
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
   end W_Package_Headers;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Package_Declaration",
         "Node_Id",
         Image (Package_Declaration (N)),
         Int (Package_Declaration (N)));
      W_Node_Attribute
        ("Withed_Packages",
         "List_Id",
         Image (Withed_Packages (N)),
         Int (Withed_Packages (N)));
      W_Node_Attribute
        ("Visible_Part",
         "List_Id",
         Image (Visible_Part (N)),
         Int (Visible_Part (N)));
      W_Node_Attribute
        ("Private_Part",
         "List_Id",
         Image (Private_Part (N)),
         Int (Private_Part (N)));
      W_Node_Attribute
        ("Is_Runtime_Package",
         "Boolean",
         Image (Is_Runtime_Package (N)));
      W_Node_Attribute
        ("Is_Subunit_Package",
         "Boolean",
         Image (Is_Subunit_Package (N)));
      W_Node_Attribute
        ("Is_Instantiated_Package",
         "Boolean",
         Image (Is_Instantiated_Package (N)));
      W_Node_Attribute
        ("Package_Instantiation",
         "Node_Id",
         Image (Package_Instantiation (N)),
         Int (Package_Instantiation (N)));
      W_Node_Attribute
        ("Package_Headers",
         "List_Id",
         Image (Package_Headers (N)),
         Int (Package_Headers (N)));
   end W_Package_Specification;

   procedure W_Package_Implementation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Package_Declaration",
         "Node_Id",
         Image (Package_Declaration (N)),
         Int (Package_Declaration (N)));
      W_Node_Attribute
        ("Withed_Packages",
         "List_Id",
         Image (Withed_Packages (N)),
         Int (Withed_Packages (N)));
      W_Node_Attribute
        ("Package_Headers",
         "List_Id",
         Image (Package_Headers (N)),
         Int (Package_Headers (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Package_Initialization",
         "List_Id",
         Image (Package_Initialization (N)),
         Int (Package_Initialization (N)));
   end W_Package_Implementation;

   procedure W_Package_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Distributed_Application_Unit",
         "Node_Id",
         Image (Distributed_Application_Unit (N)),
         Int (Distributed_Application_Unit (N)));
      W_Node_Attribute
        ("Package_Specification",
         "Node_Id",
         Image (Package_Specification (N)),
         Int (Package_Specification (N)));
      W_Node_Attribute
        ("Package_Implementation",
         "Node_Id",
         Image (Package_Implementation (N)),
         Int (Package_Implementation (N)));
      W_Node_Attribute
        ("Has_Custom_File_Name",
         "Boolean",
         Image (Has_Custom_File_Name (N)));
      W_Node_Attribute
        ("File_Name",
         "Name_Id",
         Image (File_Name (N)));
   end W_Package_Declaration;

   procedure W_Main_Subprogram_Implementation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Distributed_Application_Unit",
         "Node_Id",
         Image (Distributed_Application_Unit (N)),
         Int (Distributed_Application_Unit (N)));
      W_Node_Attribute
        ("Has_Custom_File_Name",
         "Boolean",
         Image (Has_Custom_File_Name (N)));
      W_Node_Attribute
        ("File_Name",
         "Name_Id",
         Image (File_Name (N)));
      W_Node_Attribute
        ("Subprogram_Specification",
         "Node_Id",
         Image (Subprogram_Specification (N)),
         Int (Subprogram_Specification (N)));
      W_Node_Attribute
        ("Subprogram_Implementation",
         "Node_Id",
         Image (Subprogram_Implementation (N)),
         Int (Subprogram_Implementation (N)));
   end W_Main_Subprogram_Implementation;

   procedure W_Packages (N : List_Id) is
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
   end W_Packages;

   procedure W_QoS_Distributed_Application (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("QoS_Nodes",
         "List_Id",
         Image (QoS_Nodes (N)),
         Int (QoS_Nodes (N)));
   end W_QoS_Distributed_Application;

   procedure W_QoS_Node (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Units",
         "List_Id",
         Image (Units (N)),
         Int (Units (N)));
      W_Node_Attribute
        ("Distributed_Application",
         "Node_Id",
         Image (Distributed_Application (N)),
         Int (Distributed_Application (N)));
   end W_QoS_Node;

   procedure W_HI_Distributed_Application (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Units",
         "List_Id",
         Image (Units (N)),
         Int (Units (N)));
      W_Node_Attribute
        ("HI_Nodes",
         "List_Id",
         Image (HI_Nodes (N)),
         Int (HI_Nodes (N)));
   end W_HI_Distributed_Application;

   procedure W_HI_Node (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Units",
         "List_Id",
         Image (Units (N)),
         Int (Units (N)));
      W_Node_Attribute
        ("Distributed_Application",
         "Node_Id",
         Image (Distributed_Application (N)),
         Int (Distributed_Application (N)));
   end W_HI_Node;

   procedure W_API_Unit (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Subprogram",
         "Node_Id",
         Image (Main_Subprogram (N)),
         Int (Main_Subprogram (N)));
      W_Node_Attribute
        ("Packages",
         "List_Id",
         Image (Packages (N)),
         Int (Packages (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
   end W_API_Unit;

   procedure W_QoS_Unit (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Subprogram",
         "Node_Id",
         Image (Main_Subprogram (N)),
         Int (Main_Subprogram (N)));
      W_Node_Attribute
        ("Packages",
         "List_Id",
         Image (Packages (N)),
         Int (Packages (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Helpers_Package",
         "Node_Id",
         Image (Helpers_Package (N)),
         Int (Helpers_Package (N)));
      W_Node_Attribute
        ("Servants_Package",
         "Node_Id",
         Image (Servants_Package (N)),
         Int (Servants_Package (N)));
      W_Node_Attribute
        ("Parameters_Package",
         "Node_Id",
         Image (Parameters_Package (N)),
         Int (Parameters_Package (N)));
      W_Node_Attribute
        ("Obj_Adapters_Package",
         "Node_Id",
         Image (Obj_Adapters_Package (N)),
         Int (Obj_Adapters_Package (N)));
      W_Node_Attribute
        ("Setup_Package",
         "Node_Id",
         Image (Setup_Package (N)),
         Int (Setup_Package (N)));
      W_Node_Attribute
        ("Namespaces_Package",
         "Node_Id",
         Image (Namespaces_Package (N)),
         Int (Namespaces_Package (N)));
   end W_QoS_Unit;

   procedure W_HI_Unit (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Subprogram",
         "Node_Id",
         Image (Main_Subprogram (N)),
         Int (Main_Subprogram (N)));
      W_Node_Attribute
        ("Packages",
         "List_Id",
         Image (Packages (N)),
         Int (Packages (N)));
      W_Node_Attribute
        ("Entity",
         "Node_Id",
         Image (Entity (N)),
         Int (Entity (N)));
      W_Node_Attribute
        ("Marshallers_Package",
         "Node_Id",
         Image (Marshallers_Package (N)),
         Int (Marshallers_Package (N)));
      W_Node_Attribute
        ("Activity_Package",
         "Node_Id",
         Image (Activity_Package (N)),
         Int (Activity_Package (N)));
      W_Node_Attribute
        ("Subprograms_Package",
         "Node_Id",
         Image (Subprograms_Package (N)),
         Int (Subprograms_Package (N)));
      W_Node_Attribute
        ("Transport_Package",
         "Node_Id",
         Image (Transport_Package (N)),
         Int (Transport_Package (N)));
      W_Node_Attribute
        ("Types_Package",
         "Node_Id",
         Image (Types_Package (N)),
         Int (Types_Package (N)));
      W_Node_Attribute
        ("Deployment_Package",
         "Node_Id",
         Image (Deployment_Package (N)),
         Int (Deployment_Package (N)));
      W_Node_Attribute
        ("Naming_Package",
         "Node_Id",
         Image (Naming_Package (N)),
         Int (Naming_Package (N)));
   end W_HI_Unit;

   procedure W_Parameter_Specification (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Parameter_Mode",
         "Mode_Id",
         Image (Parameter_Mode (N)));
      W_Node_Attribute
        ("Parameter_Type",
         "Node_Id",
         Image (Parameter_Type (N)),
         Int (Parameter_Type (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Parameter_Specification;

   procedure W_Parameter_Profile (N : List_Id) is
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
   end W_Parameter_Profile;

   procedure W_Subprogram_Specification (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Withed_Packages",
         "List_Id",
         Image (Withed_Packages (N)),
         Int (Withed_Packages (N)));
      W_Node_Attribute
        ("Package_Headers",
         "List_Id",
         Image (Package_Headers (N)),
         Int (Package_Headers (N)));
      W_Node_Attribute
        ("Parameter_Profile",
         "List_Id",
         Image (Parameter_Profile (N)),
         Int (Parameter_Profile (N)));
      W_Node_Attribute
        ("Return_Type",
         "Node_Id",
         Image (Return_Type (N)),
         Int (Return_Type (N)));
      W_Node_Attribute
        ("Renamed_Entity",
         "Node_Id",
         Image (Renamed_Entity (N)),
         Int (Renamed_Entity (N)));
      W_Node_Attribute
        ("Instantiated_Entity",
         "Node_Id",
         Image (Instantiated_Entity (N)),
         Int (Instantiated_Entity (N)));
      W_Node_Attribute
        ("Main_Subprogram_Unit",
         "Node_Id",
         Image (Main_Subprogram_Unit (N)),
         Int (Main_Subprogram_Unit (N)));
   end W_Subprogram_Specification;

   procedure W_Subprogram_Implementation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Withed_Packages",
         "List_Id",
         Image (Withed_Packages (N)),
         Int (Withed_Packages (N)));
      W_Node_Attribute
        ("Package_Headers",
         "List_Id",
         Image (Package_Headers (N)),
         Int (Package_Headers (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Main_Subprogram_Unit",
         "Node_Id",
         Image (Main_Subprogram_Unit (N)),
         Int (Main_Subprogram_Unit (N)));
      W_Node_Attribute
        ("Specification",
         "Node_Id",
         Image (Specification (N)),
         Int (Specification (N)));
   end W_Subprogram_Implementation;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Actual_Parameter_Part",
         "List_Id",
         Image (Actual_Parameter_Part (N)),
         Int (Actual_Parameter_Part (N)));
   end W_Subprogram_Call;

   procedure W_Parameter_Association (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Selector_Name",
         "Node_Id",
         Image (Selector_Name (N)),
         Int (Selector_Name (N)));
      W_Node_Attribute
        ("Actual_Parameter",
         "Node_Id",
         Image (Actual_Parameter (N)),
         Int (Actual_Parameter (N)));
   end W_Parameter_Association;

   procedure W_Selected_Component (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Prefix",
         "Node_Id",
         Image (Prefix (N)),
         Int (Prefix (N)));
      W_Node_Attribute
        ("Selector_Name",
         "Node_Id",
         Image (Selector_Name (N)),
         Int (Selector_Name (N)));
   end W_Selected_Component;

   procedure W_Full_Type_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Type_Definition",
         "Node_Id",
         Image (Type_Definition (N)),
         Int (Type_Definition (N)));
      W_Node_Attribute
        ("Discriminant_Spec",
         "Node_Id",
         Image (Discriminant_Spec (N)),
         Int (Discriminant_Spec (N)));
      W_Node_Attribute
        ("Is_Subtype",
         "Boolean",
         Image (Is_Subtype (N)));
   end W_Full_Type_Declaration;

   procedure W_Attribute_Definition_Clause (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Attribute_Designator",
         "Name_Id",
         Image (Attribute_Designator (N)));
   end W_Attribute_Definition_Clause;

   procedure W_Enumeration_Literals (N : List_Id) is
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
   end W_Enumeration_Literals;

   procedure W_Enumeration_Type_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Enumeration_Literals",
         "List_Id",
         Image (Enumeration_Literals (N)),
         Int (Enumeration_Literals (N)));
   end W_Enumeration_Type_Definition;

   procedure W_Enumeration_Representation_Clause (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Array_Aggregate",
         "Node_Id",
         Image (Array_Aggregate (N)),
         Int (Array_Aggregate (N)));
   end W_Enumeration_Representation_Clause;

   procedure W_Decimal_Type_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Scale",
         "Node_Id",
         Image (Scale (N)),
         Int (Scale (N)));
      W_Node_Attribute
        ("Total",
         "Value_Id",
         Image (Total (N)));
   end W_Decimal_Type_Definition;

   procedure W_Record_Aggregate (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Component_Association_List",
         "List_Id",
         Image (Component_Association_List (N)),
         Int (Component_Association_List (N)));
   end W_Record_Aggregate;

   procedure W_Component_Association (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Component_Association;

   procedure W_Protected_Object_Spec (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Visible_Part",
         "List_Id",
         Image (Visible_Part (N)),
         Int (Visible_Part (N)));
      W_Node_Attribute
        ("Private_Part",
         "List_Id",
         Image (Private_Part (N)),
         Int (Private_Part (N)));
      W_Node_Attribute
        ("Is_Type",
         "Boolean",
         Image (Is_Type (N)));
   end W_Protected_Object_Spec;

   procedure W_Protected_Object_Body (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
   end W_Protected_Object_Body;

   procedure W_Block_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Declarative_Part",
         "List_Id",
         Image (Declarative_Part (N)),
         Int (Declarative_Part (N)));
      W_Node_Attribute
        ("Exception_Handler",
         "List_Id",
         Image (Exception_Handler (N)),
         Int (Exception_Handler (N)));
   end W_Block_Statement;

   procedure W_Elsif_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
      W_Node_Attribute
        ("Then_Statements",
         "List_Id",
         Image (Then_Statements (N)),
         Int (Then_Statements (N)));
   end W_Elsif_Statement;

   procedure W_If_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
      W_Node_Attribute
        ("Then_Statements",
         "List_Id",
         Image (Then_Statements (N)),
         Int (Then_Statements (N)));
      W_Node_Attribute
        ("Elsif_Statements",
         "List_Id",
         Image (Elsif_Statements (N)),
         Int (Elsif_Statements (N)));
      W_Node_Attribute
        ("Else_Statements",
         "List_Id",
         Image (Else_Statements (N)),
         Int (Else_Statements (N)));
   end W_If_Statement;

   procedure W_Exit_When_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Condition",
         "Node_Id",
         Image (Condition (N)),
         Int (Condition (N)));
   end W_Exit_When_Statement;

   procedure W_Assignment_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Assignment_Statement;

   procedure W_Delay_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Is_Until",
         "Boolean",
         Image (Is_Until (N)));
   end W_Delay_Statement;

   procedure W_Return_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
   end W_Return_Statement;

   procedure W_For_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Range_Constraint",
         "Node_Id",
         Image (Range_Constraint (N)),
         Int (Range_Constraint (N)));
   end W_For_Statement;

   procedure W_Loop_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
   end W_Loop_Statement;

   procedure W_Case_Statement_Alternative (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Statements",
         "List_Id",
         Image (Statements (N)),
         Int (Statements (N)));
      W_Node_Attribute
        ("Discret_Choice_List",
         "List_Id",
         Image (Discret_Choice_List (N)),
         Int (Discret_Choice_List (N)));
   end W_Case_Statement_Alternative;

   procedure W_Case_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Case_Statement_Alternatives",
         "List_Id",
         Image (Case_Statement_Alternatives (N)),
         Int (Case_Statement_Alternatives (N)));
   end W_Case_Statement;

   procedure W_Case_Label (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Value",
         "Value_Id",
         Image (Value (N)));
   end W_Case_Label;

   procedure W_Pragma_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Argument_List",
         "List_Id",
         Image (Argument_List (N)),
         Int (Argument_List (N)));
   end W_Pragma_Statement;

   procedure W_Null_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
   end W_Null_Statement;

   procedure W_Package_Instantiation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Generic_Package",
         "Node_Id",
         Image (Generic_Package (N)),
         Int (Generic_Package (N)));
      W_Node_Attribute
        ("Parameter_List",
         "List_Id",
         Image (Parameter_List (N)),
         Int (Parameter_List (N)));
   end W_Package_Instantiation;

   procedure W_Raise_Statement (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Raised_Error",
         "Node_Id",
         Image (Raised_Error (N)),
         Int (Raised_Error (N)));
   end W_Raise_Statement;

   procedure W_Ada_Comment (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Has_Header_Spaces",
         "Boolean",
         Image (Has_Header_Spaces (N)));
   end W_Ada_Comment;

   procedure W_Access_Type_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Is_All",
         "Boolean",
         Image (Is_All (N)));
      W_Node_Attribute
        ("Is_Constant",
         "Boolean",
         Image (Is_Constant (N)));
      W_Node_Attribute
        ("Is_Not_Null",
         "Boolean",
         Image (Is_Not_Null (N)));
      W_Node_Attribute
        ("Subtype_Indication",
         "Node_Id",
         Image (Subtype_Indication (N)),
         Int (Subtype_Indication (N)));
   end W_Access_Type_Definition;

   procedure W_Derived_Type_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Is_Subtype",
         "Boolean",
         Image (Is_Subtype (N)));
      W_Node_Attribute
        ("Subtype_Indication",
         "Node_Id",
         Image (Subtype_Indication (N)),
         Int (Subtype_Indication (N)));
      W_Node_Attribute
        ("Is_Private_Extention",
         "Boolean",
         Image (Is_Private_Extention (N)));
      W_Node_Attribute
        ("Is_Abstract_Type",
         "Boolean",
         Image (Is_Abstract_Type (N)));
      W_Node_Attribute
        ("Record_Extension_Part",
         "Node_Id",
         Image (Record_Extension_Part (N)),
         Int (Record_Extension_Part (N)));
   end W_Derived_Type_Definition;

   procedure W_Record_Type_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Is_Abstract_Type",
         "Boolean",
         Image (Is_Abstract_Type (N)));
      W_Node_Attribute
        ("Is_Tagged_Type",
         "Boolean",
         Image (Is_Tagged_Type (N)));
      W_Node_Attribute
        ("Is_Limited_Type",
         "Boolean",
         Image (Is_Limited_Type (N)));
      W_Node_Attribute
        ("Record_Definition",
         "Node_Id",
         Image (Record_Definition (N)),
         Int (Record_Definition (N)));
   end W_Record_Type_Definition;

   procedure W_Private_Type_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
   end W_Private_Type_Definition;

   procedure W_Component_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Subtype_Indication",
         "Node_Id",
         Image (Subtype_Indication (N)),
         Int (Subtype_Indication (N)));
      W_Node_Attribute
        ("Aliased_Present",
         "Boolean",
         Image (Aliased_Present (N)));
   end W_Component_Declaration;

   procedure W_Component_List (N : List_Id) is
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
   end W_Component_List;

   procedure W_Record_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Component_List",
         "List_Id",
         Image (Component_List (N)),
         Int (Component_List (N)));
   end W_Record_Definition;

   procedure W_Range_Constraints (N : List_Id) is
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
   end W_Range_Constraints;

   procedure W_Array_Type_Definition (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Aliased_Present",
         "Boolean",
         Image (Aliased_Present (N)));
      W_Node_Attribute
        ("Range_Constraints",
         "List_Id",
         Image (Range_Constraints (N)),
         Int (Range_Constraints (N)));
      W_Node_Attribute
        ("Component_Definition",
         "Node_Id",
         Image (Component_Definition (N)),
         Int (Component_Definition (N)));
   end W_Array_Type_Definition;

   procedure W_Range_Constraint (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("First",
         "Node_Id",
         Image (First (N)),
         Int (First (N)));
      W_Node_Attribute
        ("Last",
         "Node_Id",
         Image (Last (N)),
         Int (Last (N)));
      W_Node_Attribute
        ("Index_Type",
         "Node_Id",
         Image (Index_Type (N)),
         Int (Index_Type (N)));
   end W_Range_Constraint;

   procedure W_Variant_List (N : List_Id) is
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
   end W_Variant_List;

   procedure W_Variant_Part (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Variants",
         "List_Id",
         Image (Variants (N)),
         Int (Variants (N)));
      W_Node_Attribute
        ("Discriminant",
         "Node_Id",
         Image (Discriminant (N)),
         Int (Discriminant (N)));
   end W_Variant_Part;

   procedure W_Discrete_Choice_List (N : List_Id) is
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
   end W_Discrete_Choice_List;

   procedure W_Variant (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Component_List",
         "List_Id",
         Image (Component_List (N)),
         Int (Component_List (N)));
      W_Node_Attribute
        ("Discrete_Choices",
         "List_Id",
         Image (Discrete_Choices (N)),
         Int (Discrete_Choices (N)));
   end W_Variant;

   procedure W_Object_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Renamed_Entity",
         "Node_Id",
         Image (Renamed_Entity (N)),
         Int (Renamed_Entity (N)));
      W_Node_Attribute
        ("Discriminant_Spec",
         "Node_Id",
         Image (Discriminant_Spec (N)),
         Int (Discriminant_Spec (N)));
      W_Node_Attribute
        ("Aliased_Present",
         "Boolean",
         Image (Aliased_Present (N)));
      W_Node_Attribute
        ("Constant_Present",
         "Boolean",
         Image (Constant_Present (N)));
      W_Node_Attribute
        ("Object_Definition",
         "Node_Id",
         Image (Object_Definition (N)),
         Int (Object_Definition (N)));
   end W_Object_Declaration;

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
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Value",
         "Value_Id",
         Image (Value (N)));
      W_Node_Attribute
        ("Parent_Designator",
         "Node_Id",
         Image (Parent_Designator (N)),
         Int (Parent_Designator (N)));
   end W_Literal;

   procedure W_Element_Association (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Index",
         "Node_Id",
         Image (Index (N)),
         Int (Index (N)));
   end W_Element_Association;

   procedure W_Element_List (N : List_Id) is
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
   end W_Element_List;

   procedure W_Array_Aggregate (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Elements",
         "List_Id",
         Image (Elements (N)),
         Int (Elements (N)));
   end W_Array_Aggregate;

   procedure W_Indexed_Component (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Prefix",
         "Node_Id",
         Image (Prefix (N)),
         Int (Prefix (N)));
      W_Node_Attribute
        ("Expressions",
         "List_Id",
         Image (Expressions (N)),
         Int (Expressions (N)));
   end W_Indexed_Component;

   procedure W_Exception_Declaration (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Defining_Identifier",
         "Node_Id",
         Image (Defining_Identifier (N)),
         Int (Defining_Identifier (N)));
      W_Node_Attribute
        ("Parent",
         "Node_Id",
         Image (Parent (N)),
         Int (Parent (N)));
      W_Node_Attribute
        ("Renamed_Entity",
         "Node_Id",
         Image (Renamed_Entity (N)),
         Int (Renamed_Entity (N)));
   end W_Exception_Declaration;

   procedure W_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Operator",
         "Operator_Id",
         Image (Operator (N)));
      W_Node_Attribute
        ("Left_Expr",
         "Node_Id",
         Image (Left_Expr (N)),
         Int (Left_Expr (N)));
      W_Node_Attribute
        ("Right_Expr",
         "Node_Id",
         Image (Right_Expr (N)),
         Int (Right_Expr (N)));
   end W_Expression;

   procedure W_Qualified_Expression (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Subtype_Mark",
         "Node_Id",
         Image (Subtype_Mark (N)),
         Int (Subtype_Mark (N)));
      W_Node_Attribute
        ("Aggregate",
         "Node_Id",
         Image (Aggregate (N)),
         Int (Aggregate (N)));
   end W_Qualified_Expression;

   procedure W_Type_Conversion (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Expression",
         "Node_Id",
         Image (Expression (N)),
         Int (Expression (N)));
      W_Node_Attribute
        ("Subtype_Mark",
         "Node_Id",
         Image (Subtype_Mark (N)),
         Int (Subtype_Mark (N)));
   end W_Type_Conversion;

   procedure W_Object_Instantiation (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Qualified_Expression",
         "Node_Id",
         Image (Qualified_Expression (N)),
         Int (Qualified_Expression (N)));
   end W_Object_Instantiation;

   procedure W_Boolean (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Boolean;

   procedure W_Float (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Float;

   procedure W_Integer (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Integer;

   procedure W_String (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_String;

   procedure W_Wide_String (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Wide_String;

   procedure W_Character (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Character;

   procedure W_Wide_Character (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Wide_Character;

   procedure W_Tree_Bindings (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Node",
         "Node_Id",
         Image (Main_Node (N)),
         Int (Main_Node (N)));
      W_Node_Attribute
        ("Type_Definition_Node",
         "Node_Id",
         Image (Type_Definition_Node (N)),
         Int (Type_Definition_Node (N)));
      W_Node_Attribute
        ("Feature_Subprogram_Node",
         "Node_Id",
         Image (Feature_Subprogram_Node (N)),
         Int (Feature_Subprogram_Node (N)));
      W_Node_Attribute
        ("Subprogram_Node",
         "Node_Id",
         Image (Subprogram_Node (N)),
         Int (Subprogram_Node (N)));
   end W_Tree_Bindings;

   procedure W_QoS_Tree_Bindings (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Node",
         "Node_Id",
         Image (Main_Node (N)),
         Int (Main_Node (N)));
      W_Node_Attribute
        ("Type_Definition_Node",
         "Node_Id",
         Image (Type_Definition_Node (N)),
         Int (Type_Definition_Node (N)));
      W_Node_Attribute
        ("Feature_Subprogram_Node",
         "Node_Id",
         Image (Feature_Subprogram_Node (N)),
         Int (Feature_Subprogram_Node (N)));
      W_Node_Attribute
        ("Subprogram_Node",
         "Node_Id",
         Image (Subprogram_Node (N)),
         Int (Subprogram_Node (N)));
      W_Node_Attribute
        ("Helpers_Node",
         "Node_Id",
         Image (Helpers_Node (N)),
         Int (Helpers_Node (N)));
      W_Node_Attribute
        ("Servants_Node",
         "Node_Id",
         Image (Servants_Node (N)),
         Int (Servants_Node (N)));
      W_Node_Attribute
        ("Parameters_Node",
         "Node_Id",
         Image (Parameters_Node (N)),
         Int (Parameters_Node (N)));
      W_Node_Attribute
        ("Obj_Adapters_Node",
         "Node_Id",
         Image (Obj_Adapters_Node (N)),
         Int (Obj_Adapters_Node (N)));
      W_Node_Attribute
        ("Setup_Node",
         "Node_Id",
         Image (Setup_Node (N)),
         Int (Setup_Node (N)));
      W_Node_Attribute
        ("Namespaces_Node",
         "Node_Id",
         Image (Namespaces_Node (N)),
         Int (Namespaces_Node (N)));
      W_Node_Attribute
        ("TypeCode_Node",
         "Node_Id",
         Image (TypeCode_Node (N)),
         Int (TypeCode_Node (N)));
      W_Node_Attribute
        ("From_Any_Node",
         "Node_Id",
         Image (From_Any_Node (N)),
         Int (From_Any_Node (N)));
      W_Node_Attribute
        ("To_Any_Node",
         "Node_Id",
         Image (To_Any_Node (N)),
         Int (To_Any_Node (N)));
      W_Node_Attribute
        ("Initialize_Node",
         "Node_Id",
         Image (Initialize_Node (N)),
         Int (Initialize_Node (N)));
      W_Node_Attribute
        ("Thread_Controller_Node",
         "Node_Id",
         Image (Thread_Controller_Node (N)),
         Int (Thread_Controller_Node (N)));
      W_Node_Attribute
        ("Execute_Servant_Node",
         "Node_Id",
         Image (Execute_Servant_Node (N)),
         Int (Execute_Servant_Node (N)));
      W_Node_Attribute
        ("Put_Node",
         "Node_Id",
         Image (Put_Node (N)),
         Int (Put_Node (N)));
      W_Node_Attribute
        ("Push_Back_Node",
         "Node_Id",
         Image (Push_Back_Node (N)),
         Int (Push_Back_Node (N)));
      W_Node_Attribute
        ("Get_Node",
         "Node_Id",
         Image (Get_Node (N)),
         Int (Get_Node (N)));
      W_Node_Attribute
        ("Package_Node",
         "Node_Id",
         Image (Package_Node (N)),
         Int (Package_Node (N)));
      W_Node_Attribute
        ("Reference_Node",
         "Node_Id",
         Image (Reference_Node (N)),
         Int (Reference_Node (N)));
      W_Node_Attribute
        ("Set_Node",
         "Node_Id",
         Image (Set_Node (N)),
         Int (Set_Node (N)));
      W_Node_Attribute
        ("Build_Node",
         "Node_Id",
         Image (Build_Node (N)),
         Int (Build_Node (N)));
   end W_QoS_Tree_Bindings;

   procedure W_HI_Tree_Bindings (N : Node_Id) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Next_Node",
         "Node_Id",
         Image (Next_Node (N)),
         Int (Next_Node (N)));
      W_Node_Attribute
        ("Frontend_Node",
         "Node_Id",
         Image (Frontend_Node (N)),
         Int (Frontend_Node (N)));
      W_Node_Attribute
        ("Main_Node",
         "Node_Id",
         Image (Main_Node (N)),
         Int (Main_Node (N)));
      W_Node_Attribute
        ("Type_Definition_Node",
         "Node_Id",
         Image (Type_Definition_Node (N)),
         Int (Type_Definition_Node (N)));
      W_Node_Attribute
        ("Feature_Subprogram_Node",
         "Node_Id",
         Image (Feature_Subprogram_Node (N)),
         Int (Feature_Subprogram_Node (N)));
      W_Node_Attribute
        ("Subprogram_Node",
         "Node_Id",
         Image (Subprogram_Node (N)),
         Int (Subprogram_Node (N)));
      W_Node_Attribute
        ("Marshallers_Node",
         "Node_Id",
         Image (Marshallers_Node (N)),
         Int (Marshallers_Node (N)));
      W_Node_Attribute
        ("Activity_Node",
         "Node_Id",
         Image (Activity_Node (N)),
         Int (Activity_Node (N)));
      W_Node_Attribute
        ("Subprograms_Node",
         "Node_Id",
         Image (Subprograms_Node (N)),
         Int (Subprograms_Node (N)));
      W_Node_Attribute
        ("Types_Node",
         "Node_Id",
         Image (Types_Node (N)),
         Int (Types_Node (N)));
      W_Node_Attribute
        ("Transport_Node",
         "Node_Id",
         Image (Transport_Node (N)),
         Int (Transport_Node (N)));
      W_Node_Attribute
        ("Deployment_Node",
         "Node_Id",
         Image (Deployment_Node (N)),
         Int (Deployment_Node (N)));
      W_Node_Attribute
        ("Naming_Node",
         "Node_Id",
         Image (Naming_Node (N)),
         Int (Naming_Node (N)));
      W_Node_Attribute
        ("Job_Node",
         "Node_Id",
         Image (Job_Node (N)),
         Int (Job_Node (N)));
      W_Node_Attribute
        ("Enumerator_Node",
         "Node_Id",
         Image (Enumerator_Node (N)),
         Int (Enumerator_Node (N)));
      W_Node_Attribute
        ("Marshall_Node",
         "Node_Id",
         Image (Marshall_Node (N)),
         Int (Marshall_Node (N)));
      W_Node_Attribute
        ("Unmarshall_Node",
         "Node_Id",
         Image (Unmarshall_Node (N)),
         Int (Unmarshall_Node (N)));
      W_Node_Attribute
        ("Port_Interface_Node",
         "Node_Id",
         Image (Port_Interface_Node (N)),
         Int (Port_Interface_Node (N)));
      W_Node_Attribute
        ("Port_Enumeration_Node",
         "Node_Id",
         Image (Port_Enumeration_Node (N)),
         Int (Port_Enumeration_Node (N)));
      W_Node_Attribute
        ("Deliver_Node",
         "Node_Id",
         Image (Deliver_Node (N)),
         Int (Deliver_Node (N)));
      W_Node_Attribute
        ("Send_Node",
         "Node_Id",
         Image (Send_Node (N)),
         Int (Send_Node (N)));
      W_Node_Attribute
        ("Put_Value_Node",
         "Node_Id",
         Image (Put_Value_Node (N)),
         Int (Put_Value_Node (N)));
      W_Node_Attribute
        ("Get_Value_Node",
         "Node_Id",
         Image (Get_Value_Node (N)),
         Int (Get_Value_Node (N)));
      W_Node_Attribute
        ("Get_Count_Node",
         "Node_Id",
         Image (Get_Count_Node (N)),
         Int (Get_Count_Node (N)));
      W_Node_Attribute
        ("Next_Value_Node",
         "Node_Id",
         Image (Next_Value_Node (N)),
         Int (Next_Value_Node (N)));
      W_Node_Attribute
        ("Store_Received_Message_Node",
         "Node_Id",
         Image (Store_Received_Message_Node (N)),
         Int (Store_Received_Message_Node (N)));
      W_Node_Attribute
        ("Default_Value_Node",
         "Node_Id",
         Image (Default_Value_Node (N)),
         Int (Default_Value_Node (N)));
      W_Node_Attribute
        ("Object_Node",
         "Node_Id",
         Image (Object_Node (N)),
         Int (Object_Node (N)));
   end W_HI_Tree_Bindings;

end Ocarina.Backends.Ada_Tree.Nodes;
