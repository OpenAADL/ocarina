pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.Backends.ASN1_Tree.Debug; use Ocarina.Backends.ASN1_Tree.Debug;

package body Ocarina.Backends.ASN1_Tree.Nodes is

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

   function Image (N : Base_Type) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Image;

   procedure Set_Image (N : Base_Type; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_Float
        or else Table (Types.Node_Id (N)).Kind = K_Int
        or else Table (Types.Node_Id (N)).Kind = K_Char
        or else Table (Types.Node_Id (N)).Kind = K_String);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Image;

   function Next_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value
        or else Table (Types.Node_Id (N)).Kind = K_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value
        or else Table (Types.Node_Id (N)).Kind = K_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Next_Node;

   function Frontend_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value
        or else Table (Types.Node_Id (N)).Kind = K_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Frontend_Node;

   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value
        or else Table (Types.Node_Id (N)).Kind = K_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Frontend_Node;

   function Defining_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Defining_Identifier;

   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Defining_Identifier;

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value_List);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value_List);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value_List);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value_List);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value);

      return Name_Id (Table (Types.Node_Id (N)).L (4));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Name;

   function Corresponding_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Corresponding_Node;

   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Corresponding_Node;

   function Compile_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Compile_Unit;

   procedure Set_Compile_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Compile_Unit;

   function Is_Pointer (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Pointer;

   procedure Set_Is_Pointer (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Pointer;

   function Module_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Module_Node;

   procedure Set_Module_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_File);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Module_Node;

   function Oid (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Oid;

   procedure Set_Oid (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Oid;

   function Automatic_Tags (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Automatic_Tags;

   procedure Set_Automatic_Tags (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Automatic_Tags;

   function Imported_Clauses (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Imported_Clauses;

   procedure Set_Imported_Clauses (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Imported_Clauses;

   function Exported_Clauses (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      return List_Id (Table (Types.Node_Id (N)).L (7));
   end Exported_Clauses;

   procedure Set_Exported_Clauses (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Exported_Clauses;

   function Definitions (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      return List_Id (Table (Types.Node_Id (N)).L (8));
   end Definitions;

   procedure Set_Definitions (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_ASN1_Module);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Definitions;

   function Associated_Module (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Associated_Module;

   procedure Set_Associated_Module (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Import_Export_Clause);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Associated_Module;

   function Declaration (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Declaration;

   procedure Set_Declaration (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Definition);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Declaration;

   function Values (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated
        or else Table (Types.Node_Id (N)).Kind = K_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Choice);

      return List_Id (Table (Types.Node_Id (N)).L (1));
   end Values;

   procedure Set_Values (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated
        or else Table (Types.Node_Id (N)).Kind = K_Sequence
        or else Table (Types.Node_Id (N)).Kind = K_Choice);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Values;

   function Value (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Value;

   procedure Set_Value (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Enumerated_Value
        or else Table (Types.Node_Id (N)).Kind = K_Literal);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Value;

   function Member_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Member_Name;

   procedure Set_Member_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Member_Name;

   function Member_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Member_Type;

   procedure Set_Member_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Sequence_Member
        or else Table (Types.Node_Id (N)).Kind = K_Choice_Member);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Member_Type;

   function Type_Name (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Type_Name;

   procedure Set_Type_Name (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Type_Name;

   function Constraints (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Constraints;

   procedure Set_Constraints (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Designator);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Constraints;

   function Size_Down (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      return Value_Id (Table (Types.Node_Id (N)).L (1));
   end Size_Down;

   procedure Set_Size_Down (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Size_Down;

   function Size_Up (N : Node_Id) return Value_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      return Value_Id (Table (Types.Node_Id (N)).L (4));
   end Size_Up;

   procedure Set_Size_Up (N : Node_Id; V : Value_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Type_Constraints);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Size_Up;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Definition =>
            W_Definition
              (Node_Id (N));
         when K_Defining_Identifier =>
            W_Defining_Identifier
              (Node_Id (N));
         when K_ASN1_File =>
            W_ASN1_File
              (Node_Id (N));
         when K_ASN1_Module =>
            W_ASN1_Module
              (Node_Id (N));
         when K_Import_Export_Clause =>
            W_Import_Export_Clause
              (Node_Id (N));
         when K_Type_Definition =>
            W_Type_Definition
              (Node_Id (N));
         when K_Enumerated =>
            W_Enumerated
              (Node_Id (N));
         when K_Enumerated_Value_List =>
            W_Enumerated_Value_List
              (List_Id (N));
         when K_Enumerated_Value =>
            W_Enumerated_Value
              (Node_Id (N));
         when K_Sequence =>
            W_Sequence
              (Node_Id (N));
         when K_Sequence_Member =>
            W_Sequence_Member
              (Node_Id (N));
         when K_Choice =>
            W_Choice
              (Node_Id (N));
         when K_Choice_Member =>
            W_Choice_Member
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_Type_Designator =>
            W_Type_Designator
              (Node_Id (N));
         when K_Type_Constraints =>
            W_Type_Constraints
              (Node_Id (N));
         when K_Float =>
            W_Float
              (Base_Type (N));
         when K_Int =>
            W_Int
              (Base_Type (N));
         when K_Char =>
            W_Char
              (Base_Type (N));
         when K_String =>
            W_String
              (Base_Type (N));
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
        ("Compile_Unit",
         "Node_Id",
         Image (Compile_Unit (N)),
         Int (Compile_Unit (N)));
      W_Node_Attribute
        ("Is_Pointer",
         "Boolean",
         Image (Is_Pointer (N)));
   end W_Defining_Identifier;

   procedure W_ASN1_File (N : Node_Id) is
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
        ("Module_Node",
         "Node_Id",
         Image (Module_Node (N)),
         Int (Module_Node (N)));
   end W_ASN1_File;

   procedure W_ASN1_Module (N : Node_Id) is
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
        ("Oid",
         "Node_Id",
         Image (Oid (N)),
         Int (Oid (N)));
      W_Node_Attribute
        ("Automatic_Tags",
         "Boolean",
         Image (Automatic_Tags (N)));
      W_Node_Attribute
        ("Imported_Clauses",
         "List_Id",
         Image (Imported_Clauses (N)),
         Int (Imported_Clauses (N)));
      W_Node_Attribute
        ("Exported_Clauses",
         "List_Id",
         Image (Exported_Clauses (N)),
         Int (Exported_Clauses (N)));
      W_Node_Attribute
        ("Definitions",
         "List_Id",
         Image (Definitions (N)),
         Int (Definitions (N)));
   end W_ASN1_Module;

   procedure W_Import_Export_Clause (N : Node_Id) is
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
        ("Associated_Module",
         "Name_Id",
         Image (Associated_Module (N)));
   end W_Import_Export_Clause;

   procedure W_Type_Definition (N : Node_Id) is
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
        ("Name",
         "Name_Id",
         Image (Name (N)));
      W_Node_Attribute
        ("Declaration",
         "Node_Id",
         Image (Declaration (N)),
         Int (Declaration (N)));
   end W_Type_Definition;

   procedure W_Enumerated (N : Node_Id) is
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
        ("Values",
         "List_Id",
         Image (Values (N)),
         Int (Values (N)));
   end W_Enumerated;

   procedure W_Enumerated_Value_List (N : List_Id) is
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
   end W_Enumerated_Value_List;

   procedure W_Enumerated_Value (N : Node_Id) is
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
        ("Value",
         "Value_Id",
         Image (Value (N)));
   end W_Enumerated_Value;

   procedure W_Sequence (N : Node_Id) is
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
        ("Values",
         "List_Id",
         Image (Values (N)),
         Int (Values (N)));
   end W_Sequence;

   procedure W_Sequence_Member (N : Node_Id) is
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
        ("Member_Name",
         "Name_Id",
         Image (Member_Name (N)));
      W_Node_Attribute
        ("Member_Type",
         "Node_Id",
         Image (Member_Type (N)),
         Int (Member_Type (N)));
   end W_Sequence_Member;

   procedure W_Choice (N : Node_Id) is
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
        ("Values",
         "List_Id",
         Image (Values (N)),
         Int (Values (N)));
   end W_Choice;

   procedure W_Choice_Member (N : Node_Id) is
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
        ("Member_Name",
         "Name_Id",
         Image (Member_Name (N)));
      W_Node_Attribute
        ("Member_Type",
         "Node_Id",
         Image (Member_Type (N)),
         Int (Member_Type (N)));
   end W_Choice_Member;

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
   end W_Literal;

   procedure W_Type_Designator (N : Node_Id) is
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
        ("Type_Name",
         "Node_Id",
         Image (Type_Name (N)),
         Int (Type_Name (N)));
      W_Node_Attribute
        ("Constraints",
         "Node_Id",
         Image (Constraints (N)),
         Int (Constraints (N)));
   end W_Type_Designator;

   procedure W_Type_Constraints (N : Node_Id) is
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
        ("Size_Down",
         "Value_Id",
         Image (Size_Down (N)));
      W_Node_Attribute
        ("Size_Up",
         "Value_Id",
         Image (Size_Up (N)));
   end W_Type_Constraints;

   procedure W_Float (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Float;

   procedure W_Int (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Int;

   procedure W_Char (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Char;

   procedure W_String (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_String;

end Ocarina.Backends.ASN1_Tree.Nodes;
