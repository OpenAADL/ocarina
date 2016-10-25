pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.Backends.ASN1_Tree.Nodes is

   type Node_Kind is
     (K_Base_Type,
      K_Node_Id,
      K_Definition,
      K_List_Id,
      K_Defining_Identifier,
      K_ASN1_File,
      K_ASN1_Module,
      K_Import_Export_Clause,
      K_Type_Definition,
      K_Enumerated,
      K_Enumerated_Value_List,
      K_Enumerated_Value,
      K_Sequence,
      K_Sequence_Member,
      K_Choice,
      K_Choice_Member,
      K_Literal,
      K_Type_Designator,
      K_Type_Constraints,
      K_Float,
      K_Int,
      K_Char,
      K_String);

   --
   --  Base_Type
   --
   --    Image                    : Name_Id
   --

   --
   --  Node_Id
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --

   --
   --  Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --

   procedure W_Definition (N : Node_Id);

   --
   --  List_Id
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   --
   --  Defining_Identifier
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Corresponding_Node       : Node_Id
   --    Compile_Unit             : Node_Id
   --    Is_Pointer               : Boolean
   --

   procedure W_Defining_Identifier (N : Node_Id);

   --
   --  ASN1_File
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Module_Node              : Node_Id
   --

   procedure W_ASN1_File (N : Node_Id);

   --
   --  ASN1_Module
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Oid                      : Node_Id
   --    Automatic_Tags           : Boolean
   --    Imported_Clauses         : List_Id
   --    Exported_Clauses         : List_Id
   --    Definitions              : List_Id
   --

   procedure W_ASN1_Module (N : Node_Id);

   --
   --  Import_Export_Clause
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Associated_Module        : Name_Id
   --

   procedure W_Import_Export_Clause (N : Node_Id);

   --
   --  Type_Definition
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Name                     : Name_Id
   --    Declaration              : Node_Id
   --

   procedure W_Type_Definition (N : Node_Id);

   --
   --  Enumerated
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Values                   : List_Id
   --

   procedure W_Enumerated (N : Node_Id);

   --
   --  Enumerated_Value_List
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   procedure W_Enumerated_Value_List (N : List_Id);

   --
   --  Enumerated_Value
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Value                    : Value_Id
   --

   procedure W_Enumerated_Value (N : Node_Id);

   --
   --  Sequence
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Values                   : List_Id
   --

   procedure W_Sequence (N : Node_Id);

   --
   --  Sequence_Member
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Member_Name              : Name_Id
   --    Member_Type              : Node_Id
   --

   procedure W_Sequence_Member (N : Node_Id);

   --
   --  Choice
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Values                   : List_Id
   --

   procedure W_Choice (N : Node_Id);

   --
   --  Choice_Member
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Member_Name              : Name_Id
   --    Member_Type              : Node_Id
   --

   procedure W_Choice_Member (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  Type_Designator
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Type_Name                : Node_Id
   --    Constraints              : Node_Id
   --

   procedure W_Type_Designator (N : Node_Id);

   --
   --  Type_Constraints
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Size_Down                : Value_Id
   --    Size_Up                  : Value_Id
   --

   procedure W_Type_Constraints (N : Node_Id);

   --
   --  Float
   --
   --    Image                    : Name_Id
   --

   procedure W_Float (N : Base_Type);

   --
   --  Int
   --
   --    Image                    : Name_Id
   --

   procedure W_Int (N : Base_Type);

   --
   --  Char
   --
   --    Image                    : Name_Id
   --

   procedure W_Char (N : Base_Type);

   --
   --  String
   --
   --    Image                    : Name_Id
   --

   procedure W_String (N : Base_Type);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Image (N : Base_Type) return Name_Id;
   procedure Set_Image (N : Base_Type; V : Name_Id);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function Frontend_Node (N : Node_Id) return Node_Id;
   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id);

   function Defining_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Corresponding_Node (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id);

   function Compile_Unit (N : Node_Id) return Node_Id;
   procedure Set_Compile_Unit (N : Node_Id; V : Node_Id);

   function Is_Pointer (N : Node_Id) return Boolean;
   procedure Set_Is_Pointer (N : Node_Id; V : Boolean);

   function Module_Node (N : Node_Id) return Node_Id;
   procedure Set_Module_Node (N : Node_Id; V : Node_Id);

   function Oid (N : Node_Id) return Node_Id;
   procedure Set_Oid (N : Node_Id; V : Node_Id);

   function Automatic_Tags (N : Node_Id) return Boolean;
   procedure Set_Automatic_Tags (N : Node_Id; V : Boolean);

   function Imported_Clauses (N : Node_Id) return List_Id;
   procedure Set_Imported_Clauses (N : Node_Id; V : List_Id);

   function Exported_Clauses (N : Node_Id) return List_Id;
   procedure Set_Exported_Clauses (N : Node_Id; V : List_Id);

   function Definitions (N : Node_Id) return List_Id;
   procedure Set_Definitions (N : Node_Id; V : List_Id);

   function Associated_Module (N : Node_Id) return Name_Id;
   procedure Set_Associated_Module (N : Node_Id; V : Name_Id);

   function Declaration (N : Node_Id) return Node_Id;
   procedure Set_Declaration (N : Node_Id; V : Node_Id);

   function Values (N : Node_Id) return List_Id;
   procedure Set_Values (N : Node_Id; V : List_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function Member_Name (N : Node_Id) return Name_Id;
   procedure Set_Member_Name (N : Node_Id; V : Name_Id);

   function Member_Type (N : Node_Id) return Node_Id;
   procedure Set_Member_Type (N : Node_Id; V : Node_Id);

   function Type_Name (N : Node_Id) return Node_Id;
   procedure Set_Type_Name (N : Node_Id; V : Node_Id);

   function Constraints (N : Node_Id) return Node_Id;
   procedure Set_Constraints (N : Node_Id; V : Node_Id);

   function Size_Down (N : Node_Id) return Value_Id;
   procedure Set_Size_Down (N : Node_Id; V : Value_Id);

   function Size_Up (N : Node_Id) return Value_Id;
   procedure Set_Size_Up (N : Node_Id; V : Value_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 1) of Boolean;
   type Byte_Array is array (1 .. 0) of Byte;
   type Int_Array is array (1 .. 8) of Int;

   type Node_Entry is record
      Kind : Node_Kind;
      B : Boolean_Array;
      L : Int_Array;
      Loc : Location;
   end record;

   Default_Node : constant Node_Entry :=
     (Node_Kind'First,
      (others => False),
      (others => 0),
      No_Location);

   package Entries is new GNAT.Table
     (Node_Entry, Node_Id, No_Node + 1, 1000, 100);

end Ocarina.Backends.ASN1_Tree.Nodes;
