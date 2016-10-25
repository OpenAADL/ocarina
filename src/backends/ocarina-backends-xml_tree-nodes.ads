pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.Backends.XML_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_Definition,
      K_XML_Comment,
      K_Literal,
      K_Assignement,
      K_List_Id,
      K_Defining_Identifier,
      K_XML_File,
      K_XML_Node,
      K_HI_Distributed_Application,
      K_HI_Node,
      K_HI_Unit,
      K_Base_Type,
      K_Container,
      K_String,
      K_Numeric,
      K_Float,
      K_HI_Tree_Bindings);

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
   --  XML_Comment
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --

   procedure W_XML_Comment (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  Assignement
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Left_Expression          : Node_Id
   --    Right_Expression         : Node_Id
   --

   procedure W_Assignement (N : Node_Id);

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
   --

   procedure W_Defining_Identifier (N : Node_Id);

   --
   --  XML_File
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Distributed_Application_Unit: Node_Id
   --    Root_Node                : Node_Id
   --    Is_HTML                  : Boolean
   --    XML_DTD                  : Node_Id
   --

   procedure W_XML_File (N : Node_Id);

   --
   --  XML_Node
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Items                    : List_Id
   --    Subitems                 : List_Id
   --    Node_Value               : Node_Id
   --

   procedure W_XML_Node (N : Node_Id);

   --
   --  HI_Distributed_Application
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Units                    : List_Id
   --    HI_Nodes                 : List_Id
   --

   procedure W_HI_Distributed_Application (N : Node_Id);

   --
   --  HI_Node
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Name                     : Name_Id
   --    Units                    : List_Id
   --    Distributed_Application  : Node_Id
   --

   procedure W_HI_Node (N : Node_Id);

   --
   --  HI_Unit
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Entity                   : Node_Id
   --    XML_File                 : Node_Id
   --

   procedure W_HI_Unit (N : Node_Id);

   --
   --  Base_Type
   --
   --    Image                    : Name_Id
   --

   --
   --  Container
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Content                  : Node_Id
   --

   procedure W_Container (N : Node_Id);

   --
   --  String
   --
   --    Image                    : Name_Id
   --

   procedure W_String (N : Base_Type);

   --
   --  Numeric
   --
   --    Image                    : Name_Id
   --

   procedure W_Numeric (N : Base_Type);

   --
   --  Float
   --
   --    Image                    : Name_Id
   --

   procedure W_Float (N : Base_Type);

   --
   --  HI_Tree_Bindings
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Unit                     : Node_Id
   --    Node                     : Node_Id
   --    Processes                : List_Id
   --

   procedure W_HI_Tree_Bindings (N : Node_Id);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function Frontend_Node (N : Node_Id) return Node_Id;
   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id);

   function Defining_Identifier (N : Node_Id) return Node_Id;
   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function Left_Expression (N : Node_Id) return Node_Id;
   procedure Set_Left_Expression (N : Node_Id; V : Node_Id);

   function Right_Expression (N : Node_Id) return Node_Id;
   procedure Set_Right_Expression (N : Node_Id; V : Node_Id);

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

   function Distributed_Application_Unit (N : Node_Id) return Node_Id;
   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id);

   function Root_Node (N : Node_Id) return Node_Id;
   procedure Set_Root_Node (N : Node_Id; V : Node_Id);

   function Is_HTML (N : Node_Id) return Boolean;
   procedure Set_Is_HTML (N : Node_Id; V : Boolean);

   function XML_DTD (N : Node_Id) return Node_Id;
   procedure Set_XML_DTD (N : Node_Id; V : Node_Id);

   function Items (N : Node_Id) return List_Id;
   procedure Set_Items (N : Node_Id; V : List_Id);

   function Subitems (N : Node_Id) return List_Id;
   procedure Set_Subitems (N : Node_Id; V : List_Id);

   function Node_Value (N : Node_Id) return Node_Id;
   procedure Set_Node_Value (N : Node_Id; V : Node_Id);

   function Units (N : Node_Id) return List_Id;
   procedure Set_Units (N : Node_Id; V : List_Id);

   function HI_Nodes (N : Node_Id) return List_Id;
   procedure Set_HI_Nodes (N : Node_Id; V : List_Id);

   function Distributed_Application (N : Node_Id) return Node_Id;
   procedure Set_Distributed_Application (N : Node_Id; V : Node_Id);

   function Entity (N : Node_Id) return Node_Id;
   procedure Set_Entity (N : Node_Id; V : Node_Id);

   function XML_File (N : Node_Id) return Node_Id;
   procedure Set_XML_File (N : Node_Id; V : Node_Id);

   function Image (N : Base_Type) return Name_Id;
   procedure Set_Image (N : Base_Type; V : Name_Id);

   function Content (N : Node_Id) return Node_Id;
   procedure Set_Content (N : Node_Id; V : Node_Id);

   function Unit (N : Node_Id) return Node_Id;
   procedure Set_Unit (N : Node_Id; V : Node_Id);

   function Node (N : Node_Id) return Node_Id;
   procedure Set_Node (N : Node_Id; V : Node_Id);

   function Processes (N : Node_Id) return List_Id;
   procedure Set_Processes (N : Node_Id; V : List_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 1) of Boolean;
   type Byte_Array is array (1 .. 0) of Byte;
   type Int_Array is array (1 .. 7) of Int;

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

end Ocarina.Backends.XML_Tree.Nodes;
