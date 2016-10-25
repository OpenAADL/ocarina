pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.ME_AADL.AADL_Instances.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_List_Id,
      K_AADL_Entity,
      K_Named_AADL_Entity,
      K_Identifier,
      K_Node_Container,
      K_Architecture_Instance,
      K_Entity_Instance,
      K_Entity_Reference_Instance,
      K_Declaration_Instance,
      K_Component_Instance,
      K_Subcomponent_Instance,
      K_Namespace_Instance,
      K_Feature_Instance,
      K_Port_Spec_Instance,
      K_Feature_Group_Spec_Instance,
      K_Subprogram_Spec_Instance,
      K_Parameter_Instance,
      K_Subcomponent_Access_Instance,
      K_Call_Sequence_Instance,
      K_Call_Instance,
      K_Mode_Instance,
      K_Mode_Transition_Instance,
      K_Connection_Instance,
      K_Property_Association_Instance,
      K_Annex_Instance);

   --
   --  Node_Id
   --
   --    Next_Node                : Node_Id
   --

   --
   --  List_Id
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   --
   --  AADL_Entity
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --

   procedure W_AADL_Entity (N : Node_Id);

   --
   --  Named_AADL_Entity
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --

   procedure W_Named_AADL_Entity (N : Node_Id);

   --
   --  Identifier
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Display_Name             : Name_Id
   --    Corresponding_Entity     : Node_Id
   --    Scope_Entity             : Node_Id
   --    Homonym                  : Node_Id
   --    Visible                  : Boolean
   --    Backend_Node             : Node_Id
   --

   procedure W_Identifier (N : Node_Id);

   --
   --  Node_Container
   --
   --    Next_Node                : Node_Id
   --    Item                     : Node_Id
   --    Extra_Item               : Node_Id
   --

   procedure W_Node_Container (N : Node_Id);

   --
   --  Architecture_Instance
   --
   --    Next_Node                : Node_Id
   --    Root_System              : Node_Id
   --    Namespaces               : List_Id
   --    Unnamed_Namespace        : Node_Id
   --

   procedure W_Architecture_Instance (N : Node_Id);

   --
   --  Entity_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --

   procedure W_Entity_Instance (N : Node_Id);

   --
   --  Entity_Reference_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Path                     : List_Id
   --

   procedure W_Entity_Reference_Instance (N : Node_Id);

   --
   --  Declaration_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --

   procedure W_Declaration_Instance (N : Node_Id);

   --
   --  Component_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Is_Private               : Boolean
   --    Features                 : List_Id
   --    Subcomponents            : List_Id
   --    Modes                    : List_Id
   --    Mode_transitions         : List_Id
   --    Connections              : List_Id
   --    Calls                    : List_Id
   --    Flows                    : List_Id
   --    Annexes                  : List_Id
   --    Parent_Subcomponent      : Node_Id
   --    Namespace                : Node_Id
   --

   procedure W_Component_Instance (N : Node_Id);

   --
   --  Subcomponent_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Parent_Component         : Node_Id
   --    Corresponding_Instance   : Node_Id
   --    Destinations             : List_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Subcomponent_Instance (N : Node_Id);

   --
   --  Namespace_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Declarations             : List_Id
   --

   procedure W_Namespace_Instance (N : Node_Id);

   --
   --  Feature_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Parent_Component         : Node_Id
   --    Sources                  : List_Id
   --    Destinations             : List_Id
   --

   procedure W_Feature_Instance (N : Node_Id);

   --
   --  Port_Spec_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Parent_Component         : Node_Id
   --    Sources                  : List_Id
   --    Destinations             : List_Id
   --    Is_In                    : Boolean
   --    Is_Out                   : Boolean
   --    Is_Event                 : Boolean
   --    Is_Data                  : Boolean
   --    Corresponding_Instance   : Node_Id
   --

   procedure W_Port_Spec_Instance (N : Node_Id);

   --
   --  Feature_Group_Spec_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Parent_Component         : Node_Id
   --    Sources                  : List_Id
   --    Destinations             : List_Id
   --    Features                 : List_Id
   --

   procedure W_Feature_Group_Spec_Instance (N : Node_Id);

   --
   --  Subprogram_Spec_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Parent_Component         : Node_Id
   --    Sources                  : List_Id
   --    Destinations             : List_Id
   --    Is_Server                : Boolean
   --    Corresponding_Instance   : Node_Id
   --

   procedure W_Subprogram_Spec_Instance (N : Node_Id);

   --
   --  Parameter_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Parent_Component         : Node_Id
   --    Sources                  : List_Id
   --    Destinations             : List_Id
   --    Is_In                    : Boolean
   --    Is_Out                   : Boolean
   --    Corresponding_Instance   : Node_Id
   --

   procedure W_Parameter_Instance (N : Node_Id);

   --
   --  Subcomponent_Access_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Parent_Component         : Node_Id
   --    Sources                  : List_Id
   --    Destinations             : List_Id
   --    Is_Provided              : Boolean
   --    Is_Data                  : Boolean
   --    Corresponding_Instance   : Node_Id
   --

   procedure W_Subcomponent_Access_Instance (N : Node_Id);

   --
   --  Call_Sequence_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Parent_Component         : Node_Id
   --    Subprogram_Calls         : List_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Call_Sequence_Instance (N : Node_Id);

   --
   --  Call_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Parent_Sequence          : Node_Id
   --    Corresponding_Instance   : Node_Id
   --    Path                     : List_Id
   --

   procedure W_Call_Instance (N : Node_Id);

   --
   --  Mode_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Parent_Component         : Node_Id
   --    Is_Initial               : Boolean
   --

   procedure W_Mode_Instance (N : Node_Id);

   --
   --  Mode_Transition_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Parent_Component         : Node_Id
   --    Source_Modes             : List_Id
   --    Triggers                 : List_Id
   --    Destination_Mode         : Node_Id
   --

   procedure W_Mode_Transition_Instance (N : Node_Id);

   --
   --  Connection_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Properties               : List_Id
   --    Source                   : Node_Id
   --    Destination              : Node_Id
   --    Associated_Type          : Node_Id
   --    Parent_Component         : Node_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Connection_Instance (N : Node_Id);

   --
   --  Property_Association_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    Corresponding_Declaration: Node_Id
   --    Property_Name            : Node_Id
   --    Is_Additive_Association  : Boolean
   --    Is_Constant              : Boolean
   --    Is_Private               : Boolean
   --    Is_Access                : Boolean
   --    Property_Association_Type: Node_Id
   --    Property_Association_Value: Node_Id
   --    Applies_To_Prop          : List_Id
   --    In_Binding               : Node_Id
   --    In_Modes                 : Node_Id
   --

   procedure W_Property_Association_Instance (N : Node_Id);

   --
   --  Annex_Instance
   --
   --    Next_Node                : Node_Id
   --    Next_Entity              : Node_Id
   --    Identifier               : Node_Id
   --    In_Modes                 : Node_Id
   --    Corresponding_Annex      : Node_Id
   --

   procedure W_Annex_Instance (N : Node_Id);

   function Kind (N : Node_Id) return Node_Kind;
   procedure Set_Kind (N : Node_Id; V : Node_Kind);

   function Loc (N : Node_Id) return Location;
   procedure Set_Loc (N : Node_Id; V : Location);

   function Next_Node (N : Node_Id) return Node_Id;
   procedure Set_Next_Node (N : Node_Id; V : Node_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Next_Entity (N : Node_Id) return Node_Id;
   procedure Set_Next_Entity (N : Node_Id; V : Node_Id);

   function Identifier (N : Node_Id) return Node_Id;
   procedure Set_Identifier (N : Node_Id; V : Node_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Display_Name (N : Node_Id) return Name_Id;
   procedure Set_Display_Name (N : Node_Id; V : Name_Id);

   function Corresponding_Entity (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id);

   function Scope_Entity (N : Node_Id) return Node_Id;
   procedure Set_Scope_Entity (N : Node_Id; V : Node_Id);

   function Homonym (N : Node_Id) return Node_Id;
   procedure Set_Homonym (N : Node_Id; V : Node_Id);

   function Visible (N : Node_Id) return Boolean;
   procedure Set_Visible (N : Node_Id; V : Boolean);

   function Backend_Node (N : Node_Id) return Node_Id;
   procedure Set_Backend_Node (N : Node_Id; V : Node_Id);

   function Item (N : Node_Id) return Node_Id;
   procedure Set_Item (N : Node_Id; V : Node_Id);

   function Extra_Item (N : Node_Id) return Node_Id;
   procedure Set_Extra_Item (N : Node_Id; V : Node_Id);

   function Root_System (N : Node_Id) return Node_Id;
   procedure Set_Root_System (N : Node_Id; V : Node_Id);

   function Namespaces (N : Node_Id) return List_Id;
   procedure Set_Namespaces (N : Node_Id; V : List_Id);

   function Unnamed_Namespace (N : Node_Id) return Node_Id;
   procedure Set_Unnamed_Namespace (N : Node_Id; V : Node_Id);

   function Corresponding_Declaration (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Declaration (N : Node_Id; V : Node_Id);

   function Path (N : Node_Id) return List_Id;
   procedure Set_Path (N : Node_Id; V : List_Id);

   function Properties (N : Node_Id) return List_Id;
   procedure Set_Properties (N : Node_Id; V : List_Id);

   function Is_Private (N : Node_Id) return Boolean;
   procedure Set_Is_Private (N : Node_Id; V : Boolean);

   function Features (N : Node_Id) return List_Id;
   procedure Set_Features (N : Node_Id; V : List_Id);

   function Subcomponents (N : Node_Id) return List_Id;
   procedure Set_Subcomponents (N : Node_Id; V : List_Id);

   function Modes (N : Node_Id) return List_Id;
   procedure Set_Modes (N : Node_Id; V : List_Id);

   function Mode_transitions (N : Node_Id) return List_Id;
   procedure Set_Mode_transitions (N : Node_Id; V : List_Id);

   function Connections (N : Node_Id) return List_Id;
   procedure Set_Connections (N : Node_Id; V : List_Id);

   function Calls (N : Node_Id) return List_Id;
   procedure Set_Calls (N : Node_Id; V : List_Id);

   function Flows (N : Node_Id) return List_Id;
   procedure Set_Flows (N : Node_Id; V : List_Id);

   function Annexes (N : Node_Id) return List_Id;
   procedure Set_Annexes (N : Node_Id; V : List_Id);

   function Parent_Subcomponent (N : Node_Id) return Node_Id;
   procedure Set_Parent_Subcomponent (N : Node_Id; V : Node_Id);

   function Namespace (N : Node_Id) return Node_Id;
   procedure Set_Namespace (N : Node_Id; V : Node_Id);

   function Parent_Component (N : Node_Id) return Node_Id;
   procedure Set_Parent_Component (N : Node_Id; V : Node_Id);

   function Corresponding_Instance (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Instance (N : Node_Id; V : Node_Id);

   function Destinations (N : Node_Id) return List_Id;
   procedure Set_Destinations (N : Node_Id; V : List_Id);

   function In_Modes (N : Node_Id) return Node_Id;
   procedure Set_In_Modes (N : Node_Id; V : Node_Id);

   function Declarations (N : Node_Id) return List_Id;
   procedure Set_Declarations (N : Node_Id; V : List_Id);

   function Sources (N : Node_Id) return List_Id;
   procedure Set_Sources (N : Node_Id; V : List_Id);

   function Is_In (N : Node_Id) return Boolean;
   procedure Set_Is_In (N : Node_Id; V : Boolean);

   function Is_Out (N : Node_Id) return Boolean;
   procedure Set_Is_Out (N : Node_Id; V : Boolean);

   function Is_Event (N : Node_Id) return Boolean;
   procedure Set_Is_Event (N : Node_Id; V : Boolean);

   function Is_Data (N : Node_Id) return Boolean;
   procedure Set_Is_Data (N : Node_Id; V : Boolean);

   function Is_Server (N : Node_Id) return Boolean;
   procedure Set_Is_Server (N : Node_Id; V : Boolean);

   function Is_Provided (N : Node_Id) return Boolean;
   procedure Set_Is_Provided (N : Node_Id; V : Boolean);

   function Subprogram_Calls (N : Node_Id) return List_Id;
   procedure Set_Subprogram_Calls (N : Node_Id; V : List_Id);

   function Parent_Sequence (N : Node_Id) return Node_Id;
   procedure Set_Parent_Sequence (N : Node_Id; V : Node_Id);

   function Is_Initial (N : Node_Id) return Boolean;
   procedure Set_Is_Initial (N : Node_Id; V : Boolean);

   function Source_Modes (N : Node_Id) return List_Id;
   procedure Set_Source_Modes (N : Node_Id; V : List_Id);

   function Triggers (N : Node_Id) return List_Id;
   procedure Set_Triggers (N : Node_Id; V : List_Id);

   function Destination_Mode (N : Node_Id) return Node_Id;
   procedure Set_Destination_Mode (N : Node_Id; V : Node_Id);

   function Source (N : Node_Id) return Node_Id;
   procedure Set_Source (N : Node_Id; V : Node_Id);

   function Destination (N : Node_Id) return Node_Id;
   procedure Set_Destination (N : Node_Id; V : Node_Id);

   function Associated_Type (N : Node_Id) return Node_Id;
   procedure Set_Associated_Type (N : Node_Id; V : Node_Id);

   function Property_Name (N : Node_Id) return Node_Id;
   procedure Set_Property_Name (N : Node_Id; V : Node_Id);

   function Is_Additive_Association (N : Node_Id) return Boolean;
   procedure Set_Is_Additive_Association (N : Node_Id; V : Boolean);

   function Is_Constant (N : Node_Id) return Boolean;
   procedure Set_Is_Constant (N : Node_Id; V : Boolean);

   function Is_Access (N : Node_Id) return Boolean;
   procedure Set_Is_Access (N : Node_Id; V : Boolean);

   function Property_Association_Type (N : Node_Id) return Node_Id;
   procedure Set_Property_Association_Type (N : Node_Id; V : Node_Id);

   function Property_Association_Value (N : Node_Id) return Node_Id;
   procedure Set_Property_Association_Value (N : Node_Id; V : Node_Id);

   function Applies_To_Prop (N : Node_Id) return List_Id;
   procedure Set_Applies_To_Prop (N : Node_Id; V : List_Id);

   function In_Binding (N : Node_Id) return Node_Id;
   procedure Set_In_Binding (N : Node_Id; V : Node_Id);

   function Corresponding_Annex (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Annex (N : Node_Id; V : Node_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 4) of Boolean;
   type Byte_Array is array (1 .. 0) of Byte;
   type Int_Array is array (1 .. 16) of Int;

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

end Ocarina.ME_AADL.AADL_Instances.Nodes;
