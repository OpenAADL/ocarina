pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.Backends.PN.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_List_Id,
      K_Identifier,
      K_Pn_Node,
      K_Pn_Arc,
      K_Pn_Place,
      K_Pn_Transition,
      K_Pn_Component,
      K_Pn_Box,
      K_Pn_Generated,
      K_CPN_Formalism_Class_Item_Range,
      K_CPN_Formalism_Class_Item_Enum_Item,
      K_CPN_Formalism_Class_Item_Enum,
      K_CPN_Formalism_Classes,
      K_CPN_Formalism_Domains,
      K_CPN_Formalism_Variable_Item,
      K_CPN_Formalism_Variables,
      K_CPN_Aadl_Id,
      K_CPN_Specific_Informations,
      K_CPN_Marking_Token,
      K_CPN_Marking,
      K_CPN_Place,
      K_CPN_Transition_Guard,
      K_CPN_Transition,
      K_CPN_Arc_Valuation,
      K_CPN_Arc,
      K_TPN_Processor_Priority,
      K_TPN_Specific_Informations,
      K_TPN_Guard,
      K_TPN_Place,
      K_TPN_Transition,
      K_TPN_Arc,
      K_Thread_Pattern,
      K_Port_Pattern,
      K_Data_Port_Pattern,
      K_Data_Event_Port_Pattern,
      K_Call_Sequence_Pattern,
      K_Subprogram_Call_Pattern,
      K_Spg_Parameter_Pattern,
      K_Processor_Pattern);

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
   --  Identifier
   --
   --    Next_Node                : Node_Id
   --    Name                     : Name_Id
   --    Ocarina_Node             : Node_Id
   --    Corresponding_Entity     : Node_Id
   --

   procedure W_Identifier (N : Node_Id);

   --
   --  Pn_Node
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --

   procedure W_Pn_Node (N : Node_Id);

   --
   --  Pn_Arc
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Pn_From                  : Node_Id
   --    Pn_To                    : Node_Id
   --

   procedure W_Pn_Arc (N : Node_Id);

   --
   --  Pn_Place
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --

   procedure W_Pn_Place (N : Node_Id);

   --
   --  Pn_Transition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Pn_Arcs_In               : List_Id
   --    Pn_Arcs_Out              : List_Id
   --

   procedure W_Pn_Transition (N : Node_Id);

   --
   --  Pn_Component
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --

   procedure W_Pn_Component (N : Node_Id);

   --
   --  Pn_Box
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Pn_Subcomponents         : List_Id
   --    Pn_Interconnections      : List_Id
   --

   procedure W_Pn_Box (N : Node_Id);

   --
   --  Pn_Generated
   --
   --    Next_Node                : Node_Id
   --    Pn_Box                   : Node_Id
   --    Pn_Formalism_Specific_Informations: Node_Id
   --    Formalism                : Value_Id
   --

   procedure W_Pn_Generated (N : Node_Id);

   --
   --  CPN_Formalism_Class_Item_Range
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Low                      : Value_Id
   --    High                     : Value_Id
   --

   procedure W_CPN_Formalism_Class_Item_Range (N : Node_Id);

   --
   --  CPN_Formalism_Class_Item_Enum_Item
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --

   procedure W_CPN_Formalism_Class_Item_Enum_Item (N : Node_Id);

   --
   --  CPN_Formalism_Class_Item_Enum
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Enum                     : List_Id
   --

   procedure W_CPN_Formalism_Class_Item_Enum (N : Node_Id);

   --
   --  CPN_Formalism_Classes
   --
   --    Next_Node                : Node_Id
   --    Class_List               : List_Id
   --

   procedure W_CPN_Formalism_Classes (N : Node_Id);

   --
   --  CPN_Formalism_Domains
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Domain_List              : List_Id
   --

   procedure W_CPN_Formalism_Domains (N : Node_Id);

   --
   --  CPN_Formalism_Variable_Item
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --

   procedure W_CPN_Formalism_Variable_Item (N : Node_Id);

   --
   --  CPN_Formalism_Variables
   --
   --    Next_Node                : Node_Id
   --    Class_Type               : Node_Id
   --    Variable_List            : List_Id
   --

   procedure W_CPN_Formalism_Variables (N : Node_Id);

   --
   --  CPN_Aadl_Id
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Aadl_Instance            : Node_Id
   --    Pn_Id                    : Value_Id
   --

   procedure W_CPN_Aadl_Id (N : Node_Id);

   --
   --  CPN_Specific_Informations
   --
   --    Next_Node                : Node_Id
   --    Classes                  : Node_Id
   --    Domains                  : List_Id
   --    Variables                : List_Id
   --    Threads_Count            : Value_Id
   --    Threads_Ids              : List_Id
   --    Ports_Ids                : List_Id
   --

   procedure W_CPN_Specific_Informations (N : Node_Id);

   --
   --  CPN_Marking_Token
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --

   procedure W_CPN_Marking_Token (N : Node_Id);

   --
   --  CPN_Marking
   --
   --    Next_Node                : Node_Id
   --    Tokens                   : List_Id
   --

   procedure W_CPN_Marking (N : Node_Id);

   --
   --  CPN_Place
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Domain                   : Node_Id
   --    Marking                  : Node_Id
   --    Nb_T                     : Value_Id
   --

   procedure W_CPN_Place (N : Node_Id);

   --
   --  CPN_Transition_Guard
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Operator                 : Value_Id
   --    Left_Op                  : Value_Id
   --    Right_Op                 : Value_Id
   --

   procedure W_CPN_Transition_Guard (N : Node_Id);

   --
   --  CPN_Transition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Pn_Arcs_In               : List_Id
   --    Pn_Arcs_Out              : List_Id
   --    Guards                   : List_Id
   --

   procedure W_CPN_Transition (N : Node_Id);

   --
   --  CPN_Arc_Valuation
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Is_Colored               : Boolean
   --

   procedure W_CPN_Arc_Valuation (N : Node_Id);

   --
   --  CPN_Arc
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Pn_From                  : Node_Id
   --    Pn_To                    : Node_Id
   --    Valuations               : List_Id
   --

   procedure W_CPN_Arc (N : Node_Id);

   --
   --  TPN_Processor_Priority
   --
   --    Next_Node                : Node_Id
   --    P_Instance               : Node_Id
   --    Bounded_Trans            : List_Id
   --

   procedure W_TPN_Processor_Priority (N : Node_Id);

   --
   --  TPN_Specific_Informations
   --
   --    Next_Node                : Node_Id
   --    Th_Number                : Value_Id
   --    Hyperperiod              : Value_Id
   --    Priorities               : List_Id
   --

   procedure W_TPN_Specific_Informations (N : Node_Id);

   --
   --  TPN_Guard
   --
   --    Next_Node                : Node_Id
   --    Lower_Value              : Value_Id
   --    Higher_Value             : Value_Id
   --    Braces_Mode              : Value_Id
   --

   procedure W_TPN_Guard (N : Node_Id);

   --
   --  TPN_Place
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Tokens_Number            : Value_Id
   --

   procedure W_TPN_Place (N : Node_Id);

   --
   --  TPN_Transition
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Pn_Arcs_In               : List_Id
   --    Pn_Arcs_Out              : List_Id
   --    Guard                    : Node_Id
   --    Priority                 : Value_Id
   --

   procedure W_TPN_Transition (N : Node_Id);

   --
   --  TPN_Arc
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Pn_From                  : Node_Id
   --    Pn_To                    : Node_Id
   --    Valuation                : Value_Id
   --    is_Priority              : Boolean
   --

   procedure W_TPN_Arc (N : Node_Id);

   --
   --  Thread_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    In_Ports                 : List_Id
   --    Out_Ports                : List_Id
   --    Hyperperiod              : Value_Id
   --    Call_Seq                 : List_Id
   --    Th_Instance              : Node_Id
   --

   procedure W_Thread_Pattern (N : Node_Id);

   --
   --  Port_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    Port_Instance            : Node_Id
   --    Source_Instance          : Node_Id
   --    Target_Instance          : Node_Id
   --

   procedure W_Port_Pattern (N : Node_Id);

   --
   --  Data_Port_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    Port_Instance            : Node_Id
   --    Source_Instance          : Node_Id
   --    Target_Instance          : Node_Id
   --

   procedure W_Data_Port_Pattern (N : Node_Id);

   --
   --  Data_Event_Port_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    Port_Instance            : Node_Id
   --    Source_Instance          : Node_Id
   --    Target_Instance          : Node_Id
   --    Queue_Size               : Value_Id
   --    Has_CEP                  : Boolean
   --    Dispatch_Port            : Boolean
   --

   procedure W_Data_Event_Port_Pattern (N : Node_Id);

   --
   --  Call_Sequence_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    Spg_Call                 : List_Id
   --

   procedure W_Call_Sequence_Pattern (N : Node_Id);

   --
   --  Subprogram_Call_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    Param_In                 : List_Id
   --    Param_Out                : List_Id
   --

   procedure W_Subprogram_Call_Pattern (N : Node_Id);

   --
   --  Spg_Parameter_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    Par_Instance             : Node_Id
   --

   procedure W_Spg_Parameter_Pattern (N : Node_Id);

   --
   --  Processor_Pattern
   --
   --    Next_Node                : Node_Id
   --    Identifier               : Node_Id
   --    Scoped_Name              : Node_Id
   --    Public_Interfaces        : List_Id
   --    Internal_Transitions     : List_Id
   --    Internal_Places          : List_Id
   --    Proc_Instance            : Node_Id
   --

   procedure W_Processor_Pattern (N : Node_Id);

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

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Ocarina_Node (N : Node_Id) return Node_Id;
   procedure Set_Ocarina_Node (N : Node_Id; V : Node_Id);

   function Corresponding_Entity (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Entity (N : Node_Id; V : Node_Id);

   function Identifier (N : Node_Id) return Node_Id;
   procedure Set_Identifier (N : Node_Id; V : Node_Id);

   function Scoped_Name (N : Node_Id) return Node_Id;
   procedure Set_Scoped_Name (N : Node_Id; V : Node_Id);

   function Pn_From (N : Node_Id) return Node_Id;
   procedure Set_Pn_From (N : Node_Id; V : Node_Id);

   function Pn_To (N : Node_Id) return Node_Id;
   procedure Set_Pn_To (N : Node_Id; V : Node_Id);

   function Pn_Arcs_In (N : Node_Id) return List_Id;
   procedure Set_Pn_Arcs_In (N : Node_Id; V : List_Id);

   function Pn_Arcs_Out (N : Node_Id) return List_Id;
   procedure Set_Pn_Arcs_Out (N : Node_Id; V : List_Id);

   function Public_Interfaces (N : Node_Id) return List_Id;
   procedure Set_Public_Interfaces (N : Node_Id; V : List_Id);

   function Internal_Transitions (N : Node_Id) return List_Id;
   procedure Set_Internal_Transitions (N : Node_Id; V : List_Id);

   function Internal_Places (N : Node_Id) return List_Id;
   procedure Set_Internal_Places (N : Node_Id; V : List_Id);

   function Pn_Subcomponents (N : Node_Id) return List_Id;
   procedure Set_Pn_Subcomponents (N : Node_Id; V : List_Id);

   function Pn_Interconnections (N : Node_Id) return List_Id;
   procedure Set_Pn_Interconnections (N : Node_Id; V : List_Id);

   function Pn_Box (N : Node_Id) return Node_Id;
   procedure Set_Pn_Box (N : Node_Id; V : Node_Id);

   function Pn_Formalism_Specific_Informations (N : Node_Id) return Node_Id;
   procedure Set_Pn_Formalism_Specific_Informations (N : Node_Id; V : Node_Id);

   function Formalism (N : Node_Id) return Value_Id;
   procedure Set_Formalism (N : Node_Id; V : Value_Id);

   function Low (N : Node_Id) return Value_Id;
   procedure Set_Low (N : Node_Id; V : Value_Id);

   function High (N : Node_Id) return Value_Id;
   procedure Set_High (N : Node_Id; V : Value_Id);

   function Enum (N : Node_Id) return List_Id;
   procedure Set_Enum (N : Node_Id; V : List_Id);

   function Class_List (N : Node_Id) return List_Id;
   procedure Set_Class_List (N : Node_Id; V : List_Id);

   function Domain_List (N : Node_Id) return List_Id;
   procedure Set_Domain_List (N : Node_Id; V : List_Id);

   function Class_Type (N : Node_Id) return Node_Id;
   procedure Set_Class_Type (N : Node_Id; V : Node_Id);

   function Variable_List (N : Node_Id) return List_Id;
   procedure Set_Variable_List (N : Node_Id; V : List_Id);

   function Aadl_Instance (N : Node_Id) return Node_Id;
   procedure Set_Aadl_Instance (N : Node_Id; V : Node_Id);

   function Pn_Id (N : Node_Id) return Value_Id;
   procedure Set_Pn_Id (N : Node_Id; V : Value_Id);

   function Classes (N : Node_Id) return Node_Id;
   procedure Set_Classes (N : Node_Id; V : Node_Id);

   function Domains (N : Node_Id) return List_Id;
   procedure Set_Domains (N : Node_Id; V : List_Id);

   function Variables (N : Node_Id) return List_Id;
   procedure Set_Variables (N : Node_Id; V : List_Id);

   function Threads_Count (N : Node_Id) return Value_Id;
   procedure Set_Threads_Count (N : Node_Id; V : Value_Id);

   function Threads_Ids (N : Node_Id) return List_Id;
   procedure Set_Threads_Ids (N : Node_Id; V : List_Id);

   function Ports_Ids (N : Node_Id) return List_Id;
   procedure Set_Ports_Ids (N : Node_Id; V : List_Id);

   function Tokens (N : Node_Id) return List_Id;
   procedure Set_Tokens (N : Node_Id; V : List_Id);

   function Domain (N : Node_Id) return Node_Id;
   procedure Set_Domain (N : Node_Id; V : Node_Id);

   function Marking (N : Node_Id) return Node_Id;
   procedure Set_Marking (N : Node_Id; V : Node_Id);

   function Nb_T (N : Node_Id) return Value_Id;
   procedure Set_Nb_T (N : Node_Id; V : Value_Id);

   function Operator (N : Node_Id) return Value_Id;
   procedure Set_Operator (N : Node_Id; V : Value_Id);

   function Left_Op (N : Node_Id) return Value_Id;
   procedure Set_Left_Op (N : Node_Id; V : Value_Id);

   function Right_Op (N : Node_Id) return Value_Id;
   procedure Set_Right_Op (N : Node_Id; V : Value_Id);

   function Guards (N : Node_Id) return List_Id;
   procedure Set_Guards (N : Node_Id; V : List_Id);

   function Is_Colored (N : Node_Id) return Boolean;
   procedure Set_Is_Colored (N : Node_Id; V : Boolean);

   function Valuations (N : Node_Id) return List_Id;
   procedure Set_Valuations (N : Node_Id; V : List_Id);

   function P_Instance (N : Node_Id) return Node_Id;
   procedure Set_P_Instance (N : Node_Id; V : Node_Id);

   function Bounded_Trans (N : Node_Id) return List_Id;
   procedure Set_Bounded_Trans (N : Node_Id; V : List_Id);

   function Th_Number (N : Node_Id) return Value_Id;
   procedure Set_Th_Number (N : Node_Id; V : Value_Id);

   function Hyperperiod (N : Node_Id) return Value_Id;
   procedure Set_Hyperperiod (N : Node_Id; V : Value_Id);

   function Priorities (N : Node_Id) return List_Id;
   procedure Set_Priorities (N : Node_Id; V : List_Id);

   function Lower_Value (N : Node_Id) return Value_Id;
   procedure Set_Lower_Value (N : Node_Id; V : Value_Id);

   function Higher_Value (N : Node_Id) return Value_Id;
   procedure Set_Higher_Value (N : Node_Id; V : Value_Id);

   function Braces_Mode (N : Node_Id) return Value_Id;
   procedure Set_Braces_Mode (N : Node_Id; V : Value_Id);

   function Tokens_Number (N : Node_Id) return Value_Id;
   procedure Set_Tokens_Number (N : Node_Id; V : Value_Id);

   function Guard (N : Node_Id) return Node_Id;
   procedure Set_Guard (N : Node_Id; V : Node_Id);

   function Priority (N : Node_Id) return Value_Id;
   procedure Set_Priority (N : Node_Id; V : Value_Id);

   function Valuation (N : Node_Id) return Value_Id;
   procedure Set_Valuation (N : Node_Id; V : Value_Id);

   function is_Priority (N : Node_Id) return Boolean;
   procedure Set_is_Priority (N : Node_Id; V : Boolean);

   function In_Ports (N : Node_Id) return List_Id;
   procedure Set_In_Ports (N : Node_Id; V : List_Id);

   function Out_Ports (N : Node_Id) return List_Id;
   procedure Set_Out_Ports (N : Node_Id; V : List_Id);

   function Call_Seq (N : Node_Id) return List_Id;
   procedure Set_Call_Seq (N : Node_Id; V : List_Id);

   function Th_Instance (N : Node_Id) return Node_Id;
   procedure Set_Th_Instance (N : Node_Id; V : Node_Id);

   function Port_Instance (N : Node_Id) return Node_Id;
   procedure Set_Port_Instance (N : Node_Id; V : Node_Id);

   function Source_Instance (N : Node_Id) return Node_Id;
   procedure Set_Source_Instance (N : Node_Id; V : Node_Id);

   function Target_Instance (N : Node_Id) return Node_Id;
   procedure Set_Target_Instance (N : Node_Id; V : Node_Id);

   function Queue_Size (N : Node_Id) return Value_Id;
   procedure Set_Queue_Size (N : Node_Id; V : Value_Id);

   function Has_CEP (N : Node_Id) return Boolean;
   procedure Set_Has_CEP (N : Node_Id; V : Boolean);

   function Dispatch_Port (N : Node_Id) return Boolean;
   procedure Set_Dispatch_Port (N : Node_Id; V : Boolean);

   function Spg_Call (N : Node_Id) return List_Id;
   procedure Set_Spg_Call (N : Node_Id; V : List_Id);

   function Param_In (N : Node_Id) return List_Id;
   procedure Set_Param_In (N : Node_Id; V : List_Id);

   function Param_Out (N : Node_Id) return List_Id;
   procedure Set_Param_Out (N : Node_Id; V : List_Id);

   function Par_Instance (N : Node_Id) return Node_Id;
   procedure Set_Par_Instance (N : Node_Id; V : Node_Id);

   function Proc_Instance (N : Node_Id) return Node_Id;
   procedure Set_Proc_Instance (N : Node_Id; V : Node_Id);

   procedure W_Node (N : Node_Id);

   type Boolean_Array is array (1 .. 2) of Boolean;
   type Byte_Array is array (1 .. 0) of Byte;
   type Int_Array is array (1 .. 12) of Int;

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

end Ocarina.Backends.PN.Nodes;
