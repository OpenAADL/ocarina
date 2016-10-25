pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with GNAT.Table;
pragma Warnings (Off);
with Locations; use Locations;
with Ocarina.Types;     use Ocarina.Types;
pragma Warnings (On);

package Ocarina.Backends.MAST_Tree.Nodes is

   type Node_Kind is
     (K_Node_Id,
      K_Definition,
      K_MAST_Node,
      K_Driver,
      K_Scheduling_Server,
      K_Scheduling_Server_Parameters,
      K_Scheduler,
      K_Scheduler_Policy,
      K_Literal,
      K_List_Id,
      K_Processing_Resource,
      K_Operation,
      K_Transaction,
      K_Event,
      K_Shared_Resource,
      K_Event_Timing_Requirements,
      K_Event_Handler,
      K_Defining_Identifier,
      K_MAST_File,
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
   --  MAST_Node
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --

   procedure W_MAST_Node (N : Node_Id);

   --
   --  Driver
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Is_Packet_Driver         : Boolean
   --    Send_Operation_Name      : Name_Id
   --    Scheduling_Server        : Name_Id
   --    Receive_Operation_Name   : Name_Id
   --    Message_Partitioning     : Boolean
   --    Is_RTA_Overhead_Model_Coupled: Boolean
   --

   procedure W_Driver (N : Node_Id);

   --
   --  Scheduling_Server
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Associated_Scheduler     : Name_Id
   --    Parameters               : Node_Id
   --    Is_Regular               : Boolean
   --    Server_Processing_Resource: Name_Id
   --

   procedure W_Scheduling_Server (N : Node_Id);

   --
   --  Scheduling_Server_Parameters
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Fixed_Priority           : Boolean
   --    Sched_Type               : Node_Id
   --    Priority                 : Node_Id
   --    Is_Preassigned           : Boolean
   --

   procedure W_Scheduling_Server_Parameters (N : Node_Id);

   --
   --  Scheduler
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Is_Primary_Scheduler     : Boolean
   --    Host                     : Name_Id
   --    Policy                   : Node_Id
   --    Use_Fixed_Priority       : Boolean
   --    Max_Priority             : Node_Id
   --    Min_Priority             : Node_Id
   --

   procedure W_Scheduler (N : Node_Id);

   --
   --  Scheduler_Policy
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Scheduling_Type          : Name_Id
   --    Worst_Context_Switch     : Node_Id
   --    Avg_Context_Switch       : Node_Id
   --    Best_Context_Switch      : Node_Id
   --    Max_Interrupt_Priority   : Node_Id
   --    Min_Interrupt_Priority   : Node_Id
   --

   procedure W_Scheduler_Policy (N : Node_Id);

   --
   --  Literal
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Value                    : Value_Id
   --

   procedure W_Literal (N : Node_Id);

   --
   --  List_Id
   --
   --    First_Node               : Node_Id
   --    Last_Node                : Node_Id
   --

   --
   --  Processing_Resource
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Max_Interrupt_Priority   : Node_Id
   --    Min_Interrupt_Priority   : Node_Id
   --    Worst_ISR_Switch         : Node_Id
   --    Avg_ISR_Switch           : Node_Id
   --    Best_ISR_Switch          : Node_Id
   --    Speed_Factor             : Node_Id
   --    Regular_Processor        : Boolean
   --    Packet_Based_Network     : Boolean
   --    Fixed_Priority_Processor : Boolean
   --    Is_Simplex               : Boolean
   --    Is_Half_Duplex           : Boolean
   --    Is_Full_Duplex           : Boolean
   --    Throughput               : Node_Id
   --    Max_Blocking             : Node_Id
   --    Max_Packet_Size          : Node_Id
   --    Min_Packet_Size          : Node_Id
   --    Max_Packet_Transmission_Time: Node_Id
   --    Min_Packet_Transmission_Time: Node_Id
   --    List_Of_Drivers          : List_Id
   --

   procedure W_Processing_Resource (N : Node_Id);

   --
   --  Operation
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Is_Simple                : Boolean
   --    Is_Enclosing             : Boolean
   --    Is_Message_Transmission  : Boolean
   --    Is_Composite             : Boolean
   --    Operations               : List_Id
   --    Shared_Resources_List    : List_Id
   --    Worst_Case_Execution_Time: Node_Id
   --    Best_Case_Execution_Time : Node_Id
   --    Avg_Case_Execution_Time  : Node_Id
   --    Max_Message_Size         : Node_Id
   --    Avg_Message_Size         : Node_Id
   --    Min_Message_Size         : Node_Id
   --

   procedure W_Operation (N : Node_Id);

   --
   --  Transaction
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Is_Regular               : Boolean
   --    External_Events          : List_Id
   --    Internal_Events          : List_Id
   --    Event_Handlers           : List_Id
   --

   procedure W_Transaction (N : Node_Id);

   --
   --  Event
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Is_Regular               : Boolean
   --    Is_Sporadic              : Boolean
   --    Is_Periodic              : Boolean
   --    Timing_Requirements      : Node_Id
   --    Min_Interarrival         : Node_Id
   --    Period                   : Node_Id
   --

   procedure W_Event (N : Node_Id);

   --
   --  Shared_Resource
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Node_Name                : Name_Id
   --    Node_Type                : Name_Id
   --    Is_Immediate_Ceiling_Resource: Boolean
   --

   procedure W_Shared_Resource (N : Node_Id);

   --
   --  Event_Timing_Requirements
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Is_Hard_Deadline         : Boolean
   --    Deadline                 : Node_Id
   --    Referenced_Event         : Name_Id
   --

   procedure W_Event_Timing_Requirements (N : Node_Id);

   --
   --  Event_Handler
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Is_Activity              : Boolean
   --    Input_Name               : Name_Id
   --    Output_Name              : Name_Id
   --    Operation_Name           : Name_Id
   --    Server_Name              : Name_Id
   --

   procedure W_Event_Handler (N : Node_Id);

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
   --  MAST_File
   --
   --    Next_Node                : Node_Id
   --    Frontend_Node            : Node_Id
   --    Defining_Identifier      : Node_Id
   --    Distributed_Application_Unit: Node_Id
   --    Declarations             : List_Id
   --

   procedure W_MAST_File (N : Node_Id);

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

   function Node_Name (N : Node_Id) return Name_Id;
   procedure Set_Node_Name (N : Node_Id; V : Name_Id);

   function Node_Type (N : Node_Id) return Name_Id;
   procedure Set_Node_Type (N : Node_Id; V : Name_Id);

   function Is_Packet_Driver (N : Node_Id) return Boolean;
   procedure Set_Is_Packet_Driver (N : Node_Id; V : Boolean);

   function Send_Operation_Name (N : Node_Id) return Name_Id;
   procedure Set_Send_Operation_Name (N : Node_Id; V : Name_Id);

   function Scheduling_Server (N : Node_Id) return Name_Id;
   procedure Set_Scheduling_Server (N : Node_Id; V : Name_Id);

   function Receive_Operation_Name (N : Node_Id) return Name_Id;
   procedure Set_Receive_Operation_Name (N : Node_Id; V : Name_Id);

   function Message_Partitioning (N : Node_Id) return Boolean;
   procedure Set_Message_Partitioning (N : Node_Id; V : Boolean);

   function Is_RTA_Overhead_Model_Coupled (N : Node_Id) return Boolean;
   procedure Set_Is_RTA_Overhead_Model_Coupled (N : Node_Id; V : Boolean);

   function Associated_Scheduler (N : Node_Id) return Name_Id;
   procedure Set_Associated_Scheduler (N : Node_Id; V : Name_Id);

   function Parameters (N : Node_Id) return Node_Id;
   procedure Set_Parameters (N : Node_Id; V : Node_Id);

   function Is_Regular (N : Node_Id) return Boolean;
   procedure Set_Is_Regular (N : Node_Id; V : Boolean);

   function Server_Processing_Resource (N : Node_Id) return Name_Id;
   procedure Set_Server_Processing_Resource (N : Node_Id; V : Name_Id);

   function Fixed_Priority (N : Node_Id) return Boolean;
   procedure Set_Fixed_Priority (N : Node_Id; V : Boolean);

   function Sched_Type (N : Node_Id) return Node_Id;
   procedure Set_Sched_Type (N : Node_Id; V : Node_Id);

   function Priority (N : Node_Id) return Node_Id;
   procedure Set_Priority (N : Node_Id; V : Node_Id);

   function Is_Preassigned (N : Node_Id) return Boolean;
   procedure Set_Is_Preassigned (N : Node_Id; V : Boolean);

   function Is_Primary_Scheduler (N : Node_Id) return Boolean;
   procedure Set_Is_Primary_Scheduler (N : Node_Id; V : Boolean);

   function Host (N : Node_Id) return Name_Id;
   procedure Set_Host (N : Node_Id; V : Name_Id);

   function Policy (N : Node_Id) return Node_Id;
   procedure Set_Policy (N : Node_Id; V : Node_Id);

   function Use_Fixed_Priority (N : Node_Id) return Boolean;
   procedure Set_Use_Fixed_Priority (N : Node_Id; V : Boolean);

   function Max_Priority (N : Node_Id) return Node_Id;
   procedure Set_Max_Priority (N : Node_Id; V : Node_Id);

   function Min_Priority (N : Node_Id) return Node_Id;
   procedure Set_Min_Priority (N : Node_Id; V : Node_Id);

   function Scheduling_Type (N : Node_Id) return Name_Id;
   procedure Set_Scheduling_Type (N : Node_Id; V : Name_Id);

   function Worst_Context_Switch (N : Node_Id) return Node_Id;
   procedure Set_Worst_Context_Switch (N : Node_Id; V : Node_Id);

   function Avg_Context_Switch (N : Node_Id) return Node_Id;
   procedure Set_Avg_Context_Switch (N : Node_Id; V : Node_Id);

   function Best_Context_Switch (N : Node_Id) return Node_Id;
   procedure Set_Best_Context_Switch (N : Node_Id; V : Node_Id);

   function Max_Interrupt_Priority (N : Node_Id) return Node_Id;
   procedure Set_Max_Interrupt_Priority (N : Node_Id; V : Node_Id);

   function Min_Interrupt_Priority (N : Node_Id) return Node_Id;
   procedure Set_Min_Interrupt_Priority (N : Node_Id; V : Node_Id);

   function Value (N : Node_Id) return Value_Id;
   procedure Set_Value (N : Node_Id; V : Value_Id);

   function First_Node (N : List_Id) return Node_Id;
   procedure Set_First_Node (N : List_Id; V : Node_Id);

   function Last_Node (N : List_Id) return Node_Id;
   procedure Set_Last_Node (N : List_Id; V : Node_Id);

   function Worst_ISR_Switch (N : Node_Id) return Node_Id;
   procedure Set_Worst_ISR_Switch (N : Node_Id; V : Node_Id);

   function Avg_ISR_Switch (N : Node_Id) return Node_Id;
   procedure Set_Avg_ISR_Switch (N : Node_Id; V : Node_Id);

   function Best_ISR_Switch (N : Node_Id) return Node_Id;
   procedure Set_Best_ISR_Switch (N : Node_Id; V : Node_Id);

   function Speed_Factor (N : Node_Id) return Node_Id;
   procedure Set_Speed_Factor (N : Node_Id; V : Node_Id);

   function Regular_Processor (N : Node_Id) return Boolean;
   procedure Set_Regular_Processor (N : Node_Id; V : Boolean);

   function Packet_Based_Network (N : Node_Id) return Boolean;
   procedure Set_Packet_Based_Network (N : Node_Id; V : Boolean);

   function Fixed_Priority_Processor (N : Node_Id) return Boolean;
   procedure Set_Fixed_Priority_Processor (N : Node_Id; V : Boolean);

   function Is_Simplex (N : Node_Id) return Boolean;
   procedure Set_Is_Simplex (N : Node_Id; V : Boolean);

   function Is_Half_Duplex (N : Node_Id) return Boolean;
   procedure Set_Is_Half_Duplex (N : Node_Id; V : Boolean);

   function Is_Full_Duplex (N : Node_Id) return Boolean;
   procedure Set_Is_Full_Duplex (N : Node_Id; V : Boolean);

   function Throughput (N : Node_Id) return Node_Id;
   procedure Set_Throughput (N : Node_Id; V : Node_Id);

   function Max_Blocking (N : Node_Id) return Node_Id;
   procedure Set_Max_Blocking (N : Node_Id; V : Node_Id);

   function Max_Packet_Size (N : Node_Id) return Node_Id;
   procedure Set_Max_Packet_Size (N : Node_Id; V : Node_Id);

   function Min_Packet_Size (N : Node_Id) return Node_Id;
   procedure Set_Min_Packet_Size (N : Node_Id; V : Node_Id);

   function Max_Packet_Transmission_Time (N : Node_Id) return Node_Id;
   procedure Set_Max_Packet_Transmission_Time (N : Node_Id; V : Node_Id);

   function Min_Packet_Transmission_Time (N : Node_Id) return Node_Id;
   procedure Set_Min_Packet_Transmission_Time (N : Node_Id; V : Node_Id);

   function List_Of_Drivers (N : Node_Id) return List_Id;
   procedure Set_List_Of_Drivers (N : Node_Id; V : List_Id);

   function Is_Simple (N : Node_Id) return Boolean;
   procedure Set_Is_Simple (N : Node_Id; V : Boolean);

   function Is_Enclosing (N : Node_Id) return Boolean;
   procedure Set_Is_Enclosing (N : Node_Id; V : Boolean);

   function Is_Message_Transmission (N : Node_Id) return Boolean;
   procedure Set_Is_Message_Transmission (N : Node_Id; V : Boolean);

   function Is_Composite (N : Node_Id) return Boolean;
   procedure Set_Is_Composite (N : Node_Id; V : Boolean);

   function Operations (N : Node_Id) return List_Id;
   procedure Set_Operations (N : Node_Id; V : List_Id);

   function Shared_Resources_List (N : Node_Id) return List_Id;
   procedure Set_Shared_Resources_List (N : Node_Id; V : List_Id);

   function Worst_Case_Execution_Time (N : Node_Id) return Node_Id;
   procedure Set_Worst_Case_Execution_Time (N : Node_Id; V : Node_Id);

   function Best_Case_Execution_Time (N : Node_Id) return Node_Id;
   procedure Set_Best_Case_Execution_Time (N : Node_Id; V : Node_Id);

   function Avg_Case_Execution_Time (N : Node_Id) return Node_Id;
   procedure Set_Avg_Case_Execution_Time (N : Node_Id; V : Node_Id);

   function Max_Message_Size (N : Node_Id) return Node_Id;
   procedure Set_Max_Message_Size (N : Node_Id; V : Node_Id);

   function Avg_Message_Size (N : Node_Id) return Node_Id;
   procedure Set_Avg_Message_Size (N : Node_Id; V : Node_Id);

   function Min_Message_Size (N : Node_Id) return Node_Id;
   procedure Set_Min_Message_Size (N : Node_Id; V : Node_Id);

   function External_Events (N : Node_Id) return List_Id;
   procedure Set_External_Events (N : Node_Id; V : List_Id);

   function Internal_Events (N : Node_Id) return List_Id;
   procedure Set_Internal_Events (N : Node_Id; V : List_Id);

   function Event_Handlers (N : Node_Id) return List_Id;
   procedure Set_Event_Handlers (N : Node_Id; V : List_Id);

   function Is_Sporadic (N : Node_Id) return Boolean;
   procedure Set_Is_Sporadic (N : Node_Id; V : Boolean);

   function Is_Periodic (N : Node_Id) return Boolean;
   procedure Set_Is_Periodic (N : Node_Id; V : Boolean);

   function Timing_Requirements (N : Node_Id) return Node_Id;
   procedure Set_Timing_Requirements (N : Node_Id; V : Node_Id);

   function Min_Interarrival (N : Node_Id) return Node_Id;
   procedure Set_Min_Interarrival (N : Node_Id; V : Node_Id);

   function Period (N : Node_Id) return Node_Id;
   procedure Set_Period (N : Node_Id; V : Node_Id);

   function Is_Immediate_Ceiling_Resource (N : Node_Id) return Boolean;
   procedure Set_Is_Immediate_Ceiling_Resource (N : Node_Id; V : Boolean);

   function Is_Hard_Deadline (N : Node_Id) return Boolean;
   procedure Set_Is_Hard_Deadline (N : Node_Id; V : Boolean);

   function Deadline (N : Node_Id) return Node_Id;
   procedure Set_Deadline (N : Node_Id; V : Node_Id);

   function Referenced_Event (N : Node_Id) return Name_Id;
   procedure Set_Referenced_Event (N : Node_Id; V : Name_Id);

   function Is_Activity (N : Node_Id) return Boolean;
   procedure Set_Is_Activity (N : Node_Id; V : Boolean);

   function Input_Name (N : Node_Id) return Name_Id;
   procedure Set_Input_Name (N : Node_Id; V : Name_Id);

   function Output_Name (N : Node_Id) return Name_Id;
   procedure Set_Output_Name (N : Node_Id; V : Name_Id);

   function Operation_Name (N : Node_Id) return Name_Id;
   procedure Set_Operation_Name (N : Node_Id; V : Name_Id);

   function Server_Name (N : Node_Id) return Name_Id;
   procedure Set_Server_Name (N : Node_Id; V : Name_Id);

   function Name (N : Node_Id) return Name_Id;
   procedure Set_Name (N : Node_Id; V : Name_Id);

   function Corresponding_Node (N : Node_Id) return Node_Id;
   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id);

   function Compile_Unit (N : Node_Id) return Node_Id;
   procedure Set_Compile_Unit (N : Node_Id; V : Node_Id);

   function Distributed_Application_Unit (N : Node_Id) return Node_Id;
   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id);

   function Declarations (N : Node_Id) return List_Id;
   procedure Set_Declarations (N : Node_Id; V : List_Id);

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

   type Boolean_Array is array (1 .. 6) of Boolean;
   type Byte_Array is array (1 .. 0) of Byte;
   type Int_Array is array (1 .. 23) of Int;

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

end Ocarina.Backends.MAST_Tree.Nodes;
