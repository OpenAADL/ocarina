pragma Style_Checks ("NM32766");

--  This file has been generated automatically by `mknodes'. Do not
--  hand modify this file since your changes will be overridden.

with Ocarina.Backends.MAST_Tree.Debug; use Ocarina.Backends.MAST_Tree.Debug;

package body Ocarina.Backends.MAST_Tree.Nodes is

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
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File
        or else Table (Types.Node_Id (N)).Kind = K_Container
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (7));
   end Next_Node;

   procedure Set_Next_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File
        or else Table (Types.Node_Id (N)).Kind = K_Container
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (7) := Int (V);
   end Set_Next_Node;

   function Frontend_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File
        or else Table (Types.Node_Id (N)).Kind = K_Container
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (8));
   end Frontend_Node;

   procedure Set_Frontend_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Node_Id
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Literal
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File
        or else Table (Types.Node_Id (N)).Kind = K_Container
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (8) := Int (V);
   end Set_Frontend_Node;

   function Defining_Identifier (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Defining_Identifier;

   procedure Set_Defining_Identifier (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Definition
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Defining_Identifier;

   function Node_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource);

      return Name_Id (Table (Types.Node_Id (N)).L (9));
   end Node_Name;

   procedure Set_Node_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource);

      Table (Types.Node_Id (N)).L (9) := Int (V);
   end Set_Node_Name;

   function Node_Type (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource);

      return Name_Id (Table (Types.Node_Id (N)).L (10));
   end Node_Type;

   procedure Set_Node_Type (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_Node
        or else Table (Types.Node_Id (N)).Kind = K_Driver
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource
        or else Table (Types.Node_Id (N)).Kind = K_Operation
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource);

      Table (Types.Node_Id (N)).L (10) := Int (V);
   end Set_Node_Type;

   function Is_Packet_Driver (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Packet_Driver;

   procedure Set_Is_Packet_Driver (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Packet_Driver;

   function Send_Operation_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      return Name_Id (Table (Types.Node_Id (N)).L (4));
   end Send_Operation_Name;

   procedure Set_Send_Operation_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Send_Operation_Name;

   function Scheduling_Server (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      return Name_Id (Table (Types.Node_Id (N)).L (5));
   end Scheduling_Server;

   procedure Set_Scheduling_Server (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Scheduling_Server;

   function Receive_Operation_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      return Name_Id (Table (Types.Node_Id (N)).L (6));
   end Receive_Operation_Name;

   procedure Set_Receive_Operation_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Receive_Operation_Name;

   function Message_Partitioning (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Message_Partitioning;

   procedure Set_Message_Partitioning (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Message_Partitioning;

   function Is_RTA_Overhead_Model_Coupled (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_RTA_Overhead_Model_Coupled;

   procedure Set_Is_RTA_Overhead_Model_Coupled (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Driver);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_RTA_Overhead_Model_Coupled;

   function Associated_Scheduler (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server);

      return Name_Id (Table (Types.Node_Id (N)).L (2));
   end Associated_Scheduler;

   procedure Set_Associated_Scheduler (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Associated_Scheduler;

   function Parameters (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Parameters;

   procedure Set_Parameters (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Parameters;

   function Is_Regular (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Regular;

   procedure Set_Is_Regular (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server
        or else Table (Types.Node_Id (N)).Kind = K_Transaction
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Regular;

   function Server_Processing_Resource (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server);

      return Name_Id (Table (Types.Node_Id (N)).L (4));
   end Server_Processing_Resource;

   procedure Set_Server_Processing_Resource (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Server_Processing_Resource;

   function Fixed_Priority (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Fixed_Priority;

   procedure Set_Fixed_Priority (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Fixed_Priority;

   function Sched_Type (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Sched_Type;

   procedure Set_Sched_Type (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Sched_Type;

   function Priority (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Priority;

   procedure Set_Priority (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Priority;

   function Is_Preassigned (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Preassigned;

   procedure Set_Is_Preassigned (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduling_Server_Parameters);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Preassigned;

   function Is_Primary_Scheduler (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Primary_Scheduler;

   procedure Set_Is_Primary_Scheduler (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Primary_Scheduler;

   function Host (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      return Name_Id (Table (Types.Node_Id (N)).L (3));
   end Host;

   procedure Set_Host (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Host;

   function Policy (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Policy;

   procedure Set_Policy (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Policy;

   function Use_Fixed_Priority (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Use_Fixed_Priority;

   procedure Set_Use_Fixed_Priority (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Use_Fixed_Priority;

   function Max_Priority (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Max_Priority;

   procedure Set_Max_Priority (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Max_Priority;

   function Min_Priority (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Min_Priority;

   procedure Set_Min_Priority (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Min_Priority;

   function Scheduling_Type (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Scheduling_Type;

   procedure Set_Scheduling_Type (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Scheduling_Type;

   function Worst_Context_Switch (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Worst_Context_Switch;

   procedure Set_Worst_Context_Switch (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Worst_Context_Switch;

   function Avg_Context_Switch (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Avg_Context_Switch;

   procedure Set_Avg_Context_Switch (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Avg_Context_Switch;

   function Best_Context_Switch (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Best_Context_Switch;

   procedure Set_Best_Context_Switch (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Best_Context_Switch;

   function Max_Interrupt_Priority (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Max_Interrupt_Priority;

   procedure Set_Max_Interrupt_Priority (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Max_Interrupt_Priority;

   function Min_Interrupt_Priority (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Min_Interrupt_Priority;

   procedure Set_Min_Interrupt_Priority (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Scheduler_Policy
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Min_Interrupt_Priority;

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

   function First_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end First_Node;

   procedure Set_First_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_First_Node;

   function Last_Node (N : List_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Last_Node;

   procedure Set_Last_Node (N : List_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_List_Id);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Last_Node;

   function Worst_ISR_Switch (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end Worst_ISR_Switch;

   procedure Set_Worst_ISR_Switch (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Worst_ISR_Switch;

   function Avg_ISR_Switch (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end Avg_ISR_Switch;

   procedure Set_Avg_ISR_Switch (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Avg_ISR_Switch;

   function Best_ISR_Switch (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Best_ISR_Switch;

   procedure Set_Best_ISR_Switch (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Best_ISR_Switch;

   function Speed_Factor (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Speed_Factor;

   procedure Set_Speed_Factor (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Speed_Factor;

   function Regular_Processor (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Regular_Processor;

   procedure Set_Regular_Processor (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Regular_Processor;

   function Packet_Based_Network (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Packet_Based_Network;

   procedure Set_Packet_Based_Network (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Packet_Based_Network;

   function Fixed_Priority_Processor (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Fixed_Priority_Processor;

   procedure Set_Fixed_Priority_Processor (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Fixed_Priority_Processor;

   function Is_Simplex (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Simplex;

   procedure Set_Is_Simplex (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Simplex;

   function Is_Half_Duplex (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Boolean (Table (Types.Node_Id (N)).B (5));
   end Is_Half_Duplex;

   procedure Set_Is_Half_Duplex (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).B (5) := Boolean (V);
   end Set_Is_Half_Duplex;

   function Is_Full_Duplex (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Boolean (Table (Types.Node_Id (N)).B (6));
   end Is_Full_Duplex;

   procedure Set_Is_Full_Duplex (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).B (6) := Boolean (V);
   end Set_Is_Full_Duplex;

   function Throughput (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (17));
   end Throughput;

   procedure Set_Throughput (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (17) := Int (V);
   end Set_Throughput;

   function Max_Blocking (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (18));
   end Max_Blocking;

   procedure Set_Max_Blocking (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (18) := Int (V);
   end Set_Max_Blocking;

   function Max_Packet_Size (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (19));
   end Max_Packet_Size;

   procedure Set_Max_Packet_Size (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (19) := Int (V);
   end Set_Max_Packet_Size;

   function Min_Packet_Size (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (20));
   end Min_Packet_Size;

   procedure Set_Min_Packet_Size (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (20) := Int (V);
   end Set_Min_Packet_Size;

   function Max_Packet_Transmission_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (21));
   end Max_Packet_Transmission_Time;

   procedure Set_Max_Packet_Transmission_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (21) := Int (V);
   end Set_Max_Packet_Transmission_Time;

   function Min_Packet_Transmission_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return Node_Id (Table (Types.Node_Id (N)).L (22));
   end Min_Packet_Transmission_Time;

   procedure Set_Min_Packet_Transmission_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (22) := Int (V);
   end Set_Min_Packet_Transmission_Time;

   function List_Of_Drivers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      return List_Id (Table (Types.Node_Id (N)).L (23));
   end List_Of_Drivers;

   procedure Set_List_Of_Drivers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Processing_Resource);

      Table (Types.Node_Id (N)).L (23) := Int (V);
   end Set_List_Of_Drivers;

   function Is_Simple (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Simple;

   procedure Set_Is_Simple (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Simple;

   function Is_Enclosing (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Enclosing;

   procedure Set_Is_Enclosing (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Enclosing;

   function Is_Message_Transmission (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Message_Transmission;

   procedure Set_Is_Message_Transmission (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Message_Transmission;

   function Is_Composite (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Boolean (Table (Types.Node_Id (N)).B (4));
   end Is_Composite;

   procedure Set_Is_Composite (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).B (4) := Boolean (V);
   end Set_Is_Composite;

   function Operations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return List_Id (Table (Types.Node_Id (N)).L (5));
   end Operations;

   procedure Set_Operations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Operations;

   function Shared_Resources_List (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return List_Id (Table (Types.Node_Id (N)).L (6));
   end Shared_Resources_List;

   procedure Set_Shared_Resources_List (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Shared_Resources_List;

   function Worst_Case_Execution_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Node_Id (Table (Types.Node_Id (N)).L (11));
   end Worst_Case_Execution_Time;

   procedure Set_Worst_Case_Execution_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (11) := Int (V);
   end Set_Worst_Case_Execution_Time;

   function Best_Case_Execution_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Node_Id (Table (Types.Node_Id (N)).L (12));
   end Best_Case_Execution_Time;

   procedure Set_Best_Case_Execution_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (12) := Int (V);
   end Set_Best_Case_Execution_Time;

   function Avg_Case_Execution_Time (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Node_Id (Table (Types.Node_Id (N)).L (13));
   end Avg_Case_Execution_Time;

   procedure Set_Avg_Case_Execution_Time (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (13) := Int (V);
   end Set_Avg_Case_Execution_Time;

   function Max_Message_Size (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Node_Id (Table (Types.Node_Id (N)).L (14));
   end Max_Message_Size;

   procedure Set_Max_Message_Size (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (14) := Int (V);
   end Set_Max_Message_Size;

   function Avg_Message_Size (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Node_Id (Table (Types.Node_Id (N)).L (15));
   end Avg_Message_Size;

   procedure Set_Avg_Message_Size (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (15) := Int (V);
   end Set_Avg_Message_Size;

   function Min_Message_Size (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      return Node_Id (Table (Types.Node_Id (N)).L (16));
   end Min_Message_Size;

   procedure Set_Min_Message_Size (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Operation);

      Table (Types.Node_Id (N)).L (16) := Int (V);
   end Set_Min_Message_Size;

   function External_Events (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transaction);

      return List_Id (Table (Types.Node_Id (N)).L (2));
   end External_Events;

   procedure Set_External_Events (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transaction);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_External_Events;

   function Internal_Events (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transaction);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Internal_Events;

   procedure Set_Internal_Events (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transaction);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Internal_Events;

   function Event_Handlers (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transaction);

      return List_Id (Table (Types.Node_Id (N)).L (4));
   end Event_Handlers;

   procedure Set_Event_Handlers (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Transaction);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Event_Handlers;

   function Is_Sporadic (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      return Boolean (Table (Types.Node_Id (N)).B (2));
   end Is_Sporadic;

   procedure Set_Is_Sporadic (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      Table (Types.Node_Id (N)).B (2) := Boolean (V);
   end Set_Is_Sporadic;

   function Is_Periodic (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      return Boolean (Table (Types.Node_Id (N)).B (3));
   end Is_Periodic;

   procedure Set_Is_Periodic (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      Table (Types.Node_Id (N)).B (3) := Boolean (V);
   end Set_Is_Periodic;

   function Timing_Requirements (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (4));
   end Timing_Requirements;

   procedure Set_Timing_Requirements (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Timing_Requirements;

   function Min_Interarrival (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (5));
   end Min_Interarrival;

   procedure Set_Min_Interarrival (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Min_Interarrival;

   function Period (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      return Node_Id (Table (Types.Node_Id (N)).L (6));
   end Period;

   procedure Set_Period (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event);

      Table (Types.Node_Id (N)).L (6) := Int (V);
   end Set_Period;

   function Is_Immediate_Ceiling_Resource (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Immediate_Ceiling_Resource;

   procedure Set_Is_Immediate_Ceiling_Resource (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Shared_Resource);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Immediate_Ceiling_Resource;

   function Is_Hard_Deadline (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Hard_Deadline;

   procedure Set_Is_Hard_Deadline (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Hard_Deadline;

   function Deadline (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Deadline;

   procedure Set_Deadline (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Deadline;

   function Referenced_Event (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements);

      return Name_Id (Table (Types.Node_Id (N)).L (3));
   end Referenced_Event;

   procedure Set_Referenced_Event (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Timing_Requirements);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Referenced_Event;

   function Is_Activity (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      return Boolean (Table (Types.Node_Id (N)).B (1));
   end Is_Activity;

   procedure Set_Is_Activity (N : Node_Id; V : Boolean) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      Table (Types.Node_Id (N)).B (1) := Boolean (V);
   end Set_Is_Activity;

   function Input_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      return Name_Id (Table (Types.Node_Id (N)).L (2));
   end Input_Name;

   procedure Set_Input_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Input_Name;

   function Output_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      return Name_Id (Table (Types.Node_Id (N)).L (3));
   end Output_Name;

   procedure Set_Output_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Output_Name;

   function Operation_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      return Name_Id (Table (Types.Node_Id (N)).L (4));
   end Operation_Name;

   procedure Set_Operation_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      Table (Types.Node_Id (N)).L (4) := Int (V);
   end Set_Operation_Name;

   function Server_Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      return Name_Id (Table (Types.Node_Id (N)).L (5));
   end Server_Name;

   procedure Set_Server_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Event_Handler);

      Table (Types.Node_Id (N)).L (5) := Int (V);
   end Set_Server_Name;

   function Name (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Name;

   procedure Set_Name (N : Node_Id; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Name;

   function Corresponding_Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Corresponding_Node;

   procedure Set_Corresponding_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Corresponding_Node;

   function Compile_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      return Node_Id (Table (Types.Node_Id (N)).L (3));
   end Compile_Unit;

   procedure Set_Compile_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Defining_Identifier);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Compile_Unit;

   function Distributed_Application_Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Distributed_Application_Unit;

   procedure Set_Distributed_Application_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Distributed_Application_Unit;

   function Declarations (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Declarations;

   procedure Set_Declarations (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_MAST_File);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Declarations;

   function Image (N : Base_Type) return Name_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_String
        or else Table (Types.Node_Id (N)).Kind = K_Numeric
        or else Table (Types.Node_Id (N)).Kind = K_Float);

      return Name_Id (Table (Types.Node_Id (N)).L (1));
   end Image;

   procedure Set_Image (N : Base_Type; V : Name_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Base_Type
        or else Table (Types.Node_Id (N)).Kind = K_String
        or else Table (Types.Node_Id (N)).Kind = K_Numeric
        or else Table (Types.Node_Id (N)).Kind = K_Float);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Image;

   function Content (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Container);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Content;

   procedure Set_Content (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_Container);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Content;

   function Unit (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (1));
   end Unit;

   procedure Set_Unit (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (1) := Int (V);
   end Set_Unit;

   function Node (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return Node_Id (Table (Types.Node_Id (N)).L (2));
   end Node;

   procedure Set_Node (N : Node_Id; V : Node_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (2) := Int (V);
   end Set_Node;

   function Processes (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      return List_Id (Table (Types.Node_Id (N)).L (3));
   end Processes;

   procedure Set_Processes (N : Node_Id; V : List_Id) is
   begin
      pragma Assert (False
        or else Table (Types.Node_Id (N)).Kind = K_HI_Tree_Bindings);

      Table (Types.Node_Id (N)).L (3) := Int (V);
   end Set_Processes;

   procedure W_Node (N : Node_Id) is
   begin
      case Kind (N) is
         when K_Definition =>
            W_Definition
              (Node_Id (N));
         when K_MAST_Node =>
            W_MAST_Node
              (Node_Id (N));
         when K_Driver =>
            W_Driver
              (Node_Id (N));
         when K_Scheduling_Server =>
            W_Scheduling_Server
              (Node_Id (N));
         when K_Scheduling_Server_Parameters =>
            W_Scheduling_Server_Parameters
              (Node_Id (N));
         when K_Scheduler =>
            W_Scheduler
              (Node_Id (N));
         when K_Scheduler_Policy =>
            W_Scheduler_Policy
              (Node_Id (N));
         when K_Literal =>
            W_Literal
              (Node_Id (N));
         when K_Processing_Resource =>
            W_Processing_Resource
              (Node_Id (N));
         when K_Operation =>
            W_Operation
              (Node_Id (N));
         when K_Transaction =>
            W_Transaction
              (Node_Id (N));
         when K_Event =>
            W_Event
              (Node_Id (N));
         when K_Shared_Resource =>
            W_Shared_Resource
              (Node_Id (N));
         when K_Event_Timing_Requirements =>
            W_Event_Timing_Requirements
              (Node_Id (N));
         when K_Event_Handler =>
            W_Event_Handler
              (Node_Id (N));
         when K_Defining_Identifier =>
            W_Defining_Identifier
              (Node_Id (N));
         when K_MAST_File =>
            W_MAST_File
              (Node_Id (N));
         when K_Container =>
            W_Container
              (Node_Id (N));
         when K_String =>
            W_String
              (Base_Type (N));
         when K_Numeric =>
            W_Numeric
              (Base_Type (N));
         when K_Float =>
            W_Float
              (Base_Type (N));
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
   end W_Definition;

   procedure W_MAST_Node (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
   end W_MAST_Node;

   procedure W_Driver (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Is_Packet_Driver",
         "Boolean",
         Image (Is_Packet_Driver (N)));
      W_Node_Attribute
        ("Send_Operation_Name",
         "Name_Id",
         Image (Send_Operation_Name (N)));
      W_Node_Attribute
        ("Scheduling_Server",
         "Name_Id",
         Image (Scheduling_Server (N)));
      W_Node_Attribute
        ("Receive_Operation_Name",
         "Name_Id",
         Image (Receive_Operation_Name (N)));
      W_Node_Attribute
        ("Message_Partitioning",
         "Boolean",
         Image (Message_Partitioning (N)));
      W_Node_Attribute
        ("Is_RTA_Overhead_Model_Coupled",
         "Boolean",
         Image (Is_RTA_Overhead_Model_Coupled (N)));
   end W_Driver;

   procedure W_Scheduling_Server (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Associated_Scheduler",
         "Name_Id",
         Image (Associated_Scheduler (N)));
      W_Node_Attribute
        ("Parameters",
         "Node_Id",
         Image (Parameters (N)),
         Int (Parameters (N)));
      W_Node_Attribute
        ("Is_Regular",
         "Boolean",
         Image (Is_Regular (N)));
      W_Node_Attribute
        ("Server_Processing_Resource",
         "Name_Id",
         Image (Server_Processing_Resource (N)));
   end W_Scheduling_Server;

   procedure W_Scheduling_Server_Parameters (N : Node_Id) is
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
        ("Fixed_Priority",
         "Boolean",
         Image (Fixed_Priority (N)));
      W_Node_Attribute
        ("Sched_Type",
         "Node_Id",
         Image (Sched_Type (N)),
         Int (Sched_Type (N)));
      W_Node_Attribute
        ("Priority",
         "Node_Id",
         Image (Priority (N)),
         Int (Priority (N)));
      W_Node_Attribute
        ("Is_Preassigned",
         "Boolean",
         Image (Is_Preassigned (N)));
   end W_Scheduling_Server_Parameters;

   procedure W_Scheduler (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Is_Primary_Scheduler",
         "Boolean",
         Image (Is_Primary_Scheduler (N)));
      W_Node_Attribute
        ("Host",
         "Name_Id",
         Image (Host (N)));
      W_Node_Attribute
        ("Policy",
         "Node_Id",
         Image (Policy (N)),
         Int (Policy (N)));
      W_Node_Attribute
        ("Use_Fixed_Priority",
         "Boolean",
         Image (Use_Fixed_Priority (N)));
      W_Node_Attribute
        ("Max_Priority",
         "Node_Id",
         Image (Max_Priority (N)),
         Int (Max_Priority (N)));
      W_Node_Attribute
        ("Min_Priority",
         "Node_Id",
         Image (Min_Priority (N)),
         Int (Min_Priority (N)));
   end W_Scheduler;

   procedure W_Scheduler_Policy (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Scheduling_Type",
         "Name_Id",
         Image (Scheduling_Type (N)));
      W_Node_Attribute
        ("Worst_Context_Switch",
         "Node_Id",
         Image (Worst_Context_Switch (N)),
         Int (Worst_Context_Switch (N)));
      W_Node_Attribute
        ("Avg_Context_Switch",
         "Node_Id",
         Image (Avg_Context_Switch (N)),
         Int (Avg_Context_Switch (N)));
      W_Node_Attribute
        ("Best_Context_Switch",
         "Node_Id",
         Image (Best_Context_Switch (N)),
         Int (Best_Context_Switch (N)));
      W_Node_Attribute
        ("Max_Interrupt_Priority",
         "Node_Id",
         Image (Max_Interrupt_Priority (N)),
         Int (Max_Interrupt_Priority (N)));
      W_Node_Attribute
        ("Min_Interrupt_Priority",
         "Node_Id",
         Image (Min_Interrupt_Priority (N)),
         Int (Min_Interrupt_Priority (N)));
   end W_Scheduler_Policy;

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

   procedure W_Processing_Resource (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Max_Interrupt_Priority",
         "Node_Id",
         Image (Max_Interrupt_Priority (N)),
         Int (Max_Interrupt_Priority (N)));
      W_Node_Attribute
        ("Min_Interrupt_Priority",
         "Node_Id",
         Image (Min_Interrupt_Priority (N)),
         Int (Min_Interrupt_Priority (N)));
      W_Node_Attribute
        ("Worst_ISR_Switch",
         "Node_Id",
         Image (Worst_ISR_Switch (N)),
         Int (Worst_ISR_Switch (N)));
      W_Node_Attribute
        ("Avg_ISR_Switch",
         "Node_Id",
         Image (Avg_ISR_Switch (N)),
         Int (Avg_ISR_Switch (N)));
      W_Node_Attribute
        ("Best_ISR_Switch",
         "Node_Id",
         Image (Best_ISR_Switch (N)),
         Int (Best_ISR_Switch (N)));
      W_Node_Attribute
        ("Speed_Factor",
         "Node_Id",
         Image (Speed_Factor (N)),
         Int (Speed_Factor (N)));
      W_Node_Attribute
        ("Regular_Processor",
         "Boolean",
         Image (Regular_Processor (N)));
      W_Node_Attribute
        ("Packet_Based_Network",
         "Boolean",
         Image (Packet_Based_Network (N)));
      W_Node_Attribute
        ("Fixed_Priority_Processor",
         "Boolean",
         Image (Fixed_Priority_Processor (N)));
      W_Node_Attribute
        ("Is_Simplex",
         "Boolean",
         Image (Is_Simplex (N)));
      W_Node_Attribute
        ("Is_Half_Duplex",
         "Boolean",
         Image (Is_Half_Duplex (N)));
      W_Node_Attribute
        ("Is_Full_Duplex",
         "Boolean",
         Image (Is_Full_Duplex (N)));
      W_Node_Attribute
        ("Throughput",
         "Node_Id",
         Image (Throughput (N)),
         Int (Throughput (N)));
      W_Node_Attribute
        ("Max_Blocking",
         "Node_Id",
         Image (Max_Blocking (N)),
         Int (Max_Blocking (N)));
      W_Node_Attribute
        ("Max_Packet_Size",
         "Node_Id",
         Image (Max_Packet_Size (N)),
         Int (Max_Packet_Size (N)));
      W_Node_Attribute
        ("Min_Packet_Size",
         "Node_Id",
         Image (Min_Packet_Size (N)),
         Int (Min_Packet_Size (N)));
      W_Node_Attribute
        ("Max_Packet_Transmission_Time",
         "Node_Id",
         Image (Max_Packet_Transmission_Time (N)),
         Int (Max_Packet_Transmission_Time (N)));
      W_Node_Attribute
        ("Min_Packet_Transmission_Time",
         "Node_Id",
         Image (Min_Packet_Transmission_Time (N)),
         Int (Min_Packet_Transmission_Time (N)));
      W_Node_Attribute
        ("List_Of_Drivers",
         "List_Id",
         Image (List_Of_Drivers (N)),
         Int (List_Of_Drivers (N)));
   end W_Processing_Resource;

   procedure W_Operation (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Is_Simple",
         "Boolean",
         Image (Is_Simple (N)));
      W_Node_Attribute
        ("Is_Enclosing",
         "Boolean",
         Image (Is_Enclosing (N)));
      W_Node_Attribute
        ("Is_Message_Transmission",
         "Boolean",
         Image (Is_Message_Transmission (N)));
      W_Node_Attribute
        ("Is_Composite",
         "Boolean",
         Image (Is_Composite (N)));
      W_Node_Attribute
        ("Operations",
         "List_Id",
         Image (Operations (N)),
         Int (Operations (N)));
      W_Node_Attribute
        ("Shared_Resources_List",
         "List_Id",
         Image (Shared_Resources_List (N)),
         Int (Shared_Resources_List (N)));
      W_Node_Attribute
        ("Worst_Case_Execution_Time",
         "Node_Id",
         Image (Worst_Case_Execution_Time (N)),
         Int (Worst_Case_Execution_Time (N)));
      W_Node_Attribute
        ("Best_Case_Execution_Time",
         "Node_Id",
         Image (Best_Case_Execution_Time (N)),
         Int (Best_Case_Execution_Time (N)));
      W_Node_Attribute
        ("Avg_Case_Execution_Time",
         "Node_Id",
         Image (Avg_Case_Execution_Time (N)),
         Int (Avg_Case_Execution_Time (N)));
      W_Node_Attribute
        ("Max_Message_Size",
         "Node_Id",
         Image (Max_Message_Size (N)),
         Int (Max_Message_Size (N)));
      W_Node_Attribute
        ("Avg_Message_Size",
         "Node_Id",
         Image (Avg_Message_Size (N)),
         Int (Avg_Message_Size (N)));
      W_Node_Attribute
        ("Min_Message_Size",
         "Node_Id",
         Image (Min_Message_Size (N)),
         Int (Min_Message_Size (N)));
   end W_Operation;

   procedure W_Transaction (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Is_Regular",
         "Boolean",
         Image (Is_Regular (N)));
      W_Node_Attribute
        ("External_Events",
         "List_Id",
         Image (External_Events (N)),
         Int (External_Events (N)));
      W_Node_Attribute
        ("Internal_Events",
         "List_Id",
         Image (Internal_Events (N)),
         Int (Internal_Events (N)));
      W_Node_Attribute
        ("Event_Handlers",
         "List_Id",
         Image (Event_Handlers (N)),
         Int (Event_Handlers (N)));
   end W_Transaction;

   procedure W_Event (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Is_Regular",
         "Boolean",
         Image (Is_Regular (N)));
      W_Node_Attribute
        ("Is_Sporadic",
         "Boolean",
         Image (Is_Sporadic (N)));
      W_Node_Attribute
        ("Is_Periodic",
         "Boolean",
         Image (Is_Periodic (N)));
      W_Node_Attribute
        ("Timing_Requirements",
         "Node_Id",
         Image (Timing_Requirements (N)),
         Int (Timing_Requirements (N)));
      W_Node_Attribute
        ("Min_Interarrival",
         "Node_Id",
         Image (Min_Interarrival (N)),
         Int (Min_Interarrival (N)));
      W_Node_Attribute
        ("Period",
         "Node_Id",
         Image (Period (N)),
         Int (Period (N)));
   end W_Event;

   procedure W_Shared_Resource (N : Node_Id) is
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
        ("Node_Name",
         "Name_Id",
         Image (Node_Name (N)));
      W_Node_Attribute
        ("Node_Type",
         "Name_Id",
         Image (Node_Type (N)));
      W_Node_Attribute
        ("Is_Immediate_Ceiling_Resource",
         "Boolean",
         Image (Is_Immediate_Ceiling_Resource (N)));
   end W_Shared_Resource;

   procedure W_Event_Timing_Requirements (N : Node_Id) is
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
        ("Is_Hard_Deadline",
         "Boolean",
         Image (Is_Hard_Deadline (N)));
      W_Node_Attribute
        ("Deadline",
         "Node_Id",
         Image (Deadline (N)),
         Int (Deadline (N)));
      W_Node_Attribute
        ("Referenced_Event",
         "Name_Id",
         Image (Referenced_Event (N)));
   end W_Event_Timing_Requirements;

   procedure W_Event_Handler (N : Node_Id) is
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
        ("Is_Activity",
         "Boolean",
         Image (Is_Activity (N)));
      W_Node_Attribute
        ("Input_Name",
         "Name_Id",
         Image (Input_Name (N)));
      W_Node_Attribute
        ("Output_Name",
         "Name_Id",
         Image (Output_Name (N)));
      W_Node_Attribute
        ("Operation_Name",
         "Name_Id",
         Image (Operation_Name (N)));
      W_Node_Attribute
        ("Server_Name",
         "Name_Id",
         Image (Server_Name (N)));
   end W_Event_Handler;

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
   end W_Defining_Identifier;

   procedure W_MAST_File (N : Node_Id) is
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
        ("Distributed_Application_Unit",
         "Node_Id",
         Image (Distributed_Application_Unit (N)),
         Int (Distributed_Application_Unit (N)));
      W_Node_Attribute
        ("Declarations",
         "List_Id",
         Image (Declarations (N)),
         Int (Declarations (N)));
   end W_MAST_File;

   procedure W_Container (N : Node_Id) is
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
        ("Content",
         "Node_Id",
         Image (Content (N)),
         Int (Content (N)));
   end W_Container;

   procedure W_String (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_String;

   procedure W_Numeric (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Numeric;

   procedure W_Float (N : Base_Type) is
   begin
      W_Node_Header
        (Node_Id (N));
      W_Node_Attribute
        ("Image",
         "Name_Id",
         Image (Image (N)));
   end W_Float;

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
        ("Unit",
         "Node_Id",
         Image (Unit (N)),
         Int (Unit (N)));
      W_Node_Attribute
        ("Node",
         "Node_Id",
         Image (Node (N)),
         Int (Node (N)));
      W_Node_Attribute
        ("Processes",
         "List_Id",
         Image (Processes (N)),
         Int (Processes (N)));
   end W_HI_Tree_Bindings;

end Ocarina.Backends.MAST_Tree.Nodes;
