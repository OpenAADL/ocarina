------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . M A S T _ T R E E . N U T I L S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2010-2015 ESA & ISAE.                    --
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

with Ocarina.Backends.MAST_Tree.Nodes; use Ocarina.Backends.MAST_Tree.Nodes;

package Ocarina.Backends.MAST_Tree.Nutils is

   Int0_Val : Value_Id;
   Int1_Val : Value_Id;

   Var_Suffix  : constant String := "_mast";
   Initialized : Boolean         := False;

   Output_Tree_Warnings : Boolean := False;
   Output_Unit_Withing  : Boolean := False;
   --  Control flags

   type Token_Type is
     (
   --   Token name      Token type
   --   Keywords
   Tok_First_Keyword,
      Tok_Activity_Operation,
      Tok_Activity_Server,
      Tok_Avg_Case_Execution_Time,
      Tok_Avg_Context_Switch,
      Tok_Avg_Message_Size,
      Tok_Avg_ISR_Switch,
      Tok_Best_Case_Execution_time,
      Tok_Best_Context_Switch,
      Tok_Best_ISR_Switch,
      Tok_Character_Packet_Driver,
      Tok_Composite_Operation_List,
      Tok_Coupled,
      Tok_Deadline,
      Tok_Decoupled,
      Tok_Driver,
      Tok_Event_Handlers,
      Tok_External_Events,
      Tok_Full_Duplex,
      Tok_Fixed_Priority,
      Tok_Fixed_Priority_Policy,
      Tok_Half_Duplex,
      Tok_Hard_Global_Deadline,
      Tok_Host,
      Tok_Immediate_Ceiling_Resource,
      Tok_Internal_Events,
      Tok_Input_Event,
      Tok_List_Of_Drivers,
      Tok_Max_Blocking,
      Tok_Max_Message_Size,
      Tok_Max_Packet_Size,
      Tok_Max_Packet_Transmission_Time,
      Tok_Max_Priority,
      Tok_Max_ISR_Switch,
      Tok_Message_Partitioning,
      Tok_Min_Interarrival,
      Tok_Min_Message_Size,
      Tok_Min_Packet_Size,
      Tok_Min_Packet_Transmission_Time,
      Tok_Min_Priority,
      Tok_Name,
      Tok_Output_Event,
      Tok_Packet_Server,
      Tok_Packet_Driver,
      Tok_Packet_Send_Operation,
      Tok_Packet_Receive_Operation,
      Tok_Parameters,
      Tok_Period,
      Tok_Policy,
      Tok_Primary_Scheduler,
      Tok_Processing_Resource,
      Tok_RTA_Overhead_Model,
      Tok_Referenced_Event,
      Tok_Scheduler,
      Tok_Scheduling_Server,
      Tok_Server_Processing_Resource,
      Tok_Server_Sched_Parameters,
      Tok_Shared_Resources_List,
      Tok_Simplex,
      Tok_Speed_Factor,
      Tok_Shared_Resource,
      Tok_The_Priority,
      Tok_Throughput,
      Tok_Timing_Requirements,
      Tok_Transmission,
      Tok_Type,
      Tok_Unknown,
      Tok_Worst_Case_Execution_Time,
      Tok_Worst_Context_Switch,
      Tok_Worst_ISR_Switch,
      Tok_Last_Keyword,

      Tok_Assign,          -- =>
      Tok_Colon,
      Tok_Left_Paren,      -- (
      Tok_Right_Paren,     -- )
      Tok_Semicolon);      -- ;

   Token_Image : array (Token_Type) of Name_Id;

   subtype Keyword_Type is
     Token_Type range Tok_First_Keyword .. Tok_Last_Keyword;

   procedure Reset;

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id;

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id;
   --  This function returns a new name without the suffix. If the
   --  suffix does not exist, the returned name is equal to the given
   --  name.

   procedure Append_Node_To_List (E : Node_Id; L : List_Id);
   procedure Insert_After_Node (E : Node_Id; N : Node_Id);
   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id);

   procedure Push_Entity (E : Node_Id);
   procedure Pop_Entity;
   function Current_Entity return Node_Id;
   function Current_File return Node_Id;

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id;

   function Copy_Node (N : Node_Id) return Node_Id;

   function New_Node
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return Node_Id;

   function New_List
     (Kind : Node_Kind;
      From : Node_Id := No_Node) return List_Id;

   function Image (T : Token_Type) return String;

   procedure Initialize;

   procedure New_Token (T : Token_Type; I : String := "");

   function Length (L : List_Id) return Natural;

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id);
   --  Remove node N to list L.

   function Is_Empty (L : List_Id) return Boolean;
   pragma Inline (Is_Empty);
   --  Return True when L is empty

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node) return List_Id;

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id;
   --  This function executes Next_Node Num times

   function Conventional_Base_Name (N : Name_Id) return Name_Id;
   --  Return a lower case name of N

   function Make_MAST_File (Identifier : Node_Id) return Node_Id;

   function Make_Literal (Value : Value_Id) return Node_Id;

   function Make_Container (Content : Node_Id) return Node_Id;

   type Processing_Resource_Kind is
     (PR_Regular_Processor,
      PR_Fixed_Priority_Processor,
      PR_Packet_Based_Network);

   type Transmission_Kind is (Simplex, Half_Duplex, Full_Duplex);

   function Make_Processing_Resource
     (PR_Name : Name_Id;
      PR_Type : Processing_Resource_Kind) return Node_Id;

   type Scheduling_Server_Parameter_Kind is (Fixed_Priority, Unknown);

   function Make_Scheduling_Server_Parameters
     (Server_Kind : Scheduling_Server_Parameter_Kind;
      Prio        : Unsigned_Long_Long) return Node_Id;

   function Make_Scheduling_Server
     (Server_Name          : Name_Id;
      Associated_Processor : Name_Id) return Node_Id;

   type Event_Kind is (Periodic, Sporadic, Regular);

   function Make_Event (E_Name : Name_Id; E_Kind : Event_Kind) return Node_Id;

   type Event_Handler_Kind is (Activity);

   function Make_Event_Handler
     (Kind         : Event_Handler_Kind;
      Input_Event  : Name_Id;
      Output_Event : Name_Id;
      Operation    : Name_Id;
      Server       : Name_Id) return Node_Id;

   type Transaction_Kind is (Regular);

   function Make_Transaction
     (Trans_Name : Name_Id;
      Trans_Type : Transaction_Kind) return Node_Id;

   type Operation_Kind is (Enclosing, Simple, Composite, Message_Transmission);

   function Make_Operation
     (Op_Name : Name_Id;
      Op_Kind : Operation_Kind;
      Op_List : List_Id := No_List) return Node_Id;

   type Event_Timing_Requirement_Kind is (Hard_Deadline);

   function Make_Event_Timing_Requirement
     (Req_Kind  : Event_Timing_Requirement_Kind;
      Deadline  : Unsigned_Long_Long;
      Ref_Event : Name_Id) return Node_Id;

   type Shared_Resource_Kind is (Immediate_Ceiling, Unknown);

   function Make_Shared_Resource
     (Res_Kind : Shared_Resource_Kind;
      Res_Name : Name_Id) return Node_Id;

   type RTA_Overhead_Model_Kind is (Coupled, Decoupled);

   type Driver_Kind is (Character, Packet);

   function Make_Driver
     (Driver_Name       : Name_Id;
      Drv_Kind          : Driver_Kind;
      Server_Sched_Name : Name_Id;
      Send_Name         : Name_Id;
      Receive_Name      : Name_Id;
      Partitioning      : Boolean;
      Overhead_Kind     : RTA_Overhead_Model_Kind) return Node_Id;

   function Make_Scheduler
     (Sched_Name : Name_Id;
      Host_Name  : Name_Id) return Node_Id;
end Ocarina.Backends.MAST_Tree.Nutils;
