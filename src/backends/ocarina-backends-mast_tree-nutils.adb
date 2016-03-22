------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . M A S T _ T R E E . N U T I L S     --
--                                                                          --
--                                 B o d y                                  --
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

with GNAT.Table;

with Charset;       use Charset;
with Locations;     use Locations;
with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.Backends.MAST_Values;
--  with Utils;     use Utils;

--  with Ocarina.Backends.Utils;
with Ocarina.ME_AADL.AADL_Tree.Nodes; use Ocarina.ME_AADL.AADL_Tree.Nodes;
use Ocarina.Backends.MAST_Values;
--  use Ocarina.Backends.Utils;

package body Ocarina.Backends.MAST_Tree.Nutils is

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
   package MTN renames Ocarina.Backends.MAST_Tree.Nodes;

   Keyword_Suffix : constant String := "%MAST";

   type Entity_Stack_Entry is record
      Current_File   : Node_Id;
      Current_Entity : Node_Id;
   end record;

   No_Depth : constant Int := -1;
   package Entity_Stack is new GNAT.Table
     (Entity_Stack_Entry,
      Int,
      No_Depth + 1,
      10,
      10);

   use Entity_Stack;

   ------------------------
   -- Add_Prefix_To_Name --
   ------------------------

   function Add_Prefix_To_Name
     (Prefix : String;
      Name   : Name_Id) return Name_Id
   is
   begin
      Set_Str_To_Name_Buffer (Prefix);
      Get_Name_String_And_Append (Name);
      return Name_Find;
   end Add_Prefix_To_Name;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Entity_Stack.Init;

      Initialized := False;
   end Reset;

   ------------------------
   -- Add_Suffix_To_Name --
   ------------------------

   function Add_Suffix_To_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id
   is
   begin
      Get_Name_String (Name);
      Add_Str_To_Name_Buffer (Suffix);
      return Name_Find;
   end Add_Suffix_To_Name;

   -----------------------------
   -- Remove_Suffix_From_Name --
   -----------------------------

   function Remove_Suffix_From_Name
     (Suffix : String;
      Name   : Name_Id) return Name_Id
   is
      Length   : Natural;
      Temp_Str : String (1 .. Suffix'Length);
   begin
      Set_Str_To_Name_Buffer (Suffix);
      Length := Name_Len;
      Get_Name_String (Name);
      if Name_Len > Length then
         Temp_Str := Name_Buffer (Name_Len - Length + 1 .. Name_Len);
         if Suffix = Temp_Str then
            Set_Str_To_Name_Buffer (Name_Buffer (1 .. Name_Len - Length));
            return Name_Find;
         end if;
      end if;
      return Name;
   end Remove_Suffix_From_Name;

   -------------------------
   -- Append_Node_To_List --
   -------------------------

   procedure Append_Node_To_List (E : Node_Id; L : List_Id) is
      Last : Node_Id;

   begin
      Last := MTN.Last_Node (L);
      if No (Last) then
         MTN.Set_First_Node (L, E);
      else
         MTN.Set_Next_Node (Last, E);
      end if;
      Last := E;
      while Present (Last) loop
         MTN.Set_Last_Node (L, Last);
         Last := MTN.Next_Node (Last);
      end loop;
   end Append_Node_To_List;

   -----------------------
   -- Insert_After_Node --
   -----------------------

   procedure Insert_After_Node (E : Node_Id; N : Node_Id) is
      Next : constant Node_Id := MTN.Next_Node (N);
   begin
      MTN.Set_Next_Node (N, E);
      MTN.Set_Next_Node (E, Next);
   end Insert_After_Node;

   ------------------------
   -- Insert_Before_Node --
   ------------------------

   procedure Insert_Before_Node (E : Node_Id; N : Node_Id; L : List_Id) is
      Entity : Node_Id;
   begin
      Entity := MTN.First_Node (L);
      if Entity = N then
         MTN.Set_Next_Node (E, Entity);
         MTN.Set_First_Node (L, E);
      else
         while Present (Entity) loop
            exit when MTN.Next_Node (Entity) = N;
            Entity := MTN.Next_Node (Entity);
         end loop;

         Insert_After_Node (E, Entity);
      end if;
   end Insert_Before_Node;

   ---------------
   -- Copy_Node --
   ---------------

   function Copy_Node (N : Node_Id) return Node_Id is
      C : Node_Id;
   begin
      case MTN.Kind (N) is
         when K_Literal =>
            C := New_Node (K_Literal);
            MTN.Set_Value (C, MTN.Value (N));

         when others =>
            raise Program_Error;
      end case;
      return C;
   end Copy_Node;

   -----------
   -- Image --
   -----------

   function Image (T : Token_Type) return String is
      S : String := Token_Type'Image (T);
   begin
      To_Lower (S);
      return S (5 .. S'Last);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize Nutils only once

      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Keywords.
      for I in Keyword_Type loop
         New_Token (I);
      end loop;

      --  Graphic Characters
      New_Token (Tok_Assign, "=>");
      New_Token (Tok_Left_Paren, "(");
      New_Token (Tok_Right_Paren, ")");
      New_Token (Tok_Semicolon, ";");
      New_Token (Tok_Colon, ",");

   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (L : List_Id) return Boolean is
   begin
      return L = No_List or else No (MTN.First_Node (L));
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (L : List_Id) return Natural is
      N : Node_Id;
      C : Natural := 0;
   begin
      if not Is_Empty (L) then
         N := MTN.First_Node (L);

         while Present (N) loop
            C := C + 1;
            N := MTN.Next_Node (N);
         end loop;
      end if;

      return C;
   end Length;

   ------------------------------
   -- Make_Defining_Identifier --
   ------------------------------

   function Make_Defining_Identifier (Name : Name_Id) return Node_Id is
      N : Node_Id;

   begin
      N := New_Node (K_Defining_Identifier);
      MTN.Set_Name (N, Name);
      return N;
   end Make_Defining_Identifier;

   ------------------
   -- Make_List_Id --
   ------------------

   function Make_List_Id
     (N1 : Node_Id;
      N2 : Node_Id := No_Node;
      N3 : Node_Id := No_Node) return List_Id
   is
      L : List_Id;
   begin
      L := New_List (MTN.K_List_Id);
      Append_Node_To_List (N1, L);
      if Present (N2) then
         Append_Node_To_List (N2, L);

         if Present (N3) then
            Append_Node_To_List (N3, L);
         end if;
      end if;
      return L;
   end Make_List_Id;

   -----------------
   -- Next_N_Node --
   -----------------

   function Next_N_Node (N : Node_Id; Num : Natural) return Node_Id is
      Result : Node_Id := N;
   begin
      for I in 1 .. Num loop
         Result := MTN.Next_Node (Result);
      end loop;

      return Result;
   end Next_N_Node;

   --------------
   -- New_List --
   --------------

   function New_List
     (Kind : MTN.Node_Kind;
      From : Node_Id := No_Node) return List_Id
   is
      N : Node_Id;

   begin
      MTN.Entries.Increment_Last;
      N                     := MTN.Entries.Last;
      MTN.Entries.Table (N) := MTN.Default_Node;
      Set_Kind (N, Kind);
      if Present (From) then
         MTN.Set_Loc (N, MTN.Loc (From));
      else
         MTN.Set_Loc (N, No_Location);
      end if;
      return List_Id (N);
   end New_List;

   --------------
   -- New_Node --
   --------------

   function New_Node
     (Kind : MTN.Node_Kind;
      From : Node_Id := No_Node) return Node_Id
   is
      N : Node_Id;
   begin
      MTN.Entries.Increment_Last;
      N                     := MTN.Entries.Last;
      MTN.Entries.Table (N) := MTN.Default_Node;
      MTN.Set_Kind (N, Kind);

      if Present (From) then
         MTN.Set_Loc (N, ATN.Loc (From));
      else
         MTN.Set_Loc (N, No_Location);
      end if;

      return N;
   end New_Node;

   ---------------
   -- New_Token --
   ---------------

   procedure New_Token (T : Token_Type; I : String := "") is
      Name : Name_Id;
   begin
      if T in Keyword_Type then
         --  Marking the token image as a keyword for fas searching
         --  purpose, we add the prefix to avoir collision with other
         --  languages keywords

         Set_Str_To_Name_Buffer (Image (T));
         Name := Name_Find;
         Name := Add_Suffix_To_Name (Keyword_Suffix, Name);
         Set_Name_Table_Byte
           (Name,
            Ocarina.Types.Byte (Token_Type'Pos (T) + 1));

         Set_Str_To_Name_Buffer (Image (T));
      else
         Set_Str_To_Name_Buffer (I);
      end if;
      Token_Image (T) := Name_Find;
   end New_Token;

   ----------------
   -- Pop_Entity --
   ----------------

   procedure Pop_Entity is
   begin
      if Last > No_Depth then
         Decrement_Last;
      end if;
   end Pop_Entity;

   -----------------
   -- Push_Entity --
   -----------------

   procedure Push_Entity (E : Node_Id) is
   begin
      Increment_Last;
      Table (Last).Current_Entity := E;
   end Push_Entity;

   ---------------------------
   -- Remove_Node_From_List --
   ---------------------------

   procedure Remove_Node_From_List (E : Node_Id; L : List_Id) is
      C : Node_Id;

   begin
      C := MTN.First_Node (L);
      if C = E then
         MTN.Set_First_Node (L, MTN.Next_Node (E));
         if MTN.Last_Node (L) = E then
            MTN.Set_Last_Node (L, No_Node);
         end if;
      else
         while Present (C) loop
            if MTN.Next_Node (C) = E then
               MTN.Set_Next_Node (C, MTN.Next_Node (E));
               if MTN.Last_Node (L) = E then
                  MTN.Set_Last_Node (L, C);
               end if;
               exit;
            end if;
            C := MTN.Next_Node (C);
         end loop;
      end if;
   end Remove_Node_From_List;

   ----------------------------
   -- Conventional_Base_Name --
   ----------------------------

   function Conventional_Base_Name (N : Name_Id) return Name_Id is
   begin
      Get_Name_String (N);

      for Index in 1 .. Name_Len loop
         Name_Buffer (Index) := To_Lower (Name_Buffer (Index));
      end loop;

      return Name_Find;
   end Conventional_Base_Name;

   --------------------
   -- Current_Entity --
   --------------------

   function Current_Entity return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_Entity;
      end if;
   end Current_Entity;

   ------------------
   -- Current_File --
   ------------------

   function Current_File return Node_Id is
   begin
      if Last = No_Depth then
         return No_Node;
      else
         return Table (Last).Current_File;
      end if;
   end Current_File;

   --------------------
   -- Make_MAST_File --
   --------------------

   function Make_MAST_File (Identifier : Node_Id) return Node_Id is
      File : Node_Id;
   begin
      File := New_Node (K_MAST_File);
      MTN.Set_Declarations (File, New_List (MTN.K_List_Id));
      Set_Defining_Identifier (File, Identifier);
      Set_Corresponding_Node (Identifier, File);

      return File;
   end Make_MAST_File;

   ------------------
   -- Make_Literal --
   ------------------

   function Make_Literal (Value : Value_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (K_Literal);
      MTN.Set_Value (N, Value);
      return N;
   end Make_Literal;

   --------------------
   -- Make_Container --
   --------------------

   function Make_Container (Content : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Container);
      MTN.Set_Content (N, Content);
      return N;
   end Make_Container;

   ------------------------------
   -- Make_Processing_Resource --
   ------------------------------

   function Make_Processing_Resource
     (PR_Name : Name_Id;
      PR_Type : Processing_Resource_Kind) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Processing_Resource);

      MTN.Set_Node_Name (N, PR_Name);

      MTN.Set_Regular_Processor (N, False);
      MTN.Set_Fixed_Priority_Processor (N, False);
      MTN.Set_Packet_Based_Network (N, False);

      if PR_Type = PR_Regular_Processor then
         MTN.Set_Regular_Processor (N, True);
      elsif PR_Type = PR_Fixed_Priority_Processor then
         MTN.Set_Fixed_Priority_Processor (N, True);
      else
         MTN.Set_Packet_Based_Network (N, True);
      end if;

      MTN.Set_Is_Full_Duplex (N, False);
      MTN.Set_Is_Half_Duplex (N, False);
      MTN.Set_Is_Simplex (N, False);
      MTN.Set_Throughput (N, Make_Literal (New_Floating_Point_Value (0.0)));
      MTN.Set_Max_Blocking (N, Make_Literal (New_Floating_Point_Value (0.0)));
      MTN.Set_Max_Packet_Size
        (N,
         Make_Literal (New_Floating_Point_Value (10.0)));
      MTN.Set_Min_Packet_Size
        (N,
         Make_Literal (New_Floating_Point_Value (1.0)));
      MTN.Set_Max_Packet_Transmission_Time
        (N,
         Make_Literal (New_Floating_Point_Value (10.0)));
      MTN.Set_Min_Packet_Transmission_Time
        (N,
         Make_Literal (New_Floating_Point_Value (0.1)));
      MTN.Set_List_Of_Drivers (N, New_List (MTN.K_List_Id));

      return N;
   end Make_Processing_Resource;

   ----------------------------
   -- Make_Scheduling_Server --
   ----------------------------

   function Make_Scheduling_Server
     (Server_Name          : Name_Id;
      Associated_Processor : Name_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Scheduling_Server);
      MTN.Set_Node_Name (N, Server_Name);
      MTN.Set_Server_Processing_Resource (N, Associated_Processor);
      MTN.Set_Is_Regular (N, True);
      MTN.Set_Associated_Scheduler (N, No_Name);
      MTN.Set_Parameters (N, No_Node);
      return N;
   end Make_Scheduling_Server;

   ---------------------------------------
   -- Make_Scheduling_Server_Parameters --
   ---------------------------------------

   function Make_Scheduling_Server_Parameters
     (Server_Kind : Scheduling_Server_Parameter_Kind;
      Prio        : Unsigned_Long_Long) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Scheduling_Server_Parameters);

      MTN.Set_Fixed_Priority (N, False);
      MTN.Set_Is_Preassigned (N, False);
      MTN.Set_Priority (N, Make_Literal (New_Numeric_Value (Prio, 1, 10)));

      if Server_Kind = Fixed_Priority then
         MTN.Set_Fixed_Priority (N, True);
      end if;
      return N;
   end Make_Scheduling_Server_Parameters;

   ----------------------
   -- Make_Transaction --
   ----------------------

   function Make_Transaction
     (Trans_Name : Name_Id;
      Trans_Type : Transaction_Kind) return Node_Id
   is
      pragma Unreferenced (Trans_Type);

      N : Node_Id;
   begin
      N := New_Node (MTN.K_Transaction);
      MTN.Set_Node_Name (N, Trans_Name);
      MTN.Set_Is_Regular (N, True);
      MTN.Set_External_Events (N, New_List (MTN.K_List_Id));
      MTN.Set_Internal_Events (N, New_List (MTN.K_List_Id));
      MTN.Set_Event_Handlers (N, New_List (MTN.K_List_Id));
      return N;
   end Make_Transaction;

   ----------------
   -- Make_Event --
   ----------------

   function Make_Event
     (E_Name : Name_Id;
      E_Kind : Event_Kind) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Event);
      MTN.Set_Node_Name (N, E_Name);

      MTN.Set_Timing_Requirements (N, No_Node);

      MTN.Set_Is_Regular (N, False);
      MTN.Set_Is_Periodic (N, False);
      MTN.Set_Is_Sporadic (N, False);

      if E_Kind = Regular then
         MTN.Set_Is_Regular (N, True);
      elsif E_Kind = Sporadic then
         MTN.Set_Is_Sporadic (N, True);
      else
         MTN.Set_Is_Periodic (N, True);
      end if;
      return N;
   end Make_Event;

   ------------------------
   -- Make_Event_Handler --
   ------------------------

   function Make_Event_Handler
     (Kind         : Event_Handler_Kind;
      Input_Event  : Name_Id;
      Output_Event : Name_Id;
      Operation    : Name_Id;
      Server       : Name_Id) return Node_Id
   is
      pragma Unreferenced (Kind);

      N : Node_Id;
   begin
      N := New_Node (MTN.K_Event_Handler);
      MTN.Set_Is_Activity (N, True);
      MTN.Set_Input_Name (N, Input_Event);
      MTN.Set_Output_Name (N, Output_Event);
      MTN.Set_Operation_Name (N, Operation);
      MTN.Set_Server_Name (N, Server);

      return N;
   end Make_Event_Handler;

   --------------------
   -- Make_Operation --
   --------------------

   function Make_Operation
     (Op_Name : Name_Id;
      Op_Kind : Operation_Kind;
      Op_List : List_Id := No_List) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Operation);

      MTN.Set_Node_Name (N, Op_Name);

      if Op_List /= No_List then
         MTN.Set_Operations (N, Op_List);
      end if;

      MTN.Set_Shared_Resources_List (N, No_List);

      MTN.Set_Is_Enclosing (N, False);
      MTN.Set_Is_Composite (N, False);
      MTN.Set_Is_Simple (N, False);
      MTN.Set_Is_Message_Transmission (N, False);

      if Op_Kind = Enclosing then
         MTN.Set_Is_Enclosing (N, True);
      elsif Op_Kind = Composite then
         MTN.Set_Is_Composite (N, True);
      elsif Op_Kind = Message_Transmission then
         MTN.Set_Is_Message_Transmission (N, True);
      else
         MTN.Set_Is_Simple (N, True);
      end if;

      MTN.Set_Max_Message_Size (N, No_Node);
      MTN.Set_Avg_Message_Size (N, No_Node);
      MTN.Set_Min_Message_Size (N, No_Node);
      MTN.Set_Worst_Case_Execution_Time (N, No_Node);
      MTN.Set_Best_Case_Execution_Time (N, No_Node);
      MTN.Set_Avg_Case_Execution_Time (N, No_Node);
      return N;
   end Make_Operation;

   -----------------------------------
   -- Make_Event_Timing_Requirement --
   -----------------------------------

   function Make_Event_Timing_Requirement
     (Req_Kind  : Event_Timing_Requirement_Kind;
      Deadline  : Unsigned_Long_Long;
      Ref_Event : Name_Id) return Node_Id
   is
      pragma Unreferenced (Req_Kind);

      N : Node_Id;
   begin
      N := New_Node (MTN.K_Event_Timing_Requirements);
      MTN.Set_Is_Hard_Deadline (N, True);
      MTN.Set_Deadline (N, Make_Literal (New_Numeric_Value (Deadline, 1, 10)));

      MTN.Set_Referenced_Event (N, Ref_Event);
      return N;
   end Make_Event_Timing_Requirement;

   --------------------------
   -- Make_Shared_Resource --
   --------------------------

   function Make_Shared_Resource
     (Res_Kind : Shared_Resource_Kind;
      Res_Name : Name_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Shared_Resource);
      MTN.Set_Node_Name (N, Res_Name);
      MTN.Set_Is_Immediate_Ceiling_Resource (N, False);

      if Res_Kind = Immediate_Ceiling then
         MTN.Set_Is_Immediate_Ceiling_Resource (N, True);
      end if;

      return N;
   end Make_Shared_Resource;

   -----------------
   -- Make_Driver --
   -----------------

   function Make_Driver
     (Driver_Name       : Name_Id;
      Drv_Kind          : Driver_Kind;
      Server_Sched_Name : Name_Id;
      Send_Name         : Name_Id;
      Receive_Name      : Name_Id;
      Partitioning      : Boolean;
      Overhead_Kind     : RTA_Overhead_Model_Kind) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Driver);

      MTN.Set_Is_Packet_Driver (N, False);
      if Drv_Kind = Packet then
         MTN.Set_Is_Packet_Driver (N, True);
      end if;

      MTN.Set_Node_Name (N, Driver_Name);
      MTN.Set_Scheduling_Server (N, Server_Sched_Name);
      MTN.Set_Send_Operation_Name (N, Send_Name);
      MTN.Set_Receive_Operation_Name (N, Receive_Name);
      MTN.Set_Message_Partitioning (N, Partitioning);
      MTN.Set_Is_RTA_Overhead_Model_Coupled (N, False);

      if Overhead_Kind = Coupled then
         MTN.Set_Is_RTA_Overhead_Model_Coupled (N, True);
      end if;

      return N;
   end Make_Driver;

   --------------------
   -- Make_Scheduler --
   --------------------

   function Make_Scheduler
     (Sched_Name : Name_Id;
      Host_Name  : Name_Id) return Node_Id
   is
      N : Node_Id;
   begin
      N := New_Node (MTN.K_Scheduler);
      MTN.Set_Node_Name (N, Sched_Name);
      MTN.Set_Host (N, Host_Name);
      return N;
   end Make_Scheduler;

end Ocarina.Backends.MAST_Tree.Nutils;
