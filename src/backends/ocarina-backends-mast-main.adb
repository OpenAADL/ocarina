------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           O C A R I N A . B A C K E N D S . M A S T . M A I N            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, European Space Agency (ESA).              --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Namet; use Namet;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.MAST_Tree.Nodes;
with Ocarina.Backends.MAST_Tree.Nutils;
with Ocarina.Backends.MAST_Values;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;

package body Ocarina.Backends.MAST.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.MAST_Tree.Nutils;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.MAST_Values;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package MTN renames Ocarina.Backends.MAST_Tree.Nodes;
   package MTU renames Ocarina.Backends.MAST_Tree.Nutils;

   procedure Visit_Component (E : Node_Id);
   procedure Visit_System (E : Node_Id);
   procedure Visit_Processor (E : Node_Id);
   procedure Visit_Process (E : Node_Id);
   procedure Visit_Thread (E : Node_Id);
   procedure Visit_Subprogram (E : Node_Id);
   procedure Visit_Bus (E : Node_Id);
   procedure Visit_Virtual_Processor (E : Node_Id);

   Root_System_Node                 : Node_Id := No_Node;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      case Kind (E) is
         when K_Architecture_Instance =>
            Root_System_Node := Root_System (E);
            Visit (Root_System_Node);

         when K_Component_Instance =>
            Visit_Component (E);

         when others =>
            null;
      end case;
   end Visit;

   ---------------------
   -- Visit_Component --
   ---------------------

   procedure Visit_Component (E : Node_Id) is
      Category : constant Component_Category
        := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System (E);

         when CC_Processor =>
            Visit_Processor (E);

         when CC_Subprogram =>
            Visit_Subprogram (E);

         when CC_Process =>
            Visit_Process (E);

         when CC_Thread =>
            Visit_Thread (E);

         when CC_Bus =>
            Visit_Bus (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor (E);

         when others =>
            null;
      end case;
   end Visit_Component;

   ---------------
   -- Visit_Bus --
   ---------------

   procedure Visit_Bus (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Bus;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Virtual_Processor;

   ---------------------
   -- Visit_Processor --
   ---------------------

   procedure Visit_Processor (E : Node_Id) is
      S        : Node_Id;
      N        : Node_Id;
   begin
      N := MTU.Make_Processing_Resource
         (Normalize_Name (Name (Identifier (E))),
         PR_Fixed_Priority_Processor);

      MTU.Append_Node_To_List (N, MTN.Declarations (MAST_File));

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Processor;

   -------------------
   -- Visit_Process --
   -------------------

   procedure Visit_Process (E : Node_Id) is
      S        : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Process;

   ------------------
   -- Visit_Thread --
   ------------------

   procedure Visit_Thread (E : Node_Id) is
      S                       : Node_Id;
      N                       : Node_Id;
      Call                    : Node_Id;
      Spg_Call                : Node_Id;
      Spg                     : Node_Id;
      Activation_Event        : Node_Id;
      Activation_Kind         : Event_Kind := Regular;
      Activation_Event_Name   : Name_Id;
      Output_Event            : Node_Id;
      Output_Event_Name       : Name_Id;
      Event_Handler           : Node_Id;
      Server_Parameters       : Node_Id;
      Server_Sched_Name       : Name_Id;
      Operation_Name          : Name_Id;
      Operation               : Node_Id;
      Operations_List         : constant List_Id
                  := MTU.New_List (MTN.K_List_Id);
      Output_Event_Req        : Node_Id := No_Node;
      Prio                    : Unsigned_Long_Long;
   begin
      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Normalize_Name (Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_operations");
      Operation_Name := Name_Find;

      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Normalize_Name (Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_sched_server");
      Server_Sched_Name := Name_Find;

      Prio := Get_Thread_Priority (E);
      if Prio = 0 then
         Prio := 1;
      end if;
      Server_Parameters := Make_Scheduling_Server_Parameters
         (Fixed_Priority, Prio);

      N := Make_Scheduling_Server
         (Server_Sched_Name,
          Normalize_Name (Name (Identifier (Get_Bound_Processor
          (Parent_Component (Parent_Subcomponent (E)))))));

      MTN.Set_Parameters (N, Server_Parameters);

      Append_Node_To_List (N, MTN.Declarations (MAST_File));

      N := Make_Transaction
         (Normalize_Name (Name (Identifier (E))), Regular);
      Append_Node_To_List (N, MTN.Declarations (MAST_File));

      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Normalize_Name (Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_activation_event");
      Activation_Event_Name := Name_Find;

      if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic then
         Activation_Kind := Periodic;

         Output_Event_Req
            := Make_Event_Timing_Requirement
               (Hard_Deadline,
               To_Milliseconds (Get_Thread_Deadline (E)),
               Activation_Event_Name);
      elsif Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic then
         Activation_Kind := Sporadic;
      else
         Activation_Kind := Regular;
      end if;

      Activation_Event := Make_Event (Activation_Event_Name, Activation_Kind);

      if Get_Thread_Dispatch_Protocol (E) = Thread_Periodic then
         MTN.Set_Period
            (Activation_Event,
            Make_Literal
               (New_Numeric_Value
                  (To_Milliseconds (Get_Thread_Period (E)), 1, 10)));
      else
         MTN.Set_Period (Activation_Event, No_Node);
      end if;

      Append_Node_To_List
         (Activation_Event, MTN.External_Events (N));

      Set_Str_To_Name_Buffer ("");
      Get_Name_String (Normalize_Name (Name (Identifier (E))));
      Add_Str_To_Name_Buffer ("_output_event");
      Output_Event_Name := Name_Find;

      Output_Event := Make_Event (Output_Event_Name, Regular);

      MTN.Set_Timing_Requirements (Output_Event, Output_Event_Req);

      Append_Node_To_List
         (Output_Event, MTN.Internal_Events (N));

      Event_Handler := Make_Event_Handler
         (Activity,
         Activation_Event_Name,
         Output_Event_Name,
         Operation_Name,
         Server_Sched_Name);

      Append_Node_To_List
         (Event_Handler, MTN.Event_Handlers (N));

      Operation := Make_Operation (Operation_Name, Enclosing);
      MTN.Set_Operations (Operation, Operations_List);

      if Get_Execution_Time (E) /= Empty_Time_Array then
         declare
            ET : constant Time_Array := Get_Execution_Time (E);
         begin
            MTN.Set_Best_Case_Execution_Time
               (Operation,
               Make_Literal
                  (New_Numeric_Value
                     (To_Milliseconds (ET (0)), 1, 10)));
            MTN.Set_Worst_Case_Execution_Time
               (Operation,
               Make_Literal
                  (New_Numeric_Value
                     (To_Milliseconds (ET (1)), 1, 10)));
         end;
      end if;

      Append_Node_To_List (Operation, MTN.Declarations (MAST_File));

      if not AINU.Is_Empty (Calls (E)) then
         Call := AIN.First_Node (Calls (E));
         while Present (Call) loop

            if not AINU.Is_Empty (Subprogram_Calls (Call)) then
               Spg_Call := AIN.First_Node
                  (AIN.Subprogram_Calls (Call));
               while Present (Spg_Call) loop

                  Spg := AIN.Corresponding_Instance (Spg_Call);
                  Append_Node_To_List
                     (Make_Defining_Identifier
                        (Name (Identifier (Spg))),
                     Operations_List);
                  Visit (Spg);
                  Spg_Call := AIN.Next_Node (Spg_Call);
               end loop;
            end if;
            Call := AIN.Next_Node (Call);
         end loop;
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Thread;

   ----------------------
   -- Visit_Subprogram --
   ----------------------

   procedure Visit_Subprogram (E : Node_Id) is
      Operation               : Node_Id;
      Operation_Name          : Name_Id;
      Operations_List         : constant List_Id
                  := MTU.New_List (MTN.K_List_Id);
   begin
      Operation_Name := Name (Identifier (E));
      Operation := Make_Operation (Operation_Name, Simple);
      MTN.Set_Operations (Operation, Operations_List);

      Append_Node_To_List (Operation, MTN.Declarations (MAST_File));
   end Visit_Subprogram;

   ------------------
   -- Visit_System --
   ------------------

   procedure Visit_System (E : Node_Id) is
      S                    : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System;
end Ocarina.Backends.MAST.Main;
