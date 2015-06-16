------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.PO_HI_RTSJ.ACTIVITY                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--       Copyright (C) 2009 Telecom ParisTech, 2010-2015 ESA & ISAE.        --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.PO_HI_RTSJ.Mapping;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;
with Ocarina.Backends.RTSJ_Values;

package body Ocarina.Backends.PO_HI_RTSJ.Activity is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.PO_HI_RTSJ.Mapping;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;
   use Ocarina.Backends.RTSJ_Values;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;

   --  Global variables
   Init_Declarations   : List_Id;
   Init_Statements     : List_Id;
   Tasks_Classes       : List_Id;
   Main_Class_Methods  : List_Id;
   Class_Attributes    : List_Id;
   Task_Job_Statements : List_Id;

   Offset : Unsigned_Long_Long;

   -----------------
   -- Source_File --
   -----------------
   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Make_Handler (E : Node_Id);
      procedure Make_Task (E : Node_Id);
      procedure Make_In_Port (E : Node_Id);
      procedure Make_Out_Port (E : Node_Id);

      ------------------
      -- Make_Handler --
      ------------------
      procedure Make_Handler (E : Node_Id) is
         Dispatch_Protocol : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         Class_Name       : Node_Id;
         Implement        : Node_Id;
         Init             : Name_Id;
         Recover          : Name_Id;
         Spec             : Node_Id;
         Impl             : Node_Id;
         N                : Node_Id;
         S                : constant Node_Id := Parent_Subcomponent (E);
         Init_Statements  : constant List_Id := New_List (K_Statement_List);
         Recov_Statements : constant List_Id := New_List (K_Statement_List);
         Job_Statements   : constant List_Id := New_List (K_Statement_List);
         Class_Methods    : constant List_Id := New_List (K_Method_List);
      begin

         case Dispatch_Protocol is
            when Thread_Periodic =>
               --  Create a class name for Task Handler
               Class_Name :=
                 Map_Handler_Class_Identifier (S, TaskHandler => True);

               --  Implement the right handler
               Implement := Make_Defining_Identifier (ON (O_Task_Handler));

               --  Statements of the job method
               N :=
                 Make_Call_Function
                   (Defining_Identifier => Map_Task_Job_Identifier (S));
               RTU.Append_Node_To_List (N, Job_Statements);

               --  Create the job method
               Spec :=
                 Make_Function_Specification
                   (Visibility          => Make_List_Id (RE (RE_Public)),
                    Return_Type         => New_Node (K_Void),
                    Defining_Identifier => RE (RE_Job),
                    Throws              =>
                      Make_List_Id
                        (Make_Defining_Identifier (ON (O_Program_Exception))));
               Impl :=
                 Make_Function_Implementation
                   (Specification => Spec,
                    Statements    => Job_Statements);
               RTU.Append_Node_To_List (Impl, Class_Methods);

            when Thread_Sporadic | Thread_Hybrid =>
               --  Create a class name for Event Handler
               Class_Name :=
                 Map_Handler_Class_Identifier (S, EventHandler => True);

               --  Implement the right handler
               Implement := Make_Defining_Identifier (ON (O_Event_Handler));

               --  Create the handleEvent method
               N :=
                 Make_Parameter_Specification
                   (Parameter_Type =>
                      Make_Defining_Identifier (ON (O_In_Port)),
                    Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_In_Port)));

               Spec :=
                 Make_Function_Specification
                   (Visibility          => Make_List_Id (RE (RE_Public)),
                    Return_Type         => New_Node (K_Void),
                    Defining_Identifier => RE (RE_Handle_Event),
                    Parameters          => Make_List_Id (N),
                    Throws              =>
                      Make_List_Id
                        (Make_Defining_Identifier (ON (O_Program_Exception))));

               N :=
                 Make_Call_Function
                   (Defining_Identifier => Map_Task_Job_Identifier (S));

               Impl :=
                 Make_Function_Implementation
                   (Specification => Spec,
                    Statements    => Make_List_Id (N));
               RTU.Append_Node_To_List (Impl, Class_Methods);

               --  Create the waitForEvents method
               N :=
                 Make_Parameter_Specification
                   (Parameter_Type      => New_Node (K_Int),
                    Defining_Identifier =>
                      Make_Defining_Identifier (VN (V_Entity)));

               Spec :=
                 Make_Function_Specification
                   (Visibility          => Make_List_Id (RE (RE_Public)),
                    Return_Type => Make_Defining_Identifier (ON (O_In_Port)),
                    Defining_Identifier => RE (RE_Wait_For_Events),
                    Parameters          => Make_List_Id (N),
                    Throws              =>
                      Make_List_Id
                        (Make_Defining_Identifier (ON (O_Program_Exception))));

               N :=
                 Make_Return_Statement
                   (Make_Call_Function
                      (Defining_Identifier => RE (RE_Wait_For_Incoming_Events),
                       Parameters          =>
                         Make_List_Id
                           (Make_Defining_Identifier (VN (V_Entity)))));

               Impl :=
                 Make_Function_Implementation
                   (Specification => Spec,
                    Statements    => Make_List_Id (N));
               RTU.Append_Node_To_List (Impl, Class_Methods);

            when others =>
               null;

         end case;

         --  Add user's method for initializing entrypoint
         Init := Get_Thread_Initialize_Entrypoint (E);
         if Init /= No_Name then
            RTU.Append_Node_To_List
              (Make_Defining_Identifier (Init),
               Init_Statements);
         end if;

         --  InitializeEntrypoint function
         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier => RE (RE_Initialize_Entrypoint),
              Return_Type         => New_Node (K_Void));
         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Init_Statements);
         RTU.Append_Node_To_List (Impl, Class_Methods);

         --  Add user's method for recovering entrypoint in case
         --  of errors
         Recover := Get_Thread_Recover_Entrypoint (E);
         if Recover /= No_Name then
            RTU.Append_Node_To_List
              (Make_Defining_Identifier (Recover),
               Recov_Statements);
         end if;

         --  RecoverEntrypoint function
         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier => RE (RE_Recover_Entrypoint),
              Return_Type         => New_Node (K_Void));
         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Recov_Statements);
         RTU.Append_Node_To_List (Impl, Class_Methods);

         --  Create a class which implements the right handler
         N :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Static)),
              Defining_Identifier => Class_Name,
              Implements          => Make_List_Id (Implement),
              Methods             => Class_Methods);
         RTU.Append_Node_To_List (N, Tasks_Classes);

      end Make_Handler;

      ---------------
      -- Make_Task --
      ---------------
      procedure Make_Task (E : Node_Id) is
         Dispatch_Protocol : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         N          : Node_Id;
         S          : constant Node_Id   := Parent_Subcomponent (E);
         Task_Type  : Node_Id;
         Priority   : Unsigned_Long_Long := 0;
         Stack_Size : Unsigned_Long_Long := 0;
         TT         : Time_Type;
         Params     : constant List_Id   := New_List (K_Parameter_List);
      begin

         --  Identifier of the task
         N :=
           Make_Pointed_Notation
             (Make_Defining_Identifier (ON (O_Deployment)),
              Make_Defining_Identifier (Map_RTSJ_Enumerator_Name (S)));
         RTU.Append_Node_To_List (N, Params);

         --  Period of the thread
         case Dispatch_Protocol is
            when Thread_Periodic | Thread_Sporadic | Thread_Hybrid =>

               TT := Get_Thread_Period (E);
               N  := Map_Time_Value (TT);
               RTU.Append_Node_To_List (N, Params);
               N :=
                 Make_Pointed_Notation
                   (Make_Defining_Identifier (ON (O_Time_Unit)),
                    Map_Time_Unit (TT));
               RTU.Append_Node_To_List (N, Params);

               --  Define task type and its declaration
               if Dispatch_Protocol = Thread_Periodic then
                  N :=
                    Make_Variable_Declaration
                      (Used_Type =>
                         Map_Handler_Class_Identifier (S, TaskHandler => True),
                       Defining_Identifier =>
                         Map_Handler_Identifier (S, TaskHandler => True),
                       Value =>
                         Make_New_Statement
                           (Defining_Identifier =>
                              Map_Handler_Class_Identifier
                                (S,
                                 TaskHandler => True),
                            Is_Array => False));
                  RTU.Append_Node_To_List (N, Init_Declarations);

                  Task_Type := Make_Defining_Identifier (ON (O_Periodic_Task));
                  Add_Import (RH (RH_Periodic_Task));
               elsif Dispatch_Protocol = Thread_Sporadic then
                  N :=
                    Make_Variable_Declaration
                      (Used_Type =>
                         Map_Handler_Class_Identifier
                           (S,
                            EventHandler => True),
                       Defining_Identifier =>
                         Map_Handler_Identifier (S, EventHandler => True),
                       Value =>
                         Make_New_Statement
                           (Map_Handler_Class_Identifier
                              (S,
                               EventHandler => True),
                            Is_Array => False));
                  RTU.Append_Node_To_List (N, Init_Declarations);

                  Task_Type := Make_Defining_Identifier (ON (O_Sporadic_Task));
                  Add_Import (RH (RH_Sporadic_Task));
               elsif Dispatch_Protocol = Thread_Hybrid then
                  N :=
                    Make_Variable_Declaration
                      (Used_Type =>
                         Map_Handler_Class_Identifier
                           (S,
                            EventHandler => True),
                       Defining_Identifier =>
                         Map_Handler_Identifier (S, EventHandler => True),
                       Value =>
                         Make_New_Statement
                           (Map_Handler_Class_Identifier
                              (S,
                               EventHandler => True),
                            Is_Array => False));
                  RTU.Append_Node_To_List (N, Init_Declarations);

                  Task_Type := Make_Defining_Identifier (ON (O_Hybrid_Task));
                  Add_Import (RH (RH_Hybrid_Task));
               end if;

            when others =>
               Display_Located_Error
                 (AIN.Loc (E),
                  "Thread kind is not supported",
                  Fatal => True);
         end case;

         --  Task declaration
         N :=
           Make_Variable_Declaration
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Used_Type           => Task_Type,
              Defining_Identifier => Map_RTSJ_Defining_Identifier (S));
         RTU.Append_Node_To_List (N, Class_Attributes);

         --  Deadline
         TT := Get_Thread_Deadline (E);
         N  := Map_Time_Value (TT);
         RTU.Append_Node_To_List (N, Params);
         N :=
           Make_Pointed_Notation
             (Make_Defining_Identifier (ON (O_Time_Unit)),
              Map_Time_Unit (TT));
         RTU.Append_Node_To_List (N, Params);

         --  Priority
         --  If the task has no priority, we use the DEFAULT_PRIORITY
         Priority := Get_Thread_Priority (E);
         N        :=
           Make_Variable_Declaration
             (Used_Type           => New_Node (K_Int),
              Defining_Identifier => Map_Priority_Identifier (S),
              Value               =>
                Make_Pointed_Notation
                  (Make_Defining_Identifier (ON (O_Utils)),
                   Make_Call_Function
                     (RE (RE_Compute_System_Priority),
                      Make_List_Id
                        (Make_Literal (New_Int_Value (Priority, 0, 10)),
                         Make_Literal (New_Int_Value (0, 0, 10)),
                         Make_Literal (New_Int_Value (255, 0, 10))))));
         RTU.Append_Node_To_List (N, Init_Declarations);

         RTU.Append_Node_To_List (Map_Priority_Identifier (S), Params);

         --  StackSize
         --  Default value if there is no property StackSize
         --  in the model ?
         Stack_Size := To_Bytes (Get_Thread_Stack_Size (E));
         N          :=
           Make_Expression
             (Make_Literal (New_Int_Value (Stack_Size, 0, 10)),
              Op_Mult,
              Make_Literal (New_Int_Value (1024, 0, 10)));
         RTU.Append_Node_To_List (N, Params);

         case Dispatch_Protocol is
            when Thread_Periodic =>
               RTU.Append_Node_To_List
                 (Map_Handler_Identifier (S, TaskHandler => True),
                  Params);
            when Thread_Sporadic | Thread_Hybrid =>
               RTU.Append_Node_To_List
                 (Map_Handler_Identifier (S, EventHandler => True),
                  Params);
            when others =>
               null;
         end case;

         --  Task assignment
         N :=
           Make_Assignment_Statement
             (Defining_Identifier => Map_RTSJ_Defining_Identifier (S),
              Expression          =>
                Make_New_Statement
                  (Defining_Identifier => Task_Type,
                   Parameters          => Params,
                   Is_Array            => False));
         RTU.Append_Node_To_List (N, Init_Declarations);

         --  Start the task
         N :=
           Make_Pointed_Notation
             (Map_RTSJ_Defining_Identifier (S),
              Make_Call_Function
                (Defining_Identifier =>
                   Make_Defining_Identifier (MN (M_Start))));
         RTU.Append_Node_To_List (N, Init_Statements);

      end Make_Task;

      ------------------
      -- Make_In_Port --
      ------------------
      procedure Make_In_Port (E : Node_Id) is
         P          : Node_Id;
         N          : Node_Id;
         Port_Type  : Node_Id;
         Queue_Size : Long_Long;
         Params     : constant List_Id := New_List (K_Parameter_List);
      begin

         N :=
           Make_Pointed_Notation
             (Make_Defining_Identifier (ON (O_Deployment)),
              Make_Defining_Identifier (Map_RTSJ_Enumerator_Name (E)));
         RTU.Append_Node_To_List (N, Params);

         Port_Type := Make_Defining_Identifier (ON (O_In_Port));

         if Is_Event (E) and then AIN.Is_Data (E) then
            P := RE (RE_In_Event_Data_Port);
         elsif AIN.Is_Data (E) and then not Is_Event (E) then
            P := RE (RE_In_Data_Port);
         elsif Is_Event (E) and then not AIN.Is_Data (E) then
            P := RE (RE_In_Event_Port);
         end if;

         --  defaultEntry parameter
         N := Map_Port_Default_Entry (E);
         RTU.Append_Node_To_List (N, Params);

         --  Kind of the Input port
         N :=
           Make_Pointed_Notation (Make_Defining_Identifier (ON (O_Port)), P);
         RTU.Append_Node_To_List (N, Params);

         --  fifoSize parameter
         Queue_Size := Get_Queue_Size (E);
         if Queue_Size = -1 then
            Queue_Size := Default_Queue_Size;
         elsif Queue_Size = 0 then
            Display_Located_Error
              (AIN.Loc (E),
               "Zero length  port queues are not supported",
               Fatal => True);
         end if;
         N :=
           Make_Literal
             (New_Int_Value (Unsigned_Long_Long (Queue_Size), 0, 10));
         RTU.Append_Node_To_List (N, Params);

         --  offset parameter
         N := Make_Literal (New_Int_Value (Offset, 0, 10));
         RTU.Append_Node_To_List (N, Params);

         Offset := Offset + Unsigned_Long_Long (Queue_Size);

         N :=
           Make_Variable_Declaration
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Used_Type           => Port_Type,
              Defining_Identifier => Map_Task_Port_Identifier (E));
         RTU.Append_Node_To_List (N, Class_Attributes);

         N :=
           Make_Assignment_Statement
             (Defining_Identifier => Map_Task_Port_Identifier (E),
              Expression          =>
                Make_New_Statement
                  (Defining_Identifier => Port_Type,
                   Parameters          => Params));
         RTU.Append_Node_To_List (N, Init_Declarations);

      end Make_In_Port;

      -------------------
      -- Make_Out_Port --
      -------------------
      procedure Make_Out_Port (E : Node_Id) is
         P                : Node_Id;
         N                : Node_Id;
         D                : Node_Id;
         Port_Type        : Node_Id;
         Nb_Dest          : Unsigned_Long_Long := 0;
         Destinations     : List_Id;
         Params           : constant List_Id   := New_List (K_Parameter_List);
         Destination_List : constant List_Id   :=
           New_List (K_Enumeration_Literals);
      begin

         N :=
           Make_Pointed_Notation
             (Make_Defining_Identifier (ON (O_Deployment)),
              Make_Defining_Identifier (Map_RTSJ_Enumerator_Name (E)));
         RTU.Append_Node_To_List (N, Params);

         Port_Type := Make_Defining_Identifier (ON (O_Out_Port));
         if Is_Event (E) and then AIN.Is_Data (E) then
            P := RE (RE_Out_Event_Data_Port);
         elsif AIN.Is_Data (E) and then not Is_Event (E) then
            P := RE (RE_Out_Data_Port);
         elsif Is_Event (E) and then not AIN.Is_Data (E) then
            P := RE (RE_Out_Event_Port);
         end if;

         --  Kind of the Output port
         N :=
           Make_Pointed_Notation (Make_Defining_Identifier (ON (O_Port)), P);
         RTU.Append_Node_To_List (N, Params);

         --  destinationsTab
         N := Map_Port_Destinations_Tab (E);
         RTU.Append_Node_To_List (N, Params);

         Destinations := Get_Destination_Ports (E);
         if AINU.Is_Empty (Destinations) then
            Display_Located_Error
              (AIN.Loc (E),
               "This Output port is not connected to " & "any destination",
               Fatal => True);
         end if;

         D       := AIN.First_Node (Destinations);
         Nb_Dest := 0;
         while Present (D) loop
            N :=
              Make_Assignment_Statement
                (Make_Array_Value
                   (Map_Port_Destinations_Tab (E),
                    Make_Literal (New_Int_Value (Nb_Dest, 0, 10))),
                 Make_Pointed_Notation
                   (Make_Defining_Identifier (ON (O_Deployment)),
                    Make_Defining_Identifier
                      (Map_RTSJ_Enumerator_Name (Item (D)))));
            RTU.Append_Node_To_List (N, Destination_List);

            Nb_Dest := Nb_Dest + 1;
            D       := AIN.Next_Node (D);
         end loop;

         N :=
           Make_Variable_Declaration
             (Used_Type           => New_Node (K_Int),
              Defining_Identifier =>
                Make_Array_Declaration (Map_Port_Destinations_Tab (E)),
              Value =>
                Make_New_Statement
                  (New_Node (K_Int),
                   Make_List_Id
                     (Make_Literal (New_Int_Value (Nb_Dest, 0, 10))),
                   Is_Array => True));

         N := Make_Full_Array_Declaration (N, Destination_List);
         RTU.Append_Node_To_List (N, Init_Declarations);

         N :=
           Make_Variable_Declaration
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Used_Type           => Port_Type,
              Defining_Identifier => Map_Task_Port_Identifier (E));
         RTU.Append_Node_To_List (N, Class_Attributes);

         N :=
           Make_Assignment_Statement
             (Defining_Identifier => Map_Task_Port_Identifier (E),
              Expression          =>
                Make_New_Statement
                  (Defining_Identifier => Port_Type,
                   Parameters          => Params));
         RTU.Append_Node_To_List (N, Init_Declarations);

      end Make_Out_Port;

      -----------
      -- Visit --
      -----------
      procedure Visit (E : Node_Id) is
      begin
         case AIN.Kind (E) is
            when K_Architecture_Instance =>
               Visit_Architecture_Instance (E);

            when K_Component_Instance =>
               Visit_Component_Instance (E);

            when others =>
               null;

         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------
      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------
      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
      begin
         case Category is
            when CC_System =>
               Visit_System_Instance (E);

            when CC_Process =>
               Visit_Process_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      --  Visit_System_Instance --
      ----------------------------
      procedure Visit_System_Instance (E : Node_Id) is
         P : Node_Id;
      begin
         Push_Entity (RTSJ_Root);

         --  Visit all the subcomponents of the system
         if not AINU.Is_Empty (Subcomponents (E)) then
            P := AIN.First_Node (Subcomponents (E));
            while Present (P) loop
               Visit (Corresponding_Instance (P));
               P := AIN.Next_Node (P);
            end loop;
         end if;

         Pop_Entity;
      end Visit_System_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------
      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           RTN.Distributed_Application_Unit
             (RTN.Naming_Node (Backend_Node (Identifier (E))));
         P          : constant Node_Id := RTN.Entity (U);
         N          : Node_Id;
         S          : Node_Id;
         Spec       : Node_Id;
         Impl       : Node_Id;
         Main_Class : Node_Id;
      begin
         Push_Entity (U);
         Push_Entity (P);
         RTU.Set_Activity_Source (U);

         --  Gloabal variables initialization
         Init_Declarations  := New_List (K_Declaration_List);
         Init_Statements    := New_List (K_Statement_List);
         Tasks_Classes      := New_List (K_Class_List);
         Main_Class_Methods := New_List (K_Method_List);
         Class_Attributes   := New_List (K_Attribute_List);
         Offset             := 0;

         Start_Recording_Handlings;

         --  Runtime and libraries imports
         Add_Import (RH (RH_Program_Exception));
         Add_Import (RH (RH_Suspenders));
         Add_Import (RH (RH_Deployment));

         --  Deployment initialization
         N :=
           Make_Pointed_Notation
             (Left_Member  => Make_Defining_Identifier (ON (O_Deployment)),
              Right_Member =>
                Make_Call_Function
                  (Defining_Identifier =>
                     Make_Defining_Identifier (MN (M_Initialization))));
         RTU.Append_Node_To_List (N, Init_Declarations);

         --  Suspenders initialization
         N :=
           Make_Pointed_Notation
             (Left_Member  => Make_Defining_Identifier (ON (O_Suspenders)),
              Right_Member =>
                Make_Call_Function
                  (Defining_Identifier =>
                     Make_Defining_Identifier (MN (M_Initialization))));
         RTU.Append_Node_To_List (N, Init_Declarations);

         --  Initialization function
         Spec :=
           Make_Function_Specification
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Defining_Identifier =>
                Make_Defining_Identifier (MN (M_Initialization)),
              Return_Type => New_Node (K_Void));

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Declarations  => Init_Declarations,
              Statements    => Init_Statements);
         RTU.Append_Node_To_List (Impl, Main_Class_Methods);

         --  Visit all the subcomponents of the process
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));
            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;

         --  Activity class
         Main_Class :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Activity)),
              Attributes => Class_Attributes,
              Methods    => Main_Class_Methods,
              Classes    => Tasks_Classes);
         RTU.Append_Node_To_List (Main_Class, RTN.Statements (Current_File));

         Reset_Handlings;

         Pop_Entity;  --  P
         Pop_Entity;  --  U
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------
      procedure Visit_Thread_Instance (E : Node_Id) is
         N                    : Node_Id;
         S                    : constant Node_Id := Parent_Subcomponent (E);
         F                    : Node_Id;
         Call_Seq             : Node_Id;
         Spg_Call             : Node_Id;
         Spec                 : Node_Id;
         Impl                 : Node_Id;
         Nb_In_Ports_Fifo     : Unsigned_Long_Long          := 0;
         Nb_In_Ports_Non_Fifo : constant Unsigned_Long_Long := 0;
         Params : constant List_Id            := New_List (K_Parameter_List);
      begin
         Task_Job_Statements := New_List (K_Statement_List);

         --  Class declaration of the handler associated to the task
         Make_Handler (E);

         --  Create a new task with all the parameters like
         --  period, priority, deadline, ... and start the task
         Make_Task (E);

         if Has_Ports (E) then

            --  portsRouter declaration
            N :=
              Make_Variable_Declaration
                (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
                 Used_Type => Make_Defining_Identifier (ON (O_Ports_Router)),
                 Defining_Identifier => Map_Task_Ports_Router_Identifier (S));
            RTU.Append_Node_To_List (N, Class_Attributes);

            --  In/Out ports declaration
            F := AIN.First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_In (F) then
                     --  defaultValue
                     N :=
                       Make_Variable_Declaration
                         (Used_Type =>
                            Make_Pointed_Notation
                              (Make_Defining_Identifier
                                 (ON (O_Generated_Types)),
                               Map_RTSJ_Defining_Identifier
                                 (Corresponding_Instance (F),
                                  True)),
                          Defining_Identifier => Map_Port_Default_Value (F),
                          Value               =>
                            Make_New_Statement
                              (Defining_Identifier =>
                                 Make_Pointed_Notation
                                   (Make_Defining_Identifier
                                      (ON (O_Generated_Types)),
                                    Map_RTSJ_Defining_Identifier
                                      (Corresponding_Instance (F),
                                       True)),
                               Parameters =>
                                 Make_List_Id
                                   (Make_Literal (New_Int_Value (0, 0, 10)))));
                     RTU.Append_Node_To_List (N, Init_Declarations);

                     --  defaultEntry declaration
                     N :=
                       Make_Variable_Declaration
                         (Used_Type => Make_Defining_Identifier (ON (O_Entry)),
                          Defining_Identifier => Map_Port_Default_Entry (F),
                          Value               =>
                            Make_New_Statement
                              (Defining_Identifier =>
                                 Make_Defining_Identifier (ON (O_Entry)),
                               Parameters =>
                                 Make_List_Id (Map_Port_Default_Value (F))));
                     RTU.Append_Node_To_List (N, Init_Declarations);

                     Nb_In_Ports_Fifo := Nb_In_Ports_Fifo + 1;

                     Make_In_Port (F);
                  elsif Is_Out (F) then
                     Make_Out_Port (F);
                  end if;
               end if;
               F := AIN.Next_Node (F);
            end loop;

            --  Global queue declaration
            if Offset = 0 then
               N := Make_Null_Statement;
            else
               N :=
                 Make_New_Statement
                   (Defining_Identifier =>
                      Make_Defining_Identifier (ON (O_Entry)),
                    Parameters =>
                      Make_List_Id
                        (Make_Literal (New_Int_Value (Offset, 0, 10))),
                    Is_Array => True);
            end if;
            N :=
              Make_Variable_Declaration
                (Used_Type => Make_Defining_Identifier (ON (O_Entry)),
                 Defining_Identifier => Map_Task_Entries_Identifier (S),
                 Value               => N);
            RTU.Append_Node_To_List (N, Init_Declarations);

            --  portsRouter creation
            RTU.Append_Node_To_List (Map_RTSJ_Defining_Identifier (S), Params);
            RTU.Append_Node_To_List (Map_Task_Entries_Identifier (S), Params);
            RTU.Append_Node_To_List
              (Make_Literal (New_Int_Value (Nb_In_Ports_Fifo, 0, 10)),
               Params);
            RTU.Append_Node_To_List
              (Make_Literal (New_Int_Value (Nb_In_Ports_Non_Fifo, 0, 10)),
               Params);

            N :=
              Make_Assignment_Statement
                (Defining_Identifier => Map_Task_Ports_Router_Identifier (S),
                 Expression          =>
                   Make_New_Statement
                     (Make_Defining_Identifier (ON (O_Ports_Router)),
                      Params));
            RTU.Append_Node_To_List (N, Init_Declarations);
         end if;

         --  Task job implementation
         N := Message_Comment ("Execute the task's job");
         RTU.Append_Node_To_List (N, Task_Job_Statements);

         --  Visit all the subcomponents of the thread
         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := AIN.First_Node (Calls (E));

            while Present (Call_Seq) loop
               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := AIN.First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));
                     Spg_Call := AIN.Next_Node (Spg_Call);
                  end loop;
               end if;
               Call_Seq := AIN.Next_Node (Call_Seq);
            end loop;
         end if;

         Spec :=
           Make_Function_Specification
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Return_Type         => New_Node (K_Void),
              Defining_Identifier => Map_Task_Job_Identifier (S),
              Throws              =>
                Make_List_Id
                  (Make_Defining_Identifier (ON (O_Program_Exception))));
         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Task_Job_Statements);
         RTU.Append_Node_To_List (Impl, Main_Class_Methods);

      end Visit_Thread_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------
      procedure Visit_Subprogram_Instance (E : Node_Id) is
         N : Node_Id;
      begin
         --  Generate the call of the subprogram
         if No (Get_Handling (E, By_Name, H_RTSJ_Subprogram_Spec)) then

            N :=
              Make_Call_Function
                (Defining_Identifier =>
                   Make_Pointed_Notation
                     (Left_Member =>
                        Make_Defining_Identifier (ON (O_Subprograms)),
                      Right_Member => Map_RTSJ_Defining_Identifier (E)));
            RTU.Append_Node_To_List (N, Task_Job_Statements);

            --  Mark the subprogram as being handled
            Set_Handling (E, By_Name, H_RTSJ_Subprogram_Spec, N);
         end if;

         Bind_AADL_To_Subprogram
           (Identifier (E),
            Get_Handling (E, By_Name, H_RTSJ_Subprogram_Spec));

      end Visit_Subprogram_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_RTSJ.Activity;
