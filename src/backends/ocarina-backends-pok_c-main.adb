------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . P O K _ C . M A I N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.POK_C.Runtime;
with Ocarina.Backends.C_Common.Mapping;

with Ocarina.Instances.Queries;

package body Ocarina.Backends.POK_C.Main is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.C_Values;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.POK_C.Runtime;
   use Ocarina.Backends.C_Common.Mapping;

   use Ocarina.Instances.Queries;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package CV renames Ocarina.Backends.C_Values;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is

      Tattr                 : Node_Id;
      Main_Function         : Node_Id;
      Statements            : List_Id;
      Init_Function         : Node_Id            := No_Node;
      Thread_Id             : Unsigned_Long_Long := 0;
      Process_Variable_Type : Node_Id;
      Process_Variable_Name : Node_Id;

      Error_Switch_Alternatives : List_Id;

      Use_Error_Handling : Boolean := False;

      Current_Device : Node_Id := No_Node;

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
        (E                       : Node_Id;
         Real_Process            : Boolean := True;
         Corresponding_Component : Node_Id := No_Node);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);

      procedure Setup_Thread (E : Node_Id);

      function Map_Queueing_Policy (Port : Node_Id) return Node_Id;

      function Map_Queueing_Policy (Port : Node_Id) return Node_Id is
         ARINC653_Discipline : constant ARINC653_Queuing_Discipline :=
           Get_ARINC653_Queuing_Discipline (Port);
      begin
         if ARINC653_Discipline /= Invalid
           and then ARINC653_Discipline = Priority_Based
         then

            if Use_ARINC653_API then
               return RE (RE_Priority);
            else
               return RE (RE_Pok_Port_Queueing_Discipline_Fifo);
            end if;
         end if;

         if Use_ARINC653_API then
            return RE (RE_Fifo);
         else
            return RE (RE_Pok_Port_Queueing_Discipline_Fifo);
         end if;
      end Map_Queueing_Policy;

      ------------------
      -- Setup_Thread --
      ------------------

      procedure Setup_Thread (E : Node_Id) is
         Parameters   : List_Id;
         N            : Node_Id;
         S            : constant Node_Id := Parent_Subcomponent (E);
         Member_Value : Node_Id;
         Stack_Size   : Unsigned_Long_Long;
      begin
         --  Initializes thread attributes.
         if Use_ARINC653_API = False then
            N :=
              POK_Make_Function_Call_With_Assert
                (RF (RE_Pok_Thread_Attr_Init),
                 Make_List_Id (Make_Variable_Address (Copy_Node (Tattr))));

            Append_Node_To_List (N, Statements);

            POK_Add_Return_Assertion (Statements);
         end if;

         --  Make strcpy(tattr.NAME, "prname")

         if POK_Flavor = DEOS or else POK_Flavor = VXWORKS then
            N :=
              Make_Call_Profile
                (Make_Defining_Identifier (Get_String_Name ("strcpy")),
                 Make_List_Id
                   (Make_Member_Designator (RE (RE_Name), Copy_Node (Tattr)),
                    Make_Literal
                      (CV.New_Pointed_Char_Value (Name (Identifier (S))))));
            Append_Node_To_List (N, Statements);
         end if;

         --  Make tattr.entry = entrypoint

         if Use_ARINC653_API then
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Member_Designator
                     (RE (RE_Entry_Point),
                      Copy_Node (Tattr)),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   Copy_Node
                     (CTN.Defining_Identifier
                        (CTN.Job_Node (Backend_Node (Identifier (S))))));

         else
            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Member_Designator (RE (RE_Entry), Copy_Node (Tattr)),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   Copy_Node
                     (CTN.Defining_Identifier
                        (CTN.Job_Node (Backend_Node (Identifier (S))))));
         end if;

         Append_Node_To_List (N, Statements);

         --  Add priority to tattr, tattr.priority = threadpriority

         if Get_Thread_Priority (E) /= 0 then
            N := Make_Literal (New_Int_Value (Get_Thread_Priority (E), 1, 10));

            if Use_ARINC653_API then
               N :=
                 Make_Expression
                   (Left_Expr =>
                      Make_Member_Designator
                        (RE (RE_Base_Priority),
                         Copy_Node (Tattr)),
                    Operator   => Op_Equal,
                    Right_Expr => N);
            else
               N :=
                 Make_Expression
                   (Left_Expr =>
                      Make_Member_Designator
                        (Make_Defining_Identifier (MN (M_Priority)),
                         Copy_Node (Tattr)),
                    Operator   => Op_Equal,
                    Right_Expr => N);
            end if;

            Append_Node_To_List (N, Statements);
         end if;

         if Get_Thread_Deadline (E) /= Null_Time then
            if POK_Flavor = ARINC653 then
               Member_Value :=
                 Map_Time_To_Millisecond (Get_Thread_Deadline (E));
            elsif POK_Flavor = POK then
               Member_Value := Map_Time (Get_Thread_Deadline (E));
            else
               Member_Value := No_Node;
            end if;

            if Member_Value /= No_Node then
               N :=
                 Make_Expression
                   (Left_Expr =>
                      Make_Member_Designator
                        (RE (RE_Deadline),
                         Copy_Node (Tattr)),
                    Operator   => Op_Equal,
                    Right_Expr => Member_Value);
               Append_Node_To_List (N, Statements);
            end if;
         else
            Display_Error ("Deadline not specified", Fatal => False);
         end if;

         if Get_Thread_Period (E) /= Null_Time then
            if Use_ARINC653_API then
               if POK_Flavor = POK then
                  Member_Value :=
                    Map_Time_To_Millisecond (Get_Thread_Period (E));
               elsif POK_Flavor = DEOS or else POK_Flavor = VXWORKS then
                  Member_Value :=
                    Map_Time_To_Nanosecond (Get_Thread_Period (E));
               end if;
            else
               Member_Value := Map_Time (Get_Thread_Period (E));
            end if;

            if Member_Value /= No_Node then
               N :=
                 Make_Expression
                   (Left_Expr =>
                      Make_Member_Designator
                        (RE (RE_Period),
                         Copy_Node (Tattr)),
                    Operator   => Op_Equal,
                    Right_Expr => Member_Value);

               Append_Node_To_List (N, Statements);
            end if;
         else
            Display_Error ("Period not specified", Fatal => True);
         end if;

         --
         --  Set up the Stack Size
         --  On most system, the default is 4096. So, we set
         --  up 4096 if not explicitly declared. We use
         --  The AADL property Stack_Size if declared.
         --

         Stack_Size := 4096;

         if Get_Thread_Stack_Size (E) /= Null_Size then
            Stack_Size := To_Bytes (Get_Thread_Stack_Size (E));
         end if;
         N :=
           Make_Expression
             (Left_Expr =>
                Make_Member_Designator (RE (RE_Stack_Size), Copy_Node (Tattr)),
              Operator   => Op_Equal,
              Right_Expr => Make_Literal (New_Int_Value (Stack_Size, 1, 10)));
         Append_Node_To_List (N, Statements);

         declare
            TA       : constant Time_Array := Get_Execution_Time (E);
            Capacity : Node_Id;
         begin
            if TA /= Empty_Time_Array then
               if Use_ARINC653_API then
                  if POK_Flavor = ARINC653 then
                     Capacity := Map_Time_To_Millisecond (TA (1));
                  elsif POK_Flavor = DEOS then
                     Capacity := Map_Time_To_Nanosecond (TA (1));
                  elsif POK_Flavor = VXWORKS then
                     Capacity := Map_Time_To_Nanosecond (TA (1));
                  else
                     Capacity := No_Node;
                  end if;
               else
                  Capacity := Map_Time (TA (1));
               end if;
            else
               Display_Error
                 ("Compute execution time not declared",
                  Fatal => False);

               --  By default, we allocate 1 ms for thread execution.
               Capacity :=
                 CTU.Make_Literal (CV.New_Int_Value (Thread_Id, 1, 10));
            end if;

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Member_Designator
                     (RE (RE_Time_Capacity),
                      Copy_Node (Tattr)),
                 Operator   => Op_Equal,
                 Right_Expr => Capacity);

            Append_Node_To_List (N, Statements);
         end;

         --  Add tid and other stuff to the parameters and call
         --  pok_thread_create

         Parameters := New_List (CTN.K_Parameter_List);

         if Use_ARINC653_API then
            Append_Node_To_List
              (Make_Variable_Address (Copy_Node (Tattr)),
               Parameters);

            Append_Node_To_List
              (Make_Variable_Address
                 (Make_Array_Value
                    (Copy_Node (Process_Variable_Name),
                     CTU.Make_Literal (CV.New_Int_Value (Thread_Id, 1, 10)))),
               Parameters);
            Add_Return_Variable_In_Parameters (Parameters);
         else
            Append_Node_To_List
              (Make_Variable_Address
                 (Make_Array_Value
                    (Copy_Node (Process_Variable_Name),
                     CTU.Make_Literal (CV.New_Int_Value (Thread_Id, 1, 10)))),
               Parameters);

            Append_Node_To_List
              (Make_Variable_Address (Copy_Node (Tattr)),
               Parameters);

         end if;

         if Use_ARINC653_API then
            Append_Node_To_List
              (POK_Make_Function_Call_With_Assert
                 (RF (RE_Create_Process),
                  Parameters),
               Statements);
            Parameters := New_List (CTN.K_Parameter_List);
            Append_Node_To_List
              (Make_Array_Value
                 (Copy_Node (Process_Variable_Name),
                  CTU.Make_Literal (CV.New_Int_Value (Thread_Id, 1, 10))),
               Parameters);
            Add_Return_Variable_In_Parameters (Parameters);
            Append_Node_To_List
              (POK_Make_Function_Call_With_Assert (RF (RE_Start), Parameters),
               Statements);
         else
            Append_Node_To_List
              (POK_Make_Function_Call_With_Assert
                 (RF (RE_Pok_Thread_Create),
                  Parameters),
               Statements);
         end if;

         N :=
           Message_Comment
             ("This thread was mapped from a thread component contained" &
              "in this process. The function it executes is also generated" &
              "in the file activity.c.");

         POK_Add_Return_Assertion (Statements);

         CTU.Append_Node_To_List (N, Statements);
      end Setup_Thread;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case Kind (E) is
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

            when CC_Device =>
               Visit_Device_Instance (E);

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
        (E                       : Node_Id;
         Real_Process            : Boolean := True;
         Corresponding_Component : Node_Id := No_Node)
      is
         U                : Node_Id;
         P                : Node_Id;
         N                : Node_Id;
         S                : Node_Id;
         Spec             : Node_Id;
         Declarations     : List_Id := New_List (CTN.K_Declaration_List);
         Call_Parameters  : List_Id;
         Called_Function  : Node_Id;
         Used_Type        : Node_Id;
         Used_Member      : Node_Id;
         While_Statements : constant List_Id :=
           New_List (CTN.K_Statement_List);
         Shared_Data             : Node_Id;
         Function_Call           : Node_Id;
         Communicating_Component : Node_Id;

         procedure Setup_In_Ports;
         procedure Setup_Out_Ports;

         --------------------
         -- Setup_In_Ports --
         --------------------

         procedure Setup_In_Ports is
            F               : Node_Id;
            Called_Function : Node_Id;
            Added_Parameter : Node_Id;
            Variable_Type   : Node_Id;
         begin
            if AINU.Is_Empty (Features (Communicating_Component)) then
               return;
            end if;

            F := First_Node (Features (Communicating_Component));

            while Present (F) loop
               Call_Parameters := New_List (CTN.K_Parameter_List);

               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then not Is_Out (F)
                 and then Get_Connection_Pattern (F) = Inter_Process
                 and then not Is_Virtual (Get_Port_By_Name (F, Current_Device))
               then
                  if AIN.Is_Data (F) and then Is_Event (F) then
                     Append_Node_To_List
                       (Make_Literal
                          (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                        Call_Parameters);

                     if POK_Flavor = POK then
                        N := Map_Queue_Size_With_Data (F);
                        Append_Node_To_List (N, Call_Parameters);
                     else

                        --  Map the size

                        N := Get_Data_Size (Corresponding_Instance (F));

                        Append_Node_To_List (N, Call_Parameters);

                        --  Map the queue size

                        N := Map_Queue_Size (F);
                        Append_Node_To_List (N, Call_Parameters);
                     end if;

                     if Use_ARINC653_API then
                        Added_Parameter := RE (RE_Destination);
                     else
                        Added_Parameter := RE (RE_Pok_Port_Direction_In);
                     end if;

                     Append_Node_To_List (Added_Parameter, Call_Parameters);

                     Added_Parameter := Map_Queueing_Policy (F);

                     Append_Node_To_List (Added_Parameter, Call_Parameters);

                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Defining_Identifier (Map_Port_Var (F))),
                        Call_Parameters);

                     if Use_ARINC653_API then
                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (VN (V_Ret))),
                           Call_Parameters);

                        Called_Function := RF (RE_Create_Queuing_Port);
                     else
                        Called_Function := RF (RE_Pok_Port_Queueing_Create);
                     end if;

                     Append_Node_To_List
                       (POK_Make_Function_Call_With_Assert
                          (Called_Function,
                           Call_Parameters),
                        Statements);

                     POK_Add_Return_Assertion (Statements);

                     if Use_ARINC653_API then
                        Variable_Type := RE (RE_Queuing_Port_Id_Type);
                     else
                        Variable_Type := RE (RE_Uint8_T);
                     end if;

                     Append_Node_To_List
                       (Make_Variable_Declaration
                          (Defining_Identifier =>
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Used_Type => Variable_Type),
                        CTN.Declarations (Current_File));

                     N :=
                       Message_Comment
                         ("This queueing port was mapped from an in" &
                          "event data port contained in the process. It" &
                          "is used for inter-partition communication.");

                     CTU.Append_Node_To_List (N, Statements);
                  elsif AIN.Is_Data (F) and then not Is_Event (F) then

                     --
                     --  Here, we have a sampling port.
                     --

                     Append_Node_To_List
                       (Make_Literal
                          (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                        Call_Parameters);

                     if Current_Device = No_Node then
                        N := Get_Inter_Partition_Port_Size (F);

                        if Is_Using_Virtual_Bus (F) then
                           Add_Include (RH (RH_Protocols));
                           Add_Define_Deployment (RE (RE_Pok_Needs_Protocols));
                        end if;
                     else
                        N :=
                          Get_Inter_Partition_Port_Size
                            (Get_Port_By_Name (F, Current_Device));

                        if Is_Using_Virtual_Bus
                            (Get_Port_By_Name (F, Current_Device))
                        then
                           Add_Include (RH (RH_Protocols));
                           Add_Define_Deployment (RE (RE_Pok_Needs_Protocols));
                        end if;
                     end if;

                     Append_Node_To_List (N, Call_Parameters);

                     --  Map the port of the sampling port, take in
                     --  account potential virtual bus layers.

                     if Use_ARINC653_API then
                        Append_Node_To_List
                          (RE (RE_Destination),
                           Call_Parameters);
                     else
                        Append_Node_To_List
                          (RE (RE_Pok_Port_Direction_In),
                           Call_Parameters);
                     end if;

                     if Get_POK_Refresh_Time (F) /= Null_Time then
                        if Use_ARINC653_API then
                           N :=
                             Map_Time_To_Nanosecond (Get_POK_Refresh_Time (F));
                        else
                           N := Map_Time (Get_POK_Refresh_Time (F));
                        end if;
                     else
                        if POK_Flavor = DEOS or else POK_Flavor = VXWORKS then

                           --
                           --  DeOS needs a value to refresh the port.
                           --

                           N :=
                             CTU.Make_Literal
                               (CV.New_Int_Value (1_000_000, 1, 10));
                        else
                           N := CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                        end if;
                     end if;
                     Append_Node_To_List (N, Call_Parameters);

                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Defining_Identifier (Map_Port_Var (F))),
                        Call_Parameters);

                     if Use_ARINC653_API then
                        Add_Return_Variable_In_Parameters (Call_Parameters);
                        Called_Function := RF (RE_Create_Sampling_Port);
                     else
                        Called_Function := RF (RE_Pok_Port_Sampling_Create);
                     end if;

                     Append_Node_To_List
                       (POK_Make_Function_Call_With_Assert
                          (Called_Function,
                           Call_Parameters),
                        Statements);

                     POK_Add_Return_Assertion (Statements);

                     if Use_ARINC653_API then
                        Variable_Type := RE (RE_Sampling_Port_Id_Type);
                     else
                        Variable_Type := RE (RE_Uint8_T);
                     end if;

                     Append_Node_To_List
                       (Make_Variable_Declaration
                          (Defining_Identifier =>
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Used_Type => (Variable_Type)),
                        CTN.Declarations (Current_File));

                     N :=
                       Message_Comment
                         ("This sampling port was mapped from a in data" &
                          "port contained in the process. It is used" &
                          "for inter-partition communication.");

                     CTU.Append_Node_To_List (N, Statements);
                  end if;
               end if;
               F := Next_Node (F);
            end loop;
         end Setup_In_Ports;

         ---------------------
         -- Setup_Out_Ports --
         ---------------------

         procedure Setup_Out_Ports is
            F               : Node_Id;
            Called_Function : Node_Id;
            Added_Parameter : Node_Id;
            Variable_Type   : Node_Id;
         begin

            Error_Switch_Alternatives := New_List (CTN.K_Alternatives_List);

            if AINU.Is_Empty (Features (Communicating_Component)) then
               return;
            end if;

            F := First_Node (Features (Communicating_Component));

            while Present (F) loop

               Call_Parameters := New_List (CTN.K_Parameter_List);

               if Kind (F) = K_Port_Spec_Instance
                 and then Is_Out (F)
                 and then not Is_In (F)
                 and then Get_Connection_Pattern (F) = Inter_Process
                 and then not Is_Virtual (Get_Port_By_Name (F, Current_Device))
               then
                  if AIN.Is_Data (F) and then not Is_Event (F) then
                     Append_Node_To_List
                       (Make_Literal
                          (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                        Call_Parameters);

                     if Current_Device = No_Node then
                        N := Get_Inter_Partition_Port_Size (F);

                        if Is_Using_Virtual_Bus (F) then
                           Add_Include (RH (RH_Protocols));
                           Add_Define_Deployment (RE (RE_Pok_Needs_Protocols));
                        end if;
                     else
                        N :=
                          Get_Inter_Partition_Port_Size
                            (Get_Port_By_Name (F, Current_Device));

                        if Is_Using_Virtual_Bus
                            (Get_Port_By_Name (F, Current_Device))
                        then
                           Add_Include (RH (RH_Protocols));
                           Add_Define_Deployment (RE (RE_Pok_Needs_Protocols));
                        end if;
                     end if;

                     Append_Node_To_List (N, Call_Parameters);

                     --  Map the size of the port, take in account
                     --  potential virtual bus layers.

                     if Use_ARINC653_API then
                        Append_Node_To_List (RE (RE_Source), Call_Parameters);
                     else
                        Append_Node_To_List
                          (RE (RE_Pok_Port_Direction_Out),
                           Call_Parameters);
                     end if;

                     if Get_POK_Refresh_Time (F) /= Null_Time then

                        if Use_ARINC653_API then
                           N :=
                             Map_Time_To_Millisecond
                               (Get_POK_Refresh_Time (F));
                        else
                           N := Map_Time (Get_POK_Refresh_Time (F));
                        end if;

                     else
                        if POK_Flavor = DEOS then

                           --
                           --  DeOS needs a value to refresh the port.
                           --

                           N :=
                             CTU.Make_Literal
                               (CV.New_Int_Value (1_000_000, 1, 10));
                        else
                           N := CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                        end if;
                     end if;

                     if POK_Flavor = VXWORKS then
                        N := RE (RE_Infinite_Time_Value);
                     end if;

                     Append_Node_To_List (N, Call_Parameters);

                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Defining_Identifier (Map_Port_Var (F))),
                        Call_Parameters);

                     if Use_ARINC653_API then
                        Add_Return_Variable_In_Parameters (Call_Parameters);
                        Called_Function := RF (RE_Create_Sampling_Port);
                     else
                        Called_Function := RF (RE_Pok_Port_Sampling_Create);
                     end if;

                     N :=
                       POK_Make_Function_Call_With_Assert
                         (Called_Function,
                          Call_Parameters);
                     Append_Node_To_List (N, Statements);

                     POK_Add_Return_Assertion (Statements);

                     if Use_ARINC653_API then
                        Variable_Type := RE (RE_Sampling_Port_Id_Type);
                     else
                        Variable_Type := RE (RE_Uint8_T);
                     end if;

                     Append_Node_To_List
                       (Make_Variable_Declaration
                          (Defining_Identifier =>
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Used_Type => (Variable_Type)),
                        CTN.Declarations (Current_File));

                     N :=
                       Message_Comment
                         ("This sampling port was mapped from an out" &
                          "data port contained in the process. It is" &
                          "used for inter-partition communication.");

                     CTU.Append_Node_To_List (N, Statements);
                  elsif AIN.Is_Data (F) and then Is_Event (F) then
                     Append_Node_To_List
                       (Make_Literal
                          (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                        Call_Parameters);

                     if POK_Flavor = POK then
                        N := Map_Queue_Size_With_Data (F);
                        Append_Node_To_List (N, Call_Parameters);
                     else
                        --  Map the size

                        N := Get_Data_Size (Corresponding_Instance (F));

                        Append_Node_To_List (N, Call_Parameters);

                        --  Map the queue size

                        N := Map_Queue_Size (F);
                        Append_Node_To_List (N, Call_Parameters);
                     end if;

                     if Use_ARINC653_API then
                        Added_Parameter := RE (RE_Source);
                     else
                        Added_Parameter := RE (RE_Pok_Port_Direction_Out);
                     end if;

                     Append_Node_To_List (Added_Parameter, Call_Parameters);

                     Added_Parameter := Map_Queueing_Policy (F);

                     Append_Node_To_List (Added_Parameter, Call_Parameters);

                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Defining_Identifier (Map_Port_Var (F))),
                        Call_Parameters);

                     if Use_ARINC653_API then
                        Add_Return_Variable_In_Parameters (Call_Parameters);

                        Called_Function := RF (RE_Create_Queuing_Port);
                     else
                        Called_Function := RF (RE_Pok_Port_Queueing_Create);
                     end if;

                     N :=
                       POK_Make_Function_Call_With_Assert
                         (Called_Function,
                          Call_Parameters);

                     Append_Node_To_List (N, Statements);

                     POK_Add_Return_Assertion (Statements);

                     if Use_ARINC653_API then
                        Variable_Type := RE (RE_Queuing_Port_Id_Type);
                     else
                        Variable_Type := RE (RE_Uint8_T);
                     end if;

                     Append_Node_To_List
                       (Make_Variable_Declaration
                          (Defining_Identifier =>
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Used_Type => (Variable_Type)),
                        CTN.Declarations (Current_File));

                     N :=
                       Message_Comment
                         ("This queueing port was mapped from an out " &
                          "event data port contained in the process. It" &
                          "is used for inter-partition communication.");

                     CTU.Append_Node_To_List (N, Statements);
                  end if;
               end if;
               F := Next_Node (F);
            end loop;
         end Setup_Out_Ports;

         -------------------------
         -- Setup_Virtual_Ports --
         -------------------------

         procedure Setup_Virtual_Ports is
            F : Node_Id;
         begin
            Call_Parameters := New_List (CTN.K_Parameter_List);

            if AINU.Is_Empty (Features (Communicating_Component)) then
               return;
            end if;

            F := First_Node (Features (Communicating_Component));

            while Present (F) loop
               Call_Parameters := New_List (CTN.K_Parameter_List);

               if Kind (F) = K_Port_Spec_Instance
                 and then Is_Data (F)
                 and then Get_Connection_Pattern (F) = Inter_Process
                 and then Is_Virtual (Get_Port_By_Name (F, Current_Device))
               then
                  Append_Node_To_List
                    (Make_Literal
                       (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                     Call_Parameters);

                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Defining_Identifier (Map_Port_Var (F))),
                     Call_Parameters);

                  N :=
                    Make_Call_Profile
                      (RF (RE_Pok_Port_Virtual_Create),
                       Call_Parameters);
                  Append_Node_To_List (N, Statements);

                  Append_Node_To_List
                    (Make_Variable_Declaration
                       (Defining_Identifier =>
                          (Make_Defining_Identifier (Map_Port_Var (F))),
                        Used_Type => RE (RE_Uint8_T)),
                     CTN.Declarations (Current_File));
               end if;
               F := Next_Node (F);
            end loop;
         end Setup_Virtual_Ports;

      begin
         if Real_Process then
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);

            Push_Entity (P);
            Push_Entity (U);

            Communicating_Component := E;
         else
            Communicating_Component := Corresponding_Component;
         end if;

         Set_Main_Source;

         Use_Error_Handling := False;

         Thread_Id := 1;

         Statements := New_List (CTN.K_Statement_List);

         Add_Include (E => RH (RH_Activity));
         Add_Include (E => RH (RH_Gtypes));

         if Use_ARINC653_API then
            Process_Variable_Type := RE (RE_Process_Id_Type);
            Process_Variable_Name := RE (RE_Arinc_Threads);
         else
            Process_Variable_Type := RE (RE_Uint32_T);
            Process_Variable_Name := RE (RE_Pok_Threads);
         end if;

         --  Set up ports of the partition
         Append_Node_To_List
           (Make_Variable_Declaration
              (CTU.Make_Array_Declaration
                 (Defining_Identifier => Process_Variable_Name,
                  Array_Size          => RE (RE_Pok_Config_Nb_Threads)),
               Used_Type => Process_Variable_Type),
            CTN.Declarations (Current_File));

         Set_Str_To_Name_Buffer ("tattr");
         Tattr := Make_Defining_Identifier (Name_Find);

         if Use_ARINC653_API then
            N :=
              Make_Variable_Declaration
                (Defining_Identifier => Copy_Node (Tattr),
                 Used_Type           => RE (RE_Process_Attribute_Type));
         else
            N :=
              Make_Variable_Declaration
                (Defining_Identifier => Copy_Node (Tattr),
                 Used_Type           => RE (RE_Pok_Thread_Attr_T));
         end if;

         --  Declare the variable that contain
         --  tasks/process attributes.

         Append_Node_To_List (N, Statements);

         POK_Declare_Return_Variable (Statements);

         Setup_In_Ports;
         Setup_Out_Ports;
         Setup_Virtual_Ports;

         --  Make the main function specification and add it in the current
         --  file (main.c).

         Spec :=
           Make_Function_Specification
             (Defining_Identifier => RE (RE_Main),
              Parameters          => No_List,
              Return_Type         => RE (RE_Int));

         if Init_Function /= No_Node then
            Append_Node_To_List (Init_Function, Statements);
         end if;

         --  First, generate code to create semaphores associated
         --  with data components.
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               if AINU.Is_Data (Corresponding_Instance (S)) then
                  Shared_Data := Corresponding_Instance (S);
                  --  Automatically use the types.h header if we use
                  --  protected data.

                  Add_Include (RH (RH_Types));

                  --  Declare the variable in the file

                  N :=
                    Make_Variable_Declaration
                      (Map_C_Defining_Identifier (S),
                       Map_C_Data_Type_Designator (Shared_Data));

                  Append_Node_To_List (N, CTN.Declarations (Current_File));

                  Bind_AADL_To_Object (Identifier (S), N);

                  --  If the data needs to be protected, generate
                  --  function call to create locking objects.

                  if Is_Protected_Data (Shared_Data) then
                     Call_Parameters := New_List (CTN.K_Parameter_List);

                     if Use_ARINC653_API then
                        --  Add the semaphore name.
                        Append_Node_To_List
                          (Make_Literal
                             (CV.New_Pointed_Char_Value
                                (Map_Associated_Locking_Entity_Name (S))),
                           Call_Parameters);

                        --  Current semaphore value.
                        Append_Node_To_List
                          (Make_Literal (New_Int_Value (1, 1, 10)),
                           Call_Parameters);

                        --  Maximum semaphore value.
                        Append_Node_To_List
                          (Make_Literal (New_Int_Value (1, 1, 10)),
                           Call_Parameters);

                        --  Specify the queueing port discipline
                        Append_Node_To_List (RE (RE_Fifo), Call_Parameters);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier
                                (Map_Associated_Locking_Entity_Name (S))),
                           Call_Parameters);

                        Add_Return_Variable_In_Parameters (Call_Parameters);

                        Function_Call :=
                          POK_Make_Function_Call_With_Assert
                            (RF (RE_Create_Semaphore),
                             Call_Parameters);
                     else
                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier
                                (Map_Associated_Locking_Entity_Name (S))),
                           Call_Parameters);

                        --  Current semaphore value.
                        Append_Node_To_List
                          (Make_Literal (New_Int_Value (1, 1, 10)),
                           Call_Parameters);

                        --  Maximum semaphore value.
                        Append_Node_To_List
                          (Make_Literal (New_Int_Value (1, 1, 10)),
                           Call_Parameters);

                        --  Specify the queueing port discipline
                        Append_Node_To_List
                          (RE (RE_Pok_Semaphore_Discipline_Fifo),
                           Call_Parameters);

                        Function_Call :=
                          POK_Make_Function_Call_With_Assert
                            (RF (RE_Pok_Sem_Create),
                             Call_Parameters);
                     end if;

                     Append_Node_To_List (Function_Call, Statements);
                  end if;
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  Then, generate the other components
         --  correspond to thread initialization.

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         if Use_Error_Handling then
            Call_Parameters := New_List (CTN.K_Parameter_List);

            if Use_ARINC653_API then
               Called_Function := RF (RE_Create_Error_Handler);

               Append_Node_To_List
                 (RE (RE_Pok_Error_Handler_Worker),
                  Call_Parameters);

               Append_Node_To_List
                 (Make_Literal (CV.New_Int_Value (8192, 1, 10)),
                  Call_Parameters);

               Add_Return_Variable_In_Parameters (Call_Parameters);
            else
               Called_Function := RF (RE_Pok_Error_Handler_Create);
            end if;

            N := CTU.Make_Call_Profile (Called_Function, Call_Parameters);
            Append_Node_To_List (N, Statements);

            POK_Add_Return_Assertion (Statements);

            N :=
              Message_Comment
                ("One thread inside the partition can raise faults." &
                 "We start the error handle to treat these potential" &
                 "faults.");

            CTU.Append_Node_To_List (N, Statements);
         end if;

         --  Here, all threads were created. Put the partition
         --  in the NORMAL state.

         if POK_Flavor = POK then
            N :=
              CTU.Make_Call_Profile
                (RF (RE_Pok_Partition_Set_Mode),
                 Make_List_Id (RE (RE_Pok_Partition_Mode_Normal)));
         else
            Call_Parameters := New_List (CTN.K_Parameter_List);

            Append_Node_To_List (RE (RE_Normal), Call_Parameters);
            Add_Return_Variable_In_Parameters (Call_Parameters);

            N :=
              CTU.Make_Call_Profile
                (RF (RE_Set_Partition_Mode),
                 Call_Parameters);
         end if;
         Append_Node_To_List (N, Statements);

         N :=
           Message_Comment
             ("Now, we created all resources of the process. Consequently," &
              "this thread will not be used any more and it will be kept" &
              "in a dormant state. By doing that, we also allow one more" &
              "thread in this partition");

         CTU.Append_Node_To_List (N, Statements);

         N :=
           CTU.Make_Return_Statement (Make_Literal (New_Int_Value (0, 1, 10)));
         Append_Node_To_List (N, Statements);

         Main_Function :=
           Make_Function_Implementation (Spec, Declarations, Statements);

         --  Now, we define the error handler callback.
         --  This function is used each time an error is raised by
         --  the kernel.

         if Use_Error_Handling then
            Set_Deployment_Header;

            N :=
              CTU.Make_Define_Statement
                (Defining_Identifier =>
                   RE (RE_Pok_Use_Generated_Error_Handler),
                 Value => CTU.Make_Literal (CV.New_Int_Value (1, 1, 10)));

            CTU.Append_Node_To_List (N, CTN.Declarations (CTU.Current_File));

            Set_Main_Source;

            Declarations := New_List (CTN.K_Declaration_List);
            Statements   := New_List (CTN.K_Statement_List);

            if Use_ARINC653_API then
               Used_Type := RE (RE_Error_Status_Type);
            else
               Used_Type := RE (RE_Pok_Error_Status_T);
            end if;

            Append_Node_To_List
              (Make_Variable_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Error_Status)),
                  Used_Type => Used_Type),
               Statements);

            if Use_ARINC653_API then
               Append_Node_To_List
                 (Make_Variable_Declaration
                    (Defining_Identifier =>
                       Make_Defining_Identifier (VN (V_Ret)),
                     Used_Type => RE (RE_Return_Code_Type)),
                  Statements);
            end if;

            N :=
              Message_Comment
                ("The variables error and thread are created" &
                 "to store the thread-id and the error-id" &
                 "when a fault is raised");

            CTU.Append_Node_To_List (N, Statements);

            N :=
              Message_Comment
                ("We prefer to force a default value to the variables" &
                 "error and thread");
            Append_Node_To_List (N, Statements);

            --  Here, we initialize the variables to be sure to have
            --  a good default value.

            Call_Parameters := New_List (CTN.K_Parameter_List);

            Append_Node_To_List
              (Make_Variable_Address
                 (Make_Defining_Identifier (VN (V_Error_Status))),
               Call_Parameters);

            N :=
              Message_Comment
                ("Here, we declare the fault handler as ready and" &
                 "that faulty thread-id and error-id must be stored" &
                 "in the thread and error variables");

            CTU.Append_Node_To_List (N, Statements);

            if Use_ARINC653_API then
               Called_Function := RF (RE_Stop_Self);
            else
               Called_Function := RF (RE_Pok_Thread_Stop_Self);
            end if;

            Append_Node_To_List
              (Make_Call_Profile (Called_Function, No_List),
               While_Statements);

            if Use_ARINC653_API then
               Called_Function := RF (RE_Get_Error_Status);
            else
               Called_Function := RF (RE_Pok_Error_Get);
            end if;

            Call_Parameters := New_List (CTN.K_Parameter_List);

            Append_Node_To_List
              (Make_Variable_Address
                 (Make_Defining_Identifier (VN (V_Error_Status))),
               Call_Parameters);

            if Use_ARINC653_API then
               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier (VN (V_Ret))),
                  Call_Parameters);
            end if;

            Append_Node_To_List
              (Make_Call_Profile (Called_Function, Call_Parameters),
               While_Statements);

            if Use_ARINC653_API then
               Used_Member := RE (RE_Failed_Process_Id);
            else
               Used_Member := Make_Defining_Identifier (MN (M_Failed_Thread));
            end if;
            Append_Node_To_List
              (Make_Switch_Statement
                 (Make_Member_Designator
                    (Used_Member,
                     Make_Defining_Identifier (VN (V_Error_Status))),
                  Error_Switch_Alternatives),
               While_Statements);

            --  We declare the while (1) loop executed by the task

            Append_Node_To_List
              (Make_While_Statement
                 (Make_Literal (CV.New_Int_Value (1, 0, 10)),
                  While_Statements),
               Statements);

            Spec :=
              Make_Function_Specification
                (Defining_Identifier => RE (RE_Pok_Error_Handler_Worker),
                 Parameters          => No_List,
                 Return_Type => Make_Defining_Identifier (TN (T_Void)));

            Append_Node_To_List
              (Make_Function_Implementation (Spec, Declarations, Statements),
               CTN.Declarations (Current_File));

            N :=
              Message_Comment
                ("IMPORTANT : this main function creates all resources " &
                 "(ports, data, tasks/threads/processes used by this   " &
                 "node");

            CTU.Append_Node_To_List (N, Statements);

            --  Finally, we implement the main function that handles errors.
         end if;

         Append_Node_To_List (Main_Function, CTN.Declarations (Current_File));

         if Real_Process then
            Pop_Entity; -- U
            Pop_Entity; -- P
         end if;
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.
               if AINU.Is_Processor (Corresponding_Instance (S)) then
                  Visit (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P              : constant Node_Id := CTN.Entity (U);
         Implementation : Node_Id;
         S              : Node_Id;
         Entrypoint : constant Node_Id := Get_Thread_Initialize_Entrypoint (E);
      begin
         Push_Entity (P);
         Push_Entity (U);

         Set_Main_Source;

         Current_Device := E;

         if Entrypoint /= No_Node then
            Init_Function :=
              Make_Call_Profile (Map_C_Subprogram_Identifier (Entrypoint));
         end if;

         Implementation := Get_Classifier_Property (E, "implemented_as");

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  if Get_Category_Of_Component (S) = CC_Process then
                     Visit_Process_Instance
                       (Corresponding_Instance (S),
                        False,
                        E);
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;
         end if;

         Init_Function := No_Node;

         Current_Device := No_Node;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Device_Instance;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
         S         : Node_Id;
         U         : Node_Id;
         Processes : List_Id;
      begin
         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            S         := AIN.First_Node (Processes);
            while Present (S) loop
               U := Current_Entity;
               Pop_Entity;
               Visit (AIN.Item (S));
               Push_Entity (U);
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_Virtual_Processor_Instance;

      ------------------------------
      -- Visit_Processor_Instance --
      ------------------------------

      procedure Visit_Processor_Instance (E : Node_Id) is
         S : Node_Id;
         U : Node_Id;
         P : Node_Id;
      begin
         U := CTN.Naming_Node (Backend_Node (Identifier (E)));
         P := CTN.Entity (U);

         Push_Entity (C_Root);

         Runtime.Register_Kernel_Unit (U);

         Push_Entity (P);
         Push_Entity (U);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity;
         Pop_Entity;
         Pop_Entity;
      end Visit_Processor_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is

         procedure Setup_Ports is
            F               : Node_Id;
            Call_Parameters : List_Id;
            Called_Function : Node_Id;
            Type_Used       : Node_Id;
            N               : Node_Id;
         begin
            if AINU.Is_Empty (Features (E)) then
               return;
            end if;

            F := AIN.First_Node (Features (E));
            while Present (F) loop
               Call_Parameters := New_List (CTN.K_Parameter_List);

               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then not Is_Out (F)
                 and then Get_Port_By_Name (F, Current_Device) = No_Node
                 and then Get_Connection_Pattern (F) = Intra_Process
               then
                  if AIN.Is_Data (F) and then not AIN.Is_Event (F) then
                     Append_Node_To_List
                       (Make_Literal
                          (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                        Call_Parameters);

                     N := CTU.Get_Data_Size (Corresponding_Instance (F));
                     Append_Node_To_List (N, Call_Parameters);

                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Defining_Identifier (Map_Port_Var (F))),
                        Call_Parameters);

                     if Use_ARINC653_API then
                        Add_Return_Variable_In_Parameters (Call_Parameters);

                        Called_Function := RF (RE_Create_Blackboard);
                        Type_Used       := RE (RE_Blackboard_Id_Type);
                     else
                        Called_Function := RF (RE_Pok_Blackboard_Create);
                        Type_Used       := RE (RE_Uint8_T);
                     end if;

                     N :=
                       POK_Make_Function_Call_With_Assert
                         (Called_Function,
                          Call_Parameters);
                     Append_Node_To_List (N, Statements);

                     POK_Add_Return_Assertion (Statements);

                     Append_Node_To_List
                       (Make_Extern_Entity_Declaration
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier (Map_Port_Var (F))),
                              Used_Type => Type_Used)),
                        CTN.Declarations (Current_File));

                  elsif AIN.Is_Data (F)
                    and then Is_Event (F)
                    and then Is_In (F)
                  then
                     if Use_ARINC653_API then
                        Append_Node_To_List
                          (Make_Literal
                             (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                           Call_Parameters);

                        N := CTU.Get_Data_Size (Corresponding_Instance (F));
                        Append_Node_To_List (N, Call_Parameters);

                        N := Map_Queue_Size (F);
                        Append_Node_To_List (N, Call_Parameters);

                        Append_Node_To_List
                          (Map_Queueing_Policy (F),
                           Call_Parameters);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Call_Parameters);

                        Add_Return_Variable_In_Parameters (Call_Parameters);

                        Called_Function := RF (RE_Create_Buffer);
                        Type_Used       := RE (RE_Buffer_Id_Type);
                     else
                        Append_Node_To_List
                          (Make_Literal
                             (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                           Call_Parameters);

                        N := Map_Queue_Size (F);
                        Append_Node_To_List (N, Call_Parameters);

                        N := CTU.Get_Data_Size (Corresponding_Instance (F));
                        Append_Node_To_List (N, Call_Parameters);

                        Append_Node_To_List
                          (Map_Queueing_Policy (F),
                           Call_Parameters);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Call_Parameters);

                        Called_Function := RF (RE_Pok_Buffer_Create);
                        Type_Used       := RE (RE_Uint8_T);
                     end if;

                     N :=
                       POK_Make_Function_Call_With_Assert
                         (Called_Function,
                          Call_Parameters);
                     Append_Node_To_List (N, Statements);

                     POK_Add_Return_Assertion (Statements);

                     Append_Node_To_List
                       (Make_Extern_Entity_Declaration
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier (Map_Port_Var (F))),
                              Used_Type => Type_Used)),
                        CTN.Declarations (Current_File));
                  elsif not AIN.Is_Data (F)
                    and then Is_Event (F)
                    and then Is_In (F)
                  then
                     if Use_ARINC653_API then
                        Append_Node_To_List
                          (Make_Literal
                             (CV.New_Pointed_Char_Value (Map_Port_Name (F))),
                           Call_Parameters);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Call_Parameters);

                        Add_Return_Variable_In_Parameters (Call_Parameters);

                        Called_Function := RF (RE_Create_Event);
                        Type_Used       := RE (RE_Event_Id_Type);
                     else
                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (Map_Port_Var (F))),
                           Call_Parameters);

                        Called_Function := RF (RE_Pok_Event_Create);
                        Type_Used       := RE (RE_Pok_Event_Id_T);
                     end if;

                     N :=
                       POK_Make_Function_Call_With_Assert
                         (Called_Function,
                          Call_Parameters);
                     Append_Node_To_List (N, Statements);

                     POK_Add_Return_Assertion (Statements);

                     Append_Node_To_List
                       (Make_Extern_Entity_Declaration
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier (Map_Port_Var (F))),
                              Used_Type => Type_Used)),
                        CTN.Declarations (Current_File));
                  else
                     Display_Communication_Error;
                  end if;
               end if;
               F := Next_Node (F);
            end loop;
         end Setup_Ports;

         Used_Member : Node_Id;
      begin
         Setup_Ports;
         Setup_Thread (E);

         Runtime.POK_Mode;

         if Get_POK_Recovery_Errors (E) /= POK_Empty_Handled_Errors then
            declare
               POK_Errors : constant POK_Handled_Errors :=
                 Get_POK_Recovery_Errors (E);
               POK_Actions : constant POK_Handled_Actions :=
                 Get_POK_Recovery_Actions (E);
               POK_Error : Node_Id;
               --  Corresponding error code in the POK runtime.

               Error : Supported_POK_Error;
               --  Error code from the model.

               Action : Supported_POK_Action;
               --  Error code from the model.

               Comment : Node_Id;

               Thread_Switch_Alternatives : constant List_Id :=
                 New_List (CTN.K_Alternatives_List);
            begin
               Use_Error_Handling := True;

               for Error_Id in POK_Errors'Range loop
                  --  Here, for each error, we take its associated
                  --  actions and call the right function to recover
                  --  the error.

                  Error := POK_Errors (Error_Id);
                  --  This array correspond to the property
                  --  POK_Recovery_Errors.

                  Action := POK_Actions (Error_Id);
                  --  This array correspond to the property
                  --  POK_Recovery_Actions.

                  POK_Error := Map_POK_Error (Error);

                  if POK_Error /= No_Node then
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id (POK_Error),
                           Make_List_Id
                             (Map_POK_Action (Action, Thread_Id, POK_Error))),
                        Thread_Switch_Alternatives);
                  end if;
               end loop;

               if Use_ARINC653_API then
                  Used_Member := RE (RE_Error_Code);
               else
                  Used_Member := Make_Defining_Identifier (MN (M_Error_Kind));
               end if;

               Append_Node_To_List
                 (Make_Switch_Alternative
                    (Make_List_Id
                       (Make_Literal (New_Int_Value (Thread_Id, 1, 10))),
                     Make_List_Id
                       (Make_Switch_Statement
                          (Make_Member_Designator
                             (Used_Member,
                              Make_Defining_Identifier (VN (V_Error_Status))),
                           Thread_Switch_Alternatives))),
                  Error_Switch_Alternatives);

               --  Here, we make a switch statement that is added
               --  to a global switch that choose the thread.

               Comment :=
                 Message_Comment
                   ("Here, we declare the error handling mecanisms " &
                    "for the task");
               Append_Node_To_List (Comment, Error_Switch_Alternatives);
            end;
         end if;

         if Get_ARINC653_HM_Errors (E) /= ARINC653_Empty_Errors then
            declare
               Errors : constant ARINC653_Errors := Get_ARINC653_HM_Errors (E);

               Actions : constant ARINC653_Actions :=
                 Get_ARINC653_HM_Actions (E);

               Error : Supported_ARINC653_Error;
               --  The error code of the model.

               Action : Supported_ARINC653_Action;
               --  The action of the model.

               POK_Error : Node_Id;
               --  Corresponding error code in the POK runtime.

               Comment : Node_Id;

               Thread_Switch_Alternatives : constant List_Id :=
                 New_List (CTN.K_Alternatives_List);
            begin
               Use_Error_Handling := True;

               for Error_Id in Errors'Range loop
                  --  Here, for each error, we take its associated
                  --  actions and call the right function to recover
                  --  the error.

                  Error := Errors (Error_Id);

                  Action := Actions (Error_Id);

                  POK_Error := Map_ARINC653_Error (Error);

                  if POK_Error /= No_Node then
                     Append_Node_To_List
                       (Make_Switch_Alternative
                          (Make_List_Id (POK_Error),
                           Make_List_Id
                             (Map_POK_Action (Action, Thread_Id, POK_Error))),
                        Thread_Switch_Alternatives);
                  end if;
               end loop;

               if Use_ARINC653_API then
                  Used_Member := RE (RE_Error_Code);
               else
                  Used_Member := Make_Defining_Identifier (MN (M_Error_Kind));
               end if;

               Append_Node_To_List
                 (Make_Switch_Alternative
                    (Make_List_Id
                       (Make_Literal (New_Int_Value (Thread_Id, 1, 10))),
                     Make_List_Id
                       (Make_Switch_Statement
                          (Make_Member_Designator
                             (Used_Member,
                              Make_Defining_Identifier (VN (V_Error_Status))),
                           Thread_Switch_Alternatives))),
                  Error_Switch_Alternatives);

               --  Here, we make a switch statement that is added
               --  to a global switch that choose the thread.

               Comment :=
                 Message_Comment
                   ("Here, we declare the error handling mecanisms " &
                    "for the task");
               Append_Node_To_List (Comment, Error_Switch_Alternatives);
            end;
         end if;

         Runtime.Normal_Mode;

         Thread_Id := Thread_Id + 1;
      end Visit_Thread_Instance;

   end Source_File;

end Ocarina.Backends.POK_C.Main;
