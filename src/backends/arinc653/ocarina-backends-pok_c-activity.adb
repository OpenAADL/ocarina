------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--      O C A R I N A . B A C K E N D S . P O K _ C . A C T I V I T Y       --
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
with Ocarina.Backends.Utils;
with Ocarina.ME_AADL.AADL_Instances.Entities;
with Ocarina.Backends.Properties;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.POK_C.Runtime;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.Messages;

with Ocarina.Instances.Queries;

package body Ocarina.Backends.POK_C.Activity is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Utils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.POK_C.Runtime;
   use Ocarina.Backends.Messages;

   use Ocarina.Instances.Queries;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package CV renames Ocarina.Backends.C_Values;

   Current_Device : Node_Id := No_Node;

   ------------
   -- Header --
   ------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
        (E            : Node_Id;
         Real_Process : Boolean := True);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      function Task_Job_Spec (E : Node_Id) return Node_Id;

      -------------------
      -- Task_Job_Spec --
      -------------------

      function Task_Job_Spec (E : Node_Id) return Node_Id is
         N : Node_Id;
         S : constant Node_Id := Parent_Subcomponent (E);
      begin
         N :=
           Make_Function_Specification
             (Defining_Identifier => Map_Task_Job_Identifier (S),
              Parameters          => No_List,
              Return_Type => CTU.Make_Pointer_Type (New_Node (CTN.K_Void)));
         return N;
      end Task_Job_Spec;

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

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Device =>
               Visit_Device_Instance (E);

            when others =>
               null;
         end case;

      end Visit_Component_Instance;

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
         Push_Entity (P);
         Push_Entity (U);

         Register_Kernel_Unit (U);

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

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
        (E            : Node_Id;
         Real_Process : Boolean := True)
      is
         U : Node_Id;
         P : Node_Id;
         S : Node_Id;
      begin
         if Real_Process then
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);

            Push_Entity (P);
            Push_Entity (U);
         end if;

         Set_Activity_Header;

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

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
      begin
         Push_Entity (P);
         Push_Entity (U);

         Implementation := Get_Classifier_Property (E, "implemented_as");

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  if Get_Category_Of_Component (S) = CC_Process then
                     Visit_Process_Instance
                       (Corresponding_Instance (S),
                        False);
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;

         end if;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Device_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         P : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
      begin
         case P is
            when Thread_Periodic =>

               N :=
                 Message_Comment
                   ("Periodic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               --  Create the spec of the parameterless subprogram
               --  that executes the thread job.

               N := Task_Job_Spec (E);
               Append_Node_To_List (N, CTN.Declarations (Current_File));
               Bind_AADL_To_Job (Identifier (S), N);

            when Thread_Sporadic =>
               N :=
                 Message_Comment
                   ("Sporadic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               N := Task_Job_Spec (E);
               Append_Node_To_List (N, CTN.Declarations (Current_File));
               Bind_AADL_To_Job (Identifier (S), N);

            when others =>
               Display_Error ("unknown thread type", Fatal => True);
         end case;
      end Visit_Thread_Instance;

   end Header_File;

   ------------
   -- Source --
   ------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
        (E            : Node_Id;
         Real_Process : Boolean := True);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      function Get_Timeout_Value (E : Node_Id) return Time_Type;

      function Task_Job_Body (E : Node_Id) return Node_Id;
      --  Create the parameterless subprogram body that does the
      --  thread's job.

      -----------------------
      -- Get_Timeout_Value --
      -----------------------

      function Get_Timeout_Value (E : Node_Id) return Time_Type is
      begin
         if Get_Compute_Deadline (E) /= Null_Time then
            return Get_Compute_Deadline (E);
         elsif Get_ARINC653_Timeout (E) /= Null_Time then
            return Get_ARINC653_Timeout (E);
         else
            return Null_Time;
         end if;
      end Get_Timeout_Value;

      -------------------
      -- Task_Job_Body --
      -------------------

      function Task_Job_Body (E : Node_Id) return Node_Id is
         S    : constant Node_Id := Parent_Subcomponent (E);
         Spec : constant Node_Id :=
           CTN.Job_Node (Backend_Node (Identifier (S)));
         Declarations : constant List_Id := New_List (CTN.K_Declaration_List);
         Statements   : constant List_Id := New_List (CTN.K_Statement_List);
         WStatements  : constant List_Id := New_List (CTN.K_Statement_List);
         P            : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         Call_Parameters : List_Id;
         N               : Node_Id;
         Call_Seq        : Node_Id;
         Spg_Call        : Node_Id;
         Spg             : Node_Id;
         Called_Function : Node_Id;
         Type_Used       : Node_Id;

         procedure Make_Init;
         procedure Make_Call_Sequence;
         procedure Make_Send_Out;
         procedure Make_End_Period;
         procedure Make_Receive_In;
         procedure Make_Potential_Assignments;

         ---------------
         -- Make_Init --
         ---------------

         procedure Make_Init is
            Entrypoint : constant Node_Id :=
              Get_Thread_Initialize_Entrypoint (E);
         begin
            if Entrypoint /= No_Node then
               Append_Node_To_List
                 (Make_Call_Profile (Map_C_Subprogram_Identifier (Entrypoint)),
                  Statements);
            end if;
         end Make_Init;

         ------------------------
         -- Make_Call_Sequence --
         ------------------------

         procedure Make_Call_Sequence is
            Call_Seq : constant Node_Id := First_Node (Calls (E));
         begin
            if not Has_Modes (E) or else AINU.Length (Calls (E)) = 1 then
               --  If the thread has no modes, then it should has one
               --  unique call sequence, handle it.

               CTU.Handle_Call_Sequence
                 (S,
                  Call_Seq,
                  Declarations,
                  WStatements);
            else
               N := Message_Comment ("not implemented yet");
               Append_Node_To_List (N, WStatements);
            end if;
         end Make_Call_Sequence;

         ---------------------
         -- Make_Receive_In --
         ---------------------

         procedure Make_Receive_In is
            F                  : Node_Id;
            Function_Call      : Node_Id;
            Destination_Port   : Node_Id;
            Transport_Type     : Node_Id := No_Node;
            Compute_Entrypoint : Node_Id;
            Virtual_Bus_Size   : Node_Id := No_Node;
            Virtual_Bus_Data   : Node_Id := No_Node;
         begin
            N := Message_Comment ("Get the IN ports values");
            Append_Node_To_List (N, WStatements);

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
                  Transport_Type   := No_Node;
                  Destination_Port := No_Node;

                  if Is_Data (F) then

                     if Current_Device /= No_Node then
                        Destination_Port :=
                          Get_Corresponding_Port_In_Component (F);

                        if Destination_Port /= No_Node
                          and then Is_Using_Virtual_Bus
                            (Get_Port_By_Name
                               (Destination_Port,
                                Current_Device))
                        then
                           Transport_Type :=
                             Get_Inter_Partition_Port_Type
                               (Get_Port_By_Name
                                  (Destination_Port,
                                   Current_Device));
                        end if;

                        if Transport_Type = No_Node
                          and then Is_Using_Virtual_Bus
                            (Get_Port_By_Name (F, Current_Device))
                        then
                           Transport_Type :=
                             Get_Inter_Partition_Port_Type
                               (Get_Port_By_Name (F, Current_Device));
                        end if;
                     else
                        Transport_Type :=
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (F));
                     end if;

                     if Transport_Type = No_Node then
                        Transport_Type :=
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (F));
                     end if;

                     Append_Node_To_List
                       (Make_Variable_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (Map_Port_Data (F, Current_Device)),
                           Used_Type => Transport_Type),
                        CTN.Declarations (Current_File));
                  end if;

                  Call_Parameters := New_List (CTN.K_Parameter_List);

                  if AIN.Is_Data (F) and then not AIN.Is_Event (F) then
                     if
                       (Get_Connection_Pattern (F) = Inter_Process
                        and then not Is_Virtual
                          (Get_Port_By_Name (F, Current_Device)))
                       or else
                       (Get_Port_By_Name (F, Current_Device) /= No_Node
                        and then not Is_Pure_Device_Port
                          (Get_Port_By_Name (F, Current_Device)))
                     then

                        if Current_Device /= No_Node
                          and then Is_Using_Virtual_Bus
                            (Get_Port_By_Name (F, Current_Device))
                        then
                           Add_Include (RH (RH_Protocols));
                        end if;
                        --  Check if we are in a device and if the port
                        --  of the device can transport data that was
                        --  manipulated by a virtual bus.

                        if Use_ARINC653_API then
                           Called_Function := RE (RE_Read_Sampling_Message);
                           Type_Used       := RE (RE_Sampling_Port_Id_Type);
                        else
                           Called_Function := RE (RE_Pok_Port_Sampling_Read);
                           Type_Used       := RE (RE_Uint8_T);
                        end if;

                        Function_Call :=
                          CTU.POK_Make_Function_Call_With_Assert
                            (Called_Function,
                             Call_Parameters);

                        Append_Node_To_List
                          (Make_Extern_Entity_Declaration
                             (Make_Variable_Declaration
                                (Defining_Identifier =>
                                   (Make_Defining_Identifier
                                      (Map_Port_Var (F, Current_Device))),
                                 Used_Type => Type_Used)),
                           CTN.Declarations (Current_File));

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Validity_Type);
                        else
                           Type_Used := RE (RE_Bool_T);
                        end if;

                        Append_Node_To_List
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Valid (F, Current_Device))),
                              Used_Type => Type_Used),
                           Statements);

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Message_Size_Type);
                        else
                           Type_Used := RE (RE_Pok_Port_Size_T);
                        end if;

                        Append_Node_To_List
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Length (F, Current_Device))),
                              Used_Type => Type_Used),
                           Statements);

                        Append_Node_To_List (Function_Call, WStatements);

                        POK_Add_Return_Assertion
                          (WStatements,
                           RE (RE_Pok_Errno_Empty));

                        --  Call virtual bus hierarchy

                        if AIN.First_Node (AIN.Sources (F)) /= No_Node then
                           Map_Virtual_Bus_Calls
                             (AIN.Item (AIN.First_Node (AIN.Sources (F))),
                              CTN.Declarations (Current_File),
                              WStatements,
                              Receiving,
                              Virtual_Bus_Data,
                              Virtual_Bus_Size,
                              Current_Device);
                        end if;

                        Append_Node_To_List
                          (Make_Defining_Identifier
                             (Map_Port_Var (F, Current_Device)),
                           Call_Parameters);

                        if Virtual_Bus_Data /= No_Node then
                           Append_Node_To_List
                             (Make_Variable_Address (Virtual_Bus_Data),
                              Call_Parameters);
                        else
                           if Get_Data_Representation
                               (Corresponding_Instance (F)) =
                             Data_Array
                           then
                              Append_Node_To_List
                                (Make_Defining_Identifier
                                   (Map_Port_Data (F, Current_Device)),
                                 Call_Parameters);
                           else
                              Append_Node_To_List
                                (Make_Variable_Address
                                   (Make_Defining_Identifier
                                      (Map_Port_Data (F, Current_Device))),
                                 Call_Parameters);
                           end if;
                        end if;

                        if Virtual_Bus_Size /= No_Node then
                           Append_Node_To_List
                             (Make_Variable_Address (Virtual_Bus_Size),
                              Call_Parameters);
                        else
                           Append_Node_To_List
                             (Make_Variable_Address
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Length (F, Current_Device))),
                              Call_Parameters);
                        end if;

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier
                                (Map_Port_Var_Valid (F, Current_Device))),
                           Call_Parameters);

                        if Use_ARINC653_API then
                           Add_Return_Variable_In_Parameters (Call_Parameters);
                        end if;

                     elsif Get_Connection_Pattern (F) = Intra_Process then

                        Append_Node_To_List
                          (Make_Defining_Identifier
                             (Map_Port_Var (F, Current_Device)),
                           Call_Parameters);

                        if Get_Timeout_Value (F) /= Null_Time then
                           if Use_ARINC653_API then
                              N :=
                                Map_Time_To_Millisecond
                                  (Get_Timeout_Value (F));
                           else
                              N := Map_Time (Get_Timeout_Value (F));
                           end if;
                        else
                           N := CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                        end if;
                        Append_Node_To_List (N, Call_Parameters);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier
                                (Map_Port_Data (F, Current_Device))),
                           Call_Parameters);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier
                                (Map_Port_Var_Length (F, Current_Device))),
                           Call_Parameters);

                        if Use_ARINC653_API then
                           Add_Return_Variable_In_Parameters (Call_Parameters);
                           Called_Function := RF (RE_Read_Blackboard);
                        else
                           Called_Function := RF (RE_Pok_Blackboard_Read);
                        end if;

                        Function_Call :=
                          CTU.POK_Make_Function_Call_With_Assert
                            (Called_Function,
                             Call_Parameters);

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Message_Size_Type);
                        else
                           Type_Used := RE (RE_Pok_Port_Size_T);
                        end if;

                        Append_Node_To_List
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Length (F, Current_Device))),
                              Used_Type => Type_Used),
                           Statements);

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Blackboard_Id_Type);
                        else
                           Type_Used := RE (RE_Uint8_T);
                        end if;

                        Append_Node_To_List
                          (Make_Extern_Entity_Declaration
                             (Make_Variable_Declaration
                                (Defining_Identifier =>
                                   (Make_Defining_Identifier
                                      (Map_Port_Var (F, Current_Device))),
                                 Used_Type => Type_Used)),
                           CTN.Declarations (Current_File));

                        Append_Node_To_List (Function_Call, WStatements);

                        POK_Add_Return_Assertion
                          (WStatements,
                           RE (RE_Pok_Errno_Empty));
                     end if;

                  elsif AIN.Is_Data (F) and then AIN.Is_Event (F) then

                     if Get_Connection_Pattern (F) = Inter_Process then
                        if AIN.First_Node (AIN.Sources (F)) /= No_Node then
                           Map_Virtual_Bus_Calls
                             (AIN.Item (AIN.First_Node (AIN.Sources (F))),
                              CTN.Declarations (Current_File),
                              WStatements,
                              Receiving,
                              Virtual_Bus_Data,
                              Virtual_Bus_Size,
                              Current_Device);
                        end if;

                        if Use_ARINC653_API then

                           Append_Node_To_List
                             (Make_Expression
                                (Left_Expr =>
                                   Make_Defining_Identifier
                                     (Map_Port_Var_Length (F, Current_Device)),
                                 Operator   => Op_Equal,
                                 Right_Expr =>
                                   Get_Data_Size (Corresponding_Instance (F))),
                              WStatements);

                           Append_Node_To_List
                             (Make_Defining_Identifier
                                (Map_Port_Var (F, Current_Device)),
                              Call_Parameters);

                           if Get_Timeout_Value (F) /= Null_Time then
                              if Use_ARINC653_API then
                                 N :=
                                   Map_Time_To_Millisecond
                                     (Get_Timeout_Value (F));
                              else
                                 N := Map_Time (Get_Timeout_Value (F));
                              end if;
                           else
                              N :=
                                CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                           end if;
                           Append_Node_To_List (N, Call_Parameters);

                           if Get_Data_Representation
                               (Corresponding_Instance (F)) =
                             Data_Array
                           then
                              Append_Node_To_List
                                (Make_Defining_Identifier
                                   (Map_Port_Data (F, Current_Device)),
                                 Call_Parameters);
                           else
                              Append_Node_To_List
                                (Make_Variable_Address
                                   (Make_Defining_Identifier
                                      (Map_Port_Data (F, Current_Device))),
                                 Call_Parameters);
                           end if;

                           Append_Node_To_List
                             (Make_Variable_Address
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Length (F, Current_Device))),
                              Call_Parameters);

                           Add_Return_Variable_In_Parameters (Call_Parameters);

                           Called_Function := RE (RE_Receive_Queuing_Message);

                           Type_Used := RE (RE_Queuing_Port_Id_Type);

                        else

                           Append_Node_To_List
                             (Make_Defining_Identifier
                                (Map_Port_Var (F, Current_Device)),
                              Call_Parameters);

                           if Get_Timeout_Value (F) /= Null_Time then
                              if Use_ARINC653_API then
                                 N :=
                                   Map_Time_To_Millisecond
                                     (Get_Timeout_Value (F));
                              else
                                 N := Map_Time (Get_Timeout_Value (F));
                              end if;
                           else
                              N :=
                                CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                           end if;
                           Append_Node_To_List (N, Call_Parameters);

                           N := CTU.Get_Data_Size (Corresponding_Instance (F));
                           Append_Node_To_List (N, Call_Parameters);

                           if Get_Data_Representation
                               (Corresponding_Instance (F)) =
                             Data_Array
                           then
                              Append_Node_To_List
                                (Make_Defining_Identifier
                                   (Map_Port_Data (F, Current_Device)),
                                 Call_Parameters);
                           else
                              Append_Node_To_List
                                (Make_Variable_Address
                                   (Make_Defining_Identifier
                                      (Map_Port_Data (F, Current_Device))),
                                 Call_Parameters);
                           end if;

                           Append_Node_To_List
                             (Make_Variable_Address
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Length (F, Current_Device))),
                              Call_Parameters);

                           Called_Function :=
                             RE (RE_Pok_Port_Queueing_Receive);

                           Type_Used := RE (RE_Uint8_T);

                        end if;

                        Function_Call :=
                          CTU.POK_Make_Function_Call_With_Assert
                            (Called_Function,
                             Call_Parameters);

                        Append_Node_To_List
                          (Make_Extern_Entity_Declaration
                             (Make_Variable_Declaration
                                (Defining_Identifier =>
                                   (Make_Defining_Identifier
                                      (Map_Port_Var (F, Current_Device))),
                                 Used_Type => Type_Used)),
                           CTN.Declarations (Current_File));

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Message_Size_Type);
                        else
                           Type_Used := RE (RE_Pok_Port_Size_T);
                        end if;

                        Append_Node_To_List
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Length (F, Current_Device))),
                              Used_Type => Type_Used),
                           Statements);
                     else
                        Append_Node_To_List
                          (Make_Defining_Identifier (Map_Port_Var (F)),
                           Call_Parameters);

                        if Get_Timeout_Value (F) /= Null_Time then
                           if Use_ARINC653_API then
                              N :=
                                Map_Time_To_Millisecond
                                  (Get_Timeout_Value (F));
                           else
                              N := Map_Time (Get_Timeout_Value (F));
                           end if;
                        else
                           N := CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                        end if;
                        Append_Node_To_List (N, Call_Parameters);

                        if Get_Data_Representation
                            (Corresponding_Instance (F)) =
                          Data_Array
                        then
                           Append_Node_To_List
                             (Make_Defining_Identifier (Map_Port_Data (F)),
                              Call_Parameters);
                        else
                           Append_Node_To_List
                             (Make_Variable_Address
                                (Make_Defining_Identifier (Map_Port_Data (F))),
                              Call_Parameters);
                        end if;

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier
                                (Map_Port_Var_Length (F))),
                           Call_Parameters);

                        if Use_ARINC653_API then
                           Add_Return_Variable_In_Parameters (Call_Parameters);

                           Called_Function := RE (RE_Receive_Buffer);
                        else
                           Called_Function := RE (RE_Pok_Buffer_Receive);
                        end if;

                        Function_Call :=
                          CTU.POK_Make_Function_Call_With_Assert
                            (Called_Function,
                             Call_Parameters);

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Message_Size_Type);
                        else
                           Type_Used := RE (RE_Pok_Port_Size_T);
                        end if;

                        Append_Node_To_List
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier
                                   (Map_Port_Var_Length (F))),
                              Used_Type => Type_Used),
                           Statements);

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Buffer_Id_Type);
                        else
                           Type_Used := RE (RE_Uint8_T);
                        end if;

                        Append_Node_To_List
                          (Make_Extern_Entity_Declaration
                             (Make_Variable_Declaration
                                (Defining_Identifier =>
                                   (Make_Defining_Identifier
                                      (Map_Port_Var (F))),
                                 Used_Type => Type_Used)),
                           CTN.Declarations (Current_File));
                     end if;

                     Append_Node_To_List (Function_Call, WStatements);

                     POK_Add_Return_Assertion
                       (WStatements,
                        RE (RE_Pok_Errno_Empty));

                  elsif not AIN.Is_Data (F)
                    and then AIN.Is_Event (F)
                    and then Get_Connection_Pattern (F) = Intra_Process
                  then
                     --  Now, handle events data ports across threads
                     --  located in the same partition.

                     Add_Include (RH (RH_Subprograms));

                     Compute_Entrypoint := Get_Port_Compute_Entrypoint (F);

                     if Compute_Entrypoint = No_Node then
                        Display_Error
                          ("With a pure event port, the compute_entrypoint " &
                           "property must be set",
                           Fatal => True);
                     end if;

                     Append_Node_To_List
                       (Make_Defining_Identifier (Map_Port_Var (F)),
                        Call_Parameters);

                     Append_Node_To_List
                       (CTU.Make_Literal (CV.New_Int_Value (0, 1, 10)),
                        Call_Parameters);

                     if Use_ARINC653_API then
                        Called_Function := RE (RE_Wait_Event);
                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (VN (V_Ret))),
                           Call_Parameters);

                        N :=
                          Make_Call_Profile (Called_Function, Call_Parameters);
                     else
                        Called_Function := RE (RE_Pok_Event_Wait);
                        N               :=
                          Make_Expression
                            (Left_Expr =>
                               Make_Defining_Identifier (VN (V_Ret)),
                             Operator   => Op_Equal,
                             Right_Expr =>
                               Make_Call_Profile
                                 (Called_Function,
                                  Call_Parameters));
                     end if;

                     Append_Node_To_List (N, WStatements);

                     N :=
                       Make_If_Statement
                         (Condition =>
                            Make_Expression
                              (Left_Expr =>
                                 Make_Defining_Identifier (VN (V_Ret)),
                               Operator   => Op_Equal_Equal,
                               Right_Expr => Get_Errcode_OK),
                          Statements =>
                            Make_List_Id
                              (Make_Call_Profile
                                 (Map_C_Defining_Identifier
                                    (Compute_Entrypoint),
                                  No_List)));
                     Append_Node_To_List (N, WStatements);

                     if Use_ARINC653_API then
                        Type_Used := RE (RE_Event_Id_Type);
                     else
                        Type_Used := RE (RE_Pok_Event_Id_T);
                     end if;

                     Append_Node_To_List
                       (Make_Extern_Entity_Declaration
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier (Map_Port_Var (F))),
                              Used_Type => Type_Used)),
                        CTN.Declarations (Current_File));
                  else
                     Display_Communication_Error;
                     --  Pattern not handled at this time
                  end if;

                  --  If we are in a device, then, we receive the data
                  --  that comes inside the device.

                  if Get_Connection_Pattern (F) = Inter_Process
                    and then Current_Device /= No_Node
                    and then Is_Virtual (Get_Port_By_Name (F, Current_Device))
                  then

                     Append_Node_To_List
                       (Make_Extern_Entity_Declaration
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier
                                   (Map_Port_Var (F, Current_Device))),
                              Used_Type => RE (RE_Uint8_T))),
                        CTN.Declarations (Current_File));

                     Call_Parameters := New_List (CTN.K_Parameter_List);

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_Port_Var (F, Current_Device)),
                        Call_Parameters);

                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Defining_Identifier
                             (Map_Port_Data (F, Current_Device))),
                        Call_Parameters);

                     Append_Node_To_List
                       (Get_Inter_Partition_Port_Size
                          (Get_Port_By_Name
                             (Item (First_Node (Destinations (F))),
                              Current_Device)),
                        Call_Parameters);

                     Function_Call :=
                       CTU.Make_Call_Profile
                         (Make_Defining_Identifier
                            (Map_Device_Function_Read (Current_Device)),
                          Call_Parameters);

                     Append_Node_To_List (Function_Call, WStatements);
                  end if;

               end if;

               F := Next_Node (F);
            end loop;
         end Make_Receive_In;

         --------------------------------
         -- Make_Potential_Assignments --
         --------------------------------

         procedure Make_Potential_Assignments is
            Statement : Node_Id;
            F         : Node_Id;
            D         : Node_Id;
         begin
            Statement :=
              Message_Comment
                ("Copy directly the data from IN ports to OUT ports");
            Append_Node_To_List (Statement, WStatements);

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then not Is_Out (F)
                 and then Is_Data (F)
                 and then not AINU.Is_Empty (Destinations (F))
               then
                  D := First_Node (Destinations (F));
                  while Present (D) loop
                     if Parent_Subcomponent (Parent_Component (Item (D))) =
                       Parent_Subcomponent (Parent_Component (F))
                     then

                        if Get_Data_Representation
                            (Corresponding_Instance (F)) =
                          Data_Array
                        then

                           Statement := Message_Comment ("Must handle arrays");
                        else
                           Statement :=
                             Make_Expression
                               (Make_Defining_Identifier
                                  (Map_Port_Data (Item (D), Current_Device)),
                                Op_Equal,
                                Make_Defining_Identifier
                                  (Map_Port_Data (F, Current_Device)));
                        end if;

                        Append_Node_To_List (Statement, WStatements);
                     end if;
                     D := Next_Node (D);
                  end loop;
               end if;
               F := Next_Node (F);
            end loop;
         end Make_Potential_Assignments;

         --  If the thread has ports that are directly connected (no use of a
         --  subprogram), we automatically assign OUT ports with IN ports).

         ---------------------
         -- Make_End_Period --
         ---------------------

         procedure Make_End_Period is
         begin

            Call_Parameters := New_List (CTN.K_Parameter_List);

            if Use_ARINC653_API then
               Called_Function := RF (RE_Periodic_Wait);
               Add_Return_Variable_In_Parameters (Call_Parameters);
            else
               Called_Function := RE (RE_Pok_Thread_Period);
            end if;

            Append_Node_To_List
              (CTU.Make_Call_Profile (Called_Function, Call_Parameters),
               WStatements);
         end Make_End_Period;

         -------------------
         -- Make_Send_Out --
         -------------------

         procedure Make_Send_Out is
            N                : Node_Id;
            F                : Node_Id;
            Function_Call    : Node_Id;
            Source_Port      : Node_Id;
            Transport_Type   : Node_Id := No_Node;
            Virtual_Bus_Data : Node_Id := No_Node;
            Virtual_Bus_Size : Node_Id := No_Node;
         begin
            N := Message_Comment ("Send the OUT ports");
            Append_Node_To_List (N, WStatements);

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then

                  if Is_Data (F) then
                     if Current_Device /= No_Node
                       and then not AINU.Is_Empty (AIN.Sources (F))
                     then
                        Source_Port :=
                          AIN.Item (AIN.First_Node (AIN.Sources (F)));

                        if Is_Using_Virtual_Bus
                            (Get_Port_By_Name (Source_Port, Current_Device))
                        then
                           Transport_Type :=
                             Get_Inter_Partition_Port_Type
                               (Get_Port_By_Name
                                  (Source_Port,
                                   Current_Device));
                        elsif Is_Using_Virtual_Bus
                            (Get_Port_By_Name (F, Current_Device))
                        then
                           Transport_Type :=
                             Get_Inter_Partition_Port_Type
                               (Get_Port_By_Name (F, Current_Device));
                        end if;

                        if Transport_Type = No_Node then
                           Transport_Type :=
                             Map_C_Data_Type_Designator
                               (Corresponding_Instance (F));
                        end if;
                     else
                        Transport_Type :=
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (F));
                     end if;

                     Append_Node_To_List
                       (Make_Variable_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (Map_Port_Data (F, Current_Device)),
                           Used_Type => Transport_Type),
                        CTN.Declarations (Current_File));
                  end if;

                  --  If we are in a device, then, we send the data
                  --  that comes inside the device.
                  if Get_Connection_Pattern (F) = Inter_Process
                    and then Is_Data (F)
                    and then Current_Device /= No_Node
                    and then Is_Virtual (Get_Port_By_Name (F, Current_Device))
                  then

                     Source_Port :=
                       AIN.Item (AIN.First_Node (AIN.Sources (F)));

                     Append_Node_To_List
                       (Make_Extern_Entity_Declaration
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier
                                   (Map_Port_Var (F, Current_Device))),
                              Used_Type => RE (RE_Uint8_T))),
                        CTN.Declarations (Current_File));

                     Call_Parameters := New_List (CTN.K_Parameter_List);

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_Port_Var (F, Current_Device)),
                        Call_Parameters);

                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Defining_Identifier
                             (Map_Port_Data (F, Current_Device))),
                        Call_Parameters);

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_Port_Var_Length (Source_Port, Current_Device)),
                        Call_Parameters);

                     Function_Call :=
                       CTU.Make_Call_Profile
                         (Make_Defining_Identifier
                            (Map_Device_Function_Write (Current_Device)),
                          Call_Parameters);

                     Append_Node_To_List (Function_Call, WStatements);
                  end if;

                  Call_Parameters := New_List (CTN.K_Parameter_List);

                  if AIN.Is_Data (F) and then not AIN.Is_Event (F) then
                     if Get_Connection_Pattern (F) = Inter_Process
                       and then not Is_Virtual
                         (Get_Port_By_Name (F, Current_Device))
                     then

                        Source_Port := First_Node (Destinations (F));

                        while Source_Port /= No_Node loop

                           Call_Parameters := New_List (CTN.K_Parameter_List);

                           if AIN.First_Node (AIN.Destinations (F)) /=
                             No_Node
                           then
                              Map_Virtual_Bus_Calls
                                (AIN.Item
                                   (AIN.First_Node (AIN.Destinations (F))),
                                 CTN.Declarations (Current_File),
                                 WStatements,
                                 Sending,
                                 Virtual_Bus_Data,
                                 Virtual_Bus_Size,
                                 Current_Device);
                           end if;

                           Append_Node_To_List
                             (Make_Defining_Identifier
                                (Map_Port_Var
                                   (Item (Source_Port),
                                    Current_Device)),
                              Call_Parameters);

                           if Virtual_Bus_Data = No_Node then
                              if Get_Data_Representation
                                  (Corresponding_Instance (F)) =
                                Data_Array
                              then
                                 Append_Node_To_List
                                   (Make_Defining_Identifier
                                      (Map_Port_Data (F, Current_Device)),
                                    Call_Parameters);
                              else
                                 Append_Node_To_List
                                   (Make_Variable_Address
                                      (Make_Defining_Identifier
                                         (Map_Port_Data (F, Current_Device))),
                                    Call_Parameters);
                              end if;
                           else
                              Append_Node_To_List
                                (Make_Variable_Address (Virtual_Bus_Data),
                                 Call_Parameters);
                           end if;

                           if Virtual_Bus_Size /= No_Node then
                              N := Copy_Node (Virtual_Bus_Size);
                           else
                              if Current_Device = No_Node then
                                 N := Get_Inter_Partition_Port_Size (F);
                              else
                                 N :=
                                   Get_Inter_Partition_Port_Size
                                     (Get_Port_By_Name (F, Current_Device));

                                 if Is_Using_Virtual_Bus
                                     (Get_Port_By_Name (F, Current_Device))
                                 then
                                    Add_Include (RH (RH_Protocols));
                                 end if;
                              end if;
                           end if;

                           Append_Node_To_List (N, Call_Parameters);

                           if Use_ARINC653_API then
                              Add_Return_Variable_In_Parameters
                                (Call_Parameters);
                              Called_Function :=
                                RE (RE_Write_Sampling_Message);
                              Type_Used := RE (RE_Sampling_Port_Id_Type);
                           else
                              Called_Function :=
                                RE (RE_Pok_Port_Sampling_Write);
                              Type_Used := RE (RE_Uint8_T);
                           end if;

                           N :=
                             CTU.POK_Make_Function_Call_With_Assert
                               (Called_Function,
                                Call_Parameters);

                           Append_Node_To_List
                             (Make_Extern_Entity_Declaration
                                (Make_Variable_Declaration
                                   (Defining_Identifier =>
                                      (Make_Defining_Identifier
                                         (Map_Port_Var
                                            (Item (Source_Port),
                                             Current_Device))),
                                    Used_Type => Type_Used)),
                              CTN.Declarations (Current_File));

                           Append_Node_To_List (N, WStatements);

                           POK_Add_Return_Assertion (WStatements);

                           Source_Port := Next_Node (Source_Port);
                        end loop;
                     elsif Get_Connection_Pattern (F) = Intra_Process then
                        Append_Node_To_List
                          (Make_Defining_Identifier (Map_Port_Var (F)),
                           Call_Parameters);

                        if Get_Data_Representation
                            (Corresponding_Instance (F)) =
                          Data_Array
                        then
                           Append_Node_To_List
                             (Make_Defining_Identifier (Map_Port_Data (F)),
                              Call_Parameters);
                        else
                           Append_Node_To_List
                             (Make_Variable_Address
                                (Make_Defining_Identifier (Map_Port_Data (F))),
                              Call_Parameters);
                        end if;

                        N := CTU.Get_Data_Size (Corresponding_Instance (F));
                        Append_Node_To_List (N, Call_Parameters);

                        if Use_ARINC653_API then
                           Add_Return_Variable_In_Parameters (Call_Parameters);
                           Called_Function := RF (RE_Display_Blackboard);
                        else
                           Called_Function := RF (RE_Pok_Blackboard_Display);
                        end if;

                        N :=
                          CTU.POK_Make_Function_Call_With_Assert
                            (Called_Function,
                             Call_Parameters);

                        if Use_ARINC653_API then
                           Type_Used := RE (RE_Blackboard_Id_Type);
                        else
                           Type_Used := RE (RE_Uint8_T);
                        end if;

                        Append_Node_To_List
                          (Make_Extern_Entity_Declaration
                             (Make_Variable_Declaration
                                (Defining_Identifier =>
                                   (Make_Defining_Identifier
                                      (Map_Port_Var (F))),
                                 Used_Type => Type_Used)),
                           CTN.Declarations (Current_File));

                        Append_Node_To_List (N, WStatements);

                        POK_Add_Return_Assertion (WStatements);
                     end if;

                  elsif AIN.Is_Data (F) and then AIN.Is_Event (F) then
                     if Get_Connection_Pattern (F) = Inter_Process
                       and then not Is_Virtual
                         (Get_Port_By_Name (F, Current_Device))
                     then
                        --  Call virtual bus layers
                        if AIN.First_Node (AIN.Destinations (F)) /=
                          No_Node
                        then
                           Map_Virtual_Bus_Calls
                             (AIN.Item (AIN.First_Node (AIN.Destinations (F))),
                              CTN.Declarations (Current_File),
                              WStatements,
                              Sending,
                              Virtual_Bus_Data,
                              Virtual_Bus_Size,
                              Current_Device);
                        end if;

                        Source_Port := AIN.First_Node (AIN.Destinations (F));

                        while (Source_Port /= No_Node) loop

                           Call_Parameters := New_List (CTN.K_Parameter_List);

                           Append_Node_To_List
                             (Make_Defining_Identifier
                                (Map_Port_Var (Item (Source_Port))),
                              Call_Parameters);

                           if Get_Data_Representation
                               (Corresponding_Instance (F)) =
                             Data_Array
                           then
                              Append_Node_To_List
                                (Make_Defining_Identifier (Map_Port_Data (F)),
                                 Call_Parameters);
                           else
                              Append_Node_To_List
                                (Make_Variable_Address
                                   (Make_Defining_Identifier
                                      (Map_Port_Data (F))),
                                 Call_Parameters);
                           end if;

                           N := CTU.Get_Data_Size (Corresponding_Instance (F));
                           Append_Node_To_List (N, Call_Parameters);

                           if Get_Timeout_Value (F) /= Null_Time then
                              if Use_ARINC653_API then
                                 N :=
                                   Map_Time_To_Millisecond
                                     (Get_Timeout_Value (F));
                              else
                                 N := Map_Time (Get_Timeout_Value (F));
                              end if;
                           else
                              N :=
                                CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                           end if;

                           Append_Node_To_List (N, Call_Parameters);

                           if Use_ARINC653_API then
                              Add_Return_Variable_In_Parameters
                                (Call_Parameters);

                              Called_Function := RE (RE_Send_Queuing_Message);
                              Type_Used       := RE (RE_Queuing_Port_Id_Type);
                           else
                              Called_Function :=
                                RE (RE_Pok_Port_Queueing_Send);
                              Type_Used := RE (RE_Uint8_T);
                           end if;

                           N :=
                             POK_Make_Function_Call_With_Assert
                               (Called_Function,
                                Call_Parameters);

                           Append_Node_To_List
                             (Make_Extern_Entity_Declaration
                                (Make_Variable_Declaration
                                   (Defining_Identifier =>
                                      (Make_Defining_Identifier
                                         (Map_Port_Var (Item (Source_Port)))),
                                    Used_Type => Type_Used)),
                              CTN.Declarations (Current_File));
                           Append_Node_To_List (N, WStatements);

                           POK_Add_Return_Assertion (WStatements);
                           Source_Port := Next_Node (Source_Port);
                        end loop;
                     elsif Get_Connection_Pattern (F) = Intra_Process
                       and then not Is_Virtual
                         (Get_Port_By_Name (F, Current_Device))
                     then
                        Append_Node_To_List
                          (Make_Defining_Identifier (Map_Port_Var (F)),
                           Call_Parameters);

                        Append_Node_To_List
                          (Make_Variable_Address
                             (Make_Defining_Identifier (Map_Port_Data (F))),
                           Call_Parameters);

                        N := CTU.Get_Data_Size (Corresponding_Instance (F));
                        Append_Node_To_List (N, Call_Parameters);

                        if Get_Timeout_Value (F) /= Null_Time then
                           if POK_Flavor = POK then
                              N := Map_Time (Get_Timeout_Value (F));
                           else
                              N :=
                                Map_Time_To_Millisecond
                                  (Get_Timeout_Value (F));
                           end if;
                        else
                           N := CTU.Make_Literal (CV.New_Int_Value (0, 1, 10));
                        end if;

                        Append_Node_To_List (N, Call_Parameters);

                        if Use_ARINC653_API then
                           Add_Return_Variable_In_Parameters (Call_Parameters);

                           Called_Function := RE (RE_Send_Buffer);
                           Type_Used       := RE (RE_Buffer_Id_Type);
                        else
                           Called_Function := RE (RE_Pok_Buffer_Send);
                           Type_Used       := RE (RE_Uint8_T);
                        end if;

                        N :=
                          CTU.POK_Make_Function_Call_With_Assert
                            (Called_Function,
                             Call_Parameters);

                        Append_Node_To_List
                          (Make_Extern_Entity_Declaration
                             (Make_Variable_Declaration
                                (Defining_Identifier =>
                                   (Make_Defining_Identifier
                                      (Map_Port_Var (F))),
                                 Used_Type => Type_Used)),
                           CTN.Declarations (Current_File));

                        Append_Node_To_List (N, WStatements);

                        POK_Add_Return_Assertion (WStatements);
                     end if;

                  elsif AIN.Is_Event (F)
                    and then not AIN.Is_Data (F)
                    and then Get_Connection_Pattern (F) = Intra_Process
                  then

                     if Use_ARINC653_API then
                        Type_Used       := RE (RE_Event_Id_Type);
                        Called_Function := RE (RE_Set_Event);

                        Append_Node_To_List
                          (Make_Defining_Identifier (Map_Port_Var (F)),
                           Call_Parameters);

                        Add_Return_Variable_In_Parameters (Call_Parameters);
                     else
                        Type_Used       := RE (RE_Pok_Event_Id_T);
                        Called_Function := RE (RE_Pok_Event_Signal);

                        Append_Node_To_List
                          (Make_Defining_Identifier (Map_Port_Var (F)),
                           Call_Parameters);
                     end if;

                     N :=
                       POK_Make_Function_Call_With_Assert
                         (Called_Function,
                          Call_Parameters);

                     Append_Node_To_List (N, WStatements);

                     POK_Add_Return_Assertion (WStatements);

                     Append_Node_To_List
                       (Make_Extern_Entity_Declaration
                          (Make_Variable_Declaration
                             (Defining_Identifier =>
                                (Make_Defining_Identifier (Map_Port_Var (F))),
                              Used_Type => Type_Used)),
                        CTN.Declarations (Current_File));
                  else
                     Display_Communication_Error;
                     --  Pattern not handled at this time
                  end if;
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Send_Out;
      begin
         if Use_ARINC653_API then
            Type_Used := RE (RE_Return_Code_Type);
         else
            Type_Used := RE (RE_Pok_Ret_T);
         end if;

         Append_Node_To_List
           (Make_Variable_Declaration
              (Defining_Identifier => (Make_Defining_Identifier (VN (V_Ret))),
               Used_Type           => Type_Used),
            Statements);

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Spg := Corresponding_Instance (Spg_Call);

                     if Get_Subprogram_Kind (Spg) = Subprogram_Lustre then

                        Add_Include
                          (Make_Include_Clause (Map_Source_Name (Spg)),
                           Preserve_Case => True);

                        N :=
                          Make_Variable_Declaration
                            (Defining_Identifier =>
                               Map_Lustre_Context_Name (Spg),
                             Used_Type =>
                               Make_Pointer_Type
                                 (Map_Lustre_Context_Type (Spg)));

                        Append_Node_To_List
                          (N,
                           CTN.Declarations (Current_File));

                        N :=
                          Make_Expression
                            (Left_Expr  => Map_Lustre_Context_Name (Spg),
                             Operator   => Op_Equal,
                             Right_Expr =>
                               Make_Call_Profile
                                 (Map_Lustre_Context_Init (Spg),
                                  Make_List_Id
                                    (Make_Literal
                                       (C_Values.New_Int_Value (0, 1, 10)))));
                        Append_Node_To_List (N, Statements);

                     elsif Get_Subprogram_Kind (Spg) = Subprogram_Esterel then
                        N :=
                          Make_Call_Profile (Map_Esterel_Reset_Function (Spg));
                        Append_Node_To_List (N, Statements);
                     end if;

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;

         case P is
            when Thread_Periodic =>
               N :=
                 Message_Comment
                   ("Periodic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

            when Thread_Sporadic =>
               N :=
                 Message_Comment
                   ("Sporadic task : " &
                    Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

            when others =>
               Display_Error ("unknown type of thread", Fatal => False);
               null;
         end case;

--  At this time, we don't use Check_Thread_Consistency, it does not
--  fir with our requirements.
--         Check_Thread_Consistency (E);

         Make_Init;

         if Has_In_Ports (E) then
            Make_Receive_In;
         end if;

         if not AINU.Is_Empty (Calls (E)) then
            Make_Call_Sequence;
         end if;

         if Has_In_Ports (E) and then Has_Out_Ports (E) then
            Make_Potential_Assignments;
         end if;

         if Has_Out_Ports (E) then
            Make_Send_Out;
         end if;

         Make_End_Period;

         declare
            Init_Entrypoint : constant Name_Id :=
              Get_Thread_Initialize_Entrypoint (E);
            Parameter_List : constant List_Id := New_List (CTN.K_List_Id);
         begin
            if Init_Entrypoint /= No_Name then
               N :=
                 Make_Extern_Entity_Declaration
                   (Make_Function_Specification
                      (Make_Defining_Identifier (Init_Entrypoint),
                       Parameters  => Parameter_List, --  XXX
                       Return_Type => New_Node (CTN.K_Void)));
               Append_Node_To_List (N, CTN.Declarations (Current_File));
               N :=
                 CTU.Make_Call_Profile
                   (Make_Defining_Identifier (Init_Entrypoint),
                    No_List);
               Append_Node_To_List (N, Statements);
            end if;
         end;

         --  Make the while (1){} and add all statements

         N :=
           Make_While_Statement
             (Make_Literal (CV.New_Int_Value (1, 0, 10)),
              WStatements);
         Append_Node_To_List (N, Statements);

         N := Make_Function_Implementation (Spec, Declarations, Statements);
         return N;
      end Task_Job_Body;

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

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Device =>
               Visit_Device_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
        (E            : Node_Id;
         Real_Process : Boolean := True)
      is
         U         : Node_Id;
         P         : Node_Id;
         S         : Node_Id;
         Statement : Node_Id;
      begin
         if Real_Process then
            U :=
              CTN.Distributed_Application_Unit
                (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);
            Push_Entity (P);
            Push_Entity (U);
         end if;

         CTU.Set_Activity_Source;

         --  Visit all the subcomponents of the process.

         if not AINU.Is_Empty (Subcomponents (E)) then

            --  First pass to treat all shared data
            --  access. This is especially important
            --  since the extern declaration must be
            --  before the statements that use them.
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               if AINU.Is_Data (Corresponding_Instance (S)) then
                  --  Automatically use the types.h header if we use
                  --  protected data.

                  Add_Include (RH (RH_Types));

                  Statement :=
                    Make_Extern_Entity_Declaration
                      (Make_Variable_Declaration
                         (Map_C_Defining_Identifier (S),
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (S))));

                  Append_Node_To_List
                    (Statement,
                     CTN.Declarations (Current_File));
               end if;

               S := Next_Node (S);
            end loop;

            --  Second pass to handle all threads of the current
            --  partition.

            S := First_Node (Subcomponents (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

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
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         N : Node_Id;
      begin
         N := Task_Job_Body (E);
         Append_Node_To_List (N, CTN.Declarations (Current_File));
      end Visit_Thread_Instance;

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
         Push_Entity (P);
         Push_Entity (U);

         Register_Kernel_Unit (U);

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
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P              : constant Node_Id := CTN.Entity (U);
         Implementation : Node_Id;
         S              : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);

         Current_Device := E;

         Implementation := Get_Classifier_Property (E, "implemented_as");

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  if Get_Category_Of_Component (S) = CC_Process then
                     Visit_Process_Instance
                       (Corresponding_Instance (S),
                        False);
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;

         end if;

         Current_Device := No_Node;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Device_Instance;
   end Source_File;

end Ocarina.Backends.POK_C.Activity;
