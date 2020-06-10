------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--    O C A R I N A . B A C K E N D S . P O _ H I _ C . A C T I V I T Y     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2020 ESA & ISAE.      --
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
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Common.BA;
with Ocarina.ME_AADL_BA.BA_Tree.Nutils;
with Ocarina.ME_AADL_BA.BA_Tree.Nodes;

package body Ocarina.Backends.PO_HI_C.Activity is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Utils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Common.BA;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package CV renames Ocarina.Backends.C_Values;
   package BATN renames Ocarina.ME_AADL_BA.BA_Tree.Nodes;
   package BANu renames Ocarina.ME_AADL_BA.BA_Tree.Nutils;

   Current_Device : Node_Id := No_Node;

   ------------
   -- Header --
   ------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      function Task_Job_Spec (E : Node_Id) return Node_Id;
      function Task_Deliver_Spec (E : Node_Id) return Node_Id;

      Have_Main_Deliver : Boolean := False;

      -------------------
      -- Task_Job_Spec --
      -------------------

      function Task_Job_Spec (E : Node_Id) return Node_Id is
         N : Node_Id;
         S : constant Node_Id := Parent_Subcomponent (E);
      begin
         N :=
           Make_Function_Specification
             (Defining_Identifier =>
                Map_Task_Job_Identifier (S, Current_Device),
              Parameters  => No_List,
              Return_Type => CTU.Make_Pointer_Type (New_Node (CTN.K_Void)));
         return N;
      end Task_Job_Spec;

      -----------------------
      -- Task_Deliver_Spec --
      -----------------------

      function Task_Deliver_Spec (E : Node_Id) return Node_Id is
         N          : Node_Id;
         S          : constant Node_Id := Parent_Subcomponent (E);
         Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
      begin
         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Request)),
              Parameter_Type      => Make_Pointer_Type (RE (RE_Request_T)));
         Append_Node_To_List (N, Parameters);

         N :=
           Make_Function_Specification
             (Defining_Identifier => Map_Task_Deliver_Identifier (S),
              Parameters          => Parameters,
              Return_Type         => New_Node (CTN.K_Void));

         Have_Main_Deliver := True;

         return N;
      end Task_Deliver_Spec;

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

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P          : constant Node_Id := CTN.Entity (U);
         Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
         S          : Node_Id;
         N          : Node_Id;
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Activity_Header (U);

         Have_Main_Deliver := False;

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         if Have_Main_Deliver then
            N :=
              Make_Parameter_Specification
                (Defining_Identifier =>
                   Make_Defining_Identifier (PN (P_Request)),
                 Parameter_Type => Make_Pointer_Type (RE (RE_Request_T)));
            Append_Node_To_List (N, Parameters);

            N :=
              Make_Function_Specification
                (Defining_Identifier => RE (RE_Main_Deliver),
                 Parameters          => Parameters,
                 Return_Type         => New_Node (CTN.K_Void));

            Append_Node_To_List (N, CTN.Declarations (Current_File));

            Bind_AADL_To_Job (Identifier (Parent_Subcomponent (E)), N);
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            S := First_Node (Subcomponents (The_System));
            while Present (S) loop
               if AAU.Is_Device (Corresponding_Instance (S))
                 and then
                 Get_Bound_Processor (Corresponding_Instance (S)) =
                 Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : constant Node_Id := Get_Implementation (E);
         S              : Node_Id;
      begin
         Current_Device := E;
         if Implementation /= No_Node then
            if not AAU.Is_Empty (AAN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  Visit_Component_Instance (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end if;
         Current_Device := No_Node;
      end Visit_Device_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (C_Root);

         --  Visit all the subcomponents of the system

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  C_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
      begin

         if Has_Ports (E) then
            N := Task_Deliver_Spec (E);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
            Bind_AADL_To_Global_Port (Identifier (S), N);

            N := Task_Deliver_Spec (E);

         end if;

         --  Create the spec of the parameterless subprogram
         --  that executes the thread job.

         N := Task_Job_Spec (E);
         Append_Node_To_List (N, CTN.Declarations (Current_File));
         Bind_AADL_To_Job (Identifier (S), N);

      end Visit_Thread_Instance;

   end Header_File;

   ------------
   -- Source --
   ------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);

      function Task_Job_Body (E : Node_Id) return Node_Id;
      --  Create the parameterless subprogram body that does the
      --  thread's job.

      Main_Deliver_Alternatives : List_Id;

      Current_Device : Node_Id := No_Node;

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
         Impl_Kind : constant Supported_Thread_Implementation :=
           Get_Thread_Implementation_Kind (E);
         Call_Parameters  : List_Id;
         Call_Parameters_Of_BA_Initialization_Function : List_Id;
         N, N1           : Node_Id;

         procedure Make_Wait_Event;
         procedure Make_Wait_Specific_Events;
         procedure Make_Call_Sequence;
         procedure Make_Set_Out_Ports;
         procedure Make_Send_Out_Ports (WStats : List_Id);
         procedure Make_Task_Blocking (WStats : List_Id);
         procedure Make_Fetch_In_Ports;
         procedure Make_Wait_Offset;
         procedure Make_Thread_Compute_Entrypoint;
         procedure Make_Thread_Behavior_Specification;
         procedure Make_Ports_Compute_Entrypoint;
         procedure Make_Activate_Entrypoint;
         function Make_Get_Valid_Value (F : Node_Id) return Node_Id;

         ------------------------------
         -- Make_Activate_Entrypoint --
         ------------------------------

         procedure Make_Activate_Entrypoint is
            Entrypoint : constant Node_Id :=
              Get_Thread_Activate_Entrypoint (E);

            Parameter_List : constant List_Id := New_List (CTN.K_List_Id);
         begin
            if Entrypoint /= No_Node then
               Append_Node_To_List
                 (Make_Call_Profile (Map_C_Subprogram_Identifier (Entrypoint)),
                  Statements);

               Append_Node_To_List
                 (Make_Extern_Entity_Declaration
                    (Make_Function_Specification
                       (Map_C_Subprogram_Identifier (Entrypoint),
                        Parameters  => Parameter_List, --  XXX
                        Return_Type => New_Node (CTN.K_Void))),
                  CTN.Declarations (Current_File));
            end if;
         end Make_Activate_Entrypoint;

         --------------------------
         -- Make_Get_Valid_Value --
         --------------------------

         function Make_Get_Valid_Value (F : Node_Id) return Node_Id is
            Then_Statements : constant List_Id :=
              New_List (CTN.K_Statement_List);
            Condition : Node_Id;
            N         : Node_Id;
         begin
            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier
                     (Map_C_Variable_Name (F, Port_Request => True)),
                 Used_Type => RE (RE_Request_T),
                 Is_Static => True);
            Append_Node_To_List (N, Declarations);

            Call_Parameters := New_List (CTN.K_Parameter_List);
            Append_Node_To_List
              (Make_Defining_Identifier (Map_C_Enumerator_Name (S)),
               Call_Parameters);
            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Enumerator_Name (F, Local_Port => True)),
               Call_Parameters);

            if not Get_Wait_For_All_Events (E) then
               Condition :=
                 Make_Call_Profile (RE (RE_Gqueue_Get_Count), Call_Parameters);
            else
               Condition := Make_Literal (CV.New_Int_Value (1, 0, 10));
            end if;

            --  If we wait for all events ports, the condition is always true

            Call_Parameters := New_List (CTN.K_Parameter_List);

            Append_Node_To_List
              (Make_Defining_Identifier (Map_C_Enumerator_Name (S)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Enumerator_Name (F, Local_Port => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Variable_Address
                 (Make_Defining_Identifier
                    (Map_C_Variable_Name (F, Port_Request => True))),
               Call_Parameters);

            N := Make_Call_Profile (RE (RE_Gqueue_Get_Value), Call_Parameters);

            Append_Node_To_List (N, Then_Statements);

            --  Add the call to next_value

            Call_Parameters := New_List (CTN.K_Parameter_List);

            Append_Node_To_List
              (Make_Defining_Identifier (Map_C_Enumerator_Name (S)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Enumerator_Name (F, Local_Port => True)),
               Call_Parameters);

            N :=
              Make_Call_Profile (RE (RE_Gqueue_Next_Value), Call_Parameters);

            Append_Node_To_List (N, Then_Statements);

            return Make_If_Statement (Condition, Then_Statements);
         end Make_Get_Valid_Value;

         -------------------------------
         -- Make_Wait_Specific_Events --
         -------------------------------

         procedure Make_Wait_Specific_Events is
         begin

            --  __po_hi_int32_t index_transition_to_execute;

            N :=
              Make_Variable_Declaration
                (Defining_Identifier => Make_Defining_Identifier
                   (VN (V_Index_Transition_To_Execute)),
                 Used_Type           => RE (RE_Int32_T));
            Append_Node_To_List (N, Declarations);

            --  Make the call to
            --  __po_hi_gqueue_wait_for_specific_incoming_events

            Call_Parameters := New_List (CTN.K_Parameter_List);
            N := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
            Append_Node_To_List (N, Call_Parameters);

            N := Make_Defining_Identifier (VN (V_Next_Complete_State));
            Append_Node_To_List (N, Call_Parameters);

            N := Make_Variable_Address
              (Make_Defining_Identifier (VN (V_Index_Transition_To_Execute)));
            Append_Node_To_List (N, Call_Parameters);

            N :=
              CTU.Make_Call_Profile
                (RE (RE_Gqueue_Wait_For_Specific_Incoming_Events),
                 Call_Parameters);
            Append_Node_To_List (N, WStatements);

            --  Make the call to __po_hi_compute_next_period

            Call_Parameters := New_List (CTN.K_Parameter_List);
            N               :=
              Make_Defining_Identifier
                (Map_C_Enumerator_Name (S, Custom_Parent => Current_Device));

            Append_Node_To_List (N, Call_Parameters);

            N :=
              CTU.Make_Call_Profile
                (RE (RE_Compute_Next_Period),
                 Call_Parameters);
            Append_Node_To_List (N, WStatements);
         end Make_Wait_Specific_Events;

         ---------------------
         -- Make_Wait_Event --
         ---------------------

         procedure Make_Wait_Event is
         begin

            N :=
              Make_Variable_Declaration
                (Defining_Identifier => Make_Defining_Identifier
                   (VN (V_Port)),
                 Used_Type           => RE (RE_Local_Port_T));
            Append_Node_To_List (N, Declarations);

            --  Make the call to __po_hi_gqueue_wait_for_incoming_event

            Call_Parameters := New_List (CTN.K_Parameter_List);
            N := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
            Append_Node_To_List (N, Call_Parameters);

            N :=
              Make_Variable_Address
                (Make_Defining_Identifier
                   (VN (V_Port)));
            Append_Node_To_List (N, Call_Parameters);

            N :=
              CTU.Make_Call_Profile
                (RE (RE_Gqueue_Wait_For_Incoming_Event),
                 Call_Parameters);
            Append_Node_To_List (N, WStatements);

            --  Make the call to __po_hi_compute_next_period

            Call_Parameters := New_List (CTN.K_Parameter_List);
            N               :=
              Make_Defining_Identifier
                (Map_C_Enumerator_Name (S, Custom_Parent => Current_Device));

            Append_Node_To_List (N, Call_Parameters);

            N :=
              CTU.Make_Call_Profile
                (RE (RE_Compute_Next_Period),
                 Call_Parameters);
            Append_Node_To_List (N, WStatements);
         end Make_Wait_Event;

         ------------------------
         -- Make_Task_Blocking --
         ------------------------

         procedure Make_Task_Blocking (WStats : List_Id) is
         begin
            --  Make the __po_hi_wait_for_next_period call

            Call_Parameters := New_List (CTN.K_Parameter_List);
            if Current_Device /= No_Node then
               N :=
                 Make_Defining_Identifier
                   (Map_C_Enumerator_Name
                      (S,
                       Custom_Parent => Current_Device));
            else
               N := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
            end if;
            Append_Node_To_List (N, Call_Parameters);

            N :=
              CTU.Make_Call_Profile
                (RE (RE_Wait_For_Next_Period),
                 Call_Parameters);
            Append_Node_To_List (N, WStats);
         end Make_Task_Blocking;

         ------------------------
         -- Make_Set_Out_Ports --
         ------------------------

         procedure Make_Set_Out_Ports is
            N          : Node_Id;
            L          : Node_Id;
            R          : Node_Id;
            F          : Node_Id;
            T          : Node_Id;
            Z          : Node_Id;
            Parameters : List_Id;
         begin
            N := Message_Comment ("Set the OUT port values");
            Append_Node_To_List (N, WStatements);

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Is_Out (F)
                 and then not AAU.Is_Empty (AAN.Sources (F))
               then
                  --  We do not set the ports that are connected to
                  --  subprogram out ports, this should be done during
                  --  the subprogram call sequence handling.

                  declare
                     use Ocarina.Backends.C_Tree.Nodes;
                     D   : Node_Id := AAN.First_Node (AAN.Sources (F));
                     Set : Boolean := True;
                  begin
                     while Present (D) loop
                        Set := Kind (Item (D)) /= K_Port_Spec_Instance;
                        exit when not Set;

                        D := AAN.Next_Node (D);
                     end loop;

                     if Set then
                        N :=
                          Make_Variable_Declaration
                            (Make_Defining_Identifier
                               (Map_C_Variable_Name (F, Port_Request => True)),
                             RE (RE_Request_T),
                             Is_Static => True);
                        Append_Node_To_List (N, Declarations);

                        L :=
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Member_Designator
                                      (Defining_Identifier =>
                                         Make_Defining_Identifier
                                           (Map_C_Enumerator_Name (F)),
                                       Aggregate_Name =>
                                         Make_Defining_Identifier
                                           (Map_C_Enumerator_Name (F))),
                                  Aggregate_Name =>
                                    Make_Defining_Identifier (MN (M_Vars))),
                             Aggregate_Name =>
                               Make_Defining_Identifier
                                 (Map_C_Variable_Name
                                    (F,
                                     Port_Request => True)));
                        R :=
                          Make_Defining_Identifier
                            (Map_C_Variable_Name
                               (F,
                                Request_Variable => True));

                        Z := AAN.Corresponding_Instance (F);
                        T := No_Node;

                        if Present (Backend_Node (Identifier (Z)))
                          and then Present
                            (CTN.Type_Definition_Node
                               (Backend_Node (Identifier (Z))))
                        then
                           T :=
                             CTN.Type_Name
                               (CTN.Type_Definition_Node
                                  (Backend_Node (Identifier (Z))));
                        end if;

                        if T /= No_Node
                          and then CTN.Kind (T) = CTN.K_Array_Declaration
                        then
                           Parameters := New_List (CTN.K_Parameter_List);
                           Append_Node_To_List (L, Parameters);
                           Append_Node_To_List (R, Parameters);
                           Append_Node_To_List (Get_Data_Size (Z), Parameters);
                           N :=
                             CTU.Make_Call_Profile
                               (RE (RE_Copy_Array),
                                Parameters);
                        else
                           N :=
                             Make_Expression
                               (Left_Expr  => L,
                                Operator   => Op_Equal,
                                Right_Expr => R);
                        end if;

                        Append_Node_To_List (N, WStatements);

                        N :=
                          Make_Expression
                            (Left_Expr =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Defining_Identifier (MN (M_Port)),
                                  Aggregate_Name =>
                                    Make_Defining_Identifier
                                      (Map_C_Variable_Name
                                         (F,
                                          Port_Request => True))),
                             Operator   => Op_Equal,
                             Right_Expr =>
                               Make_Defining_Identifier
                                 (Map_C_Enumerator_Name (F)));
                        Append_Node_To_List (N, WStatements);

                        Call_Parameters := New_List (CTN.K_Parameter_List);
                        N               :=
                          Make_Defining_Identifier
                            (Map_C_Enumerator_Name (S, Current_Device));
                        Append_Node_To_List (N, Call_Parameters);

                        N :=
                          Make_Defining_Identifier
                            (Map_C_Enumerator_Name (F, Local_Port => True));
                        Append_Node_To_List (N, Call_Parameters);

                        N :=
                          Make_Variable_Address
                            (Make_Defining_Identifier
                               (Map_C_Variable_Name
                                  (F,
                                   Port_Request => True)));
                        Append_Node_To_List (N, Call_Parameters);

                        N :=
                          CTU.Make_Call_Profile
                            (RE (RE_Gqueue_Store_Out),
                             Call_Parameters);

                        Append_Node_To_List (N, WStatements);
                     end if;
                  end;
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Set_Out_Ports;

         -------------------------
         -- Make_Send_Out_Ports --
         -------------------------

         procedure Make_Send_Out_Ports (WStats : List_Id) is
            N                     : Node_Id;
            F                     : Node_Id;
            Error_Already_Defined : Boolean := False;
            Decl                  : Node_Id;
         begin
            N := Message_Comment ("Send the OUT ports");
            Append_Node_To_List (N, WStats);

            F := First_Node (Features (E));

            while Present (F) loop

               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then

                  --  Then, call the send_output in the main loop.
                  Call_Parameters := New_List (CTN.K_Parameter_List);
                  N               :=
                    Make_Defining_Identifier
                      (Map_C_Enumerator_Name (S, Current_Device));
                  Append_Node_To_List (N, Call_Parameters);

                  N := Make_Defining_Identifier (Map_C_Enumerator_Name (F));
                  Append_Node_To_List (N, Call_Parameters);

                  N :=
                    CTU.Make_Call_Profile
                      (RE (RE_Send_Output),
                       Call_Parameters);

                  if Get_Miss_Rate (F) /= 0 then
                     --  XXX is this required ? from PolyORB-HI/C,
                     --  looks like it is some hack to add fake miss
                     --  of events.

                     Append_Node_To_List
                       (Make_If_Statement
                          (Condition =>
                             Make_Call_Profile
                               (RE (RE_Compute_Miss),
                                Make_List_Id
                                  (Make_Literal
                                     (CV.New_Int_Value
                                        (Get_Miss_Rate (F),
                                         0,
                                         10)))),
                           Statements => Make_List_Id (N)),
                        WStats);
                  else
                     N :=
                       Make_Assignment_Statement
                         (Variable_Identifier =>
                            Make_Defining_Identifier (VN (V_Error)),
                          Expression => N);
                     Append_Node_To_List (N, WStats);

                     declare
                        use Ocarina.Backends.C_Tree.Nodes;
                     begin
                        Decl := CTN.First_Node (Declarations);
                        while Present (Decl) loop

                           if CTN.Kind (Decl) = CTN.K_Variable_Declaration
                             and then
                               Get_Name_String
                                 (CTN.Name (CTN.Defining_Identifier (Decl)))
                             = Get_Name_String (VN (V_Error))
                           then
                              Error_Already_Defined := True;
                           end if;
                           exit when Error_Already_Defined;
                           Decl := CTN.Next_Node (Decl);
                        end loop;
                     end;

                     if not Error_Already_Defined then
                        N :=
                          Make_Variable_Declaration
                            (Defining_Identifier =>
                               Make_Defining_Identifier (VN (V_Error)),
                             Used_Type => RE (RE_Int32_T));
                        Append_Node_To_List (N, Declarations);
                        Error_Already_Defined := True;
                     end if;
                     declare
                        Rec_Entrypoint : constant Name_Id :=
                          Get_Thread_Recover_Entrypoint (E);
                        Parameter_List : constant List_Id :=
                          New_List (CTN.K_List_Id);
                        Then_Statements : constant List_Id :=
                          New_List (CTN.K_Statement_List);
                     begin
                        if Rec_Entrypoint /= No_Name then
                           N :=
                             Make_Extern_Entity_Declaration
                               (Make_Function_Specification
                                  (Make_Defining_Identifier (Rec_Entrypoint),
                                   Parameters  => Parameter_List, --  XXX
                                   Return_Type => New_Node (CTN.K_Void)));
                           Append_Node_To_List
                             (N,
                              CTN.Declarations (Current_File));

                           N :=
                             CTU.Make_Call_Profile
                               (Make_Defining_Identifier (Rec_Entrypoint),
                                No_List);
                           Append_Node_To_List (N, Then_Statements);

                           N :=
                             Make_If_Statement
                               (Condition =>
                                  Make_Expression
                                    (Make_Defining_Identifier (VN (V_Error)),
                                     Op_Not_Equal,
                                     RE (RE_SUCCESS)),
                                Statements => Then_Statements);
                           Append_Node_To_List (N, WStats);
                        end if;
                     end;

                  end if;
               end if;

               F := Next_Node (F);
            end loop;
         end Make_Send_Out_Ports;

         ----------------------
         -- Make_Wait_Offset --
         ----------------------

         procedure Make_Wait_Offset is
            D : Time_Type;
         begin
            if Get_Thread_Dispatch_Protocol (E) /= Thread_Periodic then
               return;
            end if;

            D := Get_Dispatch_Offset (E);

            if D /= Null_Time then
               N :=
                 Make_Variable_Declaration
                   (Make_Defining_Identifier (VN (V_Offset)),
                    RE (RE_Time_T));
               Append_Node_To_List (N, Statements);

               N := Map_Time (D, VN (V_Offset));
               Append_Node_To_List (N, Statements);

               Append_Node_To_List
                 (CTU.Make_Call_Profile
                    (RE (RE_Task_Wait_Offset),
                     Make_List_Id
                       (Make_Variable_Address
                          (Make_Defining_Identifier (VN (V_Offset))))),
                  Statements);
            end if;
         end Make_Wait_Offset;

         -------------------------
         -- Make_Fetch_In_Ports --
         -------------------------

         procedure Make_Fetch_In_Ports is
            F : Node_Id;
         begin
            N := Message_Comment ("Get the IN ports values");
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Is_In (F)
                 and then AAN.Is_Data (F)
               then
                  --  Assign the port value

                  N := Make_Get_Valid_Value (F);
                  Append_Node_To_List (N, WStatements);

                  Call_Parameters := New_List (CTN.K_Parameter_List);

                  Append_Node_To_List
                    (Make_Defining_Identifier (Map_C_Enumerator_Name (S)),
                     Call_Parameters);

                  Append_Node_To_List
                    (Make_Defining_Identifier (Map_C_Enumerator_Name (F)),
                     Call_Parameters);

                  N :=
                    Make_Call_Profile
                      (RE (RE_Gqueue_Next_Value),
                       Call_Parameters);

               end if;

               F := Next_Node (F);
            end loop;
         end Make_Fetch_In_Ports;

         ------------------------
         -- Make_Call_Sequence --
         ------------------------

         procedure Make_Call_Sequence is
            Call_Seq : constant Node_Id := First_Node (Calls (E));
         begin
            if not Has_Modes (E) or else AAU.Length (Calls (E)) = 1 then
               --  If the thread has no modes, then it should has one
               --  unique call sequence, handle it.

               CTU.Handle_Call_Sequence
                 (S,
                  Call_Seq,
                  Declarations,
                  WStatements,
                  Current_Device);
            else
               N := Message_Comment ("not implemented yet");
               Append_Node_To_List (N, WStatements);
            end if;
         end Make_Call_Sequence;

         -----------------------------------
         -- Make_Ports_Compute_Entrypoint --
         -----------------------------------

         procedure Make_Ports_Compute_Entrypoint is
            N                   : Node_Id;
            F                   : Node_Id;
            Switch_Alternatives : List_Id;
            Switch_Statements   : List_Id;
            Switch_Labels       : List_Id;
         begin
            N := Message_Comment ("Make_Ports_Compute_Entrypoint");
            Append_Node_To_List (N, WStatements);

            F := First_Node (Features (E));

            Switch_Alternatives := New_List (CTN.K_Alternatives_List);

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then

                  Switch_Statements := New_List (CTN.K_Statement_List);
                  Switch_Labels     := New_List (CTN.K_Label_List);

                  --  Declare local data variable if the port is a
                  --  data port.

                  if AAN.Is_Data (F) then
                     N := Make_Get_Valid_Value (F);
                     Append_Node_To_List (N, Switch_Statements);
                  end if;

                  if Is_Event (F) and then not AAN.Is_Data (F) then
                     Call_Parameters := New_List (CTN.K_Parameter_List);

                     Append_Node_To_List
                       (Make_Defining_Identifier (Map_C_Enumerator_Name (S)),
                        Call_Parameters);

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_C_Enumerator_Name (F, Local_Port => True)),
                        Call_Parameters);

                     N :=
                       Make_Call_Profile
                         (RE (RE_Gqueue_Next_Value),
                          Call_Parameters);

                     Append_Node_To_List (N, Switch_Statements);
                  end if;

                  Call_Parameters := New_List (CTN.K_Parameter_List);
                  N := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
                  Append_Node_To_List (N, Call_Parameters);

                  declare
                     Parameter_List : constant List_Id :=
                       New_List (CTN.K_List_Id);
                  begin
                     N :=
                       Make_Parameter_Specification
                         (Make_Defining_Identifier (PN (P_Self)),
                          Parameter_Type => RE (RE_Task_Id));
                     Append_Node_To_List (N, Parameter_List);

                     if AAN.Is_Data (F) then
                        N :=
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Member_Designator
                                      (Defining_Identifier =>
                                         Make_Defining_Identifier
                                           (Map_C_Enumerator_Name (F)),
                                       Aggregate_Name =>
                                         Make_Defining_Identifier
                                           (Map_C_Enumerator_Name (F))),
                                  Aggregate_Name =>
                                    Make_Defining_Identifier (MN (M_Vars))),
                             Aggregate_Name =>
                               Make_Defining_Identifier
                                 (Map_C_Variable_Name
                                    (F,
                                     Port_Request => True)));

                        Append_Node_To_List (N, Call_Parameters);

                        N :=
                          Map_C_Data_Type_Designator
                            (Corresponding_Instance (F));
                        N :=
                          Make_Parameter_Specification
                            (Map_C_Defining_Identifier (F),
                             N);
                        Append_Node_To_List (N, Parameter_List);
                     end if;

                     N :=
                       Make_Extern_Entity_Declaration
                         (Make_Function_Specification
                            (Map_C_Subprogram_Identifier (F),
                             Parameters  => Parameter_List,
                             Return_Type => New_Node (CTN.K_Void)));
                     Append_Node_To_List (N, CTN.Declarations (Current_File));
                  end;

                  N :=
                    Make_Call_Profile
                      (Map_C_Subprogram_Identifier (F),
                       Call_Parameters);
                  Append_Node_To_List (N, Switch_Statements);

                  Append_Node_To_List
                    (Make_Defining_Identifier
                       (Map_C_Enumerator_Name (F, Local_Port => True)),
                     Switch_Labels);

                  N :=
                    Make_Switch_Alternative (Switch_Labels, Switch_Statements);
                  Append_Node_To_List (N, Switch_Alternatives);
               end if;

               F := Next_Node (F);
            end loop;

            N := Make_Switch_Alternative (No_List, No_List);
            Append_Node_To_List (N, Switch_Alternatives);

            --  Make the case statement

            N :=
              Make_Switch_Statement
                (Expression   => Make_Defining_Identifier (MN (M_Port)),
                 Alternatives => Switch_Alternatives);
            Append_Node_To_List (N, WStatements);
         end Make_Ports_Compute_Entrypoint;

         ------------------------------------
         -- Make_Thread_Compute_Entrypoint --
         ------------------------------------

         procedure Make_Thread_Compute_Entrypoint is
            N              : Node_Id;
            Parameter_List : constant List_Id := New_List (CTN.K_List_Id);
         begin
            N := Message_Comment ("Make_Thread_Compute_Entrypoint");
            Append_Node_To_List (N, WStatements);

            Call_Parameters := New_List (CTN.K_Parameter_List);
            N := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
            Append_Node_To_List (N, Call_Parameters);

            N :=
              Make_Parameter_Specification
                (Make_Defining_Identifier (PN (P_Self)),
                 Parameter_Type => RE (RE_Task_Id));
            Append_Node_To_List (N, Parameter_List);

            if P = Thread_Sporadic then
               Append_Node_To_List
                 (Make_Defining_Identifier (VN (V_Port)),
                  Call_Parameters);

               N :=
                 Make_Parameter_Specification
                   (Make_Defining_Identifier (VN (V_Port)),
                    Parameter_Type => RE (RE_Port_T));
               Append_Node_To_List (N, Parameter_List);
            end if;

            if not Has_Modes (E) then
               N :=
                 Make_Extern_Entity_Declaration
                   (Make_Function_Specification
                      (Map_C_Subprogram_Identifier (E),
                       Parameters  => Parameter_List,
                       Return_Type => New_Node (CTN.K_Void)));

               Append_Node_To_List (N, CTN.Declarations (Current_File));

               N :=
                 Make_Call_Profile
                   (Map_C_Subprogram_Identifier (E),
                    Call_Parameters);
               Append_Node_To_List (N, WStatements);
            else
               Display_Located_Error
                 (Loc (E),
                  "Threads with mode controlled compute entrypoints not" &
                    " supported yet",
                  Fatal => True);
            end if;
         end Make_Thread_Compute_Entrypoint;

         ----------------------------------------
         -- Make_Thread_Behavior_Specification --
         ----------------------------------------

         procedure Make_Thread_Behavior_Specification is
            N, N1          : Node_Id;
         begin

            --  Add_Include (RH (RH_Subprograms));

            N := Message_Comment
              ("Call the function implementing"
                 & " the C-Mapping of the Behavior_Specification");

            Append_Node_To_List (N, WStatements);

            Call_Parameters := New_List (CTN.K_Parameter_List);
            N := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
            Append_Node_To_List (N, Call_Parameters);

            if P = Thread_Sporadic and then
              Compute_Nb_On_Dispatch_Transitions (E) > 1
            then

               N := Make_Defining_Identifier (VN (V_Next_Complete_State));
               Append_Node_To_List (N, Call_Parameters);

               N := Make_Defining_Identifier
                 (VN (V_Index_Transition_To_Execute));
               Append_Node_To_List (N, Call_Parameters);

            end if;
            --  add data subcomponents of the thread to the call_parameters
            --  of the procedure <<thread_instance_name>>_ba_body

            if not AAU.Is_Empty (Subcomponents (E)) then
               N1 := First_Node (Subcomponents (E));

               while Present (N1) loop
                  if AAU.Is_Data (Corresponding_Instance (N1)) then

                     N :=
                       Make_Variable_Address
                         (Map_C_Defining_Identifier (N1));

                     Append_Node_To_List (N, Call_Parameters);

                  end if;
                  N1 := Next_Node (N1);
               end loop;
            end if;

            N := Make_Extern_Entity_Declaration
              (Make_Specification_Of_BA_Related_Function
                 (E, BA_Body => True));

            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Call_Profile
                (Defining_Identifier => Make_Defining_Identifier
                   (Map_C_BA_Related_Function_Name (S, BA_Body => True)),
                 Parameters          => Call_Parameters);

            Append_Node_To_List (N, WStatements);

         end Make_Thread_Behavior_Specification;

      begin
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

            when Thread_Aperiodic =>
               N :=
                 Message_Comment
                   ("Sporadic task : " &
                      Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

            when Thread_Background =>
               N :=
                 Message_Comment
                   ("Background task : " &
                      Get_Name_String (Display_Name (Identifier (S))));
               Append_Node_To_List (N, CTN.Declarations (Current_File));

            when others =>
               Display_Error ("unknown type of thread", Fatal => False);
               null;
         end case;

         Check_Thread_Consistency (E);

         --  Visit the data subcomponents of the thread

         if not AAU.Is_Empty (Subcomponents (E)) then
            N1 := First_Node (Subcomponents (E));

            while Present (N1) loop
               if AAU.Is_Data (Corresponding_Instance (N1)) then

                  N :=
                    Make_Variable_Declaration
                      (Map_C_Defining_Identifier (N1),
                       Map_C_Data_Type_Designator
                         (Corresponding_Instance (N1)));

                  Append_Node_To_List (N, Declarations);

               end if;
               N1 := Next_Node (N1);
            end loop;
         end if;

         if not AAU.Is_Empty (Subcomponents (E)) then
            N1 := First_Node (Subcomponents (E));

            while Present (N1) loop

               if not AAU.Is_Data (Corresponding_Instance (N1)) then
                  Visit (Corresponding_Instance (N1));
               end if;
               N1 := Next_Node (N1);
            end loop;
         end if;

         if Impl_Kind = Thread_With_Behavior_Specification then
            declare
               BA : constant Node_Id := Get_Behavior_Specification (E);
            begin
               if P = Thread_Sporadic and then
                 BANu.Length (BATN.States (BA)) > 1 and then
                 Compute_Nb_On_Dispatch_Transitions (E) > 1
               then

                  --  __po_hi_ba_automata_state_t *next_complete_state =
                  --    (__po_hi_ba_automata_state_t *)
                  --      malloc(sizeof(__po_hi_ba_automata_state_t));

                  N :=
                    Make_Variable_Declaration
                      (Defining_Identifier => Make_Defining_Identifier
                         (VN (V_Next_Complete_State)),
                       Used_Type           => Make_Pointer_Type
                         (RE (RE_Ba_Automata_State_T)),
                       Value               => Make_Type_Conversion
                         (Subtype_Mark => Make_Pointer_Type
                            (RE (RE_Ba_Automata_State_T)),
                          Expression   => Make_Call_Profile
                            (Make_Defining_Identifier (FN (F_Malloc)),
                             Make_List_Id
                               (Make_Call_Profile
                                    (Make_Defining_Identifier (FN (F_Sizeof)),
                                     Make_List_Id
                                       (RE (RE_Ba_Automata_State_T)))))));

                  Append_Node_To_List (N, Declarations);

                  --  next_complete_state
                  --       ->nb_dispatch_triggers_of_each_transition =
                  --  (__po_hi_int32_t *)
                  --     malloc( sizeof(__po_hi_int32_t) *
                  --  __po_hi_consumer_max_dispatch_transitions_
                  --  per_complete_state);

                  N := Make_Assignment_Statement
                    (Variable_Identifier => Make_Member_Designator
                       (Defining_Identifier => Make_Defining_Identifier
                            (MN
                                 (M_Nb_Dispatch_Triggers_Of_Each_Transition)),
                        Aggregate_Name      => Make_Defining_Identifier
                          (VN (V_Next_Complete_State)),
                        Is_Pointer          => True),
                     Expression          => Make_Type_Conversion
                       (Subtype_Mark => Make_Pointer_Type
                            (RE (RE_Int32_T)),
                        Expression   => Make_Call_Profile
                          (Make_Defining_Identifier (FN (F_Malloc)),
                           Make_List_Id
                             (Make_Expression
                               (Left_Expr  => Make_Call_Profile
                                  (Make_Defining_Identifier (FN (F_Sizeof)),
                                   Make_List_Id
                                       (RE (RE_Int32_T))),
                                Operator   => Op_Asterisk,
                                Right_Expr => Make_Defining_Identifier
                                 (Map_C_Define_Name
                                   (S,
                                    Max_Dispatch_Transitions_Per_Complete_State
                                         => True)))))));
                  Append_Node_To_List (N, Statements);

                  --  next_complete_state->dispatch_triggers_of_all_transitions
                  --  = (__po_hi_int32_t *)
                  --  malloc( sizeof(__po_hi_int32_t) *
                  --   (__po_hi_consumer_max_dispatch_transitions_per_
                  --     complete_state*__po_hi_consumer_max_dispatch_
                  --           triggers_per_dispatch_transition));

                  N := Make_Assignment_Statement
                    (Variable_Identifier => Make_Member_Designator
                       (Defining_Identifier => Make_Defining_Identifier
                            (MN (M_Dispatch_Triggers_Of_All_Transitions)),
                        Aggregate_Name      => Make_Defining_Identifier
                          (VN (V_Next_Complete_State)),
                        Is_Pointer          => True),
                     Expression          => Make_Type_Conversion
                       (Subtype_Mark => Make_Pointer_Type
                            (RE (RE_Int32_T)),
                        Expression   => Make_Call_Profile
                          (Make_Defining_Identifier (FN (F_Malloc)),
                           Make_List_Id
                             (Make_Expression
                               (Left_Expr  => Make_Call_Profile
                                  (Make_Defining_Identifier (FN (F_Sizeof)),
                                   Make_List_Id
                                       (RE (RE_Int32_T))),
                                Operator   => Op_Asterisk,
                                Right_Expr => Make_Expression
                                  (Left_Expr  => Make_Defining_Identifier
                                 (Map_C_Define_Name
                                   (S,
                                    Max_Dispatch_Transitions_Per_Complete_State
                                         => True)),
                                   Operator   => Op_Asterisk,
                                   Right_Expr => Make_Defining_Identifier
                                 (Map_C_Define_Name
                                 (S,
                                  Max_Dispatch_Triggers_Per_Dispatch_Transition
                                         => True))))))));
                  Append_Node_To_List (N, Statements);

               end if;
            end;
         end if;

         if Has_Ports (E) then
            --  Make the __po_hi_gqueue_init call

            Call_Parameters := New_List (CTN.K_Parameter_List);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Enumerator_Name (S, Current_Device)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Define_Name (S, Nb_Ports => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Queue => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Fifo_Size => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_First => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Offsets => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Woffsets => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_N_Dest => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Destinations => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Used_Size => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_History => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Recent => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Empties => True)),
               Call_Parameters);

            Append_Node_To_List
              (Make_Defining_Identifier
                 (Map_C_Variable_Name (S, Port_Total_Fifo => True)),
               Call_Parameters);

            N := Make_Call_Profile (RE (RE_Gqueue_Init), Call_Parameters);
            Append_Node_To_List (N, Statements);
         end if;

         Make_Activate_Entrypoint;

         --  If the thread is sporadic or aperiodic, we generate the
         --  call to block waiting for events.

         if P = Thread_Aperiodic then
            Make_Wait_Event;
         elsif P = Thread_Sporadic then
            if Impl_Kind = Thread_With_Behavior_Specification and then
              Compute_Nb_On_Dispatch_Transitions (E) > 1
            then
               Make_Wait_Specific_Events;
            else
               Make_Wait_Event;
            end if;
         end if;

         --  Depending on the implementation kind, call the proper
         --  implementation routines.

         case Impl_Kind is
            when Thread_With_Call_Sequence =>
               --  This kind of implementation is the simplest
               --  one. The user has only to implementation the
               --  behaviour of subprograms and does not have to worry
               --  about sending and receiving ports.

               --  Get IN ports values and dequeue them

               if Has_In_Ports (E) then
                  Make_Fetch_In_Ports;
               end if;

               --  Handle the thread call sequences

               if not AAU.Is_Empty (Calls (E)) then
                  Make_Call_Sequence;
               end if;

               --  Set OUT ports values

               if Has_Out_Ports (E) then
                  Make_Set_Out_Ports;
               end if;

               --  Send OUT ports

               if Has_Out_Ports (E) then
                  Make_Send_Out_Ports (WStatements);
               end if;

            when Thread_With_Compute_Entrypoint =>
               --  Call the compute entrypoint. The code of the
               --  compute entry point will include the setting of
               --  the thread OUT ports.

               Make_Thread_Compute_Entrypoint;

               --  Send OUT ports

               --  FIXME: Depending on an AADL property, the code of
               --  the thread entrypoint may include the sending of
               --  OUT ports.

               if Has_Out_Ports (E) then
                  Make_Send_Out_Ports (WStatements);
               end if;

            when Thread_With_Port_Compute_Entrypoint =>
               --  Call the compute entrypoints of the triggeing
               --  port. The code of the compute entry point will
               --  include the sending of the thread OUT ports.

               Make_Ports_Compute_Entrypoint;

               --  Send OUT ports

               --  FIXME: Depending on an AADL property, the code of
               --  the port entrypoints may include the sending of OUT
               --  ports.

               if Has_Out_Ports (E) then
                  Make_Send_Out_Ports (WStatements);
               end if;

            when Thread_With_Behavior_Specification =>

               --  For a periodic thread, when its BA has more than one
               --  state, we call the <<thread_name>>_states_initialization
               --  /** Initialize states; this function is called
               --   when the BA of the thread has more than one state **/
               --
               --  producer_states_and_current_state_initialization ();
               if P = Thread_Periodic or else P = Thread_Sporadic then
                  declare
                     BA : Node_Id;
                     P1 : List_Id;
                  begin
                     BA := Get_Behavior_Specification (E);
                     if BANu.Length (BATN.States (BA)) > 1 then
                        N :=
                          Make_Doxygen_C_Comment
                            ("Initialize states",
                             Has_Header_Spaces => False);
                        Append_Node_To_List (N, Statements);

                        --  Call <<thread_name>>_states_initialization

                        if P = Thread_Periodic
                          or else (P = Thread_Sporadic and then
                              Compute_Nb_On_Dispatch_Transitions (E) = 1)
                        then
                           P1 := No_List;
                        elsif P = Thread_Sporadic and then
                          Compute_Nb_On_Dispatch_Transitions (E) > 1
                        then
                           P1 := Make_List_Id
                             (Make_Defining_Identifier
                                (VN (V_Next_Complete_State)));
                        end if;

                        N := CTU.Make_Call_Profile
                          (Defining_Identifier => Make_Defining_Identifier
                             (Map_C_BA_Related_Function_Name
                                  (S, States_Initialization => True)),
                           Parameters          =>  P1);
                        Append_Node_To_List (N, Statements);

                        N :=
                          Make_Extern_Entity_Declaration
                            (Make_Specification_Of_BA_Related_Function
                               (E, States_Initialization => True));

                        Append_Node_To_List (N,
                                             CTN.Declarations (Current_File));
                     end if;
                  end;
               end if;

               Make_Thread_Behavior_Specification;
               if Has_Out_Ports (E) then
                  Make_Send_Out_Ports (WStatements);
               end if;

            when others =>
               raise Program_Error with "Unconsistency in Task_Job_Body";
         end case;

         --  Block until the next dispatch with respect to the
         --  inter-arrival time in case of a sporadic or periodic
         --  thread.

         if P = Thread_Periodic or else P = Thread_Sporadic then
            Make_Task_Blocking (WStatements);
         end if;

         --  If an activate entrypoint has been specified for the
         --  thread, add a call to the corresponding function before
         --  initialization of the runtime is done.

         declare
            Activate_Entrypoint : constant Name_Id :=
              Get_Thread_Activate_Entrypoint (E);
            Parameter_List : constant List_Id := New_List (CTN.K_List_Id);
         begin
            if Activate_Entrypoint /= No_Name then
               N :=
                 Make_Extern_Entity_Declaration
                   (Make_Function_Specification
                      (Make_Defining_Identifier (Activate_Entrypoint),
                       Parameters  => Parameter_List, --  XXX
                       Return_Type => New_Node (CTN.K_Void)));
               Append_Node_To_List (N, CTN.Declarations (Current_File));
               N :=
                 CTU.Make_Call_Profile
                   (Make_Defining_Identifier (Activate_Entrypoint),
                    No_List);
               Append_Node_To_List (N, Statements);
            end if;
         end;

         N :=
           Make_Doxygen_C_Comment
             ("Waiting for other tasks initialization",
              Has_Header_Spaces => False);
         Append_Node_To_List (N, Statements);

         --  Call __po_hi_wait_initialization

         N := CTU.Make_Call_Profile (RE (RE_Wait_Initialization), No_List);
         Append_Node_To_List (N, Statements);

         --  Call __po_hi_wait_offset() iff necessary

         Make_Wait_Offset;

         --  Compute the next period after initialization, because
         --  the period may have passed after init.

         if P = Thread_Periodic or else P = Thread_Sporadic then
            N :=
              CTU.Make_Call_Profile
                (RE (RE_Compute_Next_Period),
                 Make_List_Id
                   (Make_Defining_Identifier
                      (Map_C_Enumerator_Name (S, Current_Device))));
            Append_Node_To_List (N, Statements);
         end if;

         if P = Thread_Periodic or else P = Thread_Sporadic then
            --  For periodic threads, we force a first wait to ensure
            --  synchronized start after initialization. The runtime
            --  computes a specific epoch to ensure such start.

            N :=
              Make_Doxygen_C_Comment
                ("Waiting for the first dispatch instant",
                 Has_Header_Spaces => False);
            Append_Node_To_List (N, Statements);

            Call_Parameters := New_List (CTN.K_Parameter_List);
            Call_Parameters_Of_BA_Initialization_Function :=
              New_List (CTN.K_Parameter_List);
            if Current_Device /= No_Node then
               N :=
                 Make_Defining_Identifier
                   (Map_C_Enumerator_Name
                      (S,
                       Custom_Parent => Current_Device));
               N1 :=
                 Make_Defining_Identifier
                   (Map_C_Enumerator_Name
                      (S,
                       Custom_Parent => Current_Device));
            else
               N := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
               N1 := Make_Defining_Identifier (Map_C_Enumerator_Name (S));
            end if;
            Append_Node_To_List (N, Call_Parameters);
            Append_Node_To_List
              (N1,
               Call_Parameters_Of_BA_Initialization_Function);
            N :=
              CTU.Make_Call_Profile
                (RE (RE_Wait_For_Next_Period),
                 Call_Parameters);
            Append_Node_To_List (N, Statements);

            if Impl_Kind = Thread_With_Behavior_Specification then
               declare
                  BA : Node_Id;
               begin
                  if not AAU.Is_Empty (Subcomponents (E)) then
                     N1 := First_Node (Subcomponents (E));

                     while Present (N1) loop
                        if AAU.Is_Data (Corresponding_Instance (N1)) then

                           N :=
                             Make_Variable_Address
                               (Map_C_Defining_Identifier (N1));

                           Append_Node_To_List
                             (N,
                              Call_Parameters_Of_BA_Initialization_Function);

                        end if;
                        N1 := Next_Node (N1);
                     end loop;
                  end if;

                  BA := Get_Behavior_Specification (E);
                  if BANu.Length (BATN.States (BA)) > 1 then

                     if Is_To_Make_Init_Sequence (E) then
                        --  Call the function implementing the initialization
                        --  sequence this is in the case when initial state
                        --  is neither complete nor final

                        N :=
                          Make_Doxygen_C_Comment
                            ("Call the function implementing the "
                             & "initialization sequence this is in the case "
                             & "when initial state is neither complete "
                             & "nor final",
                             Has_Header_Spaces => False);
                        Append_Node_To_List (N, Statements);

                        N := CTU.Make_Call_Profile
                          (Defining_Identifier => Make_Defining_Identifier
                             (Map_C_BA_Related_Function_Name
                                  (S, BA_Initialization => True)),
                           Parameters          =>
                             Call_Parameters_Of_BA_Initialization_Function);
                        Append_Node_To_List (N, Statements);

                        N :=
                          Make_Extern_Entity_Declaration
                            (Make_Specification_Of_BA_Related_Function
                               (E, BA_Initialization => True));

                        Append_Node_To_List (N,
                                             CTN.Declarations (Current_File));

                        if Has_Out_Ports (E) then
                           Make_Send_Out_Ports (Statements);
                        end if;

                        Make_Task_Blocking (Statements);

                     end if;
                  end if;
               end;
            end if;
         end if;

         if P /= Thread_Background then
            --  Make the while (1){} and add all statements
            N :=
              Make_Doxygen_C_Comment
                ("Task body",
                 Has_Header_Spaces => False);
            Append_Node_To_List (N, Statements);

            N :=
              Make_While_Statement
                (Make_Literal (CV.New_Int_Value (1, 0, 10)),
                 WStatements);
            Append_Node_To_List (N, Statements);
         else
            --  Simply append statements
            Append_Node_To_List (CTN.First_Node (WStatements), Statements);
         end if;

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

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P            : constant Node_Id := CTN.Entity (U);
         S            : Node_Id;
         N            : Node_Id;
         Declarations : constant List_Id := New_List (CTN.K_Declaration_List);
         Statements   : constant List_Id := New_List (CTN.K_Statement_List);
         The_System   : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
      begin
         Push_Entity (P);
         Push_Entity (U);
         CTU.Set_Activity_Source (U);

         Main_Deliver_Alternatives := New_List (CTN.K_Alternatives_List);

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               if AAU.Is_Data (Corresponding_Instance (S)) then

                  N :=
                    Make_Variable_Declaration
                      (Map_C_Defining_Identifier (S),
                       Map_C_Data_Type_Designator
                         (Corresponding_Instance (S)));

                  Append_Node_To_List
                    (Make_Extern_Entity_Declaration (N),
                     CTN.Declarations (Current_File));

                  Bind_AADL_To_Object (Identifier (S), N);
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop

               if not AAU.Is_Data (Corresponding_Instance (S)) then
                  Visit (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            S := First_Node (Subcomponents (The_System));
            while Present (S) loop
               if AAU.Is_Device (Corresponding_Instance (S))
                 and then
                 Get_Bound_Processor (Corresponding_Instance (S)) =
                 Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         if Present (Backend_Node (Identifier (Parent_Subcomponent (E))))
           and then Present
             (CTN.Job_Node
                (Backend_Node (Identifier (Parent_Subcomponent (E)))))
         then

            Append_Node_To_List
              (Make_Variable_Declaration
                 (Defining_Identifier =>
                    Make_Defining_Identifier (VN (V_Entity)),
                  Used_Type => RE (RE_Entity_T)),
               Declarations);

            --  Add the call to entity = __po_hi_global...[port]

            N :=
              Make_Expression
                (Left_Expr  => Make_Defining_Identifier (VN (V_Entity)),
                 Operator   => Op_Equal,
                 Right_Expr =>
                   Make_Call_Profile
                     (RE (RE_Get_Entity_From_Global_Port),
                      Make_List_Id
                        (Make_Member_Designator
                           (Defining_Identifier =>
                              Make_Defining_Identifier (MN (M_Port)),
                            Is_Pointer     => True,
                            Aggregate_Name =>
                              Make_Defining_Identifier (VN (V_Request))))));
            Append_Node_To_List (N, Statements);

            --  Add the switch which redirect to local deliver functions

            N := Make_Switch_Alternative (No_List, No_List);

            Append_Node_To_List (N, Main_Deliver_Alternatives);

            N :=
              Make_Switch_Statement
                (Expression   => Make_Defining_Identifier (VN (V_Entity)),
                 Alternatives => Main_Deliver_Alternatives);

            Append_Node_To_List (N, Statements);

            N :=
              Make_Doxygen_C_Comment
                (Is_Function  => True,
                 Element_Name =>
                   "void __po_hi_main_deliver " &
                     "(__po_hi_request_t* request)",
                 Brief => "Used to deliver request to the appropriate ports",
                 Desc  =>
                   "This function takes a request as argument (\arg request)" &
                     " and calls the appropriate function for its delivery." &
                     "To specify which function should be called, it " &
                     "extracts the receiver entity using the destination " &
                     "port.",
                 Has_Header_Spaces => False);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Function_Implementation
                (CTN.Job_Node
                   (Backend_Node (Identifier (Parent_Subcomponent (E)))),
                 Declarations,
                 Statements);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : constant Node_Id := Get_Implementation (E);
         S              : Node_Id;
      begin
         Current_Device := E;

         if Implementation /= No_Node then
            if not AAU.Is_Empty (AAN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  Visit_Component_Instance (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end if;

         Current_Device := No_Node;
      end Visit_Device_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (C_Root);

         --  Visit all the subcomponents of the system

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity; --  C_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         S                    : constant Node_Id   := Parent_Subcomponent (E);
         N                    : Node_Id;
         F                    : Node_Id;
         D                    : Node_Id;
         Fifo_Size_Values     : Node_Id;
         N_Dest_Values        : Node_Id;
         Local_Dest_Values    : Node_Id;
         Destinations_Values  : Node_Id;
         Destinations         : List_Id;
         Deliver_Declarations : List_Id;
         Deliver_Statements   : List_Id;
         Deliver_Alternatives : List_Id;
         Switch_Labels        : List_Id;
         Switch_Statements    : List_Id;
         Parameters           : List_Id;
         Queue_Size           : Long_Long;
         Fifo_Size            : Unsigned_Long_Long := 0;
         Nb_Dest              : Unsigned_Long_Long := 0;
         Has_Local_Deliver    : Boolean            := False;
      begin
         if Get_Thread_Dispatch_Protocol (E) = Thread_Sporadic and then
           not Has_In_Event_Ports (E)
         then
            Display_Located_Error
              (Loc (E),
               "None of the IN ports of this sporadic thread is an event port",
               Fatal => True);
         end if;

         if Has_Ports (E) then
            F := First_Node (Features (E));

            Fifo_Size_Values    := Make_Array_Values;
            N_Dest_Values       := Make_Array_Values;
            Destinations_Values := Make_Array_Values;

            Deliver_Declarations := New_List (CTN.K_Declaration_List);
            Deliver_Statements   := New_List (CTN.K_Statement_List);
            Deliver_Alternatives := New_List (CTN.K_Alternatives_List);

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then

                  if Is_Out (F) then
                     Destinations := Get_Destination_Ports (F, Current_Device);
                     Local_Dest_Values := Make_Array_Values;

                     if AAU.Is_Empty (Destinations) then
                        Display_Located_Error
                          (Loc (F),
                           "This OUT port is not connected to any" &
                             " destination",
                           Fatal => True);
                     end if;

                     D := First_Node (Destinations);

                     Nb_Dest := 0;

                     while Present (D) loop
                        Append_Node_To_List
                          (Make_Defining_Identifier
                             (Map_C_Enumerator_Name
                                (Item (D),
                                 Port_Type => True)),
                           CTN.Values (Local_Dest_Values));

                        Nb_Dest := Nb_Dest + 1;

                        D := Next_Node (D);
                     end loop;

                     --  Make the array which indicate all destinations
                     --  for an out port

                     N :=
                       Make_Expression
                         (Left_Expr =>
                            Make_Variable_Declaration
                              (Defining_Identifier =>
                                 Make_Array_Declaration
                                   (Defining_Identifier =>
                                      Make_Defining_Identifier
                                        (Map_C_Variable_Name
                                           (F,
                                            Port_Local_Dest => True)),
                                    Array_Size =>
                                      Make_Literal
                                        (CV.New_Int_Value (Nb_Dest, 0, 10))),
                               Used_Type => RE (RE_Port_T)),
                          Operator   => Op_Equal,
                          Right_Expr => Local_Dest_Values);
                     Append_Node_To_List (N, CTN.Declarations (Current_File));

                     --  Add the last array name in the destination array

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_C_Variable_Name (F, Port_Local_Dest => True)),
                        CTN.Values (Destinations_Values));

                     --  Add the number of destinations in the nb_dest array

                     N := RE (RE_Gqueue_Fifo_Out);
                     Append_Node_To_List
                       (Make_Literal (CV.New_Int_Value (Nb_Dest, 0, 10)),
                        CTN.Values (N_Dest_Values));

                     Append_Node_To_List (N, CTN.Values (Fifo_Size_Values));
                  else
                     if AAN.Is_Data (F) and then not Is_Event (F) then

                        Has_Local_Deliver := True;
                        N                 := RE (RE_Gqueue_Fifo_Indata);
                        Queue_Size        := 0;
                     else

                        Has_Local_Deliver := True;

                        Queue_Size := Get_Queue_Size (F);

                        if Queue_Size = -1 then
                           Queue_Size := Default_Queue_Size;

                           N :=
                             Make_Literal
                               (CV.New_Int_Value (Default_Queue_Size, 0, 10));
                        elsif Queue_Size = 0 then
                           --  0 length queues are not supported

                           Display_Located_Error
                             (Loc (F),
                              "Zero length port queues are not supported",
                              Fatal => True);
                        else
                           N :=
                             Make_Literal
                               (CV.New_Int_Value
                                  (Unsigned_Long_Long (Queue_Size),
                                   0,
                                   10));
                        end if;
                     end if;

                     Append_Node_To_List (N, CTN.Values (Fifo_Size_Values));

                     Append_Node_To_List
                       (Make_Literal (CV.New_Int_Value (0, 0, 10)),
                        CTN.Values (N_Dest_Values));

                     N :=
                       Make_Defining_Identifier
                         (CONST (C_Null),
                          C_Conversion => False);
                     Append_Node_To_List (N, CTN.Values (Destinations_Values));

                     --  Make the switch alternative for the deliver function

                     Parameters        := New_List (CTN.K_Parameter_List);
                     Switch_Statements := New_List (CTN.K_Statement_List);
                     Switch_Labels     := New_List (CTN.K_Label_List);

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_C_Enumerator_Name (F, Port_Type => True)),
                        Switch_Labels);

                     Append_Node_To_List
                       (Make_Defining_Identifier (Map_C_Enumerator_Name (S)),
                        Parameters);

                     Append_Node_To_List
                       (Make_Defining_Identifier
                          (Map_C_Enumerator_Name (F, Local_Port => True)),
                        Parameters);

                     Append_Node_To_List
                       (Make_Defining_Identifier (PN (P_Request)),
                        Parameters);

                     N :=
                       Make_Call_Profile (RE (RE_Gqueue_Store_In), Parameters);

                     Append_Node_To_List (N, Switch_Statements);

                     N :=
                       Make_Switch_Alternative
                         (Switch_Labels,
                          Switch_Statements);
                     Append_Node_To_List (N, Deliver_Alternatives);

                     Fifo_Size := Fifo_Size + Unsigned_Long_Long (Queue_Size);
                  end if;

               end if;
               F := Next_Node (F);
            end loop;

            --  Declare all the needed tables by each thread in order
            --  to handle the ports.

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_Woffsets => True)),
                      Array_Size =>
                        Make_Defining_Identifier
                          (Map_C_Define_Name (S, Nb_Ports => True))),
                 Used_Type => RE (RE_Port_Id_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_Offsets => True)),
                      Array_Size =>
                        Make_Defining_Identifier
                          (Map_C_Define_Name (S, Nb_Ports => True))),
                 Used_Type => RE (RE_Port_Id_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_Used_Size => True)),
                      Array_Size =>
                        Make_Defining_Identifier
                          (Map_C_Define_Name (S, Nb_Ports => True))),
                 Used_Type => RE (RE_Port_Id_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_Empties => True)),
                      Array_Size =>
                        Make_Defining_Identifier
                          (Map_C_Define_Name (S, Nb_Ports => True))),
                 Used_Type => RE (RE_Port_Id_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_First => True)),
                      Array_Size =>
                        Make_Defining_Identifier
                          (Map_C_Define_Name (S, Nb_Ports => True))),
                 Used_Type => RE (RE_Port_Id_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_Recent => True)),
                      Array_Size =>
                        Make_Defining_Identifier
                          (Map_C_Define_Name (S, Nb_Ports => True))),
                 Used_Type => RE (RE_Request_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_Queue => True)),
                      Array_Size =>
                        Make_Literal
                          (CV.New_Int_Value (Fifo_Size, 0, 10))),
                 Used_Type => RE (RE_Request_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Expression
                     (Left_Expr =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_Total_Fifo => True)),
                      Operator   => Op_Equal,
                      Right_Expr =>
                        Make_Literal (CV.New_Int_Value (Fifo_Size, 0, 10))),
                 Used_Type => RE (RE_Uint16_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Variable_Declaration
                (Defining_Identifier =>
                   Make_Array_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (Map_C_Variable_Name (S, Port_History => True)),
                      Array_Size =>
                        Make_Literal (CV.New_Int_Value (Fifo_Size, 0, 10))),
                 Used_Type => RE (RE_Local_Port_T));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (Map_C_Variable_Name (S, Port_N_Dest => True)),
                           Array_Size =>
                             Make_Defining_Identifier
                               (Map_C_Define_Name (S, Nb_Ports => True))),
                      Used_Type => RE (RE_Port_Id_T)),
                 Operator   => Op_Equal,
                 Right_Expr => N_Dest_Values);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (Map_C_Variable_Name
                                  (S,
                                   Port_Fifo_Size => True)),
                           Array_Size =>
                             Make_Defining_Identifier
                               (Map_C_Define_Name (S, Nb_Ports => True))),
                      Used_Type => RE (RE_Port_Id_T)),
                 Operator   => Op_Equal,
                 Right_Expr => Fifo_Size_Values);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Expression
                (Left_Expr =>
                   Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (Map_C_Variable_Name
                                  (S,
                                   Port_Destinations => True)),
                           Array_Size =>
                             Make_Defining_Identifier
                               (Map_C_Define_Name (S, Nb_Ports => True))),
                      Used_Type => Make_Pointer_Type (RE (RE_Port_T))),
                 Operator   => Op_Equal,
                 Right_Expr => Destinations_Values);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            Append_Node_To_List
              (Make_Switch_Alternative (No_List, No_List),
               Deliver_Alternatives);

            if Has_Local_Deliver then
               N :=
                 Make_Switch_Statement
                   (Expression =>
                      Make_Member_Designator
                        (Defining_Identifier =>
                           Make_Defining_Identifier (MN (M_Port)),
                         Aggregate_Name =>
                           Make_Defining_Identifier (VN (V_Request)),
                         Is_Pointer => True),
                    Alternatives => Deliver_Alternatives);

               Append_Node_To_List (N, Deliver_Statements);

               --  Make the deliver function specific to a thread
               N :=
                 Make_Doxygen_C_Comment
                   (Is_Function  => True,
                    Element_Name =>
                      "void " &
                        Get_Name_String
                        (CTN.Name
                           (CTN.Defining_Identifier
                              (CTN.Global_Port_Node
                                 (Backend_Node (Identifier (S)))))) &
                        " (__po_hi_request_t* request)",
                    Brief =>
                      "Function that delivers requests to the task " &
                        Get_Name_String (Name (Identifier (S))),
                    Desc =>
                      "When the generated application received a request," &
                        " it calls a main delivery function that redirects" &
                        " to localfunctions for each task. This function (" &
                        Get_Name_String
                        (CTN.Name
                           (CTN.Defining_Identifier
                              (CTN.Global_Port_Node
                                 (Backend_Node (Identifier (S)))))) &
                        ") stores the incoming request for the task" &
                        Get_Name_String (Name (Identifier (S))),
                    Has_Header_Spaces => False);
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               N :=
                 Make_Function_Implementation
                   (CTN.Global_Port_Node (Backend_Node (Identifier (S))),
                    Deliver_Declarations,
                    Deliver_Statements);
               Append_Node_To_List (N, CTN.Declarations (Current_File));

               --  Add a switch alternative to the main deliver
               --  in order to used our local delivery function.

               Parameters        := New_List (CTN.K_Parameter_List);
               Switch_Statements := New_List (CTN.K_Statement_List);
               Switch_Labels     := New_List (CTN.K_Label_List);

               Append_Node_To_List
                 (Make_Defining_Identifier
                    (Map_C_Enumerator_Name (S, Entity => True)),
                  Switch_Labels);

               Append_Node_To_List
                 (Make_Defining_Identifier (VN (V_Request)),
                  Parameters);

               N :=
                 Make_Call_Profile
                   (Defining_Identifier => Map_Task_Deliver_Identifier (S),
                    Parameters          => Parameters);

               Append_Node_To_List (N, Switch_Statements);

               N := Make_Switch_Alternative (Switch_Labels, Switch_Statements);

               Append_Node_To_List (N, Main_Deliver_Alternatives);
            end if;
         end if;

         N :=
           Make_Doxygen_C_Comment
             (Is_Function  => True,
              Element_Name =>
                "void* " &
                  Get_Name_String
                  (CTN.Name (Map_Task_Job_Identifier (S, Current_Device))) &
                  " (void)",
              Brief =>
                "Function executed by the task " &
                  Get_Name_String (Name (Identifier (S))),
              Desc =>
                "This function is executed as soon as the task " &
                  " is created. It performs the following operations: " &
                  " Receive incoming data, " &
                  " Execute tasks subprograms, " &
                  " Send output data.",
              Has_Header_Spaces => False);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         N := Task_Job_Body (E);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

      end Visit_Thread_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_C.Activity;
