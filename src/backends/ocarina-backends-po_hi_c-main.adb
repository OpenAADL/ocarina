------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--        O C A R I N A . B A C K E N D S . P O _ H I _ C . M A I N         --
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

with Ocarina.Namet; use Ocarina.Namet;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.PO_HI_C;
with Ocarina.Backends.PO_HI_C.Runtime;
with Ocarina.Backends.C_Common.Mapping;

package body Ocarina.Backends.PO_HI_C.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Values;
   use Ocarina.Backends.PO_HI_C.Runtime;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.Utils;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;

   ---------------------
   -- Subprogram_Body --
   ---------------------

   package body Source_File is

      Main_Function            : Node_Id;
      Current_Device           : Node_Id := No_Node;
      Period_Variable_Declared : Boolean := False;

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);

      procedure Setup_Thread (E : Node_Id);
      --  Create a task according to all its properties
      --  (like period, priority, ...). The task creation
      --  is made in the main function, calling the function
      --  __po_hi_create_<>_task().

      Ada_Initialized : Boolean := False;

      ------------------
      -- Setup_Thread --
      ------------------

      procedure Setup_Thread (E : Node_Id) is
         N          : Node_Id;
         Parameters : List_Id;
         Priority   : Unsigned_Long_Long;
         Stack_Size : Unsigned_Long_Long;
         Core_Id    : Unsigned_Long_Long;
         S          : constant Node_Id := Parent_Subcomponent (E);
      begin
         Parameters := New_List (CTN.K_Parameter_List);

         --  Add the task name to the parameters list

         N :=
           Make_Defining_Identifier
             (Map_C_Enumerator_Name (S, Custom_Parent => Current_Device));
         Append_Node_To_List (N, Parameters);

         --  Add the period of the task to the parameters list. We use
         --  the fact that an aperiodic thread is sporadic, with
         --  period of 0.
         if Period_Variable_Declared = False then
            N :=
              Make_Doxygen_C_Comment
                (Brief       => "Variable for task period",
                 Is_Variable => True,
                 Desc        =>
                   "This variable is used to store the value" &
                   "of the period of a task when we create it. The " &
                   "value put in the variable is set according to " &
                   "AADL model description",
                 Element_Name      => Get_Name_String (VN (V_Period)),
                 Has_Header_Spaces => False);
            Append_Node_To_List (N, CTN.Declarations (Main_Function));

            N :=
              Make_Variable_Declaration
                (Make_Defining_Identifier (VN (V_Period)),
                 RE (RE_Time_T));
            Append_Node_To_List (N, CTN.Declarations (Main_Function));

            Period_Variable_Declared := True;
         end if;

         N :=
           Make_Doxygen_C_Comment
             (Desc =>
                "Store the period time for task " &
                Get_Name_String (Name (Identifier (S))),
              Has_Header_Spaces => False);
         Append_Node_To_List (N, CTN.Statements (Main_Function));

         if Get_Thread_Dispatch_Protocol (E) /= Thread_Aperiodic
           and then Get_Thread_Dispatch_Protocol (E) /= Thread_Background
         then
            N := Map_Time (Get_Thread_Period (E), VN (V_Period));

         else
            N := Map_Time ((0, Second), VN (V_Period));
         end if;

         Append_Node_To_List
           (Make_Variable_Address (Make_Defining_Identifier (VN (V_Period))),
            Parameters);

         Append_Node_To_List (N, CTN.Statements (Main_Function));

         --  Add the priority of the task in the parameters list. If
         --  the task does not have any priority, we use the macro
         --  DEFAULT_PRIORITY.

         Priority := Get_Thread_Priority (E);
         if Priority = 0 then
            N := RE (RE_Default_Priority);
         else
            N := Make_Literal (New_Int_Value (Priority, 1, 10));
         end if;
         Append_Node_To_List (N, Parameters);

         --  Add thread stack size

         Stack_Size := To_Bytes (Get_Thread_Stack_Size (E));
         N          := Make_Literal (New_Int_Value (Stack_Size, 1, 10));
         Append_Node_To_List (N, Parameters);

         --  Add the core this thread is attached to

         Core_Id := Get_Core_Id (Get_Bound_Processor
                                   (Corresponding_Instance
                                      (Get_Container_Process (E))));

         N          := Make_Literal (New_Int_Value (Core_Id, 1, 10));
         Append_Node_To_List (N, Parameters);

         --  Add the name of function executed by the task in the
         --  parameters list.

         N :=
           Copy_Node
             (CTN.Defining_Identifier
                (CTN.Job_Node (Backend_Node (Identifier (S)))));
         Append_Node_To_List (N, Parameters);

         case Get_Thread_Dispatch_Protocol (E) is
            when Thread_Periodic =>
               N :=
                 Make_Doxygen_C_Comment
                   (Brief =>
                      "Making Periodic Task " &
                      Get_Name_String (Name (Identifier (S))),
                    Desc =>
                      "Make a periodic task according to " &
                      "AADL model requirements. The first " &
                      "parameter is the task identifier defined " &
                      "in deployment.h (" &
                      Get_Name_String
                        (Map_C_Enumerator_Name
                           (S,
                            Custom_Parent => Current_Device)) &
                      ") the second is the period " &
                      "defined in the AADL model. Third is the task " &
                      "priority (" &
                      Unsigned_Long_Long'Image (Get_Thread_Priority (E)) &
                      "), fourth is the stack size (" &
                      Unsigned_Long_Long'Image
                        (To_Bytes (Get_Thread_Stack_Size (E))) &
                      " bytes) and last is the subprogram executed " &
                      "by the task",
                    Has_Header_Spaces => False);
               Append_Node_To_List (N, CTN.Statements (Main_Function));

               Append_Node_To_List
                 (CTU.Make_Call_Profile
                    (RE (RE_Create_Periodic_Task),
                     Parameters),
                  CTN.Statements (Main_Function));

            when Thread_Sporadic | Thread_Aperiodic =>
               N :=
                 Make_Doxygen_C_Comment
                   ("Making Sporadic task",
                    Has_Header_Spaces => False);
               Append_Node_To_List (N, CTN.Statements (Main_Function));

               Append_Node_To_List
                 (CTU.Make_Call_Profile
                    (RE (RE_Create_Sporadic_Task),
                     Parameters),
                  CTN.Statements (Main_Function));

            when Thread_Background =>
               N :=
                 Make_Doxygen_C_Comment
                   ("Making background task",
                    Has_Header_Spaces => False);
               Append_Node_To_List (N, CTN.Statements (Main_Function));

               Append_Node_To_List
                 (CTU.Make_Call_Profile
                    (RE (RE_Create_Sporadic_Task),
                     Parameters),
                  CTN.Statements (Main_Function));

            when others =>
               Display_Located_Error
                 (Loc (E),
                  "Thread kind is not supported",
                  Fatal => True);
         end case;
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

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

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
         N            : Node_Id;
         C            : Node_Id;
         S            : Node_Id;
         Spec         : Node_Id;
         Declarations : constant List_Id := New_List (CTN.K_Declaration_List);
         Statements   : constant List_Id := New_List (CTN.K_Statement_List);
         The_System   : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
      begin
         Push_Entity (P);
         Push_Entity (U);

         Set_Main_Source;
         Ada_Initialized := False;

         Period_Variable_Declared := False;

         Add_Include (E => RH (RH_Activity));

         --  Make the main function specification and add it in the current
         --  file (main.c).

         Spec :=
           Make_Function_Specification
             (Defining_Identifier => RE (RE_Main_Name),
              Parameters          => No_List,
              Return_Type         => RE (RE_Main_Type));
         Main_Function :=
           Make_Function_Implementation (Spec, Declarations, Statements);

         N := CTU.Make_Call_Profile (RE (RE_Initialize_Early));
         Append_Node_To_List (N, CTN.Statements (Main_Function));

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.
         --  This is done to initialize all devices before system starts.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AAU.Is_Device (Corresponding_Instance (C))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (C)) =
                   Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (C));
               end if;
               C := Next_Node (C);
            end loop;
         end if;

         N :=
           Make_Doxygen_C_Comment
             ("Initialize the runtime",
              Has_Header_Spaces => False);
         Append_Node_To_List (N, CTN.Statements (Main_Function));

         N := CTU.Make_Call_Profile (RE (RE_Initialize));
         Append_Node_To_List (N, CTN.Statements (Main_Function));

         if Process_Use_Defaults_Sockets (E) then
            Add_Include
              (Make_Include_Clause
                 (Make_Defining_Identifier
                    (Get_String_Name ("drivers/po_hi_driver_sockets"))));
            N :=
              Make_Call_Profile
                (Make_Defining_Identifier
                   (Get_String_Name ("__po_hi_driver_sockets_init")),
                 Make_List_Id (Make_Literal (New_Int_Value (0, 0, 10))));
            Append_Node_To_List (N, CTN.Statements (Main_Function));
         end if;

         --  Visit all threads and see if there is an initialize entrypoint.
         --  In that case, call the initialize entrypoint.

         --  There is two ways to deal with initialize entrypoint. The
         --  entrypoint can point a subprogram classifier (a node)
         --  or just reference text. We have to handle both cases.

         if not AAU.Is_Empty (Subcomponents (E)) then
            C := First_Node (Subcomponents (E));
            while Present (C) loop

               --  First, handle the case when the initialize_entrypoint
               --  is a subprogram classifier reference.

               if AAU.Is_Thread (Corresponding_Instance (C))
                 and then
                   Get_Thread_Initialize_Entrypoint
                     (Corresponding_Instance (C)) /=
                   No_Node
               then
                  Append_Node_To_List
                    (Make_Extern_Entity_Declaration
                       (Make_Function_Specification
                          (Map_C_Subprogram_Identifier
                             (Get_Thread_Initialize_Entrypoint
                                (Corresponding_Instance (C))),
                           Parameters  => No_List,
                           Return_Type => New_Node (CTN.K_Void))),
                     CTN.Declarations (Current_File));

                  Append_Node_To_List
                    (Make_Call_Profile
                       (Map_C_Subprogram_Identifier
                          (Get_Thread_Initialize_Entrypoint
                             (Corresponding_Instance (C))),
                        No_List),
                     CTN.Statements (Main_Function));
               end if;

               --  Then, handle the case when the initialize entrypoint
               --  is just a string.

               if AAU.Is_Thread (Corresponding_Instance (C))
                 and then
                   Get_Thread_Initialize_Entrypoint
                     (Corresponding_Instance (C)) /=
                   No_Name
               then
                  Append_Node_To_List
                    (Make_Extern_Entity_Declaration
                       (Make_Function_Specification
                          (Make_Defining_Identifier
                             (Get_Thread_Initialize_Entrypoint
                                (Corresponding_Instance (C))),
                           Parameters  => No_List,
                           Return_Type => New_Node (CTN.K_Void))),
                     CTN.Declarations (Current_File));

                  Append_Node_To_List
                    (Make_Call_Profile
                       (Make_Defining_Identifier
                          (Get_Thread_Initialize_Entrypoint
                             (Corresponding_Instance (C))),
                        No_List),
                     CTN.Statements (Main_Function));
               end if;

               C := Next_Node (C);
            end loop;
         end if;

         --  Here, we should automatically initialize the sockets layer

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Handle protected data, the following lines initialize
               --  the protected_id attribute for each protected data
               --  The generated code will look like :
               --  data_variable.protected_id = value.

               if AAU.Is_Data (Corresponding_Instance (S)) then
                  --  Automatically use the types.h header if we use
                  --  protected data.

                  Add_Include (RH (RH_Types));

                  N :=
                    Make_Variable_Declaration
                      (Map_C_Defining_Identifier (S),
                       Map_C_Data_Type_Designator
                         (Corresponding_Instance (S)));

                  Append_Node_To_List (N, CTN.Declarations (Current_File));

                  if Get_Concurrency_Protocol (Corresponding_Instance (S))
                     /= None_Specified
                  then
                     N :=
                     Make_Expression
                        (Left_Expr =>
                           Make_Member_Designator
                              (Defining_Identifier =>
                                 Make_Defining_Identifier
                                    (MN (M_Protected_Id)),
                              Aggregate_Name => Map_C_Defining_Identifier (S)),
                        Operator   => Op_Equal,
                        Right_Expr =>
                         CTN.Default_Value_Node
                           (Backend_Node (Identifier (S))));
                     Append_Node_To_List (N, CTN.Statements (Main_Function));
                  end if;
               else
                  --  Visit the component instance corresponding to the
                  --  subcomponent S.

                  Visit (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         N :=
           Make_Doxygen_C_Comment
             (Is_Function  => True,
              Element_Name => "__PO_HI_MAIN_TYPE __PO_HI_MAIN_NAME (void)",
              Brief        => "Main function executed by the system",
              Desc         =>
                "Full function name and return types are available " &
                " in the PolyORB-HI-C runtime header files.",
              Has_Header_Spaces => False);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         Append_Node_To_List (Main_Function, CTN.Declarations (Current_File));
         --  Call __po_hi_wait_initialization(). With this function,
         --  the main function will wait all other tasks initialization.

         N :=
           Make_Doxygen_C_Comment
             ("Waiting for other tasks initialization",
              Has_Header_Spaces => False);
         Append_Node_To_List (N, Statements);

         N := CTU.Make_Call_Profile (RE (RE_Wait_Initialization), No_List);
         Append_Node_To_List (N, Statements);

         --  Make the call to __po_hi_wait_for_tasks(). This function will wait
         --  all other task. In fact, no task will terminate, so this function
         --  will only switch the main task to the sleep state all the time.

         N :=
           Make_Doxygen_C_Comment
             ("Used to switch the main task to sleep all the time",
              Has_Header_Spaces => False);
         Append_Node_To_List (N, CTN.Statements (Main_Function));

         if not PO_HI_C.Use_Performance_Analysis then
            N := CTU.Make_Call_Profile (RE (RE_Wait_For_Tasks));
         else
            N := CTU.Make_Call_Profile (RE (RE_Wait_End_Of_Instrumentation));
         end if;

         Append_Node_To_List (N, CTN.Statements (Main_Function));

         N :=
           Make_Doxygen_C_Comment
             ("Return Statement",
              Has_Header_Spaces => False);
         Append_Node_To_List (N, CTN.Statements (Main_Function));

         N := CTU.Make_Return_Statement (RE (RE_Main_Return));
         Append_Node_To_List (N, CTN.Statements (Main_Function));

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

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
         S        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;

         Setup_Thread (E);
      end Visit_Thread_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
--         Initialize_Function_Name : Name_Id;
--         Start_Function_Name : Name_Id;
      begin
         if Get_Subprogram_Kind (E) = Subprogram_Simulink then
            Add_Include
              (Make_Include_Clause
                 (Make_Defining_Identifier (Get_Source_Name (E), False),
                  False),
               True);

--            Set_Str_To_Name_Buffer ("MdlInitialize");

--            Initialize_Function_Name := Name_Find;

--            Append_Node_To_List
--               (CTU.Make_Call_Profile
--    (Make_Defining_Identifier (Initialize_Function_Name, False),
--                  No_List),
--               CTN.Declarations (Main_Function));

--            Set_Str_To_Name_Buffer ("MdlStart");

--            Start_Function_Name := Name_Find;

--            Append_Node_To_List
--               (CTU.Make_Call_Profile
--      (Make_Defining_Identifier (Start_Function_Name, False),
--                  No_List),
--               CTN.Declarations (Main_Function));

            Append_Node_To_List
              (CTU.Make_Call_Profile (RE (RE_Simulink_Init), No_List),
               CTN.Declarations (Main_Function));

         elsif Get_Subprogram_Kind (E) = Subprogram_Opaque_Ada_95
           and then not Ada_Initialized
         then
            Ada_Initialized := True;
            declare
               Parameter_List : constant List_Id := New_List (CTN.K_List_Id);
               N              : Node_Id;

            begin
               Set_Str_To_Name_Buffer ("adainit");
               N :=
                 Make_Extern_Entity_Declaration
                   (Make_Function_Specification
                      (Make_Defining_Identifier (Name_Find),
                       Parameters  => Parameter_List, --  XXX
                       Return_Type => New_Node (CTN.K_Void)));
               Append_Node_To_List (N, CTN.Declarations (Main_Function));

               Set_Str_To_Name_Buffer ("adainit");
               Append_Node_To_List
                 (CTU.Make_Call_Profile
                    (Make_Defining_Identifier (Name_Find),
                     No_List),
                  CTN.Statements (Main_Function));
            end;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         N          : Node_Id;
         S          : Node_Id;
         Entrypoint : constant Node_Id := Get_Thread_Initialize_Entrypoint (E);
         Impl       : constant Node_Id := Get_Implementation (E);
      begin
         Current_Device := E;

         if Entrypoint /= No_Node then
            N :=
              Make_Extern_Entity_Declaration
                (Make_Function_Specification
                   (Map_C_Subprogram_Identifier (Entrypoint),
                    Make_List_Id
                      (Make_Parameter_Specification
                         (Make_Defining_Identifier (Get_String_Name ("id")),
                          RE (RE_Device_Id))),
                    New_Node (CTN.K_Void)));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            N :=
              Make_Call_Profile
                (Map_C_Subprogram_Identifier (Entrypoint),
                 Make_List_Id
                   (Make_Defining_Identifier (Map_C_Enumerator_Name (E))));
            Append_Node_To_List (N, CTN.Statements (Main_Function));
         end if;

         if Impl /= No_Node then
            if not AAU.Is_Empty (Subcomponents (Impl)) then
               S := First_Node (Subcomponents (Impl));
               while Present (S) loop
                  --  Visit the component instance corresponding to the
                  --  subcomponent S.

                  Visit (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end if;

         Current_Device := No_Node;
      end Visit_Device_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_C.Main;
