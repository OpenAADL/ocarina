------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . P O _ H I _ R T S J . M A I N      --
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

with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.PO_HI_RTSJ.Mapping;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;

package body Ocarina.Backends.PO_HI_RTSJ.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.PO_HI_RTSJ.Mapping;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;

   -----------------
   -- Source_File --
   -----------------
   package body Source_File is

      Main_Class : Node_Id;

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);

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

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------
      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (RTSJ_Root);

         --  Visit all the subcomponents of the system
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := AIN.Next_Node (S);
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
         P                   : constant Node_Id := RTN.Entity (U);
         Exec_Logic          : Node_Id;
         Thread_Init         : Node_Id;
         Spec                : Node_Id;
         Impl                : Node_Id;
         N                   : Node_Id;
         Thread_Init_Methods : constant List_Id :=
           New_List (RTN.K_Method_List);
         Main_Statements : constant List_Id := New_List (RTN.K_Statement_List);
         Exec_Logic_Statements : constant List_Id :=
           New_List (RTN.K_Statement_List);
         Constructor_Statements : constant List_Id :=
           New_List (RTN.K_Statement_List);
         Thread_Init_Statements : constant List_Id :=
           New_List (RTN.K_Statement_List);
      begin
         Push_Entity (P);
         Push_Entity (U);
         RTU.Set_Main_Source;

         Add_Import (RH (RH_Immortal_Memory));
         Add_Import (RH (RH_No_Heap_Realtime_Thread));

         --  ExecutionLogic class
         N :=
           Make_Variable_Declaration
             (Used_Type =>
                Make_Defining_Identifier (ON (O_Thread_Initializer)),
              Defining_Identifier =>
                Make_Defining_Identifier (VN (V_Initializer)),
              Value => Make_Null_Statement);
         RTU.Append_Node_To_List (N, Exec_Logic_Statements);

         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Defining_Identifier (VN (V_Initializer)),
              Expression =>
                Make_New_Statement
                  (Defining_Identifier =>
                     Make_Defining_Identifier (ON (O_Thread_Initializer))));
         RTU.Append_Node_To_List (N, Exec_Logic_Statements);

         N :=
           Make_Pointed_Notation
             (Left_Member  => Make_Defining_Identifier (VN (V_Initializer)),
              Right_Member =>
                Make_Call_Function
                  (Defining_Identifier =>
                     Make_Defining_Identifier (MN (M_Start))));
         RTU.Append_Node_To_List (N, Exec_Logic_Statements);

         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier => Make_Defining_Identifier (MN (M_Run)),
              Return_Type         => New_Node (K_Void));

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Exec_Logic_Statements);

         Exec_Logic :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Static)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Execution_Logic)),
              Implements =>
                Make_List_Id (Make_Defining_Identifier (ON (O_Runnable))),
              Methods => Make_List_Id (Impl));

         --  ThreadInitializer class

         --  Constructor of class ThreadInitializer
         N :=
           Make_Call_Function
             (Defining_Identifier => Make_Defining_Identifier (MN (M_Super)),
              Parameters          =>
                Make_List_Id
                  (Make_Pointed_Notation
                     (Left_Member  => Make_Defining_Identifier (ON (O_Utils)),
                      Right_Member => RE (RE_Max_Priority_Parameters)),
                   (Make_Pointed_Notation
                      (Left_Member =>
                         Make_Defining_Identifier (ON (O_Immortal_Memory)),
                       Right_Member =>
                         Make_Call_Function
                           (Defining_Identifier =>
                              Make_Defining_Identifier (MN (M_Instance)))))));
         RTU.Append_Node_To_List (N, Constructor_Statements);

         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Thread_Initializer)),
              Return_Type => New_Node (K_Void));

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Constructor_Statements);
         RTU.Append_Node_To_List (Impl, Thread_Init_Methods);

         N :=
           Make_Pointed_Notation
             (Left_Member  => Make_Defining_Identifier (ON (O_Activity)),
              Right_Member =>
                Make_Call_Function
                  (Defining_Identifier =>
                     Make_Defining_Identifier (MN (M_Initialization))));
         RTU.Append_Node_To_List (N, Thread_Init_Statements);

         N := Message_Comment ("Unblock all users tasks");
         RTU.Append_Node_To_List (N, Thread_Init_Statements);

         N :=
           Make_Pointed_Notation
             (Left_Member  => Make_Defining_Identifier (ON (O_Suspenders)),
              Right_Member =>
                Make_Call_Function
                  (Defining_Identifier => RE (RE_Unblock_All_Tasks)));
         RTU.Append_Node_To_List (N, Thread_Init_Statements);

         N :=
           Message_Comment
             ("Suspend forever instead of putting an endless loop." &
              "This saves the CPU resources");
         RTU.Append_Node_To_List (N, Thread_Init_Statements);

         N :=
           Make_Pointed_Notation
             (Left_Member  => Make_Defining_Identifier (ON (O_Suspenders)),
              Right_Member =>
                Make_Call_Function
                  (Defining_Identifier => RE (RE_Suspend_Forever)));
         RTU.Append_Node_To_List (N, Thread_Init_Statements);

         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier => Make_Defining_Identifier (MN (M_Run)),
              Return_Type         => New_Node (K_Void));

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Thread_Init_Statements);
         RTU.Append_Node_To_List (Impl, Thread_Init_Methods);

         Thread_Init :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Static)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Thread_Initializer)),
              Extends =>
                Make_Defining_Identifier (ON (O_No_Heap_Realtime_Thread)),
              Methods => Thread_Init_Methods);

         --  Main class
         N :=
           Make_Variable_Declaration
             (Used_Type => Make_Defining_Identifier (ON (O_Execution_Logic)),
              Defining_Identifier =>
                Make_Defining_Identifier (VN (V_Init_Logic)),
              Value => Make_Null_Statement);
         RTU.Append_Node_To_List (N, Main_Statements);

         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Defining_Identifier (VN (V_Init_Logic)),
              Expression =>
                Make_New_Statement
                  (Defining_Identifier =>
                     Make_Defining_Identifier (ON (O_Execution_Logic))));
         RTU.Append_Node_To_List (N, Main_Statements);

         N :=
           Make_Pointed_Notation
             (Left_Member => Make_Defining_Identifier (ON (O_Immortal_Memory)),
              Right_Member =>
                Make_Pointed_Notation
                  (Left_Member =>
                     Make_Call_Function
                       (Defining_Identifier =>
                          Make_Defining_Identifier (MN (M_Instance))),
                   Right_Member =>
                     Make_Call_Function
                       (Defining_Identifier =>
                          Make_Defining_Identifier (MN (M_Execute_In_Area)),
                        Parameters =>
                          Make_List_Id
                            (Make_Defining_Identifier (VN (V_Init_Logic))))));
         RTU.Append_Node_To_List (N, Main_Statements);

         N := Message_Comment ("Main thread is also suspended forever");
         RTU.Append_Node_To_List (N, Main_Statements);

         N :=
           Make_Pointed_Notation
             (Left_Member  => Make_Defining_Identifier (ON (O_Suspenders)),
              Right_Member =>
                Make_Call_Function
                  (Defining_Identifier => RE (RE_Suspend_Forever)));
         RTU.Append_Node_To_List (N, Main_Statements);

         Spec :=
           Make_Function_Specification
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Return_Type         => New_Node (K_Void),
              Defining_Identifier => Make_Defining_Identifier (MN (M_Main)),
              Parameters          =>
                Make_List_Id
                  (Make_Parameter_Specification
                     (Parameter_Type      => New_Node (K_String),
                      Defining_Identifier =>
                        Make_Array_Declaration
                          (Defining_Identifier =>
                             Make_Defining_Identifier (VN (V_Args))))));

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Main_Statements);

         Main_Class :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier =>
                Map_RTSJ_Defining_Identifier (Parent_Subcomponent (E)),
              Methods => Make_List_Id (Impl),
              Classes => Make_List_Id (Exec_Logic, Thread_Init));
         RTU.Append_Node_To_List (Main_Class, RTN.Statements (Current_File));

         Pop_Entity;  --  U
         Pop_Entity;  --  P
      end Visit_Process_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_RTSJ.Main;
