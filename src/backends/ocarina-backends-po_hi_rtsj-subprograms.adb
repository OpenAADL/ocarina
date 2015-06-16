------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.PO_HI_RTSJ.SUBPROGRAMS                  --
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
with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.PO_HI_RTSJ.Mapping;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;

package body Ocarina.Backends.PO_HI_RTSJ.Subprograms is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.PO_HI_RTSJ.Mapping;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;

   Main_Class    : Node_Id;
   Class_Methods : List_Id;

   -----------------
   -- Source_File --
   -----------------
   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------
      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (RTSJ_Root);

         --  Visit all the subcomponents of the system
         if not AAU.Is_Empty (Subcomponents (E)) then
            S := AAN.First_Node (Subcomponents (E));

            while Present (S) loop

               --  Visit the component instance corresponding to the
               --  subcomponent S.
               Visit (Corresponding_Instance (S));
               S := AAN.Next_Node (S);

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
         P : constant Node_Id := RTN.Entity (U);
         S : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         RTU.Set_Subprograms_Source;

         --  Start recording all the instance
         Start_Recording_Handlings;

         --  Initialization
         Class_Methods := New_List (K_Method_List);

         --  Visit all the subcomponents of the process
         if not AAU.Is_Empty (Subcomponents (E)) then
            S := AAN.First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.
               Visit (Corresponding_Instance (S));
               S := AAN.Next_Node (S);
            end loop;
         end if;

         --  Subprograms class
         Main_Class :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Subprograms)),
              Methods => Class_Methods);
         RTU.Append_Node_To_List (Main_Class, RTN.Statements (Current_File));

         --  Unmark all the marked subprograms
         Reset_Handlings;

         Pop_Entity; -- P
         Pop_Entity; -- U
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------
      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Visit all the call sequences of the thread
         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := AAN.First_Node (Calls (E));

            while Present (Call_Seq) loop

               --  For each call sequence visit all the called
               --  subprograms.
               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := AAN.First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));
                     Spg_Call := AAN.Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := AAN.Next_Node (Call_Seq);
            end loop;
         end if;

      end Visit_Thread_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------
      procedure Visit_Subprogram_Instance (E : Node_Id) is
         N                     : Node_Id;
         Call_Seq              : Node_Id;
         Spg_Call              : Node_Id;
         Spec                  : Node_Id;
         Impl                  : Node_Id;
         Subprogram_Statements : constant List_Id :=
           New_List (K_Statement_List);
      begin
         --  Generate the body of the subprogram
         if No (Get_Handling (E, By_Name, H_RTSJ_Subprogram_Body)) then
            N := Map_RTSJ_Subprogram_Body (E);
            Append_Node_To_List (N, Subprogram_Statements);

            Spec :=
              Make_Function_Specification
                (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
                 Defining_Identifier => Map_RTSJ_Defining_Identifier (E),
                 Return_Type         => New_Node (K_Void));
            Impl :=
              Make_Function_Implementation
                (Specification => Spec,
                 Statements    => Subprogram_Statements);
            RTU.Append_Node_To_List (Impl, Class_Methods);

            --  Mark the data type as being handled
            Set_Handling (E, By_Name, H_RTSJ_Subprogram_Body, N);
         end if;

         --  Visit all the call sequences of the subprogram
         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := AAN.First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := AAN.First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := AAN.Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := AAN.Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      -----------
      -- Visit --
      -----------
      procedure Visit (E : Node_Id) is
      begin
         case AAN.Kind (E) is
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

   end Source_File;

end Ocarina.Backends.PO_HI_RTSJ.Subprograms;
