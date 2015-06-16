------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--       O C A R I N A . B A C K E N D S . C A R T S . M A P P I N G        --
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

with Ocarina.Namet; use Ocarina.Namet;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Instances.Queries;

with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;

package body Ocarina.Backends.Carts.Mapping is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;

   use Ocarina.Instances.Queries;

   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.XML_Tree.Nodes;
   use Ocarina.Backends.XML_Tree.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XV renames Ocarina.Backends.XML_Values;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   procedure Map_Scheduler (E : Node_Id; N : Node_Id) is
      Scheduler : Supported_POK_Scheduler;
      R         : Node_Id;
      Q         : Node_Id;
   begin
      Scheduler := Get_POK_Scheduler (E);

      if AINU.Is_Processor (E) then
         Set_Str_To_Name_Buffer ("os_scheduler");
      else
         Set_Str_To_Name_Buffer ("scheduler");
      end if;

      R := Make_Defining_Identifier (Name_Find);

      if Scheduler = RMS then
         Set_Str_To_Name_Buffer ("rms");
      elsif Scheduler = EDF then
         Set_Str_To_Name_Buffer ("edf");
      elsif Scheduler = Static then
         Set_Str_To_Name_Buffer ("static");
      else
         Set_Str_To_Name_Buffer ("DM");
      end if;

      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (R, Q), XTN.Items (N));
   end Map_Scheduler;

   ---------------------------------
   -- Map_Distributed_Application --
   ---------------------------------

   function Map_Distributed_Application (E : Node_Id) return Node_Id is
      D : constant Node_Id := New_Node (XTN.K_HI_Distributed_Application);
   begin
      pragma Assert (AINU.Is_System (E) or else AINU.Is_Processor (E));

      --  Update the global variable to be able to fetch the root of
      --  the distributed application and generate the source files.

      XML_Root := D;

      XTN.Set_Name (D, To_XML_Name (AIN.Name (AIN.Identifier (E))));
      XTN.Set_Units (D, New_List (XTN.K_List_Id));
      XTN.Set_HI_Nodes (D, New_List (XTN.K_List_Id));

      return D;
   end Map_Distributed_Application;

   -----------------
   -- Map_HI_Node --
   -----------------

   function Map_HI_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (XTN.K_HI_Node);
   begin
      pragma Assert
        (AINU.Is_Process (E)
         or else AINU.Is_System (E)
         or else AINU.Is_Processor (E));

      if AINU.Is_System (E) then
         Set_Str_To_Name_Buffer ("general");
      else
         Get_Name_String
           (To_XML_Name
              (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_carts");
      end if;

      XTN.Set_Name (N, Name_Find);

      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list

      Append_Node_To_List (N, HI_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------

   function Map_HI_Unit (E : Node_Id) return Node_Id is
      U    : Node_Id;
      N    : Node_Id;
      P    : Node_Id;
      Root : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process (E)
         or else AINU.Is_Processor (E));

      U := New_Node (XTN.K_HI_Unit, AIN.Identifier (E));

      --  Packages that are common to all nodes
      if AINU.Is_System (E) then
         Get_Name_String (To_XML_Name (Display_Name (Identifier (E))));
      else
         Get_Name_String
           (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      end if;
      Add_Str_To_Name_Buffer ("_carts");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_XML_File (N);
      Set_Distributed_Application_Unit (P, U);
      XTN.Set_XML_File (U, P);

      Root := Make_XML_Node ("", No_Name, K_Nameid);

      XTN.Set_Root_Node (P, Root);

      Append_Node_To_List (U, Units (Current_Entity));
      XTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   -------------------
   -- Map_Processor --
   -------------------

   function Map_Processor (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("system");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));
      Map_Scheduler (E, N);
      return N;
   end Map_Processor;

   ---------------------------
   -- Map_Virtual_Processor --
   ---------------------------

   function Map_Virtual_Processor (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("component");

      P :=
        Make_Defining_Identifier
          (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      --  Always make subtype=tasks for partition.

      Set_Str_To_Name_Buffer ("tasks");
      P := Make_Defining_Identifier (Name_Find);
      Set_Str_To_Name_Buffer ("subtype");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      --  Map partition scheduler.

      Map_Scheduler (E, N);

      --  Now, map the criticality of the partition.

      Set_Str_To_Name_Buffer ("criticality");
      Q := Make_Defining_Identifier (Name_Find);

      if Is_Defined_Property (E, "pok::criticality") then
         P :=
           Make_Literal
             (XV.New_Numeric_Value
                (Get_Integer_Property (E, "pok::criticality"),
                 1,
                 10));
      else
         Set_Str_To_Name_Buffer ("?");
         P := Make_Defining_Identifier (Name_Find);
      end if;

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      return N;
   end Map_Virtual_Processor;

   ----------------
   -- Map_Thread --
   ----------------

   function Map_Thread (Thread : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
      Q : Node_Id;
   begin
      N := Make_XML_Node ("task");

      P :=
        Make_Defining_Identifier
          (To_XML_Name
             (Display_Name (Identifier (Parent_Subcomponent (Thread)))));
      Set_Str_To_Name_Buffer ("name");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (N));

      Set_Str_To_Name_Buffer ("p");
      P := Make_Defining_Identifier (Name_Find);
      Q :=
        Make_Literal
          (XV.New_Numeric_Value
             (To_Milliseconds (Get_Thread_Period (Thread)),
              1,
              10));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      Set_Str_To_Name_Buffer ("d");
      P := Make_Defining_Identifier (Name_Find);
      Q :=
        Make_Literal
          (XV.New_Numeric_Value
             (To_Milliseconds (Get_Thread_Deadline (Thread)),
              1,
              10));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      --  Make the non_interrupt_fraction value of the XML node.

      Set_Str_To_Name_Buffer ("noninterrupt_fraction");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal (XV.New_Numeric_Value (0, 1, 10));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      --  Make the jitter value of the XML node.

      Set_Str_To_Name_Buffer ("jitter");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal (XV.New_Numeric_Value (0, 1, 10));

      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      declare
         TA : constant Time_Array := Get_Execution_Time (Thread);
      begin
         if TA /= Empty_Time_Array then
            Set_Str_To_Name_Buffer ("e");
            P := Make_Defining_Identifier (Name_Find);
            Q :=
              Make_Literal
                (XV.New_Numeric_Value (To_Milliseconds (TA (1)), 1, 10));

            Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));
         end if;
      end;
      return N;
   end Map_Thread;
end Ocarina.Backends.Carts.Mapping;
