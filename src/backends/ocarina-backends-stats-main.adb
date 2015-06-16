------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--          O C A R I N A . B A C K E N D S . S T A T S . M A I N           --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Utils;
with Ocarina.Backends.XML_Common.Mapping;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Stats.Mapping;

with Ocarina.Namet; use Ocarina.Namet;

package body Ocarina.Backends.Stats.Main is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.XML_Common.Mapping;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Stats.Mapping;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XV renames Ocarina.Backends.XML_Values;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);
   procedure Visit_Data_Instance (E : Node_Id);
   procedure Visit_Subprogram_Instance (E : Node_Id);
   procedure Look_For_Ports (AADL_Node : Node_Id; XML_Node : Node_Id);
   procedure Look_For_Connections (E : Node_Id; N : Node_Id);

   System_Nb_Processes : Unsigned_Long_Long := 0;
   System_Nb_Threads   : Unsigned_Long_Long := 0;
   Process_Nb_Threads  : Unsigned_Long_Long := 0;
   Current_Parent_Node : Node_Id;
   Process_Node        : Node_Id;
   HI_Node             : Node_Id;
   HI_Unit             : Node_Id;
   My_Root             : Node_Id;

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
      A : Node_Id;
   begin
      My_Root := Root_System (E);
      A       := Map_Distributed_Application (My_Root);
      Push_Entity (A);
      HI_Node := Map_HI_Node (My_Root);
      Push_Entity (HI_Node);
      HI_Unit := Map_HI_Unit (My_Root);
      Push_Entity (HI_Unit);

      Current_Parent_Node := XTN.Root_Node (XTN.XML_File (HI_Unit));
      Stats_Root_Node     := Current_Parent_Node;
      Visit (My_Root);

      Pop_Entity;
      Pop_Entity;
      Pop_Entity;
   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Thread =>
            Visit_Thread_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Bus =>
            Visit_Bus_Instance (E);

         when CC_Data =>
            Visit_Data_Instance (E);

         when CC_Subprogram =>
            Visit_Subprogram_Instance (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

   --------------------
   -- Look_For_Ports --
   --------------------

   procedure Look_For_Ports (AADL_Node : Node_Id; XML_Node : Node_Id) is
      F            : Node_Id;
      P            : Node_Id;
      Q            : Node_Id;
      Nb_Ports     : Unsigned_Long_Long;
      Nb_In_Ports  : Unsigned_Long_Long;
      Nb_Out_Ports : Unsigned_Long_Long;
   begin
      Nb_Ports     := 0;
      Nb_In_Ports  := 0;
      Nb_Out_Ports := 0;
      if Has_Ports (AADL_Node) then
         F := First_Node (Features (AADL_Node));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               Nb_Ports := Nb_Ports + 1;

               if Is_Out (F) then
                  Nb_Out_Ports := Nb_Out_Ports + 1;
               else
                  Nb_In_Ports := Nb_In_Ports + 1;
               end if;
               Append_Node_To_List (Map_Port (F), XTN.Subitems (XML_Node));
            end if;
            F := Next_Node (F);
         end loop;
      end if;

      --  Add the amount of ports of the AADL component

      P := Make_Literal (XV.New_Numeric_Value (Nb_Ports, 1, 10));
      Set_Str_To_Name_Buffer ("ports");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (XML_Node));

      --  Add the amount of IN ports of the AADL component

      P := Make_Literal (XV.New_Numeric_Value (Nb_In_Ports, 1, 10));
      Set_Str_To_Name_Buffer ("inports");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (XML_Node));

      --  Add the amount of OUT ports of the AADL component

      P := Make_Literal (XV.New_Numeric_Value (Nb_Out_Ports, 1, 10));
      Set_Str_To_Name_Buffer ("outports");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (XML_Node));

   end Look_For_Ports;

   --------------------------
   -- Look_For_Connections --
   --------------------------

   procedure Look_For_Connections (E : Node_Id; N : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (AIN.Connections (E)) then
         S := First_Node (AIN.Connections (E));
         while Present (S) loop
            if Kind (S) = K_Connection_Instance then
               if Get_Category_Of_Connection (S) = CT_Access_Data then
                  Append_Node_To_List (Map_Data_Access (S), XTN.Subitems (N));
               elsif Get_Category_Of_Connection (S) = CT_Access_Subprogram then
                  Append_Node_To_List
                    (Map_Subprogram_Access (S),
                     XTN.Subitems (N));
               elsif Get_Category_Of_Connection (S) = CT_Access_Bus then
                  Append_Node_To_List (Map_Bus_Access (S), XTN.Subitems (N));
               elsif Get_Category_Of_Connection (S) = CT_Data
                 or else Get_Category_Of_Connection (S) = CT_Event
               then
                  Append_Node_To_List
                    (Map_Port_Connection (S),
                     XTN.Subitems (N));
               end if;
            end if;
            S := Next_Node (S);
         end loop;
      end if;
   end Look_For_Connections;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      S                  : Node_Id;
      P                  : Node_Id;
      Q                  : Node_Id;
      N                  : Node_Id;
      Old_Current_Parent : Node_Id;
   begin
      System_Nb_Processes := System_Nb_Processes + 1;
      Process_Nb_Threads  := 0;

      --  Create the main node and set its name as an item
      Process_Node := Map_Process (E);

      --  Look for a possible bounded virtual processor/processor
      N := Get_Bound_Processor (E);

      if Present (N) then
         P :=
           Make_Defining_Identifier
             (To_XML_Name
                (Display_Name (Identifier (Parent_Subcomponent (N)))));
         Set_Str_To_Name_Buffer ("runtime_entity");
         Q := Make_Defining_Identifier (Name_Find);

         Append_Node_To_List
           (Make_Assignement (Q, P),
            XTN.Items (Process_Node));
      end if;

      Look_For_Ports (E, Process_Node);

      Old_Current_Parent  := Current_Parent_Node;
      Current_Parent_Node := Process_Node;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Look_For_Connections (E, Process_Node);

      Current_Parent_Node := Old_Current_Parent;

      --  Now, specify the amount of thread in the node
      P := Make_Literal (XV.New_Numeric_Value (Process_Nb_Threads, 1, 10));
      Set_Str_To_Name_Buffer ("threads");
      Q := Make_Defining_Identifier (Name_Find);
      Append_Node_To_List (Make_Assignement (Q, P), XTN.Items (Process_Node));

      Append_Node_To_List (Process_Node, XTN.Subitems (Current_Parent_Node));
   end Visit_Process_Instance;

   -------------------------
   -- Visit_Data_Instance --
   -------------------------

   procedure Visit_Data_Instance (E : Node_Id) is
      N : Node_Id;
   begin
      N := Map_Data (E);
      Append_Node_To_List (N, XTN.Subitems (Current_Parent_Node));
   end Visit_Data_Instance;

   -------------------------------
   -- Visit_Subprogram_Instance --
   -------------------------------

   procedure Visit_Subprogram_Instance (E : Node_Id) is
      N : Node_Id;
   begin
      N := Map_Subprogram (E);

      Append_Node_To_List (N, XTN.Subitems (Current_Parent_Node));
   end Visit_Subprogram_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S           : Node_Id;
      System_Node : Node_Id;
      My_Current  : Node_Id;
   begin
      System_Node := Map_System (E, E = My_Root);

      Append_Node_To_List (System_Node, XTN.Subitems (Current_Parent_Node));

      Look_For_Ports (E, System_Node);

      Look_For_Connections (E, System_Node);

      Current_Parent_Node := System_Node;
      My_Current          := System_Node;
      System_Nb_Processes := 0;
      System_Nb_Threads   := 0;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            Current_Parent_Node := My_Current;
            S                   := Next_Node (S);
         end loop;
      end if;

   end Visit_System_Instance;

   ---------------------------
   -- Visit_Thread_Instance --
   ---------------------------

   procedure Visit_Thread_Instance (E : Node_Id) is
      S        : Node_Id;
      Spg      : Node_Id;
      P        : Node_Id;
      Q        : Node_Id;
      R        : Node_Id;
      N        : Node_Id;
      Dispatch : constant Supported_Thread_Dispatch_Protocol :=
        Get_Thread_Dispatch_Protocol (E);
   begin
      System_Nb_Threads  := System_Nb_Threads + 1;
      Process_Nb_Threads := Process_Nb_Threads + 1;

      --  Make a new XML node and add it to the current process
      --  node.
      N := Map_Thread (E);

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      if not AINU.Is_Empty (Calls (E)) then
         R := Make_XML_Node ("calls");
         S := AIN.First_Node (AIN.Subprogram_Calls (First_Node (Calls (E))));
         while Present (S) loop
            Spg := AIN.Corresponding_Instance (S);
            Q   :=
              Make_XML_Node
                ("",
                 To_XML_Name (AIN.Name (AIN.Identifier (Spg))),
                 K_Nameid);
            Append_Node_To_List (Q, XTN.Subitems (R));
            S := Next_Node (S);
         end loop;
         Append_Node_To_List (R, XTN.Subitems (N));
      end if;

      Look_For_Ports (E, N);

      --  Add an item to specify the kind of thread we have
      case Dispatch is
         when Thread_Sporadic =>
            Set_Str_To_Name_Buffer ("dispatch");
            P := Make_Defining_Identifier (Name_Find);
            Set_Str_To_Name_Buffer ("sporadic");
            Q := Make_Defining_Identifier (Name_Find);
            Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

         when Thread_Periodic =>
            Set_Str_To_Name_Buffer ("dispatch");
            P := Make_Defining_Identifier (Name_Find);
            Set_Str_To_Name_Buffer ("periodic");
            Q := Make_Defining_Identifier (Name_Find);
            Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

         when others =>
            Display_Located_Error
              (Loc (E),
               "Please provide a dispatch protocol",
               Fatal => True);
      end case;

      --  Add the period of the task

      Set_Str_To_Name_Buffer ("period");
      P := Make_Defining_Identifier (Name_Find);
      Q := Map_Time (Get_Thread_Period (E));
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      --  Add the deadline of the task

      Set_Str_To_Name_Buffer ("deadline");
      P := Make_Defining_Identifier (Name_Find);
      Q := Map_Time (Get_Thread_Deadline (E));
      Append_Node_To_List (Make_Assignement (P, Q), XTN.Items (N));

      Append_Node_To_List (N, XTN.Subitems (Process_Node));

   end Visit_Thread_Instance;

   ------------------------
   -- Visit_Bus_Instance --
   ------------------------

   procedure Visit_Bus_Instance (E : Node_Id) is
      S : Node_Id;
      N : Node_Id;
      O : Node_Id;
   begin
      --  Create the main node and set its name as an item
      N                   := Map_Bus (E);
      O                   := Current_Parent_Node;
      Current_Parent_Node := N;
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
      Current_Parent_Node := O;

      Append_Node_To_List (N, XTN.Subitems (Current_Parent_Node));

   end Visit_Bus_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S : Node_Id;
      N : Node_Id;
      O : Node_Id;
   begin
      --  Create the main node and set its name as an item
      N                   := Map_Processor (E);
      O                   := Current_Parent_Node;
      Current_Parent_Node := N;
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
      Current_Parent_Node := O;

      Append_Node_To_List (N, XTN.Subitems (Current_Parent_Node));

   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S : Node_Id;
      N : Node_Id;
   begin
      --  Create the main node and set its name as an item
      N := Map_Virtual_Processor (E);

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Append_Node_To_List (N, XTN.Subitems (Current_Parent_Node));

   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Stats.Main;
