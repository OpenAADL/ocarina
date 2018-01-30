------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . C H E D D A R . M A P P I N G      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2010-2018 ESA & ISAE.                    --
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

with GNAT.OS_Lib;   use GNAT.OS_Lib;
with Ocarina.Namet; use Ocarina.Namet;
with Utils;         use Utils;

with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Build_Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.XML_Common.Mapping;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.XML_Values;

package body Ocarina.Backends.Cheddar.Mapping is

   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Build_Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.XML_Common.Mapping;
   use Ocarina.Backends.XML_Tree.Nodes;
   use Ocarina.Backends.XML_Tree.Nutils;

   --   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XV renames Ocarina.Backends.XML_Values;

   function Map_Buffer_Name (E : Node_Id; P : Node_Id) return Name_Id;
   --  Compute name of buffer by concatenating name of the thread
   --  instance and name of the port.

   ---------------------
   -- Map_Buffer_Name --
   ---------------------

   function Map_Buffer_Name (E : Node_Id; P : Node_Id) return Name_Id is
   begin
      Get_Name_String (Display_Name (Identifier (Parent_Subcomponent (E))));
      Add_Str_To_Name_Buffer ("_");
      Get_Name_String_And_Append (Display_Name (Identifier (P)));
      return To_Lower (Name_Find);
   end Map_Buffer_Name;

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
           (To_XML_Name (AIN.Name (Identifier (Parent_Subcomponent (E)))));
         Add_Str_To_Name_Buffer ("_cheddar");
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
      DTD  : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process (E)
         or else AINU.Is_Processor (E));

      U := New_Node (XTN.K_HI_Unit, Identifier (E));

      --  Packages that are common to all nodes

      if AINU.Is_System (E) then
         Get_Name_String (To_XML_Name (Display_Name (Identifier (E))));

      else
         Get_Name_String
           (To_XML_Name (Display_Name (Identifier (Parent_Subcomponent (E)))));
      end if;

      Add_Str_To_Name_Buffer ("_cheddar");
      N := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer
        (Get_Runtime_Path ("cheddar") & Directory_Separator & "cheddar.dtd");
      DTD := Make_Defining_Identifier (Name_Find);

      P := Make_XML_File (N, DTD);
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

      Schedulers : constant array
      (Supported_Scheduling_Protocol'Range) of Name_Id :=
        (RATE_MONOTONIC_PROTOCOL =>
           Get_String_Name ("RATE_MONOTONIC_PROTOCOL"),
         POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL =>
           Get_String_Name ("POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL"),
         EARLIEST_DEADLINE_FIRST_PROTOCOL =>
           Get_String_Name ("EARLIEST_DEADLINE_FIRST_PROTOCOL"),
         ROUND_ROBIN_PROTOCOL => Get_String_Name ("ROUND_ROBIN_PROTOCOL"),
         others               => No_Name);

      Quantum : constant Time_Type := Get_Scheduler_Quantum (E);
   begin
      --  The structure of a XML node for a processor is
      --  <!ELEMENT processor (name|
      --                       scheduler|
      --                       network_link)
      --  >

      N := Make_XML_Node ("processor");

      --  name: computed from processor instance name
      P := Map_Node_Identifier_To_XML_Node ("name", Parent_Subcomponent (E));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  scheduler: computed from Scheduling_Protocol policy
      P :=
        Map_To_XML_Node
          ("scheduler",
           Schedulers (Get_Scheduling_Protocol (E)));

      --  quantum: XXX use default value
      if Quantum /= Null_Time then
         declare
            Name : constant Node_Id :=
              Make_Defining_Identifier (Get_String_Name ("quantum"));
            Value : constant Node_Id :=
              Make_Literal
                (XV.New_Numeric_Value (To_Microseconds (Quantum), 1, 10));
         begin
            Append_Node_To_List
              (Make_Assignement (Name, Value),
               XTN.Items (P));
         end;
      end if;

      Append_Node_To_List (P, XTN.Subitems (N));

      --  network_link: XXX for now we put a default value
      P := Map_To_XML_Node ("network_link", Get_String_Name ("No_Network"));
      Append_Node_To_List (P, XTN.Subitems (N));

      return N;
   end Map_Processor;

   --------------
   -- Map_Data --
   --------------

   function Map_Data (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;

      Concurrency_Protocols : constant array
      (Supported_Concurrency_Control_Protocol'Range) of Name_Id :=
        (None_Specified   => Get_String_Name ("NO_PROTOCOL"),
         Priority_Ceiling => Get_String_Name ("PRIORITY_CEILING_PROTOCOL"),
         others           => No_Name);

   begin
      --  The structure of a XML node for a data is
      --  <!ELEMENT resource (cpu_name|
      --                      address_space_name|
      --                      name|
      --                      state|
      --                      protocol|
      --                      (state)?|
      --                      (resource_used_by)?)
      --  >

      N := Make_XML_Node ("resource");

      --  cpu_name: computed from the processor binding of the
      --  container process of the current data
      P :=
        Map_Node_Identifier_To_XML_Node
          ("cpu_name",
           Parent_Subcomponent
             (Get_Bound_Processor
                (Corresponding_Instance
                   (Get_Container_Process (Parent_Subcomponent (E))))));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  address_space: name of the enclosing process
      P :=
        Map_Node_Identifier_To_XML_Node
          ("address_space_name",
           Get_Container_Process (Parent_Subcomponent (E)));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  name: computed from data instance name
      P := Map_Node_Identifier_To_XML_Node ("name", Parent_Subcomponent (E));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  state: XXX ?
      P := Map_To_XML_Node ("state", Unsigned_Long_Long'(1));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  protocol: computed from Concurrency_Protocol property
      P :=
        Map_To_XML_Node
          ("protocol",
           Concurrency_Protocols (Get_Concurrency_Protocol (E)));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  resource_used_by: computed from the list of threads
      --  accessing to this data component. Per construction, it is
      --  assumed to be computed from the list of connections in the
      --  enclosing process.
      P := Make_XML_Node ("resource_used_by");
      declare
         Access_List : constant AINU.Node_Array :=
           Connections_Of
             (Corresponding_Instance
                (Get_Container_Process (Parent_Subcomponent (E))));
         K, M       : Node_Id;
      begin
         for Connection of Access_List loop
            if Kind (Connection) = K_Connection_Instance
              and then Get_Category_Of_Connection (Connection) =
              CT_Access_Data
            then
               if Item (AIN.First_Node (Path (Source (Connection)))) =
                 Parent_Subcomponent (E)
               then
                  M := Make_XML_Node ("resource_user");
                  K :=
                    Make_Defining_Identifier
                      (Fully_Qualified_Instance_Name
                         (Corresponding_Instance
                            (Item
                               (AIN.First_Node
                                  (Path (Destination (Connection)))))));
                  Append_Node_To_List (K, XTN.Subitems (M));

                  --  For now, we assume all tasks take the
                  --  resource at the beginning, and release it at
                  --  the end of their dispatch.

                  K := Make_Literal (XV.New_Numeric_Value (1, 1, 10));
                  Append_Node_To_List (K, XTN.Subitems (M));
                  K :=
                    Make_Literal
                      (XV.New_Numeric_Value
                         (To_Microseconds
                            (Get_Execution_Time
                               (Corresponding_Instance
                                  (Item
                                     (AIN.First_Node
                                        (Path (Destination (Connection))))))
                               (1)),
                          1,
                          10));
                  Append_Node_To_List (K, XTN.Subitems (M));

                  Append_Node_To_List (M, XTN.Subitems (P));
               end if;
            end if;
         end loop;
      end;

      Append_Node_To_List (P, XTN.Subitems (N));

      return N;
   end Map_Data;

   -----------------
   -- Map_Process --
   -----------------

   function Map_Process (E : Node_Id) return Node_Id is
      N : Node_Id;
      P : Node_Id;
   begin
      --  The structure of a XML node for a address_space is
      --  <!ELEMENT address_space (name|
      --                           text_memory_size|
      --                           data_memory_size|
      --                           stack_memory_size|
      --                           heap_memory_size)
      --  >

      N := Make_XML_Node ("address_space");

      --  name: computed from process instance name
      P :=
        Map_Node_Identifier_To_XML_Node
          ("name",
           Fully_Qualified_Instance_Name (E));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  cpu_name: computed from the processor binding of the
      --  container process of the current thread
      P :=
        Map_Node_Identifier_To_XML_Node
          ("cpu_name",
           Parent_Subcomponent (Get_Bound_Processor (E)));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  text_memory_size: XXX
      P := Map_To_XML_Node ("text_memory_size", Unsigned_Long_Long'(0));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  data_memory_size: XXX
      P := Map_To_XML_Node ("data_memory_size", Unsigned_Long_Long'(0));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  stack_memory_size: XXX
      P := Map_To_XML_Node ("stack_memory_size", Unsigned_Long_Long'(0));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  heap_memory_size: XXX
      P := Map_To_XML_Node ("heap_memory_size", Unsigned_Long_Long'(0));
      Append_Node_To_List (P, XTN.Subitems (N));

      return N;
   end Map_Process;

   ----------------
   -- Map_Thread --
   ----------------

   function Map_Thread (E : Node_Id) return Node_Id is
      N, P  : Node_Id;
      Value : Node_Id;
      Name  : Node_Id;

      Dispatch_Protocols : constant array
      (Supported_Thread_Dispatch_Protocol'Range) of Name_Id :=
        (Thread_Sporadic => Get_String_Name ("SPORADIC_TYPE"),
         Thread_Periodic => Get_String_Name ("PERIODIC_TYPE"),
         others          => No_Name);

      POSIX_Policies : constant array
      (Supported_POSIX_Scheduling_Policy'Range) of Name_Id :=
        (SCHED_FIFO   => Get_String_Name ("SCHED_FIFO"),
         SCHED_OTHERS => Get_String_Name ("SCHED_OTHERS"),
         SCHED_RR     => Get_String_Name ("SCHED_RR"),
         None         => No_Name);

      Dispatch : constant Supported_Thread_Dispatch_Protocol :=
        Get_Thread_Dispatch_Protocol (E);

      POSIX_Policy : constant Supported_POSIX_Scheduling_Policy :=
        Get_Thread_POSIX_Scheduling_Policy (E);

   begin
      --  The structure of a XML node for a task is
      --  <!ELEMENT task (name|
      --                  cpu_name|
      --                  address_space_name|
      --                  capacity|
      --                  start_time|
      --                  (stack_memory_size)?|
      --                  (text_memory_size)?|
      --                  (period)?|
      --                  (deadline)?|
      --                  (parameters)?|
      --                  (offsets)?|
      --                  (jitter)?|
      --                  (policy)?|
      --                  (priority)?|
      --                  (predictable_seed)?|
      --                  (blocking_time)?|
      --                  (seed)?|
      --                  (activation_rule)?)
      --  >

      N := Make_XML_Node ("task");

      --  task_type attribute
      --   supported values are PERIODIC or SPORADIC

      if Dispatch = Thread_Sporadic or else Dispatch = Thread_Periodic then
         Name  := Make_Defining_Identifier (Get_String_Name ("task_type"));
         Value := Make_Defining_Identifier (Dispatch_Protocols (Dispatch));
         Append_Node_To_List (Make_Assignement (Name, Value), XTN.Items (N));
      end if;

      --  cpu_name: computed from the processor binding of the
      --  container process of the current thread
      P :=
        Map_Node_Identifier_To_XML_Node
          ("cpu_name",
           Parent_Subcomponent
             (Get_Bound_Processor
                (Corresponding_Instance
                   (Get_Container_Process (Parent_Subcomponent (E))))));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  address_space: name of the enclosing process
      P :=
        Map_Node_Identifier_To_XML_Node
          ("address_space_name",
           Fully_Qualified_Instance_Name
             (Corresponding_Instance
                (Get_Container_Process (Parent_Subcomponent (E)))));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  name: computed from thread instance name
      P :=
        Map_Node_Identifier_To_XML_Node
          ("name",
           Fully_Qualified_Instance_Name (E));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  capacity: computed from the Compute_Execution_Time property
      --  XXX for now, we take the first value
      if Get_Execution_Time (E) = Empty_Time_Array then
         Display_Located_Error
           (AIN.Loc (E),
            "Property Compute_Exeuction_Time not set," &
            " assuming default value of 0",
            Fatal   => False,
            Warning => True);

         P := Map_To_XML_Node ("capacity", Unsigned_Long_Long'(0));
      else
         P :=
           Map_To_XML_Node
             ("capacity",
              To_Microseconds (Get_Execution_Time (E) (1)));
      end if;
      Append_Node_To_List (P, XTN.Subitems (N));

      --  start_time: computed from First_Dispatch_Time property, XXX units
      P :=
        Map_To_XML_Node
          ("start_time",
           To_Microseconds (Get_Thread_First_Dispatch_Time (E)));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  policy: computed from the POSIX_Scheduling_Policy properties
      if POSIX_Policy /= None then
         P := Map_To_XML_Node ("policy", POSIX_Policies (POSIX_Policy));
         Append_Node_To_List (P, XTN.Subitems (N));
      end if;

      if Dispatch = Thread_Periodic or else Dispatch = Thread_Sporadic then
         --  deadline: computed from Deadline property, XXX check units
         P :=
           Map_To_XML_Node
             ("deadline",
              To_Microseconds (Get_Thread_Deadline (E)));
         Append_Node_To_List (P, XTN.Subitems (N));
      end if;

      --  blocking_time: XXX
      P := Map_To_XML_Node ("blocking_time", Unsigned_Long_Long'(0));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  priority: computed from Priority property
      P := Map_To_XML_Node ("priority", Get_Thread_Priority (E));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  text_memory_size: XXX
      P := Map_To_XML_Node ("text_memory_size", Unsigned_Long_Long'(0));
      Append_Node_To_List (P, XTN.Subitems (N));

      --  stack_memory_size: computed from Source_Stack_Size property
      P :=
        Map_To_XML_Node
          ("stack_memory_size",
           To_Bytes (Get_Thread_Stack_Size (E)));
      Append_Node_To_List (P, XTN.Subitems (N));

      if Dispatch = Thread_Periodic or else Dispatch = Thread_Sporadic then
         --  period: computed from Period property, XXX check units
         P :=
           Map_To_XML_Node ("period", To_Microseconds (Get_Thread_Period (E)));
         Append_Node_To_List (P, XTN.Subitems (N));
      end if;

      --  jitter: XXX
      P := Map_To_XML_Node ("jitter", Unsigned_Long_Long'(0));
      Append_Node_To_List (P, XTN.Subitems (N));

      return N;
   end Map_Thread;

   ----------------
   -- Map_Buffer --
   ----------------

   function Map_Buffer (E : Node_Id; P : Node_Id) return Node_Id is
      N : Node_Id;
      K : Node_Id;
   begin
      --  The structure of a XML node for a buffer is
      --  <!ELEMENT buffer (cpu_name|
      --                    address_space_name|
      --                    qs|
      --                    name|
      --                    size|
      --                    (buffer_used_by)?)
      --  >

      N := Make_XML_Node ("buffer");

      --  cpu_name: computed from the processor binding of the
      --  container process of the current thread
      K :=
        Map_Node_Identifier_To_XML_Node
          ("cpu_name",
           Parent_Subcomponent
             (Get_Bound_Processor
                (Corresponding_Instance
                   (Get_Container_Process (Parent_Subcomponent (E))))));
      Append_Node_To_List (K, XTN.Subitems (N));

      --  address_space: name of the enclosing process
      K :=
        Map_Node_Identifier_To_XML_Node
          ("address_space_name",
           Get_Container_Process (Parent_Subcomponent (E)));
      Append_Node_To_List (K, XTN.Subitems (N));

      --  qs: XXX
      K := Map_To_XML_Node ("qs", Get_String_Name ("QS_PP1"));
      Append_Node_To_List (K, XTN.Subitems (N));

      --  name: computed from thread instance name + port_name
      K := Map_To_XML_Node ("name", Map_Buffer_Name (E, P));
      Append_Node_To_List (K, XTN.Subitems (N));

      --  size: computed from the queue size
      declare
         Size : Long_Long := Get_Queue_Size (P);
      begin
         if Size = -1 then
            Size := 1;
         end if;
         K := Map_To_XML_Node ("size", Unsigned_Long_Long (Size));
         Append_Node_To_List (K, XTN.Subitems (N));
      end;

      --  buffer_used_by
      declare
         L : Node_Id;
         M : Node_Id;
      begin
         --  This node list all users of a particular buffer attached
         --  to the P in event (data) port.

         L := Make_XML_Node ("buffer_used_by");

         --  The current thread is a consumer of the buffer associated
         --  to the P in event (data) port.

         M := Make_XML_Node ("buffer_user");
         Append_Node_To_List
           (Make_Assignement
              (Make_Defining_Identifier (Get_String_Name ("buffer_role")),
               Make_Defining_Identifier (Get_String_Name ("consumer"))),
            XTN.Items (M));
         K := Make_Defining_Identifier (Fully_Qualified_Instance_Name (E));
         Append_Node_To_List (K, XTN.Subitems (M));
         K := Make_Literal (XV.New_Numeric_Value (1, 1, 10));
         Append_Node_To_List (K, XTN.Subitems (M));
         K := Make_Literal (XV.New_Numeric_Value (1, 1, 10));
         Append_Node_To_List (K, XTN.Subitems (M));
         Append_Node_To_List (M, XTN.Subitems (L));

         --  Threads connected to the P in event (data) port are producers

         declare
            List_Sources : constant List_Id := Get_Source_Ports (P);
            Z            : Node_Id;
         begin
            if not AINU.Is_Empty (List_Sources) then
               Z := AIN.First_Node (List_Sources);
               while Present (Z) loop
                  M := Make_XML_Node ("buffer_user");
                  K :=
                    Make_Defining_Identifier
                      (Fully_Qualified_Instance_Name
                         (Parent_Component (Item (Z))));
                  Append_Node_To_List (K, XTN.Subitems (M));
                  K := Make_Literal (XV.New_Numeric_Value (1, 1, 10));
                  Append_Node_To_List (K, XTN.Subitems (M));
                  K := Make_Literal (XV.New_Numeric_Value (1, 1, 10));
                  Append_Node_To_List (K, XTN.Subitems (M));
                  Append_Node_To_List (M, XTN.Subitems (L));

                  Z := AIN.Next_Node (Z);
               end loop;
            end if;
            Append_Node_To_List (L, XTN.Subitems (N));
         end;
      end;

      return N;
   end Map_Buffer;

   --------------------
   -- Map_Dependency --
   --------------------

   function Map_Dependency (E : Node_Id; P : Node_Id) return Node_Id is
      N : Node_Id;
      K : Node_Id;
   begin
      --  The structure of a XML node for a dependency is
      --  XXX

      N := Make_XML_Node ("dependency");

      if Is_In (P) then
         Append_Node_To_List
           (Make_Assignement
              (Make_Defining_Identifier (Get_String_Name ("from_type")),
               Make_Defining_Identifier (Get_String_Name ("buffer"))),
            XTN.Items (N));
         K := Make_Defining_Identifier (Map_Buffer_Name (E, P));
         Append_Node_To_List (K, XTN.Subitems (N));
         K := Make_Defining_Identifier (Fully_Qualified_Instance_Name (E));
         Append_Node_To_List (K, XTN.Subitems (N));

      else
         if Present (AIN.First_Node (Get_Destination_Ports (P))) then
            --  We have to defends against the destination being an empty list.

            Append_Node_To_List
              (Make_Assignement
                 (Make_Defining_Identifier (Get_String_Name ("to_type")),
                  Make_Defining_Identifier (Get_String_Name ("buffer"))),
               XTN.Items (N));
            K := Make_Defining_Identifier (Fully_Qualified_Instance_Name (E));
            Append_Node_To_List (K, XTN.Subitems (N));
            K :=
              Make_Defining_Identifier
                (Map_Buffer_Name
                   (Parent_Component
                      (Item (AIN.First_Node (Get_Destination_Ports (P)))),
                    Item (AIN.First_Node (Get_Destination_Ports (P)))));

            Append_Node_To_List (K, XTN.Subitems (N));
         end if;
      end if;

      return N;
   end Map_Dependency;

end Ocarina.Backends.Cheddar.Mapping;
