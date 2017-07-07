------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.PO_HI_ADA.DEPLOYMENT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2015 ESA & ISAE.      --
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

with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Ada_Values;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_HI_Ada.Mapping;
with Ocarina.Backends.PO_HI_Ada.Runtime;

package body Ocarina.Backends.PO_HI_Ada.Deployment is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Ada_Values;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_HI_Ada.Mapping;
   use Ocarina.Backends.PO_HI_Ada.Runtime;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);

      Last_Node_Enum_Pos   : Nat := 1;
      Last_Thread_Enum_Pos : Nat := 1;
      Last_Port_Enum_Pos   : Nat := 1;
      --  Global variables for the representation clauses of the
      --  generated enumerator types. They point to the next free
      --  position in the corresponding enumeration type. We start by
      --  1 because 0 is the value to indicate that no position has
      --  been assigned to the enumerator yet.

      function Get_Node_Enum_Pos (Name : Name_Id) return Nat;
      function Get_Thread_Enum_Pos (Name : Name_Id) return Nat;
      function Get_Port_Enum_Pos (Name : Name_Id) return Nat;
      --  Return the position corresponding to the enumerator. If no
      --  position has been computed for the given enumerator, compute
      --  a new one.

      function Added_Internal_Name (P : Node_Id; E : Node_Id) return Name_Id;
      function Is_Added (P : Node_Id; E : Node_Id) return Boolean;
      procedure Set_Added (P : Node_Id; E : Node_Id);
      --  Used to ensure that the enumerator for Process P and all the
      --  enumerator corresponding to its threads are added only once
      --  to the deployment package of node E.

      type Get_Enum_Pos_Access is access function (Name : Name_Id) return Nat;
      procedure Insert_Node_In_List
        (N            : Node_Id;
         L            : List_Id;
         Get_Enum_Pos : Get_Enum_Pos_Access);
      --  Insert the node N into list L. N is assumed to be an
      --  enumerator or an element association relative to an
      --  enumerator. The insertion is done so that the positions of
      --  all the enumerators in the list are ordered
      --  incresingly. This is necessary in Ada since the enumerator
      --  position order must respect their declaration order.

      function Max_Payload_Size_Declaration (E : Node_Id) return Node_Id;
      --  Builds the Max_Payload_Size constant declaration
      --  corresponding to node E.

      Max_Payload    : Unsigned_Long_Long;
      Max_Payload_Of : Name_Id := No_Name;
      --  Value of Max_Payload_Size

      function Max_Node_Image_Size_Declaration (E : Node_Id) return Node_Id;
      --  Builds the Max_Node_Image_Size constant declaration
      --  corresponding to node E.

      Max_Node_Image_Size : Unsigned_Long_Long := 1;
      --  Value of Max_Node_Image_Size

      function Max_Entity_Image_Size_Declaration (E : Node_Id) return Node_Id;
      --  Builds the Max_Entity_Image_Size constant declaration
      --  corresponding to node E.

      Max_Entity_Image_Size : Unsigned_Long_Long := 1;
      --  Value of Max_Entity_Image_Size

      function Max_Port_Image_Size_Declaration (E : Node_Id) return Node_Id;
      --  Builds the Max_Port_Image_Size constant declaration
      --  corresponding to node E.

      Max_Port_Image_Size : Unsigned_Long_Long := 1;
      --  Value of Max_Port_Image_Size

      ------------------
      -- Global Lists --
      ------------------

      --  The lists below are global and initialized at the beginning
      --  of visit of each process instance. This does not cause a
      --  consistency nor a concurrency problem since the visits of
      --  processes are done sequentially and the visit of one process
      --  is performed entirely before beginning the visit of another
      --  one.

      Node_Enumerator_List : List_Id;
      --  Global list for all the distributed application nodes
      --  enumeration type.

      Node_Enumerator_Pos_List : List_Id;
      --  Global list to store the representation clause of the node
      --  enumeration type.

      Node_Image_List : List_Id;
      --  Global list that associates each node of the distributed
      --  application to its String image.

      Thread_Enumerator_List : List_Id;
      --  Global list for all the distributed application threads
      --  enumeration types.

      Thread_Enumerator_Pos_List : List_Id;
      --  Global list to store the representation clause of the thread
      --  enumeration type.

      Entity_Table_List : List_Id;
      --  Global list that associates each thread of the distributed
      --  application to its containing process. Concretly, this is a
      --  list of associations that contain for each enumerator of the
      --  Entity_Type type the corresponding enumerator of the
      --  Node_Type type.

      Entity_Image_List : List_Id;
      --  Global list that associates each thread of the distributed
      --  application to its String image.

      Port_Enumerator_List : List_Id;
      --  Global list for all the distributed application thread port
      --  enumeration types.

      Port_Enumerator_Pos_List : List_Id;
      --  Global list to store the representation clause of the thread
      --  port enumeration type.

      Port_Table_List : List_Id;
      --  Global list that associates each port of the distributed
      --  application to its containing thread. Concretly, this is a
      --  list of associations that contain for each enumerator of the
      --  Port_Type type the corresponding enumerator of the
      --  Entity_Type type.

      Port_Image_List : List_Id;
      --  Global list that associates each port of the distributed
      --  application to its image.

      -----------------------
      -- Get_Node_Enum_Pos --
      -----------------------

      function Get_Node_Enum_Pos (Name : Name_Id) return Nat is
         I_Name   : Name_Id;
         Position : Nat;
      begin
         Set_Str_To_Name_Buffer ("%node_enum_pos%");
         Get_Name_String (Name);
         I_Name := Name_Find;

         --  Check whether a value has been computed for the current
         --  enumerator. Otherwise, compute a new one.

         Position := Get_Name_Table_Info (I_Name);

         if Position = 0 then
            --  Get the next free position

            Position := Last_Node_Enum_Pos;

            --  Link it to the enumerator for future use

            Set_Name_Table_Info (I_Name, Position);

            --  Increment the next free position

            Last_Node_Enum_Pos := Last_Node_Enum_Pos + 1;
         end if;

         return Position;
      end Get_Node_Enum_Pos;

      -------------------------
      -- Get_Thread_Enum_Pos --
      -------------------------

      function Get_Thread_Enum_Pos (Name : Name_Id) return Nat is
         I_Name   : Name_Id;
         Position : Nat;
      begin
         Set_Str_To_Name_Buffer ("%thread_enum_pos%");
         Get_Name_String (Name);
         I_Name := Name_Find;

         --  Check whether a value has been computed for the current
         --  enumerator. Otherwise, compute a new one.

         Position := Get_Name_Table_Info (I_Name);

         if Position = 0 then
            --  Get the next free position

            Position := Last_Thread_Enum_Pos;

            --  Link it to the enumerator for future use

            Set_Name_Table_Info (I_Name, Position);

            --  Increment the next free position

            Last_Thread_Enum_Pos := Last_Thread_Enum_Pos + 1;
         end if;

         return Position;
      end Get_Thread_Enum_Pos;

      -----------------------
      -- Get_Port_Enum_Pos --
      -----------------------

      function Get_Port_Enum_Pos (Name : Name_Id) return Nat is
         I_Name   : Name_Id;
         Position : Nat;
      begin
         Set_Str_To_Name_Buffer ("%port_enum_pos%");
         Get_Name_String (Name);
         I_Name := Name_Find;

         --  Check whether a value has been computed for the current
         --  enumerator. Otherwise, compute a new one.

         Position := Get_Name_Table_Info (I_Name);

         if Position = 0 then
            --  Get the next free position

            Position := Last_Port_Enum_Pos;

            --  Link it to the enumerator for future use

            Set_Name_Table_Info (I_Name, Position);

            --  Increment the next free position

            Last_Port_Enum_Pos := Last_Port_Enum_Pos + 1;
         end if;

         return Position;
      end Get_Port_Enum_Pos;

      -------------------------
      -- Added_Internal_Name --
      -------------------------

      function Added_Internal_Name (P : Node_Id; E : Node_Id) return Name_Id is
      begin
         Set_Str_To_Name_Buffer ("%add%enumerator%");
         Add_Nat_To_Name_Buffer (Nat (P));
         Add_Char_To_Name_Buffer ('%');
         Add_Nat_To_Name_Buffer (Nat (E));

         return Name_Find;
      end Added_Internal_Name;

      --------------
      -- Is_Added --
      --------------

      function Is_Added (P : Node_Id; E : Node_Id) return Boolean is
         I_Name : constant Name_Id := Added_Internal_Name (P, E);
      begin
         return Get_Name_Table_Byte (I_Name) = 1;
      end Is_Added;

      ---------------
      -- Set_Added --
      ---------------

      procedure Set_Added (P : Node_Id; E : Node_Id) is
         I_Name : constant Name_Id := Added_Internal_Name (P, E);
      begin
         Set_Name_Table_Byte (I_Name, 1);
      end Set_Added;

      -------------------------
      -- Insert_Node_In_List --
      -------------------------

      procedure Insert_Node_In_List
        (N            : Node_Id;
         L            : List_Id;
         Get_Enum_Pos : Get_Enum_Pos_Access)
      is
         use ADN;

         M          : Node_Id;
         Position_N : Nat;
         Position_M : Nat;
      begin
         pragma Assert
           (Kind (N) = K_Defining_Identifier
            or else Kind (N) = K_Element_Association);

         case ADN.Kind (N) is
            when ADN.K_Defining_Identifier =>
               Position_N := Get_Enum_Pos (ADN.Name (N));
            when ADN.K_Element_Association =>
               Position_N := Get_Enum_Pos (ADN.Name (Index (N)));
            when others =>
               raise Program_Error with "Inconsistency in Insert_Node_In_List";
         end case;

         if Is_Empty (L) then
            Append_Node_To_List (N, L);
         else
            M := ADN.First_Node (L);

            while Present (M) loop
               case ADN.Kind (M) is
                  when ADN.K_Defining_Identifier =>
                     Position_M := Get_Enum_Pos (ADN.Name (M));
                  when ADN.K_Element_Association =>
                     Position_M := Get_Enum_Pos (ADN.Name (Index (M)));
                  when others =>
                     raise Program_Error
                       with "Inconsistency in Insert_Node_In_List";
               end case;

               if Position_N < Position_M then
                  Insert_Before_Node (N, M, L);

                  return; --  IMPORTANT
               end if;

               M := ADN.Next_Node (M);
            end loop;

            Append_Node_To_List (N, L);
         end if;
      end Insert_Node_In_List;

      ----------------------------------
      -- Max_Payload_Size_Declaration --
      ----------------------------------

      function Max_Payload_Size_Declaration (E : Node_Id) return Node_Id is
         pragma Unreferenced (E);

         N : Node_Id;
      begin
         --  The structure of a message payload is as follows:
         --  1 - A destination port (of type Deployment.Port_Type)
         --  2 - An optional time stamp (of type Ada.Real_Time.Time)
         --  3 - A data (of one of the marshallable types declared in
         --      the currently being generated package)

         --  Size corresponding to (3) has been built incrementally
         --  during the visiting of AADL data component instance. Add
         --  the size corresponding to (2) and (1)

         Max_Payload := Max_Payload + 64 + Port_Type_Size;
         --  64 is the size of Ada.Real_Time.Time. We cannot afford
         --  doing a Ada.Real_Time.Time'Size because 'Size for private
         --  Ada types is not a static expression.

         N :=
           Make_Object_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (PN (P_Max_Payload_Size)),
              Constant_Present  => True,
              Object_Definition => RE (RE_Integer),
              Expression        =>
                Make_Literal (New_Integer_Value (Max_Payload, 1, 10)));

         return N;
      end Max_Payload_Size_Declaration;

      -------------------------------------
      -- Max_Node_Image_Size_Declaration --
      -------------------------------------

      function Max_Node_Image_Size_Declaration (E : Node_Id) return Node_Id is
         pragma Unreferenced (E);

         N : Node_Id;
      begin
         N :=
           Make_Object_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (PN (P_Max_Node_Image_Size)),
              Constant_Present  => True,
              Object_Definition => RE (RE_Integer),
              Expression        =>
                Make_Literal (New_Integer_Value (Max_Node_Image_Size, 1, 10)));
         return N;
      end Max_Node_Image_Size_Declaration;

      ---------------------------------------
      -- Max_Entity_Image_Size_Declaration --
      ---------------------------------------

      function Max_Entity_Image_Size_Declaration
        (E : Node_Id) return Node_Id
      is
         pragma Unreferenced (E);

         N : Node_Id;
      begin
         N :=
           Make_Object_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (PN (P_Max_Entity_Image_Size)),
              Constant_Present  => True,
              Object_Definition => RE (RE_Integer),
              Expression        =>
                Make_Literal
                  (New_Integer_Value (Max_Entity_Image_Size, 1, 10)));
         return N;
      end Max_Entity_Image_Size_Declaration;

      -------------------------------------
      -- Max_Port_Image_Size_Declaration --
      -------------------------------------

      function Max_Port_Image_Size_Declaration (E : Node_Id) return Node_Id is
         pragma Unreferenced (E);

         N : Node_Id;
      begin
         N :=
           Make_Object_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (PN (P_Max_Port_Image_Size)),
              Constant_Present  => True,
              Object_Definition => RE (RE_Integer),
              Expression        =>
                Make_Literal (New_Integer_Value (Max_Port_Image_Size, 1, 10)));
         return N;
      end Max_Port_Image_Size_Declaration;

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

            when CC_Data =>
               Visit_Data_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
      begin
         --  Do not generate Ada type more than once

         if No (Get_Handling (E, By_Name, H_Ada_Deployment_Spec)) then
            --  Threads and processes cannot have opaque types in
            --  their features because this prevents the static
            --  computation of the buffer sizes.

            if Get_Source_Language (E) = Language_Ada_95 then
               Display_Located_Error
                 (Loc (E),
                  "This data type cannot be used in thread or process" &
                  " features",
                  Fatal => True);
            end if;

            --  If the type is sendable through network, we take into
            --  account its size when calculating the maximal message
            --  payload size.

            if Get_Data_Representation (E) /= Data_With_Accessors then
               declare
                  Data_Size : Unsigned_Long_Long;
               begin
                  if Get_Data_Size (E) /= Null_Size then
                     Data_Size := To_Bits (Get_Data_Size (E));
                  else
                     Data_Size := Estimate_Data_Size (E);
                  end if;

                  if Data_Size > Max_Payload then
                     Max_Payload    := Data_Size;
                     Max_Payload_Of := To_Ada_Name (Name (Identifier (E)));
                  end if;
               end;

               Set_Handling (E, By_Name, H_Ada_Deployment_Spec, E);
            end if;
         end if;
      end Visit_Data_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : constant Node_Id := Get_Implementation (E);
         N              : Node_Id;
      begin
         if Implementation /= No_Node then
            if not AAU.Is_Empty (AAN.Subcomponents (Implementation)) then
               N := First_Node (Subcomponents (Implementation));
               while Present (N) loop
                  Visit_Component_Instance (Corresponding_Instance (N));
                  N := Next_Node (N);
               end loop;
            end if;
         end if;
      end Visit_Device_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         P       : constant Node_Id   := Map_HI_Node (E);
         U       : Node_Id;
         S       : constant Node_Id   := Parent_Subcomponent (E);
         Img_Len : Unsigned_Long_Long :=
           Get_Name_String (Map_Ada_Enumerator_Name (S))'Length;
         N          : Node_Id;
         C          : Node_Id;
         F          : Node_Id;
         Src        : Node_Id;
         Dst        : Node_Id;
         Parent     : Node_Id;
         S_Parent   : Node_Id;
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
      begin
         Push_Entity (P);

         --  It is important that we push P at the top of the entity
         --  stack before generating the package unit.

         U := Map_HI_Unit (E);
         Push_Entity (U);
         Set_Deployment_Spec;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         --  Initialize Max_Payload

         if Need_Deliver (E) or else Need_Send (E) then
            Max_Payload := 0;
         end if;

         --  Create the lists

         Node_Enumerator_List       := New_List (ADN.K_Enumeration_Literals);
         Node_Enumerator_Pos_List   := New_List (ADN.K_Element_List);
         Node_Image_List            := New_List (ADN.K_Element_List);
         Thread_Enumerator_List     := New_List (ADN.K_Enumeration_Literals);
         Thread_Enumerator_Pos_List := New_List (ADN.K_Element_List);
         Entity_Table_List          := New_List (ADN.K_Element_List);
         Entity_Image_List          := New_List (ADN.K_Element_List);
         Port_Enumerator_List       := New_List (ADN.K_Enumeration_Literals);
         Port_Enumerator_Pos_List   := New_List (ADN.K_Element_List);
         Port_Table_List            := New_List (ADN.K_Element_List);
         Port_Image_List            := New_List (ADN.K_Element_List);

         --  The Deployment package must be preelaborated

         N := Make_Pragma_Statement (Pragma_Preelaborate, No_List);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  Build the enumerator corresponding to the distributed
         --  application node.

         N := Make_Defining_Identifier (Map_Ada_Enumerator_Name (S));
         Append_Node_To_List (N, Node_Enumerator_List);
         Bind_AADL_To_Enumerator (Identifier (S), N);

         --  Build the representation clause of the enumerator

         N :=
           Make_Element_Association
             (Make_Defining_Identifier (Map_Ada_Enumerator_Name (S)),
              Make_Literal
                (New_Integer_Value
                   (Unsigned_Long_Long
                      (Get_Node_Enum_Pos (Map_Ada_Enumerator_Name (S))),
                    1,
                    10)));
         Insert_Node_In_List
           (N,
            Node_Enumerator_Pos_List,
            Get_Node_Enum_Pos'Access);

         if Max_Node_Image_Size < Img_Len then
            Max_Node_Image_Size := Img_Len;
         end if;

         N :=
           Make_Element_Association
             (Make_Defining_Identifier (Map_Ada_Enumerator_Name (S)),
              Make_Literal (New_String_Value (Map_Ada_Enumerator_Name (S))));

         Insert_Node_In_List (N, Node_Image_List, Get_Node_Enum_Pos'Access);

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            C := First_Node (Subcomponents (E));

            while Present (C) loop
               Visit (Corresponding_Instance (C));

               C := Next_Node (C);
            end loop;
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AAU.Is_Device (Corresponding_Instance (C))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (C)) =
                   Get_Bound_Processor (E)
               then
                  --  Build the enumerator corresponding to the device
                  --  Note: we reuse the process name XXX

                  N := Make_Defining_Identifier (Map_Ada_Enumerator_Name (S));
                  Bind_AADL_To_Enumerator
                    (Identifier (Corresponding_Instance (C)),
                     N);

                  Visit_Device_Instance (Corresponding_Instance (C));
               end if;
               C := Next_Node (C);
            end loop;
         end if;

         --  For each of the processes P connected to E, (1) we add an
         --  enumerator corresponding to P and (2) for each one of the
         --  threads of P, we add an enumerator.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               --  The sources of F

               if not AAU.Is_Empty (Sources (F)) then
                  Src := First_Node (Sources (F));

                  while Present (Src) loop

                     Parent := Parent_Component (Item (Src));

                     if AAU.Is_Process (Parent)
                       and then Parent /= E
                       and then not Is_Added (Parent, E)
                     then
                        --  Add the process to the deployment
                        --  enumerators of E.

                        S_Parent := Parent_Subcomponent (Parent);
                        Img_Len  :=
                          Get_Name_String (Map_Ada_Enumerator_Name (S_Parent))'
                            Length;
                        N :=
                          Make_Defining_Identifier
                            (Map_Ada_Enumerator_Name (S_Parent));
                        Insert_Node_In_List
                          (N,
                           Node_Enumerator_List,
                           Get_Node_Enum_Pos'Access);

                        --  Add a representation clause for the
                        --  enumerator corresponding to Parent.

                        N :=
                          Make_Element_Association
                            (Make_Defining_Identifier
                               (Map_Ada_Enumerator_Name (S_Parent)),
                             Make_Literal
                               (New_Integer_Value
                                  (Unsigned_Long_Long
                                     (Get_Node_Enum_Pos
                                        (Map_Ada_Enumerator_Name (S_Parent))),
                                   1,
                                   10)));
                        Insert_Node_In_List
                          (N,
                           Node_Enumerator_Pos_List,
                           Get_Node_Enum_Pos'Access);

                        if Max_Node_Image_Size < Img_Len then
                           Max_Node_Image_Size := Img_Len;
                        end if;

                        N :=
                          Make_Element_Association
                            (Make_Defining_Identifier
                               (Map_Ada_Enumerator_Name (S_Parent)),
                             Make_Literal
                               (New_String_Value
                                  (Map_Ada_Enumerator_Name (S_Parent))));

                        Insert_Node_In_List
                          (N,
                           Node_Image_List,
                           Get_Node_Enum_Pos'Access);

                        --  Traverse all the subcomponents of Parent

                        if not AAU.Is_Empty (Subcomponents (Parent)) then
                           C := First_Node (Subcomponents (Parent));

                           while Present (C) loop
                              Visit (Corresponding_Instance (C));

                              C := Next_Node (C);
                           end loop;
                        end if;

                        --  Mark P as being Added

                        Set_Added (Parent, E);
                     end if;

                     Src := Next_Node (Src);
                  end loop;
               end if;

               --  The destinations of F

               if not AAU.Is_Empty (Destinations (F)) then
                  Dst := First_Node (Destinations (F));

                  while Present (Dst) loop
                     Parent := Parent_Component (Item (Dst));

                     if AAU.Is_Process (Parent)
                       and then Parent /= E
                       and then not Is_Added (Parent, E)
                     then
                        --  Add the process to the deployment
                        --  enumerators of E.

                        S_Parent := Parent_Subcomponent (Parent);
                        Img_Len  :=
                          Get_Name_String (Map_Ada_Enumerator_Name (S_Parent))'
                            Length;
                        N :=
                          Make_Defining_Identifier
                            (Map_Ada_Enumerator_Name (S_Parent));
                        Insert_Node_In_List
                          (N,
                           Node_Enumerator_List,
                           Get_Node_Enum_Pos'Access);

                        --  Add a representation clause for the
                        --  enumerator corresponding to P.

                        N :=
                          Make_Element_Association
                            (Make_Defining_Identifier
                               (Map_Ada_Enumerator_Name (S_Parent)),
                             Make_Literal
                               (New_Integer_Value
                                  (Unsigned_Long_Long
                                     (Get_Node_Enum_Pos
                                        (Map_Ada_Enumerator_Name (S_Parent))),
                                   1,
                                   10)));
                        Insert_Node_In_List
                          (N,
                           Node_Enumerator_Pos_List,
                           Get_Node_Enum_Pos'Access);

                        if Max_Node_Image_Size < Img_Len then
                           Max_Node_Image_Size := Img_Len;
                        end if;

                        N :=
                          Make_Element_Association
                            (Make_Defining_Identifier
                               (Map_Ada_Enumerator_Name (S_Parent)),
                             Make_Literal
                               (New_String_Value
                                  (Map_Ada_Enumerator_Name (S_Parent))));

                        Insert_Node_In_List
                          (N,
                           Node_Image_List,
                           Get_Node_Enum_Pos'Access);

                        --  Traverse all the subcomponensts of P

                        if not AAU.Is_Empty (Subcomponents (Parent)) then
                           C := First_Node (Subcomponents (Parent));

                           while Present (C) loop
                              Visit (Corresponding_Instance (C));

                              C := Next_Node (C);
                           end loop;
                        end if;

                        --  Mark P as being Added

                        Set_Added (Parent, E);
                     end if;

                     Dst := Next_Node (Dst);
                  end loop;
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Create the node enumeration type declaration. Note that
         --  the type creation is possible even the enumeration list
         --  is incomplete. We can do this in the first traversal
         --  since we are sure that the enumerator list is not empty.

         N :=
           Message_Comment
             ("For each node in the distributed" &
              " application add an enumerator");
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (TN (T_Node_Type)),
              Type_Definition =>
                Make_Enumeration_Type_Definition (Node_Enumerator_List));
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  Create the enumeration representation clause so that all
         --  the enumerators in all the generated deployment packages
         --  have coherent position.

         N :=
           Message_Comment
             ("Representation clause to have consistent" &
              " positions for enumerators");
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         N :=
           Make_Enumeration_Representation_Clause
             (Defining_Identifier =>
                Make_Defining_Identifier (TN (T_Node_Type)),
              Array_Aggregate =>
                Make_Array_Aggregate (Node_Enumerator_Pos_List));
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  Fix the size of type Node_Type to Node_Type_Size bit
         --  because the message stream allocates one byte for it for
         --  now. This implies a maximum value of 256 nodes per
         --  application.

         N :=
           Message_Comment
             ("Size of Node_Type fixed to" &
              Integer'Image (Node_Type_Size) &
              " bits");
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         N :=
           Make_Attribute_Definition_Clause
             (Defining_Identifier =>
                Make_Defining_Identifier (TN (T_Node_Type)),
              Attribute_Designator => A_Size,
              Expression           =>
                Make_Literal (New_Integer_Value (Node_Type_Size, 1, 10)));
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         N := Max_Node_Image_Size_Declaration (E);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         N := Message_Comment ("Maximal Node_Image size for this" & " node");
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         N := Message_Comment ("Node Image");
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         N :=
           Make_Array_Type_Definition
             (Range_Constraints =>
                Make_List_Id
                  (Make_Range_Constraint
                     (No_Node,
                      No_Node,
                      Make_Attribute_Designator
                        (Make_Designator (TN (T_Node_Type)),
                         A_Range))),
              Component_Definition =>
                Make_Indexed_Component
                  (RE (RE_String),
                   Make_List_Id
                     (Make_Range_Constraint
                        (Make_Literal (New_Integer_Value (1, 0, 10)),
                         Make_Defining_Identifier
                           (PN (P_Max_Node_Image_Size))))));

         --  Normalize Node image strings to fit in the Node_Image
         --  array constraint (Max_Node_Image_Size)

         declare
            Cur : Node_Id := ADN.First_Node (Node_Image_List);
         begin
            for J in 1 .. Length (Node_Image_List) loop
               declare
                  Str : constant String :=
                    Image (ADN.Value (ADN.Expression (Cur)));
                  Res : String (1 .. Integer (Max_Node_Image_Size));
               begin
                  --  Get the string without the quotes
                  Res (1 .. Str'Last - 2) := Str (2 .. Str'Last - 1);
                  --  Fill the end with spaces
                  for I in Str'Last - 1 .. Integer (Max_Node_Image_Size) loop
                     Res (I) := ' ';
                  end loop;
                  ADN.Set_Value
                    (ADN.Expression (Cur),
                     New_String_Value (Get_String_Name (Res)));
               end;
               Cur := ADN.Next_Node (Cur);
            end loop;
         end;

         N :=
           Make_Object_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (PN (P_Node_Image)),
              Constant_Present  => True,
              Object_Definition => N,
              Expression        => Make_Array_Aggregate (Node_Image_List));
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  Declare the constant that represents the current node

         N :=
           Make_Object_Declaration
             (Defining_Identifier => Make_Defining_Identifier (PN (P_My_Node)),
              Constant_Present    => True,
              Object_Definition   => Make_Designator (TN (T_Node_Type)),
              Expression          =>
                Make_Defining_Identifier (Map_Ada_Enumerator_Name (S)));
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  Create the thread enumeration type declaration. Note that
         --  the type creation is possible even the enumeration list
         --  is incomplete.

         if not Is_Empty (Thread_Enumerator_List) then
            N :=
              Message_Comment
                ("For each thread in the distributed" &
                 " application nodes, add an" &
                 " enumerator");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (TN (T_Entity_Type)),
                 Type_Definition =>
                   Make_Enumeration_Type_Definition (Thread_Enumerator_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Create the enumeration representation clause so that
            --  all the enumerators in all the generated deployment
            --  packages have coherent position.

            N :=
              Message_Comment
                ("Representation clause to have consistent" &
                 " positions for enumerators");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Enumeration_Representation_Clause
                (Defining_Identifier =>
                   Make_Defining_Identifier (TN (T_Entity_Type)),
                 Array_Aggregate =>
                   Make_Array_Aggregate (Thread_Enumerator_Pos_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Fix the size of type Entity_Type to Entity_Type_Size
            --  bit because the message stream allocates one byte for
            --  it for now. This implies a maximum value of 256 nodes
            --  per application.

            N :=
              Message_Comment
                ("Size of Entity_Type fixed to" &
                 Integer'Image (Entity_Type_Size) &
                 " bits");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Attribute_Definition_Clause
                (Defining_Identifier =>
                   Make_Defining_Identifier (TN (T_Entity_Type)),
                 Attribute_Designator => A_Size,
                 Expression           =>
                   Make_Literal (New_Integer_Value (Entity_Type_Size, 1, 10)));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;

         --  Declare the Entity Table when necessary

         if not Is_Empty (Entity_Table_List) then
            N := Message_Comment ("Entity Table");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Array_Type_Definition
                (Range_Constraints =>
                   Make_List_Id
                     (Make_Range_Constraint
                        (No_Node,
                         No_Node,
                         Make_Attribute_Designator
                           (Make_Designator (TN (T_Entity_Type)),
                            A_Range))),
                 Component_Definition =>
                   Make_Defining_Identifier (TN (T_Node_Type)));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (PN (P_Entity_Table)),
                 Constant_Present  => True,
                 Object_Definition => N,
                 Expression => Make_Array_Aggregate (Entity_Table_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Max_Entity_Image_Size_Declaration (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Message_Comment ("Maximal Entity_Image size for this" & " node");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Message_Comment ("Entity Image");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Array_Type_Definition
                (Range_Constraints =>
                   Make_List_Id
                     (Make_Range_Constraint
                        (No_Node,
                         No_Node,
                         Make_Attribute_Designator
                           (Make_Designator (TN (T_Entity_Type)),
                            A_Range))),
                 Component_Definition =>
                   Make_Indexed_Component
                     (RE (RE_String),
                      Make_List_Id
                        (Make_Range_Constraint
                           (Make_Literal (New_Integer_Value (1, 0, 10)),
                            Make_Defining_Identifier
                              (PN (P_Max_Entity_Image_Size))))));

            --  Normalize Entity image strings to fit in the Entity_Image
            --  array constraint (Max_Entity_Image_Size)

            declare
               Cur : Node_Id := ADN.First_Node (Entity_Image_List);
            begin
               for J in 1 .. Length (Entity_Image_List) loop
                  declare
                     Str : constant String :=
                       Image (ADN.Value (ADN.Expression (Cur)));
                     Res : String (1 .. Integer (Max_Entity_Image_Size));
                  begin
                     --  Get the string without the quotes
                     Res (1 .. Str'Last - 2) := Str (2 .. Str'Last - 1);
                     --  Fill the end with spaces
                     for I in Str'Last - 1 .. Integer (Max_Entity_Image_Size)
                     loop
                        Res (I) := ' ';
                     end loop;
                     ADN.Set_Value
                       (ADN.Expression (Cur),
                        New_String_Value (Get_String_Name (Res)));
                  end;
                  Cur := ADN.Next_Node (Cur);
               end loop;
            end;

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (PN (P_Entity_Image)),
                 Constant_Present  => True,
                 Object_Definition => N,
                 Expression => Make_Array_Aggregate (Entity_Image_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;

         --  Create the port enumeration type declaration. Note that
         --  the type creation is possible even the enumeration list
         --  is incomplete.

         if not Is_Empty (Port_Enumerator_List) then
            N :=
              Message_Comment
                ("For each thread port in the distributed" &
                 " application nodes, add an" &
                 " enumerator");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Full_Type_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (TN (T_Port_Type)),
                 Type_Definition =>
                   Make_Enumeration_Type_Definition (Port_Enumerator_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Create the enumeration representation clause so that
            --  all the enumerators in all the generated deployment
            --  packages have coherent position.

            N :=
              Message_Comment
                ("Representation clause to have consistent" &
                 " positions for enumerators");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Enumeration_Representation_Clause
                (Defining_Identifier =>
                   Make_Defining_Identifier (TN (T_Port_Type)),
                 Array_Aggregate =>
                   Make_Array_Aggregate (Port_Enumerator_Pos_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Fix the size of type Port_Type to Port_Type_Size bits
            --  to be able to instanciate a Marshallers_G for it.

            N :=
              Message_Comment
                ("Size of Port_Type fixed to" &
                 Integer'Image (Port_Type_Size) &
                 " bits");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Attribute_Definition_Clause
                (Defining_Identifier =>
                   Make_Defining_Identifier (TN (T_Port_Type)),
                 Attribute_Designator => A_Size,
                 Expression           =>
                   Make_Literal (New_Integer_Value (Port_Type_Size, 1, 10)));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;

         --  Declare the Port Table when necessary

         if not Is_Empty (Port_Table_List) then
            N := Message_Comment ("Port Table");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Make_Array_Type_Definition
                (Range_Constraints =>
                   Make_List_Id
                     (Make_Range_Constraint
                        (No_Node,
                         No_Node,
                         Make_Attribute_Designator
                           (Make_Designator (TN (T_Port_Type)),
                            A_Range))),
                 Component_Definition =>
                   Make_Defining_Identifier (TN (T_Entity_Type)));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (PN (P_Port_Table)),
                 Constant_Present  => True,
                 Object_Definition => N,
                 Expression        => Make_Array_Aggregate (Port_Table_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Max_Port_Image_Size_Declaration (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Message_Comment ("Maximal Port_Image size for this" & " node");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N :=
              Message_Comment
                ("A String subtype with Port_Image_Size" & " constraint");
            N :=
              Make_Full_Type_Declaration
                (Make_Defining_Identifier (PN (P_Port_Sized_String)),
                 Make_Indexed_Component
                   (RE (RE_String),
                    Make_List_Id
                      (Make_Range_Constraint
                         (Make_Literal (New_Integer_Value (1, 0, 10)),
                          RE (RE_Max_Port_Image_Size)))),
                 Is_Subtype => True);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Message_Comment ("Port Image");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Normalize Port image strings to fit in the Port_Image
            --  array constraint (Max_Port_Image_Size)

            declare
               Cur : Node_Id := ADN.First_Node (Port_Image_List);
            begin
               for J in 1 .. Length (Port_Image_List) loop
                  declare
                     Str : constant String :=
                       Image (ADN.Value (ADN.Expression (Cur)));
                     Res : String (1 .. Integer (Max_Port_Image_Size));
                  begin
                     --  Get the string without the quotes
                     Res (1 .. Str'Last - 2) := Str (2 .. Str'Last - 1);
                     --  Fill the end with spaces
                     for I in Str'Last - 1 .. Integer (Max_Port_Image_Size)
                     loop
                        Res (I) := ' ';
                     end loop;
                     ADN.Set_Value
                       (ADN.Expression (Cur),
                        New_String_Value (Get_String_Name (Res)));
                  end;
                  Cur := ADN.Next_Node (Cur);
               end loop;
            end;

            N :=
              Make_Array_Type_Definition
                (Range_Constraints =>
                   Make_List_Id
                     (Make_Range_Constraint
                        (No_Node,
                         No_Node,
                         Make_Attribute_Designator
                           (Make_Designator (TN (T_Port_Type)),
                            A_Range))),
                 Component_Definition =>
                   Make_Defining_Identifier (PN (P_Port_Sized_String)));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (PN (P_Port_Image)),
                 Constant_Present  => True,
                 Object_Definition => N,
                 Expression        => Make_Array_Aggregate (Port_Image_List));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;

         --  Generate the Max_Payload_Size constant declaration only
         --  if there is a real communication between threads or
         --  nodes.

         if Need_Deliver (E) or else Need_Send (E) then
            N :=
              Message_Comment
                ("Maximal message payload size for this" & " node (in bits)");
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Max_Payload_Size_Declaration (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Indicate to which type corresonds the maximal size

            --  Is not relevant if the system does not handle data
            --  or event data communication between threads or nodes

            if Max_Payload_Of /= No_Name then
               N :=
                 Message_Comment
                   ("Biggest type: " & Get_Name_String (Max_Payload_Of));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;
         end if;

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         A : constant Node_Id := Map_Distributed_Application (E);
         S : Node_Id;
      begin
         Push_Entity (A);

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

         Pop_Entity; --  A
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         N          : Node_Id;
         P          : Node_Id;
         F          : Node_Id;
         S          : constant Node_Id            := Parent_Subcomponent (E);
         Img_Length : constant Unsigned_Long_Long :=
           Get_Name_String (Map_Ada_Enumerator_Name (S))'Length;
      begin
         --  Build the enumerator corresponding to the thread

         N := Make_Defining_Identifier (Map_Ada_Enumerator_Name (S));
         Insert_Node_In_List
           (N,
            Thread_Enumerator_List,
            Get_Thread_Enum_Pos'Access);
         Bind_AADL_To_Enumerator (Identifier (S), N);

         --  Build the representation clause for the enumerator

         N :=
           Make_Element_Association
             (Make_Defining_Identifier (Map_Ada_Enumerator_Name (S)),
              Make_Literal
                (New_Integer_Value
                   (Unsigned_Long_Long
                      (Get_Thread_Enum_Pos (Map_Ada_Enumerator_Name (S))),
                    1,
                    10)));
         Insert_Node_In_List
           (N,
            Thread_Enumerator_Pos_List,
            Get_Thread_Enum_Pos'Access);

         --  For each thread, build the corresponding element
         --  association and append it to the entity list.

         --  Get the Process parent of the thread

         P := Parent_Component (S);
         pragma Assert (AAU.Is_Process (P) or else AAU.Is_Abstract (P));

         if AAU.Is_Process (P) then
            N :=
              Make_Element_Association
                (Make_Defining_Identifier (Map_Ada_Enumerator_Name (S)),
                 Make_Defining_Identifier
                   (Map_Ada_Enumerator_Name (Parent_Subcomponent (P))));

         elsif AAU.Is_Abstract (P) then
            N :=
              Make_Element_Association
                (Make_Defining_Identifier (Map_Ada_Enumerator_Name (S)),
                 Make_Defining_Identifier (PN (P_My_Node)));

         end if;
         Insert_Node_In_List
           (N,
            Entity_Table_List,
            Get_Thread_Enum_Pos'Access);

         if Max_Entity_Image_Size < Img_Length then
            Max_Entity_Image_Size := Img_Length;
         end if;

         N :=
           Make_Element_Association
             (Make_Defining_Identifier (Map_Ada_Enumerator_Name (S)),
              Make_Literal (New_String_Value (Map_Ada_Enumerator_Name (S))));

         Insert_Node_In_List
           (N,
            Entity_Image_List,
            Get_Thread_Enum_Pos'Access);

         --  For each one of the thread ports, create its
         --  corresponding enumerator, representation clause and
         --  association.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  declare
                     Enum_Name : constant Name_Id :=
                       Map_Ada_Full_Feature_Name (F, 'K');
                     Port_Img_Length : constant Unsigned_Long_Long :=
                       Get_Name_String (Enum_Name)'Length;
                  begin
                     --  Visit the feature to compute the data size

                     if Kind (F) = K_Port_Spec_Instance
                       and then AAN.Is_Data (F)
                     then
                        Visit (Corresponding_Instance (F));
                     end if;

                     --  Create the enumerator corresponding to the
                     --  port.

                     N := Make_Defining_Identifier (Enum_Name);
                     Insert_Node_In_List
                       (N,
                        Port_Enumerator_List,
                        Get_Port_Enum_Pos'Access);
                     Bind_AADL_To_Enumerator (Identifier (F), N);

                     --  Build the representation clause for the
                     --  enumerator.

                     N :=
                       Make_Element_Association
                         (Make_Defining_Identifier (Enum_Name),
                          Make_Literal
                            (New_Integer_Value
                               (Unsigned_Long_Long
                                  (Get_Port_Enum_Pos (Enum_Name)),
                                1,
                                10)));
                     Insert_Node_In_List
                       (N,
                        Port_Enumerator_Pos_List,
                        Get_Port_Enum_Pos'Access);

                     --  For each port, build the corresponding
                     --  element association and append it to the
                     --  association list.

                     N :=
                       Make_Element_Association
                         (Make_Defining_Identifier (Enum_Name),
                          Make_Defining_Identifier
                            (Map_Ada_Enumerator_Name (S)));
                     Insert_Node_In_List
                       (N,
                        Port_Table_List,
                        Get_Port_Enum_Pos'Access);

                     --  For each port, build the corresponding
                     --  image association and append it to the
                     --  association list.

                     if Max_Port_Image_Size < Port_Img_Length then
                        Max_Port_Image_Size := Port_Img_Length;
                     end if;

                     N :=
                       Make_Element_Association
                         (Make_Defining_Identifier (Enum_Name),
                          Make_Literal (New_String_Value (Enum_Name)));
                     Insert_Node_In_List
                       (N,
                        Port_Image_List,
                        Get_Port_Enum_Pos'Access);
                  end;
               end if;

               F := Next_Node (F);
            end loop;

         end if;
      end Visit_Thread_Instance;

   end Package_Spec;

end Ocarina.Backends.PO_HI_Ada.Deployment;
