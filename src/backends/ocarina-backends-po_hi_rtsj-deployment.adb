------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.PO_HI_RTSJ.DEPLOYMENT                   --
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
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.RTSJ_Values;
with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.PO_HI_RTSJ.Mapping;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;

package body Ocarina.Backends.PO_HI_RTSJ.Deployment is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;
   use Ocarina.ME_AADL.AADL_Instances.Entities;

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.RTSJ_Values;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.PO_HI_RTSJ.Mapping;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;

   --  Global variables
   Main_Class             : Node_Id;
   Init_Statements        : List_Id;
   Class_Attributes       : List_Id;
   Node_Enumerator_List   : List_Id;
   Thread_Enumerator_List : List_Id;
   Port_Enumerator_List   : List_Id;
   Entities_Table_List    : List_Id;
   Ports_Table_List       : List_Id;

   Current_Process_Instance : Node_Id;
   Task_Offset              : Unsigned_Long_Long := 0;

   Node_Identifier     : Unsigned_Long_Long;
   Thread_Identifier   : Unsigned_Long_Long;
   Nb_Local_Threads    : Unsigned_Long_Long;
   Nb_Ports_Total      : Unsigned_Long_Long;
   Nb_Ports_In_Process : Unsigned_Long_Long;
   Max_Payload         : Unsigned_Long_Long;

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);

      function Added_Internal_Name (P : Node_Id; E : Node_Id) return Name_Id;
      function Is_Added (P : Node_Id; E : Node_Id) return Boolean;
      procedure Set_Added (P : Node_Id; E : Node_Id);

      function Need_Deliver (E : Node_Id) return Boolean;
      function Need_Send (E : Node_Id) return Boolean;

      --  Builds the maxPayloadSize constant declaration
      --  corresponding to the node E
      function Max_Payload_Size_Declaration (E : Node_Id) return Node_Id;

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

      ------------------
      -- Need_Deliver --
      ------------------
      function Need_Deliver (E : Node_Id) return Boolean is
         Result : Boolean := Has_In_Ports (E);
         S      : Node_Id;
      begin
         pragma Assert (AINU.Is_Process (E));

         if not Result and then not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));

            while Present (S) and then not Result loop
               if AINU.Is_Thread (Corresponding_Instance (S)) then
                  Result :=
                    Result or else Has_In_Ports (Corresponding_Instance (S));
               end if;

               S := AIN.Next_Node (S);
            end loop;
         end if;

         return Result;
      end Need_Deliver;

      ---------------
      -- Need_Send --
      ---------------
      function Need_Send (E : Node_Id) return Boolean is
         Result : Boolean := Has_Out_Ports (E);
         S      : Node_Id;
      begin
         pragma Assert (AINU.Is_Process (E));

         if not Result and then not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));

            while Present (S) and then not Result loop
               if AINU.Is_Thread (Corresponding_Instance (S)) then
                  Result :=
                    Result or else Has_Out_Ports (Corresponding_Instance (S));
               end if;

               S := AIN.Next_Node (S);
            end loop;
         end if;

         return Result;
      end Need_Send;

      ----------------------------------
      -- Max_Payload_Size_Declaration --
      ----------------------------------
      function Max_Payload_Size_Declaration (E : Node_Id) return Node_Id is
         pragma Unreferenced (E);

         N : Node_Id;
      begin
         --  The structure of a message payload is as follows :
         --  1 - A destination port
         --  2 - An optional timestamp
         --  3 - A data (of one of the marshallable types declared in
         --     the currently being generated file

         --  Size coresponding to (3) has been built incrementally
         --  during the visiting of AADL data component instance.
         --  Add the size corresponding to (2) and (1)

         --  16 bits is the size of the destination port
         --  64 bits is the size of the optional timestamp
         Max_Payload := Max_Payload + 16 + 64;

         N :=
           Make_Assignment_Statement
             (Make_Pointed_Notation
                (Make_Defining_Identifier (ON (O_Context)),
                 RE (RE_Max_Payload_Size)),
              Make_Literal (New_Int_Value (Max_Payload, 0, 10)));

         return N;
      end Max_Payload_Size_Declaration;

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

            when CC_Thread =>
               Visit_Thread_Instance (E);

            when CC_Data =>
               Visit_Data_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------
      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           RTN.Distributed_Application_Unit
             (RTN.Naming_Node (Backend_Node (Identifier (E))));
         P        : constant Node_Id := RTN.Entity (U);
         Root_Sys : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));
         N                 : Node_Id;
         C                 : Node_Id;
         Q                 : Node_Id;
         F                 : Node_Id;
         Src               : Node_Id;
         Dst               : Node_Id;
         Parent            : Node_Id;
         Spec              : Node_Id;
         Impl              : Node_Id;
         Init_Declarations : constant List_Id :=
           New_List (RTN.K_Declaration_List);
      begin
         pragma Assert (AINU.Is_System (Root_Sys));

         Set_Added (E, E);
         Current_Process_Instance := E;

         Push_Entity (P);
         Push_Entity (U);
         RTU.Set_Deployment_Source;

         --  Runtime and libraries imports
         Add_Import (RH (RH_Deployment));

         Class_Attributes       := New_List (K_Attribute_List);
         Init_Statements        := New_List (K_Statement_List);
         Node_Enumerator_List   := New_List (K_Enumeration_Literals);
         Thread_Enumerator_List := New_List (K_Enumeration_Literals);
         Port_Enumerator_List   := New_List (K_Enumeration_Literals);
         Entities_Table_List    := New_List (K_Enumeration_Literals);
         Ports_Table_List       := New_List (K_Enumeration_Literals);

         Node_Identifier     := 0;
         Thread_Identifier   := 0;
         Nb_Local_Threads    := 0;
         Nb_Ports_In_Process := 0;
         Nb_Ports_Total      := 0;

         if Need_Deliver (E) or else Need_Send (E) then
            Max_Payload := 0;
         end if;

         N :=
           Message_Comment
             ("For each node in the distributed" &
              " application, add a constant");
         RTU.Append_Node_To_List (N, Node_Enumerator_List);

         Q := AIN.First_Node (Subcomponents (Root_Sys));

         while Present (Q) loop
            if AINU.Is_Process (Corresponding_Instance (Q)) then
               if Is_Added (Corresponding_Instance (Q), E) then

                  N :=
                    Make_Variable_Declaration
                      (Visibility =>
                         Make_List_Id
                           (RE (RE_Public),
                            RE (RE_Static),
                            RE (RE_Final)),
                       Used_Type           => New_Node (K_Int),
                       Defining_Identifier =>
                         Make_Defining_Identifier
                           (Map_RTSJ_Enumerator_Name (Q)),
                       Value =>
                         Make_Literal
                           (New_Int_Value (Node_Identifier, 0, 10)));
                  RTU.Append_Node_To_List (N, Node_Enumerator_List);
                  Node_Identifier := Node_Identifier + 1;

               end if;
            end if;

            Q := AIN.Next_Node (Q);
         end loop;

         N := Make_Enumerator (Node_Enumerator_List);
         RTU.Append_Node_To_List (N, Class_Attributes);

         N :=
           Message_Comment
             ("For each task in the distributed" &
              " application, add a constant");
         RTU.Append_Node_To_List (N, Thread_Enumerator_List);

         N :=
           Message_Comment
             ("For each thread port in the distributed application nodes, "
                & "add a number between 0 and 65536");
         RTU.Append_Node_To_List (N, Port_Enumerator_List);

         --  Visit all the subcomponents of the process
         if not AINU.Is_Empty (Subcomponents (E)) then
            C := AIN.First_Node (Subcomponents (E));

            while Present (C) loop

               --  Visit the component instance corresponding to the
               --  subcomponent S.
               Visit (Corresponding_Instance (C));
               C := AIN.Next_Node (C);
            end loop;
         end if;

         N := Make_Enumerator (Thread_Enumerator_List);
         RTU.Append_Node_To_List (N, Class_Attributes);

         N := Make_Enumerator (Port_Enumerator_List);
         RTU.Append_Node_To_List (N, Class_Attributes);

         --  For each of the processes P connected to E, (1) we add an
         --  enumerator corresponding to P and (2) for each one of the
         --  threads of P, we add an enumerator.
         if not AINU.Is_Empty (Features (E)) then
            F := AIN.First_Node (Features (E));

            while Present (F) loop

               --  The sources of F
               if not AINU.Is_Empty (AIN.Sources (F)) then
                  Src := AIN.First_Node (AIN.Sources (F));

                  while Present (Src) loop

                     Parent := Parent_Component (Item (Src));

                     if AINU.Is_Process (Parent) and then Parent /= E then
                        Set_Added (Parent, E);

                        --  Traverse all the subcomponents of Parent
                        if not AINU.Is_Empty (Subcomponents (Parent)) then
                           C := AIN.First_Node (Subcomponents (Parent));

                           while Present (C) loop
                              Visit (Corresponding_Instance (C));

                              C := AIN.Next_Node (C);
                           end loop;
                        end if;

                     end if;

                     Src := AIN.Next_Node (Src);
                  end loop;
               end if;

               --  The destinations of F
               if not AINU.Is_Empty (Destinations (F)) then
                  Dst := AIN.First_Node (Destinations (F));

                  while Present (Dst) loop
                     Parent := Parent_Component (Item (Dst));

                     if AINU.Is_Process (Parent) and then Parent /= E then
                        Set_Added (Parent, E);

                        if not AINU.Is_Empty (Subcomponents (Parent)) then
                           C := AIN.First_Node (Subcomponents (Parent));

                           while Present (C) loop
                              Visit (Corresponding_Instance (C));

                              C := AIN.Next_Node (C);
                           end loop;
                        end if;
                     end if;

                     Dst := AIN.Next_Node (Dst);
                  end loop;
               end if;

               F := AIN.Next_Node (F);
            end loop;
         end if;

         --  entitiesTable declaration
         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Left_Member  => Make_Defining_Identifier (ON (O_Context)),
                   Right_Member => RE (RE_Entities_Table)),
              Expression =>
                Make_New_Statement
                  (Defining_Identifier => New_Node (K_Int),
                   Parameters          =>
                     Make_List_Id
                       (Make_Literal
                          (New_Int_Value (Thread_Identifier, 0, 10))),
                   Is_Array => True));

         N :=
           Make_Full_Array_Declaration
             (Array_Declaration => N,
              Array_Assignments => Entities_Table_List);
         RTU.Append_Node_To_List (N, Init_Statements);

         --  portsTable declaration
         if Nb_Ports_Total /= 0 then
            N :=
              Make_Assignment_Statement
                (Defining_Identifier =>
                   Make_Pointed_Notation
                     (Left_Member => Make_Defining_Identifier (ON (O_Context)),
                      Right_Member => RE (RE_Ports_Table)),
                 Expression =>
                   Make_New_Statement
                     (Defining_Identifier => New_Node (K_Int),
                      Parameters          =>
                        Make_List_Id
                          (Make_Literal
                             (New_Int_Value (Nb_Ports_In_Process, 0, 10))),
                      Is_Array => True));
         end if;

         N :=
           Make_Full_Array_Declaration
             (Array_Declaration => N,
              Array_Assignments => Ports_Table_List);
         RTU.Append_Node_To_List (N, Init_Statements);

         if Node_Identifier > 1 then
            N :=
              Make_Assignment_Statement
                (Defining_Identifier =>
                   Make_Pointed_Notation
                     (Make_Defining_Identifier (ON (O_Context)),
                      RE (RE_Transport)),
                 Expression =>
                   Make_New_Statement
                     (Defining_Identifier =>
                        Make_Defining_Identifier
                          (ON (O_Transport_High_Level_Impl))));
            RTU.Append_Node_To_List (N, Init_Statements);
         end if;

         --  entitiesOffset initialization
         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Left_Member  => Make_Defining_Identifier (ON (O_Context)),
                   Right_Member => RE (RE_Entities_Offset)),
              Expression => Make_Literal (New_Int_Value (Task_Offset, 0, 10)));
         RTU.Append_Node_To_List (N, Init_Statements);
         Task_Offset := Task_Offset + Nb_Local_Threads;

         --  Initialization of variables of the Context class

         --  Variable : myNode
         --  Identifier of the current process
         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                (Make_Pointed_Notation
                   (Left_Member  => Make_Defining_Identifier (ON (O_Context)),
                    Right_Member => RE (RE_My_Node))),
              Expression =>
                Make_Pointed_Notation
                  (Left_Member => Make_Defining_Identifier (ON (O_Deployment)),
                   Right_Member =>
                     Make_Defining_Identifier
                       (Map_RTSJ_Enumerator_Name (Parent_Subcomponent (E)))));
         RTU.Append_Node_To_List (N, Init_Declarations);

         --  Variable : nbNodes
         --  Total number of processes
         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Left_Member  => Make_Defining_Identifier (ON (O_Context)),
                   Right_Member => RE (RE_Nb_Nodes)),
              Expression =>
                Make_Literal (New_Int_Value (Node_Identifier, 0, 10)));
         RTU.Append_Node_To_List (N, Init_Declarations);

         --  Variable : nbLocalEntities
         --  Total number of threads for the current process
         N :=
           Make_Assignment_Statement
             (Defining_Identifier =>
                Make_Pointed_Notation
                  (Left_Member  => Make_Defining_Identifier (ON (O_Context)),
                   Right_Member => RE (RE_Nb_Local_Entities)),
              Expression =>
                Make_Literal (New_Int_Value (Nb_Local_Threads, 0, 10)));
         RTU.Append_Node_To_List (N, Init_Declarations);

         --  Naming initialization
         if not AINU.Is_Empty (Features (E)) then
            F := AIN.First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  N :=
                    Make_Pointed_Notation
                      (Make_Defining_Identifier (ON (O_Naming)),
                       Make_Call_Function
                         (Make_Defining_Identifier (MN (M_Initialization))));
                  RTU.Append_Node_To_List (N, Init_Statements);
                  exit;
               end if;
               F := AIN.Next_Node (F);
            end loop;
         end if;

         --  TransportHighLevel and maxPauloadSize constant initializations
         if Need_Deliver (E) or else Need_Send (E) then
            N :=
              Make_Assignment_Statement
                (Make_Pointed_Notation
                   (Make_Defining_Identifier (ON (O_Context)),
                    Make_Defining_Identifier (VN (V_Transport))),
                 Make_New_Statement
                   (Make_Defining_Identifier (ON (O_Transport_High_Level))));
            RTU.Append_Node_To_List (N, Init_Statements);

            N := Max_Payload_Size_Declaration (E);
            RTU.Append_Node_To_List (N, Init_Declarations);
         end if;

         Spec :=
           Make_Function_Specification
             (Visibility => Make_List_Id (RE (RE_Public), RE (RE_Static)),
              Defining_Identifier =>
                Make_Defining_Identifier (MN (M_Initialization)),
              Return_Type => New_Node (K_Void));

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Declarations  => Init_Declarations,
              Statements    => Init_Statements);

         Main_Class :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Deployment)),
              Attributes => Class_Attributes,
              Methods    => Make_List_Id (Impl));
         RTU.Append_Node_To_List (Main_Class, RTN.Statements (Current_File));

         Pop_Entity;  --  U
         Pop_Entity;  --  P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------
      procedure Visit_Thread_Instance (E : Node_Id) is
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
         F : Node_Id;
      begin
         N :=
           Make_Variable_Declaration
             (Visibility =>
                Make_List_Id (RE (RE_Public), RE (RE_Static), RE (RE_Final)),
              Defining_Identifier =>
                Make_Defining_Identifier (Map_RTSJ_Enumerator_Name (S)),
              Used_Type => New_Node (K_Int),
              Value     =>
                Make_Literal (New_Int_Value (Thread_Identifier, 0, 10)));
         RTU.Append_Node_To_List (N, Thread_Enumerator_List);

         Thread_Identifier := Thread_Identifier + 1;
         Nb_Local_Threads  := Nb_Local_Threads + 1;

         if Parent_Component (Parent_Subcomponent (E)) =
           Current_Process_Instance
         then
            --  entitiesTable assignment
            N :=
              Make_Assignment_Statement
                (Defining_Identifier =>
                   Make_Pointed_Notation
                     (Make_Defining_Identifier (ON (O_Context)),
                      Make_Array_Value
                        (Defining_Identifier => RE (RE_Entities_Table),
                         Array_Item          =>
                           Make_Defining_Identifier
                             (Map_RTSJ_Enumerator_Name (S)))),
                 Expression =>
                   Make_Defining_Identifier
                     (Map_RTSJ_Enumerator_Name
                        (Parent_Subcomponent (Parent_Component (S)))));
            RTU.Append_Node_To_List (N, Entities_Table_List);
         end if;

         if not AINU.Is_Empty (Features (E)) then
            F := AIN.First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then

                  if Parent_Component (Parent_Subcomponent (E)) =
                    Current_Process_Instance
                  then
                     --  portsTable enumeration
                     N :=
                       Make_Assignment_Statement
                         (Defining_Identifier =>
                            Make_Pointed_Notation
                              (Make_Defining_Identifier (ON (O_Context)),
                               Make_Array_Value
                                 (Defining_Identifier => RE (RE_Ports_Table),
                                  Array_Item          =>
                                    Make_Defining_Identifier
                                      (Map_RTSJ_Enumerator_Name (F)))),
                          Expression =>
                            Make_Defining_Identifier
                              (Map_RTSJ_Enumerator_Name (S)));
                     RTU.Append_Node_To_List (N, Ports_Table_List);

                     Nb_Ports_In_Process := Nb_Ports_In_Process + 1;
                  end if;

                  --  ports enumeration
                  N :=
                    Make_Variable_Declaration
                      (Visibility =>
                         Make_List_Id
                           (RE (RE_Public),
                            RE (RE_Static),
                            RE (RE_Final)),
                       Defining_Identifier =>
                         Make_Defining_Identifier
                           (Map_RTSJ_Enumerator_Name (F)),
                       Used_Type => New_Node (K_Int),
                       Value     =>
                         Make_Literal
                           (New_Int_Value (Nb_Ports_Total, 0, 100)));
                  RTU.Append_Node_To_List (N, Port_Enumerator_List);

                  Nb_Ports_Total := Nb_Ports_Total + 1;

                  --  Visit data to compute the maxPayloadSize
                  if AIN.Is_Data (F) then
                     Visit (Corresponding_Instance (F));
                  end if;

               end if;

               F := AIN.Next_Node (F);
            end loop;
         end if;

      end Visit_Thread_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------
      procedure Visit_Data_Instance (E : Node_Id) is
      begin
         --  Visit the data only once
         if No (Get_Handling (E, By_Name, H_RTSJ_Deployment)) then
            if Get_Source_Language (E) = Language_RTSJ then
               Display_Located_Error
                 (AIN.Loc (E),
                  "This data type cannot be used in thread or process " &
                  "features",
                  Fatal => True);
            end if;

            if Get_Data_Representation (E) /= Data_With_Accessors then
               declare
                  Data_Size : Unsigned_Long_Long;
               begin
                  if Get_Data_Size (E) /= Null_Size then
                     Data_Size := To_Bits (Get_Data_Size (E));
                  end if;

                  if Data_Size > Max_Payload then
                     Max_Payload := Data_Size;
                  end if;
               end;

               --  Mark the data as being handled
               Set_Handling (E, By_Name, H_RTSJ_Deployment, E);
            end if;
         end if;
      end Visit_Data_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------
      procedure Visit_System_Instance (E : Node_Id) is
         N : Node_Id;
      begin
         Push_Entity (RTSJ_Root);

         if not AINU.Is_Empty (Subcomponents (E)) then
            N := AIN.First_Node (Subcomponents (E));
            while Present (N) loop
               Visit (Corresponding_Instance (N));
               N := AIN.Next_Node (N);
            end loop;
         end if;

         Pop_Entity;
      end Visit_System_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_RTSJ.Deployment;
