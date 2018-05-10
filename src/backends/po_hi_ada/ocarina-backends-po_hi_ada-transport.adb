------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.PO_HI_ADA.TRANSPORT                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2018 ESA & ISAE.      --
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
with Ocarina.Backends.Ada_Values;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_HI_Ada.Mapping;
with Ocarina.Backends.PO_HI_Ada.Runtime;

package body Ocarina.Backends.PO_HI_Ada.Transport is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_HI_Ada.Mapping;
   use Ocarina.Backends.PO_HI_Ada.Runtime;
   use Ocarina.Backends.Ada_Values;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   function Deliver_Spec (E : Node_Id; With_Aspect : Boolean) return Node_Id;
   --  Create a subprogram specification corresponding to the
   --  message delivery routine.

   function Send_Spec (E : Node_Id; With_Aspect : Boolean) return Node_Id;
   --  Create the subprogram specification corresponding to the
   --  transport layer Send routine.

   ------------------
   -- Deliver_Spec --
   ------------------

   function Deliver_Spec (E : Node_Id; With_Aspect : Boolean) return Node_Id is
      pragma Unreferenced (E);

      Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
      N       : Node_Id;
      Aspect : Node_Id := No_Node;
   begin
      --  Entity

      N :=
        Make_Parameter_Specification
          (Defining_Identifier => Make_Defining_Identifier (PN (P_Entity)),
           Subtype_Mark        => RE (RE_Entity_Type),
           Parameter_Mode      => Mode_In);
      Append_Node_To_List (N, Profile);

      --  Message

      N :=
        Make_Parameter_Specification
          (Defining_Identifier => Make_Defining_Identifier (PN (P_Message)),
           Subtype_Mark        => RE (RE_Stream_Element_Array),
           Parameter_Mode      => Mode_In);
      Append_Node_To_List (N, Profile);

      --  Pre-condition

      if Add_SPARK2014_Annotations and then With_Aspect then
         Aspect := Make_Aspect_Specification
           (Make_List_Id
              (Make_Aspect (ASN (A_Pre),
                            Make_Pre
                              (Make_Subprogram_Call
                                 (RE (RE_Valid),
                                  Make_List_Id (Make_Defining_Identifier
                                                  (PN (P_Message))))))));
      end if;

      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Deliver)),
           Parameter_Profile   => Profile,
           Return_Type         => No_Node,
           Aspect_Specification => Aspect);

      return N;
   end Deliver_Spec;

   ---------------
   -- Send_Spec --
   ---------------

   function Send_Spec (E : Node_Id; With_Aspect : Boolean) return Node_Id is
      pragma Unreferenced (E);

      Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
      Aspect  : Node_Id := No_Node;
      N       : Node_Id;
   begin
      --  From

      N :=
        Make_Parameter_Specification
          (Defining_Identifier => Make_Defining_Identifier (PN (P_From)),
           Subtype_Mark        => RE (RE_Entity_Type),
           Parameter_Mode      => Mode_In);
      Append_Node_To_List (N, Profile);

      --  Entity

      N :=
        Make_Parameter_Specification
          (Defining_Identifier => Make_Defining_Identifier (PN (P_Entity)),
           Subtype_Mark        => RE (RE_Entity_Type),
           Parameter_Mode      => Mode_In);
      Append_Node_To_List (N, Profile);

      --  Message

      N :=
        Make_Parameter_Specification
          (Defining_Identifier => Make_Defining_Identifier (PN (P_Message)),
           Subtype_Mark        => RE (RE_Message_Type),
           Parameter_Mode      => Mode_In);
      Append_Node_To_List (N, Profile);

      --  Pre-condition
      if Add_SPARK2014_Annotations and then With_Aspect then
         Aspect := Make_Aspect_Specification
           (Make_List_Id
              (Make_Aspect
                 (ASN (A_Pre),
                  Make_Pre
                    (Make_Subprogram_Call
                       (RE (Re_Not_Empty),
                        Make_List_Id (Make_Defining_Identifier
                                     (PN (P_Message))))))));
      end if;

      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Send)),
           Parameter_Profile   => Profile,
           Return_Type         => RE (RE_Error_Kind),
           Aspect_Specification => Aspect);

      return N;
   end Send_Spec;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

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

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         N : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Transport_Spec;

         --  Generate a delivery spec

         N := Deliver_Spec (E, With_Aspect => True);
         Bind_AADL_To_Deliver (Identifier (E), N);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         --  Generate the message sending spec if necessary

         N := Send_Spec (E, With_Aspect => True);
         Bind_AADL_To_Send (Identifier (E), N);
         Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
      begin
         Push_Entity (Ada_Root);

         --  Visit all the subcomponents of the system

         Visit_Subcomponents_Of (E);

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

   end Package_Spec;

   ------------------
   -- Package_Body --
   ------------------

   package body Package_Body is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      function Internal_Deliver_Spec (E : Node_Id) return Node_Id;
      function Internal_Deliver_Body (E : Node_Id) return Node_Id;
      --  Create the internal delivery routine corresponding to the
      --  thread E.

      function Internal_Send_Spec (E : Node_Id) return Node_Id;
      function Internal_Send_Body (E : Node_Id) return Node_Id;
      --  Create the internal sending routine corresponding to the
      --  thread E.

      function Deliver_Body (E : Node_Id) return Node_Id;
      --  Create a subprogram implementation corresponding to the
      --  message delivery routine.

      function Send_Body (E : Node_Id) return Node_Id;
      --  Create a subprogram implementation corresponding to the
      --  message sending routine.

      ------------------
      -- Deliver_Body --
      ------------------

      function Deliver_Body (E : Node_Id) return Node_Id is
         Spec : constant Node_Id := Deliver_Spec (E, With_Aspect => False);

         Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Alternatives : constant List_Id := New_List (ADN.K_List_Id);
         N            : Node_Id;
         T            : Node_Id;
      begin
         pragma Assert (AAU.Is_Process (E));

         if not Need_Deliver (E) then
            --  Generate a dummy Deliver

            N :=
              Make_Pragma_Statement
                (Pragma_Unreferenced,
                 Make_List_Id
                   (Make_Defining_Identifier (PN (P_Entity)),
                    Make_Defining_Identifier (PN (P_Message))));
            Append_Node_To_List (N, Declarations);

            N := Make_Null_Statement;
            Append_Node_To_List (N, Statements);
         else
            --  Declarative part

            N := Make_Used_Package (RU (RU_PolyORB_HI_Generated_Deployment));
            Append_Node_To_List (N, Declarations);

            N :=
              Make_Object_Declaration
                (Defining_Identifier => Make_Defining_Identifier (PN (P_Msg)),
                 Object_Definition   => RE (RE_Message_Type));
            Append_Node_To_List (N, Declarations);

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (PN (P_Value)),
                 Object_Definition => RE (RE_Unsigned_16));
            Append_Node_To_List (N, Declarations);

            N :=
              Make_Object_Declaration
                (Defining_Identifier => Make_Defining_Identifier (PN (P_Port)),
                 Object_Definition   => RE (RE_Port_Type_1));
            Append_Node_To_List (N, Declarations);

            --  Add a use clause for the
            --  Ada.Streams.Stream_Element_Offset type to have
            --  visibility on its operators.

            N := Make_Used_Type (RE (RE_Stream_Element_Offset));
            Append_Node_To_List (N, Declarations);

            --  Statements

            --  Get the message payload

            N :=
              Make_Expression
                (Make_Attribute_Designator
                   (Make_Designator (PN (P_Message)),
                    A_First),
                 Op_Plus,
                 RE (RE_Header_Size));

            N :=
              Make_Range_Constraint
                (N,
                 Make_Attribute_Designator
                   (Make_Designator (PN (P_Message)),
                    A_Last));

            N :=
              Make_Subprogram_Call
                (Make_Designator (PN (P_Message)),
                 Make_List_Id (N));

            N :=
              Make_Subprogram_Call
                (RE (RE_Write),
                 Make_List_Id (Make_Defining_Identifier (PN (P_Msg)), N));
            Append_Node_To_List (N, Statements);

            --  Unmarshall the destination port

            N :=
              Make_Subprogram_Call
                (RE (RE_Unmarshall_1),
                 Make_List_Id
                   (Make_Defining_Identifier (PN (P_Value)),
                    Make_Defining_Identifier (PN (P_Msg))));
            Append_Node_To_List (N, Statements);

            N :=
              Make_Subprogram_Call
                (RE (RE_Corresponding_Port),
                 Make_List_Id (Make_Defining_Identifier (PN (P_Value))));
            N :=
              Make_Assignment_Statement
                (Variable_Identifier => Make_Defining_Identifier (PN (P_Port)),
                 Expression          => N);

            Append_Node_To_List (N, Statements);

            --  The case statement: for each thread of the current
            --  process, we generate a case statement alternative to
            --  call its specific delivery routine.

            T := First_Node (Subcomponents (E));

            while Present (T) loop
               if AAU.Is_Thread (Corresponding_Instance (T))
                 and then Has_In_Ports (Corresponding_Instance (T))
               then
                  --  Generate the spec of the internal delivery
                  --  routine of thread T. It is important to do this
                  --  before adding the global delivery body to the
                  --  package statemnets because it uses the internal
                  --  delivery routines.

                  N := Internal_Deliver_Spec (Corresponding_Instance (T));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

                  --  Call the internal delivery routine of the thread

                  N :=
                    Make_Subprogram_Call
                      (Make_Defining_Identifier
                         (Map_Deliver_Name (Corresponding_Instance (T))),
                       Make_List_Id
                         (Make_Defining_Identifier (PN (P_Port)),
                          Make_Subprogram_Call
                            (RE (RE_Sender),
                             Make_List_Id (Make_Designator (PN (P_Message)))),
                          Make_Defining_Identifier (PN (P_Msg))));

                  --  The case statement alternative

                  N :=
                    Make_Elsif_Statement
                      (Make_Expression
                         (Make_Defining_Identifier (PN (P_Entity)),
                          Op_Equal,
                          Extract_Enumerator (Corresponding_Instance (T))),
                       Make_List_Id (N));
                  Append_Node_To_List (N, Alternatives);
               end if;

               T := Next_Node (T);
            end loop;

            declare
               Elsif_Statements : constant List_Id := New_List (ADN.K_List_Id);

            begin
               ADN.Set_First_Node
                 (Elsif_Statements,
                  ADN.Next_Node (ADN.First_Node (Alternatives)));

               N :=
                 Make_If_Statement
                   (Condition => ADN.Condition (ADN.First_Node (Alternatives)),
                    Then_Statements =>
                      ADN.Then_Statements (ADN.First_Node (Alternatives)),
                    Elsif_Statements => Elsif_Statements);

               Append_Node_To_List (N, Statements);
            end;
         end if;

         N := Make_Subprogram_Implementation (Spec, Declarations, Statements);
         return N;
      end Deliver_Body;

      ---------------
      -- Send_Body --
      ---------------

      function Send_Body (E : Node_Id) return Node_Id is
         Spec : constant Node_Id := Send_Spec (E, With_Aspect => False);

         Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Alternatives : constant List_Id := New_List (ADN.K_List_Id);
         N            : Node_Id;
         T            : Node_Id;
         Msg_T : Node_Id;

      begin
         pragma Assert (AAU.Is_Process (E));

         if not Need_Send (E) then
            --  Generate a dummy Send

            N :=
              Make_Pragma_Statement
                (Pragma_Unreferenced,
                 Make_List_Id
                   (Make_Defining_Identifier (PN (P_From)),
                    Make_Defining_Identifier (PN (P_Entity)),
                    Make_Defining_Identifier (PN (P_Message))));
            Append_Node_To_List (N, Declarations);

            N :=
              Make_Qualified_Expression
                (RE (RE_Error_Kind),
                 Make_Record_Aggregate
                   (Make_List_Id (RE (RE_Error_Transport))));
            N := Make_Return_Statement (N);
            Append_Node_To_List (N, Statements);

         else
            --  Declarative part

            N := Make_Used_Package (RU (RU_PolyORB_HI_Generated_Deployment));
            Append_Node_To_List (N, Declarations);

            N :=
              Make_Range_Constraint
                (Make_Literal (New_Integer_Value (1, 1, 10)),
                 Make_Subprogram_Call
                   (RE (RE_Size),
                   Make_List_Id (Make_Defining_Identifier (PN (P_Message)))));

            Msg_T :=
              Make_Subprogram_Call
                (RE (RE_Stream_Element_Array),
                 Make_List_Id (N));

            N :=
              Make_Object_Declaration
                (Defining_Identifier => Make_Defining_Identifier (PN (P_Msg)),
                 Constant_Present    => False,
                 Object_Definition   => Msg_T);
            Append_Node_To_List (N, Declarations);

            --  Statements

            --  Call Encapsulate

            N := Make_Subprogram_Call
              (RE (RE_Encapsulate),
               Make_List_Id
                 (Make_Defining_Identifier (PN (P_Message)),
                  Make_Defining_Identifier (PN (P_From)),
                  Make_Defining_Identifier (PN (P_Entity)),
                  Make_Defining_Identifier (PN (P_Msg))));
            Append_Node_To_List (N, Statements);

            --  The if/elsif statement: for each thread of the current
            --  process, we generate a case statement alternative to
            --  call its specific sending routine.

            T := First_Node (Subcomponents (E));

            while Present (T) loop
               if AAU.Is_Thread (Corresponding_Instance (T))
                 and then Has_Out_Ports (Corresponding_Instance (T))
               then
                  --  Generate the spec of the internal sending
                  --  routine of thread T. It is important to do this
                  --  before adding the global sending body to the
                  --  package statemnets because it uses the internal
                  --  sending routines.

                  N := Internal_Send_Spec (Corresponding_Instance (T));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

                  --  Call the internal sending routine of the thread

                  N :=
                    Make_Subprogram_Call
                      (Make_Defining_Identifier
                         (Map_Send_Name (Corresponding_Instance (T))),
                       Make_List_Id
                         (Make_Defining_Identifier (PN (P_Entity)),
                          Make_Defining_Identifier (PN (P_Msg))));
                  N := Make_Return_Statement (N);

                  --  The case statement alternative

                  N :=
                    Make_Elsif_Statement
                      (Make_Expression
                         (Make_Defining_Identifier (PN (P_From)),
                          Op_Equal,
                          Extract_Enumerator (Corresponding_Instance (T))),
                       Make_List_Id (N));
                  Append_Node_To_List (N, Alternatives);
               end if;

               T := Next_Node (T);
            end loop;

            declare
               Elsif_Statements : constant List_Id := New_List (ADN.K_List_Id);
               Else_Statements  : constant List_Id := New_List (ADN.K_List_Id);

            begin
               if Present (ADN.First_Node (Alternatives)) then
                  N :=
                    Make_Qualified_Expression
                      (RE (RE_Error_Kind),
                       Make_Record_Aggregate
                         (Make_List_Id (RE (RE_Error_Transport))));
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Else_Statements);

                  ADN.Set_First_Node
                    (Elsif_Statements,
                     ADN.Next_Node (ADN.First_Node (Alternatives)));

                  N :=
                    Make_If_Statement
                      (Condition =>
                         ADN.Condition (ADN.First_Node (Alternatives)),
                       Then_Statements =>
                         ADN.Then_Statements (ADN.First_Node (Alternatives)),
                       Elsif_Statements => Elsif_Statements,
                       Else_Statements  => Else_Statements);

                  Append_Node_To_List (N, Statements);
               else
                  N :=
                    Make_Qualified_Expression
                      (RE (RE_Error_Kind),
                       Make_Record_Aggregate
                         (Make_List_Id (RE (RE_Error_Transport))));
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Statements);
               end if;
            end;
         end if;

         N := Make_Subprogram_Implementation (Spec, Declarations, Statements);
         return N;
      end Send_Body;

      ---------------------------
      -- Internal_Deliver_Spec --
      ---------------------------

      function Internal_Deliver_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N       : Node_Id;
      begin
         --  The Port parameter

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Port)),
              Subtype_Mark        => RE (RE_Port_Type_1),
              Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  The Sender parameter

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_From)),
              Subtype_Mark        => RE (RE_Entity_Type),
              Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  The Msg parameter

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Msg)),
              Subtype_Mark        => RE (RE_Message_Type),
              Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (N, Profile);

         --  The subprogram spec

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (Map_Deliver_Name (E)),
              Parameter_Profile => Profile,
              Return_Type       => No_Node);
         return N;
      end Internal_Deliver_Spec;

      ---------------------------
      -- Internal_Deliver_Body --
      ---------------------------

      function Internal_Deliver_Body (E : Node_Id) return Node_Id is
         Spec         : constant Node_Id := Internal_Deliver_Spec (E);
         Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Alternatives : constant List_Id := New_List (ADN.K_List_Id);
         N            : Node_Id;
         F            : Node_Id;

         Time_Stamp_Declared : Boolean := False;
      begin
         if not AAU.Is_Empty (Features (E)) and then Has_In_Ports (E) then
            --  Add a 'use' clause to the Activity package

            Add_With_Package
              (RU (RU_PolyORB_HI_Generated_Activity, False),
               Used => True);

            --  Declare a local variable of type the thread interface

            N := Make_Used_Package (RU (RU_PolyORB_HI_Generated_Deployment));
            Append_Node_To_List (N, Declarations);

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (VN (V_Thread_Interface)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Port_Interface_Name (E)));
            Append_Node_To_List (N, Declarations);

            --  For each port of the thread, create a switch case
            --  alternative to store the message to the proper
            --  destination.

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
                  declare
                     St : constant List_Id := New_List (ADN.K_Statement_List);
                     Call_Profile : constant List_Id :=
                       New_List (ADN.K_List_Id);
                  begin
                     --  In case of a data port, unmarshalls the time
                     --  stamp.

                     if not Is_Event (F) then
                        --  Declare the time stamp local variable if
                        --  it has not been done yet.

                        if not Time_Stamp_Declared then
                           N :=
                             Make_Object_Declaration
                               (Defining_Identifier =>
                                  Make_Defining_Identifier (VN (V_Time_Stamp)),
                                Object_Definition => RE (RE_Time));
                           Append_Node_To_List (N, Declarations);

                           Time_Stamp_Declared := True;
                        end if;

                        N :=
                          Make_Subprogram_Call
                            (RE (RE_Unmarshall_2),
                             Make_List_Id
                               (Make_Defining_Identifier (VN (V_Time_Stamp)),
                                Make_Defining_Identifier (PN (P_Msg))));
                        Append_Node_To_List (N, St);
                     end if;

                     --  Unmarshall the received message

                     N :=
                       Make_Subprogram_Call
                         (Extract_Designator
                            (ADN.Unmarshall_Node
                               (Backend_Node (Identifier (E)))),
                          Make_List_Id
                            (Map_Ada_Defining_Identifier (F),
                             Make_Defining_Identifier
                               (VN (V_Thread_Interface)),
                             Make_Defining_Identifier (PN (P_Msg))));
                     Append_Node_To_List (N, St);

                     --  Store the received message

                     N :=
                       Extract_Designator
                         (ADN.Store_Received_Message_Node
                            (Backend_Node (Identifier (E))));

                     Append_Node_To_List
                       (Extract_Enumerator (E),
                        Call_Profile);

                     Append_Node_To_List
                       (Make_Defining_Identifier (VN (V_Thread_Interface)),
                        Call_Profile);

                     Append_Node_To_List
                       (Make_Defining_Identifier (PN (P_From)),
                        Call_Profile);

                     if not Is_Event (F) then
                        Append_Node_To_List
                          (Make_Defining_Identifier (VN (V_Time_Stamp)),
                           Call_Profile);
                     end if;

                     N := Make_Subprogram_Call (N, Call_Profile);
                     Append_Node_To_List (N, St);

                     --  Create the case statement alternative

                     N :=
                       Make_Elsif_Statement
                         (Make_Expression
                            (Make_Defining_Identifier (PN (P_Port)),
                             Op_Equal,
                             Extract_Enumerator (F)),
                          St);
                     Append_Node_To_List (N, Alternatives);
                  end;
               end if;

               F := Next_Node (F);
            end loop;

            declare
               Elsif_Statements : constant List_Id := New_List (ADN.K_List_Id);

            begin
               ADN.Set_First_Node
                 (Elsif_Statements,
                  ADN.Next_Node (ADN.First_Node (Alternatives)));

               N :=
                 Make_If_Statement
                   (Condition => ADN.Condition (ADN.First_Node (Alternatives)),
                    Then_Statements =>
                      ADN.Then_Statements (ADN.First_Node (Alternatives)),
                    Elsif_Statements => Elsif_Statements);

               Append_Node_To_List (N, Statements);
            end;
         end if;

         N := Make_Subprogram_Implementation (Spec, Declarations, Statements);
         return N;
      end Internal_Deliver_Body;

      ------------------------
      -- Internal_Send_Spec --
      ------------------------

      function Internal_Send_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N       : Node_Id;
      begin
         --  Entity

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Entity)),
              Subtype_Mark        => RE (RE_Entity_Type),
              Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  Message

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Message)),
              Subtype_Mark        => RE (RE_Stream_Element_Array),
              Parameter_Mode      => Mode_In);
         Append_Node_To_List (N, Profile);

         --  The subprogram spec

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (Map_Send_Name (E)),
              Parameter_Profile => Profile,
              Return_Type       => RE (RE_Error_Kind));
         return N;
      end Internal_Send_Spec;

      ------------------------
      -- Internal_Send_Body --
      ------------------------

      function Internal_Send_Body (E : Node_Id) return Node_Id is
         Handled_Thread : constant String := "%HandledFor%";

         function Is_Handled (T : Node_Id) return Boolean;
         procedure Set_Handled (T : Node_Id);
         --  Used to avoid duplicating case statement alternatives

         ----------------
         -- Is_Handled --
         ----------------

         function Is_Handled (T : Node_Id) return Boolean is
            I_Name : constant Name_Id :=
              Get_String_Name
                (Node_Id'Image (T) & Handled_Thread & Node_Id'Image (E));
         begin
            return Get_Name_Table_Byte (I_Name) = 1;
         end Is_Handled;

         -----------------
         -- Set_Handled --
         -----------------

         procedure Set_Handled (T : Node_Id) is
            I_Name : constant Name_Id :=
              Get_String_Name
                (Node_Id'Image (T) & Handled_Thread & Node_Id'Image (E));
         begin
            Set_Name_Table_Byte (I_Name, 1);
         end Set_Handled;

         Spec         : constant Node_Id := Internal_Send_Spec (E);
         Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Alternatives : constant List_Id := New_List (ADN.K_List_Id);
         N            : Node_Id;
         F            : Node_Id;
      begin
         if not AAU.Is_Empty (Features (E)) and then Has_Out_Ports (E) then
            --  We loop through all the OUT ports of the thread, then
            --  through all their destinations and create a switch
            --  case alternative for each thread that may be a
            --  destination of E to send the message using the proper
            --  transport layer.

            N :=
              Make_Pragma_Statement
                (Pragma_Warnings,
                 Make_List_Id
                   (RE (RE_Off),
                    Make_Defining_Identifier (PN (P_Message))));
            Append_Node_To_List (N, Declarations);
            N :=
              Make_Pragma_Statement
                (Pragma_Warnings,
                 Make_List_Id
                   (RE (RE_Off),
                    Make_Defining_Identifier (PN (P_Entity))));
            Append_Node_To_List (N, Declarations);

            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then
                  declare
                     Dest_List : constant List_Id := Get_Destination_Ports (F);
                     Dest_Th   : Node_Id;
                     Dest      : Node_Id;
                     B, K      : Node_Id;
                     Device    : Node_Id;
                  begin
                     if not AAU.Is_Empty (Dest_List) then
                        Dest := First_Node (Dest_List);

                        while Present (Dest) loop
                           --  Get the thread that contains Dest

                           Dest_Th := Parent_Component (Item (Dest));

                           --  If Dest_Th has not been handled in the
                           --  context of E yet, then create a case
                           --  statement alternative corresponding to
                           --  Dest_Th and mark it as handled.

                           if AAU.Is_Thread (Dest_Th)
                             and then not Is_Handled (Dest_Th)
                           then
                              --  Get the bus that connects the two ports

                              B := Extra_Item (Dest);

                              Device := Get_Device_Of_Process (B, E);

                              if No (B) then
                                 --  There is no bus involved in the
                                 --  connection, therefore it is a
                                 --  local communication: use the
                                 --  deliver routine.

                                 K :=
                                   Make_Subprogram_Call
                                     (Make_Defining_Identifier
                                        (SN (S_Deliver)),
                                      Make_List_Id
                                        (Make_Defining_Identifier
                                           (PN (P_Entity)),
                                         Make_Defining_Identifier
                                           (PN (P_Message))));

                                 --  Local delivery implies no
                                 --  potential error, we return
                                 --  Error_None.

                                 N :=
                                   Make_Qualified_Expression
                                     (RE (RE_Error_Kind),
                                      Make_Record_Aggregate
                                        (Make_List_Id (RE (RE_Error_None))));

                                 N := Make_Return_Statement (N);

                                 --  Create the case statement alternative

                                 N :=
                                   Make_Elsif_Statement
                                     (Make_Expression
                                        (Make_Defining_Identifier
                                           (PN (P_Entity)),
                                         Op_Equal,
                                         Extract_Enumerator (Dest_Th)),
                                      Make_List_Id (K, N));
                                 Append_Node_To_List (N, Alternatives);

                              elsif Device /= No_Node
                                and then AAU.Is_Device
                                  (Corresponding_Instance (Device))
                              then
                                 K :=
                                   Message_Comment
                                     ("User-provided transport mechanism, " &
                                      "device " &
                                      Get_Name_String
                                        (Name (Identifier (Device))));

                                 declare
                                    Profile : constant List_Id :=
                                      New_List (ADN.K_Parameter_Profile);
                                    A : Node_Id;
                                 begin
                                    --  Entity

                                    A :=
                                      Make_Parameter_Specification
                                        (Make_Defining_Identifier
                                           (PN (P_Node)),
                                         RE (RE_Node_Type),
                                         Mode_In);
                                    Append_Node_To_List (A, Profile);

                                    --  Message

                                    A :=
                                      Make_Parameter_Specification
                                        (Make_Defining_Identifier
                                           (PN (P_Message)),
                                         RE (RE_Stream_Element_Array),
                                         Mode_In);
                                    Append_Node_To_List (A, Profile);

                                    A :=
                                      Make_Parameter_Specification
                                        (Make_Defining_Identifier
                                           (PN (P_Size)),
                                         RE (RE_Stream_Element_Offset),
                                         Mode_In);
                                    Append_Node_To_List (A, Profile);
                                 end;

                                 N :=
                                   Make_Designator
                                     (Unit_Name
                                        (Get_Send_Function_Name
                                           (Corresponding_Instance (Device))));
                                 Add_With_Package (N);

                                 N :=
                                   Make_Designator
                                     (Local_Name
                                        (Get_Send_Function_Name
                                           (Corresponding_Instance (Device))),
                                      Unit_Name
                                        (Get_Send_Function_Name
                                           (Corresponding_Instance (Device))));

                                 N :=
                                   Make_Subprogram_Call
                                     (ADN.Defining_Identifier (N),
                                      Make_List_Id
                                        (Make_Indexed_Component
                                           (RE (RE_Entity_Table),
                                            Make_List_Id
                                              (Make_Defining_Identifier
                                                 (PN (P_Entity)))),
                                         Make_Defining_Identifier
                                           (PN (P_Message)),
                                         Make_Attribute_Designator
                                           (Prefix =>
                                              Make_Defining_Identifier
                                                (PN (P_Message)),
                                            Attribute => A_Length)));

                                 N := Make_Return_Statement (N);

                                 --  Create the case statement alternative

                                 N :=
                                   Make_Elsif_Statement
                                     (Make_Expression
                                        (Make_Defining_Identifier
                                           (PN (P_Entity)),
                                         Op_Equal,
                                         Extract_Enumerator (Dest_Th)),
                                      Make_List_Id (K, N));
                                 Append_Node_To_List (N, Alternatives);

                              else
                                 --  If the user did not specify any
                                 --  specific device drivers in the
                                 --  AADL model, use default transport
                                 --  mechanism provided by the
                                 --  runtime.

                                 pragma Assert
                                   (Get_Transport_API (B) /= Transport_None);

                                 K :=
                                   Message_Comment
                                     ("Default transport mechanism");

                                 N :=
                                   Make_Subprogram_Call
                                     (RE (RE_Send_3),
                                      Make_List_Id
                                        (Make_Indexed_Component
                                           (RE (RE_Entity_Table),
                                            Make_List_Id
                                              (Make_Defining_Identifier
                                                 (PN (P_Entity)))),
                                         Make_Defining_Identifier
                                           (PN (P_Message))));

                                 N := Make_Return_Statement (N);

                                 --  Create the case statement alternative

                                 N :=
                                   Make_Elsif_Statement
                                     (Make_Expression
                                        (Make_Defining_Identifier
                                           (PN (P_Entity)),
                                         Op_Equal,
                                         Extract_Enumerator (Dest_Th)),
                                      Make_List_Id (K, N));
                                 Append_Node_To_List (N, Alternatives);
                              end if;

                              Set_Handled (Dest_Th);

                           elsif AAU.Is_Device (Dest_Th)
                             and then not Is_Handled (Dest_Th)
                           then

                              N := Message_Comment ("Device");
                              Append_Node_To_List (N, Statements);

                              Set_Handled (Dest_Th);

                           elsif not Is_Handled (Dest_Th) then
                              raise Program_Error;
                           end if;

                           Dest := Next_Node (Dest);
                        end loop;
                     end if;
                  end;
               end if;

               F := Next_Node (F);
            end loop;

            --  Raise an error if other ports are targeted.

            if Length (Alternatives) /= 0 then
               N :=
                 Make_Used_Package (RU (RU_PolyORB_HI_Generated_Deployment));
               Append_Node_To_List (N, Declarations);

               declare
                  Elsif_Statements : constant List_Id :=
                    New_List (ADN.K_List_Id);
                  Else_Statements : constant List_Id :=
                    New_List (ADN.K_List_Id);

               begin
                  N :=
                    Make_Qualified_Expression
                      (RE (RE_Error_Kind),
                       Make_Record_Aggregate
                         (Make_List_Id (RE (RE_Error_Transport))));
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Else_Statements);

                  ADN.Set_First_Node
                    (Elsif_Statements,
                     ADN.Next_Node (ADN.First_Node (Alternatives)));

                  N :=
                    Make_If_Statement
                      (Condition =>
                         ADN.Condition (ADN.First_Node (Alternatives)),
                       Then_Statements =>
                         ADN.Then_Statements (ADN.First_Node (Alternatives)),
                       Elsif_Statements => Elsif_Statements,
                       Else_Statements  => Else_Statements);

                  Append_Node_To_List (N, Statements);
               end;

            else
               N :=
                 Make_Qualified_Expression
                   (RE (RE_Error_Kind),
                    Make_Record_Aggregate
                      (Make_List_Id (RE (RE_Error_Transport))));
               N := Make_Return_Statement (N);
               Append_Node_To_List (N, Statements);
            end if;
         end if;

         N := Make_Subprogram_Implementation (Spec, Declarations, Statements);
         return N;
      end Internal_Send_Body;

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
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         N : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Transport_Body;

         --  Generate a delivery body

         N := Deliver_Body (E);
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Generate a sending body if necessary

         N := Send_Body (E);
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Visit all the subcomponents of the process

         Visit_Subcomponents_Of (E);

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
      begin
         Push_Entity (Ada_Root);

         --  Visit all the subcomponents of the system

         Visit_Subcomponents_Of (E);

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         N : Node_Id;
      begin
         if Has_In_Ports (E) then
            --  Generate the body of the internal delivery routine

            N := Internal_Deliver_Body (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;

         if Has_Out_Ports (E) then
            --  Generate the body of the internal sending routine

            N := Internal_Send_Body (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;
      end Visit_Thread_Instance;

   end Package_Body;

end Ocarina.Backends.PO_HI_Ada.Transport;
