------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--            OCARINA.BACKENDS.PO_HI_RTSJ.TRANSPORT_HIGH_LEVEL              --
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
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.RTSJ_Values;
with Ocarina.Backends.PO_HI_RTSJ.Mapping;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;

package body Ocarina.Backends.PO_HI_RTSJ.Transport_High_Level is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.RTSJ_Values;
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

      procedure Make_Deliver_In_Ports (E : Node_Id);
      procedure Make_Send_Out_Ports (E : Node_Id);
      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);

      --  Global variables declaration
      Class_Methods : List_Id;
      Send_Cases    : List_Id;
      Deliver_Cases : List_Id;

      In_Ports  : Boolean;
      Out_Ports : Boolean;

      ---------------------------
      -- Make_Deliver_In_Ports --
      ---------------------------
      procedure Make_Deliver_In_Ports (E : Node_Id) is
         S                : constant Node_Id := Parent_Subcomponent (E);
         N                : Node_Id;
         F                : Node_Id;
         Spec             : Node_Id;
         Impl             : Node_Id;
         Param            : Node_Id;
         C                : Node_Id;
         Switch_Statement : Node_Id;
         Params           : constant List_Id := New_List (K_Parameter_List);
         Cases            : constant List_Id := New_List (K_Statement_List);
         Case_Statements  : constant List_Id := New_List (K_Statement_List);
      begin
         F := AIN.First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then

               N :=
                 Make_Pointed_Notation
                   (Make_Defining_Identifier (ON (O_Activity)),
                    Make_Call_Function
                      (Make_Defining_Identifier
                         (MN (M_Store_Received_Message)),
                       Make_List_Id
                         (Make_Pointed_Notation
                            (Make_Defining_Identifier (ON (O_Activity)),
                             Map_Task_Port_Identifier (F)),
                          Make_Defining_Identifier (VN (V_Msg)),
                          Make_Literal (New_Int_Value (1, -1, 10)))));
               RTU.Append_Node_To_List (N, Case_Statements);

               N := RE (RE_Break);
               RTU.Append_Node_To_List (N, Case_Statements);

               C :=
                 Make_Case_Statement
                   (Labels =>
                      Make_List_Id
                        (Make_Pointed_Notation
                           (Make_Defining_Identifier (ON (O_Deployment)),
                            Make_Defining_Identifier
                              (Map_RTSJ_Enumerator_Name (F)))),
                    Statements => Case_Statements);
               RTU.Append_Node_To_List (C, Cases);

               F := AIN.Next_Node (F);

            end if;
         end loop;

         --  portNumber parameter
         Param :=
           Make_Parameter_Specification
             (RE (RE_Port_Number),
              New_Node (K_Int));
         RTU.Append_Node_To_List (Param, Params);

         --  msg parameter
         Param :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (VN (V_Msg)),
              Make_Defining_Identifier (ON (O_Message)));
         RTU.Append_Node_To_List (Param, Params);

         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Private)),
              Return_Type         => New_Node (K_Void),
              Defining_Identifier => Map_Task_Deliver_Identifier (S),
              Parameters          => Params,
              Throws              =>
                Make_List_Id
                  (Make_Defining_Identifier (ON (O_Program_Exception))));

         --  default case of switch statement
         N :=
           Make_Case_Statement
             (Labels     => Make_List_Id (RE (RE_Default)),
              Statements =>
                Make_List_Id
                  (Make_Throw_Statement
                     (Defining_Identifier =>
                        Make_New_Statement
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (ON (O_Program_Exception))))));
         RTU.Append_Node_To_List (N, Cases);

         Switch_Statement :=
           Make_Switch_Statement
             (Expr            => RE (RE_Port_Number),
              Case_Statements => Cases);

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Make_List_Id (Switch_Statement));
         RTU.Append_Node_To_List (Impl, Class_Methods);
      end Make_Deliver_In_Ports;

      -------------------------
      -- Make_Send_Out_Ports --
      -------------------------
      procedure Make_Send_Out_Ports (E : Node_Id) is
         S                   : constant Node_Id := Parent_Subcomponent (E);
         Spec                : Node_Id;
         Impl                : Node_Id;
         Param               : Node_Id;
         N                   : Node_Id;
         C                   : Node_Id;
         F                   : Node_Id;
         Dest                : Node_Id;
         Switch_Statement    : Node_Id;
         Destinations_List   : List_Id;
         Labels_Destinations : constant List_Id := New_List (K_Statement_List);
         Case_Statements     : constant List_Id := New_List (K_Statement_List);
         Cases               : constant List_Id := New_List (K_Statement_List);
      begin
         F := AIN.First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance and then Is_Out (F) then

               --  Find all the IN ports corresponding to this OUT port
               --  construct the labels for the case statement
               Destinations_List := Get_Destination_Ports (F);
               if not AINU.Is_Empty (Destinations_List) then
                  C := AIN.First_Node (Destinations_List);
                  while Present (C) loop
                     Dest := Parent_Component (Item (C));
                     N    :=
                       Make_Pointed_Notation
                         (Make_Defining_Identifier (ON (O_Deployment)),
                          Make_Defining_Identifier
                            (Map_RTSJ_Enumerator_Name
                               (Parent_Subcomponent (Dest))));
                     RTU.Append_Node_To_List (N, Labels_Destinations);

                     C := AIN.Next_Node (C);
                  end loop;
               end if;

               N :=
                 Message_Comment
                   ("Rewind read/write position of the message payload to "
                    & "the beginning for the unmarshalling operations");
               RTU.Append_Node_To_List (N, Case_Statements);

               N :=
                 Make_Pointed_Notation
                   (Make_Defining_Identifier (VN (V_Msg)),
                    Make_Call_Function (RE (RE_Rewind)));
               RTU.Append_Node_To_List (N, Case_Statements);

               N :=
                 Make_Pointed_Notation
                   (RE (RE_This),
                    Make_Call_Function
                      (RE (RE_Deliver),
                       Make_List_Id (Make_Defining_Identifier (VN (V_Msg)))));
               RTU.Append_Node_To_List (N, Case_Statements);

               N := RE (RE_Break);
               RTU.Append_Node_To_List (N, Case_Statements);

               N :=
                 Make_Case_Statement
                   (Labels     => Labels_Destinations,
                    Statements => Case_Statements);
               RTU.Append_Node_To_List (N, Cases);

            end if;

            F := AIN.Next_Node (F);
         end loop;

         Param :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (VN (V_Msg)),
              Make_Defining_Identifier (ON (O_Message)));

         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Private)),
              Return_Type         => New_Node (K_Void),
              Defining_Identifier => Map_Task_Send_Identifier (S),
              Parameters          => Make_List_Id (Param),
              Throws              =>
                Make_List_Id
                  (Make_Defining_Identifier (ON (O_Program_Exception))));

         --  default case of switch statement
         N :=
           Make_Case_Statement
             (Labels     => Make_List_Id (RE (RE_Default)),
              Statements =>
                Make_List_Id
                  (Make_Throw_Statement
                     (Defining_Identifier =>
                        Make_New_Statement
                          (Defining_Identifier =>
                             Make_Defining_Identifier
                               (ON (O_Program_Exception))))));
         RTU.Append_Node_To_List (N, Cases);

         Switch_Statement :=
           Make_Switch_Statement
             (Expr =>
                Make_Pointed_Notation
                  (Make_Defining_Identifier (VN (V_Msg)),
                   RE (RE_Get_Destination_Entity)),
              Case_Statements => Cases);

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Make_List_Id (Switch_Statement));
         RTU.Append_Node_To_List (Impl, Class_Methods);
      end Make_Send_Out_Ports;

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
         --  Visit all the subcomponents of the system
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_System_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------
      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           RTN.Distributed_Application_Unit
             (RTN.Naming_Node (Backend_Node (Identifier (E))));
         P          : constant Node_Id := RTN.Entity (U);
         S          : Node_Id;
         N          : Node_Id;
         Spec       : Node_Id;
         Impl       : Node_Id;
         Main_Class : Node_Id;
         Param      : Node_Id;
         Params     : constant List_Id := New_List (K_Parameter_List);
      begin
         Push_Entity (P);
         Push_Entity (U);
         RTU.Set_Transport_High_Level_Source;

         --  Global variables initialization
         Class_Methods := New_List (K_Method_List);
         Send_Cases    := New_List (K_Statement_List);
         Deliver_Cases := New_List (K_Statement_List);

         In_Ports  := False;
         Out_Ports := False;

         Add_Import (RH (RH_Program_Exception));

         --  Visit all the subcomponents of the process
         if not AINU.Is_Empty (Subcomponents (E)) then
            S := AIN.First_Node (Subcomponents (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;

         --  Send method

         --  senderEntity parameter
         Param :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (VN (V_Sender_Entity)),
              New_Node (K_Int));
         RTU.Append_Node_To_List (Param, Params);

         --  destinationEntity parameter
         Param :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (VN (V_Destination_Entity)),
              New_Node (K_Int));
         RTU.Append_Node_To_List (Param, Params);

         --  msg parameter
         Param :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (VN (V_Msg)),
              Make_Defining_Identifier (ON (O_Message)));
         RTU.Append_Node_To_List (Param, Params);

         Spec :=
           Make_Function_Specification
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Return_Type         => New_Node (K_Void),
              Defining_Identifier => RE (RE_Send),
              Parameters          => Params,
              Throws              =>
                Make_List_Id
                  (Make_Defining_Identifier (ON (O_Program_Exception))));

         if Out_Ports then

            --  default case of switch statement
            N :=
              Make_Case_Statement
                (Labels     => Make_List_Id (RE (RE_Default)),
                 Statements =>
                   Make_List_Id
                     (Make_Throw_Statement
                        (Defining_Identifier =>
                           Make_New_Statement
                             (Defining_Identifier =>
                                Make_Defining_Identifier
                                  (ON (O_Program_Exception))))));
            RTU.Append_Node_To_List (N, Send_Cases);

            N :=
              Make_Switch_Statement
                (Expr => Make_Defining_Identifier (VN (V_Sender_Entity)),
                 Case_Statements => Send_Cases);

         else

            N :=
              Make_Throw_Statement
                (Defining_Identifier =>
                   Make_New_Statement
                     (Defining_Identifier =>
                        Make_Defining_Identifier (ON (O_Program_Exception))));
            RTU.Append_Node_To_List (N, Send_Cases);

         end if;

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Make_List_Id (N));
         RTU.Append_Node_To_List (Impl, Class_Methods);

         --  Deliver method
         Spec :=
           Make_Function_Specification
             (Visibility =>
                Make_List_Id (RE (RE_Public), RE (RE_Synchronized)),
              Return_Type         => New_Node (K_Void),
              Defining_Identifier => RE (RE_Deliver),
              Parameters          =>
                Make_List_Id
                  (Make_Parameter_Specification
                     (Defining_Identifier =>
                        Make_Defining_Identifier (VN (V_Msg)),
                      Parameter_Type =>
                        Make_Defining_Identifier (ON (O_Message)))),
              Throws =>
                Make_List_Id
                  (Make_Defining_Identifier (ON (O_Program_Exception))));

         if In_Ports then

            --  default case of switch statement
            N :=
              Make_Case_Statement
                (Labels     => Make_List_Id (RE (RE_Default)),
                 Statements =>
                   Make_List_Id
                     (Make_Throw_Statement
                        (Defining_Identifier =>
                           Make_New_Statement
                             (Defining_Identifier =>
                                Make_Defining_Identifier
                                  (ON (O_Program_Exception))))));
            RTU.Append_Node_To_List (N, Deliver_Cases);

            N :=
              Make_Switch_Statement
                (Expr =>
                   Make_Pointed_Notation
                     (Left_Member  => Make_Defining_Identifier (VN (V_Msg)),
                      Right_Member =>
                        Make_Call_Function
                          (Defining_Identifier =>
                             RE (RE_Get_Destination_Entity))),
                 Case_Statements => Deliver_Cases);

         else

            N :=
              Make_Throw_Statement
                (Defining_Identifier =>
                   Make_New_Statement
                     (Defining_Identifier =>
                        Make_Defining_Identifier (ON (O_Program_Exception))));
            RTU.Append_Node_To_List (N, Deliver_Cases);

         end if;

         Impl :=
           Make_Function_Implementation
             (Specification => Spec,
              Statements    => Make_List_Id (N));
         RTU.Append_Node_To_List (Impl, Class_Methods);

         --  Main class declaration
         Main_Class :=
           Make_Class_Statement
             (Visibility          => Make_List_Id (RE (RE_Public)),
              Defining_Identifier =>
                Make_Defining_Identifier (ON (O_Transport_High_Level_Impl)),
              Implements =>
                Make_List_Id
                  (Make_Defining_Identifier (ON (O_Transport_High_Level))),
              Methods => Class_Methods);
         RTU.Append_Node_To_List (Main_Class, RTN.Statements (Current_File));

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------
      procedure Visit_Thread_Instance (E : Node_Id) is
         N               : Node_Id;
         S               : constant Node_Id := Parent_Subcomponent (E);
         Case_Statements : constant List_Id := New_List (K_Statement_List);
      begin

         if Has_In_Ports (E) then
            Make_Deliver_In_Ports (E);

            N :=
              Make_Pointed_Notation
                (RE (RE_This),
                 Make_Call_Function
                   (Defining_Identifier => Map_Task_Deliver_Identifier (S),
                    Parameters          =>
                      Make_List_Id
                        (Make_Pointed_Notation
                           (Make_Defining_Identifier (ON (O_Marshallers)),
                            Make_Call_Function
                              (RE (RE_Unmarshall_Port),
                               Make_List_Id
                                 (Make_Defining_Identifier (VN (V_Msg))))),
                         Make_Defining_Identifier (VN (V_Msg)))));
            RTU.Append_Node_To_List (N, Case_Statements);

            N := RE (RE_Break);
            RTU.Append_Node_To_List (N, Case_Statements);

            N :=
              Make_Case_Statement
                (Labels =>
                   Make_List_Id
                     (Make_Pointed_Notation
                        (Make_Defining_Identifier (ON (O_Deployment)),
                         Make_Defining_Identifier
                           (Map_RTSJ_Enumerator_Name (S)))),
                 Statements => Case_Statements);
            RTU.Append_Node_To_List (N, Deliver_Cases);

            In_Ports := True;
         end if;

         if Has_Out_Ports (E) then
            Make_Send_Out_Ports (E);

            N :=
              Make_Pointed_Notation
                (Make_Defining_Identifier (VN (V_Msg)),
                 Make_Call_Function
                   (RE (RE_Set_Destination_Entity),
                    Make_List_Id
                      (Make_Defining_Identifier (VN (V_Destination_Entity)))));
            RTU.Append_Node_To_List (N, Case_Statements);

            N :=
              Make_Pointed_Notation
                (Make_Defining_Identifier (VN (V_Msg)),
                 Make_Call_Function
                   (RE (RE_Set_Sender_Entity),
                    Make_List_Id
                      (Make_Defining_Identifier (VN (V_Sender_Entity)))));
            RTU.Append_Node_To_List (N, Case_Statements);

            N :=
              Make_Pointed_Notation
                (RE (RE_This),
                 Make_Call_Function
                   (Map_Task_Send_Identifier (S),
                    Make_List_Id (Make_Defining_Identifier (VN (V_Msg)))));
            RTU.Append_Node_To_List (N, Case_Statements);

            N := RE (RE_Break);
            RTU.Append_Node_To_List (N, Case_Statements);

            N :=
              Make_Case_Statement
                (Labels =>
                   Make_List_Id
                     (Make_Pointed_Notation
                        (Make_Defining_Identifier (ON (O_Deployment)),
                         Make_Defining_Identifier
                           (Map_RTSJ_Enumerator_Name (S)))),
                 Statements => Case_Statements);
            RTU.Append_Node_To_List (N, Send_Cases);

            Out_Ports := True;
         end if;

      end Visit_Thread_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_RTSJ.Transport_High_Level;
