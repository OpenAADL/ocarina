------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ H I _ A D A . A C T I V I T Y   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2019 ESA & ISAE.      --
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
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Values;
with Ocarina.Backends.PO_HI_Ada.Mapping;
with Ocarina.Backends.PO_HI_Ada.Runtime;

package body Ocarina.Backends.PO_HI_Ada.Activity is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Ada_Values;
   use Ocarina.Backends.PO_HI_Ada.Mapping;
   use Ocarina.Backends.PO_HI_Ada.Runtime;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   function Send_Output_Spec (E : Node_Id) return Node_Id;
   function Put_Value_Spec (E : Node_Id) return Node_Id;
   function Receive_Input_Spec (E : Node_Id) return Node_Id;
   function Get_Value_Spec (E : Node_Id) return Node_Id;
   function Get_Value_Spec_2 (E : Node_Id) return Node_Id;
   function Get_Sender_Spec (E : Node_Id) return Node_Id;
   function Get_Count_Spec (E : Node_Id) return Node_Id;
   function Get_Time_Stamp_Spec (E : Node_Id) return Node_Id;
   function Next_Value_Spec (E : Node_Id) return Node_Id;
   function Store_Received_Message_Spec (E : Node_Id) return Node_Id;
   function Wait_For_Incoming_Events_Spec (E : Node_Id) return Node_Id;
   --  Runtime routines provided for each AADL thread

   function Runtime_Spec_Aspect_Definition return Node_Id;
   --  Build aspect definition for runtime services

   ------------------------------------
   -- Runtime_Spec_Aspect_Definition --
   ------------------------------------

   function Runtime_Spec_Aspect_Definition return Node_Id is
   begin
      if Add_SPARK2014_Annotations then
         return Make_Aspect_Specification
           (Make_List_Id
              (Make_Aspect (ASN (A_Volatile_Function)),
               Make_Aspect
                 (ASN (A_Global),
                  Make_Global_Specification
                    (Make_List_Id
                       (Make_Moded_Global_List
                          (Mode_In,
                           Make_Defining_Identifier
                             (PN (P_Elaborated_Variables))))))));
      else
         return No_Node;
      end if;
   end Runtime_Spec_Aspect_Definition;

   ----------------------
   -- Send_Output_Spec --
   ----------------------

   function Send_Output_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier =>
             Make_Defining_Identifier (SN (S_Send_Output)),
           Parameter_Profile =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type => RE (RE_Error_Kind),
           Aspect_Specification => Runtime_Spec_Aspect_Definition);

      return N;
   end Send_Output_Spec;

   --------------------
   -- Put_Value_Spec --
   --------------------

   function Put_Value_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Put_Value)),
           Parameter_Profile   =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Thread_Interface)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Interface_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type => No_Node);

      return N;
   end Put_Value_Spec;

   ------------------------
   -- Receive_Input_Spec --
   ------------------------

   function Receive_Input_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier =>
             Make_Defining_Identifier (SN (S_Receive_Input)),
           Parameter_Profile =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type => No_Node);

      return N;
   end Receive_Input_Spec;

   --------------------
   -- Get_Value_Spec --
   --------------------

   function Get_Value_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
      Aspect_Node : Node_Id := No_Node;
   begin
      if Add_SPARK2014_Annotations then
         Aspect_Node := Make_Aspect_Specification
           (Make_List_Id
              (Make_Aspect
                 (ASN (A_Global),
                  Make_Global_Specification
                    (Make_List_Id
                       (Make_Moded_Global_List
                          (Mode_In,
                           Make_Defining_Identifier
                             (PN (P_Elaborated_Variables))))))));

      end if;

      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Get_Value)),
           Parameter_Profile   =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In),
                   Make_Parameter_Specification
                     (Defining_Identifier =>
                        Make_Defining_Identifier (PN (P_Result)),
                      Subtype_Mark =>
                        Make_Defining_Identifier (Map_Port_Interface_Name (E)),
                      Parameter_Mode => Mode_Inout)
               ),
           Aspect_Specification => Aspect_Node);

      return N;
   end Get_Value_Spec;

   function Get_Value_Spec_2 (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Get_Value)),
           Parameter_Profile   =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type =>
             Make_Defining_Identifier (Map_Port_Interface_Name (E)),
           Aspect_Specification => Runtime_Spec_Aspect_Definition);
      return N;
   end Get_Value_Spec_2;

   ---------------------
   -- Get_Sender_Spec --
   ---------------------

   function Get_Sender_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Get_Sender)),
           Parameter_Profile   =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type => RE (RE_Entity_Type),
           Aspect_Specification => Runtime_Spec_Aspect_Definition);

      return N;
   end Get_Sender_Spec;

   --------------------
   -- Get_Count_Spec --
   --------------------

   function Get_Count_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Get_Count)),
           Parameter_Profile   =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type => RE (RE_Integer),
           Aspect_Specification => Runtime_Spec_Aspect_Definition);

      return N;
   end Get_Count_Spec;

   -------------------------
   -- Get_Time_Stamp_Spec --
   -------------------------

   function Get_Time_Stamp_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier =>
             Make_Defining_Identifier (SN (S_Get_Time_Stamp)),
           Parameter_Profile =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type => RE (RE_Time),
           Aspect_Specification => Runtime_Spec_Aspect_Definition);

      return N;
   end Get_Time_Stamp_Spec;

   ---------------------
   -- Next_Value_Spec --
   ---------------------

   function Next_Value_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier => Make_Defining_Identifier (SN (S_Next_Value)),
           Parameter_Profile   =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_In)),
           Return_Type => No_Node);

      return N;
   end Next_Value_Spec;

   ---------------------------------
   -- Store_Received_Message_Spec --
   ---------------------------------

   function Store_Received_Message_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier =>
             Make_Defining_Identifier (SN (S_Store_Received_Message)),
           Parameter_Profile =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Thread_Interface)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Interface_Name (E)),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_From)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Time_Stamp)),
                   Subtype_Mark   => RE (RE_Time),
                   Parameter_Mode => Mode_In,
                   Expression     => RE (RE_Clock))),
           Return_Type => No_Node);

      return N;
   end Store_Received_Message_Spec;

   -----------------------------------
   -- Wait_For_Incoming_Events_Spec --
   -----------------------------------

   function Wait_For_Incoming_Events_Spec (E : Node_Id) return Node_Id is
      N : Node_Id;
   begin
      N :=
        Make_Subprogram_Specification
          (Defining_Identifier =>
             Make_Defining_Identifier (SN (S_Wait_For_Incoming_Events)),
           Parameter_Profile =>
             Make_List_Id
               (Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Entity)),
                   Subtype_Mark   => RE (RE_Entity_Type),
                   Parameter_Mode => Mode_In),
                Make_Parameter_Specification
                  (Defining_Identifier =>
                     Make_Defining_Identifier (PN (P_Port)),
                   Subtype_Mark =>
                     Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
                   Parameter_Mode => Mode_Out)),
           Return_Type => No_Node);
      return N;
   end Wait_For_Incoming_Events_Spec;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      procedure Runtime_Routine_Specs (E : Node_Id);
      --  Creates the specs of all the routines provided by the runtime
      --  to the user-code to manipulate thread interface.

      function Make_Mode_Updater_Spec (E : Node_Id) return Node_Id;
      --  Create the procedure which will update the current mode

      function Make_Modes_Enumeration (E : Node_Id) return Node_Id;
      --  Create the mode enumeration

      ---------------------------
      -- Runtime_Routine_Specs --
      ---------------------------

      procedure Runtime_Routine_Specs (E : Node_Id) is
         N : Node_Id;
      begin
         if Has_Out_Ports (E) then
            --  The following functions are made visible iff the
            --  thread has *out* ports

            --  Send_Output

            N := Send_Output_Spec (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Put_Value

            N := Put_Value_Spec (E);
            Bind_AADL_To_Put_Value (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;

         if Has_In_Ports (E) then
            --  The following functions are made visible iff the
            --  thread has *in* ports

            --  Receive_Input

            N := Receive_Input_Spec (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Get_Value

            N := Get_Value_Spec (E);
            Bind_AADL_To_Get_Value (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Get_Value_Spec_2 (E);
            Bind_AADL_To_Get_Value (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Get_Sender

            N := Get_Sender_Spec (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Get_Count

            N := Get_Count_Spec (E);
            Bind_AADL_To_Get_Count (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Get_Time_Stamp

            N := Get_Time_Stamp_Spec (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Next_Value

            N := Next_Value_Spec (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            N := Store_Received_Message_Spec (E);
            Bind_AADL_To_Store_Received_Message (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Wait_For_Incoming_Events

            N := Wait_For_Incoming_Events_Spec (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;
      end Runtime_Routine_Specs;

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
         P                   : constant Node_Id              := ADN.Entity (U);
         S                   : Node_Id;
         N                   : Node_Id;
         Scheduling_Protocol : Supported_Scheduling_Protocol :=
           Get_Scheduling_Protocol (Get_Bound_Processor (E));
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));

         function Package_Spec_Aspect_Definition return Node_Id is
         begin
            if Add_SPARK2014_Annotations then
               return Make_Aspect_Specification
                 (Make_List_Id
                    (Make_Aspect (ASN (A_Initializes),
                                  Make_Initialization_Spec
                                    (Make_List_Id
                                       (Make_Defining_Identifier
                                          (PN (P_Elaborated_Variables))))),
                     Make_Aspect
                       (ASN (A_Abstract_State),
                        Make_Abstract_State_List
                          (Make_List_Id
                             (Make_State_Name_With_Option
                                (Make_Defining_Identifier
                                   (PN (P_Elaborated_Variables)),
                                 Synchronous => True,
                                 External => True))))));
            else
               return No_Node;
            end if;
         end Package_Spec_Aspect_Definition;

      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Activity_Spec;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         if Scheduling_Protocol = Unknown_Scheduler then
            Display_Located_Error
              (Loc (Get_Bound_Processor (E)),
               "Undefined scheduling protocol, " &
               "will use FIFO_WITHIN_PRIORITIES",
               Fatal   => False,
               Warning => True);

            Scheduling_Protocol := POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL;
         elsif Scheduling_Protocol /=
           POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL
           and then Scheduling_Protocol /= ROUND_ROBIN_PROTOCOL
         then
            Display_Located_Error
              (Loc (Parent_Subcomponent (E)),
               "Incompatible scheduling protocol, " &
               "PolyORB-HI/Ada runtime requires " &
               "POSIX_1003_HIGHEST_PRIORITY_FIRST_PROTOCOL or" &
               " ROUND_ROBIN_PROTOCOL",
               Fatal => True);

            --  XXX In case of Round robin, we should also check that
            --  the scheduler is set to non-preemptive mode.
         end if;

         ADN.Set_Aspect_Specification (Current_Package,
                                       Package_Spec_Aspect_Definition);

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  If the process has a data subcomponent, then map a
               --  shared variable.

               if AINU.Is_Data (Corresponding_Instance (S)) then
                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier => Map_Ada_Defining_Identifier (S),
                       Object_Definition   =>
                         Map_Ada_Data_Type_Designator
                           (Corresponding_Instance (S)));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

                  --  Link the variable and the object

                  Bind_AADL_To_Object (Identifier (S), N);

                  if Get_Concurrency_Protocol (Corresponding_Instance (S)) =
                    Priority_Ceiling
                  then
                     --  XXX For now, we disable SPARK_Mode due to the
                     --  inability of SPARK GPL2015 to support
                     --  variable that denotes protected objects.

                     N :=
                       Make_Pragma_Statement
                         (Pragma_SPARK_Mode,
                          Make_List_Id (RE (RE_Off)));

                     Append_Node_To_List
                       (N,
                        ADN.Package_Headers (Current_Package));
                  end if;

               else
                  --  Visit the component instance corresponding to the
                  --  subcomponent S.

                  Visit (Corresponding_Instance (S));
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            S := First_Node (Subcomponents (The_System));
            while Present (S) loop
               if AAU.Is_Device (Corresponding_Instance (S))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (S)) =
                   Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : constant Node_Id := Get_Implementation (E);
         S              : Node_Id;
      begin
         if Implementation /= No_Node then
            if not AAU.Is_Empty (AAN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  Visit_Component_Instance (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end if;
      end Visit_Device_Instance;

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
         P : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
         O : Node_Id;

      begin
         if Has_Ports (E) then
            --  Create the spec of the subprograms to interact with
            --  thread ports.

            case P is
               when Thread_Periodic =>
                  N :=
                    Message_Comment
                      ("Periodic task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Sporadic =>
                  N :=
                    Message_Comment
                      ("Sporadic task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Hybrid =>
                  N :=
                    Message_Comment
                      ("Hybrid task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Aperiodic =>
                  N :=
                    Message_Comment
                      ("Aperiodic task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_Background =>
                  N :=
                    Message_Comment
                      ("Background task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when Thread_ISR =>
                  N :=
                    Message_Comment
                      ("ISR task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               when others =>
                  Display_Located_Error
                    (AIN.Loc (E),
                     "Unsupported dispatch protocol",
                     Fatal => True);
            end case;

            --  The data types and the interrogation routines
            --  generated from a thread are not instance specific. We
            --  generate them once per thread component. This allows
            --  us to support having multiple instances of the same
            --  thread in the model. Multiple thread instances of the
            --  same component share the same generated entities. This
            --  avoids having instance-specific code inside compute
            --  entrypoints.

            if No
                (Get_Handling
                   (Corresponding_Declaration (E),
                    By_Node,
                    H_Ada_Activity_Interr_Spec))
            then
               Set_Handling
                 (Corresponding_Declaration (E),
                  By_Node,
                  H_Ada_Activity_Interr_Spec,
                  E);

               N :=
                 Message_Comment
                   ("BEGIN: Entities used by all instances of component " &
                    Get_Name_String (Display_Name (Identifier (E))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Declare the enumeration type gathering all the
               --  thread ports.

               N := Map_Port_Enumeration (E);
               Bind_AADL_To_Port_Enumeration (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Declare the thread interface discriminated record type

               N := Map_Port_Interface (E);
               Bind_AADL_To_Port_Interface (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Declare the routines that allow user code to
               --  manipulate the thread.

               Runtime_Routine_Specs (E);

               N :=
                 Message_Comment
                   ("END: Entities used by all instances of component " &
                    Get_Name_String (Display_Name (Identifier (E))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            else
               --  We bind the entities generated for the found
               --  instance to be able to reference them later.

               declare
                  Found : constant Node_Id :=
                    Get_Handling
                      (Corresponding_Declaration (E),
                       By_Node,
                       H_Ada_Activity_Interr_Spec);
                  BE : constant Node_Id := Backend_Node (Identifier (Found));
               begin
                  Bind_AADL_To_Port_Enumeration
                    (Identifier (E),
                     ADN.Port_Enumeration_Node (BE));
                  Bind_AADL_To_Port_Interface
                    (Identifier (E),
                     ADN.Port_Interface_Node (BE));
                  Bind_AADL_To_Put_Value
                    (Identifier (E),
                     ADN.Put_Value_Node (BE));
                  Bind_AADL_To_Get_Value
                    (Identifier (E),
                     ADN.Get_Value_Node (BE));
                  Bind_AADL_To_Get_Count
                    (Identifier (E),
                     ADN.Get_Count_Node (BE));
                  Bind_AADL_To_Store_Received_Message
                    (Identifier (E),
                     ADN.Store_Received_Message_Node (BE));
               end;
            end if;
         end if;

         if Has_Modes (E) then
            --  If the thread has operational modes, then generate the
            --  enumeration type corresponding to the thread mode list
            --  and the procedure allowing to update the current mode.

            N := Make_Modes_Enumeration (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            if Is_Fusioned (E) then
               N := Make_Mode_Updater_Spec (E);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;
         end if;

         --  Visit thread local objects

         if not AINU.Is_Empty (Subcomponents (E)) then
            O := First_Node (Subcomponents (E));

            while Present (O) loop
               if AINU.Is_Data (Corresponding_Instance (O)) then
                  N :=
                    Make_Object_Declaration
                      (Defining_Identifier => Map_Ada_Defining_Identifier (O),
                       Object_Definition   =>
                         Map_Ada_Data_Type_Designator
                           (Corresponding_Instance (O)));
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

                  --  Link the variable and the object

                  Bind_AADL_To_Object (Identifier (O), N);
               end if;

               O := Next_Node (O);
            end loop;
         end if;
      end Visit_Thread_Instance;

      ----------------------------
      -- Make_Modes_Enumeration --
      ----------------------------

      function Make_Modes_Enumeration (E : Node_Id) return Node_Id is
         Enum_List : constant List_Id := New_List (ADN.K_Enumeration_Literals);
         M         : Node_Id;
         N         : Node_Id;
      begin
         M := First_Node (Modes (E));

         while Present (M) loop
            N := Map_Ada_Defining_Identifier (M);
            Append_Node_To_List (N, Enum_List);

            M := Next_Node (M);
         end loop;

         N :=
           Make_Full_Type_Declaration
             (Defining_Identifier =>
                Make_Defining_Identifier (Map_Modes_Enumeration_Name (E)),
              Type_Definition => Make_Enumeration_Type_Definition (Enum_List));

         return N;
      end Make_Modes_Enumeration;

      ----------------------------
      -- Make_Mode_Updater_Spec --
      ----------------------------

      function Make_Mode_Updater_Spec (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (SN (S_Change_Mode)),
              Parameter_Profile =>
                Make_List_Id
                  (Make_Parameter_Specification
                     (Defining_Identifier =>
                        Make_Defining_Identifier (PN (P_Mode)),
                      Subtype_Mark =>
                        Make_Defining_Identifier
                          (Map_Modes_Enumeration_Name (E)),
                      Parameter_Mode => Mode_In)),
              Return_Type => No_Node);
         Set_Backend_Node (Identifier (First_Node (Modes (E))), N);

         return N;
      end Make_Mode_Updater_Spec;

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
      procedure Visit_Device_Instance (E : Node_Id);
      procedure Visit_Subcomponents_Of is new Visit_Subcomponents_Of_G (Visit);

      procedure Runtime_Routine_Bodies (E : Node_Id);
      --  Creates the implementations of all the routines provided by
      --  the runtime to the user-code to manipulate thread interface.

      function Make_Current_Mode_Declaration (E : Node_Id) return Node_Id;
      --  Create, if necessary, the current mode variable declaration for
      --  thread E.

      function Make_Mode_Updater_body (E : Node_Id) return Node_Id;
      --  Create the procedure which will update the current mode

      Has_Hybrid_Threads       : Boolean            := False;
      Hybrid_Thread_Elements   : List_Id            := No_List;
      Last_Hybrid_Thread_Index : Unsigned_Long_Long := 0;

      Current_Mode_Identifier : Node_Id;

      --  The runtime routines are generated per thread component and
      --  not per thread instance. For each thread instance, we must
      --  complete the case alternative specific to it in each one of
      --  the routines. To perform this, we attache to each thread
      --  component a set of List_Id's which represent the case
      --  statement of the corresponding routines. The entities below
      --  allow to Get/Set these list for each thread component.

      type Runtime_Routine is
        (RR_Send_Output,
         RR_Put_Value,
         RR_Receive_Input,
         RR_Get_Value,
         RR_Get_Sender,
         RR_Get_Count,
         RR_Get_Time_Stamp,
         RR_Next_Value,
         RR_Store_Received_Message,
         RR_Wait_For_Incoming_Events);

      function Get_List_Internal_Name
        (Thread : Node_Id;
         RR     : Runtime_Routine) return Name_Id;
      --  Code factorization between the two subprograms below

      function Get_List
        (Thread : Node_Id;
         RR     : Runtime_Routine) return List_Id;
      --  Return the List_Id corresponding to the runtime routine 'RR'
      --  and associated to 'Thread'.

      procedure Set_List (Thread : Node_Id; RR : Runtime_Routine; L : List_Id);
      --  Set a new value (L) to the list corresponding to the runtime
      --  routine 'RR' and associated to 'Thread'.

      Interrogation_Routine_List : List_Id;
      --  This list will hold all the bodies declarations of the
      --  interrogation routines. We do this to ensure all the bodies
      --  are appended after all entities generated for threads since
      --  they need visibility on these entities.

      Package_Body_Refined_States : List_Id := No_List;

      function Runtime_Body_Aspect_Definition
        (E : Node_Id; RR : Runtime_Routine) return Node_Id;
      --  Build aspect definition for runtime services

      ------------------------------------
      -- Runtime_Body_Aspect_Definition --
      ------------------------------------

      function Runtime_Body_Aspect_Definition
        (E : Node_Id; RR : Runtime_Routine) return Node_Id is
      begin
         if Add_SPARK2014_Annotations and then
           (RR = RR_Send_Output or else
              RR = RR_Get_Value or else
              RR = RR_Get_Sender or else
              RR = RR_Get_Count or else
              RR = Rr_Get_Time_Stamp)
         then
            return Make_Aspect_Specification
              (Make_List_Id
                 (Make_Aspect
                    (ASN (A_Refined_Global),
                     Make_Global_Specification
                       (Make_List_Id
                          (Make_Moded_Global_List
                             (Mode_In,
                              Map_Refined_Global_Name (E)))))));
         else
            return No_Node;
         end if;
      end Runtime_Body_Aspect_Definition;

      ----------------------------
      -- Runtime_Routine_Bodies --
      ----------------------------

      procedure Runtime_Routine_Bodies (E : Node_Id) is
         N           : Node_Id;
         Not_Handled : constant Boolean :=
           No
             (Get_Handling
                (Corresponding_Declaration (E),
                 By_Node,
                 H_Ada_Activity_Interr_Body));
      begin
         --  The data types and the interrogation routines generated
         --  from a thread are not instance specific. We generate them
         --  once per thread component. This allows us to support
         --  having multiple instances of the same thread in the
         --  model. Multiple thread instances of the same component
         --  share the same generated entities. This avoids having
         --  instance-specific code inside compute entrypoints.

         Set_Handling
           (Corresponding_Declaration (E),
            By_Node,
            H_Ada_Activity_Interr_Body,
            E);

         if Not_Handled then
            N :=
              Message_Comment
                ("BEGIN: Data types used by all instances of component " &
                 Get_Name_String (Display_Name (Identifier (E))));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  Declare the <Thread>_Integer_Array type

            N :=
              Make_Full_Type_Declaration
                (Make_Defining_Identifier (Map_Integer_Array_Name (E)),
                 Make_Array_Type_Definition
                   (Make_List_Id
                      (Make_Defining_Identifier
                         (Map_Port_Enumeration_Name (E))),
                    RE (RE_Integer)));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  Declare the <Thread>_Kind_Array type

            N :=
              Make_Full_Type_Declaration
                (Make_Defining_Identifier (Map_Kind_Array_Name (E)),
                 Make_Array_Type_Definition
                   (Make_List_Id
                      (Make_Defining_Identifier
                         (Map_Port_Enumeration_Name (E))),
                    RE (RE_Port_Kind)));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  Declare the <Thread>_Image_Array type

            N :=
              Make_Full_Type_Declaration
                (Make_Defining_Identifier (Map_Image_Array_Name (E)),
                 Make_Array_Type_Definition
                   (Range_Constraints =>
                      Make_List_Id
                        (Make_Defining_Identifier
                           (Map_Port_Enumeration_Name (E))),
                    Component_Definition =>
                      Make_Indexed_Component
                        (RE (RE_String),
                         Make_List_Id
                           (Make_Range_Constraint
                              (Make_Literal (New_Integer_Value (1, 0, 10)),
                               RE (RE_Max_Port_Image_Size))))));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  Declare the <Thread>_Address_Array type

            N :=
              Make_Full_Type_Declaration
                (Make_Defining_Identifier (Map_Address_Array_Name (E)),
                 Make_Array_Type_Definition
                   (Make_List_Id
                      (Make_Defining_Identifier
                         (Map_Port_Enumeration_Name (E))),
                    RE (RE_Address)));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  Declare the <Thread>_Overflow_Protocol_Array type

            N :=
              Make_Full_Type_Declaration
                (Make_Defining_Identifier
                   (Map_Overflow_Protocol_Array_Name (E)),
                 Make_Array_Type_Definition
                   (Make_List_Id
                      (Make_Defining_Identifier
                         (Map_Port_Enumeration_Name (E))),
                    RE (RE_Overflow_Handling_Protocol)));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Message_Comment
                ("END: Data types used by all instances of component " &
                 Get_Name_String (Display_Name (Identifier (E))));
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;

         --  Declare the FIFO size related entities

         declare
            F                    : Node_Id;
            Port_Kinds_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
            Overflow_Protocols_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
            Port_Images_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
            FIFO_Sizes_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
            Offset_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
            Urgencies_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
            Total_FIFO_Size   : Unsigned_Long_Long := 0;
            Port_Kind         : RE_Id;
            Overflow_Protocol : RE_Id;
            Queue_Size        : Long_Long;
            Queue_Size_V      : Value_Id;
            Offset_V          : Value_Id;
         begin
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_Event (F) then
                     if not AIN.Is_Data (F) then
                        Port_Kind := RE_In_Event_Port;
                     else
                        Port_Kind := RE_In_Event_Data_Port;
                     end if;
                  else
                     Port_Kind := RE_In_Data_Port;
                  end if;

                  if Is_In (F) then
                     if Is_Out (F) then
                        Port_Kind := RE_Id'Val (RE_Id'Pos (Port_Kind) + 3);
                     end if;
                  else
                     Port_Kind := RE_Id'Val (RE_Id'Pos (Port_Kind) + 6);
                  end if;

                  N :=
                    Make_Element_Association
                      (Map_Ada_Defining_Identifier (F),
                       RE (Port_Kind));
                  Append_Node_To_List (N, Port_Kinds_Aggregate);

                  N :=
                    Make_Element_Association
                      (Map_Ada_Defining_Identifier (F),
                       (Make_Indexed_Component
                          (RE (RE_Port_Image),
                           Make_List_Id (Extract_Enumerator (F)))));
                  Append_Node_To_List (N, Port_Images_Aggregate);

                  case Get_Overflow_Handling_Protocol (F) is
                     when Overflow_Handling_Protocol_DropOldest |
                       Overflow_Handling_Protocol_None          =>
                        Overflow_Protocol := RE_DropOldest;
                     when Overflow_Handling_Protocol_DropNewest =>
                        Overflow_Protocol := RE_DropNewest;
                     when Overflow_Handling_Protocol_Error =>
                        Overflow_Protocol := RE_Error;
                  end case;
                  N :=
                    Make_Element_Association
                      (Map_Ada_Defining_Identifier (F),
                       RE (Overflow_Protocol));
                  Append_Node_To_List (N, Overflow_Protocols_Aggregate);

                  --  Element association for the Urgencies array

                  N :=
                    Make_Element_Association
                      (Map_Ada_Defining_Identifier (F),
                       Make_Literal
                         (New_Integer_Value (Get_Port_Urgency (F), 1, 10)));
                  Append_Node_To_List (N, Urgencies_Aggregate);

                  --  Convention for queue sizes:

                  --  IN [OUT] EVENT [DADA] ports: user-specified or
                  --  else default value.

                  --  IN [OUT] DATA ports: 1 or 2 depending on the
                  --  connection natue.

                  --  OUT [EVENT] [DATA] ports: -2

                  Queue_Size := 0;

                  if Is_Out (F) then
                     Queue_Size_V := New_Integer_Value (1, -1, 10);
                     Offset_V     := New_Integer_Value (0, 1, 10);
                  elsif AIN.Is_Data (F) and then not Is_Event (F) then

                     --  Get a connection whose source is F

                     if Is_Delayed (F) then
                        --  This data port belongs to a delayed
                        --  connection: tell the middlware routines
                        --  about it

                        Queue_Size := 2;
                     else
                        --  Immediate connection

                        Queue_Size := 1;
                     end if;

                     Queue_Size_V :=
                       New_Integer_Value
                         (Unsigned_Long_Long (Queue_Size),
                          1,
                          10);
                     Offset_V :=
                       New_Integer_Value (Total_FIFO_Size + 1, 1, 10);
                  else
                     Queue_Size := Get_Queue_Size (F);

                     if Queue_Size = -1 then
                        Queue_Size := Default_Queue_Size;
                        --  For the calculation of the total queue
                        --  size.

                        --  Allocate a default size

                        Queue_Size_V :=
                          New_Integer_Value (Default_Queue_Size, 1, 10);
                     elsif Queue_Size = 0 then
                        --  0 length queues are not supported

                        Display_Located_Error
                          (Loc (F),
                           "Zero length port queues are not supported yet",
                           Fatal => True);
                     else
                        Queue_Size_V :=
                          New_Integer_Value
                            (Unsigned_Long_Long (Queue_Size),
                             1,
                             10);
                     end if;

                     --  The offset value is equal to the current
                     --  value of Total_FIFO_Size + 1.

                     Offset_V :=
                       New_Integer_Value (Total_FIFO_Size + 1, 1, 10);
                  end if;

                  --  Element association for the FIFO sizes array

                  N :=
                    Make_Element_Association
                      (Map_Ada_Defining_Identifier (F),
                       Make_Literal (Queue_Size_V));
                  Append_Node_To_List (N, FIFO_Sizes_Aggregate);

                  --  Element association for the offset array

                  N :=
                    Make_Element_Association
                      (Map_Ada_Defining_Identifier (F),
                       Make_Literal (Offset_V));
                  Append_Node_To_List (N, Offset_Aggregate);

                  --  Update the global FIFO size in case Queue_Size is
                  --  positive.

                  if Queue_Size > 0 then
                     Total_FIFO_Size :=
                       Total_FIFO_Size + Unsigned_Long_Long (Queue_Size);
                  end if;
               end if;

               F := Next_Node (F);
            end loop;

            --  Declare the entities

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Port_Kinds_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Kind_Array_Name (E)),
                 Constant_Present => True,
                 Expression => Make_Array_Aggregate (Port_Kinds_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Port_Images_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Image_Array_Name (E)),
                 Constant_Present => True,
                 Expression => Make_Array_Aggregate (Port_Images_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_FIFO_Sizes_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Integer_Array_Name (E)),
                 Constant_Present => True,
                 Expression => Make_Array_Aggregate (FIFO_Sizes_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Offsets_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Integer_Array_Name (E)),
                 Constant_Present => True,
                 Expression       => Make_Array_Aggregate (Offset_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Overflow_Protocols_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier
                     (Map_Overflow_Protocol_Array_Name (E)),
                 Constant_Present => True,
                 Expression       =>
                   Make_Array_Aggregate (Overflow_Protocols_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Urgencies_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Integer_Array_Name (E)),
                 Constant_Present => True,
                 Expression => Make_Array_Aggregate (Urgencies_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Total_Size_Name (E)),
                 Object_Definition => RE (RE_Integer),
                 Constant_Present  => True,
                 Expression        =>
                   Make_Literal (New_Integer_Value (Total_FIFO_Size, 1, 10)));
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end;

         --  For each OUT port, we create an array of its
         --  destinations, we declare the number of its destinations
         --  and create an element association in the global
         --  destinations array.

         declare
            F                       : Node_Id;
            N_Destination_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
            Destination_Aggregate : constant List_Id :=
              New_List (ADN.K_Element_List);
         begin
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  --  For OUT ports, we generate an array to indicate
                  --  their destinations and we put relevant element
                  --  associations in the N_Destinations and the
                  --  Destinnations arrays. For IN ports, we generate
                  --  nothing and we put dummy element association.

                  if Is_Out (F) then
                     declare
                        D                  : Node_Id;
                        Port_Dst_Aggregate : constant List_Id :=
                          New_List (ADN.K_Element_List);
                        Destinations : constant List_Id :=
                          Get_Destination_Ports (F);
                        Dst_Index : Unsigned_Long_Long := 1;
                     begin
                        if AINU.Is_Empty (Destinations) then
                           Display_Located_Error
                             (Loc (F),
                              "This OUT port is not connected to any" &
                              " destination",
                              Fatal => True);
                        end if;

                        D := First_Node (Destinations);

                        while Present (D) loop
                           if not AINU.Is_Device
                               (Parent_Component (Item (D)))
                           then
                              N :=
                                Make_Element_Association
                                  (Make_Literal
                                     (New_Integer_Value (Dst_Index, 1, 10)),
                                   Extract_Enumerator (Item (D)));
                              Append_Node_To_List (N, Port_Dst_Aggregate);
                              Dst_Index := Dst_Index + 1;
                           end if;
                           D := Next_Node (D);
                        end loop;

                        --  Declare the port specific destination
                        --  array.

                        if not Is_Empty (Port_Dst_Aggregate) then
                           N :=
                             Make_Object_Declaration
                               (Defining_Identifier =>
                                  Make_Defining_Identifier
                                    (Map_Destination_Name (F)),
                                Object_Definition =>
                                  Make_Array_Type_Definition
                                    (Range_Constraints =>
                                       Make_List_Id
                                         (Make_Range_Constraint
                                            (No_Node,
                                             No_Node,
                                             RE (RE_Positive))),
                                     Component_Definition =>
                                       RE (RE_Port_Type_1)),
                                Constant_Present => True,
                                Expression       =>
                                  Make_Array_Aggregate (Port_Dst_Aggregate));
                           Append_Node_To_List
                             (N,
                              ADN.Statements (Current_Package));

                           N :=
                             Make_Element_Association
                               (Map_Ada_Defining_Identifier (F),
                                Make_Literal
                                  (New_Integer_Value
                                     (Unsigned_Long_Long
                                        (AINU.Length (Destinations)),
                                      1,
                                      10)));
                           Append_Node_To_List (N, N_Destination_Aggregate);

                           N :=
                             Make_Element_Association
                               (Map_Ada_Defining_Identifier (F),
                                Make_Attribute_Designator
                                  (Make_Designator (Map_Destination_Name (F)),
                                   A_Address));
                           Append_Node_To_List (N, Destination_Aggregate);
                        else
                           N :=
                             Make_Element_Association
                               (Map_Ada_Defining_Identifier (F),
                                Make_Literal (New_Integer_Value (0, 1, 10)));
                           Append_Node_To_List (N, N_Destination_Aggregate);

                           N :=
                             Make_Element_Association
                               (Map_Ada_Defining_Identifier (F),
                                RE (RE_Null_Address));
                           Append_Node_To_List (N, Destination_Aggregate);
                        end if;

                        --  Update the element associations of the
                        --  N_Destinations and the Destinations
                        --  arrays.
                     end;
                  else
                     --  Dummy element associations

                     N :=
                       Make_Element_Association
                         (Map_Ada_Defining_Identifier (F),
                          Make_Literal (New_Integer_Value (0, 1, 10)));
                     Append_Node_To_List (N, N_Destination_Aggregate);

                     N :=
                       Make_Element_Association
                         (Map_Ada_Defining_Identifier (F),
                          RE (RE_Null_Address));
                     Append_Node_To_List (N, Destination_Aggregate);
                  end if;

               end if;

               F := Next_Node (F);
            end loop;

            --  Declare the N_Destinations and the Destinations
            --  arrays.

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_N_Destination_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Integer_Array_Name (E)),
                 Constant_Present => True,
                 Expression => Make_Array_Aggregate (N_Destination_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            N :=
              Make_Object_Declaration
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Destination_Name (E)),
                 Object_Definition =>
                   Make_Defining_Identifier (Map_Address_Array_Name (E)),
                 Constant_Present => True,
                 Expression => Make_Array_Aggregate (Destination_Aggregate));
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end;

         --  Instantiate the PolyORB_HI.Interrogators generic

         declare
            Inst_Profile : constant List_Id :=
              New_List (ADN.K_Parameter_Profile);
         begin
            --  The 'Port_Type' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (TN (T_Port_Type)),
                 Make_Defining_Identifier (Map_Port_Enumeration_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Integer_Array' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (TN (T_Integer_Array)),
                 Make_Defining_Identifier (Map_Integer_Array_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Kind_Array' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (TN (T_Port_Kind_Array)),
                 Make_Defining_Identifier (Map_Kind_Array_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Image_Array' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (TN (T_Port_Image_Array)),
                 Make_Defining_Identifier (Map_Image_Array_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Address_Array' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (TN (T_Address_Array)),
                 Make_Defining_Identifier (Map_Address_Array_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Overflow_Protocol_Array' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (TN (T_Overflow_Protocol_Array)),
                 Make_Defining_Identifier
                   (Map_Overflow_Protocol_Array_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Thread_Interface' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (TN (T_Thread_Interface_Type)),
                 Make_Defining_Identifier (Map_Port_Interface_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Current_Entity' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Current_Entity)),
                 Extract_Enumerator (E));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Thread_Port_Kinds' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Thread_Port_Kinds)),
                 Make_Defining_Identifier (Map_Port_Kinds_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Has_Event_Ports' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Has_Event_Ports)),
                 Make_Literal
                   (New_Boolean_Value
                      (Has_In_Event_Ports (E)
                       or else Has_Out_Event_Ports (E))));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Thread_Port_Images' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Thread_Port_Images)),
                 Make_Defining_Identifier (Map_Port_Images_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Thread_Fifo_Sizes' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Thread_Fifo_Sizes)),
                 Make_Defining_Identifier (Map_FIFO_Sizes_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Thread_Fifo_Offsets' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Thread_Fifo_Offsets)),
                 Make_Defining_Identifier (Map_Offsets_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Thread_Overflow_Protocols' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Thread_Overflow_Protocols)),
                 Make_Defining_Identifier (Map_Overflow_Protocols_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Urgencies' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Urgencies)),
                 Make_Defining_Identifier (Map_Urgencies_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Global_Data_Queue_Size' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Global_Data_Queue_Size)),
                 Make_Defining_Identifier (Map_Total_Size_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'N_Destinations' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_N_Destinations)),
                 Make_Defining_Identifier (Map_N_Destination_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Destinations' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (PN (P_Destinations)),
                 Make_Defining_Identifier (Map_Destination_Name (E)));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Marshall' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (SN (S_Marshall)),
                 Extract_Designator
                   (ADN.Marshall_Node (Backend_Node (Identifier (E)))));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Send' generic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (SN (S_Send)),
                 Extract_Designator
                   (ADN.Send_Node
                      (Backend_Node
                         (Identifier
                            (Corresponding_Instance
                               (Get_Container_Process
                                  (Parent_Subcomponent (E))))))));
            Append_Node_To_List (N, Inst_Profile);

            --  The 'Next_Deadline' genertic formal

            N :=
              Make_Parameter_Association
                (Make_Defining_Identifier (SN (S_Next_Deadline)),
                 RE (RE_Clock));
            --                 Make_Selected_Component
--                   (Map_Task_Identifier (E),
--                    Make_Defining_Identifier (SN (S_Next_Deadline))));
            Append_Node_To_List (N, Inst_Profile);

            N :=
              Make_Package_Instantiation
                (Defining_Identifier =>
                   Make_Defining_Identifier (Map_Interrogators_Name (E)),
                 Generic_Package =>
                   RU (RU_PolyORB_HI_Thread_Interrogators, Elaborated => True),
                 Parameter_List => Inst_Profile);

            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end;

         --  Implementations of the runtime routines.

         declare
            procedure Implement_Subprogram
              (Spec                 : Node_Id;
               RR                   : Runtime_Routine;
               Add_Error_Management : Boolean := False);
            --  Generate a subprogram implementation from the given
            --  interrogation routine spec. The new subprogram uses
            --  the value of the first parameter in order to invoke
            --  the proper instance specific interrogation routine.

            procedure Add_Alternative (Spec : Node_Id; RR : Runtime_Routine);
            --  Add the case alternative corresponding to runtime 'RR'
            --  and associated to the current thread instance to the
            --  list of case alternative of the corresponding thread
            --  component.

            function Make_RR_Call
              (Spec : Node_Id;
               RR   : Runtime_Routine) return Node_Id;

            --------------------------
            -- Implement_Subprogram --
            --------------------------

            procedure Implement_Subprogram
              (Spec                 : Node_Id;
               RR                   : Runtime_Routine;
               Add_Error_Management : Boolean := False)
            is
               Alternatives : constant List_Id := New_List (ADN.K_List_Id);
               Declarations : constant List_Id :=
                 New_List (ADN.K_Declaration_List);
               Statements : constant List_Id :=
                 New_List (ADN.K_Statement_List);
               N                : Node_Id;
               Else_Statements  : constant List_Id := New_List (ADN.K_List_Id);
               Elsif_Statements : constant List_Id := New_List (ADN.K_List_Id);

               Pragma_Warnings_Off_Value : Value_Id;

            begin
               --  Initialize the list associated to the current
               --  thread component.

               Set_List (E, RR, Alternatives);

               N :=
                 Make_Used_Package (RU (RU_PolyORB_HI_Generated_Deployment));
               Append_Node_To_List (N, Declarations);

               N :=
                 Make_Pragma_Statement
                   (Pragma_Unreferenced,
                    Make_List_Id (Make_Defining_Identifier (PN (P_Entity))));
               Append_Node_To_List (N, Declarations);

               --  Build a string literal for the pragma Warnings On|Off:
               --
               --  If there is no error recovery function, and the
               --  current subprogram is a function, we need to shut
               --  down the warning on missing return: by construction
               --  of the source code, there cannot be situation in
               --  which we exit without entering one of the if
               --  statements.

               Set_Str_To_Name_Buffer ("*return*");
               Pragma_Warnings_Off_Value := New_String_Value (Name_Find);

               if (not Add_Error_Management)
                 and then Present (ADN.Return_Type (Spec))
               then
                  N :=
                    Make_Pragma_Statement
                      (Pragma_Warnings,
                       Make_List_Id
                         (RE (RE_Off),
                          Make_Literal (Pragma_Warnings_Off_Value)));
                  Append_Node_To_List (N, Statements);
               end if;

               if Add_Error_Management then
                  N :=
                    Make_Qualified_Expression
                      (RE (RE_Error_Kind),
                       Make_Record_Aggregate
                         (Make_List_Id (RE (RE_Error_Transport))));
                  N := Make_Return_Statement (N);
                  Append_Node_To_List (N, Else_Statements);
               end if;

               --  Add the alternative of the current instance

               Add_Alternative (Spec, RR);

               --  Make the if statement: to avoid a useless if
               --  statement, we take the head of the Alternatives as
               --  first statement, and the tail for the elsif part.

               ADN.Set_First_Node
                 (Elsif_Statements,
                  ADN.Next_Node (ADN.First_Node (Alternatives)));

               N :=
                 Make_If_Statement
                   (Condition => ADN.Condition (ADN.First_Node (Alternatives)),
                    Then_Statements =>
                      ADN.Then_Statements (ADN.First_Node (Alternatives)),
                    Elsif_Statements => Elsif_Statements,
                    Else_Statements  => Else_Statements);

               N := Make_RR_Call (Spec, RR);
               Append_Node_To_List (N, Statements);

               if (not Add_Error_Management)
                 and then Present (ADN.Return_Type (Spec))
               then
                  N :=
                    Make_Pragma_Statement
                      (Pragma_Warnings,
                       Make_List_Id
                         (RE (RE_On),
                          Make_Literal (Pragma_Warnings_Off_Value)));
                  Append_Node_To_List (N, Statements);
               end if;

               --  Make the subprogram implementation

               N :=
                 Make_Subprogram_Implementation
                   (Spec,
                    Declarations,
                    Statements,
                    Runtime_Body_Aspect_Definition (E, RR));
               Append_Node_To_List (N, Interrogation_Routine_List);
            end Implement_Subprogram;

            ------------------
            -- Make_RR_Call --
            ------------------

            function Make_RR_Call
              (Spec : Node_Id;
               RR   : Runtime_Routine) return Node_Id
            is
               Alternatives  : constant List_Id := Get_List (E, RR);
               Actual_Implem : constant Node_Id :=
                 Make_Defining_Identifier
                   (ADN.Name (ADN.Defining_Identifier (Spec)));
               Call_Profile  : constant List_Id := New_List (ADN.K_List_Id);
               Param_Profile : constant List_Id :=
                 ADN.Parameter_Profile (Spec);
               P : Node_Id;
               N : Node_Id;
            begin
               pragma Assert (Alternatives /= No_List);

               Set_Homogeneous_Parent_Unit_Name
                 (Actual_Implem,
                  Make_Defining_Identifier (Map_Interrogators_Name (E)));

               --  Skip the first parameter of Spec

               P := ADN.Next_Node (ADN.First_Node (Param_Profile));

               while Present (P) loop
                  N :=
                    Make_Defining_Identifier
                      (ADN.Name (ADN.Defining_Identifier (P)));
                  Append_Node_To_List (N, Call_Profile);

                  P := ADN.Next_Node (P);
               end loop;

               N := Make_Subprogram_Call (Actual_Implem, Call_Profile);
               --  If we deal with a function, make a return statement
               --  instead of a procedure call.

               if Present (ADN.Return_Type (Spec)) then
                  N := Make_Return_Statement (N);
               end if;
               return N;
            end Make_RR_Call;

            ---------------------
            -- Add_Alternative --
            ---------------------

            procedure Add_Alternative (Spec : Node_Id; RR : Runtime_Routine) is
               Alternatives  : constant List_Id := Get_List (E, RR);
               Actual_Implem : constant Node_Id :=
                 Make_Defining_Identifier
                   (ADN.Name (ADN.Defining_Identifier (Spec)));
               Call_Profile  : constant List_Id := New_List (ADN.K_List_Id);
               Param_Profile : constant List_Id :=
                 ADN.Parameter_Profile (Spec);
               P : Node_Id;
               N : Node_Id;
            begin
               pragma Assert (Alternatives /= No_List);

               Set_Homogeneous_Parent_Unit_Name
                 (Actual_Implem,
                  Make_Defining_Identifier (Map_Interrogators_Name (E)));

               --  Skip the first parameter of Spec

               P := ADN.Next_Node (ADN.First_Node (Param_Profile));

               while Present (P) loop
                  N :=
                    Make_Defining_Identifier
                      (ADN.Name (ADN.Defining_Identifier (P)));
                  Append_Node_To_List (N, Call_Profile);

                  P := ADN.Next_Node (P);
               end loop;

               N := Make_Subprogram_Call (Actual_Implem, Call_Profile);
               --  If we deal with a function, make a return statement
               --  instead of a procedure call.

               if Present (ADN.Return_Type (Spec)) then
                  N := Make_Return_Statement (N);
               end if;

               --  Make the alternative

               N :=
                 Make_Elsif_Statement
                   (Make_Expression
                      (Make_Defining_Identifier (PN (P_Entity)),
                       Op_Equal,
                       Extract_Enumerator (E)),
                    Make_List_Id (N));
               Append_Node_To_List (N, Alternatives);
            end Add_Alternative;

         begin
            --  Add the current interrogator to the package refined state

            Append_Node_To_List
              (Map_Refined_Global_Name (E),
               Package_Body_Refined_States);

            --  All the runtime routines below are also generated once
            --  per thread component.

            if Not_Handled then
               if Has_Out_Ports (E) then
                  Implement_Subprogram
                    (Send_Output_Spec (E),
                     RR_Send_Output,
                     True);
                  Implement_Subprogram (Put_Value_Spec (E), RR_Put_Value);
               end if;

               if Has_In_Ports (E) then
                  Implement_Subprogram
                    (Receive_Input_Spec (E),
                     RR_Receive_Input);
                  Implement_Subprogram (Get_Value_Spec (E), RR_Get_Value);
                  Implement_Subprogram (Get_Value_Spec_2 (E), RR_Get_Value);
                  Implement_Subprogram (Get_Sender_Spec (E), RR_Get_Sender);
                  Implement_Subprogram (Get_Count_Spec (E), RR_Get_Count);
                  Implement_Subprogram
                    (Get_Time_Stamp_Spec (E),
                     RR_Get_Time_Stamp);
                  Implement_Subprogram (Next_Value_Spec (E), RR_Next_Value);
                  Implement_Subprogram
                    (Wait_For_Incoming_Events_Spec (E),
                     RR_Wait_For_Incoming_Events);
                  Implement_Subprogram
                    (Store_Received_Message_Spec (E),
                     RR_Store_Received_Message);
               end if;

            else
               --  Complete the case alternatives corresponding to the
               --  current instance.

               Add_Alternative (Send_Output_Spec (E), RR_Send_Output);
               Add_Alternative (Put_Value_Spec (E), RR_Put_Value);
               Add_Alternative (Receive_Input_Spec (E), RR_Receive_Input);
               Add_Alternative (Get_Value_Spec (E), RR_Get_Value);
               Add_Alternative (Get_Value_Spec_2 (E), RR_Get_Value);
               Add_Alternative (Get_Sender_Spec (E), RR_Get_Sender);
               Add_Alternative (Get_Count_Spec (E), RR_Get_Count);
               Add_Alternative (Get_Time_Stamp_Spec (E), RR_Get_Time_Stamp);
               Add_Alternative (Next_Value_Spec (E), RR_Next_Value);
               Add_Alternative
                 (Store_Received_Message_Spec (E),
                  RR_Store_Received_Message);
               Add_Alternative
                 (Wait_For_Incoming_Events_Spec (E),
                  RR_Wait_For_Incoming_Events);
            end if;
         end;
      end Runtime_Routine_Bodies;

      -----------------------------------
      -- Make_Current_Mode_Declaration --
      -----------------------------------

      function Make_Current_Mode_Declaration (E : Node_Id) return Node_Id is
         M : Node_Id;
         N : Node_Id;
      begin
         --  The value of the global variable is the enumeratioin
         --  literal corresponding to the initial mode of the thread.

         M := First_Node (Modes (E));
         N := No_Node;

         while Present (M) loop
            if Is_Initial (M) then
               N := Map_Ada_Defining_Identifier (M);
               exit;
            end if;

            M := Next_Node (M);
         end loop;

         --  If no initial mode has been found, there is definitely an
         --  error in the analyzer.

         if No (N) then
            raise Program_Error with "No initial mode in mode list";
         end if;

         --  Declare the variable

         Current_Mode_Identifier :=
           Make_Defining_Identifier (Map_Current_Mode_Name (E));

         N :=
           Make_Object_Declaration
             (Defining_Identifier => Current_Mode_Identifier,
              Object_Definition   =>
                Make_Defining_Identifier (Map_Modes_Enumeration_Name (E)),
              Expression => N);

         return N;
      end Make_Current_Mode_Declaration;

      ----------------------------
      -- Get_List_Internal_Name --
      ----------------------------

      function Get_List_Internal_Name
        (Thread : Node_Id;
         RR     : Runtime_Routine) return Name_Id
      is
      begin
         pragma Assert (AINU.Is_Thread (Thread));

         Set_Nat_To_Name_Buffer
           (Nat (Parent_Component (Parent_Subcomponent (Thread))));
         Add_Str_To_Name_Buffer ("%");
         Add_Nat_To_Name_Buffer (Nat (Corresponding_Declaration (Thread)));
         Add_Str_To_Name_Buffer ("%RR%" & RR'Img);
         return Name_Find;
      end Get_List_Internal_Name;

      --------------
      -- Get_List --
      --------------

      function Get_List
        (Thread : Node_Id;
         RR     : Runtime_Routine) return List_Id
      is
         I_Name : constant Name_Id := Get_List_Internal_Name (Thread, RR);
      begin
         return List_Id (Get_Name_Table_Info (I_Name));
      end Get_List;

      --------------
      -- Set_List --
      --------------

      procedure Set_List
        (Thread : Node_Id;
         RR     : Runtime_Routine;
         L      : List_Id)
      is
         I_Name : constant Name_Id := Get_List_Internal_Name (Thread, RR);
      begin
         Set_Name_Table_Info (I_Name, Nat (L));
      end Set_List;

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

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         Implementation : constant Node_Id := Get_Implementation (E);

      begin
         if Implementation /= No_Node then

            --  A device may be "implemented" using an abstract
            --  component, representing its driver. We iterate on its
            --  subcomponents to attach specific threads associated.

            Visit_Subcomponents_Of (Implementation);
         end if;
      end Visit_Device_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P          : constant Node_Id := ADN.Entity (U);
         S          : Node_Id;
         N          : Node_Id;
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));

         function Package_Body_Aspect_Definition return Node_Id is
         begin
            if Add_SPARK2014_Annotations then
               return Make_Aspect_Specification
                 (Make_List_Id
                    (Make_Aspect
                       (ASN (A_Refined_State),
                        Make_Refinement_List
                          (Make_List_Id
                             (Make_Refinement_Clause
                                (Make_Defining_Identifier
                                   (PN (P_Elaborated_Variables)),
                                 Package_Body_Refined_States))))));
            else
               return No_Node;
            end if;
         end Package_Body_Aspect_Definition;

      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Activity_Body;

         --  Start recording the handling since they have to be reset
         --  for each node.

         Start_Recording_Handlings;

         --  Reset hybrid thread related global variables

         Has_Hybrid_Threads       := False;
         Hybrid_Thread_Elements   := No_List;
         Last_Hybrid_Thread_Index := 0;

         --  Initialize the runtime routine list

         Interrogation_Routine_List := New_List (ADN.K_Statement_List);
         Package_Body_Refined_States := New_List (ADN.K_List_Id);

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  If the process has a data subcomponent, then map a
               --  shared variable.

               if AINU.Is_Data (Corresponding_Instance (S))
                 and then
                   Get_Concurrency_Protocol (Corresponding_Instance (S)) =
                   Priority_Ceiling
               then
                  --  XXX For now, we disable SPARK_Mode due to the
                  --  inability of SPARK GPL2015 to support
                  --  variable that denotes protected objects.

                  N :=
                    Make_Pragma_Statement
                      (Pragma_SPARK_Mode,
                       Make_List_Id (RE (RE_Off)));

                  Append_Node_To_List
                    (N,
                     ADN.Package_Headers (Current_Package));
               end if;

               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         --  Append the runtime routines

         Append_Node_To_List
           (ADN.First_Node (Interrogation_Routine_List),
            ADN.Statements (Current_Package));

         if Has_Hybrid_Threads then
            declare
               Profile : constant List_Id := New_List (ADN.K_List_Id);
            begin
               pragma Assert (not Is_Empty (Hybrid_Thread_Elements));

               N :=
                 Message_Comment
                   ("In order for them to work correctly," &
                    " hybrid task need the presence of" &
                    " a driver task to trigger them at" &
                    " their period");
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Declare the hybrid task set

               N :=
                 Make_Object_Declaration
                   (Defining_Identifier =>
                      Make_Defining_Identifier (PN (P_Hybrid_Task_Set)),
                    Object_Definition => RE (RE_Hybrid_Task_Info_Array),
                    Expression        =>
                      Make_Array_Aggregate (Hybrid_Thread_Elements));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Instantiate the hybrid task driver

               N := Make_Defining_Identifier (PN (P_Hybrid_Task_Set));
               Append_Node_To_List (N, Profile);

               N := Make_Attribute_Designator (RE (RE_Priority), A_Last);
               Append_Node_To_List (N, Profile);

               N := Make_Literal (New_Integer_Value (128_000, 1, 10));
               Append_Node_To_List (N, Profile);

               N := RE (RE_Deliver);
               Append_Node_To_List (N, Profile);

               N :=
                 Make_Package_Instantiation
                   (Make_Defining_Identifier (PN (P_Hybrid_Task_Driver)),
                    RU (RU_PolyORB_HI_Hybrid_Task_Driver_Driver),
                    Profile);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               N :=
                 Make_Pragma_Statement
                   (Pragma_Unreferenced,
                    Make_List_Id
                      (Make_Defining_Identifier (PN (P_Hybrid_Task_Driver))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));
            end;
         end if;

         --  Visit all devices attached to the parent system that
         --  share the same processor as process E.

         if not AAU.Is_Empty (Subcomponents (The_System)) then
            S := First_Node (Subcomponents (The_System));
            while Present (S) loop
               if AAU.Is_Device (Corresponding_Instance (S))
                 and then
                   Get_Bound_Processor (Corresponding_Instance (S)) =
                   Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;

         ADN.Set_Aspect_Specification
           (Current_Package,
            Package_Body_Aspect_Definition);

         --  Unmark all the marked types

         Reset_Handlings;

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
         P : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         S : constant Node_Id := Parent_Subcomponent (E);
         N : Node_Id;
      begin
         if Has_Ports (E) then
            --  Implement the routines that allow user code to
            --  manipulate the thread.

            case P is
               when Thread_Periodic =>
                  N :=
                    Message_Comment
                      ("Periodic task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

               when Thread_Sporadic =>
                  N :=
                    Message_Comment
                      ("Sporadic task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

               when Thread_Aperiodic =>
                  N :=
                    Message_Comment
                      ("Aperiodic task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

               when Thread_Background =>
                  N :=
                    Message_Comment
                      ("Background task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

               when Thread_ISR =>
                  N :=
                    Message_Comment
                      ("ISR task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

               when Thread_Hybrid =>
                  N :=
                    Message_Comment
                      ("Hybrid task : " &
                         Get_Name_String (Display_Name (Identifier (S))));
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

                  --  Hybrid threads requires an extra driver thread to be
                  --  created.

                  declare
                     Aggr : constant List_Id :=
                       New_List (ADN.K_Component_List);
                  begin
                     Has_Hybrid_Threads := True;

                     if Hybrid_Thread_Elements = No_List then
                        Hybrid_Thread_Elements :=
                          New_List (ADN.K_Element_List);
                     end if;

                     --  Append the element association corresponding to
                     --  E to the hybrid task set.

                     N := Extract_Enumerator (E);
                     Append_Node_To_List (N, Aggr);

                     --  We know that the last node added to the feature
                     --  list of E is the one appended at exapnsion time
                     --  and corresponding to the fake event part that
                     --  will receive the dispatch messages from the
                     --  driver.

                     N := Extract_Enumerator (Last_Node (Features (E)));
                     Append_Node_To_List (N, Aggr);

                     N := Map_Ada_Time (Get_Thread_Period (E));
                     Append_Node_To_List (N, Aggr);

                     N := RE (RE_System_Startup_Time);
                     Append_Node_To_List (N, Aggr);

                     N := RE (RE_True);
                     Append_Node_To_List (N, Aggr);

                     N :=
                       Make_Qualified_Expression
                         (RE (RE_Hybrid_Task_Info),
                          Make_Record_Aggregate (Aggr));

                     Last_Hybrid_Thread_Index := Last_Hybrid_Thread_Index + 1;

                     N :=
                       Make_Element_Association
                         (Make_Literal
                            (New_Integer_Value
                               (Last_Hybrid_Thread_Index, 1, 10)),
                          N);
                     Append_Node_To_List (N, Hybrid_Thread_Elements);
                  end;

               when others =>
                  raise Program_Error;
            end case;

            Runtime_Routine_Bodies (E);
         end if;

         if Has_Modes (E) then
            --  If the thread has operational modes, then generate the
            --  body of the mode updater procedure and the global
            --  variable designating the current mode. there is no
            --  harm using a global variable because
            --  it is accessed exclusively by the thread.
            --  We also with a package instance of teh corresponding
            --  scheduler

            N := Make_Current_Mode_Declaration (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            if Is_Fusioned (E) then
               N := Make_Mode_Updater_body (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               N :=
                 Make_Withed_Package
                   (Make_Defining_Identifier
                      (Map_Scheduler_Instance_Name (E)));
               Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));
            end if;
         end if;

      end Visit_Thread_Instance;

      ----------------------------
      -- Make_Mode_Updater_Body --
      ----------------------------

      function Make_Mode_Updater_body (E : Node_Id) return Node_Id is
         N    : Node_Id;
         Spec : constant Node_Id :=
           Backend_Node (Identifier (First_Node (Modes (E))));
         Stats : constant List_Id := New_List (ADN.K_List_Id);
      begin
         N :=
           Make_Assignment_Statement
             (Variable_Identifier => Current_Mode_Identifier,
              Expression          => Make_Defining_Identifier (PN (P_Mode)));
         Append_Node_To_List (N, Stats);
         N := Make_Subprogram_Implementation (Spec, No_List, Stats);
         return N;
      end Make_Mode_Updater_body;

   end Package_Body;

end Ocarina.Backends.PO_HI_Ada.Activity;
