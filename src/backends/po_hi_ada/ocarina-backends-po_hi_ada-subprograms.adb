------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.PO_HI_ADA.SUBPROGRAMS                   --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Values;
with Ocarina.Backends.PO_HI_Ada.Mapping;
with Ocarina.Backends.PO_HI_Ada.Runtime;

package body Ocarina.Backends.PO_HI_Ada.Subprograms is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Ada_Values;
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
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);

      function Put_Value_Spec (E : Node_Id) return Node_Id;
      function Get_Value_Spec (E : Node_Id) return Node_Id;
      function Next_Value_Spec (E : Node_Id) return Node_Id;
      function Get_Count_Spec (E : Node_Id) return Node_Id;
      --  Routines to raise and collect subprogram events in a thread
      --  safe manner.

      --------------------
      -- Put_Value_Spec --
      --------------------

      function Put_Value_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N       : Node_Id;
      begin
         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Status)),
              Subtype_Mark        =>
                Make_Defining_Identifier (Map_Port_Status_Name (E)),
              Parameter_Mode => Mode_Inout);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Parameter_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (PN (P_Spg_Interface)),
              Subtype_Mark =>
                Make_Defining_Identifier (Map_Port_Interface_Name (E)),
              Parameter_Mode => Mode_In);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (SN (S_Put_Value)),
              Parameter_Profile => Profile,
              Return_Type       => No_Node);
         return N;
      end Put_Value_Spec;

      --------------------
      -- Get_Value_Spec --
      --------------------

      function Get_Value_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N       : Node_Id;
      begin
         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Status)),
              Subtype_Mark        =>
                Make_Defining_Identifier (Map_Port_Status_Name (E)),
              Parameter_Mode => Mode_In);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Port)),
              Subtype_Mark        =>
                Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
              Parameter_Mode => Mode_In);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (SN (S_Get_Value)),
              Parameter_Profile => Profile,
              Return_Type       =>
                Make_Defining_Identifier (Map_Port_Interface_Name (E)));
         return N;
      end Get_Value_Spec;

      ---------------------
      -- Next_Value_Spec --
      ---------------------

      function Next_Value_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N       : Node_Id;
      begin
         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Status)),
              Subtype_Mark        =>
                Make_Defining_Identifier (Map_Port_Status_Name (E)),
              Parameter_Mode => Mode_Inout);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Port)),
              Subtype_Mark        =>
                Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
              Parameter_Mode => Mode_In);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (SN (S_Next_Value)),
              Parameter_Profile => Profile,
              Return_Type       => No_Node);
         return N;
      end Next_Value_Spec;

      --------------------
      -- Get_Count_Spec --
      --------------------

      function Get_Count_Spec (E : Node_Id) return Node_Id is
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
         N       : Node_Id;
      begin
         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Status)),
              Subtype_Mark        =>
                Make_Defining_Identifier (Map_Port_Status_Name (E)),
              Parameter_Mode => Mode_In);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Parameter_Specification
             (Defining_Identifier => Make_Defining_Identifier (PN (P_Port)),
              Subtype_Mark        =>
                Make_Defining_Identifier (Map_Port_Enumeration_Name (E)),
              Parameter_Mode => Mode_In);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Subprogram_Specification
             (Defining_Identifier =>
                Make_Defining_Identifier (SN (S_Get_Count)),
              Parameter_Profile => Profile,
              Return_Type       => RE (RE_Integer));
         return N;
      end Get_Count_Spec;

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

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

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
         Data_Representation : constant Supported_Data_Representation :=
           Get_Data_Representation (E);
         S : Node_Id;
      begin
         if Data_Representation = Data_With_Accessors then
            --  Visit all the accessor subprograms of the data type

            S := First_Node (Features (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_Data_Instance;

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
                  if not AAU.Is_Subprogram (Corresponding_Instance (S)) then
                     Visit_Component_Instance (Corresponding_Instance (S));
                  end if;
                  S := Next_Node (S);
               end loop;
            end if;
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
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));

      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Subprograms_Spec;

         --  Start recording all the handlings

         Start_Recording_Handlings;

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
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

         --  Unmark all the marked subprograms

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         N        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Generate the spec of the subprogram

         if No (Get_Handling (E, By_Name, H_Ada_Subprogram_Spec)) then
            --  Mark the subprogram as being handled

            Set_Handling (E, By_Name, H_Ada_Subprogram_Spec, E);

            if Has_Out_Ports (E) then
               --  If the subprogram contains out event [data] ports,
               --  declare the following entities.

               --  An enumeration type for the SPG out ports

               N := Map_Port_Enumeration (E);
               Bind_AADL_To_Port_Enumeration (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  A Subprogram_Interface discriminated record

               N := Map_Port_Interface (E);
               Bind_AADL_To_Port_Interface (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  The same AADL subprogram, may be invoked by
               --  different threads. The user implementation DOES NOT
               --  HAVE TO know which thread is actually running the
               --  subprogram. In partivular, if a subprogram, raises
               --  events on one of its out ports, the venet must be
               --  dispatched to the thread running the subprogra in a
               --  way which is transparent to the user. A simple way
               --  to perform this is the use of an opaque IN OUT
               --  parameter which is given to the subprogram. This
               --  implies that the thread is aware of the subprogram
               --  event raise AFTER the complete run of the
               --  subprogram.

               --  A private type called <spg>_Port_Status.

               N := Map_Port_Status (E, Full_Declaration => False);
               Bind_AADL_To_Type_Definition (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               N := Map_Port_Status (E, Full_Declaration => True);
               Append_Node_To_List (N, ADN.Private_Part (Current_Package));

               --  Spec of the Put_Value subprogram, generally used by
               --  the user code to raise an event [data].

               N := Put_Value_Spec (E);
               Bind_AADL_To_Put_Value (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Spec of the Get_Value subprogram, generally used by
               --  the thread code to get the raised events.

               N := Get_Value_Spec (E);
               Bind_AADL_To_Get_Value (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Spec of the Next_Value subprogram, generally used by
               --  the thread code to get the raised events.

               N := Next_Value_Spec (E);
               Bind_AADL_To_Next_Value (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Spec of the Get_Count subprogram, generally used
               --  by the thread code to get the raised events.

               N := Get_Count_Spec (E);
               Bind_AADL_To_Get_Count (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            end if;

            N := Map_Ada_Subprogram_Spec (E);
            Bind_AADL_To_Subprogram (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         else
            declare
               H : constant Node_Id :=
                 Get_Handling (E, By_Name, H_Ada_Subprogram_Spec);
            begin
               Bind_AADL_To_Subprogram
                 (Identifier (E),
                  ADN.Subprogram_Node (Backend_Node (Identifier (H))));

               if Has_Out_Ports (E) then
                  Bind_AADL_To_Port_Enumeration
                    (Identifier (E),
                     ADN.Port_Enumeration_Node
                       (Backend_Node (Identifier (H))));
                  Bind_AADL_To_Port_Interface
                    (Identifier (E),
                     ADN.Port_Interface_Node (Backend_Node (Identifier (H))));
                  Bind_AADL_To_Type_Definition
                    (Identifier (E),
                     ADN.Type_Definition_Node (Backend_Node (Identifier (H))));
                  Bind_AADL_To_Put_Value
                    (Identifier (E),
                     ADN.Put_Value_Node (Backend_Node (Identifier (H))));
                  Bind_AADL_To_Get_Value
                    (Identifier (E),
                     ADN.Get_Value_Node (Backend_Node (Identifier (H))));
               end if;
            end;
         end if;

         --  Visit all the call sequences of the subprogram

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (Ada_Root);

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

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Visit all the call sequences of the thread

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Thread_Instance;

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
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);

      function Put_Value_Body (E : Node_Id) return Node_Id;
      function Get_Value_Body (E : Node_Id) return Node_Id;
      function Next_Value_Body (E : Node_Id) return Node_Id;
      function Get_Count_Body (E : Node_Id) return Node_Id;
      --  Routines to raise and collect subprogram events in a thread
      --  safe manner.

      --------------------
      -- Put_Value_Body --
      --------------------

      function Put_Value_Body (E : Node_Id) return Node_Id is
         Spec : constant Node_Id :=
           ADN.Put_Value_Node (Backend_Node (Identifier (E)));
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Alternatives : constant List_Id := New_List (ADN.K_List_Id);
         F            : Node_Id;
         N            : Node_Id;
      begin
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               declare
                  St : constant List_Id := New_List (ADN.K_Statement_List);
               begin
                  --  Set the boolean flag corresponding to the
                  --  component to 'True'.

                  N :=
                    Make_Assignment_Statement
                      (Make_Selected_Component
                         (Make_Defining_Identifier (PN (P_Status)),
                          Map_Ada_Defining_Identifier (F)),
                       RE (RE_True));
                  Append_Node_To_List (N, St);

                  if AAN.Is_Data (F) then
                     --  Update the component correspodning to the out
                     --  port in the status structure

                     N :=
                       Make_Assignment_Statement
                         (Make_Selected_Component
                            (Make_Defining_Identifier (PN (P_Status)),
                             Make_Defining_Identifier
                               (Map_Ada_Component_Name (F))),
                          Make_Selected_Component
                            (Make_Defining_Identifier (PN (P_Spg_Interface)),
                             Make_Defining_Identifier
                               (Map_Ada_Component_Name (F))));
                     Append_Node_To_List (N, St);
                  end if;

                  --  Create the case alternative

                  N :=
                    Make_Case_Statement_Alternative
                      (Make_List_Id (Map_Ada_Defining_Identifier (F)),
                       St);
                  Append_Node_To_List (N, Alternatives);
               end;
            end if;

            F := Next_Node (F);
         end loop;

         N :=
           Make_Case_Statement
             (Make_Selected_Component
                (Make_Defining_Identifier (PN (P_Spg_Interface)),
                 Make_Defining_Identifier (CN (C_Port))),
              Alternatives);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, No_List, Statements);
         return N;
      end Put_Value_Body;

      --------------------
      -- Get_Value_Body --
      --------------------

      function Get_Value_Body (E : Node_Id) return Node_Id is
         Spec : constant Node_Id :=
           ADN.Get_Value_Node (Backend_Node (Identifier (E)));
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Alternatives : constant List_Id := New_List (ADN.K_List_Id);
         F            : Node_Id;
         N            : Node_Id;
      begin
         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               declare
                  Aggr : constant List_Id := New_List (ADN.K_List_Id);
               begin
                  N :=
                    Make_Component_Association
                      (Make_Defining_Identifier (CN (C_Port)),
                       Map_Ada_Defining_Identifier (F));
                  Append_Node_To_List (N, Aggr);

                  if AAN.Is_Data (F) then
                     --  Update the component correspodning to the out
                     --  port in the status structure.

                     N :=
                       Make_Component_Association
                         (Make_Defining_Identifier
                            (Map_Ada_Component_Name (F)),
                          Make_Selected_Component
                            (Make_Defining_Identifier (PN (P_Status)),
                             Make_Defining_Identifier
                               (Map_Ada_Component_Name (F))));
                     Append_Node_To_List (N, Aggr);
                  end if;

                  N := Make_Return_Statement (Make_Record_Aggregate (Aggr));

                  --  Create the case alternative

                  N :=
                    Make_Case_Statement_Alternative
                      (Make_List_Id (Map_Ada_Defining_Identifier (F)),
                       Make_List_Id (N));
                  Append_Node_To_List (N, Alternatives);
               end;
            end if;

            F := Next_Node (F);
         end loop;

         N :=
           Make_Case_Statement
             (Make_Defining_Identifier (PN (P_Port)),
              Alternatives);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, No_List, Statements);
         return N;
      end Get_Value_Body;

      ---------------------
      -- Next_Value_Body --
      ---------------------

      function Next_Value_Body (E : Node_Id) return Node_Id is
         Spec : constant Node_Id :=
           ADN.Next_Value_Node (Backend_Node (Identifier (E)));
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Declarations : constant List_Id := New_List (ADN.K_Declaration_List);
         N            : Node_Id;
      begin
         --  FIXME: Not implemented yet for now

         N :=
           Make_Pragma_Statement
             (Pragma_Unreferenced,
              Make_List_Id
                (Make_Defining_Identifier (PN (P_Status)),
                 Make_Defining_Identifier (PN (P_Port))));
         Append_Node_To_List (N, Declarations);

         N := Message_Comment ("Not implemented yet!");
         Append_Node_To_List (N, Statements);

         N :=
           Make_Raise_Statement
             (Make_Defining_Identifier (EN (E_Program_Error)));
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, Declarations, Statements);
         return N;
      end Next_Value_Body;

      --------------------
      -- Get_Count_Body --
      --------------------

      function Get_Count_Body (E : Node_Id) return Node_Id is
         Spec : constant Node_Id :=
           ADN.Get_Count_Node (Backend_Node (Identifier (E)));
         Statements   : constant List_Id := New_List (ADN.K_Statement_List);
         Alternatives : constant List_Id := New_List (ADN.K_List_Id);
         F            : Node_Id;
         N            : Node_Id;
      begin
         --  FIXME: For now, the returned value is either 0 or 1, we
         --  must take into account the port fifo size.

         F := First_Node (Features (E));

         while Present (F) loop
            if Kind (F) = K_Port_Spec_Instance then
               declare
                  St : constant List_Id := New_List (ADN.K_Statement_List);
               begin
                  --  If the boolean flag corresponding to the
                  --  component is 'True' then return 1, else return 0

                  N :=
                    Make_If_Statement
                      (Condition =>
                         Make_Selected_Component
                           (Make_Defining_Identifier (PN (P_Status)),
                            Map_Ada_Defining_Identifier (F)),
                       Then_Statements =>
                         Make_List_Id
                           (Make_Return_Statement
                              (Make_Literal (New_Integer_Value (1, 1, 10)))),
                       Else_Statements =>
                         Make_List_Id
                           (Make_Return_Statement
                              (Make_Literal (New_Integer_Value (0, 1, 10)))));
                  Append_Node_To_List (N, St);

                  --  Create the case alternative

                  N :=
                    Make_Case_Statement_Alternative
                      (Make_List_Id (Map_Ada_Defining_Identifier (F)),
                       St);
                  Append_Node_To_List (N, Alternatives);
               end;
            end if;

            F := Next_Node (F);
         end loop;

         N :=
           Make_Case_Statement
             (Make_Defining_Identifier (PN (P_Port)),
              Alternatives);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation (Spec, No_List, Statements);
         return N;
      end Get_Count_Body;

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

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

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
         Data_Representation : constant Supported_Data_Representation :=
           Get_Data_Representation (E);
         S : Node_Id;
      begin
         if Data_Representation = Data_With_Accessors then
            --  Visit all the accessor subprograms of the data type

            S := First_Node (Features (E));

            while Present (S) loop

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;
      end Visit_Data_Instance;

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
                  if not AAU.Is_Subprogram (Corresponding_Instance (S)) then
                     Visit_Component_Instance (Corresponding_Instance (S));
                  end if;
                  S := Next_Node (S);
               end loop;
            end if;
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
         The_System : constant Node_Id :=
           Parent_Component (Parent_Subcomponent (E));

      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Subprograms_Body;

         --  Start recording all the handlings

         Start_Recording_Handlings;

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
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

         --  Unmark all the marked subprograms

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         N        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Generate the body of the subprogram

         if No (Get_Handling (E, By_Name, H_Ada_Subprogram_Body)) then
            N := Map_Ada_Subprogram_Body (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            if Has_Out_Ports (E) then
               --  If the subprogram contains out event [data] ports,
               --  declare the following entities.

               N := Put_Value_Body (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               N := Get_Value_Body (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               N := Next_Value_Body (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               N := Get_Count_Body (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));
            end if;

            --  Mark the data type as being handled

            Set_Handling (E, By_Name, H_Ada_Subprogram_Body, N);
         end if;

         --  Visit all the call sequences of the subprogram

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (Ada_Root);

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

         Pop_Entity; --  Ada_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         N        : Node_Id;
      begin
         --  Visit all the call sequences of the thread

         if not AAU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AAU.Is_Empty (Subprogram_Calls (Call_Seq)) then
                  Spg_Call := First_Node (Subprogram_Calls (Call_Seq));

                  while Present (Spg_Call) loop
                     Visit (Corresponding_Instance (Spg_Call));

                     Spg_Call := Next_Node (Spg_Call);
                  end loop;
               end if;

               Call_Seq := Next_Node (Call_Seq);
            end loop;
         end if;

         if Has_Modes (E) and then Is_Fusioned (E) then
            N :=
              Make_Withed_Package
                (Make_Defining_Identifier (Map_Scheduler_Instance_Name (E)),
                 Used => True);
            Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));
         end if;

      end Visit_Thread_Instance;

   end Package_Body;

end Ocarina.Backends.PO_HI_Ada.Subprograms;
