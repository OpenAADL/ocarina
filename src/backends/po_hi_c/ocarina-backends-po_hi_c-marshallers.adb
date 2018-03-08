------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.PO_HI_C.MARSHALLERS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2008-2009 Telecom ParisTech, 2010-2017 ESA & ISAE.      --
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
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Values;
with Ocarina.Backends.C_Common.Mapping;
with Ocarina.Backends.PO_HI_C.Runtime;

package body Ocarina.Backends.PO_HI_C.Marshallers is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Common.Mapping;
   use Ocarina.Backends.PO_HI_C.Runtime;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;
   package CTU renames Ocarina.Backends.C_Tree.Nutils;
   package CV renames Ocarina.Backends.C_Values;

   Marshaller_Request_Spec   : Node_Id;
   Unmarshaller_Request_Spec : Node_Id;

   Marshaller_Asn1_Request_Spec   : Node_Id;
   Unmarshaller_Asn1_Request_Spec : Node_Id;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      function Marshall_Type_Spec (E : Node_Id) return Node_Id;
      function Unmarshall_Type_Spec (E : Node_Id) return Node_Id;
      function Marshall_Request_Spec (E : Node_Id) return Node_Id;
      function Unmarshall_Request_Spec (E : Node_Id) return Node_Id;

      ---------------------------
      -- Marshall_Request_Spec --
      ---------------------------

      function Marshall_Request_Spec (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
      begin
         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Request)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Request_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Message)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Msg_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Offset)),
               Parameter_Type      => Make_Pointer_Type (RE (RE_Uint32_T))),
            Parameters);

         N :=
           Make_Function_Specification
             (Defining_Identifier =>
                Map_C_Marshaller_Subprogram (E, Is_Request => True),
              Parameters  => Parameters,
              Return_Type => New_Node (CTN.K_Void));
         return N;
      end Marshall_Request_Spec;

      -----------------------------
      -- Unmarshall_Request_Spec --
      -----------------------------

      function Unmarshall_Request_Spec (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
      begin
         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Request)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Request_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Message)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Msg_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Offset)),
               Parameter_Type      => Make_Pointer_Type (RE (RE_Uint32_T))),
            Parameters);

         N :=
           Make_Function_Specification
             (Defining_Identifier =>
                Map_C_Marshaller_Subprogram
                  (E,
                   Is_Request    => True,
                   Is_Unmarshall => True),
              Parameters  => Parameters,
              Return_Type => New_Node (CTN.K_Void));
         return N;
      end Unmarshall_Request_Spec;

      ------------------------
      -- Marshall_Type_Spec --
      ------------------------

      function Marshall_Type_Spec (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
      begin

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Value)),
               Parameter_Type      => Map_C_Defining_Identifier (E)),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Message)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Msg_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Offset)),
               Parameter_Type      => Make_Pointer_Type (RE (RE_Uint32_T))),
            Parameters);

         N :=
           Make_Function_Specification
             (Defining_Identifier => Map_C_Marshaller_Subprogram (E),
              Parameters          => Parameters,
              Return_Type         => New_Node (CTN.K_Void));
         return N;
      end Marshall_Type_Spec;

      --------------------------
      -- Unmarshall_Type_Spec --
      --------------------------

      function Unmarshall_Type_Spec (E : Node_Id) return Node_Id is
         N          : Node_Id;
         Parameters : constant List_Id := New_List (CTN.K_Parameter_List);
      begin

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Value)),
               Parameter_Type      =>
                 Make_Pointer_Type (Map_C_Defining_Identifier (E))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Message)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Msg_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Offset)),
               Parameter_Type      => Make_Pointer_Type (RE (RE_Uint32_T))),
            Parameters);

         N :=
           Make_Function_Specification
             (Defining_Identifier =>
                Map_C_Marshaller_Subprogram (E, Is_Unmarshall => True),
              Parameters  => Parameters,
              Return_Type => New_Node (CTN.K_Void));
         return N;
      end Unmarshall_Type_Spec;

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

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         N : Node_Id;
         S : Node_Id;
      begin
         Add_Include (RH (RH_Types));

         if No (Get_Handling (E, By_Name, H_C_Marshall_Spec)) then
            N := Marshall_Type_Spec (E);
            Set_Handling (E, By_Name, H_C_Marshall_Spec, N);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if No (Get_Handling (E, By_Name, H_C_Unmarshall_Spec)) then
            N := Unmarshall_Type_Spec (E);
            Set_Handling (E, By_Name, H_C_Unmarshall_Spec, N);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         Bind_AADL_To_Marshaller
           (Identifier (E),
            Get_Handling (E, By_Name, H_C_Marshall_Spec));

         Bind_AADL_To_Unmarshaller
           (Identifier (E),
            Get_Handling (E, By_Name, H_C_Unmarshall_Spec));

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               if AAU.Is_Data (Corresponding_Instance (S)) then
                  Visit (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P          : constant Node_Id := CTN.Entity (U);
         S          : Node_Id;
         Parameters : List_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Marshallers_Header;

         --  Start recording the handling since they have to be reset
         --  for each node.

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

         --  Make the main marshall_request function

         Parameters := New_List (CTN.K_Parameter_List);
         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Request)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Request_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Message)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Msg_T))),
            Parameters);

         Marshaller_Request_Spec :=
           Make_Function_Specification
             (Defining_Identifier => RE (RE_Marshall_Request),
              Parameters          => Parameters,
              Return_Type         => New_Node (CTN.K_Void));
         Append_Node_To_List
           (Marshaller_Request_Spec,
            CTN.Declarations (Current_File));

         --  Make the main unmarshall_request function

         Parameters := New_List (CTN.K_Parameter_List);
         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Request)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Request_T))),
            Parameters);

         Append_Node_To_List
           (Make_Parameter_Specification
              (Defining_Identifier =>
                 Make_Defining_Identifier (PN (P_Message)),
               Parameter_Type => Make_Pointer_Type (RE (RE_Msg_T))),
            Parameters);

         Unmarshaller_Request_Spec :=
           Make_Function_Specification
             (Defining_Identifier => RE (RE_Unmarshall_Request),
              Parameters          => Parameters,
              Return_Type         => New_Node (CTN.K_Void));
         Append_Node_To_List
           (Unmarshaller_Request_Spec,
            CTN.Declarations (Current_File));

         if Use_ASN1_Deployment then
            --  Make the main marshall_asn1_request function

            Parameters := New_List (CTN.K_Parameter_List);
            Append_Node_To_List
              (Make_Parameter_Specification
                 (Defining_Identifier =>
                    Make_Defining_Identifier (PN (P_Request)),
                  Parameter_Type => Make_Pointer_Type (RE (RE_Request_T))),
               Parameters);

            Append_Node_To_List
              (Make_Parameter_Specification
                 (Defining_Identifier => Make_Defining_Identifier (PN (P_Pkt)),
                  Parameter_Type => Make_Pointer_Type (RE (RE_Asn1_Pkt_T))),
               Parameters);

            Marshaller_Asn1_Request_Spec :=
              Make_Function_Specification
                (Defining_Identifier => RE (RE_Marshall_Asn1_Request),
                 Parameters          => Parameters,
                 Return_Type         => New_Node (CTN.K_Void));
            Append_Node_To_List
              (Marshaller_Asn1_Request_Spec,
               CTN.Declarations (Current_File));

            --  Make the main unmarshall_request function

            Parameters := New_List (CTN.K_Parameter_List);
            Append_Node_To_List
              (Make_Parameter_Specification
                 (Defining_Identifier =>
                    Make_Defining_Identifier (PN (P_Request)),
                  Parameter_Type => Make_Pointer_Type (RE (RE_Request_T))),
               Parameters);

            Append_Node_To_List
              (Make_Parameter_Specification
                 (Defining_Identifier => Make_Defining_Identifier (PN (P_Pkt)),
                  Parameter_Type => Make_Pointer_Type (RE (RE_Asn1_Pkt_T))),
               Parameters);

            Unmarshaller_Asn1_Request_Spec :=
              Make_Function_Specification
                (Defining_Identifier => RE (RE_Unmarshall_Asn1_Request),
                 Parameters          => Parameters,
                 Return_Type         => New_Node (CTN.K_Void));
            Append_Node_To_List
              (Unmarshaller_Asn1_Request_Spec,
               CTN.Declarations (Current_File));
         end if;

         --  Unmark all the marked types

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         F : Node_Id;
      begin
         --  Declare all necessary data types

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  Display_Located_Error
                    (Loc (F),
                     "Port features in subprogram are not supported",
                     Fatal => True);
               end if;

               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (C_Root);

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

         Pop_Entity; --  C_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         F        : Node_Id;
         N        : Node_Id;
      begin
         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Ocarina.ME_AADL.AADL_Instances.Nodes.Is_Data (F)
               then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

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

         if Has_Ports (E) then
            F := First_Node (Features (E));
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  N := Marshall_Request_Spec (F);
                  Set_Handling (F, By_Name, H_C_Marshall_Spec, N);
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

                  N := Unmarshall_Request_Spec (F);
                  Set_Handling (F, By_Name, H_C_Unmarshall_Spec, N);
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

                  Bind_AADL_To_Marshaller
                    (Identifier (F),
                     Get_Handling (F, By_Name, H_C_Marshall_Spec));

                  Bind_AADL_To_Unmarshaller
                    (Identifier (F),
                     Get_Handling (F, By_Name, H_C_Unmarshall_Spec));
               end if;
               F := Next_Node (F);
            end loop;
         end if;

      end Visit_Thread_Instance;

   end Header_File;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      function Get_Marshall_Function_Name (E : Node_Id) return Node_Id;
      function Get_Unmarshall_Function_Name (E : Node_Id) return Node_Id;

      Asn1_Marshall_Alternatives   : List_Id;
      Asn1_Unmarshall_Alternatives : List_Id;
      Marshall_Alternatives        : List_Id;
      Unmarshall_Alternatives      : List_Id;

      -----------------------------
      -- Make_Marshall_Type_Body --
      -----------------------------

      function Make_Marshall_Type_Body (E : Node_Id) return Node_Id is
         Data_Representation : Supported_Data_Representation;
         C                   : Node_Id;
         N                   : Node_Id;
         Declarations        : List_Id;
         Statements          : List_Id;
         Parameters          : List_Id;
         Marshall_Spec       : constant Node_Id :=
           CTN.Marshaller_Node (Backend_Node (Identifier (E)));
      begin
         --  Create the marshall function for this type

         Declarations := New_List (CTN.K_Declaration_List);
         Statements   := New_List (CTN.K_Statement_List);

         Data_Representation := Get_Data_Representation (E);

         case Data_Representation is
            when Data_With_Accessors | Data_Struct =>
               --  For structures, the marshaller calls the
               --  marshallers of each enclosed member.

               C := First_Node (Subcomponents (E));

               while Present (C) loop
                  if AAU.Is_Data (Corresponding_Instance (C)) then
                     Visit (Corresponding_Instance (C));

                     Parameters := New_List (CTN.K_Parameter_List);
                     Append_Node_To_List
                       (Make_Member_Designator
                          (Aggregate_Name =>
                             Make_Defining_Identifier (PN (P_Value)),
                           Defining_Identifier =>
                             Map_C_Defining_Identifier (C),
                           Is_Pointer => False),
                        Parameters);
                     Append_Node_To_List
                       (Make_Defining_Identifier (PN (P_Message)),
                        Parameters);
                     Append_Node_To_List
                       (Make_Defining_Identifier (PN (P_Offset)),
                        Parameters);
                     N :=
                       Make_Call_Profile
                         (Get_Marshall_Function_Name
                            (Corresponding_Instance (C)),
                          Parameters);
                     Append_Node_To_List (N, Statements);
                  end if;
                  C := Next_Node (C);
               end loop;

            when Data_Array | Data_String =>
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Value)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List (CTU.Get_Data_Size (E), Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Marshall_Array), Parameters);
               Append_Node_To_List (N, Statements);

            when Data_Bounded_Array =>
               --  In the case of a bounded array, we marshall first
               --  the length of the array, then the actual buffer

               --  1/ Marshall buffer length

               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Member_Designator
                    (Defining_Identifier =>
                       Make_Defining_Identifier (MN (M_Length)),
                     Aggregate_Name =>
                       Make_Defining_Identifier (PN (P_Value))),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Marshall_Int32), Parameters);
               Append_Node_To_List (N, Statements);

               --  2/ Marshall buffer data

               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Member_Designator
                    (Defining_Identifier =>
                       Make_Defining_Identifier (MN (M_Data)),
                     Aggregate_Name =>
                       Make_Defining_Identifier (PN (P_Value))),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List
                 (CTU.Get_Data_Size (E, Is_Pointer => False), Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Marshall_Array), Parameters);
               Append_Node_To_List (N, Statements);

            when Data_Enum =>
               --  In C, an enumeration constant is equivalent to an
               --  int constant, therefore we marshall the value as an
               --  __po_hi_int32 parameter.

               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Value)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Marshall_Int32), Parameters);
               Append_Node_To_List (N, Statements);

            when Data_None =>
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Defining_Identifier (PN (P_Value))),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List (CTU.Get_Data_Size (E), Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Marshall_Array), Parameters);
               Append_Node_To_List (N, Statements);

            when others =>
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Value)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N :=
                 Make_Call_Profile
                   (Get_Marshall_Function_Name (E),
                    Parameters);
               Append_Node_To_List (N, Statements);
         end case;

         N :=
           Make_Function_Implementation
             (Marshall_Spec,
              Declarations,
              Statements);
         return N;
      end Make_Marshall_Type_Body;

      -------------------------------
      -- Make_Unmarshall_Type_Body --
      -------------------------------

      function Make_Unmarshall_Type_Body (E : Node_Id) return Node_Id is
         Data_Representation : Supported_Data_Representation;
         N                   : Node_Id;
         C                   : Node_Id;
         Declarations        : List_Id;
         Statements          : List_Id;
         Parameters          : List_Id;
         Unmarshall_Spec     : constant Node_Id :=
           CTN.Unmarshaller_Node (Backend_Node (Identifier (E)));
      begin
         --  Create the unmarshall function for this type

         Declarations := New_List (CTN.K_Declaration_List);
         Statements   := New_List (CTN.K_Statement_List);

         Data_Representation := Get_Data_Representation (E);

         case Data_Representation is
            when Data_With_Accessors | Data_Struct =>

               C := First_Node (Subcomponents (E));

               while Present (C) loop
                  if AAU.Is_Data (Corresponding_Instance (C)) then
                     Visit (Corresponding_Instance (C));

                     Parameters := New_List (CTN.K_Parameter_List);
                     Append_Node_To_List
                       (Make_Variable_Address
                          (Make_Member_Designator
                             (Aggregate_Name =>
                                Make_Defining_Identifier (PN (P_Value)),
                              Defining_Identifier =>
                                Map_C_Defining_Identifier (C),
                              Is_Pointer => True)),
                        Parameters);
                     Append_Node_To_List
                       (Make_Defining_Identifier (PN (P_Message)),
                        Parameters);
                     Append_Node_To_List
                       (Make_Defining_Identifier (PN (P_Offset)),
                        Parameters);
                     N :=
                       Make_Call_Profile
                         (Get_Unmarshall_Function_Name
                            (Corresponding_Instance (C)),
                          Parameters);
                     Append_Node_To_List (N, Statements);
                  end if;
                  C := Next_Node (C);
               end loop;

            when Data_Array | Data_String =>
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Value)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List (CTU.Get_Data_Size (E), Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Unmarshall_Array), Parameters);
               Append_Node_To_List (N, Statements);

            when Data_Bounded_Array =>
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Member_Designator
                       (Defining_Identifier =>
                          Make_Defining_Identifier (MN (M_Length)),
                        Aggregate_Name =>
                          Make_Defining_Identifier (PN (P_Value)),
                        Is_Pointer => True)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Unmarshall_Int32), Parameters);
               Append_Node_To_List (N, Statements);

               --  2/
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Variable_Address
                    (Make_Member_Designator
                       (Defining_Identifier =>
                          Make_Defining_Identifier (MN (M_Data)),
                        Aggregate_Name =>
                          Make_Defining_Identifier (PN (P_Value)),
                        Is_Pointer => True)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List (CTU.Get_Data_Size (E, Is_Pointer => True),
                                    Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Unmarshall_Array), Parameters);
               Append_Node_To_List (N, Statements);

            when Data_Enum =>
               --  In C, an enumeration constant is equivalent to an
               --  int constant, therefore we unmarshall the value as
               --  a __po_hi_int32 parameter.
               --  XXX should be changed to an INT

               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Type_Conversion
                    (Make_Pointer_Type (RE (RE_Int32_T)),
                     Make_Defining_Identifier (PN (P_Value))),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Unmarshall_Int32), Parameters);
               Append_Node_To_List (N, Statements);

            when Data_None =>
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Value)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List (CTU.Get_Data_Size (E), Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N := Make_Call_Profile (RE (RE_Unmarshall_Array), Parameters);
               Append_Node_To_List (N, Statements);

            when others =>
               Parameters := New_List (CTN.K_Parameter_List);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Value)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Message)),
                  Parameters);
               Append_Node_To_List
                 (Make_Defining_Identifier (PN (P_Offset)),
                  Parameters);
               N :=
                 Make_Call_Profile
                   (Get_Unmarshall_Function_Name (E),
                    Parameters);
               Append_Node_To_List (N, Statements);
         end case;

         N :=
           Make_Function_Implementation
             (Unmarshall_Spec,
              Declarations,
              Statements);
         return N;
      end Make_Unmarshall_Type_Body;

      --------------------------------
      -- Get_Marshall_Function_Name --
      --------------------------------

      function Get_Marshall_Function_Name (E : Node_Id) return Node_Id is
         Data_Representation : constant Supported_Data_Representation :=
           Get_Data_Representation (E);
         Data_Size        : constant Size_Type          := Get_Data_Size (E);
         Actual_Data_Size : constant Unsigned_Long_Long :=
           To_Bytes (Data_Size);
         Number_Representation : constant Supported_Number_Representation :=
           Get_Number_Representation (E);
         Is_Signed : constant Boolean := Number_Representation = Signed;

      begin
         case Data_Representation is
            when Data_Boolean =>
               return (RE (RE_Marshall_Bool));

            when Data_Character =>
               return (RE (RE_Marshall_Char));

            when Data_Integer =>
               if Data_Size.S = 0 then
                  return (RE (RE_Marshall_Int));

               elsif Actual_Data_Size = 1 and then Is_Signed then
                  return (RE (RE_Marshall_Int8));

               elsif Actual_Data_Size = 1 and then not Is_Signed then
                  return (RE (RE_Marshall_Uint8));

               elsif Actual_Data_Size = 2 and then Is_Signed then
                  return (RE (RE_Marshall_Int16));

               elsif Actual_Data_Size = 2 and then not Is_Signed then
                  return (RE (RE_Marshall_Uint16));

               elsif Actual_Data_Size = 4 and then Is_Signed then
                  return (RE (RE_Marshall_Int32));

               elsif Actual_Data_Size = 4 and then not Is_Signed then
                  return (RE (RE_Marshall_Uint32));

               elsif Actual_Data_Size = 8 and then Is_Signed then
                  return (RE (RE_Marshall_Int64));

               elsif Actual_Data_Size = 8 and then not Is_Signed then
                  return (RE (RE_Marshall_Uint64));

               else
                  Display_Error ("Unsupported data size", Fatal => True);
                  return (No_Node);
               end if;

            when Data_Float | Data_Fixed =>
               if Data_Size.S = 0 then
                  return (RE (RE_Marshall_Float));

               elsif Actual_Data_Size = 4 then
                  return (RE (RE_Marshall_Float32));

               elsif Actual_Data_Size = 8 then
                  return (RE (RE_Marshall_Float64));

               else
                  Display_Error ("Unsupported data size", Fatal => True);
                  return (No_Node);
               end if;

            when others =>
               return (Map_C_Marshaller_Subprogram (E));
         end case;
      end Get_Marshall_Function_Name;

      ----------------------------------
      -- Get_Unmarshall_Function_Name --
      ----------------------------------

      function Get_Unmarshall_Function_Name (E : Node_Id) return Node_Id is
         Data_Representation : constant Supported_Data_Representation :=
           Get_Data_Representation (E);
         Data_Size        : constant Size_Type          := Get_Data_Size (E);
         Actual_Data_Size : constant Unsigned_Long_Long :=
           To_Bytes (Data_Size);
         Number_Representation : constant Supported_Number_Representation :=
           Get_Number_Representation (E);
         Is_Signed : constant Boolean := Number_Representation = Signed;

      begin
         case Data_Representation is
            when Data_Boolean =>
               return (RE (RE_Unmarshall_Bool));

            when Data_Character =>
               return (RE (RE_Unmarshall_Char));

            when Data_Integer =>
               if Data_Size.S = 0 then
                  return (RE (RE_Unmarshall_Int));

               elsif Actual_Data_Size = 1 and then Is_Signed then
                  return (RE (RE_Unmarshall_Int8));

               elsif Actual_Data_Size = 1 and then not Is_Signed then
                  return (RE (RE_Unmarshall_Uint8));

               elsif Actual_Data_Size = 2 and then Is_Signed then
                  return (RE (RE_Unmarshall_Int16));

               elsif Actual_Data_Size = 2 and then not Is_Signed then
                  return (RE (RE_Unmarshall_Uint16));

               elsif Actual_Data_Size = 4 and then Is_Signed then
                  return (RE (RE_Unmarshall_Int32));

               elsif Actual_Data_Size = 4 and then not Is_Signed then
                  return (RE (RE_Unmarshall_Uint32));

               elsif Actual_Data_Size = 8 and then Is_Signed then
                  return (RE (RE_Unmarshall_Int64));

               elsif Actual_Data_Size = 8 and then not Is_Signed then
                  return (RE (RE_Unmarshall_Uint64));

               else
                  Display_Error ("Unsupported data size", Fatal => True);
                  return (No_Node);
               end if;

            when Data_Float | Data_Fixed =>
               if Data_Size.S = 0 then
                  return (RE (RE_Unmarshall_Float));

               elsif Actual_Data_Size = 4 then
                  return (RE (RE_Unmarshall_Float32));

               elsif Actual_Data_Size = 8 then
                  return (RE (RE_Unmarshall_Float64));

               else
                  Display_Error ("Unsupported data size", Fatal => True);
                  return (No_Node);
               end if;

            when others =>
               return (Map_C_Marshaller_Subprogram (E, Is_Unmarshall => True));
         end case;
      end Get_Unmarshall_Function_Name;

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

            when CC_Subprogram =>
               Visit_Subprogram_Instance (E);

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         N : Node_Id;
         S : Node_Id;
      begin

         Add_Include (RH (RH_Types));
         Add_Include (RH (RH_PO_HI_Types));

         if No (Get_Handling (E, By_Name, H_C_Marshall_Body))
           and then Present
             (CTN.Marshaller_Node (Backend_Node (Identifier (E))))
         then
            N := Make_Marshall_Type_Body (E);
            Set_Handling (E, By_Name, H_C_Marshall_Body, N);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if No (Get_Handling (E, By_Name, H_C_Unmarshall_Body))
           and then Present
             (CTN.Unmarshaller_Node (Backend_Node (Identifier (E))))
         then
            N := Make_Unmarshall_Type_Body (E);
            Set_Handling (E, By_Name, H_C_Unmarshall_Body, N);
            Append_Node_To_List (N, CTN.Declarations (Current_File));
         end if;

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               if AAU.Is_Data (Corresponding_Instance (S)) then
                  Visit (Corresponding_Instance (S));
               end if;
               S := Next_Node (S);
            end loop;
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           CTN.Distributed_Application_Unit
             (CTN.Naming_Node (Backend_Node (Identifier (E))));
         P            : constant Node_Id := CTN.Entity (U);
         S            : Node_Id;
         N            : Node_Id;
         C            : Node_Id;
         I            : Node_Id;
         D            : Node_Id;
         F            : Node_Id;
         J            : Node_Id;
         Declarations : List_Id;
         Statements   : List_Id;
         Parameters   : List_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Marshallers_Source;

         Start_Recording_Handlings;

         Asn1_Marshall_Alternatives   := New_List (CTN.K_Alternatives_List);
         Asn1_Unmarshall_Alternatives := New_List (CTN.K_Alternatives_List);
         Marshall_Alternatives        := New_List (CTN.K_Alternatives_List);
         Unmarshall_Alternatives      := New_List (CTN.K_Alternatives_List);

         if not AAU.Is_Empty (Features (E)) then
            C := First_Node (Features (E));

            while Present (C) loop
               if Kind (C) = K_Port_Spec_Instance
                 and then not AAU.Is_Empty (Destinations (C))
               then
                  D := First_Node (Destinations (C));
                  while Present (D) loop
                     I := Item (D);

                     if Present (I)
                       and then Kind (I) = K_Port_Spec_Instance
                       and then not AAU.Is_Empty (Destinations (I))
                     then
                        F := First_Node (Get_Destination_Ports (I));
                        while Present (F) loop
                           J := Item (F);

                           if Present (J) then
                              Visit (Parent_Component (J));
                           end if;
                           F := Next_Node (F);
                        end loop;
                     end if;
                     D := Next_Node (D);
                  end loop;
               end if;

               C := Next_Node (C);
            end loop;
         end if;

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

         --  Make the global __po_hi_marshall_request function

         Declarations := New_List (CTN.K_Declaration_List);
         Statements   := New_List (CTN.K_Statement_List);

         Append_Node_To_List
           (Make_Variable_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Offset)),
               Used_Type           => RE (RE_Uint32_T)),
            Declarations);

         Append_Node_To_List
           (Make_Expression
              (Left_Expr  => Make_Defining_Identifier (PN (P_Offset)),
               Operator   => Op_Equal,
               Right_Expr => Make_Literal (CV.New_Int_Value (0, 1, 10))),
            Statements);

         Parameters := New_List (CTN.K_Parameter_List);
         Append_Node_To_List
           (Make_Member_Designator
              (Defining_Identifier => Make_Defining_Identifier (MN (M_Port)),
               Aggregate_Name => Make_Defining_Identifier (PN (P_Request)),
               Is_Pointer          => True),
            Parameters);

         Append_Node_To_List
           (Make_Defining_Identifier (PN (P_Message)),
            Parameters);

         N := Make_Call_Profile (RE (RE_Marshall_Port), Parameters);

         Append_Node_To_List (N, Statements);

         if not Is_Empty (Marshall_Alternatives) then
            N := Make_Switch_Alternative (No_List, No_List);
            Append_Node_To_List (N, Marshall_Alternatives);

            N :=
              Make_Switch_Statement
                (Expression =>
                   Make_Member_Designator
                     (Defining_Identifier =>
                        Make_Defining_Identifier (MN (M_Port)),
                      Aggregate_Name =>
                        Make_Defining_Identifier (VN (V_Request)),
                      Is_Pointer => True),
                 Alternatives => Marshall_Alternatives);

         else
            N := Message_Comment ("No alternative was declared");
         end if;

         Append_Node_To_List (N, Statements);

         N :=
           Make_Function_Implementation
             (Marshaller_Request_Spec,
              Declarations,
              Statements);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  Make the global __po_hi_unmarshall_request function

         Declarations := New_List (CTN.K_Declaration_List);
         Statements   := New_List (CTN.K_Statement_List);

         Append_Node_To_List
           (Make_Variable_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Offset)),
               Used_Type           => RE (RE_Uint32_T)),
            Declarations);

         Append_Node_To_List
           (Make_Expression
              (Left_Expr  => Make_Defining_Identifier (PN (P_Offset)),
               Operator   => Op_Equal,
               Right_Expr => Make_Literal (CV.New_Int_Value (0, 1, 10))),
            Statements);

         Parameters := New_List (CTN.K_Parameter_List);
         Append_Node_To_List
           (Make_Variable_Address
              (Make_Member_Designator
                 (Defining_Identifier =>
                    Make_Defining_Identifier (MN (M_Port)),
                  Aggregate_Name => Make_Defining_Identifier (PN (P_Request)),
                  Is_Pointer     => True)),
            Parameters);

         Append_Node_To_List
           (Make_Defining_Identifier (PN (P_Message)),
            Parameters);

         N := Make_Call_Profile (RE (RE_Unmarshall_Port), Parameters);

         Append_Node_To_List (N, Statements);

         if not Is_Empty (Unmarshall_Alternatives) then
            N := Make_Switch_Alternative (No_List, No_List);
            Append_Node_To_List (N, Unmarshall_Alternatives);

            N :=
              Make_Switch_Statement
                (Expression =>
                   Make_Member_Designator
                     (Defining_Identifier =>
                        Make_Defining_Identifier (MN (M_Port)),
                      Aggregate_Name =>
                        Make_Defining_Identifier (VN (V_Request)),
                      Is_Pointer => True),
                 Alternatives => Unmarshall_Alternatives);
         else
            N := Message_Comment ("No alternative was declared");
         end if;

         Append_Node_To_List (N, Statements);

         N :=
           Make_Function_Implementation
             (Unmarshaller_Request_Spec,
              Declarations,
              Statements);
         Append_Node_To_List (N, CTN.Declarations (Current_File));

         --  Unmark all the marked types

         if Use_ASN1_Deployment then
            if not Is_Empty (Asn1_Marshall_Alternatives) then
               N := Make_Switch_Alternative (No_List, No_List);
               Append_Node_To_List (N, Asn1_Marshall_Alternatives);

               N :=
                 Make_Switch_Statement
                   (Expression =>
                      Make_Member_Designator
                        (Defining_Identifier =>
                           Make_Defining_Identifier (MN (M_Port)),
                         Aggregate_Name =>
                           Make_Defining_Identifier (VN (V_Request)),
                         Is_Pointer => True),
                    Alternatives => Asn1_Marshall_Alternatives);
            else
               N := Message_Comment ("No alternative was declared");
            end if;

            N :=
              Make_Function_Implementation
                (Marshaller_Asn1_Request_Spec,
                 No_List,
                 Make_List_Id (N));
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            if not Is_Empty (Asn1_Unmarshall_Alternatives) then
               N := Make_Switch_Alternative (No_List, No_List);
               Append_Node_To_List (N, Asn1_Unmarshall_Alternatives);

               N :=
                 Make_Switch_Statement
                   (Expression =>
                      Make_Member_Designator
                        (Defining_Identifier =>
                           Make_Member_Designator
                             (Defining_Identifier =>
                                Make_Defining_Identifier (MN (M_Kind)),
                              Aggregate_Name =>
                                Make_Defining_Identifier (MN (M_Msg))),
                         Aggregate_Name =>
                           Make_Defining_Identifier (VN (V_Pkt)),
                         Is_Pointer => True),
                    Alternatives => Asn1_Unmarshall_Alternatives);
            else
               N := Message_Comment ("No alternative was declared");
            end if;

            N :=
              Make_Function_Implementation
                (Unmarshaller_Asn1_Request_Spec,
                 Make_List_Id (N),
                 No_List);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

         end if;

         Reset_Handlings;

         Pop_Entity; -- U
         Pop_Entity; -- P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         F : Node_Id;
      begin
         --  Declare all necessary data types

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  Display_Located_Error
                    (Loc (F),
                     "Port features in subprogram are not supported",
                     Fatal => True);
               end if;

               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;
      end Visit_Subprogram_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Push_Entity (C_Root);

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

         Pop_Entity; --  C_Root
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq                : Node_Id;
         Spg_Call                : Node_Id;
         F                       : Node_Id;
         N                       : Node_Id;
         D                       : Node_Id;
         Parameters              : List_Id;
         Marshall_Declarations   : List_Id;
         Unmarshall_Declarations : List_Id;
         Marshall_Statements     : List_Id;
         Unmarshall_Statements   : List_Id;
         Switch_Labels           : List_Id;
         Switch_Statements       : List_Id;
         Marshall_Spec           : Node_Id;
         Unmarshall_Spec         : Node_Id;
      begin
         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Ocarina.ME_AADL.AADL_Instances.Nodes.Is_Data (F)
               then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

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

         if Has_Ports (E) then
            F := First_Node (Features (E));
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then Ocarina.ME_AADL.AADL_Instances.Nodes.Is_Data (F)
                 and then No (Get_Handling (F, By_Node, H_C_Marshall_Body))
               then
                  Unmarshall_Spec :=
                    CTN.Unmarshaller_Node (Backend_Node (Identifier (F)));
                  Marshall_Spec :=
                    CTN.Marshaller_Node (Backend_Node (Identifier (F)));

                  D := Corresponding_Instance (F);

                  Marshall_Declarations   := New_List (CTN.K_Declaration_List);
                  Unmarshall_Declarations := New_List (CTN.K_Declaration_List);
                  Unmarshall_Statements   := New_List (CTN.K_Statement_List);
                  Marshall_Statements     := New_List (CTN.K_Statement_List);

                  Parameters := New_List (CTN.K_Parameter_List);

                  Append_Node_To_List
                    (Make_Member_Designator
                       (Defining_Identifier =>
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Defining_Identifier
                                 (Map_C_Enumerator_Name (F)),
                             Aggregate_Name =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Defining_Identifier
                                      (Map_C_Enumerator_Name (F)),
                                  Aggregate_Name =>
                                    Make_Defining_Identifier (MN (M_Vars)))),
                        Aggregate_Name =>
                          Make_Defining_Identifier (PN (P_Request)),
                        Is_Pointer => True),
                     Parameters);

                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Message)),
                     Parameters);

                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Offset)),
                     Parameters);

                  N :=
                    Make_Call_Profile
                      (Map_C_Marshaller_Subprogram (D),
                       Parameters);

                  Append_Node_To_List (N, Marshall_Statements);

                  Parameters := New_List (CTN.K_Parameter_List);
                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Member_Designator
                          (Defining_Identifier =>
                             Make_Member_Designator
                               (Defining_Identifier =>
                                  Make_Defining_Identifier
                                    (Map_C_Enumerator_Name (F)),
                                Aggregate_Name =>
                                  Make_Member_Designator
                                    (Defining_Identifier =>
                                       Make_Defining_Identifier
                                         (Map_C_Enumerator_Name (F)),
                                     Aggregate_Name =>
                                       Make_Defining_Identifier
                                         (MN (M_Vars)))),
                           Aggregate_Name =>
                             Make_Defining_Identifier (PN (P_Request)),
                           Is_Pointer => True)),
                     Parameters);
                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Message)),
                     Parameters);
                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Offset)),
                     Parameters);

                  N :=
                    Make_Call_Profile
                      (Map_C_Marshaller_Subprogram (D, Is_Unmarshall => True),
                       Parameters);

                  Append_Node_To_List (N, Unmarshall_Statements);

                  N :=
                    Make_Function_Implementation
                      (Marshall_Spec,
                       Marshall_Declarations,
                       Marshall_Statements);
                  Set_Handling (E, By_Name, H_C_Marshall_Body, N);
                  Set_Handling (F, By_Node, H_C_Marshall_Body, N);
                  Append_Node_To_List (N, CTN.Declarations (Current_File));

                  N :=
                    Make_Function_Implementation
                      (Unmarshall_Spec,
                       Unmarshall_Declarations,
                       Unmarshall_Statements);
                  Set_Handling (F, By_Name, H_C_Unmarshall_Body, N);

                  Append_Node_To_List (N, CTN.Declarations (Current_File));

                  --  Now, add alternatives to the main switches to use
                  --  our marshallers functions.

                  --  First, handle the generic marshallers.

                  Switch_Statements := New_List (CTN.K_Statement_List);
                  Switch_Labels     := New_List (CTN.K_Label_List);
                  Parameters        := New_List (CTN.K_Parameter_List);

                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Request)),
                     Parameters);

                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Message)),
                     Parameters);

                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Defining_Identifier (PN (P_Offset))),
                     Parameters);

                  N :=
                    Make_Call_Profile
                      (Map_C_Marshaller_Subprogram (F, Is_Request => True),
                       Parameters);

                  Append_Node_To_List (N, Switch_Statements);

                  Append_Node_To_List
                    (Make_Defining_Identifier (Map_C_Enumerator_Name (F)),
                     Switch_Labels);

                  N :=
                    Make_Switch_Alternative (Switch_Labels, Switch_Statements);
                  Append_Node_To_List (N, Marshall_Alternatives);

                  --  Then, handle the ASN1 marshallers
                  Switch_Statements := New_List (CTN.K_Statement_List);
                  Switch_Labels     := New_List (CTN.K_Label_List);
                  Parameters        := New_List (CTN.K_Parameter_List);

                  Append_Node_To_List
                    (Make_Expression
                       (Left_Expr =>
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    (Make_Defining_Identifier (MN (M_Kind))),
                                  Aggregate_Name =>
                                    Make_Defining_Identifier (MN (M_Msg))),
                             Aggregate_Name =>
                               Make_Defining_Identifier (VN (V_Pkt)),
                             Is_Pointer => True),
                        Operator   => Op_Equal,
                        Right_Expr =>
                          Make_Defining_Identifier
                            (Map_Port_Name_Present_For_Asn1 (F),
                             C_Conversion => False)),
                     Switch_Statements);

                  Append_Node_To_List
                    (Make_Member_Designator
                       (Defining_Identifier =>
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Member_Designator
                                      (Defining_Identifier =>
                                         Make_Defining_Identifier
                                           (Get_String_Name ("arr")),
                                       Aggregate_Name =>
                                         (Make_Defining_Identifier
                                            (Map_Port_Name_For_Asn1 (F)))),
                                  Aggregate_Name =>
                                    (Make_Defining_Identifier
                                       (Get_String_Name ("u")))),
                             Aggregate_Name =>
                               Make_Defining_Identifier (MN (M_Msg))),
                        Aggregate_Name =>
                          Make_Defining_Identifier (VN (V_Pkt)),
                        Is_Pointer => True),
                     Parameters);

                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Member_Designator
                          (Defining_Identifier =>
                             Make_Member_Designator
                               (Defining_Identifier =>
                                  Make_Member_Designator
                                    (Defining_Identifier =>
                                       (Make_Defining_Identifier
                                          (Map_C_Enumerator_Name (F))),
                                     Aggregate_Name =>
                                       (Make_Defining_Identifier
                                          (Map_C_Enumerator_Name (F)))),
                                Aggregate_Name =>
                                  Make_Defining_Identifier (MN (M_Vars))),
                           Aggregate_Name =>
                             Make_Defining_Identifier (VN (V_Request)),
                           Is_Pointer => True)),
                     Parameters);
                  Append_Node_To_List
                    (Get_Data_Size (Corresponding_Instance (F)),
                     Parameters);

                  N := Make_Call_Profile (RE (RE_Copy_Array), Parameters);

                  Append_Node_To_List (N, Switch_Statements);

                  Append_Node_To_List
                    (Make_Expression
                       (Left_Expr =>
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Member_Designator
                                      (Defining_Identifier =>
                                         Make_Member_Designator
                                           (Defining_Identifier =>
                                              Make_Defining_Identifier
                                                (Get_String_Name ("nCount"),
                                                 C_Conversion => False),
                                            Aggregate_Name =>
                                              (Make_Defining_Identifier
                                                 (Map_Port_Name_For_Asn1
                                                    (F)))),
                                       Aggregate_Name =>
                                         (Make_Defining_Identifier
                                            (Get_String_Name ("u")))),
                                  Aggregate_Name =>
                                    Make_Defining_Identifier (MN (M_Msg))),
                             Aggregate_Name =>
                               Make_Defining_Identifier (VN (V_Pkt)),
                             Is_Pointer => True),
                        Operator   => Op_Equal,
                        Right_Expr =>
                          Get_Data_Size (Corresponding_Instance (F))),
                     Switch_Statements);

                  Append_Node_To_List
                    (Make_Defining_Identifier (Map_C_Enumerator_Name (F)),
                     Switch_Labels);

                  N :=
                    Make_Switch_Alternative (Switch_Labels, Switch_Statements);
                  Append_Node_To_List (N, Asn1_Marshall_Alternatives);

                  --  Make the alternative for the global unmarshall_request
                  --  function.

                  Switch_Statements := New_List (CTN.K_Statement_List);
                  Switch_Labels     := New_List (CTN.K_Label_List);
                  Parameters        := New_List (CTN.K_Parameter_List);

                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Request)),
                     Parameters);

                  Append_Node_To_List
                    (Make_Defining_Identifier (PN (P_Message)),
                     Parameters);

                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Defining_Identifier (PN (P_Offset))),
                     Parameters);

                  N :=
                    Make_Call_Profile
                      (Map_C_Marshaller_Subprogram
                         (F,
                          Is_Request    => True,
                          Is_Unmarshall => True),
                       Parameters);

                  Append_Node_To_List (N, Switch_Statements);

                  Append_Node_To_List
                    (Make_Defining_Identifier (Map_C_Enumerator_Name (F)),
                     Switch_Labels);

                  N :=
                    Make_Switch_Alternative (Switch_Labels, Switch_Statements);
                  Append_Node_To_List (N, Unmarshall_Alternatives);

                  --  Then, handle the ASN1 unmarshallers
                  Switch_Statements := New_List (CTN.K_Statement_List);
                  Switch_Labels     := New_List (CTN.K_Label_List);
                  Parameters        := New_List (CTN.K_Parameter_List);

                  Append_Node_To_List
                    (Make_Expression
                       (Left_Expr =>
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Defining_Identifier (PN (P_Port)),
                             Aggregate_Name =>
                               Make_Defining_Identifier (VN (V_Request)),
                             Is_Pointer => True),
                        Operator   => Op_Equal,
                        Right_Expr =>
                          Make_Defining_Identifier
                            (Map_C_Enumerator_Name (F))),
                     Switch_Statements);

                  Append_Node_To_List
                    (Make_Variable_Address
                       (Make_Member_Designator
                          (Defining_Identifier =>
                             Make_Member_Designator
                               (Defining_Identifier =>
                                  Make_Member_Designator
                                    (Defining_Identifier =>
                                       (Make_Defining_Identifier
                                          (Map_C_Enumerator_Name (F))),
                                     Aggregate_Name =>
                                       (Make_Defining_Identifier
                                          (Map_C_Enumerator_Name (F)))),
                                Aggregate_Name =>
                                  Make_Defining_Identifier (MN (M_Vars))),
                           Aggregate_Name =>
                             Make_Defining_Identifier (VN (V_Request)),
                           Is_Pointer => True)),
                     Parameters);

                  Append_Node_To_List
                    (Make_Member_Designator
                       (Defining_Identifier =>
                          Make_Member_Designator
                            (Defining_Identifier =>
                               Make_Member_Designator
                                 (Defining_Identifier =>
                                    Make_Member_Designator
                                      (Defining_Identifier =>
                                         Make_Defining_Identifier
                                           (Get_String_Name ("arr")),
                                       Aggregate_Name =>
                                         (Make_Defining_Identifier
                                            (Map_Port_Name_For_Asn1 (F)))),
                                  Aggregate_Name =>
                                    (Make_Defining_Identifier
                                       (Get_String_Name ("u")))),
                             Aggregate_Name =>
                               Make_Defining_Identifier (MN (M_Msg))),
                        Aggregate_Name =>
                          Make_Defining_Identifier (VN (V_Pkt)),
                        Is_Pointer => True),
                     Parameters);

                  Append_Node_To_List
                    (Get_Data_Size (Corresponding_Instance (F)),
                     Parameters);

                  N := Make_Call_Profile (RE (RE_Copy_Array), Parameters);

                  Append_Node_To_List (N, Switch_Statements);

                  Append_Node_To_List
                    (Make_Defining_Identifier
                       (Map_Port_Name_Present_For_Asn1 (F),
                        C_Conversion => False),
                     Switch_Labels);

                  N :=
                    Make_Switch_Alternative (Switch_Labels, Switch_Statements);
                  Append_Node_To_List (N, Asn1_Unmarshall_Alternatives);

               end if;
               F := Next_Node (F);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Source_File;

end Ocarina.Backends.PO_HI_C.Marshallers;
