------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.PO_HI_ADA.MARSHALLERS                   --
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
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_HI_Ada.Runtime;
with Ocarina.Backends.PO_HI_Ada.Mapping;

package body Ocarina.Backends.PO_HI_Ada.Marshallers is

   use Ocarina.Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_HI_Ada.Runtime;
   use Ocarina.Backends.PO_HI_Ada.Mapping;

   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;
   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;

   function Get_Marshalled_Type (E : Node_Id) return Node_Id;
   --  Return depending on the category of component E, the type that
   --  should be used in procedure Marshall and Unmarshall.

   -------------------------
   -- Get_Marshalled_Type --
   -------------------------

   function Get_Marshalled_Type (E : Node_Id) return Node_Id is
      Category : constant Component_Category := Get_Category_Of_Component (E);
      T        : Node_Id;
   begin
      case Category is
         when CC_Thread =>
            T :=
              Extract_Designator
                (ADN.Port_Interface_Node (Backend_Node (Identifier (E))));

         when CC_Data =>
            T :=
              Extract_Designator
                (ADN.Type_Definition_Node (Backend_Node (Identifier (E))));

         when others =>
            raise Program_Error
              with "Cannot generate Marshall procedure" &
              " for a " &
              Component_Category'Image (Category);
      end case;

      return T;
   end Get_Marshalled_Type;

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

      function Marshall_Spec (E : Node_Id) return Node_Id;
      --  Creates a spec for a Marshall procedure for a data type
      --  generated from an AADL data component, a Thread_Port type
      --  generated from an AADL thread component or a Port_Type type
      --  generated from an AADL process component.

      function Unmarshall_Spec (E : Node_Id) return Node_Id;
      --  Same as above but with an Unmarshall procedure

      -------------------
      -- Marshall_Spec --
      -------------------

      function Marshall_Spec (E : Node_Id) return Node_Id is
         N       : Node_Id;
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
      begin
         --  The 'Data' parameter

         N :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Data)),
              Get_Marshalled_Type (E),
              Mode_In);
         Append_Node_To_List (N, Profile);

         --  The 'Message' parameter

         N :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Message)),
              RE (RE_Message_Type),
              Mode_Inout);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Subprogram_Specification
             (Make_Defining_Identifier (SN (S_Marshall)),
              Profile);

         return N;
      end Marshall_Spec;

      ---------------------
      -- Unmarshall_Spec --
      ---------------------

      function Unmarshall_Spec (E : Node_Id) return Node_Id is
         Category : constant Component_Category :=
           Get_Category_Of_Component (E);
         N       : Node_Id;
         Profile : constant List_Id := New_List (ADN.K_Parameter_Profile);
      begin
         --  If we deal with a thread, there is an extra parameter
         --  correspodning to the <T>_Ports enumerator useful for the
         --  marshalling.

         if Category = CC_Thread then
            N :=
              Make_Parameter_Specification
                (Make_Defining_Identifier (PN (P_Port)),
                 Extract_Designator
                   (ADN.Port_Enumeration_Node (Backend_Node (Identifier (E)))),
                 Mode_In);
            Append_Node_To_List (N, Profile);
         end if;

         --  The 'Data' parameter

         N :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Data)),
              Get_Marshalled_Type (E),
              Mode_Out);
         Append_Node_To_List (N, Profile);

         --  The 'Message' parameter

         N :=
           Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Message)),
              RE (RE_Message_Type),
              Mode_Inout);
         Append_Node_To_List (N, Profile);

         N :=
           Make_Subprogram_Specification
             (Make_Defining_Identifier (SN (S_Unmarshall)),
              Profile);

         return N;
      end Unmarshall_Spec;

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
      begin
         --  Do not generate Marshallers more than once per node

         if No (Get_Handling (E, By_Name, H_Ada_Marshallers_Spec)) then
            --  Marshallers are generated only for types which can
            --  sent through data ports and event data ports.

            if Get_Data_Representation (E) /= Data_With_Accessors then

               N :=
                 Message_Comment
                   ("Marshallers for DATA type " &
                    Get_Name_String (Name (Identifier (E))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Marshall procedure

               N := Marshall_Spec (E);
               Bind_AADL_To_Marshall (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Unmarshall procedure

               N := Unmarshall_Spec (E);
               Bind_AADL_To_Unmarshall (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Mark the data type as being handled.

               Set_Handling
                 (E,
                  By_Name,
                  H_Ada_Marshallers_Spec,
                  Identifier (E));
            end if;
         else
            --  Do the tree bindings only

            Bind_AADL_To_Marshall
              (Identifier (E),
               ADN.Marshall_Node
                 (Backend_Node
                    (Get_Handling (E, By_Name, H_Ada_Marshallers_Spec))));

            Bind_AADL_To_Unmarshall
              (Identifier (E),
               ADN.Unmarshall_Node
                 (Backend_Node
                    (Get_Handling (E, By_Name, H_Ada_Marshallers_Spec))));
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         S : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Marshallers_Spec;

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
         N : Node_Id;
         F : Node_Id;
      begin
         if Has_Ports (E) then
            --  Generate marshallers for the Port_Type enumeration

            if No
                (Get_Handling
                   (Corresponding_Declaration (E),
                    By_Node,
                    H_Ada_Marshallers_Spec))
            then
               Set_Handling
                 (Corresponding_Declaration (E),
                  By_Node,
                  H_Ada_Marshallers_Spec,
                  E);

               N :=
                 Message_Comment
                   ("Marshallers for interface type of thread " &
                    Get_Name_String (Name (Identifier (E))));
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Marshall procedure

               N := Marshall_Spec (E);
               Bind_AADL_To_Marshall (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  Unmarshall procedure

               N := Unmarshall_Spec (E);
               Bind_AADL_To_Unmarshall (Identifier (E), N);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
            else
               declare
                  Found : constant Node_Id :=
                    Get_Handling
                      (Corresponding_Declaration (E),
                       By_Node,
                       H_Ada_Marshallers_Spec);
                  BE : constant Node_Id := Backend_Node (Identifier (Found));
               begin
                  Bind_AADL_To_Marshall
                    (Identifier (E),
                     ADN.Marshall_Node (BE));
                  Bind_AADL_To_Unmarshall
                    (Identifier (E),
                     ADN.Unmarshall_Node (BE));
               end;
            end if;

         end if;

         --  The only data that need to be marshalled or unmarshalled
         --  is the data that is meant to be sent between threads
         --  (locally or remotly). So we visit only thread features.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then AAN.Is_Data (F) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
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

      function Marshall_Implementation (E : Node_Id) return Node_Id;
      --  Creates an implementation for a Marshall procedure

      function Unmarshall_Implementation (E : Node_Id) return Node_Id;
      --  Same as above but with an Unmarshall procedure

      function Marshallers_Instantiation (E : Node_Id) return Node_Id;
      --  Creates a generic instantiation for the Marshallers_G
      --  package corresponding to the node E.

      -----------------------------
      -- Marshall_Implementation --
      -----------------------------

      function Marshall_Implementation (E : Node_Id) return Node_Id is
         Spec : constant Node_Id :=
           ADN.Marshall_Node (Backend_Node (Identifier (E)));
         N : Node_Id;
      begin
         --  The marshallers for data component are simple renaming of
         --  intantiated ones. Fo thread components, the body is more
         --  complex.

         if not AAU.Is_Thread (E) then
            N :=
              Make_Selected_Component
                (Make_Defining_Identifier (Map_Marshallers_Name (E)),
                 Make_Defining_Identifier (SN (S_Marshall)));

            N :=
              Make_Subprogram_Specification
                (Defining_Identifier => ADN.Defining_Identifier (Spec),
                 Parameter_Profile   => ADN.Parameter_Profile (Spec),
                 Return_Type         => ADN.Return_Type (Spec),
                 Renamed_Subprogram  => N);
         else
            declare
               Alternatives : constant List_Id := New_List (ADN.K_List_Id);
               Statements   : List_Id;
               Declarations : List_Id;
               F            : Node_Id;
               Has_Data     : Boolean          := False;
            begin
               --  Check if the thread conrains at least one OUT DATA
               --  port, other wise, there is nothing to marshall

               F := First_Node (Features (E));

               while Present (F) loop
                  if Kind (F) = K_Port_Spec_Instance
                    and then Is_Out (F)
                    and then AAN.Is_Data (F)
                  then
                     Has_Data := True;
                     exit;
                  end if;

                  F := Next_Node (F);
               end loop;

               if Has_Data then
                  --  If we are at this point, we are sure that the
                  --  thread contains at least one data port. We must
                  --  also take in account the presence of pure event
                  --  ports, bu adding null case alternative for them

                  F := First_Node (Features (E));

                  while Present (F) loop
                     if Kind (F) = K_Port_Spec_Instance
                       and then Is_Out (F)
                     then
                        --  The statements (if any)

                        Statements := New_List (ADN.K_Statement_List);

                        if AAN.Is_Data (F) then
                           N :=
                             Make_Subprogram_Call
                               (Extract_Designator
                                  (ADN.Marshall_Node
                                     (Backend_Node
                                        (Identifier
                                           (Corresponding_Instance (F))))),
                                Make_List_Id
                                  (Make_Selected_Component
                                     (Make_Designator (PN (P_Data)),
                                      Make_Defining_Identifier
                                        (Map_Ada_Component_Name (F))),
                                   Make_Defining_Identifier (PN (P_Message))));
                           Append_Node_To_List (N, Statements);
                        else
                           Append_Node_To_List
                             (Make_Null_Statement,
                              Statements);
                        end if;

                        N :=
                          Make_Elsif_Statement
                            (Make_Expression
                               (Make_Selected_Component
                                  (Make_Designator (PN (P_Data)),
                                   Make_Designator (PN (P_Port))),
                                Op_Equal,
                                Extract_Enumerator (F, False)),
                             Statements);
                        Append_Node_To_List (N, Alternatives);
                     end if;

                     F := Next_Node (F);
                  end loop;

                  declare
                     Declarations : constant List_Id :=
                       New_List (ADN.K_Declaration_List);
                     Elsif_Statements : constant List_Id :=
                       New_List (ADN.K_List_Id);
                  begin
                     N :=
                       Make_Used_Package
                         (RU (RU_PolyORB_HI_Generated_Activity));
                     Append_Node_To_List (N, Declarations);

                     ADN.Set_First_Node
                       (Elsif_Statements,
                        ADN.Next_Node (ADN.First_Node (Alternatives)));

                     N :=
                       Make_If_Statement
                         (Condition =>
                            ADN.Condition (ADN.First_Node (Alternatives)),
                          Then_Statements =>
                            ADN.Then_Statements
                              (ADN.First_Node (Alternatives)),
                          Elsif_Statements => Elsif_Statements);

                     N :=
                       Make_Subprogram_Implementation
                         (Spec,
                          Declarations,
                          Make_List_Id (N));
                  end;

               else
                  Declarations := New_List (ADN.K_Declaration_List);

                  --  Add a pragma unreferenced for parameters

                  N :=
                    Make_Pragma_Statement
                      (Pragma_Unreferenced,
                       Make_List_Id
                         (Make_Defining_Identifier (PN (P_Message))));
                  Append_Node_To_List (N, Declarations);

                  N :=
                    Make_Pragma_Statement
                      (Pragma_Unreferenced,
                       Make_List_Id (Make_Defining_Identifier (PN (P_Data))));
                  Append_Node_To_List (N, Declarations);

                  N :=
                    Make_Subprogram_Implementation
                      (Spec,
                       Declarations,
                       No_List);
               end if;
            end;
         end if;

         return N;
      end Marshall_Implementation;

      -------------------------------
      -- Unmarshall_Implementation --
      -------------------------------

      function Unmarshall_Implementation (E : Node_Id) return Node_Id is
         Spec : constant Node_Id :=
           ADN.Unmarshall_Node (Backend_Node (Identifier (E)));
         N : Node_Id;
      begin
         --  The marshallers for data component are simple renaming of
         --  intantiated ones. Fo thread components, the body is more
         --  complex.

         if not AAU.Is_Thread (E) then
            N :=
              Make_Selected_Component
                (Make_Defining_Identifier (Map_Marshallers_Name (E)),
                 Make_Defining_Identifier (SN (S_Unmarshall)));

            N :=
              Make_Subprogram_Specification
                (Defining_Identifier => ADN.Defining_Identifier (Spec),
                 Parameter_Profile   => ADN.Parameter_Profile (Spec),
                 Return_Type         => ADN.Return_Type (Spec),
                 Renamed_Subprogram  => N);
         else
            declare
               Alternatives : constant List_Id := New_List (ADN.K_List_Id);
               Declarations : constant List_Id :=
                 New_List (ADN.K_Declaration_List);
               Statements  : List_Id;
               Aggregates  : List_Id;
               Ref_Message : Boolean := False;
               F           : Node_Id;
            begin
               --  If the thread has not IN port, there is nothing to
               --  unmarshall

               if Has_In_Ports (E) then
                  --  If we are at this point, we are sure that the
                  --  thread contains at least one port

                  F := First_Node (Features (E));

                  while Present (F) loop
                     if Kind (F) = K_Port_Spec_Instance and then Is_In (F) then
                        --  The record aggregate

                        Aggregates := New_List (ADN.K_Statement_List);

                        N :=
                          Make_Component_Association
                            (Make_Defining_Identifier (PN (P_Port)),
                             Extract_Enumerator (F, False));
                        Append_Node_To_List (N, Aggregates);

                        --  The statements (if any)

                        Statements := New_List (ADN.K_Statement_List);

                        if AAN.Is_Data (F) then
                           --  Declare the temporary variable

                           N :=
                             Make_Object_Declaration
                               (Defining_Identifier =>
                                  Make_Defining_Identifier
                                    (Map_Ada_Component_Name (F)),
                                Object_Definition =>
                                  Map_Ada_Data_Type_Designator
                                    (Corresponding_Instance (F)));
                           Append_Node_To_List (N, Declarations);

                           N :=
                             Make_Subprogram_Call
                               (Extract_Designator
                                  (ADN.Unmarshall_Node
                                     (Backend_Node
                                        (Identifier
                                           (Corresponding_Instance (F))))),
                                Make_List_Id
                                  (Make_Defining_Identifier
                                     (Map_Ada_Component_Name (F)),
                                   Make_Defining_Identifier (PN (P_Message))));
                           Append_Node_To_List (N, Statements);

                           --  Append the extra aggregate

                           N :=
                             Make_Component_Association
                               (Make_Defining_Identifier
                                  (Map_Ada_Component_Name (F)),
                                Make_Defining_Identifier
                                  (Map_Ada_Component_Name (F)));
                           Append_Node_To_List (N, Aggregates);

                           --  Mark the message formal parameter as
                           --  being referenced.

                           Ref_Message := True;
                        end if;

                        --  Assign the port value

                        N :=
                          Make_Assignment_Statement
                            (Make_Defining_Identifier (PN (P_Data)),
                             Make_Qualified_Expression
                               (Extract_Designator
                                  (ADN.Port_Interface_Node
                                     (Backend_Node (Identifier (E)))),
                                Make_Record_Aggregate (Aggregates)));
                        Append_Node_To_List (N, Statements);

                        N :=
                          Make_Elsif_Statement
                            (Make_Expression
                               (Make_Defining_Identifier (PN (P_Port)),
                                Op_Equal,
                                Extract_Enumerator (F, False)),
                             Statements);
                        Append_Node_To_List (N, Alternatives);
                     end if;

                     F := Next_Node (F);
                  end loop;

                  if not Ref_Message then
                     --  Add a pragma unreferenced for 'Message'

                     N :=
                       Make_Pragma_Statement
                         (Pragma_Unreferenced,
                          Make_List_Id
                            (Make_Defining_Identifier (PN (P_Message))));
                     Append_Node_To_List (N, Declarations);
                  end if;

                  declare
                     --  Declarations : constant List_Id
                     --    := New_List (ADN.K_Declaration_List);
                     Elsif_Statements : constant List_Id :=
                       New_List (ADN.K_List_Id);
                  begin
                     N :=
                       Make_Used_Package
                         (RU (RU_PolyORB_HI_Generated_Activity));
                     Append_Node_To_List (N, Declarations);

                     ADN.Set_First_Node
                       (Elsif_Statements,
                        ADN.Next_Node (ADN.First_Node (Alternatives)));

                     N :=
                       Make_If_Statement
                         (Condition =>
                            ADN.Condition (ADN.First_Node (Alternatives)),
                          Then_Statements =>
                            ADN.Then_Statements
                              (ADN.First_Node (Alternatives)),
                          Elsif_Statements => Elsif_Statements);

                     N :=
                       Make_Subprogram_Implementation
                         (Spec,
                          Declarations,
                          Make_List_Id (N));
                  end;

               else
                  --  Add a pragma unreferenced for parameters

                  N :=
                    Make_Pragma_Statement
                      (Pragma_Unreferenced,
                       Make_List_Id (Make_Defining_Identifier (PN (P_Port))));
                  Append_Node_To_List (N, Declarations);

                  N :=
                    Make_Pragma_Statement
                      (Pragma_Unreferenced,
                       Make_List_Id
                         (Make_Defining_Identifier (PN (P_Message))));
                  Append_Node_To_List (N, Declarations);

                  N :=
                    Make_Pragma_Statement
                      (Pragma_Unreferenced,
                       Make_List_Id (Make_Defining_Identifier (PN (P_Data))));
                  Append_Node_To_List (N, Declarations);

                  N :=
                    Make_Subprogram_Implementation
                      (Spec,
                       Declarations,
                       No_List);
               end if;
            end;
         end if;

         return N;
      end Unmarshall_Implementation;

      -------------------------------
      -- Marshallers_Instantiation --
      -------------------------------

      function Marshallers_Instantiation (E : Node_Id) return Node_Id is
      begin
         return Make_Package_Instantiation
           (Defining_Identifier =>
              Make_Defining_Identifier (Map_Marshallers_Name (E)),
            Generic_Package =>
              RU (RU_PolyORB_HI_Marshallers_G, Elaborated => True),
            Parameter_List  => Make_List_Id (Get_Marshalled_Type (E)));
      end Marshallers_Instantiation;

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
      begin
         --  Do not generate Marshallers more than once per node

         if No (Get_Handling (E, By_Name, H_Ada_Marshallers_Body)) then
            --  Marshallers are generated only for types which can
            --  sent through data ports and event data ports.

            if Get_Data_Representation (E) /= Data_With_Accessors then

               N :=
                 Message_Comment
                   ("Marshallers for DATA type " &
                    Get_Name_String (Name (Identifier (E))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Package instantiation

               N := Marshallers_Instantiation (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Marshall procedure

               N := Marshall_Implementation (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Unmarshall procedure

               N := Unmarshall_Implementation (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Mark the data type as being handled.

               Set_Handling
                 (E,
                  By_Name,
                  H_Ada_Marshallers_Body,
                  Identifier (E));
            end if;
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id :=
           ADN.Distributed_Application_Unit
             (ADN.Deployment_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         S : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Marshallers_Body;

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
         N : Node_Id;
         F : Node_Id;
      begin
         if Has_Ports (E) then
            if No
                (Get_Handling
                   (Corresponding_Declaration (E),
                    By_Node,
                    H_Ada_Marshallers_Body))
            then
               Set_Handling
                 (Corresponding_Declaration (E),
                  By_Node,
                  H_Ada_Marshallers_Body,
                  E);

               --  Generate marshallers for the Port_Type enumeration

               N :=
                 Message_Comment
                   ("Marshallers for interface type of thread " &
                    Get_Name_String (Name (Identifier (E))));
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Marshall procedure

               N := Marshall_Implementation (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));

               --  Unmarshall procedure

               N := Unmarshall_Implementation (E);
               Append_Node_To_List (N, ADN.Statements (Current_Package));
            end if;
         end if;

         --  The only data that need to be marshalled or unmarshalled
         --  is the data that is meant to be sent between threads
         --  (locally or remotly). So we visit only thread features.

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance and then AAN.Is_Data (F) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;
      end Visit_Thread_Instance;

   end Package_Body;

end Ocarina.Backends.PO_HI_Ada.Marshallers;
