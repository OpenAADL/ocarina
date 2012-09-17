------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                 OCARINA.BACKENDS.PO_QOS_ADA.NAMESPACES                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--    Copyright (C) 2006-2009 Telecom ParisTech, 2010-2012 ESA & ISAE.      --
--                                                                          --
-- Ocarina  is free software;  you  can  redistribute  it and/or  modify    --
-- it under terms of the GNU General Public License as published by the     --
-- Free Software Foundation; either version 2, or (at your option) any      --
-- later version. Ocarina is distributed  in  the  hope  that it will be    --
-- useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details. You should have received  a copy of the --
-- GNU General Public License distributed with Ocarina; see file COPYING.   --
-- If not, write to the Free Software Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable to be   --
-- covered  by the  GNU  General  Public  License. This exception does not  --
-- however invalidate  any other reasons why the executable file might be   --
-- covered by the GNU Public License.                                       --
--                                                                          --
--                 Ocarina is maintained by the TASTE project               --
--                      (taste-users@lists.tuxfamily.org)                   --
--                                                                          --
------------------------------------------------------------------------------

with Utils;  use Utils;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.PO_QoS_Ada.Mapping;
with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_QoS_Ada.Namespaces is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.PO_QoS_Ada.Mapping;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.Ada_Values;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;
   package ADU renames Ocarina.Backends.Ada_Tree.Nutils;

   ------------------
   -- Package_Spec --
   ------------------

   package body Package_Spec is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Namespace_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);

      Current_Architecture_Instance : Node_Id;
      --  Points to the root of the instance tree

      Current_Process_Instance : Node_Id;
      --  Points to the current visited AADL process instance

      function Get_Ada_Unit (E : Node_Id) return Node_Id;
      pragma Inline (Get_Ada_Unit);
      --  Return the Ada unit inside which the data or subprogram
      --  component E has to be generated.

      function Protected_Type_Routines
        (E          : Node_Id;
         Components : List_Id)
        return List_Id;
      --  Declares the routines corresponding to a protected AADL data
      --  component. Components is pre-built list of Ada component
      --  declaration corresponding to the data subcomponents.

      ------------------
      -- Get_Ada_Unit --
      ------------------

      function Get_Ada_Unit (E : Node_Id) return Node_Id is
         N : Node_Id;
         P : Node_Id;
         U : Node_Id;
      begin
         pragma Assert (AAU.Is_Data (E) or else AAU.Is_Subprogram (E));

         N := Namespace (E);
         P := ADN.Namespaces_Node
           (Backend_Node
            (Bind_Two_Nodes
             (N, Current_Process_Instance)));
         U := ADN.Distributed_Application_Unit (P);

         return U;
      end Get_Ada_Unit;

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

            when K_Namespace_Instance =>
               Visit_Namespace_Instance (E);

            when others =>
               null;
         end case;
      end Visit;

      ---------------------------------
      -- Visit_Architecture_Instance --
      ---------------------------------

      procedure Visit_Architecture_Instance (E : Node_Id) is
      begin
         Current_Architecture_Instance := E;
         Visit (Root_System (E));
      end Visit_Architecture_Instance;

      ------------------------------
      -- Visit_Component_Instance --
      ------------------------------

      procedure Visit_Component_Instance (E : Node_Id) is
         Category : constant Component_Category
           := Get_Category_Of_Component (E);
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
         U                   : constant Node_Id := Get_Ada_Unit (E);
         Data_Representation : Supported_Data_Representation;
         N                   : Node_Id;
         S                   : Node_Id;
      begin
         --  Push the Ada unit correspoding to the AADL namespace

         Push_Entity (U);

         Set_Namespaces_Spec;

         --  Do not generate Ada type more than once

         if No (Get_Handling (E, By_Name, H_Ada_Namespaces_Spec)) then
            --  FIXME: For now, arrays are unsupported

            Data_Representation := Get_Data_Representation (E);

            case Data_Representation is
               when Data_Integer =>
                  N := Make_Full_Type_Declaration
                    (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                     Type_Definition     => Make_Derived_Type_Definition
                       (RE (RE_Integer)));

               when Data_Float =>
                  N := Make_Full_Type_Declaration
                    (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                     Type_Definition     => Make_Derived_Type_Definition
                       (RE (RE_Float_2)));

               when Data_Fixed =>
                  declare
                     Data_Digits : constant Unsigned_Long_Long
                       := Get_Data_Digits (E);
                     Data_Scale : constant Unsigned_Long_Long
                       := Get_Data_Scale (E);
                  begin
                     if Data_Digits /= 0 and then Data_Scale /= 0 then
                        N := Make_Full_Type_Declaration
                          (Defining_Identifier => Map_Ada_Defining_Identifier
                             (E),
                           Type_Definition     => Make_Decimal_Type_Definition
                             (Data_Digits,
                              Data_Scale));

                     else
                        if Data_Digits = 0 then
                           Display_Located_Error
                             (Loc (E),
                              "Missing the digit number of fixed point type!",
                              Fatal => True);
                        end if;

                        if Data_Scale = 0 then
                           Display_Located_Error
                             (Loc (E),
                              "Missing the scale of fixed point type!",
                              Fatal => True);
                        end if;
                     end if;
                  end;

               when Data_Boolean =>
                  N := Make_Full_Type_Declaration
                    (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                     Type_Definition     => Make_Derived_Type_Definition
                       (RE (RE_Boolean_2)));

               when Data_Character =>
                  N := Make_Full_Type_Declaration
                    (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                     Type_Definition     => Make_Derived_Type_Definition
                       (RE (RE_Character_2)));

               when Data_Wide_Character =>
                  N := Make_Full_Type_Declaration
                    (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                     Type_Definition     => Make_Derived_Type_Definition
                       (RE (RE_Wide_Character_2)));

               when Data_String =>
                  --  Bounded string data types require special
                  --  handling: we don't map string to the
                  --  'Standard.String' type since this is an
                  --  unconstrained type and would prevent us to build
                  --  data structures (buffers, records) with it. So
                  --  we use the Ada.Strings.Bounded packages

                  declare
                     Dimension : constant ULL_Array := Get_Dimension (E);
                  begin
                     N := Make_Package_Instantiation
                       (Defining_Identifier => Map_Ada_Package_Identifier (E),
                        Generic_Package     => RU
                          (RU_Ada_Strings_Bounded_Generic_Bounded_Length),
                        Parameter_List      => Make_List_Id
                          (Make_Literal
                           (New_Integer_Value
                            (Dimension (Dimension'First),
                             1,
                             10))));
                     Append_Node_To_List
                       (N, ADN.Visible_Part (Current_Package));

                     N := Make_Full_Type_Declaration
                       (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                        Type_Definition     => Make_Derived_Type_Definition
                          (Make_Selected_Component
                           (Map_Ada_Package_Identifier (E),
                            Make_Defining_Identifier
                            (TN (T_Bounded_String)))));
                  end;

               when Data_Wide_String =>
                  --  Bounded wide string data types require special
                  --  handling: we don't map string to the
                  --  'Standard.Wide_String' type since this is an
                  --  unconstrained type and would prevent us to build
                  --  data structures (buffers, records) with it. So
                  --  we use the Ada.Strings.Wide_Bounded packages

                  declare
                     Dimension : constant ULL_Array := Get_Dimension (E);
                  begin
                     N := Make_Package_Instantiation
                       (Defining_Identifier => Map_Ada_Package_Identifier (E),
                        Generic_Package     => RU
                          (RU_Ada_Strings_Wide_Bounded_Generic_Bounded_Length),
                        Parameter_List      => Make_List_Id
                          (Make_Literal
                           (New_Integer_Value
                            (Dimension (Dimension'First),
                             1,
                             10))));
                     Append_Node_To_List
                       (N, ADN.Visible_Part (Current_Package));

                     N := Make_Full_Type_Declaration
                       (Defining_Identifier => Map_Ada_Defining_Identifier (E),
                        Type_Definition     => Make_Derived_Type_Definition
                          (Make_Selected_Component
                           (Map_Ada_Package_Identifier (E),
                            Make_Defining_Identifier
                            (TN (T_Bounded_Wide_String)))));
                  end;
               when Data_Array =>
                  Display_Located_Error
                    (Loc (E),
                     "Bounded arrays not supported yet!",
                     Fatal => True);

               when Data_Struct | Data_With_Accessors =>
                  declare
                     Components : constant List_Id := New_List
                       (ADN.K_Component_List);
                     Conc_Proto : constant
                       Supported_Concurrency_Control_Protocol :=
                       Get_Concurrency_Protocol (E);
                     C          : Node_Id := First_Node (Subcomponents (E));
                     L          : List_Id;
                  begin
                     --  Build the component list

                     while Present (C) loop
                        --  Generate the Ada type corresponding to the
                        --  subcomponent.

                        Visit (Corresponding_Instance (C));

                        --  Make the record or private type component

                        N := Make_Component_Declaration
                          (Defining_Identifier => Map_Ada_Defining_Identifier
                             (C),
                           Subtype_Indication  => Map_Ada_Data_Type_Designator
                             (Corresponding_Instance (C)));
                        Append_Node_To_List (N, Components);

                        C := Next_Node (C);
                     end loop;

                     if Data_Representation = Data_Struct
                       and then Conc_Proto = Concurrency_NoneSpecified
                     then
                        --  Simple record type

                        N := Make_Full_Type_Declaration
                          (Defining_Identifier => Map_Ada_Defining_Identifier
                             (E),
                           Type_Definition     => Make_Record_Type_Definition
                             (Make_Record_Definition
                                (Components)));

                     elsif Conc_Proto = Concurrency_Protected_Access then
                        --  Protected type

                        L := Protected_Type_Routines (E, Components);

                        --  The first element of the list L is the
                        --  protected type declaration.

                        N := ADN.First_Node (L);
                     else
                        Display_Located_Error
                          (Loc (E),
                           "Unsupported concurrency protocol "
                           & Conc_Proto'Img,
                           Fatal => True);
                     end if;
                  end;

               when others =>
                  Display_Located_Error
                    (Loc (E), "Unsupported data type!", Fatal => True);
            end case;

            --  Mark the data type as being handled.

            Set_Handling (E, By_Name, H_Ada_Namespaces_Spec, N);

            --  In the case of a data type with accessor, visit the
            --  parameters of its features subprograms. It is
            --  important to do this *after* marking the type as
            --  handled, to avoid endless loops and *before* adding
            --  the type declaration to the package statements because
            --  the declaration order of type is important in Ada. In
            --  parallel, we visit the subprograms to create their
            --  specs

            if Data_Representation = Data_With_Accessors then
               S := First_Node (Features (E));

               while Present (S) loop
                  Visit (Corresponding_Instance (S));

                  S := Next_Node (S);
               end loop;
            end if;

            --  Append the type declaration to the package spec

            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));
         end if;

         --  Bind the type to its mapping

         Bind_AADL_To_Type_Definition
           (Identifier (E),
            Get_Handling (E, By_Name, H_Ada_Namespaces_Spec));

         Pop_Entity; --  U
      end Visit_Data_Instance;

      ------------------------------
      -- Visit_Namespace_Instance --
      ------------------------------

      procedure Visit_Namespace_Instance (E : Node_Id) is
         U : constant Node_Id := Map_QoS_Unit (E, Current_Process_Instance);
         pragma Unreferenced (U); --  Not read
      begin
         null;
      end Visit_Namespace_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         P : constant Node_Id := Map_QoS_Node (E);
         U : Node_Id;
         pragma Unreferenced (U);
         N : Node_Id;
         S : Node_Id;
      begin
         Current_Process_Instance := E;

         Push_Entity (P);

         --  It is important to push P before creating U

         U := Map_QoS_Unit (E);
         --  Do not push U. We just need to ensure the creation of the
         --  main subprogram node before the namespace node.

         --  We begin by visiting all the namespaces of the current
         --  architecture instance. Note that this is necessary for
         --  creating the empty packages corresponding to each
         --  namespace instance. It is important to do this after
         --  pushing the entity corresponding to the node at the top
         --  of the entity stack so that the namespace packages would
         --  be attched to the current node. Note also that generating
         --  an empty package for each namespace does not necessarily
         --  imply the generation of a source file. Only the packages
         --  that contain declarations (depending on the current node)
         --  will be generated.

         --  Visit the unnamed namespace of the current archirtecture
         --  instance.

         if Present (Unnamed_Namespace (Current_Architecture_Instance)) then
            Visit (Unnamed_Namespace (Current_Architecture_Instance));
         else
            --  This is an instantiation error

            Display_Located_Error
              (Loc (Current_Architecture_Instance),
               "This AADL architecture has no unnamed namespace",
               Fatal => True);
         end if;

         --  Visit all the namespace instances of the architecture
         --  instance.

         if
           not AAU.Is_Empty (AAN.Namespaces (Current_Architecture_Instance))
         then
            N := First_Node (AAN.Namespaces (Current_Architecture_Instance));

            while Present (N) loop
               Visit (N);
               N := Next_Node (N);
            end loop;
         end if;

         --  After creating the package declarations, we need to set,
         --  for each package declaration generating from a namespace
         --  instance, its corresponding parent package
         --  declaration. This has to be done *after* creating all
         --  package declarations because in AADL, we can declare a
         --  child package *before* it parent. This has to be done
         --  only for the namespaces corresponding to AADL packages
         --  (the unnamed namespace has no parent).

         if
           not AAU.Is_Empty (AAN.Namespaces (Current_Architecture_Instance))
         then
            N := First_Node (AAN.Namespaces (Current_Architecture_Instance));

            while Present (N) loop
               declare
                  Pkg_Dcl : constant Node_Id := ADN.Namespaces_Node
                    (Backend_Node
                     (Bind_Two_Nodes
                      (N,
                       Current_Process_Instance)));
                  Parent_Id : constant Node_Id := ADN.Parent_Unit_Name
                    (ADN.Defining_Identifier (Pkg_Dcl));
               begin
                  if Present (Pkg_Dcl) then
                     ADN.Set_Parent (Pkg_Dcl, Get_Bound_Package (Parent_Id));
                  end if;
               end;
               N := Next_Node (N);
            end loop;
         end if;

         --  Now that all the namespace packages are created, we visit
         --  recursively all the subcomponents of the process and map
         --  them to their corresponding packages.

         --  Start recording all handlings because we want to reset
         --  them for each node.

         Start_Recording_Handlings;

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         --  After all the entities are generated in the namespaces
         --  packages, we must ensure that, for each package P.Q, the
         --  parent spec P is generated even if P has no declarations.

         if
           not AAU.Is_Empty (AAN.Namespaces (Current_Architecture_Instance))
         then
            N := First_Node (AAN.Namespaces (Current_Architecture_Instance));

            while Present (N) loop
               declare
                  Pkg_Dcl     : constant Node_Id := ADN.Namespaces_Node
                    (Backend_Node
                     (Bind_Two_Nodes
                      (N,
                       Current_Process_Instance)));
                  Parent_Dcl  : Node_Id;
                  Parent_Spec : Node_Id;
               begin
                  Parent_Dcl := ADN.Parent (Pkg_Dcl);

                  while Present (Parent_Dcl) loop
                     Parent_Spec := ADN.Package_Specification (Parent_Dcl);

                     if ADU.Is_Empty (ADN.Visible_Part (Parent_Spec)) and then
                       ADU.Is_Empty (ADN.Private_Part (Parent_Spec))
                     then
                        Append_Node_To_List
                          (Message_Comment
                           ("This package specification has to be generated"
                            & " because it has at least one child package"),
                           ADN.Visible_Part (Parent_Spec));
                     end if;

                     Parent_Dcl := ADN.Parent (Parent_Dcl);
                  end loop;
               end;

               N := Next_Node (N);
            end loop;
         end if;

         --  Reset all the recorded handlings

         Reset_Handlings;

         Pop_Entity; --  P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         U        : constant Node_Id := Get_Ada_Unit (E);
         N        : Node_Id;
         F        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
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

         if No (Get_Handling (E, By_Name, H_Ada_Namespaces_Spec)) then
            --  Push the Ada unit correspoding to the AADL namespace

            Push_Entity (U);
            Set_Namespaces_Spec;

            N := Map_Ada_Subprogram_Spec (E);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  Mark the data type as being handled

            Set_Handling (E, By_Name, H_Ada_Namespaces_Spec, N);

            --  If the subprogram is hybrid, generate extra
            --  declarations.

            if Get_Subprogram_Kind (E) = Subprogram_Hybrid_Ada_95 then
               --  The status record type declaration

               N := Map_Ada_Subprogram_Status (E);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               --  The subprogram access type

               N := Map_Ada_Call_Seq_Access (E);
               Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

               Call_Seq := First_Node (Calls (E));

               while Present (Call_Seq) loop
                  --  For each call sequence create a subprogram spec

                  N := Map_Ada_Call_Seq_Subprogram_Spec (E, Call_Seq);
                  Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

                  Call_Seq := Next_Node (Call_Seq);
               end loop;
            end if;

            Pop_Entity; --  U
         end if;

         Bind_AADL_To_Subprogram
           (Identifier (E),
            Get_Handling (E, By_Name, H_Ada_Namespaces_Spec));

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
         D : Node_Id;
         S : Node_Id;
      begin
         D := Map_Distributed_Application (E);
         Push_Entity (D);

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity;
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         F        : Node_Id;
      begin
         --  Visit all the thread features

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then AAN.Is_Data (F)
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
      end Visit_Thread_Instance;

      -----------------------------
      -- Protected_Type_Routines --
      -----------------------------

      function Protected_Type_Routines
        (E          : Node_Id;
         Components : List_Id)
        return List_id
      is
         Routines : constant List_Id := New_List (ADN.K_Statement_List);
         N        : Node_Id;
         A        : Node_Id;
         Accessor : Name_Id;
      begin
         --  Declare the private type in the package visible part
         --  (which is the Routines list)

         N := Make_Full_Type_Declaration
           (Defining_Identifier => Map_Ada_Defining_Identifier (E),
            Type_Definition     => Make_Private_Type_Definition);
         Append_Node_To_List (N, Routines);

         --  Decalre the full type in the parivate part of the package

         --  Add mutex field to the component list

         N := Make_Object_Declaration
           (Defining_Identifier => Make_Defining_Identifier (VN (V_Mutex)),
            Object_Definition   => RE (RE_Mutex_Access));
         Append_Node_To_List (N, Components);

         N := Make_Full_Type_Declaration
           (Defining_Identifier => Map_Ada_Defining_Identifier (E),
            Type_Definition     => Make_Record_Type_Definition
              (Make_Record_Definition
               (Components)));
         Append_Node_To_List (N, ADN.Private_Part (Current_Package));

         --  Specification of the subprogram that builds one instance
         --  of the protected type.

         N := Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Build)),
            Make_List_Id
            (Make_Parameter_Specification
             (Make_Defining_Identifier (PN (P_Self)),
              Map_Ada_Defining_Identifier (E),
              Mode_Out)));
         Append_Node_To_List (N, Routines);
         Bind_AADL_To_Build (Identifier (E), N);

         --  For each field, create an accessor subprogram
         --  specification.

         A := First_Node (Subcomponents (E));

         while Present (A) loop

            --  Setter spec

            Accessor := Add_Prefix_To_Name
              ("Set_", To_Ada_Name (Name (Identifier (A))));

            N := Make_Subprogram_Specification
                 (Make_Defining_Identifier (Accessor),
                  Make_List_Id
                  (Make_Parameter_Specification
                   (Make_Defining_Identifier (PN (P_Self)),
                    Map_Ada_Defining_Identifier (E),
                    Mode_Inout),
                   Make_Parameter_Specification
                   (Make_Defining_Identifier (PN (P_Value)),
                    Map_Ada_Data_Type_Designator
                    (Corresponding_Instance (A)))));
            Append_Node_To_List (N, Routines);
            Bind_AADL_To_Set (Identifier (A), N);

            --  Getter spec

            Accessor := Add_Prefix_To_Name
              ("Get_", To_Ada_Name (Name (Identifier (A))));

            N := Make_Subprogram_Specification
                 (Make_Defining_Identifier (Accessor),
                  Make_List_Id
                  (Make_Parameter_Specification
                   (Make_Defining_Identifier (PN (P_Self)),
                    Map_Ada_Defining_Identifier (E),
                    Mode_In),
                   Make_Parameter_Specification
                   (Make_Defining_Identifier (PN (P_Value)),
                    Map_Ada_Data_Type_Designator
                    (Corresponding_Instance (A)),
                    Mode_Out)));
            Append_Node_To_List (N, Routines);
            Bind_AADL_To_Get (Identifier (A), N);

            A := Next_Node (A);
         end loop;

         return Routines;
      end Protected_Type_Routines;

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
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);

      Current_Process_Instance : Node_Id;
      --  Points to the current visited AADL process instance

      function Get_Ada_Unit (E : Node_Id) return Node_Id;
      pragma Inline (Get_Ada_Unit);
      --  Return the Ada unit inside which the data or subprogram
      --  component E has to be generated.

      function Protected_Type_Routines (E : Node_Id) return List_Id;
      --  Declares the routines corresponding to a protected AADL data
      --  component. Components is pre-built list of Ada component
      --  declaration corresponding to the data subcomponents.

      ------------------
      -- Get_Ada_Unit --
      ------------------

      function Get_Ada_Unit (E : Node_Id) return Node_Id is
         N : Node_Id;
         P : Node_Id;
         U : Node_Id;
      begin
         pragma Assert (AAU.Is_Data (E) or else AAU.Is_Subprogram (E));

         N := Namespace (E);
         P := ADN.Namespaces_Node
           (Backend_Node
            (Bind_Two_Nodes
             (N, Current_Process_Instance)));
         U := ADN.Distributed_Application_Unit (P);

         return U;
      end Get_Ada_Unit;

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
         Category : constant Component_Category
           := Get_Category_Of_Component (E);
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
         U                   : constant Node_Id := Get_Ada_Unit (E);
         Data_Representation : Supported_Data_Representation;
         N                   : Node_Id;
      begin
         --  Push the Ada unit correspoding to the AADL namespace

         Push_Entity (U);

         Set_Namespaces_Body;

         if No (Get_Handling (E, By_Name, H_Ada_Namespaces_Body)) then
            Data_Representation := Get_Data_Representation (E);

            case Data_Representation is
               when Data_With_Accessors | Data_Struct =>
                  declare
                     Conc_Proto : constant
                       Supported_Concurrency_Control_Protocol :=
                       Get_Concurrency_Protocol (E);
                     C          : Node_Id := First_Node (Subcomponents (E));
                     L          : List_Id;
                     S          : Node_Id;
                  begin
                     --  Visit the subcomponents

                     while Present (C) loop
                        Visit (Corresponding_Instance (C));
                        C := Next_Node (C);
                     end loop;

                     if Conc_Proto = Concurrency_Protected_Access then
                        --  Protected type

                        L := Protected_Type_Routines (E);

                        N := ADN.First_Node (L);

                        Append_Node_To_List
                          (N, ADN.Statements (Current_Package));
                     end if;

                     --  Mark the data type as being handled

                     Set_Handling (E, By_Name, H_Ada_Namespaces_Body, E);

                     --  Bodies of the subprogram features. It is
                     --  important to do this *after* marking the type
                     --  as being visited to avoid endless recursion.

                     if Data_Representation = Data_With_Accessors then
                        S := First_Node (Features (E));

                        while Present (S) loop
                           Visit (Corresponding_Instance (S));

                           S := Next_Node (S);
                        end loop;
                     end if;
                  end;

               when others =>
                  null;
            end case;
         end if;

         Pop_Entity; -- U
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Current_Process_Instance := E;

         --  Visit recursively all the subcomponents of the process
         --  and map them to their corresponding packages.

         --  Start recording all handlings because we want to reset
         --  them for each node.

         Start_Recording_Handlings;

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         --  Reset all the recorded handlings

         Reset_Handlings;
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         U        : constant Node_Id := Get_Ada_Unit (E);
         N        : Node_Id;
         F        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Declare all necessary data types

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         --  Generate the body of the subprogram

         if No (Get_Handling (E, By_Name, H_Ada_Namespaces_Body)) then
            Push_Entity (U);
            Set_Namespaces_Body;

            N := Map_Ada_Subprogram_Body (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  Mark the data type as being handled

            Set_Handling (E, By_Name, H_Ada_Namespaces_Body, N);

            --  If the subprogram is hybrid, generate extra entities

            if Get_Subprogram_Kind (E) = Subprogram_Hybrid_Ada_95 then
               Call_Seq := First_Node (Calls (E));

               while Present (Call_Seq) loop
                  --  For each call sequence create a subprogram body

                  N := Map_Ada_Call_Seq_Subprogram_Body (E, Call_Seq);
                  Append_Node_To_List (N, ADN.Statements (Current_Package));

                  Call_Seq := Next_Node (Call_Seq);
               end loop;
            end if;
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

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

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
         F        : Node_Id;
      begin
         --  Visit all the thread features

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance
                 and then AAN.Is_Data (F)
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
      end Visit_Thread_Instance;

      -----------------------------
      -- Protected_Type_Routines --
      -----------------------------

      function Protected_Type_Routines (E : Node_Id) return List_Id is
         Routines   : constant List_Id := New_List (ADN.K_Statement_List);
         N          : Node_Id;
         A          : Node_Id;
         Spec       : Node_Id;
         Statements : List_Id;
      begin
         --  Builder implementation

         Spec := ADN.Build_Node (Backend_Node (Identifier (E)));
         Statements := New_List (ADN.K_Statement_List);
         N := Make_Subprogram_Call
           (RE (RE_Create_2),
            Make_List_Id
            (Make_Selected_Component
             (Make_Defining_Identifier (PN (P_Self)),
              Make_Defining_Identifier (VN (V_Mutex)))));
         Append_Node_To_List (N, Statements);
         N := Make_Subprogram_Implementation (Spec, No_List, Statements);
         Append_Node_To_List (N, Routines);

         A := First_Node (Subcomponents (E));

         while Present (A) loop
            --  Setter implementation

            Spec := ADN.Set_Node (Backend_Node (Identifier (A)));

            Statements := New_List (ADN.K_Statement_List);

            N := Make_Subprogram_Call
              (RE (RE_Enter),
               Make_List_Id
               (Make_Selected_Component
                (Make_Defining_Identifier (PN (P_Self)),
                 Make_Defining_Identifier (VN (V_Mutex)))));
            Append_Node_To_List (N, Statements);

            N := Make_Assignment_Statement
              (Make_Selected_Component
               (Make_Defining_Identifier (PN (P_Self)),
                Map_Ada_Defining_Identifier (A)),
               Make_Defining_Identifier (PN (P_Value)));
            ADU.Append_Node_To_List (N, Statements);

            N := Make_Subprogram_Call
              (RE (RE_Leave),
               Make_List_Id
               (Make_Selected_Component
                (Make_Defining_Identifier (PN (P_Self)),
                 Make_Defining_Identifier (VN (V_Mutex)))));
            Append_Node_To_List (N, Statements);

            N := Make_Subprogram_Implementation (Spec, No_List, Statements);
            ADU.Append_Node_To_List (N, Routines);

            --  Getter implementation

            Spec := ADN.Get_Node (Backend_Node (Identifier (A)));

            Statements := New_List (ADN.K_Statement_List);

            N := Make_Subprogram_Call
              (RE (RE_Enter),
               Make_List_Id
               (Make_Selected_Component
                (Make_Defining_Identifier (PN (P_Self)),
                 Make_Defining_Identifier (VN (V_Mutex)))));
            Append_Node_To_List (N, Statements);

            N := Make_Assignment_Statement
              (Make_Defining_Identifier (PN (P_Value)),
               Make_Selected_Component
               (Make_Defining_Identifier (PN (P_Self)),
                Map_Ada_Defining_Identifier (A)));
            ADU.Append_Node_To_List (N, Statements);

            N := Make_Subprogram_Call
              (RE (RE_Leave),
               Make_List_Id
               (Make_Selected_Component
                (Make_Defining_Identifier (PN (P_Self)),
                 Make_Defining_Identifier (VN (V_Mutex)))));
            Append_Node_To_List (N, Statements);

            N := Make_Subprogram_Implementation (Spec, No_List, Statements);
            ADU.Append_Node_To_List (N, Routines);

            A := Next_Node (A);
         end loop;

         return Routines;
      end Protected_Type_Routines;

   end Package_Body;

end Ocarina.Backends.PO_QoS_Ada.Namespaces;
