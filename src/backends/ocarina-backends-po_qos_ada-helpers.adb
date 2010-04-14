------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A . H E L P E R S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2006-2009, GET-Telecom Paris.                --
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
--                 Ocarina is maintained by the Ocarina team                --
--                       (ocarina-users@listes.enst.fr)                     --
--                                                                          --
------------------------------------------------------------------------------

with Namet;
with Utils;  use Utils;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_QoS_Ada.Mapping;
with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_QoS_Ada.Helpers is

   use Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_QoS_Ada.Mapping;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.Ada_Values;

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
      procedure Visit_Subprogram_Instance (E : Node_Id);

      function TypeCode_Declaration (E : Node_Id) return Node_Id;
      --  Makes a TypeCode variable declaration corresponding to the
      --  type definition node given as parameter

      function From_Any_Spec (E : Node_Id) return Node_Id;
      --  Makes a spec for the 'From_Any' function corresponding to
      --  the type definition given as a parameter

      function To_Any_Spec (E : Node_Id) return Node_Id;
      --  Makes a spec for the 'To_Any' function corresponding to
      --  the type definition given as a parameter

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
         N : Node_Id;
         S : Node_Id;
      begin
         --  Protected objects with accessors are not destined to
         --  network transfert. They do not have helper routines.

         if Get_Data_Representation (E) = Data_With_Accessors then
            return;
         end if;

         --  Fixed point types are not supported yet

         if Get_Data_Representation (E) = Data_Fixed
           or else Get_Data_Representation (E) = Data_Array
         then
            Display_Located_Error
              (AAN.Loc (E),
               "Helper generation for this type not supported yet",
               Fatal => True);
         end if;

         Set_Helpers_Spec;

         if No (Get_Handling (E, By_Name, H_Ada_Helpers_Spec)) then
            N := Message_Comment
              ("Data type "
               & Get_Name_String (AAU.Compute_Full_Name_Of_Instance (E)));
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  For each data type definition, we generate the
            --  following entities in the Helpers package spec:

            --  1) The TypeCode variable

            N := TypeCode_Declaration (E);
            Bind_AADL_To_TypeCode (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  2) The From_Any function spec

            N := From_Any_Spec (E);
            Bind_AADL_To_From_Any (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            --  2) The To_Any function spec

            N := To_Any_Spec (E);
            Bind_AADL_To_To_Any (Identifier (E), N);
            Append_Node_To_List (N, ADN.Visible_Part (Current_Package));

            Set_Handling (E, By_Name, H_Ada_Helpers_Spec, E);

            --  Visit the subcomponents of E (if any)

            if not AAU.Is_Empty (Subcomponents (E)) then
               S := First_Node (Subcomponents (E));

               while Present (S) loop
                  Visit (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         else
            --  This type has already been handled, take the bindings
            --  correspodning to the first handled instance

            Bind_AADL_To_TypeCode
              (Identifier (E),
               ADN.TypeCode_Node
               (Backend_Node
                (Identifier
                 (Get_Handling
                  (E,
                   By_Name,
                   H_Ada_Helpers_Spec)))));

            Bind_AADL_To_From_Any
              (Identifier (E),
               ADN.From_Any_Node
               (Backend_Node
                (Identifier
                 (Get_Handling
                  (E,
                   By_Name,
                   H_Ada_Helpers_Spec)))));

            Bind_AADL_To_To_Any
              (Identifier (E),
               ADN.To_Any_Node
               (Backend_Node
                (Identifier
                 (Get_Handling
                  (E,
                   By_Name,
                   H_Ada_Helpers_Spec)))));
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id := ADN.Distributed_Application_Unit
           (ADN.Helpers_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         S : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);

         --  Start recording all handlings because we want to reset
         --  them for each node.

         Start_Recording_Handlings;

         --  Visit recursively all the subcomponents of the process

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

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         F        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Visit all data types

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
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

      --------------------------
      -- TypeCode_Declaration --
      --------------------------

      function TypeCode_Declaration (E : Node_Id) return Node_Id is
         V_Identifier : Node_Id;
         N            : Node_Id;
      begin
         pragma Assert (AAU.Is_Data (E));

         V_Identifier := Map_TC_Variable_Identifier (E);

         N := Make_Object_Declaration
           (Defining_Identifier => V_Identifier,
            Object_Definition   => RE (RE_Object_Ptr));

         return N;
      end TypeCode_Declaration;

      -------------------
      -- From_Any_Spec --
      -------------------

      function From_Any_Spec (E : Node_Id) return Node_Id is
         Return_Type : Node_Id;
         N           : Node_Id;
      begin
         pragma Assert (AAU.Is_Data (E));

         --  Get the Ada type that was mapped from E

         Return_Type := Map_Ada_Data_Type_Designator (E);

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_From_Any)),
            Parameter_Profile   => Make_List_Id
              (Make_Parameter_Specification
               (Make_Defining_Identifier (PN (P_Item)),
                RE (RE_Any))),
            Return_Type         => Return_Type);
         return N;
      end From_Any_Spec;

      -----------------
      -- To_Any_Spec --
      -----------------

      function To_Any_Spec (E : Node_Id) return Node_Id is
         Item_Type : Node_Id;
         N         : Node_Id;
      begin
         pragma Assert (AAU.Is_Data (E));

         --  Get the Ada type that was mapped from E

         Item_Type := Map_Ada_Data_Type_Designator (E);

         N := Make_Subprogram_Specification
           (Defining_Identifier => Make_Defining_Identifier
              (SN (S_To_Any)),
            Parameter_Profile   => Make_List_Id
              (Make_Parameter_Specification
               (Make_Defining_Identifier (PN (P_Item)),
                Item_Type)),
            Return_Type         => RE (RE_Any));
         return N;
      end To_Any_Spec;

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

      function From_Any_Body (E : Node_Id) return Node_Id;
      --  Makes the body of the From_Any function corresponding to the
      --  data type E

      function To_Any_Body (E : Node_Id) return Node_Id;
      --  Makes the body of the To_Any function corresponding to the
      --  data type E

      Initialization_Specs  : List_Id;
      --  Contains the spec and the flags of the initilization
      --  routines of all data types

      Initialization_Bodies : List_Id;
      --  Contains the bodies of the initilization routines of all
      --  data types

      function Initialization_Flag_Declaration (E : Node_Id) return Node_Id;
      --  Makes the declaration of the boolean flag that indicates
      --  wether the type E is already initialized or not

      function Initialize_Spec (E : Node_Id) return Node_Id;
      --  Makes the spec of the Initialize_<Data_Type_Name> procedure

      function Initialize_Body (E : Node_Id) return Node_Id;
      --  Makes the body of the Initialize_<Data_Type_Name> procedure

      function Deferred_Initialization_Spec return Node_Id;
      --  Makes the Spec of the deferred initialization procedure

      function Deferred_Initialization_Body return Node_Id;
      --  Makes the body of the deferred initialization procedure

      function Helper_Initialization return Node_Id;
      --  Makes the package initialization routine (which is called at
      --  the eleboration

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
         N : Node_Id;
         S : Node_Id;
      begin
         --  Protected objects with accessors are not destined to
         --  network transfert. They do not have helper routines.

         if Get_Data_Representation (E) = Data_With_Accessors then
            return;
         end if;

         Set_Helpers_Body;

         if No (Get_Handling (E, By_Name, H_Ada_Helpers_Body)) then
            N := Message_Comment
              ("Data type "
               & Get_Name_String (AAU.Compute_Full_Name_Of_Instance (E)));
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  For each data type definition, we generate the
            --  following entities in the Helpers package body:

            --  1) The From_Any function body

            N := From_Any_Body (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  2) The To_Any function body

            N := To_Any_Body (E);
            Append_Node_To_List (N, ADN.Statements (Current_Package));

            --  3) The initialization routines

            N := Initialization_Flag_Declaration (E);
            Append_Node_To_List (N, Initialization_Specs);

            N := Initialize_Spec (E);
            Append_Node_To_List (N, Initialization_Specs);

            N := Initialize_Body (E);
            Append_Node_To_List (N, Initialization_Bodies);

            Set_Handling (E, By_Name, H_Ada_Helpers_Body, N);

            --  Visit the subcomponents of E (if any)

            if not AAU.Is_Empty (Subcomponents (E)) then
               S := First_Node (Subcomponents (E));

               while Present (S) loop
                  Visit (Corresponding_Instance (S));
                  S := Next_Node (S);
               end loop;
            end if;
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U : constant Node_Id := ADN.Distributed_Application_Unit
           (ADN.Helpers_Node (Backend_Node (Identifier (E))));
         P : constant Node_Id := ADN.Entity (U);
         S : Node_Id;
         N : Node_Id;

         Initialization_Specs_Backup  : constant List_Id :=
           Initialization_Specs;
         Initialization_Bodies_Backup : constant List_Id :=
           Initialization_Bodies;
      begin
         Push_Entity (P);
         Push_Entity (U);
         Set_Helpers_Body;

         Initialization_Specs  := New_List (ADN.K_Statement_List);
         Initialization_Bodies := New_List (ADN.K_Statement_List);

         --  Start recording all handlings because we want to reset
         --  them for each node.

         Start_Recording_Handlings;

         --  Visit recursively all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         --  Spec of the deferrend initialization procedure

         N := Deferred_Initialization_Spec;
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  The body of the deferrend initialization procedure

         N := Deferred_Initialization_Body;

         --  It's important to append the lists below after the
         --  building of the deferrend initialization body and before
         --  the appending this body to the statements of the package

         --  Append the Initialization spec to statements

         Append_Node_To_List (ADN.First_Node (Initialization_Specs),
                              ADN.Statements (Current_Package));

         --  Append the Initialization bodies to statements

         Append_Node_To_List (ADN.First_Node (Initialization_Bodies),
                              ADN.Statements (Current_Package));

         --  Append the body of the deferrend initialization procedure

         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Restaure the old values of the lists

         Initialization_Specs  := Initialization_Specs_Backup;
         Initialization_Bodies := Initialization_Bodies_Backup;

         --  Finally, the package initialization:

         N := Helper_Initialization;
         ADN.Set_Package_Initialization
           (Current_Package,
            New_List (ADN.K_List_Id));
         Append_Node_To_List (N, ADN.Package_Initialization (Current_Package));

         --  Reset all the recorded handlings

         Reset_Handlings;

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         F        : Node_Id;
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
      begin
         --  Visit all data types

         if not AAU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Present (Corresponding_Instance (F)) then
                  Visit (Corresponding_Instance (F));
               end if;

               F := Next_Node (F);
            end loop;
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

         Pop_Entity; -- Ada_Root
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

      -------------------
      -- From_Any_Body --
      -------------------

      function From_Any_Body (E : Node_Id) return Node_Id is
         use ADN;

         Spec                : constant Node_Id := From_Any_Node
           (Backend_Node (Identifier (E)));
         Declarative_Part    : constant List_Id
           := New_List (K_Declaration_List);
         Statements          : constant List_Id
           := New_List (K_Statement_List);
         Data_Representation : constant Supported_Data_Representation
           := Get_Data_Representation (E);
         N                   : Node_Id;
         P                   : Node_Id;
         S                   : Node_Id;

         procedure Declare_Predefined_Type_Result (T : Node_Id);
         pragma Inline (Declare_Predefined_Type_Result);
         --  Declares the 'Result' constant that corresponds to the
         --  predefined type T

         ------------------------------------
         -- Declare_Predefined_Type_Result --
         ------------------------------------

         procedure Declare_Predefined_Type_Result (T : Node_Id) is
         begin
            N := Make_Subprogram_Call
              (RE (RE_From_Any_2),
               Make_List_Id (Make_Defining_Identifier (PN (P_Item))));

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Result)),
               Constant_Present    => True,
               Object_Definition   => T,
               Expression          => N);
            Append_Node_To_List (N, Declarative_Part);
         end Declare_Predefined_Type_Result;

      begin
         --  Declarative part

         case Data_Representation is
            when Data_Integer =>
               Declare_Predefined_Type_Result (RE (RE_Long));

            when Data_Boolean =>
               Declare_Predefined_Type_Result (RE (RE_Boolean_1));

            when Data_Float =>
               Declare_Predefined_Type_Result (RE (RE_Float_1));

            when Data_String =>
               Declare_Predefined_Type_Result (RE (RE_String_1));

            when Data_Wide_String =>
               Declare_Predefined_Type_Result (RE (RE_Wide_String_1));

            when Data_Character =>
               Declare_Predefined_Type_Result (RE (RE_Character_1));

            when Data_Wide_Character =>
               Declare_Predefined_Type_Result (RE (RE_Wide_Character_1));

            when Data_Struct =>
               --  Declare a temporary variable to store the fields
               --  values

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (VN (V_Index)),
                  Object_Definition   => RE (RE_Any));
               Append_Node_To_List (N, Declarative_Part);

               --  For each field of the record, a variable having the
               --  type of this field.

               S := AAN.First_Node (Subcomponents (E));

               while Present (S) loop
                  N := Make_Object_Declaration
                    (Defining_Identifier => Map_Record_Field_Identifier
                       (S),
                     Object_Definition   => Map_Ada_Data_Type_Designator
                       (Corresponding_Instance (S)));
                  Append_Node_To_List (N, Declarative_Part);

                  S := AAN.Next_Node (S);
               end loop;

            when others =>
               --  This cannot happen

               raise Program_Error;
         end case;

         --  Statements

         case Data_Representation is
            when Data_Integer
              | Data_Boolean
              | Data_Float
              | Data_Character
              | Data_Wide_Character =>

               N := Make_Type_Conversion
                 (Map_Ada_Data_Type_Designator (E),
                  Make_Defining_Identifier (PN (P_Result)));

            when Data_String =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String),
                  Make_List_Id (Make_Defining_Identifier (PN (P_Result))));

               P := Make_Defining_Identifier (SN (S_To_Bounded_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P, Map_Package_Instantiation_Designator (E));

               N := Make_Subprogram_Call (P, Make_List_Id (N));
               N := Make_Type_Conversion (Map_Ada_Data_Type_Designator (E), N);

            when Data_Wide_String =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_Wide_String),
                  Make_List_Id (Make_Defining_Identifier (PN (P_Result))));

               P := Make_Defining_Identifier (SN (S_To_Bounded_Wide_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P, Map_Package_Instantiation_Designator (E));

               N := Make_Subprogram_Call (P, Make_List_Id (N));
               N := Make_Type_Conversion (Map_Ada_Data_Type_Designator (E), N);

            when Data_Struct =>
               declare
                  Aggregate_List : constant List_Id := New_List (K_List_Id);
                  Index          : Unsigned_Long_Long := 0;
                  C_Instance     : Node_Id;
               begin
                  S := AAN.First_Node (Subcomponents (E));

                  while Present (S) loop
                     --  Get the correspodning data instance

                     C_Instance := Corresponding_Instance (S);

                     --  Get a designator for the TypeCode variable

                     N := Extract_Designator
                       (TypeCode_Node
                        (Backend_Node
                         (Identifier
                          (C_Instance))));

                     N := Make_Subprogram_Call
                       (RE (RE_To_Ref), Make_List_Id (N));

                     N := Make_Subprogram_Call
                       (RE (RE_Get_Aggregate_Element),
                        Make_List_Id
                        (Make_Defining_Identifier (PN (P_Item)),
                         N,
                         Make_Literal
                         (New_Integer_Value (Index, 1, 10))));
                     N := Make_Assignment_Statement
                       (Make_Defining_Identifier (VN (V_Index)), N);
                     Append_Node_To_List (N, Statements);

                     N := Make_Subprogram_Call
                       (Extract_Designator
                        (From_Any_Node
                         (Backend_Node
                          (Identifier
                           (C_Instance)))),
                        Make_List_Id
                        (Make_Defining_Identifier
                         (VN (V_Index))));

                     --  The Result_<Field_Name> variable

                     N := Make_Assignment_Statement
                       (Map_Record_Field_Identifier (S), N);
                     Append_Node_To_List (N, Statements);

                     --  Make a component association and append it to
                     --  the final result.

                     N := Make_Component_Association
                       (Map_Ada_Defining_Identifier (S),
                        Map_Record_Field_Identifier (S));
                     Append_Node_To_List (N, Aggregate_List);

                     Index := Index + 1;
                     S := AAN.Next_Node (S);
                  end loop;

                  N := Make_Record_Aggregate (Aggregate_List);
               end;

            when others =>
               --  This cannot happen

               raise Program_Error;
         end case;

         N := Make_Return_Statement (N);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, Declarative_Part, Statements);
         return N;
      end From_Any_Body;

      -----------------
      -- To_Any_Body --
      -----------------

      function To_Any_Body (E : Node_Id) return Node_Id is
         use ADN;

         Spec                : constant Node_Id := To_Any_Node
           (Backend_Node (Identifier (E)));
         Declarative_Part    : constant List_Id
           := New_List (K_Declaration_List);
         Statements          : constant List_Id
           := New_List (K_Statement_List);
         Data_Representation : constant Supported_Data_Representation
           := Get_Data_Representation (E);
         N                   : Node_Id;
         P                   : Node_Id;
         S                   : Node_Id;

         procedure Declare_Predefined_Type_Result (V : Node_Id);
         pragma Inline (Declare_Predefined_Type_Result);
         --  Declares the 'Result' which is the Any value
         --  corresponding to the predfined type value V

         procedure Predefined_Type_Statements (E : Node_Id);
         pragma Inline (Predefined_Type_Statements);
         --  Makes the To_Any statments in case of a predefined type E

         ------------------------------------
         -- Declare_Predefined_Type_Result --
         ------------------------------------

         procedure Declare_Predefined_Type_Result (V : Node_Id) is
         begin
            N := Make_Subprogram_Call
              (RE (RE_To_Any_2), Make_List_Id (V));

            N := Make_Object_Declaration
              (Defining_Identifier => Make_Defining_Identifier (PN (P_Result)),
               Object_Definition   => RE (RE_Any),
               Expression          => N);
            Append_Node_To_List (N, Declarative_Part);
         end Declare_Predefined_Type_Result;

         --------------------------------
         -- Predefined_Type_Statements --
         --------------------------------

         procedure Predefined_Type_Statements (E : Node_Id) is
         begin
            N := Extract_Designator
              (TypeCode_Node
               (Backend_Node
                (Identifier
                 (E))));

            N := Make_Subprogram_Call
              (RE (RE_Set_Type),
               Make_List_Id
               (Make_Defining_Identifier (PN (P_Result)), N));
            Append_Node_To_List (N, Statements);

            N := Make_Return_Statement
              (Make_Defining_Identifier (PN (P_Result)));
            Append_Node_To_List (N, Statements);
         end Predefined_Type_Statements;

      begin

         --  Declarative part

         case Data_Representation is
            when Data_Integer =>
               N := Make_Type_Conversion
                 (RE (RE_Long),
                  Make_Defining_Identifier (PN (P_Item)));
               Declare_Predefined_Type_Result (N);

            when Data_Float =>
               N := Make_Type_Conversion
                 (RE (RE_Float_1),
                  Make_Defining_Identifier (PN (P_Item)));
               Declare_Predefined_Type_Result (N);

            when Data_Boolean =>
               N := Make_Type_Conversion
                 (RE (RE_Boolean_1),
                  Make_Defining_Identifier (PN (P_Item)));
               Declare_Predefined_Type_Result (N);

            when Data_Character =>
               N := Make_Type_Conversion
                 (RE (RE_Character_1),
                  Make_Defining_Identifier (PN (P_Item)));
               Declare_Predefined_Type_Result (N);

            when Data_Wide_Character =>
               N := Make_Type_Conversion
                 (RE (RE_Wide_Character_1),
                  Make_Defining_Identifier (PN (P_Item)));
               Declare_Predefined_Type_Result (N);

            when Data_String =>
               P := Make_Defining_Identifier (TN (T_Bounded_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P, Map_Package_Instantiation_Designator (E));
               N := Make_Type_Conversion
                 (P, Make_Defining_Identifier (PN (P_Item)));

               P := Make_Defining_Identifier (SN (S_To_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P, Map_Package_Instantiation_Designator (E));
               N := Make_Subprogram_Call (P, Make_List_Id (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  Make_List_Id (N));

               Declare_Predefined_Type_Result (N);

            when Data_Wide_String =>
               P := Make_Defining_Identifier (TN (T_Bounded_Wide_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P, Map_Package_Instantiation_Designator (E));
               N := Make_Type_Conversion
                 (P, Make_Defining_Identifier (PN (P_Item)));

               P := Make_Defining_Identifier (SN (S_To_Wide_String));
               Set_Homogeneous_Parent_Unit_Name
                 (P, Map_Package_Instantiation_Designator (E));
               N := Make_Subprogram_Call (P, Make_List_Id (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Wide_String),
                  Make_List_Id (N));

               Declare_Predefined_Type_Result (N);

            when Data_Struct =>
               --  The Result variable

               N := Extract_Designator
                 (TypeCode_Node
                  (Backend_Node
                   (Identifier
                    (E))));

               N := Make_Subprogram_Call
                 (RE (RE_To_Ref), Make_List_Id (N));

               N := Make_Subprogram_Call
                 (RE (RE_Get_Empty_Any_Aggregate),
                  Make_List_Id (N));

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Result)),
                  Object_Definition   => RE (RE_Any),
                  Expression          => N);
               Append_Node_To_List (N, Declarative_Part);

            when others =>
               --  This cannot happen

               raise Program_Error;
         end case;

         --  Statements

         case Data_Representation is
            when Data_Integer
              | Data_Float
              | Data_Boolean
              | Data_Character
              | Data_Wide_Character
              | Data_String
              | Data_Wide_String =>
               Predefined_Type_Statements (E);

            when Data_Struct =>
               declare
                  C_Instance : Node_Id;
               begin
                  S := AAN.First_Node (Subcomponents (E));

                  while Present (S) loop
                     C_Instance := Corresponding_Instance (S);

                     N := Make_Selected_Component
                       (Make_Defining_Identifier (PN (P_Item)),
                        Map_Ada_Defining_Identifier (S));

                     N := Make_Subprogram_Call
                       (Extract_Designator
                        (To_Any_Node
                         (Backend_Node
                          (Identifier
                           (C_Instance)))),
                        Make_List_Id (N));

                     N := Make_Subprogram_Call
                       (RE (RE_Add_Aggregate_Element),
                        Make_List_Id
                        (Make_Defining_Identifier (PN (P_Result)), N));
                     Append_Node_To_List (N, Statements);

                     S := AAN.Next_Node (S);
                  end loop;

                  N := Make_Return_Statement
                    (Make_Defining_Identifier (PN (P_Result)));
                  Append_Node_To_List (N, Statements);
               end;

            when others =>
               --  This cannot happen
               raise Program_Error;
         end case;

         N := Make_Subprogram_Implementation
           (Spec, Declarative_Part, Statements);
         return N;
      end To_Any_Body;

      -------------------------------------
      -- Initialization_Flag_Declaration --
      -------------------------------------

      function Initialization_Flag_Declaration (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N := Make_Object_Declaration
           (Defining_Identifier => Map_Initialized_Flag_Identifier (E),
            Object_Definition   => RE (RE_Boolean_2),
            Expression          => RE (RE_False));
         return N;
      end Initialization_Flag_Declaration;

      ---------------------
      -- Initialize_Spec --
      ---------------------

      function Initialize_Spec (E : Node_Id) return Node_Id is
         N : Node_Id;
      begin
         N := Make_Subprogram_Specification
           (Defining_Identifier => Map_Initialize_Identifier (E),
            Parameter_Profile   => No_List,
            Return_Type         => No_Node);
         return N;
      end Initialize_Spec;

      ---------------------
      -- Initialize_Body --
      ---------------------

      function Initialize_Body (E : Node_Id) return Node_Id is
         use ADN;

         Spec                : constant Node_Id := Initialize_Spec (E);
         Declarative_Part    : constant List_Id
           := New_List (K_Declaration_List);
         Statements          : constant List_Id := New_List (K_Statement_List);
         Data_Representation : constant Supported_Data_Representation
           := Get_Data_Representation (E);
         N                   : Node_Id;
         TC                  : Node_Id;
         S                   : Node_Id;
         Type_Name           : Name_Id := AAN.Name (Identifier (E));
         If_Statements       : constant List_Id := New_List (K_Statement_List);
      begin
         --  Mark the type as initialized

         N := Make_Assignment_Statement
           (Map_Initialized_Flag_Identifier (E), RE (RE_True));
         Append_Node_To_List (N, If_Statements);

         --  Common declarations and statements

         case Data_Representation is
            when Data_Integer
              | Data_Float
              | Data_Boolean
              | Data_Character
              | Data_Wide_Character
              | Data_String
              | Data_Wide_String
              | Data_Struct =>

               --  1) The 'Name' variable

               N := Make_Literal (New_String_Value (Type_Name));
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String), Make_List_Id (N));
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Name)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_String_1),
                  Expression          => N);
               Append_Node_To_List (N, Declarative_Part);

               --  1) The 'Id' variable

               N := Make_Literal
                 (New_String_Value
                  (Add_Suffix_To_Name (":1.0", Type_Name)));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String), Make_List_Id (N));
               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier (PN (P_Id)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_String_1),
                  Expression          => N);
               Append_Node_To_List (N, Declarative_Part);

               --  2) The 'Type_Code' variable

               if Data_Representation = Data_Struct then
                  N := RE (RE_TC_Struct);
               else
                  N := RE (RE_TC_Alias);
               end if;

               --  Declare the TypeCode reference

               N := Make_Object_Declaration
                 (Defining_Identifier => Make_Defining_Identifier
                    (PN (P_Type_Code)),
                  Constant_Present    => True,
                  Object_Definition   => RE (RE_Local_Ref),
                  Expression          => N);
               Append_Node_To_List (N, Declarative_Part);

               --  Append the 'Name' parameter

               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (Make_Defining_Identifier (PN (P_Name))));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

               --  Append the 'Id' parameter

               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (Make_Defining_Identifier (PN (P_Id))));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when others =>
               null;
         end case;

         --  Specific declarations and statements

         case Data_Representation is
            when Data_Integer =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (RE (RE_TC_Long)));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when Data_Float =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (RE (RE_TC_Float)));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when Data_Boolean =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (RE (RE_TC_Boolean)));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when Data_Character =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (RE (RE_TC_Character)));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when Data_Wide_Character =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (RE (RE_TC_Wide_Character)));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when Data_String =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (RE (RE_TC_String)));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when Data_Wide_String =>
               N := Make_Subprogram_Call
                 (RE (RE_To_Any_2),
                  Make_List_Id (RE (RE_TC_Wide_String)));
               N := Make_Subprogram_Call
                 (RE (RE_Add_Parameter),
                  Make_List_Id
                  (Make_Defining_Identifier (PN (P_Type_Code)), N));
               Append_Node_To_List (N, If_Statements);

            when Data_Struct =>
               declare
                  C_Instance : Node_Id;
               begin
                  S := AAN.First_Node (Subcomponents (E));

                  while Present (S) loop
                     C_Instance := Corresponding_Instance (S);

                     --  1) Declare a variable which will cntain the
                     --  field name

                     Type_Name := AAN.Name (Identifier (S));
                     N := Make_Literal (New_String_Value (Type_Name));
                     N := Make_Subprogram_Call
                       (RE (RE_To_PolyORB_String), Make_List_Id (N));
                     N := Make_Object_Declaration
                       (Defining_Identifier => Map_Record_Field_Identifier (S),
                        Constant_Present    => True,
                        Object_Definition   => RE (RE_String_1),
                        Expression          => N);
                     Append_Node_To_List (N, Declarative_Part);

                     --  2) Initialize the data type corresponding to
                     --  the aggregate

                     N := Make_Subprogram_Call
                       (Map_Initialize_Identifier (C_Instance), No_List);
                     Append_Node_To_List (N, If_Statements);

                     --  3) Add the parameter corresponding to the
                     --  TypeCode of the aggregate

                     N := Extract_Designator
                       (TypeCode_Node
                        (Backend_Node
                         (Identifier
                          (C_Instance))));

                     N := Make_Subprogram_Call
                       (RE (RE_To_Ref), Make_List_Id (N));

                     N := Make_Subprogram_Call
                       (RE (RE_To_Any_2),
                        Make_List_Id (N));

                     N := Make_Subprogram_Call
                       (RE (RE_Add_Parameter),
                        Make_List_Id
                        (Make_Defining_Identifier (PN (P_Type_Code)), N));
                     Append_Node_To_List (N, If_Statements);

                     --  4) Add the parameter corresponding to the
                     --  aggregate name

                     N := Make_Subprogram_Call
                       (RE (RE_To_Any_2),
                        Make_List_Id
                        (Map_Record_Field_Identifier (S)));
                     N := Make_Subprogram_Call
                       (RE (RE_Add_Parameter),
                        Make_List_Id
                        (Make_Defining_Identifier (PN (P_Type_Code)), N));
                     Append_Node_To_List (N, If_Statements);

                     S := AAN.Next_Node (S);
                  end loop;
               end;

            when others =>
               --  This cannot happen

               raise Program_Error;
         end case;

         --  Get the TypeCode object access

         TC := Extract_Designator
           (TypeCode_Node
            (Backend_Node
             (Identifier
              (E))));

         --  Assign the value of the TypeCode object

         N := Make_Subprogram_Call
           (RE (RE_Object_Of),
            Make_List_Id (Make_Defining_Identifier (PN (P_Type_Code))));

         N := Make_Assignment_Statement (TC, N);
         Append_Node_To_List (N, If_Statements);

         --  Disable reference counting on the TypeCode object

         N := Make_Subprogram_Call
           (RE (RE_Disable_Reference_Counting),
            Make_List_Id (Make_Explicit_Dereference (TC)));
         Append_Node_To_List (N, If_Statements);

         --  Build the 'if' statements

         N := Make_Expression (Map_Initialized_Flag_Identifier (E), Op_Not);
         N := Make_If_Statement (N, If_Statements);
         Append_Node_To_List (N, Statements);

         N := Make_Subprogram_Implementation
           (Spec, Declarative_Part, Statements);
         return N;
      end Initialize_Body;

      ----------------------------------
      -- Deferred_Initialization_Spec --
      ----------------------------------

      function Deferred_Initialization_Spec return Node_Id is
      begin
         return Make_Subprogram_Specification
           (Make_Defining_Identifier (SN (S_Deferred_Initialization)),
            No_List,
            No_Node);
      end Deferred_Initialization_Spec;

      ----------------------------------
      -- Deferred_Initialization_Body --
      ----------------------------------

      function Deferred_Initialization_Body return Node_Id is
         use ADN;

         Spec       : constant Node_Id := Deferred_Initialization_Spec;
         Statements : constant List_Id := New_List (K_Statement_List);
         S          : Node_Id;
         N          : Node_Id;
      begin
         --  This function call all the Initialize_<..> procedures

         S := ADN.First_Node (Initialization_Specs);

         while Present (S) loop
            if ADN.Kind (S) = K_Subprogram_Specification then
               N := Make_Subprogram_Call
                 (Copy_Node (Defining_Identifier (S)), No_List);
               Append_Node_To_List (N, Statements);
            end if;

            S := ADN.Next_Node (S);
         end loop;

         N := Make_Subprogram_Implementation (Spec, No_List, Statements);
         return N;
      end Deferred_Initialization_Body;

      ---------------------------
      -- Helper_Initialization --
      ---------------------------

      function Helper_Initialization return Node_Id is
         use ADN;

         N                : Node_Id;
         V                : Value_Id;
         Aggregates       : constant List_Id := New_List (K_Component_List);
         Declarative_Part : constant List_Id := New_List (K_Declaration_List);
         Statements       : constant List_Id := New_List (K_Statement_List);
      begin
         --  Declarative part
         --  Adding 'use' clauses to make the code more readable

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings));
         Append_Node_To_List (N, Declarative_Part);

         N := Make_Used_Package (RU (RU_PolyORB_Utils_Strings_Lists));
         Append_Node_To_List (N, Declarative_Part);

         --  Statements

         --  The package name

         N := Defining_Identifier
           (Package_Declaration (Current_Package));
         V := New_String_Value (Fully_Qualified_Name (N));
         N := Make_Expression (Make_Literal (V), Op_Plus);
         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Name)),
            Expression     => N);
         Append_Node_To_List (N, Aggregates);

         --  The conflicts

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Conflicts)),
            Expression     => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Building the dependancy list of the package

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Depends)),
            Expression     => Make_Expression
              (Map_Dependency (RU (RU_PolyORB_Any_Initialization, False)),
               Op_Plus));
         Append_Node_To_List (N, Aggregates);

         --  Provides

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Provides)),
            Expression     => RE (RE_Empty));
         Append_Node_To_List (N, Aggregates);

         --  Implicit

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Implicit)),
            Expression     => RE (RE_False));
         Append_Node_To_List (N, Aggregates);

         --  Init procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Init)),
            Expression     => Make_Type_Attribute
              (Make_Designator (SN (S_Deferred_Initialization)),
               A_Access));
         Append_Node_To_List (N, Aggregates);

         --  Shutdown procedure

         N := Make_Component_Association
           (Selector_Name  => Make_Defining_Identifier (PN (P_Shutdown)),
            Expression     => Make_Null_Statement);
         Append_Node_To_List (N, Aggregates);

         --  Registering the module

         N := Make_Record_Aggregate
           (Aggregates);

         N := Make_Qualified_Expression
           (Subtype_Mark => RE (RE_Module_Info),
            Aggregate    => N);

         N := Make_Subprogram_Call (RE (RE_Register_Module), Make_List_Id (N));
         Append_Node_To_List (N, Statements);

         --  Building the initialization block statement

         N := Make_Block_Statement
           (Declarative_Part => Declarative_Part,
            Statements       => Statements);
         return N;
      end Helper_Initialization;

   end Package_Body;

end Ocarina.Backends.PO_QoS_Ada.Helpers;
