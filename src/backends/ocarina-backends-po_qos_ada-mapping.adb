------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A . M A P P I N G   --
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

with GNAT.Case_Util;

with Namet; use Namet;

with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.Backends.Utils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_QoS_Ada.Mapping is

   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Ada_Tree.Nodes;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.Ada_Values;

   package AAN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   function Package_Binding_Internal_Name (I : Node_Id) return Name_Id;
   --  For code factorization purpose between
   --  Bind_Ada_Identifier_To_Package and Get_Bound_Package.

   function Get_Servant_Internal_Name (Entity : Node_Id) return Name_Id;
   --  Return a conventional used to bind entities to servant numbers
   --  and indices.

   ---------------------------------
   -- Map_Distributed_Application --
   ---------------------------------

   function Map_Distributed_Application (E : Node_Id) return Node_Id is
      D : constant Node_Id := New_Node (ADN.K_QoS_Distributed_Application);
   begin
      pragma Assert (AAU.Is_System (E));

      --  Update the global variable to be able to fetch the root of
      --  the distributed application and generate the source files.

      Ada_Root := D;

      ADN.Set_Name (D, To_Ada_Name
                    (AAN.Display_Name
                     (AAN.Identifier
                      (E))));
      ADN.Set_QoS_Nodes (D, New_List (ADN.K_List_Id));

      return D;
   end Map_Distributed_Application;

   ------------------
   -- Map_QoS_Node --
   ------------------

   function Map_QoS_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (ADN.K_QoS_Node);
   begin
      pragma Assert (AAU.Is_Process (E));

      --  The name of the node is not the name of the process
      --  component instance, but the name of the process subcomponent
      --  corresponding to this instance.

      ADN.Set_Name (N, To_Ada_Name
                    (AAN.Display_Name
                     (AAN.Identifier
                      (AAN.Parent_Subcomponent
                       (E)))));

      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list of the PolyORB-HI
      --  distributed application. We are sure that the top of the
      --  entity stack contains the Ada distributed application node.

      Append_Node_To_List (N, QoS_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_QoS_Node;

   ------------------
   -- Map_QoS_Unit --
   ------------------

   function Map_QoS_Unit
     (E : Node_Id;
      F : Node_Id := No_Node)
     return Node_Id
   is
      U        : Node_Id;
      L        : List_Id;
      P        : Node_Id;
      N        : Node_Id;
      Ada_Name : Name_Id;
   begin
      pragma Assert (Is_Namespace (E) or else AAU.Is_Process (E));

      U := New_Node (K_QoS_Unit, Identifier (E));
      L := New_List (K_Packages);
      Set_Packages (U, L);

      if AAU.Is_Process (E) then
         Ada_Name := To_Ada_Name
           (AAN.Display_Name
            (AAN.Identifier
             (AAN.Parent_Subcomponent
              (E))));

         --  Main suprogram

         P := Make_Main_Subprogram_Implementation
           (Make_Defining_Identifier
            (Ada_Name));
         Set_Distributed_Application_Unit (P, U);
         Set_Main_Subprogram (U, P);
         Append_Node_To_List (P, L);
         Bind_AADL_To_Main (Identifier (E), P);

         --  'Helpers' package

         N := Defining_Identifier (RU (RU_Helpers, False));
         P := Make_Package_Declaration (N);
         Set_Distributed_Application_Unit (P, U);
         Set_Helpers_Package (U, P);
         Append_Node_To_List (P, L);
         Bind_AADL_To_Helpers (Identifier (E), P);

         --  'Servants' package

         N := Defining_Identifier (RU (RU_Servants, False));
         P := Make_Package_Declaration (N);
         Set_Distributed_Application_Unit (P, U);
         Set_Servants_Package (U, P);
         Append_Node_To_List (P, L);
         Bind_AADL_To_Servants (Identifier (E), P);

         --  'Setup' package

         N := Defining_Identifier (RU (RU_ARAO_Setup_Application, False));
         P := Make_Package_Declaration (N);
         Set_Distributed_Application_Unit (P, U);
         Set_Setup_Package (U, P);
         Append_Node_To_List (P, L);
         Bind_AADL_To_Setup (Identifier (E), P);

         --  'PolyORB.Parameters.Partition' package

         N := Defining_Identifier
           (RU (RU_PolyORB_Parameters_Partition, False));
         P := Make_Package_Declaration (N);
         Set_Distributed_Application_Unit (P, U);
         Set_Parameters_Package (U, P);
         Append_Node_To_List (P, L);
         Bind_AADL_To_Parameters (Identifier (E), P);

         --  'ARAO.Object_Adapter' package

         N := Defining_Identifier (RU (RU_ARAO_Object_Adapter, False));
         P := Make_Package_Declaration (N);
         Set_Distributed_Application_Unit (P, U);
         Set_Obj_Adapters_Package (U, P);
         Append_Node_To_List (P, L);
         Bind_AADL_To_Obj_Adapters (Identifier (E), P);
      elsif Is_Namespace (E) then
         --  'Partition.<..>' package

         N := Map_Ada_Namespace_Defining_Identifier (E, "Partition");
         P := Make_Package_Declaration (N);
         Set_Distributed_Application_Unit (P, U);
         Set_Namespaces_Package (U, P);
         Append_Node_To_List (P, L);

         --  We generate a Partition.<..> package per Namespace and
         --  Process. So the binding must use the namespace and
         --  process nodes (E and F).

         Bind_AADL_To_Namespaces (Bind_Two_Nodes (E, F), P);
         Bind_Ada_Identifier_To_Package (N, P);

         --  Set also the Main_Subprogram node to know to which
         --  nodebelongs this mapped unit.

         Set_Main_Subprogram (U, Main_Node (Backend_Node (Identifier (F))));
      end if;

      --  Append the Unit to the units list of the current Ada
      --  partition. The top of the entity stack is necessarily an Ada
      --  partition node

      Append_Node_To_List (U, Units (Current_Entity));
      ADN.Set_Entity (U, Current_Entity);

      return U;
   end Map_QoS_Unit;

   --------------------------------
   -- Map_TC_Variable_Identifier --
   --------------------------------

   function Map_TC_Variable_Identifier (E : Node_Id) return Node_Id is
      Full_Name : Name_Id;
   begin
      pragma Assert (AAU.Is_Data (E));

      Full_Name := To_Ada_Name (Display_Name (Identifier (E)));

      Set_Str_To_Name_Buffer ("TC_");
      Get_Name_String_And_Append (Full_Name);
      return Make_Defining_Identifier (Name_Find);
   end Map_TC_Variable_Identifier;

   ---------------------------------
   -- Map_Record_Field_Identifier --
   ---------------------------------

   function Map_Record_Field_Identifier (S : Node_Id) return Node_Id is
   begin
      pragma Assert (Kind (S) = K_Subcomponent_Instance and then
                     AAU.Is_Data (Corresponding_Instance (S)));

      Set_Str_To_Name_Buffer ("Result_");
      Get_Name_String_And_Append (Display_Name (Identifier (S)));

      return Make_Defining_Identifier (Name_Find);
   end Map_Record_Field_Identifier;

   -------------------------------
   -- Map_Initialize_Identifier --
   -------------------------------

   function Map_Initialize_Identifier (E : Node_Id) return Node_Id is
      Type_Name : Name_Id;
   begin
      pragma Assert (AAU.Is_Data (E));

      Type_Name := To_Ada_Name (Display_Name (Identifier (E)));

      Set_Str_To_Name_Buffer ("Initialize_");
      Get_Name_String_And_Append (Type_Name);

      return Make_Defining_Identifier (Name_Find);
   end Map_Initialize_Identifier;

   -------------------------------------
   -- Map_Initialized_Flag_Identifier --
   -------------------------------------

   function Map_Initialized_Flag_Identifier (E : Node_Id) return Node_Id is
      Type_Name : Name_Id;
   begin
      pragma Assert (AAU.Is_Data (E));

      Type_Name := To_Ada_Name (Display_Name (Identifier (E)));

      Get_Name_String (Type_Name);
      Add_Str_To_Name_Buffer ("_Initialized");
      return Make_Defining_Identifier (Name_Find);
   end Map_Initialized_Flag_Identifier;

   ------------------------------------------
   -- Map_Package_Instantiation_Designator --
   ------------------------------------------

   function Map_Package_Instantiation_Designator
     (E : Node_Id)
     return Node_Id
   is
      N : Node_Id;
   begin
      pragma Assert (AAU.Is_Data (E));

      N := Map_Ada_Data_Type_Designator (E);
      Get_Name_String (ADN.Name (ADN.Defining_Identifier (N)));
      Add_Str_To_Name_Buffer ("_PKG");
      ADN.Set_Name (ADN.Defining_Identifier (N), Name_Find);

      return N;
   end Map_Package_Instantiation_Designator;

   --------------------
   -- Map_Dependency --
   --------------------

   function Map_Dependency (Dep : Node_Id) return Node_Id
   is
      function "=" (Name : Name_Id; Node : Node_Id) return Boolean;
      function Is_Internal_Unit (Unit : Node_Id) return Boolean;

      ---------
      -- "=" --
      ---------

      function "=" (Name : Name_Id; Node : Node_Id) return Boolean is
      begin
         return Name = Fully_Qualified_Name (Node);
      end "=";

      ----------------------
      -- Is_Internal_Unit --
      ----------------------

      function Is_Internal_Unit (Unit : Node_Id) return Boolean is
         N : Node_Id := Unit;
      begin
         if ADN.Kind (N) = K_Designator then
            N := Defining_Identifier (N);
         end if;

         return ADN.Kind (Corresponding_Node (N)) = K_Package_Instantiation;
      end Is_Internal_Unit;

      Dep_Name : Name_Id;
      V        : Value_Id;
      N        : Node_Id;
   begin
      if Is_Internal_Unit (Dep) then
         return No_Node;
      end if;

      Dep_Name := Fully_Qualified_Name (Dep);

      --  Second case : We lower the case of these entities
      --  * ARAO.Setup.OA.Multithreaded

      if Dep_Name = RU (RU_ARAO_Setup_OA_Multithreaded, False) then
         Get_Name_String (Dep_Name);
         GNAT.Case_Util.To_Lower (Name_Buffer (1 .. Name_Len));
         Dep_Name := Name_Find;

         --  Third case : Some PolyORB units have a customized
         --  initialization name

      elsif Dep_Name = RU (RU_PolyORB_Setup_OA_Basic_RT_POA, False) then
         Set_Str_To_Name_Buffer ("rt_poa");
         Dep_Name := Name_Find;
      elsif Dep_Name = RU (RU_PolyORB_Any_Initialization, False) then
         Set_Str_To_Name_Buffer ("any");
         Dep_Name := Name_Find;
      end if;

      V := New_String_Value (Dep_Name);
      N := Make_Literal (V);
      return N;
   end Map_Dependency;

   --------------------------------
   -- Map_Object_Type_Identifier --
   --------------------------------

   function Map_Object_Type_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      Get_Name_String
        (AAU.Compute_Full_Name_Of_Instance
         (Parent_Subcomponent (E), True, False));
      Add_Str_To_Name_Buffer ("_Object");
      return Make_Defining_Identifier (Name_Find);
   end Map_Object_Type_Identifier;

   ------------------------------
   -- Map_Reference_Identifier --
   ------------------------------

   function Map_Reference_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (AAU.Is_Thread (E) or else
                     AAN.Kind (E) = K_Port_Spec_Instance);

      if AAU.Is_Thread (E) then
         Get_Name_String
           (AAU.Compute_Full_Name_Of_Instance
            (Parent_Subcomponent (E), True, False));
      else
         Get_Name_String (AAU.Compute_Full_Name_Of_Instance (E, True, False));
      end if;

      Add_Str_To_Name_Buffer ("_Ref");

      return Make_Defining_Identifier (Name_Find);
   end  Map_Reference_Identifier;

   --------------------------------------
   -- Map_Thread_Controller_Identifier --
   --------------------------------------

   function Map_Thread_Controller_Identifier (E : Node_Id) return Node_Id is
   begin
      pragma Assert (AAU.Is_Thread (E));

      Get_Name_String
        (AAU.Compute_Full_Name_Of_Instance
         (Parent_Subcomponent (E), True, False));
      Add_Str_To_Name_Buffer ("_Controller");

      return Make_Defining_Identifier (Name_Find);
   end Map_Thread_Controller_Identifier;

   ------------------
   -- Map_Ada_Time --
   ------------------

   function Map_Ada_Time (T : Time_Type) return Node_Id is
      Time : Unsigned_Long_Long;
      S    : Node_Id;
   begin
      case T.U is
         when Picosecond =>
            --  If we can convert it into nanosecond, we are
            --  OK. Otherwise this is an error because Ada.Real_Time
            --  does not support picoseconds

            if T.T mod 1000 = 0 then
               Time := T.T / 1000;
               S := RE (RE_Nanoseconds);
            else
               return No_Node;
            end if;

         when Nanosecond =>
            Time := T.T;
            S := RE (RE_Nanoseconds);

         when Microsecond =>
            Time := T.T;
            S := RE (RE_Microseconds);

         when Millisecond =>
            Time := T.T;
            S := RE (RE_Milliseconds);

         when Second =>
            Time := T.T;
            S := RE (RE_Seconds);

         when Minute =>
            Time := T.T;
            S := RE (RE_Minutes);

         when Hour =>
            --  Convert it into minutes

            Time := T.T * 60;
            S := RE (RE_Minutes);
      end case;

      return Make_Subprogram_Call
        (S, Make_List_Id (Make_Literal (New_Integer_Value (Time, 1, 10))));
   end Map_Ada_Time;

   ----------------------
   -- Map_Ada_Priority --
   ----------------------

   function Map_Ada_Priority (P : Unsigned_Long_Long) return Node_Id is
   begin
      --  XXX we should use the priority_mapping property from AADLv2

      return Make_Literal (New_Integer_Value (P, 1, 10));
   end Map_Ada_Priority;

   ------------------------------------
   -- Map_Buffer_Instance_Identifier --
   ------------------------------------

   function Map_Buffer_Instance_Identifier (P : Node_Id) return Node_Id is
      Port_Name   : Name_Id;
      Thread_Name : Name_Id;
   begin
      pragma Assert (Kind (P) = K_Port_Spec_Instance);

      Port_Name := To_Ada_Name (Display_Name (Identifier (P)));
      Thread_Name := To_Ada_Name
        (Display_Name
         (Identifier
          (Parent_Subcomponent
           (Parent_Component
            (P)))));

      Get_Name_String (Thread_Name);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Port_Name);
      Add_Str_To_Name_Buffer ("_Buffer");

      return Make_Defining_Identifier (Name_Find);
   end Map_Buffer_Instance_Identifier;

   -----------------------------
   -- Map_Variable_Identifier --
   -----------------------------

   function Map_Variable_Identifier (P : Node_Id) return Node_Id is
      Port_Name   : Name_Id;
      Thread_Name : Name_Id;
   begin
      pragma Assert (Kind (P) = K_Port_Spec_Instance);

      Port_Name := To_Ada_Name (Display_Name (Identifier (P)));
      Thread_Name := To_Ada_Name
        (Display_Name
         (Identifier
          (Parent_Subcomponent
           (Parent_Component
            (P)))));

      Get_Name_String (Thread_Name);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Port_Name);
      Add_Str_To_Name_Buffer ("_Var");

      return Make_Defining_Identifier (Name_Find);
   end Map_Variable_Identifier;

   --------------------------
   -- Map_Mutex_Identifier --
   --------------------------

   function Map_Mutex_Identifier (E : Node_Id) return Node_Id is
      Thread_Name : Name_Id;
   begin
      pragma Assert (AAU.Is_Thread (E));

      --  The mutex name is mapped from the subcomponent name to avoid
      --  name clashing.

      Thread_Name := To_Ada_Name
        (Display_Name
         (Identifier
          (Parent_Subcomponent
           (E))));

      Get_Name_String (Thread_Name);
      Add_Str_To_Name_Buffer ("_Mutex");

      return Make_Defining_Identifier (Name_Find);
   end Map_Mutex_Identifier;

   ----------------------------------
   -- Map_Port_Argument_Identifier --
   ----------------------------------

   function Map_Port_Argument_Identifier (E : Node_Id) return Node_Id is
      Port_Name   : Name_Id;
   begin
      pragma Assert (Kind (E) = K_Port_Spec_Instance);

      Port_Name := To_Ada_Name (Display_Name (Identifier (E)));

      Get_Name_String (Port_Name);
      Add_Str_To_Name_Buffer ("_Arg");

      return Make_Defining_Identifier (Name_Find);
   end Map_Port_Argument_Identifier;

   ---------------------------------
   -- Map_Port_Boolean_Identifier --
   ---------------------------------

   function Map_Port_Boolean_Identifier (E : Node_Id) return Node_Id is
      Port_Name   : Name_Id;
   begin
      pragma Assert (Kind (E) = K_Port_Spec_Instance);

      Port_Name := To_Ada_Name (Display_Name (Identifier (E)));

      Get_Name_String (Port_Name);
      Add_Str_To_Name_Buffer ("_Present");

      return Make_Defining_Identifier (Name_Find);
   end Map_Port_Boolean_Identifier;

   -----------------------------------
   -- Map_Get_Subprogram_Identifier --
   -----------------------------------

   function Map_Get_Subprogram_Identifier (E : Node_Id) return Node_Id is
      Entity_Name   : Name_Id;
      Thread_Name : Name_Id;
   begin
      pragma Assert
        (AAU.Is_Data (E) or else Kind (E) = K_Port_Spec_Instance);

      if AAU.Is_Data (E) then
         Entity_Name := AAU.Compute_Full_Name_Of_Instance (E, True, False);
      else
         Entity_Name := To_Ada_Name (Display_Name (Identifier (E)));
         Thread_Name := To_Ada_Name
           (Display_Name
            (Identifier
             (Parent_Subcomponent
              (Parent_Component
               (E)))));
      end if;

      Set_Str_To_Name_Buffer ("Get_");

      if Kind (E) = K_Port_Spec_Instance then
         Get_Name_String_And_Append (Thread_Name);
         Add_Char_To_Name_Buffer ('_');
      end if;

      Get_Name_String_And_Append (Entity_Name);

      return Make_Defining_Identifier (Name_Find);
   end Map_Get_Subprogram_Identifier;

   -----------------------------------
   -- Map_Put_Subprogram_Identifier --
   -----------------------------------

   function Map_Put_Subprogram_Identifier (E : Node_Id) return Node_Id is
      Port_Name   : Name_Id;
      Thread_Name : Name_Id;
   begin
      pragma Assert (Kind (E) = K_Port_Spec_Instance);

      Port_Name := To_Ada_Name (Display_Name (Identifier (E)));
      Thread_Name := To_Ada_Name
        (Display_Name
         (Identifier
          (Parent_Subcomponent
           (Parent_Component
            (E)))));

      Set_Str_To_Name_Buffer ("Put_");
      Get_Name_String_And_Append (Thread_Name);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Port_Name);
      return Make_Defining_Identifier (Name_Find);
   end Map_Put_Subprogram_Identifier;

   -----------------------------------------
   -- Map_Push_Back_Subprogram_Identifier --
   -----------------------------------------

   function Map_Push_Back_Subprogram_Identifier (E : Node_Id) return Node_Id is
      Port_Name   : Name_Id;
      Thread_Name : Name_Id;
   begin
      pragma Assert (Kind (E) = K_Port_Spec_Instance);

      Port_Name := To_Ada_Name (Display_Name (Identifier (E)));
      Thread_Name := To_Ada_Name
        (Display_Name
         (Identifier
          (Parent_Subcomponent
           (Parent_Component
            (E)))));

      Set_Str_To_Name_Buffer ("Push_Back_");
      Get_Name_String_And_Append (Thread_Name);
      Add_Char_To_Name_Buffer ('_');
      Get_Name_String_And_Append (Port_Name);

      return Make_Defining_Identifier (Name_Find);
   end Map_Push_Back_Subprogram_Identifier;

   ---------------------------
   -- Must_Have_Port_Number --
   ---------------------------

   function Must_Have_Port_Number (E : Node_Id) return Boolean is
      T : Node_Id;
   begin
      pragma Assert (AAU.Is_Process (E));

      if Has_In_Ports (E) then
         return True;
      end if;

      if not AAU.Is_Empty (Subcomponents (E)) then
         T := AAN.First_Node (Subcomponents (E));

         while Present (T) loop
            if AAU.Is_Thread (Corresponding_Instance (T)) and then
              Has_In_Ports (Corresponding_Instance (T))
            then
               return True;
            end if;

            T := AAN.Next_Node (T);
         end loop;
      end if;

      return False;
   end Must_Have_Port_Number;

   -----------------------------------
   -- Package_Binding_Internal_Name --
   -----------------------------------

   function Package_Binding_Internal_Name (I : Node_Id) return Name_Id is
   begin
      if No (I) then
         return No_Name;
      end if;

      pragma Assert (Kind (I) = K_Defining_Identifier);

      if ADN.Name (I) = No_Name then
         return No_Name;
      end if;

      Set_Str_To_Name_Buffer ("%package%id%binding%");
      Get_Name_String_And_Append (ADN.Name (I));

      return Name_Find;
   end Package_Binding_Internal_Name;

   ------------------------------------
   -- Bind_Ada_Identifier_To_Package --
   ------------------------------------

   procedure Bind_Ada_Identifier_To_Package (I : Node_Id; P : Node_Id) is
      N : constant Name_Id := Package_Binding_Internal_Name (I);
   begin
      if N = No_Name then
         raise Program_Error with "Bind_Ada_Identifier_To_Package:"
           & " Try to bind a nul node";
      end if;

      Set_Name_Table_Info (N, Nat (P));
   end Bind_Ada_Identifier_To_Package;

   -----------------------
   -- Get_Bound_Package --
   -----------------------

   function Get_Bound_Package (I : Node_Id) return Node_Id is
      N : constant Name_Id := Package_Binding_Internal_Name (I);
   begin
      if N = No_Name then
         return No_Node;
      end if;

      return Node_Id (Get_Name_Table_Info (N));
   end Get_Bound_Package;

   ------------------------
   -- Bind_AADL_To_Setup --
   ------------------------

   procedure Bind_AADL_To_Setup (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Setup_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Setup;

   -----------------------------
   -- Bind_AADL_To_Parameters --
   -----------------------------

   procedure Bind_AADL_To_Parameters (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Parameters_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Parameters;

   -------------------------------
   -- Bind_AADL_To_Obj_Adapters --
   -------------------------------

   procedure Bind_AADL_To_Obj_Adapters (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Obj_Adapters_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Obj_Adapters;

   --------------------------
   -- Bind_AADL_To_Helpers --
   --------------------------

   procedure Bind_AADL_To_Helpers (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Helpers_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Helpers;

   -----------------------
   -- Bind_AADL_To_Main --
   -----------------------

   procedure Bind_AADL_To_Main (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Main_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Main;

   ---------------------------
   -- Bind_AADL_To_Servants --
   ---------------------------

   procedure Bind_AADL_To_Servants (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Servants_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Servants;

   -----------------------------
   -- Bind_AADL_To_Namespaces --
   -----------------------------

   procedure Bind_AADL_To_Namespaces (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Namespaces_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Namespaces;

   ---------------------------
   -- Bind_AADL_To_TypeCode --
   ---------------------------

   procedure Bind_AADL_To_TypeCode (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_TypeCode_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_TypeCode;

   ----------------------------------
   -- Bind_AADL_To_Execute_Servant --
   ----------------------------------

   procedure Bind_AADL_To_Execute_Servant (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Execute_Servant_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Execute_Servant;

   ---------------------------
   -- Bind_AADL_To_From_Any --
   ---------------------------

   procedure Bind_AADL_To_From_Any (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_From_Any_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_From_Any;

   -----------------------------
   -- Bind_AADL_To_Initialize --
   -----------------------------

   procedure Bind_AADL_To_Initialize (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Initialize_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Initialize;

   ----------------------------
   -- Bind_AADL_To_Reference --
   ----------------------------

   procedure Bind_AADL_To_Reference (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Reference_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Reference;

   -----------------------------
   -- Bind_AADL_To_Subprogram --
   -----------------------------

   procedure Bind_AADL_To_Subprogram (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Subprogram_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Subprogram;

   -------------------------------------
   -- Bind_AADL_To_Feature_Subprogram --
   -------------------------------------

   procedure Bind_AADL_To_Feature_Subprogram (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_HI_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Feature_Subprogram_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Feature_Subprogram;

   ----------------------
   -- Bind_AADL_To_Set --
   ----------------------

   procedure Bind_AADL_To_Set (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Set_Node (N, A);
   end Bind_AADL_To_Set;

   ------------------------
   -- Bind_AADL_To_Build --
   ------------------------

   procedure Bind_AADL_To_Build (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Build_Node (N, A);
   end Bind_AADL_To_Build;

   -------------------------
   -- Bind_AADL_To_To_Any --
   -------------------------

   procedure Bind_AADL_To_To_Any (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_To_Any_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_To_Any;

   ------------------------------------
   -- Bind_AADL_To_Thread_Controller --
   ------------------------------------

   procedure Bind_AADL_To_Thread_Controller (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Thread_Controller_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Thread_Controller;

   ----------------------------------
   -- Bind_AADL_To_Type_Definition --
   ----------------------------------

   procedure Bind_AADL_To_Type_Definition (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Type_Definition_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Type_Definition;

   --------------------------
   -- Bind_AADL_To_Package --
   --------------------------

   procedure Bind_AADL_To_Package (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Package_Node (N, A);
   end Bind_AADL_To_Package;

   ----------------------
   -- Bind_AADL_To_Put --
   ----------------------

   procedure Bind_AADL_To_Put (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Put_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Put;

   ----------------------------
   -- Bind_AADL_To_Push_Back --
   ----------------------------

   procedure Bind_AADL_To_Push_Back (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Push_Back_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Push_Back;

   ----------------------
   -- Bind_AADL_To_Get --
   ----------------------

   procedure Bind_AADL_To_Get (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AAN.Backend_Node (G);

      if No (N) then
         N := New_Node (ADN.K_QoS_Tree_Bindings);
         AAN.Set_Backend_Node (G, N);
      end if;

      ADN.Set_Get_Node (N, A);
      ADN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Get;

   -------------------------------
   -- Get_Servant_Internal_Name --
   -------------------------------

   function Get_Servant_Internal_Name (Entity : Node_Id) return Name_Id is
   begin
      pragma Assert (AAU.Is_Process (Entity) or else AAU.Is_Thread (Entity));

      if AAU.Is_Process (Entity) then
         Set_Str_To_Name_Buffer ("%process%servants%");
      else
         Set_Str_To_Name_Buffer ("%thread%servant%");
      end if;

      Add_Nat_To_Name_Buffer (Nat (Entity));

      return Name_Find;
   end Get_Servant_Internal_Name;

   ---------------------------
   -- Compute_Servant_Index --
   ---------------------------

   procedure Compute_Servant_Index (T : Node_Id) is
      T_Internal_Name : constant Name_Id := Get_Servant_Internal_Name (T);
      Parent_Process  : constant Node_Id := Parent_Component
        (Parent_Subcomponent (T));
      P_Internal_Name : constant Name_Id := Get_Servant_Internal_Name
        (Parent_Process);

      Info : Int;
   begin
      --  The Name_Table_Infos are initialized to 0, which is exactly
      --  what we want.

      Info := Get_Name_Table_Info (P_Internal_Name) + 1;
      Set_Name_Table_Info (T_Internal_Name, Info);

      --  Update the info for next threads

      Set_Name_Table_Info (P_Internal_Name, Info);
   end Compute_Servant_Index;

   -----------------------
   -- Get_Servant_Index --
   -----------------------

   function Get_Servant_Index (T : Node_Id) return Nat is
      T_Internal_Name : constant Name_Id := Get_Servant_Internal_Name (T);
      Info            : constant Int := Get_Name_Table_Info (T_Internal_Name);
   begin
      return Info;
   end Get_Servant_Index;

end Ocarina.Backends.PO_QoS_Ada.Mapping;
