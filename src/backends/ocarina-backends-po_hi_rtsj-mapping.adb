------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--  O C A R I N A . B A C K E N D S . P O _ H I _ R T S J . M A P P I N G   --
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

with Ocarina.Namet;
with Utils; use Utils;

with Ocarina.Backends.Messages;
with Ocarina.Backends.RTSJ_Values;
with Ocarina.Backends.RTSJ_Tree.Nodes;
with Ocarina.Backends.RTSJ_Tree.Nutils;
with Ocarina.Backends.PO_HI_RTSJ.Runtime;
with Ocarina.Backends.RTSJ_Tree.Debug;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;

package body Ocarina.Backends.PO_HI_RTSJ.Mapping is

   use Ocarina.Namet;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.RTSJ_Values;
   use Ocarina.Backends.RTSJ_Tree.Nodes;
   use Ocarina.Backends.RTSJ_Tree.Nutils;
   use Ocarina.Backends.PO_HI_RTSJ.Runtime;
   use Ocarina.Backends.RTSJ_Tree.Debug;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Nutils;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package RTN renames Ocarina.Backends.RTSJ_Tree.Nodes;
   package RTU renames Ocarina.Backends.RTSJ_Tree.Nutils;

   ---------------------------------
   -- Map_Distributed_Application --
   ---------------------------------
   function Map_Distributed_Application (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (RTN.K_HI_Distributed_Application);
   begin
      pragma Assert (AINU.Is_System (E) or else AINU.Is_Processor (E));

      --  Update the global variable to be able to fetch the root of
      --  the distributed application and generate the source files.
      RTN.Set_Name (N, To_RTSJ_Name (AIN.Name (AIN.Identifier (E))));

      RTN.Set_Units (N, New_List (RTN.K_List_Id));
      RTN.Set_HI_Nodes (N, New_List (RTN.K_List_Id));

      return N;
   end Map_Distributed_Application;

   -----------------
   -- Map_HI_Node --
   -----------------
   function Map_HI_Node (E : Node_Id) return Node_Id is
      N : constant Node_Id := New_Node (K_HI_Node);
   begin
      pragma Assert
        (AINU.Is_Process_Or_Device (E)
         or else AINU.Is_System (E)
         or else AINU.Is_Processor (E));

      --  The name of the node is not the name of the process
      --  component instance, but the name of the process subcomponent
      --  corresponding to this instance.
      RTN.Set_Name
        (N,
         To_RTSJ_Name
           (AIN.Name (AIN.Identifier (AIN.Parent_Subcomponent (E)))));
      Set_Units (N, New_List (K_List_Id));

      --  Append the partition N to the node list of the PolyORB-HI
      --  distributed application. We are sure that the top of the
      --  entity stack contains the C distributed application node.
      RTU.Append_Node_To_List (N, HI_Nodes (Current_Entity));
      Set_Distributed_Application (N, Current_Entity);

      return N;
   end Map_HI_Node;

   -----------------
   -- Map_HI_Unit --
   -----------------
   function Map_HI_Unit (E : Node_Id) return Node_Id is
      U : Node_Id;
      S : List_Id;
      N : Node_Id;
      P : Node_Id;
   begin
      pragma Assert
        (AINU.Is_System (E)
         or else AINU.Is_Process_Or_Device (E)
         or else AINU.Is_Processor (E));

      U := New_Node (RTN.K_HI_Unit, AIN.Identifier (E));
      S := RTU.New_List (K_Source_Files);

      --  Packages that are common to all nodes
      Set_Str_To_Name_Buffer ("subprograms");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      RTN.Set_Subprograms_Source (U, P);
      RTU.Append_Node_To_List (P, S);
      Bind_AADL_To_Subprogram (Identifier (E), P);

      Set_Str_To_Name_Buffer ("activity");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      RTN.Set_Activity_Source (U, P);
      RTU.Append_Node_To_List (P, S);
      Bind_AADL_To_Activity (Identifier (E), P);

      Set_Str_To_Name_Buffer ("main");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      RTN.Set_Main_Source (U, P);
      RTU.Append_Node_To_List (P, S);
      Bind_AADL_To_Main (Identifier (E), P);

      Set_Str_To_Name_Buffer ("deployment");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      RTN.Set_Deployment_Source (U, P);
      RTU.Append_Node_To_List (P, S);
      Bind_AADL_To_Deployment (Identifier (E), P);

      Set_Str_To_Name_Buffer ("naming");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      RTN.Set_Naming_Source (U, P);
      RTU.Append_Node_To_List (P, S);
      Bind_AADL_To_Naming (Identifier (E), P);

      Set_Str_To_Name_Buffer ("generated_types");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      RTN.Set_Generated_Types_Source (U, P);
      RTU.Append_Node_To_List (P, S);
      Bind_AADL_To_Generated_Types (Identifier (E), P);

      Set_Str_To_Name_Buffer ("transport_high_level_impl");
      N := Make_Defining_Identifier (Name_Find);
      P := Make_Source_File (N);
      Set_Distributed_Application_Unit (P, U);
      RTN.Set_Transport_High_Level_Source (U, P);
      RTU.Append_Node_To_List (P, S);
      Bind_AADL_To_Transport_High_Level (Identifier (E), P);

      --  Append the Unit to the units list of the current
      --  RTSJ partition.
      RTN.Set_Sources (U, S);

      RTU.Append_Node_To_List (U, Units (Current_Entity));
      RTN.Set_Entity (U, Current_Entity);

      return U;
   end Map_HI_Unit;

   ----------------------------------
   -- Map_RTSJ_Defining_Identifier --
   ----------------------------------
   function Map_RTSJ_Defining_Identifier
     (E      : Node_Id;
      Is_Obj : Boolean := False) return Node_Id
   is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (Display_Name (Identifier (E)), Is_Obj));
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_RTSJ_Defining_Identifier;

   ------------------------------
   -- Map_RTSJ_Enumerator_Name --
   ------------------------------
   function Map_RTSJ_Enumerator_Name (E : Node_Id) return Name_Id is
      RTSJ_Name_1 : Name_Id;
      RTSJ_Name_2 : Name_Id;
      RTSJ_Name_3 : Name_Id;
      P           : Node_Id;
      S           : Node_Id;
   begin
      if Kind (E) = K_Port_Spec_Instance then
         P := Parent_Subcomponent (Parent_Component (E));
         S := Parent_Subcomponent (Parent_Component (P));

         RTSJ_Name_1 := RTU.To_RTSJ_Name (AIN.Display_Name (Identifier (S)));
         RTSJ_Name_2 := RTU.To_RTSJ_Name (AIN.Display_Name (Identifier (P)));
         RTSJ_Name_3 := RTU.To_RTSJ_Name (AIN.Display_Name (Identifier (E)));

         Get_Name_String (RTSJ_Name_1);
         Add_Char_To_Name_Buffer ('_');
         Get_Name_String_And_Append (RTSJ_Name_2);
         Add_Char_To_Name_Buffer ('_');
         Get_Name_String_And_Append (RTSJ_Name_3);
         Add_Str_To_Name_Buffer ("_K");

      elsif AINU.Is_Subprogram (E)
        or else
        (Present (Corresponding_Instance (E))
         and then AINU.Is_Process (Corresponding_Instance (E)))
      then
         --  For the subprograms and processes, the enumerator name
         --  is mapped from the entity name.
         Get_Name_String (RTU.To_RTSJ_Name (Display_Name (Identifier (E))));
         Add_Str_To_Name_Buffer ("_K");

      elsif AINU.Is_Thread (Corresponding_Instance (E))
        or else Kind (E) = K_Port_Spec_Instance
      then
         --  For threads, the enumerator name is mapped from the
         --  containing process name and the thread subcomponent name.

         --  Verify if that the thread is a subcomponent of a process
         pragma Assert (AINU.Is_Process (Parent_Component (E)));

         RTSJ_Name_1 :=
           RTU.To_RTSJ_Name
             (AIN.Display_Name
                (Identifier (Parent_Subcomponent (Parent_Component (E)))));

         RTSJ_Name_2 := RTU.To_RTSJ_Name (AIN.Display_Name (Identifier (E)));

         Get_Name_String (RTSJ_Name_1);
         Add_Char_To_Name_Buffer ('_');
         Get_Name_String_And_Append (RTSJ_Name_2);
         Add_Str_To_Name_Buffer ("_K");

      else
         raise Program_Error
           with "Wrong node kind for Map_Java_Enumerator_Name";
      end if;

      RTSJ_Name_1 := Name_Find;
      RTSJ_Name_1 := To_Upper (RTSJ_Name_1);

      return RTSJ_Name_1;
   end Map_RTSJ_Enumerator_Name;

   ------------------------------------
   -- Map_RTSJ_Subprogram_Identifier --
   ------------------------------------
   function Map_RTSJ_Subprogram_Identifier (E : Node_Id) return Node_Id is
      Spg_Name : Name_Id;
   begin
      pragma Assert (AINU.Is_Thread (E) or else AINU.Is_Subprogram (E));

      --  if AINU.Is_Subprogram (E)
      --  and then Get_Source_Language (E) /= Language_RTSJ then
      --  Display_Error ("This is not a RTSJ function", Fatal => True);
      --  end if;

      W_Node_Id (E);

      --  Get the subprogram name
      if AINU.Is_Subprogram (E) then
         Spg_Name := Get_Source_Name (E);
      end if;
      Get_Name_String (Spg_Name);

      return Make_Defining_Identifier (Name_Find);
   end Map_RTSJ_Subprogram_Identifier;

   ------------------------------
   -- Map_RTSJ_Subprogram_Spec --
   ------------------------------
   function Map_RTSJ_Subprogram_Spec (E : Node_Id) return Node_Id is
      N      : Node_Id;
      Params : constant List_Id := New_List (K_Parameter_List);
   begin
      pragma Assert (AINU.Is_Subprogram (E));

      N :=
        RTU.Make_Function_Specification
          (Visibility          => No_List,
           Defining_Identifier => Map_RTSJ_Subprogram_Identifier (E),
           Parameters          => Params,
           Return_Type         => New_Node (K_Void));

      return N;
   end Map_RTSJ_Subprogram_Spec;

   ------------------------------
   -- Map_RTSJ_Subprogram_Body --
   ------------------------------
   function Map_RTSJ_Subprogram_Body (E : Node_Id) return Node_Id is
      Spec         : constant Node_Id := Map_RTSJ_Subprogram_Spec (E);
      Declarations : constant List_Id := New_List (K_Declaration_List);
      Statements   : constant List_Id := New_List (K_Statement_List);
      Params       : constant List_Id := New_List (K_Parameter_List);
      N            : Node_Id;
      P            : Node_Id;
   begin
      case Get_Subprogram_Kind (E) is
         when Subprogram_Empty =>
            --  An empty AADL subprogram is mapped into a Java
            --  subprogram that does nothing
            N := Message_Comment ("Empty subprogram");
            RTU.Append_Node_To_List (N, Statements);

            return RTU.Make_Function_Implementation
                (Spec,
                 Declarations,
                 Statements);

         when Subprogram_Opaque_RTSJ =>
            if not RTU.Is_Empty (Parameters (Spec)) then
               P := RTN.First_Node (RTN.Parameters (Spec));
               while Present (P) loop
                  RTU.Append_Node_To_List
                    (RTU.Copy_Node (Defining_Identifier (P)),
                     Params);
                  P := RTN.Next_Node (P);
               end loop;
            end if;

            --  Then, call the function provided by the user in our subprogram
            return RTU.Make_Call_Function
                (Make_Defining_Identifier (Get_Source_Name (E)),
                 Params);

         when others =>
            Display_Located_Error
              (AIN.Loc (E),
               "This kind of subprogram is not supported" &
               Get_Subprogram_Kind (E)'Img,
               Fatal => True);
            return No_Node;
      end case;
   end Map_RTSJ_Subprogram_Body;

   ----------------------------
   -- Map_Handler_Identifier --
   ----------------------------
   function Map_Handler_Identifier
     (E            : Node_Id;
      TaskHandler  : Boolean := False;
      EventHandler : Boolean := False) return Node_Id
   is
      Name : Name_Id;
   begin
      if Present (Corresponding_Instance (E))
        and then AINU.Is_Thread (Corresponding_Instance (E))
      then
         Get_Name_String
           (To_RTSJ_Conventional_Name
              (AIN.Display_Name (Identifier (E)),
               False));
         if TaskHandler then
            Add_Str_To_Name_Buffer ("TaskHandler");
         elsif EventHandler then
            Add_Str_To_Name_Buffer ("EventHandler");
         end if;
         Name := Name_Find;
      end if;

      return Make_Defining_Identifier (Name);
   end Map_Handler_Identifier;

   ----------------------------------
   -- Map_Handler_Class_Identifier --
   ----------------------------------
   function Map_Handler_Class_Identifier
     (E            : Node_Id;
      TaskHandler  : Boolean := False;
      EventHandler : Boolean := False) return Node_Id
   is
      Name : Name_Id;
   begin
      if Present (Corresponding_Instance (E))
        and then AINU.Is_Thread (Corresponding_Instance (E))
      then

         Get_Name_String
           (To_RTSJ_Conventional_Name
              (AIN.Display_Name (Identifier (E)),
               True));
         if TaskHandler then
            Add_Str_To_Name_Buffer ("TaskHandler");
         elsif EventHandler then
            Add_Str_To_Name_Buffer ("EventHandler");
         end if;
         Name := Name_Find;
      end if;

      return Make_Defining_Identifier (Name);
   end Map_Handler_Class_Identifier;

   -----------------------------
   -- Map_Priority_Identifier --
   -----------------------------
   function Map_Priority_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      if Present (Corresponding_Instance (E))
        and then AINU.Is_Thread (Corresponding_Instance (E))
      then

         Get_Name_String
           (To_RTSJ_Conventional_Name
              (AIN.Display_Name (Identifier (E)),
               False));
         Add_Str_To_Name_Buffer ("Priority");
         Name := Name_Find;
      end if;

      return Make_Defining_Identifier (Name);
   end Map_Priority_Identifier;

   -----------------------------
   -- Map_Task_Job_Identifier --
   -----------------------------
   function Map_Task_Job_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("Job");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Task_Job_Identifier;

   --------------------------------------
   -- Map_Task_Ports_Router_Identifier --
   --------------------------------------
   function Map_Task_Ports_Router_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("Router");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Task_Ports_Router_Identifier;

   ------------------------------
   -- Map_Task_Port_Identifier --
   ------------------------------
   function Map_Task_Port_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("Port");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Task_Port_Identifier;

   ---------------------------------
   -- Map_Task_Entries_Identifier --
   ---------------------------------
   function Map_Task_Entries_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("Entries");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Task_Entries_Identifier;

   ---------------------------------
   -- Map_Task_Deliver_Identifier --
   ---------------------------------
   function Map_Task_Deliver_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("Deliver");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Task_Deliver_Identifier;

   ------------------------------
   -- Map_Task_Send_Identifier --
   ------------------------------
   function Map_Task_Send_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("Send");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Task_Send_Identifier;

   ----------------------------
   -- Map_Port_Default_Entry --
   ----------------------------
   function Map_Port_Default_Entry (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("DefaultEntry");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Port_Default_Entry;

   ----------------------------
   -- Map_Port_Default_Value --
   ----------------------------
   function Map_Port_Default_Value (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("DefaultValue");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Port_Default_Value;

   -------------------------------
   -- Map_Port_Destinations_Tab --
   -------------------------------
   function Map_Port_Destinations_Tab (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("DestinationsTab");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Port_Destinations_Tab;

   -------------------------------------
   -- Map_Subprogram_Param_Identifier --
   -------------------------------------
   function Map_Subprogram_Param_Identifier (E : Node_Id) return Node_Id is
      Name : Name_Id;
   begin
      Get_Name_String
        (To_RTSJ_Conventional_Name (AIN.Display_Name (Identifier (E)), False));
      Add_Str_To_Name_Buffer ("Param");
      Name := Name_Find;

      return Make_Defining_Identifier (Name);
   end Map_Subprogram_Param_Identifier;

   -------------------
   -- Map_Time_Unit --
   -------------------
   function Map_Time_Unit (T : Time_Type) return Node_Id is
   begin
      case T.U is
         when Picosecond =>
            return No_Node;

         when Nanosecond =>
            return RE (RE_Nano_Second);

         when Microsecond =>
            return RE (RE_Micro_Second);

         when Millisecond =>
            return RE (RE_Milli_Second);

         when Second =>
            return RE (RE_Second);

         when Minute =>
            return RE (RE_Minute);

         when Hour =>
            return RE (RE_Hour);
      end case;

   end Map_Time_Unit;

   --------------------
   -- Map_Time_Value --
   --------------------
   function Map_Time_Value (T : Time_Type) return Node_Id is
   begin
      return Make_Literal (New_Int_Value (T.T, 0, 10));
   end Map_Time_Value;

   -----------------------
   -- Bind_AADL_To_Main --
   -----------------------
   procedure Bind_AADL_To_Main (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (RTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      RTN.Set_Main_Node (N, A);
      RTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Main;

   ---------------------------
   -- Bind_AADL_To_Activity --
   ---------------------------
   procedure Bind_AADL_To_Activity (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (RTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      RTN.Set_Activity_Node (N, A);
      RTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Activity;

   -----------------------------
   -- Bind_AADL_To_Deployment --
   -----------------------------
   procedure Bind_AADL_To_Deployment (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (RTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      RTN.Set_Deployment_Node (N, A);
      RTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Deployment;

   -----------------------------
   -- Bind_AADL_To_Subprogram --
   -----------------------------
   procedure Bind_AADL_To_Subprogram (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (RTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      RTN.Set_Subprograms_Node (N, A);
      RTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Subprogram;

   -------------------------
   -- Bind_AADL_To_Naming --
   -------------------------
   procedure Bind_AADL_To_Naming (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (RTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      RTN.Set_Naming_Node (N, A);
      RTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Naming;

   ----------------------------------
   -- Bind_AADL_To_Generated_Types --
   ----------------------------------
   procedure Bind_AADL_To_Generated_Types (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (RTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      RTN.Set_Generated_Types_Node (N, A);
      RTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Generated_Types;

   ---------------------------------------
   -- Bind_AADL_To_Transport_High_Level --
   ---------------------------------------
   procedure Bind_AADL_To_Transport_High_Level (G : Node_Id; A : Node_Id) is
      N : Node_Id;
   begin
      N := AIN.Backend_Node (G);

      if No (N) then
         N := New_Node (RTN.K_HI_Tree_Bindings);
         AIN.Set_Backend_Node (G, N);
      end if;

      RTN.Set_Transport_High_Level_Node (N, A);
      RTN.Set_Frontend_Node (A, G);
   end Bind_AADL_To_Transport_High_Level;

end Ocarina.Backends.PO_HI_RTSJ.Mapping;
