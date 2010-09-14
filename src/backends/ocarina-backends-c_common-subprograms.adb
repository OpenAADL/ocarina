------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--                  OCARINA.BACKENDS.C_COMMON.SUBPROGRAMS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2008-2010, GET-Telecom Paris.                --
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

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.C_Tree.Nutils;
with Ocarina.Backends.C_Tree.Nodes;
with Ocarina.Backends.C_Common.Mapping;

package body Ocarina.Backends.C_Common.Subprograms is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.C_Tree.Nutils;
   use Ocarina.Backends.C_Common.Mapping;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package CTN renames Ocarina.Backends.C_Tree.Nodes;

   C_Root : Node_Id;

   -----------------
   -- Header_File --
   -----------------

   package body Header_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
         (E : Node_Id; Real_Process : Boolean := True);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id; C : Node_Id := No_Node) is
      begin
         if C /= No_Node then
            C_Root := C;
         end if;

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

            when CC_Device =>
               Visit_Device_Instance (E);

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

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

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         U               : Node_Id;
         P               : Node_Id;
         Implementation  : Node_Id;
         S               : Node_Id;
      begin
         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            U := CTN.Distributed_Application_Unit
              (CTN.Naming_Node (Backend_Node (Identifier (E))));

            P := CTN.Entity (U);

            Push_Entity (P);
            Push_Entity (U);
         end if;

         Implementation := Get_Implementation (E);

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  if Get_Category_Of_Component (S) = CC_Process then
                     Visit_Process_Instance
                        (Corresponding_Instance (S), False);
                  end if;

                  if Get_Current_Backend_Kind = PolyORB_HI_C then
                     Visit_Component_Instance (Corresponding_Instance (S));
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;
         end if;

         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            Pop_Entity; -- U
            Pop_Entity; -- P
         end if;
      end Visit_Device_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         Data_Type : constant Supported_Data_Representation
                              := Get_Data_Representation (E);
         S         : Node_Id;
      begin
         if Data_Type = Data_With_Accessors then
            --  Visit all the accessor subprograms of the data type

            S := First_Node (Features (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;
      end Visit_Data_Instance;

      ------------------------------
      -- Visit_Processor_Instance --
      ------------------------------

      procedure Visit_Processor_Instance (E : Node_Id) is
         S : Node_Id;
         A : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         A := CTN.Deployment_Node (Backend_Node (Identifier (E)));
         Push_Entity (C_Root);
         Push_Entity (A);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity;
         Pop_Entity;
      end Visit_Processor_Instance;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
         Processes   : List_Id;
         S           : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            S := AIN.First_Node (Processes);
            while Present (S) loop
               Visit (AIN.Item (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_Virtual_Processor_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
         (E : Node_Id; Real_Process : Boolean := True) is
         U                    : Node_Id;
         P                    : Node_Id;
         S                    : Node_Id;
         C                    : Node_Id;
         The_System : constant Node_Id := Parent_Component
           (Parent_Subcomponent (E));
      begin
         if Real_Process then
            U := CTN.Distributed_Application_Unit
               (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);

            Push_Entity (P);
            Push_Entity (U);
         end if;

         Set_Subprograms_Header;

         Start_Recording_Handlings;

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
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

         if Get_Current_Backend_Kind = PolyORB_HI_C and then
            not AINU.Is_Empty (Subcomponents (The_System)) then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AINU.Is_Device (Corresponding_Instance (C))
               and then
                 Get_Bound_Processor (Corresponding_Instance (C))
                 = Get_Bound_Processor (E)
               then
                  --  Build the enumerator corresponding to the device
                  --  Note: we reuse the process name XXX
                  Visit_Device_Instance (Corresponding_Instance (C));
               end if;
               C := Next_Node (C);
            end loop;
         end if;

         --  Unmark all the marked subprograms

         Reset_Handlings;

         if Real_Process then
            Pop_Entity; -- U
            Pop_Entity; -- P
         end if;
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         N           : Node_Id;
         Call_Seq    : Node_Id;
         Spg_Call    : Node_Id;
      begin
         --  Generate the spec of the subprogram

         if No (Get_Handling (E, By_Name, H_C_Subprogram_Spec)) then
            N := Map_C_Subprogram_Spec (E);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            --  Mark the subprogram as being handled

            Set_Handling (E, By_Name, H_C_Subprogram_Spec, N);
         end if;

         Bind_AADL_To_Subprogram
           (Identifier (E),
            Get_Handling (E, By_Name, H_C_Subprogram_Spec));

         --  Visit all the call sequences of the subprogram

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
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
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            Push_Entity (C_Root);
         end if;

         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.
               if Get_Current_Backend_Kind = PolyORB_Kernel_C
                 and then Get_Category_Of_Component
                 (Corresponding_Instance (S)) = CC_Process
               then
                  null;
               else
                  if Get_Category_Of_Component
                    (Corresponding_Instance (S)) /= CC_Device
                  then
                     Visit (Corresponding_Instance (S));
                  end if;
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            Pop_Entity;
         end if;
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         Feature  : Node_Id;
      begin
         if Has_In_Ports (E) then
            Feature := First_Node (Features (E));

            while Present (Feature) loop
               if Kind (Feature) = K_Port_Spec_Instance
                 and then Is_In (Feature)
                 and then Is_Event (Feature)
                 and then not Is_Data (Feature)
               then
                  if Get_Port_Compute_Entrypoint (Feature) /= No_Node then
                     Visit (Get_Port_Compute_Entrypoint (Feature));
                  end if;
               end if;
               Feature := Next_Node (Feature);
            end loop;
         end if;

         --  Visit all the call sequences of the thread

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
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

   end Header_File;

   -----------------
   -- Source_File --
   -----------------

   package body Source_File is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance
         (E : Node_Id; Real_Process : Boolean := True);
      procedure Visit_Processor_Instance (E : Node_Id);
      procedure Visit_Virtual_Processor_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);
      procedure Visit_Subprogram_Instance (E : Node_Id);
      procedure Visit_Data_Instance (E : Node_Id);
      procedure Visit_Device_Instance (E : Node_Id);

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

            when CC_Device =>
               Visit_Device_Instance (E);

            when CC_Processor =>
               Visit_Processor_Instance (E);

            when CC_Virtual_Processor =>
               Visit_Virtual_Processor_Instance (E);

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

      ------------------------------
      -- Visit_Processor_Instance --
      ------------------------------

      procedure Visit_Processor_Instance (E : Node_Id) is
         S : Node_Id;
         A : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         A := CTN.Deployment_Node (Backend_Node (Identifier (E)));
         Push_Entity (C_Root);
         Push_Entity (A);

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.

               Visit (Corresponding_Instance (S));
               S := Next_Node (S);
            end loop;
         end if;

         Pop_Entity;
         Pop_Entity;
      end Visit_Processor_Instance;

      --------------------------------------
      -- Visit_Virtual_Processor_Instance --
      --------------------------------------

      procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
         Processes   : List_Id;
         S           : Node_Id;
      begin
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            return;
         end if;

         if Present (Backend_Node (Identifier (E))) then
            Processes := CTN.Processes (Backend_Node (Identifier (E)));
            S := AIN.First_Node (Processes);
            while Present (S) loop
               Visit (AIN.Item (S));
               S := AIN.Next_Node (S);
            end loop;
         end if;
      end Visit_Virtual_Processor_Instance;

      -------------------------
      -- Visit_Data_Instance --
      -------------------------

      procedure Visit_Data_Instance (E : Node_Id) is
         Data_Type : constant Supported_Data_Representation
                              := Get_Data_Representation (E);
         S         : Node_Id;
      begin
         if Data_Type = Data_With_Accessors then
            --  Visit all the accessor subprograms of the data type

            S := First_Node (Features (E));

            while Present (S) loop
               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;
      end Visit_Data_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance
         (E             : Node_Id;
         Real_Process   : Boolean := True) is
         U          : Node_Id;
         P          : Node_Id;
         S          : Node_Id;
         C          : Node_Id;
         N          : Node_Id;
         The_System : constant Node_Id := Parent_Component
           (Parent_Subcomponent (E));
      begin
         if Real_Process then
            U := CTN.Distributed_Application_Unit
               (CTN.Naming_Node (Backend_Node (Identifier (E))));
            P := CTN.Entity (U);
            Push_Entity (P);
            Push_Entity (U);
         end if;

         Set_Subprograms_Source;

         --  Start recording all the handlings

         Start_Recording_Handlings;

         --  First, generate extern declaration for globvars.

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               if AINU.Is_Data (Corresponding_Instance (S)) then
                  if Get_Current_Backend_Kind = PolyORB_HI_C and then
                     Get_Data_Representation (Corresponding_Instance (S)) =
                     Data_With_Accessors then

                     --  For POHIC, generate globvars thathave only accessors.

                     N := Make_Variable_Declaration
                        (Map_C_Defining_Identifier (S),
                        Map_C_Data_Type_Designator
                        (Corresponding_Instance (S)));

                     N := Make_Extern_Entity_Declaration (N);
                     Append_Node_To_List
                        (N, CTN.Declarations (Current_File));

                  else
                     --  For POK, generate all variables that are
                     --  declared in the process.
                     N := Make_Variable_Declaration
                        (Map_C_Defining_Identifier (S),
                        Map_C_Data_Type_Designator
                        (Corresponding_Instance (S)));

                     N := Make_Extern_Entity_Declaration (N);
                     Append_Node_To_List
                        (N, CTN.Declarations (Current_File));
                  end if;
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         --  Visit all the subcomponents of the process

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               Visit (Corresponding_Instance (S));

               if Get_Current_Backend_Kind = PolyORB_HI_C and then
                  AINU.Is_Data (Corresponding_Instance (S)) and then
                  Get_Data_Representation (Corresponding_Instance (S)) =
                  Data_With_Accessors then

                  N := Make_Variable_Declaration
                    (Map_C_Defining_Identifier (S),
                     Map_C_Data_Type_Designator
                     (Corresponding_Instance (S)));

                  N := Make_Extern_Entity_Declaration (N);
                  Append_Node_To_List
                    (N, CTN.Declarations (Current_File));
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         if Get_Current_Backend_Kind = PolyORB_HI_C and then
            not AINU.Is_Empty (Subcomponents (The_System)) then
            C := First_Node (Subcomponents (The_System));
            while Present (C) loop
               if AINU.Is_Device (Corresponding_Instance (C))
               and then
                 Get_Bound_Processor (Corresponding_Instance (C))
                 = Get_Bound_Processor (E)
               then
                  Visit_Device_Instance (Corresponding_Instance (C));
               end if;
               C := Next_Node (C);
            end loop;
         end if;

         --  Unmark all the marked subprograms

         Reset_Handlings;

         if Real_Process then
            Pop_Entity; -- U
            Pop_Entity; -- P
         end if;
      end Visit_Process_Instance;

      -------------------------------
      -- Visit_Subprogram_Instance --
      -------------------------------

      procedure Visit_Subprogram_Instance (E : Node_Id) is
         N                    : Node_Id;
         Call_Seq             : Node_Id;
         Spg_Call             : Node_Id;
      begin
         --  Generate the body of the subprogram

         if No (Get_Handling (E, By_Name, H_C_Subprogram_Body)) then
            if Get_Subprogram_Kind (E) = Subprogram_Scade then
               Append_Node_To_List
                  (Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier (VN (V_In)),
                     Used_type =>
                        Map_Scade_Struct_In (E)),
                  CTN.Declarations (Current_File));

               Append_Node_To_List
                  (Make_Variable_Declaration
                     (Defining_Identifier =>
                        Make_Defining_Identifier (VN (V_Out)),
                     Used_type =>
                        Map_Scade_Struct_Out (E)),
                  CTN.Declarations (Current_File));
            end if;

            N := Map_C_Subprogram_Body (E);
            Append_Node_To_List (N, CTN.Declarations (Current_File));

            --  Mark the subprogram as being handled

            Set_Handling (E, By_Name, H_C_Subprogram_Body, N);
         end if;

         --  Visit all the call sequences of the subprogram

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
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
         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            Push_Entity (C_Root);
         end if;

         --  Visit all the subcomponents of the system

         if not AINU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));
            while Present (S) loop
               --  Visit the component instance corresponding to the
               --  subcomponent S.
               if Get_Current_Backend_Kind = PolyORB_Kernel_C
                  and then Get_Category_Of_Component
                     (Corresponding_Instance (S)) = CC_Process then
                  null;
               else
                  if Get_Category_Of_Component
                     (Corresponding_Instance (S)) /= CC_Device then
                     Visit (Corresponding_Instance (S));
                  end if;
               end if;

               S := Next_Node (S);
            end loop;
         end if;

         if Get_Current_Backend_Kind /= PolyORB_Kernel_C then
            Pop_Entity;
            Pop_Entity;
         end if;
      end Visit_System_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         Call_Seq : Node_Id;
         Spg_Call : Node_Id;
         Feature  : Node_Id;
      begin
         if Has_In_Ports (E) then
            Feature := First_Node (Features (E));

            while Present (Feature) loop
               if Kind (Feature) = K_Port_Spec_Instance
                 and then Is_In (Feature)
                 and then Is_Event (Feature)
                 and then not Is_Data (Feature) then

                  if Get_Port_Compute_Entrypoint (Feature) /= No_Node then
                     Visit (Get_Port_Compute_Entrypoint (Feature));
                  end if;
               end if;
               Feature := Next_Node (Feature);
            end loop;
         end if;

         --  Visit all the call sequences of the thread

         if not AINU.Is_Empty (Calls (E)) then
            Call_Seq := First_Node (Calls (E));

            while Present (Call_Seq) loop
               --  For each call sequence visit all the called
               --  subprograms.

               if not AINU.Is_Empty (Subprogram_Calls (Call_Seq)) then
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

      ---------------------------
      -- Visit_Device_Instance --
      ---------------------------

      procedure Visit_Device_Instance (E : Node_Id) is
         U               : Node_Id;
         P               : Node_Id;
         Implementation  : Node_Id;
         S               : Node_Id;
      begin
         if Get_Current_Backend_Kind = PolyORB_Kernel_C then

            U := CTN.Distributed_Application_Unit
               (CTN.Naming_Node (Backend_Node (Identifier (E))));

            P := CTN.Entity (U);

            Push_Entity (P);
            Push_Entity (U);
         end if;

         Implementation := Get_Implementation (E);

         if Implementation /= No_Node then
            if not AINU.Is_Empty (AIN.Subcomponents (Implementation)) then
               S := First_Node (Subcomponents (Implementation));
               while Present (S) loop
                  if Get_Category_Of_Component (S) = CC_Process then
                     Visit_Process_Instance
                        (Corresponding_Instance (S), False);
                  end if;

                  if Get_Current_Backend_Kind = PolyORB_HI_C then
                     Visit_Component_Instance (Corresponding_Instance (S));
                  end if;

                  S := Next_Node (S);
               end loop;
            end if;

         end if;

         if Get_Current_Backend_Kind = PolyORB_Kernel_C then
            Pop_Entity; -- U
            Pop_Entity; -- P
         end if;
      end Visit_Device_Instance;
   end Source_File;

end Ocarina.Backends.C_Common.Subprograms;
