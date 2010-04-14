------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--     O C A R I N A . B A C K E N D S . P O _ Q O S _ A D A . M A I N      --
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

with GNAT.OS_Lib;
with GNAT.Expect;

with Namet;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Ada_Tree.Nutils;
with Ocarina.Backends.Ada_Tree.Nodes;
with Ocarina.Backends.PO_QoS_Ada.Runtime;
with Ocarina.Backends.PO_QoS_Ada.Mapping;
with Ocarina.Backends.Ada_Values;

package body Ocarina.Backends.PO_QoS_Ada.Main is

   use Namet;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Ada_Tree.Nutils;
   use Ocarina.Backends.PO_QoS_Ada.Runtime;
   use Ocarina.Backends.PO_QoS_Ada.Mapping;
   use Ocarina.Backends.Ada_Values;

   package AAU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package ADN renames Ocarina.Backends.Ada_Tree.Nodes;

   ---------------------
   -- Subprogram_Body --
   ---------------------

   package body Subprogram_Body is

      procedure Visit_Architecture_Instance (E : Node_Id);
      procedure Visit_Component_Instance (E : Node_Id);
      procedure Visit_System_Instance (E : Node_Id);
      procedure Visit_Process_Instance (E : Node_Id);
      procedure Visit_Thread_Instance (E : Node_Id);

      procedure ORB_Setup;
      --  Build the Setup clause according to the process properties

      function Get_IOR_Reference
        (Host_Location : Name_Id;
         Port_Number   : Unsigned_Long_Long;
         Servant_Index : Unsigned_Long_Long;
         Creator       : Name_Id;
         Protocol      : Name_Id;
         Priority      : Unsigned_Long_Long)
        return Name_Id;
      --  Build an IOR reference according to the given infomation.

      --  FIXME: Investigate very deeply the possible interference
      --  with this function when using Map_Ada_Priority

      function Is_Real_Time (P : Node_Id) return Boolean;
      --  Return True if a process need an RT_POA to be installed

      type Process_Parameters_Type is
         record
            Self                   : Node_Id;
            --  The current process

            N_Periodic_Threads     : Nat;
            N_Non_Periodic_Threads : Nat;

            Get_Ref_List           : List_Id;
            --  Contains the statements to collect references to other
            --  ports which have to come after all the thread own
            --  refernce putting.

            Periodic_Thread_List   : List_Id;
            --  Contains the statements to create the periodic threads
            --  which have to come after all references have been put
            --  or got.
         end record;

      Current_Process_Parameters      : Process_Parameters_Type;
      Current_Distributed_Application : Node_Id;

      ------------------
      -- Is_Real_Time --
      ------------------

      function Is_Real_Time (P : Node_Id) return Boolean is
         S  : Node_Id;
         RT : Boolean := False;
         NT : Natural := 0;
      begin
         pragma Assert (AAU.Is_Process (P));

         if not AAU.Is_Empty (Subcomponents (P)) then
            S := First_Node (Subcomponents (P));

            while Present (S) loop
               declare
                  SC : Node_Id;
                  P  : Unsigned_Long_Long;
               begin
                  SC := Corresponding_Instance (S);

                  if AAU.Is_Thread (SC) then
                     P  := Get_Thread_Priority (SC);
                     NT := NT + 1;

                     if P > 0 then
                        RT := True;
                     end if;
                  end if;
               end;

               S := Next_Node (S);
            end loop;
         end if;

         --  A process needs an RT_POA if it contains more than one
         --  thread and if one of the threads has been assigned a
         --  priority.

         RT := RT and then NT > 1;

         return RT;
      end Is_Real_Time;

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

            when others =>
               null;
         end case;
      end Visit_Component_Instance;

      ----------------------------
      -- Visit_Process_Instance --
      ----------------------------

      procedure Visit_Process_Instance (E : Node_Id) is
         U  : constant Node_Id := ADN.Distributed_Application_Unit
           (ADN.Helpers_Node (Backend_Node (Identifier (E))));
         P  : constant Node_Id := ADN.Entity (U);
         N  : Node_Id;
         S  : Node_Id;
      begin
         Push_Entity (P);
         Push_Entity (U);

         Set_Main_Body;

         --  Initialize the process parameters

         Current_Process_Parameters :=
           (Self                   => E,
            N_Periodic_Threads     => 0,
            N_Non_Periodic_Threads => 0,
            Get_Ref_List           => New_List (ADN.K_Statement_List),
            Periodic_Thread_List   => New_List (ADN.K_Statement_List));

         --  First loop to update the process parameters

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               declare
                  SC : Node_Id;
                  DP : Supported_Thread_Dispatch_Protocol;
               begin
                  SC := Corresponding_Instance (S);

                  if AAU.Is_Thread (SC) then
                     DP := Get_Thread_Dispatch_Protocol (SC);

                     if DP /= Thread_Periodic and then
                       DP /= Thread_Aperiodic and then
                       DP /= Thread_Sporadic
                     then
                        Display_Located_Error
                          (Loc (SC),
                           "Unsupported thread dispatching protocol",
                           Fatal => True);
                     end if;

                     if DP = Thread_Periodic then
                        Current_Process_Parameters.N_Periodic_Threads :=
                          Current_Process_Parameters.N_Periodic_Threads + 1;
                     else
                        Current_Process_Parameters.N_Non_Periodic_Threads :=
                          Current_Process_Parameters.N_Non_Periodic_Threads
                          + 1;
                     end if;
                  end if;
               end;

               S := Next_Node (S);
            end loop;
         end if;

         --  Initialize_World

         N := Make_Subprogram_Call (RE (RE_Initialize_World), No_List);
         Append_Node_To_List (N, ADN.Statements (Current_Package));

         --  Visit all the subcomponents of the process

         if not AAU.Is_Empty (Subcomponents (E)) then
            S := First_Node (Subcomponents (E));

            while Present (S) loop
               --  Visit the corresponding component instance

               Visit (Corresponding_Instance (S));

               S := Next_Node (S);
            end loop;
         end if;

         --  Append the Get_Ref statements

         if not Is_Empty (Current_Process_Parameters.Get_Ref_List) then
            Append_Node_To_List
              (ADN.First_Node (Current_Process_Parameters.Get_Ref_List),
               ADN.Statements (Current_Package));
         end if;

         --  Append the Periodic threads creation statements

         if not Is_Empty (Current_Process_Parameters.Periodic_Thread_List) then
            Append_Node_To_List
              (ADN.First_Node
               (Current_Process_Parameters.Periodic_Thread_List),
               ADN.Statements (Current_Package));
         end if;

         --  Run the ORB

         if Has_In_Ports (E) then
            N := Make_Subprogram_Call
              (RE (RE_Run),
               Make_List_Id
               (RE (RE_The_ORB),
                Make_Component_Association
                (Make_Defining_Identifier (PN (P_May_Exit)), RE (RE_False))));
            Append_Node_To_List (N, ADN.Statements (Current_Package));
         end if;

         --  Setup the tasking runtime

         ORB_Setup;

         Pop_Entity; --  U
         Pop_Entity; --  P
      end Visit_Process_Instance;

      ---------------------------
      -- Visit_System_Instance --
      ---------------------------

      procedure Visit_System_Instance (E : Node_Id) is
         S : Node_Id;
      begin
         Current_Distributed_Application := E;

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
         N               : Node_Id;
         L               : List_Id;
         F               : Node_Id;
         D               : Node_Id;
         Priority        : constant Unsigned_Long_Long :=
           Get_Thread_Priority (E);
         Byte_Stack_Size : constant Unsigned_Long_Long :=
           To_Bytes (Get_Thread_Stack_Size (E));
         DP              : constant Supported_Thread_Dispatch_Protocol :=
           Get_Thread_Dispatch_Protocol (E);
         Binding_Done    : Boolean := False;
      begin
         Set_Main_Body;

         --  Handle the thread features

         if not AAU.Is_Empty (Features (E)) then

            F := First_Node (Features (E));
            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  if Is_In (F) then
                     if not Binding_Done then
                        Binding_Done := True;

                        --  Binding between the Object implementation
                        --  and the reference.

                        N := Make_Object_Instantiation
                          (Extract_Designator
                           (ADN.Type_Definition_Node
                            (Backend_Node
                             (Identifier
                              (E)))));

                        if Is_Real_Time (Current_Process_Parameters.Self) then
                           L := Make_List_Id
                             (N,
                              Extract_Designator
                              (ADN.Reference_Node
                               (Backend_Node
                                (Identifier
                                 (E)))),
                              Make_Literal
                              (New_String_Value
                               (Name
                                (Identifier
                                 (Parent_Subcomponent
                                  (E))))));

                           if Priority /= 0 then
                              Append_Node_To_List
                                (Make_Literal
                                 (New_Integer_Value
                                  (Priority, 0, 10)),
                                 L);
                           end if;

                           N := Make_Subprogram_Call
                             (RE (RE_Link_To_Obj_Adapter_2), L);
                        else
                           L := Make_List_Id
                             (N, Extract_Designator
                              (ADN.Reference_Node
                               (Backend_Node
                                (Identifier (E)))));
                           N := Make_Subprogram_Call
                             (RE (RE_Link_To_Obj_Adapter), L);
                        end if;
                        Append_Node_To_List
                          (N, ADN.Statements (Current_Package));

                     end if;
                  end if;

                  if Is_Out (F) then
                     D := First_Node (Get_Destination_Ports (F));

                     while Present (D) loop
                        declare
                           Params          : constant List_Id :=
                             New_List (ADN.K_List_Id);
                           Parent_Thread   : constant Node_Id :=
                             Parent_Component (Item (D));
                           Parent_Process  : constant Node_Id :=
                             Parent_Component
                             (Parent_Subcomponent
                              (Parent_Thread));
                           Host_Location   : constant Name_Id :=
                             Get_Location
                             (Get_Bound_Processor (Parent_Process));
                           Port_Number     : constant Value_Id :=
                             To_Ada_Value
                             (Get_Port_Number (Parent_Process));

                           --  At this point we are sure that the
                           --  remote process has a valid port number
                           --  because we generate the
                           --  PolyORB.Parameters.Partition body
                           --  before the main body.

                           Creator         : constant Name_Id := Name
                             (Identifier
                              (Parent_Subcomponent
                               (Parent_Thread)));
                           Protocol        : constant Protocol_Type :=
                             Get_Protocol (Current_Distributed_Application);
                           Remote_Priority : constant Unsigned_Long_Long :=
                             Get_Thread_Priority (Parent_Thread);
                           Servant_Index   : constant Unsigned_Long_Long :=
                             Unsigned_Long_Long
                             (Get_Servant_Index (Parent_Thread));
                           Proto_Name      : Name_Id;
                        begin
                           --  Add a use clauyse to the servants
                           --  package for more code lisibility.

                           Add_With_Package
                             (Extract_Designator
                              (ADN.Servants_Package (Current_Entity), False),
                              Used => True);

                           --  Compute the reference of the remote
                           --  thread.

                           N := Map_Reference_Identifier (Item (D));
                           Append_Node_To_List (N, Params);

                           --  The way the remoted reference is
                           --  computed depends on the real-time
                           --  characteristics of the application.

                           if Is_Real_Time (Parent_Process) then
                              case Protocol is
                                 when Protocol_DIOP =>
                                    Proto_Name := Get_String_Name ("diop");
                                 when others =>
                                    Proto_Name := Get_String_Name ("iiop");
                              end case;

                              declare
                                 IOR : constant Name_Id := Get_IOR_Reference
                                   (Host_Location,
                                    Value (Port_Number).IVal,
                                    Servant_Index,
                                    Creator,
                                    Proto_Name,
                                    Remote_Priority);
                              begin
                                 N := Make_Literal (New_String_Value (IOR));
                                 Append_Node_To_List (N, Params);

                                 N := Make_Subprogram_Call
                                   (RE (RE_Get_GIOP_Ref), Params);
                                 Append_Node_To_List
                                   (N,
                                    Current_Process_Parameters.Get_Ref_List);
                              end;
                           else
                              N := Make_Literal
                                (New_String_Value
                                 (Host_Location));
                              Append_Node_To_List (N, Params);

                              N := Make_Literal (Port_Number);
                              Append_Node_To_List (N, Params);

                              N := Make_Literal
                                (New_Integer_Value
                                 (Servant_Index, 1, 10));
                              Append_Node_To_List (N, Params);

                              case Protocol is
                                 when Protocol_DIOP =>
                                    Set_Str_To_Name_Buffer ("diop");
                                 when others =>
                                    Set_Str_To_Name_Buffer ("iiop");
                              end case;

                              N := Make_Literal (New_String_Value (Name_Find));
                              Append_Node_To_List (N, Params);

                              N := Make_Subprogram_Call
                                (RE (RE_Get_Ref), Params);
                              Append_Node_To_List
                                (N, Current_Process_Parameters.Get_Ref_List);
                           end if;
                        end;
                        D := Next_Node (D);
                     end loop;
                  end if;
               end if;

               F := Next_Node (F);
            end loop;
         end if;

         if DP = Thread_Periodic then
            Current_Process_Parameters.N_Periodic_Threads :=
              Current_Process_Parameters.N_Periodic_Threads + 1;

            --  Create the periodic thread

            N := Extract_Designator
              (ADN.Thread_Controller_Node
               (Backend_Node
                (Identifier
                 (E))));

            N := Make_Attribute_Designator (N, A_Access);
            N := Make_Component_Association
              (Make_Defining_Identifier (PN (P_Tp)), N);
            L := Make_List_Id (N);

            if Priority /= 0 then
               N := Make_Literal (New_Integer_Value (Priority, 1, 10));
               N := Make_Component_Association
                 (Make_Defining_Identifier (PN (P_Priority)), N);
               Append_Node_To_List (N, L);
            end if;

            if Byte_Stack_Size /= 0 then
               N := Make_Literal (New_Integer_Value (Byte_Stack_Size, 1, 10));
               N := Make_Component_Association
                 (Make_Defining_Identifier (PN (P_Storage_Size)), N);
               Append_Node_To_List (N, L);
            end if;

            N := Make_Subprogram_Call
              (RE (RE_Create_Periodic_Thread), L);
            Append_Node_To_List
              (N, Current_Process_Parameters.Periodic_Thread_List);
         else
            Current_Process_Parameters.N_Non_Periodic_Threads :=
              Current_Process_Parameters.N_Non_Periodic_Threads + 1;
         end if;
      end Visit_Thread_Instance;

      ---------------
      -- ORB_Setup --
      ---------------

      procedure ORB_Setup is
         Multitask : constant Boolean :=
           (Current_Process_Parameters.N_Periodic_Threads +
            Current_Process_Parameters.N_Non_Periodic_Threads) > 1;
         N         : Node_Id;
      begin
         if Multitask then
            --  Full tasking mode

            N := Message_Comment ("Full tasking mode");
            Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));

            Add_With_Package
              (E            => RU (RU_ARAO_Setup_Application, False),
               Used         => False,
               Warnings_Off => True,
               Elaborated   => True);

            Add_With_Package
              (E            => RU (RU_ARAO_Setup_Tasking_Full_Tasking, False),
               Used         => False,
               Warnings_Off => True,
               Elaborated   => True);
         else
            --  No tasking mode

            N := Message_Comment ("No tasking mode");
            Append_Node_To_List (N, ADN.Withed_Packages (Current_Package));

            Add_With_Package
              (E            => RU (RU_ARAO_Setup_Application, False),
               Used         => False,
               Warnings_Off => True,
               Elaborated   => True);

            Add_With_Package
              (E            => RU (RU_ARAO_Setup_Tasking_No_Tasking, False),
               Used         => False,
               Warnings_Off => True,
               Elaborated   => True);
         end if;
      end ORB_Setup;

      -----------------------
      -- Get_IOR_Reference --
      -----------------------

      function Get_IOR_Reference
        (Host_Location : Name_Id;
         Port_Number   : Unsigned_Long_Long;
         Servant_Index : Unsigned_Long_Long;
         Creator       : Name_Id;
         Protocol      : Name_Id;
         Priority      : Unsigned_Long_Long)
        return Name_Id
      is
         use GNAT.OS_Lib;
         use GNAT.Expect;

         Args : Argument_List :=
           (new String'("-t"),
            new String'("IDL:AADL_Model:1.0"),
            new String'("-pn"),
            new String'("1"),
            new String'("-pt"),
            new String'(Get_Name_String (Protocol)),
            new String'("-i"),
            new String'(Unsigned_Long_Long'Image (Servant_Index)),
            new String'("-g"),
            new String'("-cr"),
            new String'("RTPOA_" & Get_Name_String (Creator)),
            new String'("-vmj"),
            new String'("1"),
            new String'("-vmn"),
            new String'("2"),
            new String'("-a"),
            new String'(Get_Name_String (Host_Location)),
            new String'("-p"),
            new String'(Unsigned_Long_Long'Image (Port_Number)),
            new String'("-cn"),
            new String'("2"),
            new String'("-ct"),
            new String'("code_set"),
            new String'("-char"),
            new String'("16#00010001#"),
            new String'("-s"),
            new String'("0"),
            new String'("-wchar"),
            new String'("16#00010100#"),
            new String'("-s"),
            new String'("2"),
            new String'("16#00010101#"),
            new String'("16#00010102#"),
            new String'("-ce"),
            new String'("-ct"),
            new String'("policies"),
            new String'("-pol_nb"),
            new String'("1"),
            new String'("-model"),
            new String'("SERVER_DECLARED"),
            new String'("-priority"),
            new String'(Unsigned_Long_Long'Image (Priority)),
            new String'("-ce"),
            new String'("-pe"));

         PO_CRR : constant String := "po_createref";

         S      : String_Access;
         Status : aliased Integer := 0;
         Path   : String_Access := Getenv ("PATH");
      begin
         --  Check whether 'po_createref' exists in the PATH

         S := Locate_Exec_On_Path (PO_CRR);

         if S = null then
            --  Deallocate the argument list

            for J in Args'Range loop
               Free (Args (J));
            end loop;

            Display_Error
              ("Command not found: " & PO_CRR,
               Fatal => True);
         else
            pragma Debug (Display_Debug_Message (PO_CRR & ": " & S.all, True));
            Name_Len := 0;
            for J in Args'Range loop
               pragma Debug (Add_Str_To_Name_Buffer (Args (J).all & " "));
               null;
            end loop;

            pragma Debug (Display_Debug_Message
                          ("Options: " & Name_Buffer (1 .. Name_Len), True));
            Free (S);
         end if;

         pragma Debug (Display_Debug_Message ("PATH=" & Path.all, True));
         Free (Path);

         --  Invoke po_createref

         declare
            PO_CRR_Output : constant String :=
              Get_Command_Output
              (Command    => PO_CRR,
               Arguments  => Args,
               Input      => "",
               Status     => Status'Access,
               Err_To_Out => True);

            Last          : Integer := PO_CRR_Output'Last;
         begin
            --  Deallocate the argument list

            for J in Args'Range loop
               Free (Args (J));
            end loop;

            pragma Debug
              (Display_Debug_Message
               (PO_CRR & " output: """ & PO_CRR_Output & """", True));

            --  Verify we get indeed an IOR as a result

            if Status /= 0 then
               raise Program_Error with "po_createref termined abnormaly => "
                 & "check for parameters specifications";
            end if;

            if PO_CRR_Output'Length < 4 or else
              PO_CRR_Output (PO_CRR_Output'First .. PO_CRR_Output'First + 3)
              /= "IOR:"
            then
               raise Program_Error with "Invalid IOR reference";
            end if;

            --  On platforms where lines end with CRLF, we remove the
            --  trailing CR character.

            if PO_CRR_Output (Last) = ASCII.CR then
               Last := Last - 1;
            end if;

            --  Every thing went fine

            return Get_String_Name
              (PO_CRR_Output (PO_CRR_Output'First .. Last));
         end;
      end Get_IOR_Reference;

   end Subprogram_Body;

end Ocarina.Backends.PO_QoS_Ada.Main;
