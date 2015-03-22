--  with Ada.Strings; use Ada.Strings;
--  with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ocarina.Backends.Messages;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Vxworks653_Conf.Mapping;

package body Ocarina.Backends.Vxworks653_Conf.Partitions is

   use Ocarina.ME_AADL;

   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Vxworks653_Conf.Mapping;

   package AIN renames Ocarina.ME_AADL.AADL_Instances.Nodes;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   Root_Node                : Node_Id := No_Node;
   Partitions_Node          : Node_Id := No_Node;
   Partition_Identifier     : Integer := 1;
   Process_Nb_Threads       : Unsigned_Long_Long := 0;
   Process_Nb_Buffers       : Unsigned_Long_Long := 0;
   Process_Nb_Events        : Unsigned_Long_Long := 0;
   Process_Nb_Lock_Objects  : Unsigned_Long_Long := 0;
   Process_Nb_Blackboards   : Unsigned_Long_Long := 0;
   Process_Blackboards_Size : Unsigned_Long_Long := 0;
   Process_Buffers_Size     : Unsigned_Long_Long := 0;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Thread_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   function Find_Associated_Process (Runtime : Node_Id;
                                     Current_Node : Node_Id := Root_Node)
                                     return Node_Id;

   -----------------------------
   -- Find_Associated_Process --
   -----------------------------

   function Find_Associated_Process (Runtime : Node_Id;
                                     Current_Node : Node_Id := Root_Node)
                                     return Node_Id is
      T : Node_Id;
      S : Node_Id;
   begin
      if Get_Category_Of_Component (Current_Node) = CC_Process and then
         Get_Bound_Processor (Current_Node) = Runtime
      then
         return Current_Node;
      end if;

      if not AINU.Is_Empty (Subcomponents (Current_Node)) then
         S := First_Node (Subcomponents (Current_Node));
         while Present (S) loop
            T := Find_Associated_Process
               (Runtime, Corresponding_Instance (S));

            if T /= No_Node then
               return T;
            end if;

            S := Next_Node (S);
         end loop;
      end if;

      return No_Node;
   end Find_Associated_Process;

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
      Root_Node := Root_System (E);
      Visit (Root_Node);
   end Visit_Architecture_Instance;

   ------------------------------
   -- Visit_Component_Instance --
   ------------------------------

   procedure Visit_Component_Instance (E : Node_Id) is
      Category : constant Component_Category := Get_Category_Of_Component (E);
   begin
      case Category is
         when CC_System =>
            Visit_System_Instance (E);

         when CC_Process =>
            Visit_Process_Instance (E);

         when CC_Thread =>
            Visit_Thread_Instance (E);

         when CC_Processor =>
            Visit_Processor_Instance (E);

         when CC_Bus =>
            Visit_Bus_Instance (E);

         when CC_Virtual_Processor =>
            Visit_Virtual_Processor_Instance (E);

         when others =>
            null;
      end case;
   end Visit_Component_Instance;

      ---------------------------
      -- Visit_Thread_Instance --
      ---------------------------

      procedure Visit_Thread_Instance (E : Node_Id) is
         F : Node_Id;
      begin
         Process_Nb_Threads := Process_Nb_Threads + 1;

         if not AINU.Is_Empty (Features (E)) then
            F := First_Node (Features (E));

            while Present (F) loop
               if Kind (F) = K_Port_Spec_Instance then
                  if Get_Connection_Pattern (F) = Intra_Process
                    and then Is_In (F)
                  then
                     if AIN.Is_Data (F) and then not AIN.Is_Event (F) then
                        Process_Nb_Blackboards := Process_Nb_Blackboards + 1;
                        Process_Blackboards_Size :=
                           Process_Blackboards_Size +
                              To_Bytes
                                 (Get_Data_Size
                                    (Corresponding_Instance (F)));
                     elsif AIN.Is_Data (F) and then AIN.Is_Event (F) then
                        Process_Nb_Buffers := Process_Nb_Buffers + 1;
                     elsif AIN.Is_Event (F) and then not AIN.Is_Data (F) then
                        Process_Nb_Events := Process_Nb_Events + 1;
                     else
                        Display_Error ("Communication Pattern not handled",
                                       Fatal => True);
                     end if;

                     Process_Nb_Lock_Objects := Process_Nb_Lock_Objects + 1;
                  end if;
               end if;
               F := Next_Node (F);
            end loop;
         end if;

      end Visit_Thread_Instance;

   ----------------------------
   -- Visit_Process_Instance --
   ----------------------------

   procedure Visit_Process_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S : Node_Id;
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.
            if AINU.Is_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
            end if;
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_System_Instance;

   ------------------------
   -- Visit_Bus_Instance --
   ------------------------

   procedure Visit_Bus_Instance (E : Node_Id) is
      pragma Unreferenced (E);
   begin
      null;
   end Visit_Bus_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S : Node_Id;
      U : Node_Id;
      P : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      P := XTN.Node (Backend_Node (Identifier (E)));

      Push_Entity (P);
      Push_Entity (U);

      Partition_Identifier := 1;

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Partitions_Node := Make_XML_Node ("Partitions");

      Append_Node_To_List
        (Partitions_Node,
         XTN.Subitems (Current_XML_Node));

      --
      --  First, make the <Partition/> nodes
      --

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
            --  Visit the component instance corresponding to the
            --  subcomponent S.

            if AINU.Is_Virtual_Processor (Corresponding_Instance (S)) then
               Visit (Corresponding_Instance (S));
            end if;
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
      S                       : Node_Id;
      Corresponding_Process   : Node_Id := No_Node;
      Partition_Node          : Node_Id;
   begin
      Corresponding_Process := Find_Associated_Process (E);

      if Corresponding_Process /= No_Node then

         Process_Nb_Threads         := 0;
         Process_Nb_Buffers         := 0;
         Process_Nb_Events          := 0;
         Process_Nb_Lock_Objects    := 0;
         Process_Nb_Blackboards     := 0;
         Process_Blackboards_Size   := 0;
         Process_Buffers_Size       := 0;

         Visit (Corresponding_Process);

         --
         --  First, we create the description of the partition.
         --
         Partition_Node := Map_Partition (Corresponding_Process,
                            E,
                            Partition_Identifier,
                            Process_Nb_Threads,
                            Process_Nb_Buffers,
                            Process_Nb_Events,
                            Process_Nb_Lock_Objects,
                            Process_Nb_Blackboards,
                            Process_Blackboards_Size,
                            Process_Buffers_Size);
         Append_Node_To_List
            (Partition_Node,
             XTN.Subitems (Partitions_Node));

         Partition_Identifier := Partition_Identifier + 1;
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Vxworks653_Conf.Partitions;
