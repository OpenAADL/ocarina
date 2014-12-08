--  with Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Deos_Conf.Mapping;

package body Ocarina.Backends.Deos_Conf.Partitions is

--   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.Deos_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XTU renames Ocarina.Backends.XML_Tree.Nutils;

   Root_Node : Node_Id := No_Node;
   Partitions_Node : Node_Id := No_Node;
   Memory_Regions : Node_Id := No_Node;
   Partition_Identifier : Integer := 1;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);
   function Find_Associated_Process (Runtime : Node_Id;
                                     Current_Node : Node_Id := Root_Node)
                                     return Node_Id;

   function Make_Default_Memory_Region return Node_Id;

   --------------------------------
   -- Make_Default_Memory_Region --
   --------------------------------

   function Make_Default_Memory_Region return Node_Id is
      N : Node_Id;
   begin
      N := Make_XML_Node ("MemoryRegion");

      XTU.Add_Attribute ("Name", "Initial RAM Pool", N);
      XTU.Add_Attribute ("Type", "Initial RAM Pool", N);
      XTU.Add_Attribute ("Address", "0x0", N);
      XTU.Add_Attribute ("Size", "0x19000", N);
      XTU.Add_Attribute ("AccessRights", "READ_WRITE", N);
      XTU.Add_Attribute ("PlatformMemoryPool", "0", N);

      return N;
   end Make_Default_Memory_Region;

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
      S : Node_Id;
      Corresponding_Process : Node_Id := No_Node;
      Partition_Node : Node_Id;
   begin
      Corresponding_Process := Find_Associated_Process (E);

      if Corresponding_Process /= No_Node then
         --
         --  First, we create the description of the partition.
         --
         Partition_Node := Map_Partition (Corresponding_Process,
                            E,
                            Partition_Identifier);
         Append_Node_To_List
            (Partition_Node,
             XTN.Subitems (Partitions_Node));

         --
         --  Then, we associate the partition with memory region
         --

         Memory_Regions := Make_XML_Node ("MemoryRegions");

         --
         --  FIXME: for now, we associate with the default
         --  memory. Has to work to get the AADL memory component.
         --
         Append_Node_To_List
            (Make_Default_Memory_Region,
            XTN.Subitems (Memory_Regions));

         Append_Node_To_List
            (Memory_Regions,
            XTN.Subitems (Partition_Node));
      end if;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Deos_Conf.Partitions;
