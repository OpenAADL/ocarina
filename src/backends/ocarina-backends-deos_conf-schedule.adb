--  with Locations;
with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

--  with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
--  with Ocarina.Backends.Deos_Conf.Mapping;

package body Ocarina.Backends.Deos_Conf.Schedule is

--   use Locations;
   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.XML_Tree.Nutils;
--   use Ocarina.Backends.Properties;
--   use Ocarina.Backends.Deos_Conf.Mapping;

   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;

   Root_Node : Node_Id := No_Node;
   Schedule_Node : Node_Id := No_Node;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Bus_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);

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

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Schedule_Node := Make_XML_Node ("Schedule");

      Append_Node_To_List
        (Schedule_Node,
         XTN.Subitems (Current_XML_Node));

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
   begin
      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Deos_Conf.Schedule;
