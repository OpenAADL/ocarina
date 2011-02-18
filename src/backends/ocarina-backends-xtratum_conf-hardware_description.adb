------------------------------------------------------------------------------
--                                                                          --
--                           OCARINA COMPONENTS                             --
--                                                                          --
--           OCARINA.BACKENDS.XTRATUM_CONF.HARDWARE_DESCRIPTION             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, European Space Agency (ESA).              --
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

with Namet; use Namet;

with Ocarina.ME_AADL;
with Ocarina.ME_AADL.AADL_Tree.Nodes;
--  with Ocarina.ME_AADL.AADL_Tree.Entities;
with Ocarina.ME_AADL.AADL_Instances.Nodes;
with Ocarina.ME_AADL.AADL_Instances.Nutils;
with Ocarina.ME_AADL.AADL_Instances.Entities;

with Ocarina.Backends.Utils;
with Ocarina.Backends.Messages;
with Ocarina.Backends.Properties;
with Ocarina.Backends.XML_Values;
with Ocarina.Backends.XML_Tree.Nodes;
with Ocarina.Backends.XML_Tree.Nutils;
with Ocarina.Backends.Xtratum_Conf.Mapping;

package body Ocarina.Backends.Xtratum_Conf.Hardware_Description is

   use Ocarina.ME_AADL;
   use Ocarina.ME_AADL.AADL_Instances.Nodes;
   use Ocarina.ME_AADL.AADL_Instances.Entities;
   use Ocarina.Backends.Utils;
   use Ocarina.Backends.Messages;
   use Ocarina.Backends.Properties;
   use Ocarina.Backends.XML_Values;
   use Ocarina.Backends.XML_Tree.Nutils;
   use Ocarina.Backends.Xtratum_Conf.Mapping;

   package ATN renames Ocarina.ME_AADL.AADL_Tree.Nodes;
--   package ATE renames Ocarina.ME_AADL.AADL_Tree.Entities;
   package AINU renames Ocarina.ME_AADL.AADL_Instances.Nutils;
   package XTN renames Ocarina.Backends.XML_Tree.Nodes;
   package XV  renames Ocarina.Backends.XML_Values;

   procedure Visit_Architecture_Instance (E : Node_Id);
   procedure Visit_Component_Instance (E : Node_Id);
   procedure Visit_System_Instance (E : Node_Id);
   procedure Visit_Process_Instance (E : Node_Id);
   procedure Visit_Processor_Instance (E : Node_Id);
   procedure Visit_Virtual_Processor_Instance (E : Node_Id);

   Scheduling_Node      : Node_Id;
   Window_Number        : Unsigned_Long_Long := 0;
   Processor_Identifier : Unsigned_Long_Long := 0;

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

         when CC_Processor =>
            Visit_Processor_Instance (E);

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
      N : Node_Id;
   begin
      Map_Process_Scheduling (E, Window_Number, N);
      Append_Node_To_List (N, XTN.Subitems (Scheduling_Node));
   end Visit_Process_Instance;

   ---------------------------
   -- Visit_System_Instance --
   ---------------------------

   procedure Visit_System_Instance (E : Node_Id) is
      S              : Node_Id;
      Hw_Desc_Node   : Node_Id;
      Processor_Node : Node_Id;
      U              : Node_Id;
      R              : Node_Id;
   begin
      U := XTN.Unit (Backend_Node (Identifier (E)));
      R := XTN.Node (Backend_Node (Identifier (E)));

      Current_XML_Node := XTN.Root_Node (XTN.XML_File (U));

      Push_Entity (U);
      Push_Entity (R);

      Hw_Desc_Node := Make_XML_Node ("HwDescription");

      Append_Node_To_List (Hw_Desc_Node,
                           XTN.Subitems (Current_XML_Node));

      Processor_Node := Make_XML_Node ("ProcessorTable");

      Append_Node_To_List (Processor_Node,
                           XTN.Subitems (Hw_Desc_Node));

      Current_XML_Node := Processor_Node;

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
      Pop_Entity;
      Pop_Entity;
   end Visit_System_Instance;

   ------------------------------
   -- Visit_Processor_Instance --
   ------------------------------

   procedure Visit_Processor_Instance (E : Node_Id) is
      S                 : Node_Id;
      P                 : Node_Id;
      Q                 : Node_Id;
      Processor_Node    : Node_Id;
      Plan_Table_Node   : Node_Id;
      Plan_Node         : Node_Id;
      Start_Time        : Unsigned_Long_Long := 0;
      Slot_Node         : Node_Id;
--      Partition         : Node_Id;
      Slot_Identifier   : Unsigned_Long_Long := 0;
      Slots             : constant Time_Array
                           := Get_POK_Slots (E);
      Slots_Allocation  : constant List_Id
                           := Get_POK_Slots_Allocation (E);
   begin
      if Slots_Allocation = No_List then
         Display_Error
           ("You must provide the slots allocation for each processor",
            Fatal => True);
      end if;

      Scheduling_Node := Make_XML_Node ("Processor");

      --  First, add a <Processor> node
      Processor_Node := Make_XML_Node ("Processor");

      Set_Str_To_Name_Buffer ("id");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal
         (XV.New_Numeric_Value (Processor_Identifier, 0, 10));

      Append_Node_To_List
         (Make_Assignement (P, Q), XTN.Items (Processor_Node));

      Set_Str_To_Name_Buffer ("frequency");
      P := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer ("50Mhz");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List
         (Make_Assignement (P, Q), XTN.Items (Processor_Node));

      --  Within the <processor/> node, add a <CyclicPlanTable/>
      --  node.
      Plan_Table_Node := Make_XML_Node ("CyclicPlanTable");
      Append_Node_To_List (Plan_Table_Node, XTN.Subitems (Processor_Node));

      --  Then, describe the scheduling plan for this node.
      --  At this time, we support only one plan for each processor.
      Plan_Node := Make_XML_Node ("Plan");
      Append_Node_To_List (Plan_Node, XTN.Subitems (Plan_Table_Node));

      Set_Str_To_Name_Buffer ("id");
      P := Make_Defining_Identifier (Name_Find);
      Q := Make_Literal (XV.New_Numeric_Value (0, 0, 10));

      Append_Node_To_List
         (Make_Assignement (P, Q), XTN.Items (Plan_Node));

      Set_Str_To_Name_Buffer ("majorFrame");
      P := Make_Defining_Identifier (Name_Find);

      Set_Str_To_Name_Buffer
         (Unsigned_Long_Long'Image
            (To_Milliseconds (Get_POK_Major_Frame (E))));
      Add_Str_To_Name_Buffer ("ms");
      Q := Make_Defining_Identifier (Name_Find);

      Append_Node_To_List
         (Make_Assignement (P, Q), XTN.Items (Plan_Node));

      --  For each time slot for a partition, we declare
      --  it in the scheduling plan.
      S := ATN.First_Node (Slots_Allocation);
      for I in Slots'Range loop

         --  Create the node that corresponds to the slot.

         Slot_Node := Make_XML_Node ("Slot");

         --  Associate a fixed identifier to the slot.

         Set_Str_To_Name_Buffer ("id");
         P := Make_Defining_Identifier (Name_Find);
         Q := Make_Literal (XV.New_Numeric_Value (Slot_Identifier, 0, 10));

         Append_Node_To_List
            (Make_Assignement (P, Q), XTN.Items (Slot_Node));

         --  Define the duration attribute of the <slot/> element.
         Set_Str_To_Name_Buffer ("duration");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer
            (Unsigned_Long_Long'Image
               (To_Milliseconds (Slots (I))));
         Add_Str_To_Name_Buffer ("ms");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List
            (Make_Assignement (P, Q), XTN.Items (Slot_Node));

         --  Define the start attribute of the <slot/> element.
         Set_Str_To_Name_Buffer ("start");
         P := Make_Defining_Identifier (Name_Find);
         Set_Str_To_Name_Buffer
            (Unsigned_Long_Long'Image (Start_Time));
         Add_Str_To_Name_Buffer ("ms");
         Q := Make_Defining_Identifier (Name_Find);
         Append_Node_To_List
            (Make_Assignement (P, Q), XTN.Items (Slot_Node));

         Append_Node_To_List (Slot_Node, XTN.Subitems (Plan_Node));

         Slot_Identifier := Slot_Identifier + 1;

         Start_Time := Start_Time + To_Milliseconds (Slots (I));

         S := ATN.Next_Node (S);
      end loop;

      if not AINU.Is_Empty (Subcomponents (E)) then
         S := First_Node (Subcomponents (E));
         while Present (S) loop
         --  Visit the component instance corresponding to the
         --  subcomponent S.

            Visit (Corresponding_Instance (S));
            S := Next_Node (S);
         end loop;
      end if;

      Processor_Identifier := Processor_Identifier + 1;

      Append_Node_To_List (Processor_Node, XTN.Subitems (Current_XML_Node));
   end Visit_Processor_Instance;

   --------------------------------------
   -- Visit_Virtual_Processor_Instance --
   --------------------------------------

   procedure Visit_Virtual_Processor_Instance (E : Node_Id) is
      S           : Node_Id;
      Processes   : List_Id;
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

      if Present (Backend_Node (Identifier (E))) then
         Processes := XTN.Processes (Backend_Node (Identifier (E)));
         S := XTN.First_Node (Processes);
         while Present (S) loop
            Visit (XTN.Content (S));
            S := XTN.Next_Node (S);
         end loop;
      end if;
   end Visit_Virtual_Processor_Instance;

end Ocarina.Backends.Xtratum_Conf.Hardware_Description;
